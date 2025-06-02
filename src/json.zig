const std = @import("std");

// https://www.rfc-editor.org/rfc/pdfrfc/rfc8259.txt.pdf

pub const JsonValue = union(enum) {
    object: JsonObject,
    array: JsonArray,
    primitive: JsonPrimitive,
};

// JsonObject = "{" [ member *( "," member) ] "}"
// member = string ":" JsonValue
pub const JsonObject = std.StringHashMap(JsonValue);

// JsonArray = "[" [ JsonValue *( "," JsonValue ) ] "]"
pub const JsonArray = std.ArrayList(JsonValue);

pub const JsonPrimitive = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    null: void,
};

pub const Json = struct {
    root: JsonValue,
    aa: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Json) void {
        self.aa.deinit();
    }
};

pub fn parseFile(allocator: std.mem.Allocator, file: std.fs.File) !Json {
    var aa = std.heap.ArenaAllocator.init(allocator);
    // If the parse fails, the aa (and all allocated memory with it).
    // If the parse succeeds, the aa deinit is in the hands of the caller.
    errdefer aa.deinit();
    const aa_allocator = aa.allocator();

    // Tokenize file into a more useful form.
    const tokens = try tokenize(aa_allocator, file);
    for (tokens.items) |item| {
        std.debug.print("token: {any}\n", .{item});
    }

    const root = try parser(aa_allocator, tokens);

    return Json{
        .root = root,
        .aa = aa,
        .allocator = aa_allocator,
    };
}

const Token = struct {
    token_type: TokenType,
    value: []const u8,
    line: u32,
    column: u32,

    const TokenType = enum {
        begin_array,
        begin_object,
        end_array,
        end_object,
        name_separator,
        value_separator,
        string_literal,
        null_literal,
        bool_literal,
        number_literal,

        fn getTokenType(value: []const u8) TokenType {
            switch (value[0]) {
                '[' => return TokenType.begin_array,
                '{' => return TokenType.begin_object,
                ']' => return TokenType.end_array,
                '}' => return TokenType.end_object,
                ':' => return TokenType.name_separator,
                ',' => return TokenType.value_separator,
                '"' => return TokenType.string_literal,
                'n' => return TokenType.null_literal,
                't', 'f' => return TokenType.bool_literal,
                else => return TokenType.number_literal,
            }
        }
    };

    fn init(value: []const u8, tokenizer_state: TokenizerState) Token {
        const token_type = TokenType.getTokenType(value);

        return Token{
            .token_type = token_type,
            .value = value,
            .line = tokenizer_state.token_start_line,
            .column = tokenizer_state.token_start_column,
        };
    }
};

const TokenizerState = struct {
    line: u32 = 1,
    column: u32 = 1,
    in_string: bool = false,
    token_start_line: u32 = 1,
    token_start_column: u32 = 1,
};

fn tokenize(allocator: std.mem.Allocator, file: std.fs.File) !std.ArrayList(Token) {
    var br = std.io.bufferedReader(file.reader());
    const reader = br.reader();

    var tokens = std.ArrayList(Token).init(allocator);

    // Token stored as ArrayList so that building is efficient.
    var current_token = std.ArrayList(u8).init(allocator);
    defer current_token.deinit();

    var state = TokenizerState{};

    var b_last: u8 = 0;
    var b = try reader.readByte();
    while (b > 0) : ({
        state.column += 1;
        b_last = b;
        b = reader.readByte() catch break;
    }) {
        std.debug.print("b: {c} ({d})\n", .{ b, b });
        // Start of string
        if (!state.in_string and b == '"') {
            state.in_string = true;

            try advanceToken(allocator, &current_token, &tokens, &state);

            try addToTokenBuilder(&current_token, b, &state);
            continue;
        }

        // End of string
        if (state.in_string and b == '"' and b_last != '\\') {
            state.in_string = false;

            try addToTokenBuilder(&current_token, b, &state);

            try advanceToken(allocator, &current_token, &tokens, &state);
            continue;
        }

        // Inside of string
        if (state.in_string) {
            try addToTokenBuilder(&current_token, b, &state);
            continue;
        }

        // If whitespace (and not in string), advance token.
        // Whitespace is not stored in the token list.
        if (std.ascii.isWhitespace(b)) {
            try advanceToken(allocator, &current_token, &tokens, &state);
            if (b == '\n') {
                state.line += 1;
                state.column = 0;
            }
            continue;
        }

        // If we encounter a symbol (outside strings),
        // advance token, store this symbol in new token, and end that token.
        if (isJsonSymbol(b)) {
            try advanceToken(allocator, &current_token, &tokens, &state);
            try addToTokenBuilder(&current_token, b, &state);
            try advanceToken(allocator, &current_token, &tokens, &state);
            continue;
        }

        // At this stage, the token must be a non-string primitive.
        try current_token.append(b);
    }

    // We should not be in a string at this point.
    // If we are, the JSON is invalid.
    if (state.in_string) {
        try std.io.getStdErr().writer().print(
            "Parse Error: Unclosed string beginning on line {d}\n",
            .{state.token_start_line},
        );

        return JsonError.UnclosedString;
    }

    return tokens;
}

/// Helper function for tokenize() for storing the current token and setting up for the next.
fn advanceToken(allocator: std.mem.Allocator, token_builder: *std.ArrayList(u8), tokens: *std.ArrayList(Token), state: *TokenizerState) !void {
    if (token_builder.items.len == 0) {
        return;
    }

    const token = try finalizeToken(allocator, token_builder, state.*);
    try tokens.append(token);
    token_builder.clearRetainingCapacity();

    state.token_start_line = state.line;
    state.token_start_column = state.column + 1;
}

/// Helper function for tokenize() to determine if a character is a symbol used in JSON.
/// e.g. '[' or ','
fn isJsonSymbol(char: u8) bool {
    return switch (char) {
        ':', '[', ']', '{', '}', ',' => true,
        else => false,
    };
}

/// Helper function for tokenize() to turn ArrayList tokens into string tokens.
fn finalizeToken(allocator: std.mem.Allocator, token_builder: *std.ArrayList(u8), state: TokenizerState) !Token {
    var text = try allocator.alloc(u8, token_builder.items.len);
    @memcpy(text[0..], token_builder.items);

    const token = Token.init(text, state);

    return token;
}

/// Helper function for tokenize() to add a character to a token in ArrayList form.
/// It also sets the token_start_line and token_start_column values correctly.
fn addToTokenBuilder(token_builder: *std.ArrayList(u8), b: u8, state: *TokenizerState) !void {
    if (token_builder.items.len == 0) {
        state.token_start_line = state.line;
        state.token_start_column = state.column;
    }

    try token_builder.append(b);
}

fn parser(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) !JsonValue {
    const root = if (tokens.items[0].token_type == Token.TokenType.begin_object)
        JsonValue{ .object = JsonObject.init(allocator) }
    else if (tokens.items[0].token_type == Token.TokenType.begin_array)
        JsonValue{ .array = JsonArray.init(allocator) }
    else
        return JsonError.RootNotObjectOrArray;

    var node_stack = try std.ArrayList(JsonValue).initCapacity(allocator, 1);
    try node_stack.append(root);

    for (tokens.items[1..]) |token| {
        _ = token;
    }

    return root;
}

/// Print out JSON in human-readable format.
pub fn printJson(json: JsonValue, stream: std.fs.File) !void {
    const writer = stream.writer().any();

    try printJsonHelper(json, "", writer);
}

fn printJsonHelper(json: JsonValue, prefix: []const u8, writer: std.io.AnyWriter) !void {
    var buf: [64]u8 = undefined;
    const new_prefix = try std.fmt.bufPrint(&buf, "  {s}", .{prefix});

    switch (json) {
        .object => |o| {
            try writer.print("{{\n", .{});

            var iter = o.iterator();
            while (iter.next()) |i| {
                try writer.print("\"{s}\": ", .{i.key_ptr.*});
                try printJsonHelper(i.value_ptr.*, new_prefix, writer);
            }

            try writer.print("{s}}}\n", .{prefix});
        },
        .array => |a| {
            try writer.print("[\n", .{});

            for (a.items) |i| {
                try printJsonHelper(i, new_prefix, writer);
            }

            try writer.print("{s}]\n", .{prefix});
        },
        .primitive => |_| {},
    }
}

const JsonError = error{
    UnclosedString,
    NameSeparatorInArray,
    NoNameInObject,
    RootNotObjectOrArray,
};

test "parse example json" {
    const allocator = std.testing.allocator;

    const file = try std.fs.cwd().openFile("example.json", .{});
    defer file.close();

    var json = try parseFile(allocator, file);
    defer json.deinit();

    try printJson(json.root, std.io.getStdErr());
}

test "parse all test files" {
    const allocator = std.testing.allocator;

    const test_dir = try std.fs.cwd().openDir("test", .{ .iterate = true });
    var test_files = test_dir.iterate();

    var total_tests: u32 = 0;
    var incorrect_tests: u32 = 0;

    while (try test_files.next()) |entry| {
        total_tests += 1;
        std.debug.print("file: {s}\n", .{entry.name});

        const should_pass = std.mem.eql(u8, entry.name[0..4], "pass");

        const file = try test_dir.openFile(entry.name, .{});

        const result = parseFile(allocator, file);

        if (should_pass) {
            var good_result = result catch {
                // Failed when should have passed.
                incorrect_tests += 1;
                continue;
            };
            defer good_result.deinit();
        } else {
            var good_result = result catch {
                continue;
            };
            // Passed when should have failed.
            incorrect_tests += 1;
            defer good_result.deinit();
        }
    }

    std.debug.print("Parse all test files results:\n", .{});
    std.debug.print("  correct: {d}/{d}\n", .{ total_tests - incorrect_tests, total_tests });
    try std.testing.expectEqual(0, incorrect_tests);
}
