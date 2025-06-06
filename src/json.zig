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

pub fn parseFile(allocator: std.mem.Allocator, file: std.fs.File, diag: *JsonDiag) !Json {
    var aa = std.heap.ArenaAllocator.init(allocator);
    // If the parse fails, the aa (and all allocated memory with it).
    // If the parse succeeds, the aa deinit is in the hands of the caller.
    errdefer aa.deinit();
    const aa_allocator = aa.allocator();

    // Tokenize file into a more useful form.
    const tokens = try tokenize(aa_allocator, file, diag);

    const root = try parser(aa_allocator, tokens, diag);

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

fn tokenize(allocator: std.mem.Allocator, file: std.fs.File, diag: *JsonDiag) !std.ArrayList(Token) {
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
        // Start of string
        if (!state.in_string and b == '"') {
            state.in_string = true;

            try advanceToken(allocator, &current_token, &tokens, &state);

            try addToTokenBuilder(&current_token, b, &state);
            continue;
        }

        // End of string
        if (state.in_string and b == '"' and !isQuoteEscaped(current_token)) {
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
        diag.line = state.token_start_line;
        diag.column = state.token_start_column;

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

/// Helper function for tokenize() to check if a quote in a string is escaped or not.
fn isQuoteEscaped(token: std.ArrayList(u8)) bool {
    // Count number of contiguous backslashes at the end of the string.
    // If the number is odd, the quote will be escaped.
    // If even, it is not escaped.

    var backslash_count: u32 = 0;

    var i = token.items.len - 1;
    while (i > 0) : (i -= 1) {
        if (token.items[i] == '\\')
            backslash_count += 1
        else
            break;
    }

    const isEscaped = backslash_count % 2 == 1;

    return isEscaped;
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

fn parser(allocator: std.mem.Allocator, tokens: std.ArrayList(Token), diag: *JsonDiag) !JsonValue {
    var root: JsonValue = if ((try parserGetToken(tokens, 0, diag)).token_type == Token.TokenType.begin_object)
        JsonValue{ .object = JsonObject.init(allocator) }
    else if ((try parserGetToken(tokens, 0, diag)).token_type == Token.TokenType.begin_array)
        JsonValue{ .array = JsonArray.init(allocator) }
    else {
        diag.line = (try parserGetToken(tokens, 0, diag)).line;
        diag.column = (try parserGetToken(tokens, 0, diag)).column;

        return JsonError.RootNotObjectOrArray;
    };

    var node_stack = try std.ArrayList(struct { *JsonValue, Token }).initCapacity(allocator, 1);
    try node_stack.append(.{ &root, try parserGetToken(tokens, 0, diag) });

    // new_scope track whether a new array or object has started.
    // Used to determine if a value_separator should be expected.
    var new_scope = true;

    var curr: usize = 1;

    while (curr < tokens.items.len and node_stack.items.len > 0) : (curr += 1) {
        if (!new_scope) {
            const curr_token = try parserGetToken(tokens, curr, diag);
            // If we're not entering a new scope we must see one of three things:
            // value_separator, end_object, or end_array.
            if (curr_token.token_type == .value_separator) {
                // Skip over the value_separator token.
                curr += 1;
            } else if (curr_token.token_type == .end_array or curr_token.token_type == .end_object) {
                // This is okay. We don't need to skip them.
            } else {
                // This is not okay, a value_separator was likely omitted.
                diag.line = curr_token.line;
                diag.column = curr_token.column;

                return JsonError.NoValueSeparator;
            }
        }

        new_scope = switch (node_stack.items[node_stack.items.len - 1][0].*) {
            .array => |*a| try parseWithinArray(allocator, a, tokens, &curr, &node_stack, diag),
            .object => |*o| try parseWithinObject(allocator, o, tokens, &curr, &node_stack, diag),
            .primitive => unreachable,
        };
    }

    // There should be no tokens after the node_stack empties.
    if (curr < tokens.items.len) {
        const violating_token = try parserGetToken(tokens, curr, diag);
        diag.line = violating_token.line;
        diag.column = violating_token.column;
        return JsonError.TokensAfterRootClose;
    }

    // There should be no node_stack entries after the tokens run out
    if (node_stack.items.len > 0) {
        const last_unclosed_container = node_stack.items[node_stack.items.len - 1][1];
        diag.line = last_unclosed_container.line;
        diag.column = last_unclosed_container.column;
        return JsonError.UnclosedContainer;
    }

    return root;
}

/// Helper function for parser().
/// Wraps accessing tokens with error handling in case of EOF.
fn parserGetToken(tokens: std.ArrayList(Token), index: usize, diag: *JsonDiag) !Token {
    if (index >= tokens.items.len) {
        const final_token = tokens.items[tokens.items.len - 1];

        diag.line = final_token.line;
        diag.column = final_token.column + @as(u32, @intCast(final_token.value.len));

        return JsonError.UnexpectedEOF;
    }

    return tokens.items[index];
}

/// Helper function for parser() to handle parsing tokens inside arrays.
fn parseWithinArray(allocator: std.mem.Allocator, array: *JsonArray, tokens: std.ArrayList(Token), curr: *usize, node_stack: *std.ArrayList(struct { *JsonValue, Token }), diag: *JsonDiag) !bool {
    const curr_token = try parserGetToken(tokens, curr.*, diag);

    if (curr_token.token_type == .end_array) {
        // Check if the last token was a value_separator. This is not allowed.
        if ((try parserGetToken(tokens, curr.* - 1, diag)).token_type == .value_separator) {
            diag.line = curr_token.line;
            diag.column = curr_token.column;
            return JsonError.TrailingValueSeparator;
        }

        _ = node_stack.pop();
        return false;
    }

    if (curr_token.token_type == .end_object) {
        diag.line = curr_token.line;
        diag.column = curr_token.column;
        return JsonError.EndObjectInArray;
    }

    if (curr_token.token_type == .name_separator) {
        diag.line = curr_token.line;
        diag.column = curr_token.column;
        return JsonError.NameSeparatorInArray;
    }

    if (curr_token.token_type == .value_separator) {
        diag.line = curr_token.line;
        diag.column = curr_token.column;
        return JsonError.InvalidToken;
    }

    const value = try parseToken(allocator, curr_token, diag);

    try array.append(value);

    // If this was an object or array, we need to add it to the stack, and return true.
    switch (value) {
        .object, .array => {
            try node_stack.append(.{ &array.items[array.items.len - 1], curr_token });
            return true;
        },
        else => return false,
    }
}

/// Helper function for parser() to handle parsing tokens inside objects.
fn parseWithinObject(allocator: std.mem.Allocator, object: *JsonObject, tokens: std.ArrayList(Token), curr: *usize, node_stack: *std.ArrayList(struct { *JsonValue, Token }), diag: *JsonDiag) !bool {
    const curr_token = try parserGetToken(tokens, curr.*, diag);

    if (curr_token.token_type == .end_object) {
        // Check if the last token was a value_separator. This is not allowed.
        if ((try parserGetToken(tokens, curr.* - 1, diag)).token_type == .value_separator) {
            diag.line = curr_token.line;
            diag.column = curr_token.column;
            return JsonError.TrailingValueSeparator;
        }
        _ = node_stack.pop();
        return false;
    }

    if (curr_token.token_type == .end_array) {
        diag.line = curr_token.line;
        diag.column = curr_token.column;
        return JsonError.EndArrayInObject;
    }

    if (curr_token.token_type == .value_separator) {
        diag.line = curr_token.line;
        diag.column = curr_token.column;
        return JsonError.InvalidToken;
    }

    if (curr_token.token_type == .string_literal and (try parserGetToken(tokens, curr.* + 1, diag)).token_type == .name_separator) {
        const name_token = curr_token;
        const name = name_token.value[1 .. name_token.value.len - 1];

        const value_token = try parserGetToken(tokens, curr.* + 2, diag);
        const value = try parseToken(allocator, value_token, diag);

        try object.put(name, value);

        curr.* += 2;

        // If this was an object or array, we need to add it to the stack, and return true.
        switch (value) {
            .object, .array => {
                try node_stack.append(.{ object.getPtr(name).?, value_token });
                return true;
            },
            else => return false,
        }
    } else {
        diag.line = curr_token.line;
        diag.column = curr_token.column;

        return JsonError.InvalidToken;
    }
}

/// Helper function for parser() to parse Tokens into JsonValue.
fn parseToken(allocator: std.mem.Allocator, token: Token, diag: *JsonDiag) !JsonValue {
    switch (token.token_type) {
        .begin_object => return JsonValue{ .object = JsonObject.init(allocator) },
        .begin_array => return JsonValue{ .array = JsonArray.init(allocator) },
        .string_literal, .bool_literal, .null_literal, .number_literal => return try parseLiteral(token, diag),
        else => {
            diag.line = token.line;
            diag.column = token.column;
            return JsonError.InvalidToken;
        },
    }
}

/// Helper function for parser() to parse literals.
fn parseLiteral(token: Token, diag: *JsonDiag) !JsonValue {
    return JsonValue{ .primitive = switch (token.token_type) {
        .null_literal => JsonPrimitive.null,
        .string_literal => JsonPrimitive{ .string = token.value[1 .. token.value.len - 1] },
        .bool_literal => try parseBoolLiteral(token, diag),
        .number_literal => try parseNumberLiteral(token, diag),
        else => unreachable,
    } };
}

/// Helper function for parseLiteral() to parse null literals.
fn parseNullLiteral(token: Token, diag: *JsonDiag) !JsonPrimitive {
    if (token.value.len == 4 and std.mem.eql(u8, token.value, "null")) {
        return JsonPrimitive.null;
    } else {
        diag.line = token.line;
        diag.column = token.column;
        return JsonError.InvalidValue;
    }
}

/// Helper function for parseLiteral() to parse boolean literals.
fn parseBoolLiteral(token: Token, diag: *JsonDiag) !JsonPrimitive {
    if (token.value.len == 4 and std.mem.eql(u8, token.value, "true")) {
        return JsonPrimitive{ .boolean = true };
    } else if (token.value.len == 5 and std.mem.eql(u8, token.value, "false")) {
        return JsonPrimitive{ .boolean = false };
    } else {
        diag.line = token.line;
        diag.column = token.column;
        return JsonError.InvalidValue;
    }
}

/// Helper function for parseLiteral()
/// There are a couple edge cases that have to be handled when parsing a number.
fn parseNumberLiteral(token: Token, diag: *JsonDiag) !JsonPrimitive {
    // Numbers are not allowed leading zeros.
    // The exceptions being that zero is the only number: "0",
    // or that it is followed by a point: "0.2".
    // This also stops hexademical values from parsing.
    if (token.value.len >= 2 and token.value[0] == '0' and token.value[1] != '.') {
        diag.line = token.line;
        diag.column = token.column;
        return JsonError.InvalidNumberLiteral;
    }

    return JsonPrimitive{ .number = std.fmt.parseFloat(f64, token.value) catch {
        diag.line = token.line;
        diag.column = token.column;
        return JsonError.InvalidValue;
    } };
}

/// Print out JSON in human-readable format.
pub fn printJson(json: JsonValue, stream: std.fs.File) !void {
    const writer = stream.writer().any();

    try printJsonHelper(json, "", true, writer);
}

fn printJsonHelper(json: JsonValue, prefix: []const u8, use_prefix: bool, writer: std.io.AnyWriter) !void {
    var buf: [64]u8 = undefined;
    const new_prefix = try std.fmt.bufPrint(&buf, "  {s}", .{prefix});
    const effective_prefix = if (use_prefix) prefix else "";

    switch (json) {
        .object => |o| {
            try writer.print("{s}{{\n", .{effective_prefix});

            var iter = o.iterator();
            while (iter.next()) |v| {
                try writer.print("{s}{s}: ", .{ new_prefix, v.key_ptr.* });
                try printJsonHelper(v.value_ptr.*, new_prefix, false, writer);
            }

            try writer.print("{s}}}\n", .{prefix});
        },
        .array => |a| {
            try writer.print("{s}[\n", .{effective_prefix});

            for (a.items) |v| {
                try printJsonHelper(v, new_prefix, true, writer);
            }

            try writer.print("{s}]\n", .{prefix});
        },
        .primitive => |p| {
            switch (p) {
                .string => |s| try writer.print("{s}{s}\n", .{ effective_prefix, s }),
                .null => try writer.print("{s}null\n", .{effective_prefix}),
                .boolean => |b| if (b) try writer.print("{s}true\n", .{effective_prefix}) else try writer.print("{s}false\n", .{effective_prefix}),
                .number => |f| try writer.print("{s}{d}\n", .{ effective_prefix, f }),
            }
        },
    }
}

const JsonError = error{
    UnclosedString,
    NameSeparatorInArray,
    NoNameInObject,
    RootNotObjectOrArray,
    EndObjectInArray,
    EndArrayInObject,
    InvalidValue,
    InvalidToken,
    NoValueSeparator,
    TrailingValueSeparator,
    UnexpectedEOF,
    TokensAfterRootClose,
    UnclosedContainer,
    InvalidNumberLiteral,
};

/// Small struct to provide context in the event of an error.
pub const JsonDiag = struct {
    line: ?u32 = null,
    column: ?u32 = null,

    pub fn print(self: JsonDiag, stream: std.fs.File) !void {
        const writer = stream.writer();

        if (self.line) |l| {
            try writer.print("Error occured on line {d}", .{l});
            if (self.column) |c| {
                try writer.print(", column {d}\n", .{c});
            } else {
                try writer.print("\n", .{});
            }
        }
    }
};

test "parse all test files" {
    const allocator = std.testing.allocator;

    const test_dir = try std.fs.cwd().openDir("test", .{ .iterate = true });
    var test_files = test_dir.iterate();

    var total_tests: u32 = 0;
    var false_positive: u32 = 0;
    var false_negative: u32 = 0;

    var diag = JsonDiag{};

    while (try test_files.next()) |entry| {
        total_tests += 1;

        const should_pass = std.mem.eql(u8, entry.name[0..4], "pass");

        const file = try test_dir.openFile(entry.name, .{});

        diag = JsonDiag{};
        const result = parseFile(allocator, file, &diag);

        if (should_pass) {
            var good_result = result catch {
                // Failed when should have passed.
                try diag.print(std.io.getStdErr());
                false_negative += 1;
                continue;
            };
            defer good_result.deinit();
        } else {
            var good_result = result catch {
                continue;
            };
            // Passed when should have failed.
            false_positive += 1;
            defer good_result.deinit();
        }
    }

    std.debug.print("Parse all test files results:\n", .{});
    const incorrect_tests = false_positive + false_negative;
    std.debug.print("  correct: {d}/{d}\n", .{ total_tests - incorrect_tests, total_tests });
    std.debug.print("  false positives: {d}\n", .{false_positive});
    std.debug.print("  false negatives: {d}\n", .{false_negative});
    try std.testing.expectEqual(0, incorrect_tests);
}
