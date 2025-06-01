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
        std.debug.print("token: {s}\n", .{item});
    }

    // TODO: This is temporary to make the compiler happy.
    const root = JsonObject.init(aa_allocator);

    return Json{
        .root = JsonValue{ .object = root },
        .aa = aa,
        .allocator = aa_allocator,
    };
}

fn tokenize(allocator: std.mem.Allocator, file: std.fs.File) !std.ArrayList([]u8) {
    var br = std.io.bufferedReader(file.reader());
    const reader = br.reader();

    var tokens = std.ArrayList([]u8).init(allocator);

    // Token stored as ArrayList so that building is efficient.
    var current_token = std.ArrayList(u8).init(allocator);
    defer current_token.deinit();

    var in_string = false;
    var b_last: u8 = 0;
    var b = try reader.readByte();
    while (b > 0) : ({
        b_last = b;
        b = reader.readByte() catch break;
    }) {
        // Start of string
        if (!in_string and b == '"') {
            in_string = true;

            try advanceToken(allocator, &current_token, &tokens);

            try current_token.append(b);
            continue;
        }

        // End of string
        if (in_string and b == '"' and b_last != '\\') {
            in_string = false;

            try current_token.append(b);

            try advanceToken(allocator, &current_token, &tokens);
            continue;
        }

        // Inside of string
        if (in_string) {
            try current_token.append(b);
            continue;
        }

        // If whitespace (and not in string), advance token.
        // Whitespace is not stored in the token list.
        if (std.ascii.isWhitespace(b)) {
            try advanceToken(allocator, &current_token, &tokens);
            continue;
        }

        // If we encounter a symbol (outside strings),
        // advance token, store this symbol in new token, and end that token.
        if (isJsonSymbol(b)) {
            try advanceToken(allocator, &current_token, &tokens);
            try current_token.append(b);
            try advanceToken(allocator, &current_token, &tokens);
        }

        // At this stage, the token must be a non-string primitive.
        try current_token.append(b);
    }

    return tokens;
}

/// Helper function for tokenize() for storing the current token and setting up for the next.
fn advanceToken(allocator: std.mem.Allocator, token_builder: *std.ArrayList(u8), tokens: *std.ArrayList([]u8)) !void {
    if (token_builder.items.len == 0) {
        return;
    }

    const token = try finalizeToken(allocator, token_builder);
    try tokens.append(token);
    token_builder.clearAndFree();
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
fn finalizeToken(allocator: std.mem.Allocator, token_builder: *std.ArrayList(u8)) ![]u8 {
    var token = try allocator.alloc(u8, token_builder.items.len);
    @memcpy(token[0..], token_builder.items);

    return token;
}

/// Print out JSON in human-readable format.
pub fn printJson(json: JsonValue) !void {
    const stdout = std.io.getStdOut().writer().any();

    try printJsonHelper(json, "", stdout);
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
            try writer.print("{{\n", .{});

            for (a.items) |i| {
                try printJsonHelper(i, new_prefix, writer);
            }

            try writer.print("{s}}}\n", .{prefix});
        },
        .primitive => |_| {},
    }
}

test "parse all test files" {
    const allocator = std.testing.allocator;

    const test_dir = try std.fs.cwd().openDir("test", .{ .iterate = true });
    var test_files = test_dir.iterate();

    while (try test_files.next()) |entry| {
        std.debug.print("file: {s}\n", .{entry.name});

        const should_pass = std.mem.eql(u8, entry.name[0..4], "pass");

        const file = try test_dir.openFile(entry.name, .{});

        const result = parseFile(allocator, file);

        if (should_pass) {
            var json_result = try result;
            defer json_result.deinit();
        } else {
            _ = std.testing.expectError(anyerror, result);
        }
    }
}
