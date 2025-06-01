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

pub const JsonParseResult = struct {
    root: JsonValue,
    aa: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *JsonParseResult) void {
        self.aa.deinit();
    }
};

pub fn parseFile(allocator: std.mem.Allocator, file: std.fs.File) JsonError!JsonParseResult {
    var aa = std.heap.ArenaAllocator.init(allocator);
    // If the parse fails, the aa (and all allocated memory with it).
    // If the parse succeeds, the aa deinit is in the hands of the caller.
    errdefer aa.deinit();
    const aa_allocator = aa.allocator();

    const tokens = tokenize(aa_allocator, allocator, file);
    _ = tokens;

    const root = JsonObject.init(aa_allocator);

    return JsonParseResult{
        .root = JsonValue{ .object = root },
        .aa = aa,
        .allocator = aa_allocator,
    };
}

/// allocator is used for the final strings.
/// scratch_allocator is used to generate an ArenaAllocator for all the objects that do not need to persist after this function.
fn tokenize(aa: std.heap.ArenaAllocator, scratch_allocator: std.mem.Allocator, file: std.fs.File) !std.ArrayList([]const u8) {
    var scratch_aa = std.heap.ArenaAllocator(scratch_allocator);
    defer scratch_aa.deinit();
    const scratch_aa_allocator = scratch_aa.allocator();

    var br = std.io.bufferedReader(file.reader());
    const reader = br.reader();

    _ = aa;
    _ = reader;
    _ = scratch_aa_allocator;
}

pub const JsonError = error{};

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
            _ = std.testing.expectError(JsonError, result);
        }
    }
}
