const std = @import("std");
const json = @import("json.zig");

pub fn main() !void {
    var args = std.process.args();

    _ = args.skip();

    const file_name = args.next() orelse {
        std.debug.print("usage: json_parser <filename>\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    var parsed_json = try json.parseFile(allocator, file);
    defer parsed_json.deinit();

    try json.printJson(parsed_json.root);
}
