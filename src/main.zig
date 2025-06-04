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

    var diag = json.JsonDiag{};
    var parsed_json = json.parseFile(allocator, file, &diag) catch |err| {
        try diag.print(std.io.getStdErr());
        return err;
    };
    defer parsed_json.deinit();

    try json.printJson(parsed_json.root, std.io.getStdOut());
}
