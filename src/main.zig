const std = @import("std");
const process = std.process;
const log = std.log;

pub const std_options: std.Options = .{
    .logFn = @import("log.zig").logFn,
};

pub fn main() !void {
    const argv = std.os.argv;

    log.debug("Argc: {}", .{argv.len});

    for (argv) |arg| {
        log.debug("{s}", .{arg});
    }


}

test "fuzz example" {
    const global = struct {
        fn testOne(input: []const u8) anyerror!void {
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(global.testOne, .{});
}
