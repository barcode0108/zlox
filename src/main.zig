const std = @import("std");
const process = std.process;
const log = std.log;

const vm = @import("vm.zig");

pub const std_options: std.Options = .{
    .logFn = @import("log.zig").logFn,
};

pub fn main() !void {
    const argv = std.os.argv;

    log.debug("Argc: {}", .{argv.len});

    vm.init();
    defer vm.deinit();

    switch (argv.len) {
        1 => repl(),
        2 => runFile(std.mem.span(argv[1])),
        else => {
            _ = try std.io.getStdErr().write("Usage: zlox [path]\n");
            process.exit(64);
        },
    }
}

fn repl() void {
    var buf: [1024]u8 = undefined;

    var out = std.io.getStdOut();
    var in = std.io.getStdIn();

    while (true) {
        out.writer().print("> ", .{}) catch unreachable;

        if (in.reader().readUntilDelimiterOrEof(buf[0..], '\n') catch |e| switch (e) {
            error.StreamTooLong => {
                out.writer().print("Too long\n", .{}) catch unreachable;
                in.reader().skipUntilDelimiterOrEof('\n') catch {
                    break;
                };
                continue;
            },
            else => unreachable,
        }) |line| {
            out.writer().print("\n", .{}) catch unreachable;

            vm.interpret(line) catch {};
        } else {
            break;
        }
    }
}

fn runFile(path: []const u8) void {
    _ = path;
}
