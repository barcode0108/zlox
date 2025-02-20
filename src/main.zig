const std = @import("std");
const builtin = @import("builtin");
const fs = std.fs;
const process = std.process;
const log = std.log;

const memory = @import("memory.zig");

const vm = @import("vm.zig");

pub const std_options: std.Options = .{
    .logFn = @import("io.zig").logFn,
};

pub fn main() !void {
    const argv = std.os.argv;

    log.debug("Argc: {}", .{argv.len});

    vm.init(std.io.getStdOut().writer(), std.io.getStdErr().writer());
    defer vm.deinit();

    switch (argv.len) {
        1 => try repl(),
        2 => try runFile(std.mem.span(argv[1])),
        else => {
            try std.io.getStdErr().writeAll("Usage: zlox [path]\n");
            process.exit(64);
        },
    }
}

fn repl() !void {
    var buf: [1024]u8 = undefined;

    var out = std.io.getStdOut();
    var in = std.io.getStdIn();

    while (true) {
        try out.writer().print("> ", .{});

        if (in.reader().readUntilDelimiterOrEof(buf[0..], '\n') catch |e| switch (e) {
            error.StreamTooLong => {
                try out.writer().print("Too long\n", .{});
                try in.reader().skipUntilDelimiterOrEof('\n');
                continue;
            },
            else => return e,
        }) |line| {
            try out.writer().print("\n", .{});
            vm.interpret(line) catch {};
        } else {
            break;
        }
    }
}

fn runFile(path: []const u8) !void {
    const source = try readFile(path);
    defer memory.gpa.free(source);

    vm.interpret(source) catch |e| switch (e) {
        error.CompileError => process.exit(65),
        error.RuntimeError => process.exit(70),
    };
}

fn readFile(path: []const u8) ![]u8 {
    var err = std.io.getStdErr();
    const file = fs.cwd().openFile(path, .{}) catch {
        try err.writer().print("Could not open file \"{s}\".\n", .{path});
        process.exit(74);
    };

    defer file.close();

    const stat = try file.stat(); // TODO: handle error

    const buf = memory.gpa.alloc(u8, stat.size) catch {
        try err.writer().print("Not enough memory to read \"{s}\".\n", .{path});
        process.exit(74);
    };

    _ = file.readAll(buf) catch {
        try err.writer().print("Could not read file \"{s}\".\n", .{path});
        process.exit(74);
    };

    return buf;
}
