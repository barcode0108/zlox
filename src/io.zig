const std = @import("std");
const File = std.fs.File;

pub fn logFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const E = "\x1b[";

    const scope_prefix = E ++ "34m" ++ switch (scope) {
        std.log.default_log_scope => "",
        else => @tagName(scope),
    } ++ E ++ "0m";

    const level_prefix = E ++ "1;" ++ switch (level) {
        .debug => "32m[D]",
        .info => "36m[I]",
        .warn => "33m[W]",
        .err => "31m[E]",
    } ++ E ++ "0m";

    const prefix = comptime level_prefix ++ scope_prefix ++ ": ";

    // Print the message to stderr, silently ignoring any errors
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
}

pub const Writer = File.Writer;
