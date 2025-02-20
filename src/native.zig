const std = @import("std");
const time = std.time;
const lox = @import("lox.zig");
const Value = lox.Value;

pub fn clock(_: []const Value) Value {
    return Value.from((@as(f64, @floatFromInt(time.milliTimestamp())) / time.ms_per_s));
}
