const std = @import("std");
const testing = std.testing;

comptime {
    if (@sizeOf(Value) != @sizeOf(f64)) {
        @compileError("something wrong");
    }
}

pub const Value = packed union {
    const Self = @This();

    Raw: u64,
    Double: f64,
    Packed: packed struct {
        rest: packed struct {
            tag: Tag,
            _: u48 = 0,
        },
        nan_bits: u13,
        sign: u1, // sign bit
    },

    const q_nan = std.math.maxInt(u13);
    const Tag = enum(u2) {
        Nil = 0b01,
        False = 0b10,
        True = 0b11,
    };

    pub fn fromBool(b: bool) Self {
        return .{
            .Packed = .{
                .sign = 0,
                .nan_bits = q_nan,
                .rest = .{
                    .tag = if (b) .True else .False,
                },
            },
        };
    }

    pub fn fromDouble(d: f64) Self {
        return .{
            .Double = d,
        };
    }

    pub const nil: Self = .{
        .Packed = .{
            .sign = 0,
            .nan_bits = q_nan,
            .rest = .{
                .tag = .Nil,
            },
        },
    };

    pub fn from(v: anytype) Self {
        const T = @TypeOf(v);
        return switch (@typeInfo(T)) {
            .float, .comptime_float, .comptime_int => Self.fromDouble(v),
            .bool => Self.fromBool(v),
            .null => Self.nil,
            else => unreachable,
        };
    }

    pub fn asDouble(self: Self) f64 {
        std.debug.assert(self.isDouble());
        return self.Double;
    }

    pub fn asBool(self: Self) bool {
        std.debug.assert(self.isBool());
        return self.Packed.rest.tag == .True;
    }

    pub fn isDouble(self: Self) bool {
        return self.Packed.nan_bits != q_nan;
    }

    pub fn isBool(self: Self) bool {
        return self.Packed.nan_bits == q_nan and (self.Packed.rest.tag == .True or self.Packed.rest.tag == .False);
    }

    pub fn isNil(self: Self) bool {
        return self.Packed.nan_bits == q_nan and self.Packed.rest.tag == .Nil;
    }

    pub fn isFalsey(self: Self) bool {
        return self.isNil() or (self.isBool() and !self.asBool());
    }

    pub fn equal(self: Self, other: Self) bool {
        if (self.isDouble() and other.isDouble()) return self.asDouble() == other.asDouble();
        return self.Raw == other.Raw;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.isDouble()) {
            try writer.print("{d}", .{self.Double});
            return;
        }

        try writer.print("{s}", .{switch (self.Packed.rest.tag) {
            .Nil => "nil",
            .True => "true",
            .False => "false",
        }});
    }
};

test "Value" {
    const t = Value.from(true);
    const f = Value.from(4.2);
    const n = Value.from(null);

    std.debug.print("{s}\n", .{f});
    std.debug.print("{s}\n", .{t});
    std.debug.print("{s}\n", .{n});

    try testing.expect(!t.isDouble());
    try testing.expect(t.isBool());
    try testing.expect(f.isDouble());
    try testing.expect(!f.isBool());
    try testing.expect(!n.isDouble());
    try testing.expect(n.isNil());
}
