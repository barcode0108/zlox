const std = @import("std");
const testing = std.testing;
const Object = @import("object.zig");

comptime {
    if (@sizeOf(Value) != @sizeOf(f64)) {
        @compileError("something wrong");
    }
}

pub const Value = packed union {
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

    pub fn fromBool(b: bool) Value {
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

    pub fn fromDouble(d: f64) Value {
        return .{
            .Double = d,
        };
    }

    pub fn fromObject(o: *Object) Value {
        var ret = Value{
            .Raw = @intFromPtr(o),
        };

        ret.Packed.nan_bits = q_nan;
        ret.Packed.sign = 1;

        return ret;
    }

    pub const nil: Value = .{
        .Packed = .{
            .sign = 0,
            .nan_bits = q_nan,
            .rest = .{
                .tag = .Nil,
            },
        },
    };

    pub fn from(v: anytype) Value {
        const T = @TypeOf(v);
        return switch (@typeInfo(T)) {
            .float, .comptime_float, .comptime_int => Value.fromDouble(v),
            .bool => Value.fromBool(v),
            .null => Value.nil,
            .pointer => |p| {
                if (p.size != .one) @compileError("size should be one");
                if (comptime p.child == Object) {
                    return Value.fromObject(v);
                }

                inline for (comptime std.meta.tags(Object.Tag)) |tag| {
                    const O = std.meta.TagPayload(Object.Types, tag);
                    if (comptime p.child == O) {
                        return Value.fromObject(&@field(v.*, "obj"));
                    }
                }
                @compileLog(p.child);
                @compileError("Invalid pointer type");
            },
            else => unreachable,
        };
    }

    pub inline fn asDouble(self: Value) f64 {
        std.debug.assert(self.isDouble());
        return self.Double;
    }

    pub inline fn asBool(self: Value) bool {
        std.debug.assert(self.isBool());
        return self.Packed.rest.tag == .True;
    }

    pub inline fn asObject(self: Value) *Object {
        std.debug.assert(self.isObject());
        var r = self;
        r.Packed.nan_bits = 0;
        r.Packed.sign = 0;
        return @ptrFromInt(r.Raw);
    }

    pub inline fn isDouble(self: Value) bool {
        return self.Packed.nan_bits != q_nan;
    }

    pub inline fn isBool(self: Value) bool {
        return self.Packed.nan_bits == q_nan and (self.Packed.rest.tag == .True or self.Packed.rest.tag == .False);
    }

    pub inline fn isNil(self: Value) bool {
        return self.Packed.nan_bits == q_nan and self.Packed.rest.tag == .Nil;
    }

    pub inline fn isFalsey(self: Value) bool {
        return self.isNil() or (self.isBool() and !self.asBool());
    }

    pub inline fn isObject(self: Value) bool {
        return self.Packed.nan_bits == q_nan and self.Packed.sign == 1;
    }

    pub inline fn equal(self: Value, other: Value) bool {
        if (self.isDouble() and other.isDouble()) return self.asDouble() == other.asDouble();
        return self.Raw == other.Raw;
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.isDouble()) {
            try writer.print("{d}", .{self.Double});
        } else if (self.isObject()) {
            const obj = self.asObject();
            switch (obj.tag) {
                inline else => |tag| try writer.print("{s}", .{obj.as(tag)}),
            }
        } else {
            try writer.print("{s}", .{switch (self.Packed.rest.tag) {
                .Nil => "nil",
                .True => "true",
                .False => "false",
            }});
        }
    }
};

test "Value" {
    const t = Value.from(true);
    const f = Value.from(4.2);
    const n = Value.from(null);

    try testing.expect(!t.isDouble());
    try testing.expect(t.isBool());
    try testing.expect(f.isDouble());
    try testing.expect(!f.isBool());
    try testing.expect(!n.isDouble());
    try testing.expect(n.isNil());
}

test "Value Object" {
    const string = Object.String.copy("123");
    const v = Value.from(string);

    const o = v.asObject();
    const s = o.as(.string);

    try testing.expect(string == s);
}
