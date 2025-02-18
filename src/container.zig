const std = @import("std");

pub fn Array(comptime T: type, comptime size: usize) type {
    return struct {
        buf: [size]T,
        len: usize,

        const Self = @This();

        pub fn init() Self {
            return .{
                .buf = undefined,
                .len = 0,
            };
        }

        pub fn full(self: *const Self) bool {
            return self.len == self.buf.len;
        }

        pub fn append(self: *Self, v: T) void {
            self.buf[self.len] = v;
            self.len += 1;
        }

        pub fn backPtr(self: *Self) *T {
            return &self.buf[self.len - 1];
        }

        pub fn addOne(self: *Self) *T {
            self.len += 1;
            return self.backPtr();
        }
    };
}

pub fn Stack(comptime T: type, comptime size: usize) type {
    return struct {
        items: [size]T,
        top: [*]T,

        const Self = @This();

        pub fn init() Self {
            var ret = Self{
                .items = undefined,
                .top = undefined,
            };

            ret.top = &ret.items;
            return ret;
        }

        pub inline fn reset(self: *Self) void {
            self.top = &self.items;
        }

        pub inline fn len(self: *const Self) usize {
            return self.top - &self.items;
        }

        pub fn push(self: *Self, v: T) void {
            std.debug.assert(self.len() < self.items.len);
            self.top[0] = v;
            self.top += 1;
        }

        pub fn pop(self: *Self) T {
            std.debug.assert(self.len() != 0);
            self.top -= 1;
            return self.top[0];
        }

        pub fn peek(self: *const Self, d: usize) T {
            const tmp = self.top - (d + 1);
            return tmp[0];
        }
    };
}
