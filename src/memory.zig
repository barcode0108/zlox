const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const lox = @import("lox.zig");
const log = std.log.scoped(.GC);

const debug_testing_allocator = @import("debug").testing_allocator;

var debug_allocator = std.heap.GeneralPurposeAllocator(.{}).init;
const default_allocator = init: {
    if (debug_testing_allocator) {
        break :init std.testing.allocator;
    }

    break :init switch (builtin.mode) {
        .Debug, .ReleaseSafe => debug_allocator.allocator(),
        .ReleaseFast, .ReleaseSmall => std.heap.c_allocator,
    };
};

pub var gpa = default_allocator;
pub var gc = GCAllocator.init(default_allocator);

const GCAllocator = struct {
    backing_allocator: Allocator,
    objects: ?*lox.Object,

    pub fn allocator(self: *GCAllocator) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    pub fn init(backing: Allocator) GCAllocator {
        return .{
            .backing_allocator = backing,
            .objects = null,
        };
    }

    pub fn freeObjects(self: *GCAllocator) void {
        var obj: ?*lox.Object = self.objects;
        while (obj) |object| {
            log.debug("free obj: {*} {s}", .{ object, @tagName(object.tag) });
            const next = object.next;
            switch (object.tag) {
                inline else => |tag| {
                    const p = object.as(tag);
                    if (comptime std.meta.hasMethod(@TypeOf(p), "deinit")) {
                        p.deinit();
                    }
                    self.allocator().destroy(p);
                },
            }

            obj = next;
        }
    }

    fn alloc(ctx: *anyopaque, n: usize, alignment: mem.Alignment, ra: usize) ?[*]u8 {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        return self.backing_allocator.rawAlloc(n, alignment, ra);
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        alignment: mem.Alignment,
        new_len: usize,
        ra: usize,
    ) bool {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        return self.backing_allocator.rawResize(buf, alignment, new_len, ra);
    }

    fn remap(
        ctx: *anyopaque,
        memory: []u8,
        alignment: mem.Alignment,
        new_len: usize,
        ra: usize,
    ) ?[*]u8 {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        return self.backing_allocator.rawRemap(memory, alignment, new_len, ra);
    }

    fn free(ctx: *anyopaque, buf: []u8, alignment: mem.Alignment, ra: usize) void {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));
        return self.backing_allocator.rawFree(buf, alignment, ra);
    }
};
