const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const lox = @import("lox.zig");
const vm = @import("vm.zig");
const Table = @import("table.zig");
const Compiler = @import("compiler.zig").Compiler;
const log = std.log.scoped(.GC);

const Allocator = std.mem.Allocator;

const debug_testing_allocator = @import("debug").testing_allocator;
const debug_stress_gc = @import("debug").stress_gc;

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
    allocated: usize,
    next_gc: usize,
    gray_stack: GrayStack,

    const GrayStack = std.ArrayListUnmanaged(*lox.Object);
    const heap_grow_factor = 2;

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
            .allocated = 0,
            .next_gc = 1024 * 1024,
            .gray_stack = GrayStack{},
        };
    }

    pub fn deinit(self: *GCAllocator) void {
        self.freeObjects();
        self.gray_stack.deinit(self.backing_allocator);

        if (comptime builtin.mode == .Debug) {
            _ = debug_allocator.deinit();
        }
    }

    pub fn collectGarbage(self: *GCAllocator) void {
        log.debug("-- gc begin", .{});
        const before = self.allocated;

        self.markRoots();
        self.traceReferences();
        vm.strings.removeWhite();

        self.sweep();

        self.next_gc = self.allocated * heap_grow_factor;

        log.debug("-- gc end", .{});
        log.debug("   collected {d} bytes (from {d} to {d}) next at {d}", .{
            before - self.allocated,
            before,
            self.allocated,
            self.next_gc,
        });
    }

    fn freeObjects(self: *GCAllocator) void {
        var obj: ?*lox.Object = self.objects;
        while (obj) |object| {
            const next = object.next;
            self.freeObject(object);
            obj = next;
        }
    }

    fn freeObject(self: *GCAllocator, object: *lox.Object) void {
        log.debug("{*} free type {s}", .{ object, @tagName(object.tag) });
        switch (object.tag) {
            inline else => |tag| {
                const p = object.as(tag);
                if (comptime std.meta.hasMethod(@TypeOf(p), "deinit")) {
                    p.deinit();
                }
                self.allocator().destroy(p);
            },
        }
    }

    pub fn markObjects(self: *GCAllocator, object: ?*lox.Object) void {
        const obj = object orelse return;
        if (obj.marked) return;
        log.debug("{*} mark {s}", .{ obj, lox.Value.from(obj) });
        obj.marked = true;

        self.gray_stack.append(self.backing_allocator, obj) catch @panic("OOM");
    }

    fn sweep(self: *GCAllocator) void {
        var prev: ?*lox.Object = null;
        var objects = self.objects;

        while (objects) |object| {
            if (object.marked) {
                object.marked = false;
                prev = object;
                objects = object.next;
            } else {
                const unreached = object;
                objects = object.next;

                if (prev) |p| {
                    p.next = objects;
                } else {
                    self.objects = objects;
                }

                self.freeObject(unreached);
            }
        }
    }

    fn traceReferences(self: *GCAllocator) void {
        while (self.gray_stack.pop()) |object| {
            self.blackenObject(object);
        }
    }

    fn blackenObject(self: *GCAllocator, object: *lox.Object) void {
        log.debug("{*} blacken {s}", .{ object, lox.Value.from(object) });

        switch (object.tag) {
            .closure => {
                const closure: *lox.Closure = object.as(.closure);

                self.markObjects(&closure.function.obj);
                for (closure.upvalues) |upvalue| {
                    self.markObjects(&upvalue.obj);
                }
            },
            .function => {
                const function: *lox.Function = object.as(.function);
                if (function.name) |name| {
                    self.markObjects(&name.obj);
                }
                self.markArray(function.chunk.constants.items);
            },
            .upvalue => self.markValue(object.as(.upvalue).closed),
            .string, .native => {},
        }
    }

    fn markArray(self: *GCAllocator, array: []const lox.Value) void {
        for (array) |value| {
            self.markValue(value);
        }
    }

    fn markCompilerRoots(self: *GCAllocator, compiler: *Compiler) void {
        var compilers: ?*Compiler = compiler;
        while (compilers) |com| : (compilers = com.enclosing) {
            self.markObjects(&com.function.obj);
        }
    }

    fn markRoots(self: *GCAllocator) void {
        const len = vm.stack.len();
        for (vm.stack.items[0..len]) |slot| {
            self.markValue(slot);
        }

        for (vm.frames.buf[0..vm.frames.len]) |*frame| {
            self.markObjects(&frame.closure.obj);
        }

        var upvalues: ?*lox.Upvalue = vm.open_upvalues;
        while (upvalues) |upvalue| : (upvalues = upvalue.next) {
            self.markObjects(&upvalue.obj);
        }

        self.markTable(&vm.globals);
        if (vm.current) |parser| {
            if (parser.current_compiler) |com| {
                self.markCompilerRoots(com);
            }
        }
    }

    fn markTable(self: *GCAllocator, table: *Table) void {
        for (table.entries) |entry| {
            if (entry.key) |k| {
                self.markObjects(&k.obj);
            }
            self.markValue(entry.value);
        }
    }

    fn markValue(self: *GCAllocator, value: lox.Value) void {
        if (value.isObject()) self.markObjects(value.asObject());
    }

    fn alloc(ctx: *anyopaque, n: usize, alignment: mem.Alignment, ra: usize) ?[*]u8 {
        const self: *GCAllocator = @ptrCast(@alignCast(ctx));

        self.allocated += n;

        if (comptime debug_stress_gc) {
            self.collectGarbage();
        }

        if (self.allocated > self.next_gc) {
            self.collectGarbage();
        }

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
        self.allocated -= buf.len;
        return self.backing_allocator.rawFree(buf, alignment, ra);
    }
};
