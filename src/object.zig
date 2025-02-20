const std = @import("std");
const meta = std.meta;
const memory = @import("memory.zig");
const log = std.log.scoped(.GC);
const vm = @import("vm.zig");
const lox = @import("lox.zig");
const Chunk = @import("chunk.zig");

const Obj = @This();

pub const Types = union(Tag) {
    string: String,
    function: Function,
    native: Native,
    closure: Closure,
    upvalue: Upvalue,
    class: Class,
};

pub const Tag = enum { string, function, native, closure, upvalue, class };

next: ?*Obj,
tag: Tag,
marked: bool,

pub fn init(tag: Tag, n: ?*Obj) Obj {
    return .{
        .next = n,
        .tag = tag,
        .marked = false,
    };
}

pub fn create(comptime tag: Tag) *meta.TagPayload(Types, tag) {
    const T = meta.TagPayload(Types, tag);
    const p = memory.gc.allocator().create(T) catch @panic("OOM");
    p.obj = Obj.init(tag, memory.gc.objects);

    log.debug("{*} allocate {d} for {s}", .{ p, @sizeOf(T), @tagName(tag) });
    memory.gc.objects = &p.obj;

    return p;
}

pub fn format(
    self: *const Obj,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;

    try writer.print(" .tag = {s}, .mark = {}", .{ @tagName(self.tag), self.marked });
    if (comptime std.mem.eql(u8, fmt, "*")) {
        try writer.print(" .next = {?*}", .{self.next});
    } else {
        try writer.print(" .next = {?}", .{self.next});
    }
}

pub inline fn is(self: *const Obj, comptime tag: Tag) bool {
    return self.tag == tag;
}

pub inline fn as(self: *Obj, comptime tag: Tag) *meta.TagPayload(Types, tag) {
    std.debug.assert(self.is(tag));
    return @fieldParentPtr("obj", self);
}

// std.fmt.formatType  struct
fn formatObj(
    value: anytype,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
    max_depth: usize,
) !void {
    _ = fmt;
    const T = @TypeOf(value);
    const info = @typeInfo(T).@"struct";

    comptime std.debug.assert(@hasField(T, "obj"));

    try writer.writeAll(@typeName(T));
    if (max_depth == 0) {
        return writer.writeAll("{ ... }");
    }
    try writer.writeAll("{");
    inline for (info.fields, 0..) |f, i| {
        if (comptime !std.mem.eql(u8, f.name, "obj")) {
            if (i == 0) {
                try writer.writeAll(" .");
            } else {
                try writer.writeAll(", .");
            }

            try writer.writeAll(f.name);
            try writer.writeAll(" = ");
        }

        const v = @field(value, f.name);
        const FT = @TypeOf(v);
        switch (@typeInfo(FT)) {
            .pointer => |p| if (p.size == .slice and p.child == u8) {
                try writer.writeAll("\"");
                try std.fmt.formatBuf(v, options, writer);
                try writer.writeAll("\"");
                continue;
            },
            else => {},
        }
        try std.fmt.formatType(@field(value, f.name), "any", options, writer, max_depth - 1);
    }
    try writer.writeAll(" }");
}

pub const String = struct {
    obj: Obj,
    str: []const u8,
    hash: u32,

    pub fn deinit(self: *String) void {
        memory.gc.allocator().free(self.str);
    }

    pub fn copy(buf: []const u8) *String {
        const hash = hashString(buf);

        if (vm.strings.findString(buf, hash)) |interned| {
            return interned;
        }

        const tmp = memory.gc.allocator().alloc(u8, buf.len) catch @panic("OOM");
        @memcpy(tmp, buf);
        return String.create(tmp, hash);
    }

    pub fn concat(self: *const String, other: *const String) *String {
        const len = self.str.len + other.str.len;
        const gc = memory.gc.allocator();
        const buf = gc.alloc(u8, len) catch @panic("OOM");

        @memcpy(buf[0..self.str.len], self.str);
        @memcpy(buf[self.str.len..], other.str);

        const hash = hashString(buf);

        if (vm.strings.findString(buf, hash)) |interned| {
            gc.free(buf);
            return interned;
        }

        return String.create(buf, hash);
    }

    fn create(buf: []u8, hash: u32) *String {
        const p: *String = Obj.create(.string);
        p.str = buf;
        p.hash = hash;

        // table does not go through gc so its fine.
        _ = vm.strings.set(p, lox.Value.from(null));

        return p;
    }

    pub fn format(
        self: *const String,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (comptime fmt.len == 0) {
            try formatObj(self.*, fmt, options, writer, 2);
        } else {
            try writer.print("{s}", .{self.str});
        }
    }

    fn hashString(str: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (str) |c| {
            hash ^= c;
            @setRuntimeSafety(false);
            hash *= 16777619;
        }

        return hash;
    }
};

pub const Function = struct {
    obj: Obj,
    name: ?*String,
    arity: u32,

    upvalue_count: u32,
    chunk: Chunk,

    pub fn create(name: ?[]const u8) *Function {
        const p: *Function = Obj.create(.function);

        p.chunk = Chunk.init(memory.gpa);
        p.arity = 0;
        p.name = if (name) |n| lox.String.copy(n) else null;
        p.upvalue_count = 0;

        return p;
    }

    pub fn deinit(self: *Function) void {
        self.chunk.deinit();
    }

    pub fn format(
        self: *const Function,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (comptime fmt.len == 0) {
            try formatObj(self.*, fmt, options, writer, 2);
        } else {
            if (self.name) |name| {
                try writer.print("<fn {s}>", .{name});
            } else {
                try writer.writeAll("<script>");
            }
        }
    }
};

pub const Closure = struct {
    obj: Obj,
    function: *Function,
    upvalues: []*Upvalue,

    pub fn create(func: *Function) *Closure {
        const p: *Closure = Obj.create(.closure);
        p.function = func;
        p.upvalues = memory.gc.allocator().alloc(*Upvalue, func.upvalue_count) catch @panic("OOM");

        return p;
    }

    pub fn deinit(self: *Closure) void {
        memory.gc.allocator().free(self.upvalues);
    }

    pub fn format(
        self: *const Closure,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (comptime fmt.len == 0) {
            try formatObj(self.*, fmt, options, writer, 2);
        } else {
            try writer.print("{s}", .{self.function});
        }
    }
};

pub const Native = struct {
    pub const NativeFn = fn ([]const lox.Value) lox.Value;
    obj: Obj,
    function: *const NativeFn,

    pub fn create(comptime func: NativeFn) *Native {
        const p: *Native = Obj.create(.native);
        p.function = &func;

        return p;
    }

    pub fn format(
        self: *const Native,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (comptime fmt.len == 0) {
            try formatObj(self.*, fmt, options, writer, 2);
        } else {
            try writer.writeAll("<native fn>");
        }
    }
};

pub const Upvalue = struct {
    obj: Obj,
    location: *lox.Value,
    next: ?*Upvalue,
    closed: lox.Value,

    pub fn create(slot: *lox.Value) *Upvalue {
        const p: *Upvalue = Obj.create(.upvalue);
        p.location = slot;
        p.next = null;
        p.closed = lox.Value.nil;
        return p;
    }

    pub fn format(
        self: *const Upvalue,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (comptime fmt.len == 0) {
            try formatObj(self.*, fmt, options, writer, 2);
        } else {
            try writer.writeAll("upvalue");
        }
    }
};

pub const Class = struct {
    obj: Obj,
    name: *String,

    pub fn create(name: *String) *Class {
        const p: *Class = Obj.create(.class);
        p.name = name;
        return p;
    }

    pub fn format(
        self: *const Class,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (comptime fmt.len == 0) {
            try formatObj(self.*, fmt, options, writer, 2);
        } else {
            try writer.writeAll("class");
        }
    }
};

test "Object" {}
