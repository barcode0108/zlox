const std = @import("std");
const lox = @import("lox.zig");

const log = std.log.scoped(.Chunk);

const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    SetGlobal,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Return,
};

const Self = @This();

const Array = std.ArrayListUnmanaged;
const MArray = std.MultiArrayList;

constants: Array(lox.Value),
codes: MArray(Code),
gpa: Allocator,

pub const Code = struct {
    code: u8,
    line: u32,
};

pub fn init(gpa: Allocator) Self {
    return .{
        .gpa = gpa,
        .codes = MArray(Code){},
        .constants = Array(lox.Value){},
    };
}

pub fn deinit(self: *Self) void {
    self.codes.deinit(self.gpa);
    self.constants.deinit(self.gpa);

    self.* = undefined;
}

pub fn write(self: *Self, byte: u8, line: u32) void {
    self.codes.append(self.gpa, .{
        .code = byte,
        .line = line,
    }) catch {
        @panic("OOM");
    };
}

pub fn addConstant(self: *Self, value: lox.Value) usize {
    self.constants.append(self.gpa, value) catch {
        @panic("OOM");
    };
    return self.constants.items.len - 1;
}

pub fn disassemble(self: *const Self, name: []const u8) void {
    log.debug("== {s} ==", .{name});

    var offset: usize = 0;
    while (offset < self.codes.len) {
        offset = self.disassembleAt(offset);
    }
}

pub fn disassembleAt(self: *const Self, offset: usize) usize {
    var buf: [1024]u8 = undefined;
    var i = bk: {
        const s = std.fmt.bufPrint(buf[0..], "{d:04} ", .{offset}) catch
            unreachable;
        break :bk s.len;
    };

    const slice = self.codes.slice();
    const codes = slice.items(.code);
    const lines = slice.items(.line);

    i += bk: {
        const es = if (offset > 0 and lines[offset] == lines[offset - 1])
            std.fmt.bufPrint(buf[i..], "   | ", .{})
        else
            std.fmt.bufPrint(buf[i..], "{d:4} ", .{lines[offset]});

        if (es) |s| {
            break :bk s.len;
        } else |_| {
            unreachable;
        }
    };

    const instr: OpCode = @enumFromInt(codes[offset]);

    const ret = switch (instr) {
        .Nil,
        .True,
        .False,
        .Pop,
        .Equal,
        .Greater,
        .Less,
        .Add,
        .Subtract,
        .Multiply,
        .Divide,
        .Not,
        .Negate,
        .Print,
        .Return,
        => simpleInstruction(buf[i..], instr, offset),
        .Constant, .SetGlobal, .GetGlobal => constantInstruction(buf[i..], instr, self, offset),
        .SetLocal, .GetLocal => byteInstruction(buf[i..], instr, self, offset),

        else => simpleInstruction(buf[i..], instr, offset),
    };

    log.debug("{s}", .{buf});

    return ret;
}

fn simpleInstruction(buf: []u8, code: OpCode, offset: usize) usize {
    _ = std.fmt.bufPrint(buf, "{s}", .{@tagName(code)}) catch unreachable;
    return offset + 1;
}

fn constantInstruction(buf: []u8, code: OpCode, chunk: *const Self, offset: usize) usize {
    const idx: u8 = chunk.codes.items(.code)[offset + 1];
    std.debug.assert(idx < chunk.constants.items.len);

    _ = std.fmt.bufPrint(buf, "{s:<16} {d:4} '{s}'", .{
        @tagName(code),
        idx,
        chunk.constants.items[idx],
    }) catch unreachable;

    return offset + 2;
}

fn byteInstruction(buf: []u8, code: OpCode, chunk: *const Self, offset: usize) usize {
    const idx: u8 = chunk.codes.items(.code)[offset + 1];

    _ = std.fmt.bufPrint(buf, "{s:<16} {d:4}", .{
        @tagName(code),
        idx,
    }) catch unreachable;

    return offset + 2;
}

test "Chunk" {
    std.testing.log_level = .debug;
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var chunk = Self.init(gpa.allocator());
    defer chunk.deinit();

    chunk.write(@intFromEnum(OpCode.Return), 1);
    chunk.write(@intFromEnum(OpCode.Nil), 1);
    chunk.write(@intFromEnum(OpCode.True), 1);
    const i = chunk.addConstant(lox.Value.from(69));
    chunk.write(@intFromEnum(OpCode.Constant), 2);
    chunk.write(@intCast(i), 2);

    chunk.disassemble("test");
}
