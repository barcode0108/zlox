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
    GetUpvalue,
    SetUpvalue,
    GetGlobal,
    SetGlobal,
    DefineGlobal,
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
    Loop,
    Call,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    SetProperty,
    GetProperty,
    Method,
    Invoke,
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

pub inline fn writeOp(self: *Self, opcode: OpCode, line: u32) void {
    self.write(@intFromEnum(opcode), line);
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
        inline .Nil,
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
        .CloseUpvalue,
        .Return,
        => |op| simpleInstruction(buf[i..], @tagName(op), offset),
        inline .Constant,
        .SetGlobal,
        .GetGlobal,
        .DefineGlobal,
        .Closure,
        .Class,
        .GetProperty,
        .SetProperty,
        .Method,
        => |op| constantInstruction(buf[i..], @tagName(op), self, offset),
        inline .Call,
        .SetLocal,
        .GetLocal,
        .SetUpvalue,
        .GetUpvalue,
        => |op| byteInstruction(buf[i..], @tagName(op), self, offset),
        inline .JumpIfFalse,
        .Jump,
        .Loop,
        => |op| jumpInstruction(buf[i..], if (op == .Loop) -1 else 1, @tagName(op), codes, offset),
        inline .Invoke => |op| invokeInstruction(buf[i..], @tagName(op), self, offset),
    };

    log.debug("{s}", .{buf});

    if (instr == .Closure) {
        var off = ret;
        const func: *lox.Function = self.constants.items[codes[off - 1]].asObject().as(.function);

        for (0..func.upvalue_count) |_| {
            const is_local = codes[off];
            const index = codes[off + 1];

            off += 2;

            log.debug("{d:04}      |                     {s} {d}", .{
                off - 2,
                if (is_local == 1) "local" else "upvalue",
                index,
            });
        }

        return off;
    }

    return ret;
}

fn simpleInstruction(buf: []u8, comptime name: []const u8, offset: usize) usize {
    _ = std.fmt.bufPrint(buf, "{s}", .{name}) catch unreachable;
    return offset + 1;
}

fn constantInstruction(buf: []u8, comptime name: []const u8, chunk: *const Self, offset: usize) usize {
    const idx: u8 = chunk.codes.items(.code)[offset + 1];
    std.debug.assert(idx < chunk.constants.items.len);

    _ = std.fmt.bufPrint(buf, "{s:<16} {d:4} '{s}'", .{
        name,
        idx,
        chunk.constants.items[idx],
    }) catch unreachable;

    return offset + 2;
}

fn byteInstruction(buf: []u8, comptime name: []const u8, chunk: *const Self, offset: usize) usize {
    const idx: u8 = chunk.codes.items(.code)[offset + 1];

    _ = std.fmt.bufPrint(buf, "{s:<16} {d:4}", .{
        name,
        idx,
    }) catch unreachable;

    return offset + 2;
}

fn jumpInstruction(buf: []u8, sign: comptime_int, comptime name: []const u8, codes: []const u8, offset: usize) usize {
    const jump: u16 = @as(u16, codes[offset + 1]) << 8 | codes[offset + 2];
    const dist: i32 = @as(i32, @intCast(offset)) + @as(i32, jump) * sign;

    _ = std.fmt.bufPrint(buf, "{s:<16} {d:4} -> {d}", .{ name, offset, 3 + dist }) catch unreachable;

    return offset + 3;
}

fn invokeInstruction(buf: []u8, comptime name: []const u8, chunk: *const Self, offset: usize) usize {
    const codes = chunk.codes.items(.code);
    const constant = codes[offset + 1];
    const arg_count = codes[offset + 2];

    _ = std.fmt.bufPrint(buf, "{s:<16} ({d} args) {d:4} '{s}'", .{
        name,
        arg_count,
        constant,
        chunk.constants.items[constant],
    }) catch unreachable;

    return offset + 3;
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
