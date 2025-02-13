const std = @import("std");

const Chunk = @import("chunk.zig");
const OpCode = Chunk.OpCode;
const compiler = @import("compiler.zig");
const memory = @import("memory.zig");
const lox = @import("lox.zig");
const log = std.log.scoped(.VM);

const debug_trace_execution = @import("debug").trace_execution;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const stack_max = 256;

var chunk: ?*Chunk = null;
var ip: ?[*]const u8 = null;

var stack: [stack_max]lox.Value = undefined;
var stack_top: [*]lox.Value = &stack;

inline fn resetStack() void {
    stack_top = &stack;
}

pub fn push(v: lox.Value) void {
    std.debug.assert(stack_top - &stack < stack_max);
    stack_top[0] = v;
    stack_top += 1;
}

pub fn pop() lox.Value {
    std.debug.assert(stack_top != &stack);
    stack_top -= 1;
    return stack_top[0];
}

pub fn peek(d: usize) lox.Value {
    const tmp = stack_top - (d + 1);
    return tmp[0];
}

pub fn init() void {
    resetStack();
}

pub fn deinit() void {}

pub fn interpret(source: []const u8) InterpretError!void {
    var chk = Chunk.init(memory.gpa.allocator());
    defer chk.deinit();

    compiler.compile(source, &chk) catch {
        return InterpretError.CompileError;
    };

    chunk = &chk;
    ip = chunk.?.codes.items(.code).ptr;

    try run();
}

fn runtimeError(comptime fmt: []const u8, args: anytype) void {
    var out = std.io.getStdErr();
    out.writer().print(fmt ++ "\n", args) catch return;
}

inline fn readByte() u8 {
    if (ip) |*p| {
        const r = p.*[0];
        p.* += 1;
        return r;
    } else {
        unreachable;
    }
}

inline fn readConstant() lox.Value {
    if (chunk) |chk| {
        return chk.constants.items[readByte()];
    } else {
        unreachable;
    }
}

const Op = enum { add, sub, gt, lt, mul, div };

inline fn binaryOp(comptime op: Op) InterpretError!void {
    if (!peek(0).isDouble() or !peek(1).isDouble()) {
        runtimeError("Operands must be numbers.", .{});
        return InterpretError.RuntimeError;
    }

    const b = pop().asDouble();
    const a = pop().asDouble();

    push(lox.Value.from(switch (op) {
        .add => a + b,
        .sub => a - b,
        .gt => a > b,
        .lt => a < b,
        .div => a / b,
        .mul => a * b,
    }));
}

fn run() InterpretError!void {
    const out = std.io.getStdOut();

    while (true) {
        if (comptime debug_trace_execution) {
            var buf: [1024]u8 = .{' '} ** 1024;

            const len = stack_top - &stack;
            var i: usize = 10;
            for (stack[0..len]) |slot| {
                i += (std.fmt.bufPrint(buf[i..], "[ {s} ]", .{slot}) catch unreachable).len;
            }
            log.debug("{s}", .{buf[0..i]});

            const offset = ip.? - chunk.?.codes.items(.code).ptr;
            _ = chunk.?.disassembleAt(offset);
        }

        const instr: OpCode = @enumFromInt(readByte());

        switch (instr) {
            .Constant => {
                const v = readConstant();
                push(v);
            },
            .Nil => push(lox.Value.from(null)),
            .True => push(lox.Value.from(true)),
            .False => push(lox.Value.from(false)),
            .Pop => _ = pop(),
            .Equal => {
                const b = pop();
                const a = pop();
                push(lox.Value.from(a.equal(b)));
            },

            .Greater => try binaryOp(.gt),
            .Less => try binaryOp(.lt),
            .Add => try binaryOp(.add),
            .Subtract => try binaryOp(.sub),
            .Multiply => try binaryOp(.mul),
            .Divide => try binaryOp(.div),

            .Not => push(lox.Value.from(pop().isFalsey())),
            .Negate => {
                if (!peek(0).isDouble()) {
                    runtimeError("Operand must be a number.", .{});
                    return InterpretError.RuntimeError;
                }

                push(lox.Value.from(-pop().asDouble()));
            },

            .Print => {
                const v = pop();
                out.writer().print("{s}\n", .{v}) catch {};
            },
            .Return => return,
            else => {},
        }
    }
}
