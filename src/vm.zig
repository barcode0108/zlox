const std = @import("std");

const Chunk = @import("chunk.zig");
const OpCode = Chunk.OpCode;
const Compiler = @import("compiler.zig");
const Table = @import("table.zig");
const container = @import("container.zig");
const memory = @import("memory.zig");
const lox = @import("lox.zig");
const io = @import("io.zig");
const log = std.log.scoped(.VM);

const debug_trace_execution = @import("debug").trace_execution;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const frame_max = 64;
const stack_max = frame_max * (std.math.maxInt(u8) + 1);

const CallFrame = struct {
    ip: [*]u8,
    slots: [*]lox.Value,

    function: *lox.Function,

    pub inline fn readByte(self: *CallFrame) u8 {
        const r = self.ip[0];
        self.ip += 1;
        return r;
    }

    pub inline fn readShort(self: *CallFrame) u16 {
        const r: u16 = (@as(u16, self.ip[0]) << 8) | self.ip[1];
        self.ip += 2;
        return r;
    }

    pub inline fn readConstant(self: *CallFrame) lox.Value {
        return self.function.chunk.constants.items[self.readByte()];
    }

    pub inline fn readString(self: *CallFrame) *lox.String {
        return self.readConstant().asObject().as(.string);
    }
};

const CallFrames = container.Array(CallFrame, frame_max);

var frames: CallFrames = undefined;

var out_writer: io.Writer = undefined;
var err_writer: io.Writer = undefined;

pub var stack: container.Stack(lox.Value, stack_max) = undefined;
pub var strings: Table = undefined;
pub var globals: Table = undefined;

pub fn init(out: io.Writer, err: io.Writer) void {
    stack.reset();
    frames.len = 0;

    out_writer = out;
    err_writer = err;

    strings = Table.init(memory.gpa);
    globals = Table.init(memory.gpa);
}

pub fn deinit() void {
    strings.deinit();
    globals.deinit();
    memory.gc.freeObjects();
}

pub fn interpret(source: []const u8) InterpretError!void {
    var compiler = Compiler.init(err_writer);
    defer compiler.deinit();

    const function = compiler.compile(source) catch {
        log.debug("Compile error", .{});
        return InterpretError.CompileError;
    };

    stack.push(lox.Value.from(function));
    try call(function, 0);

    try run();
}

fn runtimeError(comptime fmt: []const u8, args: anytype) void {
    err_writer.print(fmt ++ "\n", args) catch return;

    var it = std.mem.reverseIterator(frames.buf[0..frames.len]);
    while (it.nextPtr()) |frame| {
        const function: *lox.Function = frame.function;

        const slices = function.chunk.codes.slice();
        const codes = slices.items(.code);
        const lines = slices.items(.line);

        const idx = (frame.ip - codes.ptr) - 1;
        std.debug.assert(idx < function.chunk.codes.len);

        err_writer.print("[line {d}] in ", .{lines[idx]}) catch return;
        if (function.name) |name| {
            err_writer.print("{s}()\n", .{name.str}) catch return;
        } else {
            err_writer.writeAll("script\n") catch return;
        }
    }

    stack.reset();
}

const Op = enum { add, sub, gt, lt, mul, div };

inline fn binaryOp(comptime op: Op) InterpretError!void {
    if (!stack.peek(0).isDouble() or !stack.peek(1).isDouble()) {
        runtimeError("Operands must be numbers.", .{});
        return InterpretError.RuntimeError;
    }

    const b = stack.pop().asDouble();
    const a = stack.pop().asDouble();

    @setRuntimeSafety(false);
    stack.push(lox.Value.from(switch (op) {
        .add => a + b,
        .sub => a - b,
        .gt => a > b,
        .lt => a < b,
        .div => a / b,
        .mul => a * b,
    }));
}

inline fn isString(v: lox.Value) bool {
    return v.isObject() and v.asObject().is(.string);
}

fn run() InterpretError!void {
    var frame = frames.backPtr();

    while (true) {
        if (comptime debug_trace_execution) {
            var buf: [1024]u8 = .{' '} ** 1024;

            var i: usize = 10;
            for (stack.items[0..stack.len()]) |slot| {
                i += (std.fmt.bufPrint(buf[i..], "[ {s} ]", .{slot}) catch unreachable).len;
            }
            log.debug("{s}", .{buf[0..i]});

            const offset = frame.ip - frame.function.chunk.codes.items(.code).ptr;
            _ = frame.function.chunk.disassembleAt(offset);
        }

        const instr: OpCode = @enumFromInt(frame.readByte());

        switch (instr) {
            .Constant => {
                const v = frame.readConstant();
                stack.push(v);
            },
            .Nil => stack.push(lox.Value.from(null)),
            .True => stack.push(lox.Value.from(true)),
            .False => stack.push(lox.Value.from(false)),
            .Pop => _ = stack.pop(),
            .GetLocal => {
                const slot = frame.readByte();

                stack.push(frame.slots[slot]);
            },
            .SetLocal => {
                const slot = frame.readByte();
                frame.slots[slot] = stack.peek(0);
            },
            .GetGlobal => {
                const name = frame.readString();
                if (globals.get(name)) |v| {
                    stack.push(v);
                } else {
                    runtimeError("Undefined variable '{s}'", .{name});
                    return error.RuntimeError;
                }
            },
            .DefineGlobal => {
                const name = frame.readString();

                _ = globals.set(name, stack.peek(0));
                _ = stack.pop();
            },
            .SetGlobal => {
                const name = frame.readString();
                if (globals.set(name, stack.peek(0))) {
                    globals.delete(name) catch unreachable;
                    runtimeError("Undefined variable '{s}'", .{name});
                    return error.RuntimeError;
                }
            },

            .Equal => {
                const b = stack.pop();
                const a = stack.pop();
                stack.push(lox.Value.from(a.equal(b)));
            },

            .Greater => try binaryOp(.gt),
            .Less => try binaryOp(.lt),
            .Add => {
                const b = stack.peek(0);
                const a = stack.peek(1);
                if (isString(a) and isString(b)) {
                    const sb: *lox.String = b.asObject().as(.string);
                    const sa: *lox.String = a.asObject().as(.string);
                    const result = sa.concat(sb);
                    _ = stack.pop();
                    _ = stack.pop();
                    stack.push(lox.Value.from(result));
                } else if (a.isDouble() and b.isDouble()) {
                    const bf = stack.pop().asDouble();
                    const af = stack.pop().asDouble();

                    @setRuntimeSafety(false);
                    const v = lox.Value.from(bf + af);
                    @setRuntimeSafety(true);
                    stack.push(v);
                } else {
                    runtimeError("Operands must be two numbers or two strings.", .{});
                    return InterpretError.RuntimeError;
                }
            },
            .Subtract => try binaryOp(.sub),
            .Multiply => try binaryOp(.mul),
            .Divide => try binaryOp(.div),

            .Not => stack.push(lox.Value.from(stack.pop().isFalsey())),
            .Negate => {
                if (!stack.peek(0).isDouble()) {
                    runtimeError("Operand must be a number.", .{});
                    return InterpretError.RuntimeError;
                }

                stack.push(lox.Value.from(-stack.pop().asDouble()));
            },

            .Print => {
                const v = stack.pop();
                out_writer.print("{s}\n", .{v}) catch {};
            },
            .Jump => {
                const offset = frame.readShort();
                frame.ip += offset;
            },
            .JumpIfFalse => {
                const offset = frame.readShort();
                if (stack.peek(0).isFalsey()) frame.ip += offset;
            },
            .Loop => {
                const offset = frame.readShort();
                frame.ip -= offset;
            },
            .Call => {
                const arg_count = frame.readByte();
                try callValue(stack.peek(arg_count), arg_count);
                frame = frames.backPtr();
            },
            .Return => {
                const result = stack.pop();
                frames.len -= 1;

                if (frames.len == 0) {
                    _ = stack.pop();
                    return;
                }

                stack.top = frame.slots;
                stack.push(result);
                frame = frames.backPtr();
            },
            else => {},
        }
    }
}

fn callValue(callee: lox.Value, arg_count: u32) !void {
    if (callee.isObject()) {
        const obj = callee.asObject();

        switch (obj.tag) {
            inline .function => |tag| return call(obj.as(tag), arg_count),
            else => {},
        }
    }

    runtimeError("Can only call functions and classes.", .{});
    return error.RuntimeError;
}

fn call(function: *lox.Function, arg_count: u32) !void {
    if (arg_count != function.arity) {
        runtimeError("Expected {} arguments but got {}.", .{ function.arity, arg_count });
        return error.RuntimeError;
    }

    if (frames.len == frames.buf.len) {
        runtimeError("Stack overflow.", .{});
        return error.RuntimeError;
    }

    const frame = frames.addOne();
    frame.function = function;
    frame.ip = function.chunk.codes.items(.code).ptr;
    frame.slots = stack.top - arg_count - 1;
}
