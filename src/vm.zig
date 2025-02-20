const std = @import("std");

const Chunk = @import("chunk.zig");
const OpCode = Chunk.OpCode;
const Compiler = @import("compiler.zig");
const Table = @import("table.zig");
const container = @import("container.zig");
const memory = @import("memory.zig");
const lox = @import("lox.zig");
const native = @import("native.zig");
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

    closure: *const lox.Closure,

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
        return self.closure.function.chunk.constants.items[self.readByte()];
    }

    pub inline fn readString(self: *CallFrame) *lox.String {
        return self.readConstant().asObject().as(.string);
    }
};

const CallFrames = container.Array(CallFrame, frame_max);

var out_writer: io.Writer = undefined;
var err_writer: io.Writer = undefined;

pub var frames: CallFrames = undefined;
pub var stack: container.Stack(lox.Value, stack_max) = undefined;
pub var strings: Table = undefined;
pub var globals: Table = undefined;
pub var open_upvalues: ?*lox.Upvalue = null;
pub var current: ?*Compiler = null;
pub var init_string: *lox.String = undefined;

pub fn init(out: io.Writer, err: io.Writer) void {
    resetStack();
    frames.len = 0;

    out_writer = out;
    err_writer = err;

    open_upvalues = null;
    current = null;
    strings = Table.init(memory.gpa);
    globals = Table.init(memory.gpa);

    init_string = lox.String.copy("init");

    defineNative("clock", native.clock);
}

pub fn deinit() void {
    strings.deinit();
    globals.deinit();
    init_string = undefined;
    memory.gc.deinit();
}

pub fn interpret(source: []const u8) InterpretError!void {
    var compiler = Compiler.init(err_writer);
    current = &compiler;
    defer compiler.deinit();
    defer current = null;

    const function = compiler.compile(source) catch {
        log.debug("Compile error", .{});
        return InterpretError.CompileError;
    };

    stack.push(lox.Value.from(function));
    const closure = lox.Closure.create(function);
    _ = stack.pop();
    stack.push(lox.Value.from(closure));

    try call(closure, 0);

    try run();
}

fn resetStack() void {
    stack.reset();
    frames.len = 0;

    open_upvalues = null;
}

fn runtimeError(comptime fmt: []const u8, args: anytype) void {
    err_writer.print(fmt ++ "\n", args) catch return;

    var it = std.mem.reverseIterator(frames.buf[0..frames.len]);
    while (it.nextPtr()) |frame| {
        const function = frame.closure.function;

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

    resetStack();
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

            const offset = frame.ip - frame.closure.function.chunk.codes.items(.code).ptr;
            _ = frame.closure.function.chunk.disassembleAt(offset);
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
            .Closure => {
                const func = frame.readConstant().asObject().as(.function);
                const closure = lox.Closure.create(func);
                stack.push(lox.Value.from(closure));

                for (closure.upvalues) |*upvalue| {
                    const is_local = frame.readByte();
                    const index = frame.readByte();

                    if (is_local == 1) {
                        upvalue.* = captureUpvalue(&frame.slots[index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .GetUpvalue => {
                const slot = frame.readByte();
                stack.push(frame.closure.upvalues[slot].location.*);
            },
            .SetUpvalue => {
                const slot = frame.readByte();
                frame.closure.upvalues[slot].location.* = stack.peek(0);
            },
            .CloseUpvalue => {
                const p = stack.top - 1;
                closeUpvalue(&p[0]);
                _ = stack.pop();
            },
            .Return => {
                const result = stack.pop();
                closeUpvalue(&frame.slots[0]);
                frames.len -= 1;

                if (frames.len == 0) {
                    _ = stack.pop();
                    return;
                }

                stack.top = frame.slots;
                stack.push(result);
                frame = frames.backPtr();
            },
            .Class => {
                const class = lox.Class.create(frame.readString());
                stack.push(lox.Value.from(class));
            },
            .GetProperty => {
                const instance: *lox.Instance = stack.peek(0).asObject().as(.instance);
                const name = frame.readString();

                if (instance.field.get(name)) |value| {
                    _ = stack.pop(); // instance
                    stack.push(value);
                } else {
                    try bindMethod(instance.class, name);
                }
            },
            .SetProperty => {
                const v = stack.peek(1);
                if (!v.isObject() and v.asObject().is(.instance)) {
                    runtimeError("Only instances have fields.", .{});
                    return error.RuntimeError;
                }
                const instance: *lox.Instance = stack.peek(1).asObject().as(.instance);
                const name = frame.readString();
                _ = instance.field.set(name, stack.peek(0));

                const value = stack.pop();
                _ = stack.pop();

                stack.push(value);
            },
            .Method => defineMethod(frame.readString()),
            .Invoke => {
                const method = frame.readString();
                const arg_count = frame.readByte();

                try invoke(method, arg_count);

                frame = frames.backPtr();
            },
            .Inherit => {
                const superclassv = stack.peek(1);
                const subclass: *lox.Class = stack.peek(0).asObject().as(.class);

                if (!superclassv.isObject() and !superclassv.asObject().is(.class)) {
                    runtimeError("Superclass must be a class.", .{});
                    return error.RuntimeError;
                }

                const superclass: *lox.Class = superclassv.asObject().as(.class);
                subclass.method.addAll(superclass.method.entries);

                _ = stack.pop(); // subclass

            },
            .GetSuper => {
                const name = frame.readString();

                const superclass = stack.pop().asObject().as(.class);

                try bindMethod(superclass, name);
            },
            .InvokeSuper => {
                const method = frame.readString();
                const count = frame.readByte();
                const superclass = stack.pop().asObject().as(.class);
                try invokeFromClass(superclass, method, count);
                frame = frames.backPtr();
            },
        }
    }
}

fn invoke(name: *lox.String, arg_count: u8) !void {
    const receiver = stack.peek(arg_count);

    if (!receiver.isObject() and !receiver.asObject().is(.instance)) {
        runtimeError("Only instances have methods.", .{});
        return error.RuntimeError;
    }

    const instance: *lox.Instance = receiver.asObject().as(.instance);

    if (instance.field.get(name)) |value| {
        const p = stack.top - arg_count - 1;
        p[0] = value;

        return callValue(value, arg_count);
    }

    try invokeFromClass(instance.class, name, arg_count);
}

fn invokeFromClass(class: *const lox.Class, name: *lox.String, arg_count: u8) !void {
    if (class.method.get(name)) |method| {
        return call(method.asObject().as(.closure), arg_count);
    } else {
        runtimeError("Undefined property '{s}'.", .{name});
        return error.RuntimeError;
    }
}

fn bindMethod(class: *const lox.Class, name: *lox.String) !void {
    if (class.method.get(name)) |method| {
        const bound = lox.BoundMethod.create(stack.peek(0), method.asObject().as(.closure));
        _ = stack.pop();
        stack.push(lox.Value.from(bound));
    } else {
        runtimeError("Undefined property '{s}'.", .{name});
        return error.RuntimeError;
    }
}

fn defineMethod(name: *lox.String) void {
    const method = stack.peek(0);

    const class: *lox.Class = stack.peek(1).asObject().as(.class);
    _ = class.method.set(name, method);

    _ = stack.pop();
}

fn captureUpvalue(local: *lox.Value) *lox.Upvalue {
    var prev: ?*lox.Upvalue = null;
    var upvalues = open_upvalues;
    while (upvalues) |upvalue| : (upvalues = upvalue.next) {
        if (@intFromPtr(upvalue.location) <= @intFromPtr(local)) break;

        prev = upvalue;
    }

    if (upvalues) |upvalue| {
        if (upvalue.location == local) {
            return upvalue;
        }
    }

    const created = lox.Upvalue.create(local);
    created.next = upvalues;

    if (prev) |p| {
        p.next = created;
    } else {
        open_upvalues = created;
    }

    return created;
}

fn closeUpvalue(last: *lox.Value) void {
    while (open_upvalues) |open| : (open_upvalues = open.next) {
        if (@intFromPtr(open.location) < @intFromPtr(last)) break;

        open.closed = open.location.*;
        open.location = &open.closed;
    }
}

fn callValue(callee: lox.Value, arg_count: u32) !void {
    if (callee.isObject()) {
        const obj = callee.asObject();

        switch (obj.tag) {
            .bound_method => {
                const bound: *lox.BoundMethod = obj.as(.bound_method);
                const p = stack.top - arg_count - 1;
                p[0] = bound.receiver;

                return call(bound.method, arg_count);
            },
            .class => {
                const class: *lox.Class = obj.as(.class);
                const p = stack.top - arg_count - 1;
                p[0] = lox.Value.from(lox.Instance.create(class));

                if (class.method.get(init_string)) |initializer| {
                    return call(initializer.asObject().as(.closure), arg_count);
                } else if (arg_count != 0) {
                    runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                    return error.RuntimeError;
                }

                return;
            },
            .closure => return call(obj.as(.closure), arg_count),
            .native => {
                const nativ = obj.as(.native);
                const result = nativ.function((stack.top - arg_count)[0..arg_count]);
                stack.top -= arg_count + 1;
                stack.push(result);
                return;
            },
            else => {},
        }
    }

    runtimeError("Can only call functions and classes.", .{});
    return error.RuntimeError;
}

fn call(closure: *const lox.Closure, arg_count: u32) !void {
    if (arg_count != closure.function.arity) {
        runtimeError("Expected {} arguments but got {}.", .{ closure.function.arity, arg_count });
        return error.RuntimeError;
    }

    if (frames.len == frames.buf.len) {
        runtimeError("Stack overflow.", .{});
        return error.RuntimeError;
    }

    const frame = frames.addOne();
    frame.closure = closure;
    frame.ip = closure.function.chunk.codes.items(.code).ptr;
    frame.slots = stack.top - arg_count - 1;
}

fn defineNative(comptime name: []const u8, comptime func: lox.Native.NativeFn) void {
    stack.push(lox.Value.from(lox.String.copy(name)));
    stack.push(lox.Value.from(lox.Native.create(func)));
    _ = globals.set(stack.items[0].asObject().as(.string), stack.items[1]);

    _ = stack.pop();
    _ = stack.pop();
}
