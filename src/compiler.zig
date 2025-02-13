const std = @import("std");

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;

const Chunk = @import("chunk.zig");
const OpCode = Chunk.OpCode;
const lox = @import("lox.zig");

const debug_print_code = @import("debug").print_code;

var curr: Token = undefined;
var prev: Token = undefined;

var had_error: bool = false;
var panic_mode: bool = false;
var lexer: Lexer = undefined;

var compilingChunk: ?*Chunk = null;

pub fn compile(source: []const u8, chunk: *Chunk) error{CompileError}!void {
    lexer = Lexer.init(source);

    compilingChunk = chunk;
    had_error = false;
    panic_mode = false;

    advance();

    while (!match(.Eof)) {
        declaration();
    }

    endCompiler();
    if (had_error) return error.CompileError;
}

fn endCompiler() void {
    emitReturn();

    if (comptime debug_print_code) {
        currentChunk().disassemble("code");
    }


}

fn currentChunk() *Chunk {
    if (compilingChunk) |chk| {
        return chk;
    } else {
        unreachable;
    }
}

fn errorAtCurr(msg: []const u8) void {
    errorAt(&curr, msg);
}

fn errorAtPrev(msg: []const u8) void {
    errorAt(&prev, msg);
}

fn errorAt(token: *const Token, msg: []const u8) void {
    if (panic_mode) return;
    panic_mode = true;

    const out = std.io.getStdErr();
    var writer = std.io.bufferedWriter(out.writer());
    defer writer.flush() catch {};
    defer had_error = true;

    writer.writer().print("[line {d}] Error", .{token.line}) catch return;

    switch (token.type) {
        .Error => {},
        .Eof => writer.writer().print(" at end", .{}) catch return,
        else => writer.writer().print(" at '{s}'", .{token.lexeme}) catch return,
    }

    writer.writer().print(" {s}\n", .{msg}) catch return;
}

fn emitByte(byte: u8) void {
    currentChunk().write(byte, prev.line);
}

fn emitOp(op: OpCode) void {
    emitByte(@intFromEnum(op));
}

fn emitBytes(op: OpCode, byte: u8) void {
    emitOp(op);
    emitByte(byte);
}

fn emitConstant(v: lox.Value) void {
    emitBytes(.Constant, makeConstant(v));
}

fn emitReturn() void {
    emitOp(.Return);
}

fn makeConstant(v: lox.Value) u8 {
    const i = currentChunk().addConstant(v);
    if (i > std.math.maxInt(u8)) {
        errorAtPrev("Too many constants in one chunk.");
        return 0;
    }

    return @intCast(i);
}

fn advance() void {
    prev = curr;

    while (true) {
        curr = lexer.scan();
        if (curr.type != .Error) break;

        errorAtCurr(curr.lexeme);
    }
}

fn consume(comptime t: TokenType, comptime msg: []const u8) void {
    if (curr.type == t) {
        advance();
        return;
    }

    errorAtCurr(msg);
}

fn match(t: TokenType) bool {
    if (!check(t)) return false;

    advance();
    return true;
}

inline fn check(t: TokenType) bool {
    return curr.type == t;
}

fn synchronize() void {
    panic_mode = false;

    while (curr.type != .Eof) : (advance()) {
        if (prev.type == .Semicolon) return;
        switch (curr.type) {
            .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
            else => {},
        }
    }
}

const ParseFn = fn (bool) void;
const Precedence = enum(u8) {
    None = 0,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};
const ParseRule = struct {
    prefix: ?*const ParseFn,
    infix: ?*const ParseFn,
    precedence: Precedence,
};

const rules = init_rules: {
    var r = struct {
        items: [@typeInfo(TokenType).@"enum".fields.len]ParseRule = undefined,

        fn setd(self: *@This(), comptime t: TokenType, prefix: ?ParseFn) void {
            self.set(t, prefix, null, .None);
        }

        fn set(self: *@This(), comptime t: TokenType, prefix: ?ParseFn, infix: ?ParseFn, prec: Precedence) void {
            self.items[@intFromEnum(t)] = .{
                .prefix = if (prefix) |p| &p else null,
                .infix = if (infix) |i| &i else null,
                .precedence = prec,
            };
        }
    }{};

    r.set(.LeftParen, grouping, null, .Call);

    r.set(.Minus, unary, binary, .Term);
    r.set(.Plus, null, binary, .Term);
    r.set(.Star, null, binary, .Factor);
    r.set(.Slash, null, binary, .Factor);

    r.setd(.Bang, unary);
    r.set(.BangEqual, null, binary, .Equality);
    r.set(.EqualEqual, null, binary, .Equality);
    r.set(.Greater, null, binary, .Comparison);
    r.set(.GreaterEqual, null, binary, .Comparison);
    r.set(.Less, null, binary, .Comparison);
    r.set(.LessEqual, null, binary, .Comparison);

    r.setd(.False, literal);
    r.setd(.True, literal);
    r.setd(.Nil, literal);
    r.setd(.Number, number);
    r.setd(.String, string);

    break :init_rules struct {
        items: [@typeInfo(TokenType).@"enum".fields.len]ParseRule = undefined,

        pub fn get(self: *const @This(), t: TokenType) ParseRule {
            return self.items[@intFromEnum(t)];
        }
    }{ .items = r.items };
};

fn parsePrecedence(prec: Precedence) void {
    advance();

    const prefix = rules.get(prev.type).prefix orelse {
        errorAtPrev("Expect expression.");
        return;
    };

    const can_assign = @intFromEnum(prec) <= @intFromEnum(Precedence.Assignment);
    prefix(can_assign);

    while (@intFromEnum(prec) <= @intFromEnum(rules.get(curr.type).precedence)) {
        advance();

        if (rules.get(prev.type).infix) |infix| {
            infix(can_assign);
        } else {
            unreachable;
        }
    }

    if (can_assign and match(.Equal)) {
        errorAtPrev("Invalid assignment target.");
    }
}

fn number(_: bool) void {
    const v = std.fmt.parseFloat(f64, prev.lexeme) catch 0;
    emitConstant(lox.Value.from(v));
}

fn grouping(_: bool) void {
    expression();
    consume(.RightParen, "Expect ')' after expression.");
}

fn unary(_: bool) void {
    const op = prev.type;

    // Compile the operand.
    parsePrecedence(.Unary);

    emitOp(switch (op) {
        .Minus => .Negate,
        .Bang => .Not,
        else => unreachable,
    });
}

fn binary(_: bool) void {
    const op = prev.type;
    const rule = rules.get(op);

    parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    const opcode: OpCode = sw: switch (op) {
        .Plus => .Add,
        .Minus => .Subtract,
        .Star => .Multiply,
        .Slash => .Divide,
        .BangEqual => {
            emitOp(.Equal);
            break :sw .Not;
        },
        .EqualEqual => .Equal,
        .Greater => .Greater,
        .GreaterEqual => {
            emitOp(.Less);
            break :sw .Not;
        },
        .Less => .Less,
        .LessEqual => {
            emitOp(.Greater);
            break :sw .Not;
        },
        else => unreachable,
    };

    emitOp(opcode);
}

fn string(_: bool) void {}

fn literal(_: bool) void {
    emitOp(switch (prev.type) {
        .False => .False,
        .True => .True,
        .Nil => .Nil,
        else => unreachable,
    });
}

fn expression() void {
    parsePrecedence(.Assignment);
}

fn declaration() void {
    if (match(.Fun)) {} else if (match(.Var)) {} else {
        statement();
    }

    if (panic_mode) synchronize();
}

fn statement() void {
    if (match(.Print)) {
        printStatement();
    } else if (match(.Return)) {
        returnStatement();
    } else {
        expressionStatement();
    }
}

fn printStatement() void {
    expression();
    consume(.Semicolon, "Expect ';' after value.");
    emitOp(.Print);
}

fn returnStatement() void {}

fn expressionStatement() void {
    expression();
    consume(.Semicolon, "Expect ';' after expression.");
    emitOp(.Pop);
}

test {
    std.debug.print("{any}\n", .{rules.get(.LeftParen)});
}
