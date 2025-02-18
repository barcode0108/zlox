const std = @import("std");

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;
const Chunk = @import("chunk.zig");
const OpCode = Chunk.OpCode;

const container = @import("container.zig");
const lox = @import("lox.zig");
const io = @import("io.zig");

const log = std.log.scoped(.Compiler);

const debug_print_code = @import("debug").print_code;

const FunctionType = enum {
    function,
    initailizer,
    method,
    script,
};

const Compiler = struct {
    enclosing: ?*Compiler,
    function: *lox.Function,
    ftype: FunctionType,

    locals: Locals,
    scope_depth: u32,

    const Locals = container.Array(Local, std.math.maxInt(u8) + 1);

    pub fn init(comptime ft: FunctionType, name: []const u8, enclosing: ?*Compiler) Compiler {
        var ret = Compiler{
            .enclosing = enclosing,
            .function = lox.Function.create(if (ft == .script) null else name),
            .ftype = ft,
            .locals = Locals.init(),
            .scope_depth = 0,
        };

        const p = ret.locals.addOne();
        p.name.lexeme = "";
        p.depth = 0;

        return ret;
    }

    pub fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    pub fn endScope(self: *Compiler) void {
        self.scope_depth -= 1;

        const lines = self.function.chunk.codes.items(.line);
        const line = lines[lines.len - 1];

        while (self.locals.len > 0 and
            if (self.locals.backPtr().depth) |d| d > self.scope_depth else false) : (self.locals.len -= 1)
        {
            self.function.chunk.writeOp(.Pop, line);
        }
    }

    pub fn end(self: *Compiler) *lox.Function {
        const lines = self.function.chunk.codes.items(.line);
        const line = lines[lines.len - 1];
        self.addReturn(line);

        if (comptime debug_print_code) {
            var buf: [512]u8 = undefined;
            const name = std.fmt.bufPrint(buf[0..], "{s}", .{self.function}) catch unreachable;
            self.function.chunk.disassemble(name);
        }

        return self.function;
    }

    pub fn addReturn(self: *Compiler, line: u32) void {
        self.function.chunk.writeOp(.Nil, line);
        self.function.chunk.writeOp(.Return, line);
    }
};

const Local = struct {
    name: Token,
    depth: ?u32,
};

const Parser = @This();

err_writer: io.Writer,

curr: Token,
prev: Token,
had_error: bool,
panic_mode: bool,
lexer: Lexer,

current_compiler: *Compiler,

pub fn init(err: io.Writer) Parser {
    return .{
        .curr = undefined,
        .prev = undefined,
        .had_error = false,
        .panic_mode = false,
        .lexer = undefined,
        .current_compiler = undefined,
        .err_writer = err,
    };
}

pub fn compile(self: *Parser, source: []const u8) error{CompileError}!*lox.Function {
    self.lexer = Lexer.init(source);

    var compiler = Compiler.init(.script, self.prev.lexeme, null);
    self.current_compiler = &compiler;

    self.had_error = false;
    self.panic_mode = false;

    self.advance();

    while (!self.match(.Eof)) {
        self.declaration();
    }

    const ret = self.endCompiler();

    if (self.had_error) return error.CompileError;

    return ret;
}

pub fn deinit(self: *Parser) void {
    _ = self;
}

fn endCompiler(self: *Parser) *lox.Function {
    const p = self.current_compiler.end();
    self.current_compiler = self.current_compiler.enclosing orelse undefined;
    return p;
}

fn currentChunk(self: *Parser) *Chunk {
    return &self.current_compiler.function.chunk;
}

fn errorAtCurr(self: *Parser, msg: []const u8) void {
    self.errorAt(&self.curr, msg);
}

fn errorAtPrev(self: *Parser, msg: []const u8) void {
    self.errorAt(&self.prev, msg);
}

fn errorAt(self: *Parser, token: *const Token, msg: []const u8) void {
    if (self.panic_mode) return;
    self.panic_mode = true;

    var writer = std.io.bufferedWriter(self.err_writer);
    defer writer.flush() catch {};
    defer self.had_error = true;

    writer.writer().print("[line {d}] Error", .{token.line}) catch return;

    switch (token.type) {
        .Error => {},
        .Eof => writer.writer().print(" at end", .{}) catch return,
        else => writer.writer().print(" at '{s}'", .{token.lexeme}) catch return,
    }

    writer.writer().print(" {s}\n", .{msg}) catch return;
}

fn emitByte(self: *Parser, byte: u8) void {
    self.currentChunk().write(byte, self.prev.line);
}

fn emitOp(self: *Parser, op: OpCode) void {
    self.currentChunk().writeOp(op, self.prev.line);
}

fn emitBytes(self: *Parser, op: OpCode, byte: u8) void {
    self.emitOp(op);
    self.emitByte(byte);
}

fn emitConstant(self: *Parser, v: lox.Value) void {
    self.emitBytes(.Constant, self.makeConstant(v));
}

fn emitReturn(self: *Parser) void {
    self.current_compiler.addReturn(self.prev.line);
}

fn emitJump(self: *Parser, op: OpCode) usize {
    self.emitOp(op);
    self.emitByte(0xff);
    self.emitByte(0xff);

    return self.currentChunk().codes.len - 2;
}

fn emitLoop(self: *Parser, loop_start: usize) void {
    self.emitOp(.Loop);

    const offset = self.currentChunk().codes.len - loop_start + 2;

    if (offset > std.math.maxInt(u16)) self.errorAtPrev("Loop body too large.");

    self.emitByte(@truncate(offset >> 8));
    self.emitByte(@truncate(offset));
}

fn patchJump(self: *Parser, offset: usize) void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump = self.currentChunk().codes.len - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        self.errorAtPrev("Too much code to jump over.");
    }

    const codes: []u8 = self.currentChunk().codes.items(.code);
    codes[offset] = @truncate(jump >> 8 & 0xff);
    codes[offset + 1] = @truncate(jump & 0xff);
}

fn makeConstant(self: *Parser, v: lox.Value) u8 {
    const i = self.currentChunk().addConstant(v);
    if (i > std.math.maxInt(u8)) {
        self.errorAtPrev("Too many constants in one chunk.");
        return 0;
    }

    return @intCast(i);
}

fn identifierEqual(a: *const Token, b: *const Token) bool {
    return std.mem.eql(u8, a.lexeme, b.lexeme);
}

fn identifierConstant(self: *Parser, name: *const Token) u8 {
    const s = lox.String.copy(name.lexeme);
    return self.makeConstant(lox.Value.from(s));
}

fn addLocal(self: *Parser, name: *const Token) void {
    if (self.current_compiler.locals.full()) {
        self.errorAtPrev("Too many local variables in function.");
    }

    const p = self.current_compiler.locals.addOne();

    p.name = name.*;
    p.depth = 0;
}

fn advance(self: *Parser) void {
    self.prev = self.curr;

    while (true) {
        self.curr = self.lexer.scan();
        if (self.curr.type != .Error) break;

        self.errorAtCurr(self.curr.lexeme);
    }
}

fn consume(self: *Parser, comptime t: TokenType, comptime msg: []const u8) void {
    if (self.curr.type == t) {
        self.advance();
        return;
    }

    self.errorAtCurr(msg);
}

fn match(self: *Parser, t: TokenType) bool {
    if (!self.check(t)) return false;

    self.advance();
    return true;
}

fn check(self: *Parser, t: TokenType) bool {
    return self.curr.type == t;
}

fn synchronize(self: *Parser) void {
    self.panic_mode = false;

    while (self.curr.type != .Eof) : (self.advance()) {
        if (self.prev.type == .Semicolon) return;
        switch (self.curr.type) {
            .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
            else => {},
        }
    }
}

const ParseFn = fn (*Parser, bool) void;
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

    pub fn increment(self: Precedence) Precedence {
        return @enumFromInt(@intFromEnum(self) + 1);
    }

    pub fn lessEqual(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) <= @intFromEnum(other);
    }
};

const ParseRule = struct {
    prefix: ?*const ParseFn = null,
    infix: ?*const ParseFn = null,
    precedence: Precedence = .None,

    pub fn init(prefix: ?ParseFn, infix: ?ParseFn, prec: Precedence) ParseRule {
        return .{
            .prefix = if (prefix) |p| &p else null,
            .infix = if (infix) |i| &i else null,
            .precedence = prec,
        };
    }

    pub fn initPrefix(prefix: ParseFn) ParseRule {
        return .{
            .prefix = &prefix,
        };
    }
};

const Rules = std.EnumArray(TokenType, ParseRule);

const rules = Rules.initDefault(ParseRule{}, .{
    .LeftParen = ParseRule.init(grouping, call, .Call),

    .Minus = ParseRule.init(unary, binary, .Term),
    .Plus = ParseRule.init(null, binary, .Term),
    .Star = ParseRule.init(null, binary, .Factor),
    .Slash = ParseRule.init(null, binary, .Factor),

    .Bang = ParseRule.initPrefix(unary),
    .BangEqual = ParseRule.init(null, binary, .Equality),
    .EqualEqual = ParseRule.init(null, binary, .Equality),
    .Greater = ParseRule.init(null, binary, .Comparison),
    .GreaterEqual = ParseRule.init(null, binary, .Comparison),
    .Less = ParseRule.init(null, binary, .Comparison),
    .LessEqual = ParseRule.init(null, binary, .Comparison),

    .Identifier = ParseRule.initPrefix(variable),

    .False = ParseRule.initPrefix(literal),
    .True = ParseRule.initPrefix(literal),
    .Nil = ParseRule.initPrefix(literal),
    .Number = ParseRule.initPrefix(number),
    .String = ParseRule.initPrefix(string),
});

fn parsePrecedence(self: *Parser, prec: Precedence) void {
    self.advance();

    const prefix = rules.getPtrConst(self.prev.type).prefix orelse {
        self.errorAtPrev("Expect expression.");
        return;
    };

    const can_assign = prec.lessEqual(.Assignment);
    prefix(self, can_assign);

    while (prec.lessEqual(rules.getPtrConst(self.curr.type).precedence)) {
        self.advance();

        if (rules.getPtrConst(self.prev.type).infix) |infix| {
            infix(self, can_assign);
        } else {
            unreachable;
        }
    }

    if (can_assign and self.match(.Equal)) {
        self.errorAtPrev("Invalid assignment target.");
    }
}

fn number(self: *Parser, _: bool) void {
    const v = std.fmt.parseFloat(f64, self.prev.lexeme) catch 0;
    self.emitConstant(lox.Value.from(v));
}

fn grouping(self: *Parser, _: bool) void {
    self.expression();
    self.consume(.RightParen, "Expect ')' after expression.");
}

fn unary(self: *Parser, _: bool) void {
    const op = self.prev.type;

    // Compile the operand.
    self.parsePrecedence(.Unary);

    self.emitOp(switch (op) {
        .Minus => .Negate,
        .Bang => .Not,
        else => unreachable,
    });
}

fn binary(self: *Parser, _: bool) void {
    const op = self.prev.type;
    const rule = rules.getPtrConst(op);

    self.parsePrecedence(rule.precedence.increment());

    const opcode: OpCode = sw: switch (op) {
        .Plus => .Add,
        .Minus => .Subtract,
        .Star => .Multiply,
        .Slash => .Divide,
        .BangEqual => {
            self.emitOp(.Equal);
            break :sw .Not;
        },
        .EqualEqual => .Equal,
        .Greater => .Greater,
        .GreaterEqual => {
            self.emitOp(.Less);
            break :sw .Not;
        },
        .Less => .Less,
        .LessEqual => {
            self.emitOp(.Greater);
            break :sw .Not;
        },
        else => unreachable,
    };

    self.emitOp(opcode);
}

fn string(self: *Parser, _: bool) void {
    const s = lox.String.copy(self.prev.lexeme[1..(self.prev.lexeme.len - 1)]);
    const v = lox.Value.from(s);
    self.emitConstant(v);
}

fn literal(self: *Parser, _: bool) void {
    self.emitOp(switch (self.prev.type) {
        .False => .False,
        .True => .True,
        .Nil => .Nil,
        else => unreachable,
    });
}

fn variable(self: *Parser, can_assign: bool) void {
    self.namedVariable(self.prev, can_assign);
}

fn call(self: *Parser, _: bool) void {
    const arg_count = self.argumentList();
    self.emitBytes(.Call, arg_count);
}

fn argumentList(self: *Parser) u8 {
    var count: u8 = 0;

    if (!self.check(.RightParen)) {
        while (true) {
            self.expression();

            if (count == 255) {
                self.errorAtPrev("Can't have more than 255 arguments.");
            }

            @setRuntimeSafety(false);
            count += 1;
            @setRuntimeSafety(true);

            if (!self.match(.Comma)) break;
        }
    }

    self.consume(.RightParen, "Expect ')' after arguments.");

    return count;
}

fn function(self: *Parser, comptime ft: FunctionType) void {
    var compiler = Compiler.init(ft, self.prev.lexeme, self.current_compiler);
    self.current_compiler = &compiler;
    compiler.beginScope();

    self.consume(.LeftParen, "Expect '(' after function name.");
    if (!self.check(.RightParen)) {
        // arguments
        while (true) {
            self.current_compiler.function.arity += 1;
            if (self.current_compiler.function.arity > std.math.maxInt(u8)) {
                self.errorAtCurr("Can't have more than 255 parameters.");
            }

            const constant = self.parseVariable("Expect parameter name.");
            self.defineVariable(constant);
            if (!self.match(.Comma)) break;
        }
    }

    self.consume(.RightParen, "Expect ')' after parameter.");
    self.consume(.LeftBrace, "Expect '{' before function body.");

    self.block();

    const func = self.endCompiler();
    self.emitBytes(.Constant, self.makeConstant(lox.Value.from(func)));
}

fn namedVariable(self: *Parser, name: Token, can_assign: bool) void {
    const Ops = struct {
        get_op: OpCode,
        set_op: OpCode,
        arg: u8,
    };

    const ops = bk: {
        if (self.resolveLocal(self.current_compiler, &name)) |a| {
            break :bk Ops{ .get_op = .GetLocal, .set_op = .SetLocal, .arg = a };
        } else if (self.resolveUpvalue(self.current_compiler, &name)) |a| {
            break :bk Ops{ .get_op = .GetUpvalue, .set_op = .SetUpvalue, .arg = a };
        } else {
            break :bk Ops{ .get_op = .GetGlobal, .set_op = .SetGlobal, .arg = self.identifierConstant(&name) };
        }
    };

    if (can_assign and self.match(.Equal)) {
        self.expression();
        self.emitBytes(ops.set_op, ops.arg);
    } else {
        self.emitBytes(ops.get_op, ops.arg);
    }
}

fn resolveLocal(self: *Parser, compiler: *const Compiler, name: *const Token) ?u8 {
    var i: usize = compiler.locals.len;
    while (i > 0) {
        i -= 1;
        const local = &compiler.locals.buf[i];
        if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
            if (local.depth == null) {
                self.errorAtPrev("Can't read local variable in its own initializer.");
            }

            return @intCast(i);
        }
    }

    return null;
}

fn resolveUpvalue(self: *Parser, compiler: *Compiler, name: *const Token) ?u8 {
    _ = self;
    _ = compiler;
    _ = name;

    // TODO:
    return null;
}

fn block(self: *Parser) void {
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        self.declaration();
    }

    self.consume(.RightBrace, "Expect '}' after block.");
}

fn expression(self: *Parser) void {
    self.parsePrecedence(.Assignment);
}

fn declaration(self: *Parser) void {
    if (self.match(.Fun)) {
        self.funDeclaration();
    } else if (self.match(.Var)) {
        self.varDeclaration();
    } else {
        self.statement();
    }

    if (self.panic_mode) self.synchronize();
}

fn funDeclaration(self: *Parser) void {
    const global = self.parseVariable("Expect function name.");
    self.markInitialized();

    self.function(.function);
    self.defineVariable(global);
}

fn varDeclaration(self: *Parser) void {
    const global = self.parseVariable("Expect variable name.");

    if (self.match(.Equal)) {
        self.expression();
    } else {
        self.emitOp(.Nil);
    }

    self.consume(.Semicolon, "Expect ';' after variable declaration.");
    self.defineVariable(global);
}

fn parseVariable(self: *Parser, comptime msg: []const u8) u8 {
    self.consume(.Identifier, msg);

    self.declareVariable();
    if (self.current_compiler.scope_depth > 0) return 0;

    return self.identifierConstant(&self.prev);
}

fn defineVariable(self: *Parser, code: u8) void {
    if (self.current_compiler.scope_depth > 0) {
        self.markInitialized();
        return;
    }

    self.emitBytes(.DefineGlobal, code);
}

fn declareVariable(self: *Parser) void {
    if (self.current_compiler.scope_depth == 0) return;

    const name = &self.prev;

    var it = std.mem.reverseIterator(self.current_compiler.locals.buf[0..self.current_compiler.locals.len]);
    while (it.nextPtr()) |local| {
        if (local.depth != null and local.depth.? < self.current_compiler.scope_depth) {
            break;
        }

        if (identifierEqual(name, &local.name)) {
            self.errorAtPrev("Already a variable with this name in this scope.");
        }
    }

    self.addLocal(name);
}

fn markInitialized(self: *Parser) void {
    if (self.current_compiler.scope_depth == 0) return;
    self.current_compiler.locals.backPtr().depth = self.current_compiler.scope_depth;
}

fn statement(self: *Parser) void {
    if (self.match(.Print)) {
        self.printStatement();
    } else if (self.match(.For)) {
        self.forStatement();
    } else if (self.match(.If)) {
        self.ifStatement();
    } else if (self.match(.While)) {
        self.whileStatement();
    } else if (self.match(.LeftBrace)) {
        self.current_compiler.beginScope();
        self.block();
        self.current_compiler.endScope();
    } else if (self.match(.Return)) {
        self.returnStatement();
    } else {
        self.expressionStatement();
    }
}

fn printStatement(self: *Parser) void {
    self.expression();
    self.consume(.Semicolon, "Expect ';' after value.");
    self.emitOp(.Print);
}

fn forStatement(self: *Parser) void {
    self.current_compiler.beginScope();
    defer self.current_compiler.endScope();

    self.consume(.LeftParen, "Expect '(' after 'for'.");

    if (self.match(.Semicolon)) {
        // no init
    } else if (self.match(.Var)) {
        self.varDeclaration();
    } else {
        self.expressionStatement();
    }

    var loop_start = self.currentChunk().codes.len;

    // condition

    var exit_jump: ?usize = null;
    if (!self.match(.Semicolon)) {
        self.expression();
        self.consume(.Semicolon, "Expect ';' after loop condition.");

        exit_jump = self.emitJump(.JumpIfFalse);
        self.emitOp(.Pop);
    }

    defer {
        if (exit_jump) |e| {
            self.patchJump(e);
            self.emitOp(.Pop);
        }
    }

    // increment
    if (!self.match(.Semicolon)) {
        const body_jump = self.emitJump(.Jump);

        const increment_start = self.currentChunk().codes.len;

        self.expression();
        self.emitOp(.Pop);

        self.consume(.RightParen, "Expect ')' after for clauses.");

        self.emitLoop(loop_start);
        loop_start = increment_start;
        self.patchJump(body_jump);
    }

    self.statement();
    self.emitLoop(loop_start);
}

fn ifStatement(self: *Parser) void {
    self.consume(.LeftParen, "Expect '(' after 'if'.");
    self.expression();
    self.consume(.RightParen, "Expect '(' after 'if'.");

    const then_jump = self.emitJump(.JumpIfFalse);
    self.emitOp(.Pop);
    self.statement();

    const else_jump = self.emitJump(.Jump);

    self.patchJump(then_jump);
    self.emitOp(.Pop);

    if (self.match(.Else)) self.statement();
    self.patchJump(else_jump);
}

fn whileStatement(self: *Parser) void {
    const loop_start = self.currentChunk().codes.len;
    self.consume(.LeftParen, "Expect '(' after 'while'.");
    self.expression();
    self.consume(.RightParen, "Expect ')' after condition.");

    const exit_jump = self.emitJump(.JumpIfFalse);
    self.emitOp(.Pop);
    self.statement();
    self.emitLoop(loop_start);

    self.patchJump(exit_jump);
    self.emitOp(.Pop);
}

fn returnStatement(self: *Parser) void {
    if(self.current_compiler.ftype == .script) {
        errorAtPrev("Can't return from top-level code.");
        return;
    }

    if(self.match(.Semicolon)) {
        self.current_compiler.addReturn(self.prev.line);
    } else {
        self.expression();
        self.consume(.Semicolon, "Expect ';' after return value.");
        self.emitOp(.Return);
    }
}

fn expressionStatement(self: *Parser) void {
    self.expression();
    self.consume(.Semicolon, "Expect ';' after expression.");
    self.emitOp(.Pop);
}
