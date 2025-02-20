const std = @import("std");
const testing = std.testing;

pub const TokenType = enum {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // 1 or 2 char
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8 = "",
    line: u32 = 0,
};

start: [*]const u8,
current: [*]const u8,
source: []const u8,
line: u32,

const Self = @This();

pub fn init(source: []const u8) Self {
    return .{
        .start = source.ptr,
        .current = source.ptr,
        .source = source,
        .line = 1,
    };
}

pub fn scan(self: *Self) Token {
    self.skipWhitespace();

    self.start = self.current;

    if (self.eof()) return self.makeToken(.Eof);

    const c = self.advance();

    const t: TokenType = switch (c) {
        'a'...'z', 'A'...'Z', '_' => self.identifier(),
        '0'...'9' => self.number(),
        '(' => .LeftParen,
        ')' => .RightParen,
        '{' => .LeftBrace,
        '}' => .RightBrace,
        ';' => .Semicolon,
        ',' => .Comma,
        '.' => .Dot,
        '+' => .Plus,
        '-' => .Minus,
        '*' => .Star,
        '/' => .Slash,
        '!' => if (self.match('=')) .BangEqual else .Bang,
        '=' => if (self.match('=')) .EqualEqual else .Equal,
        '>' => if (self.match('=')) .GreaterEqual else .Greater,
        '<' => if (self.match('=')) .LessEqual else .Less,

        '"' => self.string() catch return self.errorToken("Unterminated string."),
        else => return self.errorToken("Unexpected character."),
    };

    return self.makeToken(t);
}

fn number(self: *Self) TokenType {
    while (isDigit(self.peek())) self.current += 1;

    if (self.peek() == '.' and isDigit(self.peekNext())) {
        self.current += 1;

        while (isDigit(self.peek())) self.current += 1;
    }

    return .Number;
}

fn string(self: *Self) error{UnterminatedString}!TokenType {
    while (self.peek() != '"' and !self.eof()) {
        if (self.peek() == '\n') self.line += 1;

        self.current += 1;
    }

    if (self.eof()) return error.UnterminatedString;
    self.current += 1; // "

    return .String;
}

fn identifier(self: *Self) TokenType {
    while (isAlpha(self.peek()) or isDigit(self.peek())) {
        self.current += 1;
    }

    const t: TokenType = bk: switch (self.start[0]) {
        'a' => self.keyword(1, .And),
        'c' => self.keyword(1, .Class),
        'e' => self.keyword(1, .Else),
        'f' => if (self.current - self.start > 1)
            switch (self.start[1]) {
                'a' => self.keyword(2, .False),
                'o' => self.keyword(2, .For),
                'u' => self.keyword(2, .Fun),
                else => break :bk null,
            }
        else
            null,
        'i' => self.keyword(1, .If),
        'n' => self.keyword(1, .Nil),
        'o' => self.keyword(1, .Or),
        'p' => self.keyword(1, .Print),
        'r' => self.keyword(1, .Return),
        's' => self.keyword(1, .Super),
        't' => if (self.current - self.start > 1)
            switch (self.start[1]) {
                'h' => self.keyword(2, .This),
                'r' => self.keyword(2, .True),
                else => break :bk null,
            }
        else
            null,
        'v' => self.keyword(1, .Var),
        'w' => self.keyword(1, .While),
        else => null,
    } orelse .Identifier;

    return t;
}

fn keyword(self: *const Self, comptime offset: usize, comptime t: TokenType) TokenType {
    const rest = @tagName(t)[offset..];
    const len = self.current - self.start;
    return if (std.mem.eql(u8, self.start[offset..len], rest)) t else .Identifier;
}

fn makeToken(self: *const Self, t: TokenType) Token {
    return .{
        .type = t,
        .line = self.line,
        .lexeme = self.start[0..(self.current - self.start)],
    };
}

fn errorToken(self: *const Self, comptime msg: []const u8) Token {
    return .{
        .type = .Error,
        .line = self.line,
        .lexeme = msg,
    };
}

fn eof(self: *const Self) bool {
    return (self.current - self.source.ptr) >= self.source.len;
}

fn peek(self: *const Self) u8 {
    return self.current[0];
}

fn peekNext(self: *const Self) u8 {
    return if (self.eof()) 0 else self.current[1];
}

fn advance(self: *Self) u8 {
    const r = self.current[0];
    self.current += 1;
    return r;
}

fn match(self: *Self, expect: u8) bool {
    if (self.eof()) return false;

    if (self.current[0] != expect) return false;

    self.current += 1;
    return true;
}

fn skipWhitespace(self: *Self) void {
    sw: switch (self.peek()) {
        ' ', '\r', '\t' => {
            self.current += 1;
            continue :sw self.peek();
        },
        '\n' => {
            self.line += 1;
            self.current += 1;
            continue :sw self.peek();
        },
        '/' => if (self.peekNext() == '/') {
            while (self.peek() != '\n' and !self.eof()) self.current += 1;
            continue :sw self.peek();
        } else {
            return;
        },
        else => return,
    }
}

fn isAlpha(c: u8) bool {
    return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

test "Lexer basic" {
    const src = "abcd";
    var lexer = Self.init(src);
    try testing.expectEqual(lexer.peek(), 'a');
    try testing.expectEqual(lexer.peekNext(), 'b');
    try testing.expectEqual(lexer.advance(), 'a');
    try testing.expectEqual(lexer.peek(), 'b');
    try testing.expectEqual(lexer.advance(), 'b');
    try testing.expectEqual(lexer.advance(), 'c');
    try testing.expectEqual(lexer.peek(), 'd');
    try testing.expectEqual(lexer.peekNext(), 0);
    try testing.expectEqual(lexer.advance(), 'd');
    try testing.expect(lexer.eof());
}

test "Lexer utils" {
    const src = "abc  \t 1\n//123\nZ";
    var lexer = Self.init(src);

    try testing.expect(lexer.match('a'));
    try testing.expect(lexer.match('b'));
    try testing.expect(lexer.match('c'));
    lexer.skipWhitespace();
    try testing.expect(lexer.match('1'));
    lexer.skipWhitespace();
    try testing.expectEqual(lexer.line, 3);
    try testing.expect(lexer.match('Z'));
}

test "Lexer scan basic" {
    const src = "()!==*";
    var lexer = Self.init(src);

    const t = lexer.scan();
    try testing.expectEqual(t.type, TokenType.LeftParen);
    try testing.expectEqualStrings(t.lexeme, "(");
    try testing.expectEqual((lexer.scan()).type, TokenType.RightParen);
    try testing.expectEqual((lexer.scan()).type, TokenType.BangEqual);
    try testing.expectEqual((lexer.scan()).type, TokenType.Equal);
    try testing.expectEqual((lexer.scan()).type, TokenType.Star);

    try testing.expectEqual((lexer.scan()).type, TokenType.Eof);
}

test "Lexer scan string number" {
    const src = "\"string\"42.69";
    var lexer = Self.init(src);

    const str = lexer.scan();
    try testing.expectEqual(str.type, TokenType.String);
    try testing.expectEqualStrings(str.lexeme, "\"string\"");

    const num = lexer.scan();
    try testing.expectEqual(num.type, TokenType.Number);
    try testing.expectEqualStrings(num.lexeme, "42.69");

    try testing.expectEqual((lexer.scan()).type, TokenType.Eof);
}

test "Lexer scan keyword" {
    const kw = [_]TokenType{ .And, .This, .True, .False, .Fun, .For, .If, .Class };

    const lexeme = comptime bk: {
        var r: [kw.len][]const u8 = undefined;
        for (kw, 0..) |k, i| {
            r[i] = .{std.ascii.toLower(@tagName(k)[0])} ++ @tagName(k)[1..];
        }
        break :bk r;
    };

    const src = comptime bk: {
        var r: []const u8 = "";
        for (lexeme) |str| {
            r = r ++ str ++ " ";
        }
        break :bk r;
    };

    var lexer = Self.init(src);

    inline for (kw, lexeme) |k, s| {
        const token = lexer.scan();
        try testing.expectEqual(token.type, k);
        try testing.expectEqualStrings(token.lexeme, s);
    }

    try testing.expectEqual((lexer.scan()).type, TokenType.Eof);
}
