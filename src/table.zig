const std = @import("std");
const testing = std.testing;
const lox = @import("lox.zig");

const Allocator = std.mem.Allocator;

pub const Entry = struct {
    key: ?*lox.String = null,
    value: lox.Value = lox.Value.nil,
};

const Self = @This();

const max_load = 0.75;

gpa: Allocator,
entries: []Entry,
len: usize,

pub fn init(gpa: Allocator) Self {
    return .{
        .gpa = gpa,
        .entries = &[_]Entry{},
        .len = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.gpa.free(self.entries);
    self.* = undefined;
}

fn findEntry(entries: []Entry, key: *lox.String) *Entry {
    var tombstone: ?*Entry = null;

    var idx = key.hash & (entries.len - 1); // % len
    while (true) : (idx = (idx + 1) & (entries.len - 1)) {
        const entry = &entries[idx];
        if (entry.key) |k| {
            if (k == key)
                return entry;
        } else {
            if (entry.value.isNil()) {
                // empty
                return tombstone orelse entry;
            } else {
                tombstone = tombstone orelse entry;
            }
        }
    }
}

fn adjustCapacity(self: *Self, size: usize) void {
    const new = self.gpa.alloc(Entry, size) catch @panic("OOM");
    for (new) |*e| {
        e.* = Entry{};
    }
    self.len = 0;

    for (self.entries) |entry| {
        if (entry.key) |key| {
            const dest = findEntry(new, key);
            dest.key = key;
            dest.value = entry.value;
            self.len += 1;
        }
    }

    self.gpa.free(self.entries);
    self.entries = new;
}

inline fn growCapacity(size: usize) usize {
    return if (size < 8) 8 else size * 2;
}

inline fn maxLoadFactor(self: *const Self) usize {
    return @intFromFloat(@trunc(@as(f64, @floatFromInt(self.entries.len)) * max_load));
}

pub fn set(self: *Self, key: *lox.String, val: lox.Value) bool {
    if (self.len + 1 > self.maxLoadFactor()) {
        self.adjustCapacity(growCapacity(self.entries.len));
    }

    const entry = findEntry(self.entries, key);
    const is_new_key = entry.key == null;

    if (is_new_key and entry.value.isNil()) self.len += 1;

    entry.key = key;
    entry.value = val;

    return is_new_key;
}

pub fn get(self: *const Self, key: *lox.String) ?lox.Value {
    if (self.len == 0) return null;

    const e = findEntry(self.entries, key);
    if (e.key == null) return null;

    return e.value;
}

pub fn delete(self: *Self, key: *lox.String) error{NotFound}!void {
    if (self.len == 0) return error.NotFound;

    const e = findEntry(self.entries, key);

    if (e.key == null) return error.NotFound;

    e.key = null;
    e.value = lox.Value.from(true);
}

pub fn findString(self: *const Self, buf: []const u8, hash: u32) ?*lox.String {
    if (self.len == 0) return null;

    var idx = hash & (self.entries.len - 1);

    while (true) : (idx = (idx + 1) & (self.entries.len - 1)) {
        const e = &self.entries[idx];
        if (e.key) |key| {
            if (key.hash == hash and std.mem.eql(u8, key.str, buf)) {
                return key;
            }
        } else {
            if (e.value.isNil()) return null;
        }
    }
}

pub fn removeWhite(self: *Self) void {
    for (self.entries) |*entry| {
        if (entry.key) |key| {
            if (!key.obj.marked) {
                self.delete(key) catch unreachable;
            }
        }
    }
}

test "Table" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var table = Self.init(gpa.allocator());
    defer table.deinit();

    const string = lox.String.copy("123");
    const str2 = lox.String.copy("str");

    try testing.expect(table.set(string, lox.Value.from(69)));
    try testing.expect(table.set(str2, lox.Value.from(true)));

    for (table.entries) |entry| {
        std.debug.print("{any}\n", .{entry});
    }
    try testing.expect(table.len == 2);

    const v = table.get(str2);
    try testing.expect(v != null);

    try testing.expect(v.?.asBool());
}
