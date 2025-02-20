pub const Value = @import("value.zig").Value;

pub const Object = @import("object.zig");
pub const String = Object.String;
pub const Function = Object.Function;
pub const Native = Object.Native;
pub const Closure = Object.Closure;
pub const Upvalue = Object.Upvalue;
pub const Class = Object.Class;
pub const Instance = Object.Instance;
pub const BoundMethod = Object.BoundMethod;
