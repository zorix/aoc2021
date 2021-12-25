const std = @import("std");
const io = std.io;
const fs = std.fs;
const mem = std.mem;

const Alu = struct {
  w: i64 = 0,
  x: i64 = 0,
  y: i64 = 0,
  z: i64 = 0,

  pub fn getRegOrNumber(self: Alu, regOrNumber: []const u8) i64 {
    if (mem.eql(u8, regOrNumber, "w")) {
      return self.w;
    }
    if (mem.eql(u8, regOrNumber, "x")) {
      return self.x;
    }
    if (mem.eql(u8, regOrNumber, "y")) {
      return self.y;
    }
    if (mem.eql(u8, regOrNumber, "z")) {
      return self.z;
    }
    return std.fmt.parseInt(i64, regOrNumber[0..regOrNumber.len], 10) catch |err| switch (err) {
      error.Overflow => @panic("Overflow"),
      error.InvalidCharacter => @panic("InvalidCharacter"),
    };
  }

  pub fn add(self: Alu, l: []const u8, r: []const u8) Alu {
    const value = getRegOrNumber(self, r);

    var newAlu = Alu {
      .w = self.w,
      .x = self.x,
      .y = self.y,
      .z = self.z,
    };
  
    if (mem.eql(u8, l, "w")) {
      newAlu.w += value;
    } else if (mem.eql(u8, l, "x")) {
      newAlu.x += value;
    } else if (mem.eql(u8, l, "y")) {
      newAlu.y += value;
    } else if (mem.eql(u8, l, "z")) {
      newAlu.z += value;
    } else {
      @panic("Invalid register");
    }
    return newAlu;
  }

  pub fn mul(self: Alu, l: []const u8, r: []const u8) Alu {
    const value = getRegOrNumber(self, r);

    var newAlu = Alu {
      .w = self.w,
      .x = self.x,
      .y = self.y,
      .z = self.z,
    };
  
    if (mem.eql(u8, l, "w")) {
      newAlu.w *= value;
    } else if (mem.eql(u8, l, "x")) {
      newAlu.x *= value;
    } else if (mem.eql(u8, l, "y")) {
      newAlu.y *= value;
    } else if (mem.eql(u8, l, "z")) {
      newAlu.z *= value;
    } else {
      @panic("Invalid register");
    }
    return newAlu;
  }

  pub fn div(self: Alu, l: []const u8, r: []const u8) Alu {
    const value = getRegOrNumber(self, r);

    var newAlu = Alu {
      .w = self.w,
      .x = self.x,
      .y = self.y,
      .z = self.z,
    };
  
    if (mem.eql(u8, l, "w")) {
      newAlu.w = @divTrunc(newAlu.w, value);
    } else if (mem.eql(u8, l, "x")) {
      newAlu.x = @divTrunc(newAlu.x, value);
    } else if (mem.eql(u8, l, "y")) {
      newAlu.y = @divTrunc(newAlu.y, value);
    } else if (mem.eql(u8, l, "z")) {
      newAlu.z = @divTrunc(newAlu.z, value);
    } else {
      @panic("Invalid register");
    }
    return newAlu;
  }

  pub fn mod(self: Alu, l: []const u8, r: []const u8) Alu {
    const value = getRegOrNumber(self, r);

    var newAlu = Alu {
      .w = self.w,
      .x = self.x,
      .y = self.y,
      .z = self.z,
    };
  
    if (mem.eql(u8, l, "w")) {
      newAlu.w = @rem(newAlu.w, value);
    } else if (mem.eql(u8, l, "x")) {
      newAlu.x = @rem(newAlu.x, value);
    } else if (mem.eql(u8, l, "y")) {
      newAlu.y = @rem(newAlu.y, value);
    } else if (mem.eql(u8, l, "z")) {
      newAlu.z = @rem(newAlu.z, value);
    } else {
      @panic("Invalid register");
    }
    return newAlu;
  }

  pub fn eql(self: Alu, l: []const u8, r: []const u8) Alu {
    const value = getRegOrNumber(self, r);

    var newAlu = Alu {
      .w = self.w,
      .x = self.x,
      .y = self.y,
      .z = self.z,
    };
  
    if (mem.eql(u8, l, "w")) {
      newAlu.w = if (newAlu.w == value) 1 else 0;
    } else if (mem.eql(u8, l, "x")) {
      newAlu.x = if (newAlu.x == value) 1 else 0;
    } else if (mem.eql(u8, l, "y")) {
      newAlu.y = if (newAlu.y == value) 1 else 0;
    } else if (mem.eql(u8, l, "z")) {
      newAlu.z = if (newAlu.z == value) 1 else 0;
    } else {
      @panic("Invalid register");
    }
    return newAlu;
  }

  pub fn set(self: Alu, reg: []const u8, value: i64) Alu {
    var newAlu = Alu {
      .w = self.w,
      .x = self.x,
      .y = self.y,
      .z = self.z,
    };
  
    if (mem.eql(u8, reg, "w")) {
      newAlu.w = value;
    } else if (mem.eql(u8, reg, "x")) {
      newAlu.x = value;
    } else if (mem.eql(u8, reg, "y")) {
      newAlu.y = value;
    } else if (mem.eql(u8, reg, "z")) {
      newAlu.z = value;
    } else {
      @panic("Invalid register");
    }
    return newAlu;
  }
};

fn load(filename: []const u8) !std.ArrayList([]u8) {
  var file = try fs.cwd().openFile(filename, .{});
  defer file.close();
  
  const reader = io.bufferedReader(file.reader()).reader();

  const allocator = std.heap.page_allocator;
  var lines = std.ArrayList([]u8).init(allocator);

  var buf: [1024]u8 = undefined;
  while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
    const str:[]u8 = try allocator.alloc(u8, line.len);
    mem.copy(u8, str, line);
    try lines.append(str);
  }

  return lines;
}

const IndexContext = struct {
  pub fn eql(self: IndexContext, a: State, b: State) bool {
    _ = self;
    return a.op_index == b.op_index and a.alu.w == b.alu.w
       and a.alu.x == b.alu.x and a.alu.y == b.alu.y and a.alu.z == b.alu.z;
  }

  pub fn hash(self: IndexContext, x: State) u64 {
    _ = self;
    return @intCast(u64, x.op_index) + @intCast(u64, x.alu.w) * 1000
        + @intCast(u64, x.alu.x) * 100000 + @intCast(u64, x.alu.y) * 1000000
        + @intCast(u64, x.alu.z) * 10000000;
  }
};

const State = struct {
  alu: Alu,
  op_index: usize,

  pub fn eql(self: State, a: State, b: State) bool {
    _ = self;
    return a.op_index == b.op_index and a.alu.w == b.alu.w
       and a.alu.x == b.alu.x and a.alu.y == b.alu.y and a.alu.z == b.alu.z;
  }

  pub fn hash(self: State, x: State) u64 {
    _ = self;
    return @intCast(u64, x.op_index) + @intCast(u64, x.alu.w) * 1000
        + @intCast(u64, x.alu.x) * 100000 + @intCast(u64, x.alu.y) * 1000000
        + @intCast(u64, x.alu.z) * 10000000;
  }
};

const Cache = std.HashMapUnmanaged(State, ?IterResult, IndexContext, std.hash_map.default_max_load_percentage);
var cache: Cache = .{};

const IterResult = struct {
  alu: Alu,
  val: i64,
};

fn iter(ops: std.ArrayList([]u8), op_index: usize, in_alu: Alu, values: [9]u8) ?IterResult {
  //const stdout = io.getStdOut().writer();
  var index = op_index;
  var alu = in_alu;

  {
    const state = State{
      .op_index = index,
      .alu = in_alu,
    };
    const result = cache.get(state);
    if (result != null) {
      return result.?;
    }
  }

  while (index < ops.items.len) {
    const opLine = ops.items[index];
    index += 1;
    var tokens = mem.split(u8, opLine, " ");
    const op = tokens.next().?;

    if (alu.z > 10000000) {
      return null;
    }

    if (mem.eql(u8, op, "inp")) {
      const reg = tokens.next().?;
      for (values) |val| {
        //stdout.print("{} {}\n", .{op_index, val}) catch |err| switch (err) {
        //  else => @panic("ioerror"),
        //};
        const new_alu = alu.set(reg, val);
        const result = iter(ops, index, new_alu, values);
        const state = State{
          .op_index = index,
          .alu = new_alu,
        };
        cache.put(std.heap.page_allocator, state, result) catch |err| switch (err) {
          else => @panic("cache.put"),
        };
        if (result != null) {
          return IterResult{
            .alu = new_alu,
            .val = result.?.val * 10 + val,
          };
        }
      }
      return null;
    } else if (mem.eql(u8, op, "add")) {
      alu = alu.add(tokens.next().?, tokens.next().?);
    } else if (mem.eql(u8, op, "mul")) {
      alu = alu.mul(tokens.next().?, tokens.next().?);
    } else if (mem.eql(u8, op, "div")) {
      alu = alu.div(tokens.next().?, tokens.next().?);
    } else if (mem.eql(u8, op, "mod")) {
      alu = alu.mod(tokens.next().?, tokens.next().?);
    } else if (mem.eql(u8, op, "eql")) {
      alu = alu.eql(tokens.next().?, tokens.next().?);
    } else {
      @panic("unexpected");
    }
  }

  if (alu.z == 0)
  {
    return IterResult{
      .alu = alu,
      .val = 0,
    };
  }
  return null;
}

fn intToString(int: i64, buf: []u8) ![]u8 {
    return try std.fmt.bufPrint(buf, "{}", .{int});
}

test "Part A" {
  cache = .{};

  const stdout = io.getStdOut().writer();
  try stdout.print("\n", .{});
  var ops = try load("input.txt");
  var alu = Alu{};

  const values = [9]u8{9, 8, 7, 6, 5, 4, 3, 2, 1};
  const result = iter(ops, 0, alu, values);

  var buf: [32]u8 = undefined;
  const num = try intToString(result.?.val, &buf);
  mem.reverse(u8, num);
  
  try stdout.print("Part A: {s}\n", .{num});
}

test "Part B" {
  cache = .{};

  const stdout = io.getStdOut().writer();
  try stdout.print("\n", .{});
  var ops = try load("input.txt");
  var alu = Alu{};

  const values = [9]u8{1, 2, 3, 4, 5, 6, 7, 8, 9};
  const result = iter(ops, 0, alu, values);
  
  var buf: [32]u8 = undefined;
  const num = try intToString(result.?.val, &buf);
  mem.reverse(u8, num);
  
  try stdout.print("Part B: {s}\n", .{num});
}
