const std = @import("std");
const TokenList = @import("lexer.zig").TokenList;

pub const InputStream = struct {
    data: []const u8,
    pos: u32 = 0,
    pub fn init(data: []const u8) InputStream {
        return .{ .data = data, .pos = 0 };
    }
    pub fn fromFile(allocator: std.mem.Allocator, file_path: []const u8) !InputStream {
        // Open the file
        const file = try std.fs.cwd().openFile(file_path, .{ .mode = .read_only });
        defer file.close();

        // Get file size
        const file_size = try file.getEndPos();

        // Allocate memory for file contents
        const data = try allocator.alloc(u8, file_size);
        errdefer allocator.free(data);

        // Read entire file
        const bytes_read = try file.readAll(data);
        std.debug.assert(bytes_read == file_size);

        return init(data);
    }

    pub fn deinit(self: *InputStream, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
        self.* = undefined;
    }
    pub fn consume(self: *InputStream) u8 {
        if (self.data.len > self.pos) {
            self.pos += 1;
            return self.data[self.pos - 1];
        }
        return 0;
    }
    pub fn current_char(self: *InputStream) u8 {
        if (self.data.len > self.pos) {
            return self.data[self.pos];
        }
        return 0;
    }
    pub fn consume_substring(self: *InputStream, length: u64) []const u8 {
        // Gets the substring from the current pos of a certain length.
        // consumes the characters
        const substring = self.data[self.pos .. self.pos + length];
        self.pos += length;
        return substring;
    }
    pub fn get_substring(self: *InputStream, start: u64, end: u64) []const u8 {
        const substring = self.data[start..end];
        return substring;
    }
};
