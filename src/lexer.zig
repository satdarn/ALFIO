const std = @import("std");
const InputStream = @import("stream.zig").InputStream;

const type_keywords = [_][]const u8{ "int", "void", "bool", "char" };
const keywords = [_][]const u8{ "fn", "print", "byte_in", "byte_out", "return", "if", "else", "while" };
pub const BaseTypes = enum {
    int,
    void,
    bool,
    char,
    char_array,
};

fn as_type(base: BaseTypes) Types {
    return switch (base) {
        .int => Types{ .int = {} },
        .void => Types{ .void = {} },
        .bool => Types{ .bool = {} },
        .char => Types{ .char = {} },
        .char_array => Types{ .char_array = 1 },
    };
}
pub const Types = union(BaseTypes) {
    int,
    void,
    bool,
    char,
    char_array: u64,
    pub fn print(self: Types) void {
        switch (self) {
            .int => std.debug.print("int", .{}),
            .void => std.debug.print("void", .{}),
            .bool => std.debug.print("bool", .{}),
            .char => std.debug.print("char", .{}),
            .char_array => |size| std.debug.print("char_array({})", .{size}),
        }
    }
};

pub const Token = struct {
    const Data = union(enum) {
        character: u8,
        double_character: [2]u8,
        identifier: []const u8,
        integer_literal: u64,
        character_literal: u8,
        string_literal: []const u8,
        type_keyword: Types,
        keyword: []const u8,
    };
    line: u32,
    data: Data,
    next: ?*Token = null,
    prev: ?*Token = null,
    pub fn print_list(self: *Token) void {
        switch (self.data) {
            .character => std.debug.print("character({c}) -> ", .{self.data.character}),
            .double_character => std.debug.print("characters({s}) -> ", .{self.data.double_character}),
            .identifier => std.debug.print("identifier({s}) -> ", .{self.data.identifier}),
            .integer_literal => std.debug.print("integer literal({}) -> ", .{self.data.integer_literal}),
            .character_literal => std.debug.print("character literal({c}) -> ", .{self.data.character_literal}),
            .string_literal => std.debug.print("string_literal ({s}) ->", .{self.data.string_literal}),
            .type_keyword => std.debug.print("type_keyword({s}) ->", .{@tagName(self.data.type_keyword)}),
            .keyword => std.debug.print("keyword({s}) ->", .{self.data.keyword}),
        }
        if (self.next) |next| next.print_list() else std.debug.print("END OF FILE()\n", .{});
    }
    pub fn print(self: *Token) void {
        switch (self.data) {
            .character => std.debug.print("character({c}) ", .{self.data.character}),
            .double_character => std.debug.print("characters({s}) ", .{self.data.double_character}),
            .identifier => std.debug.print("identifier({s})", .{self.data.identifier}),
            .integer_literal => std.debug.print("integer literal({})", .{self.data.integer_literal}),
            .character_literal => std.debug.print("character literal({c})", .{self.data.character_literal}),
            .string_literal => std.debug.print("string literal({s})", .{self.data.string_literal}),
            .type_keyword => std.debug.print("type_keyword({s}) ", .{@tagName(self.data.type_keyword)}),
            .keyword => std.debug.print("keyword({s})", .{self.data.keyword}),
        }
    }
    pub fn deinit(self: *Token, allocator: std.mem.Allocator) void {
        if (self.next) |next| {
            next.deinit(allocator);
        }
        if (self.data == .identifier) allocator.free(self.data.identifier);
        if (self.data == .string_literal) allocator.free(self.data.string_literal);
        allocator.destroy(self);
    }
};

pub const TokenList = struct {
    head: ?*Token,
    end: ?*Token,
    len: u32,
    current_token: ?*Token,
    pub const default: TokenList = .{ .head = null, .end = null, .len = 0, .current_token = null };
    pub fn append(self: *TokenList, allocator: std.mem.Allocator, data: Token.Data, line: u32) !void {
        const node = try allocator.create(Token);
        node.* = Token{ .data = data, .line = line, .prev = self.end, .next = null };

        if (self.end) |end| {
            end.next = node;
        } else {
            self.head = node;
            self.current_token = node;
        }

        self.end = node;
        self.len += 1;
    }
    pub fn isPeekChar(self: *TokenList, char: u8) bool {
        _ = self.peek() orelse return false;
        return self.peek().?.data == .character and self.peek().?.data.character == char;
    }
    pub fn isPeekDouble(self: *TokenList, chars: []const u8) bool {
        _ = self.peek() orelse return false;
        return self.peek().?.data == .double_character and std.mem.eql(u8, &self.peek().?.data.double_character, chars);
    }
    pub fn isPeekIdent(self: *TokenList, ident: []const u8) bool {
        _ = self.peek() orelse return false;
        return self.peek().?.data == .identifier and std.mem.eql(u8, self.peek().?.data.identifier, ident);
    }

    pub fn isPeekKeyword(self: *TokenList, keyword: []const u8) bool {
        _ = self.peek() orelse return false;
        return self.peek().?.data == .keyword and std.mem.eql(u8, self.peek().?.data.keyword, keyword);
    }

    pub fn isPeekTypeKeyword(self: *TokenList, type_keyword: []const u8) bool {
        _ = self.peek() orelse return false;
        return self.peek().?.data == .type_keyword and std.mem.eql(u8, self.peek().?.data.type_keyword, type_keyword);
    }

    pub fn isPeekAnyTypeKeyword(self: *TokenList) bool {
        _ = self.peek() orelse return false;
        return self.peek().?.data == .type_keyword;
    }

    pub fn peek(self: *TokenList) ?*Token {
        if (self.current_token) |next| {
            return next;
        } else {
            return null;
        }
    }
    pub fn peek_n(self: *TokenList, n: u64) ?*Token {
        var curr = self.current_token;
        for (0..n) |_| {
            if (curr.?.next) |next| {
                curr = next;
            } else {
                return null;
            }
        }
        return curr;
    }
    pub fn consume(self: *TokenList) ?*Token {
        if (self.current_token) |curr| {
            self.current_token = curr.next;
            return curr;
        } else {
            return null;
        }
    }
    pub fn deinit(self: *TokenList, allocator: std.mem.Allocator) void {
        if (self.len > 0) {
            self.head.?.deinit(allocator);
        }
    }
};

pub fn tokenize(allocator: std.mem.Allocator, stream: *InputStream) !TokenList {
    var token_list: TokenList = .default;
    var line: u32 = 0;
    while (true) {
        if (stream.current_char() == 0) {
            break;
        }
        if (stream.current_char() == '#') {
            while (stream.current_char() != '\n' and stream.current_char() != '\r') {
                _ = stream.consume();
            }
            line += 1;
        }
        if (stream.current_char() == '"') {
            const start = stream.pos;
            _ = stream.consume();
            while (stream.current_char() != '"' and stream.current_char() != 0) _ = stream.consume();
            _ = stream.consume();
            const string_copy = try allocator.dupe(u8, stream.get_substring(start, stream.pos));
            try token_list.append(allocator, .{ .string_literal = string_copy }, line);
        }

        if (std.ascii.isWhitespace(stream.current_char())) {
            if (stream.current_char() == '\n' or stream.current_char() == '\r') {
                line += 1;
            }
            _ = stream.consume();
            continue;
        }
        if (std.ascii.isAlphabetic(stream.current_char())) {
            const start = stream.pos;
            while (std.ascii.isAlphabetic(stream.current_char()) or stream.current_char() == '_') _ = stream.consume();
            const ident_copy = try allocator.dupe(u8, stream.get_substring(start, stream.pos));
            try token_list.append(allocator, .{ .identifier = ident_copy }, line);
            continue;
        }
        if (std.ascii.isDigit(stream.current_char())) {
            const start = stream.pos;
            while (std.ascii.isDigit(stream.current_char())) _ = stream.consume();
            var integer_literal: u64 = 0;
            for (stream.get_substring(start, stream.pos), 0..) |digit, i| {
                integer_literal += (digit - '0') * std.math.pow(usize, 10, stream.pos - start - 1 - i);
            }
            try token_list.append(allocator, .{ .integer_literal = integer_literal }, line);
            continue;
        } else {
            try token_list.append(allocator, .{ .character = stream.consume() }, line);
            continue;
        }
    }
    var current = token_list.head;
    while (current) |curr| {
        if (curr.data == .identifier) {
            for (type_keywords) |keyword| {
                if (std.mem.eql(u8, curr.data.identifier, keyword)) {
                    if (curr.next) |next| {
                        if (next.data == .character and next.data.character == '[') {
                            if (next.next) |next_next| {
                                if (next_next.data == .integer_literal) {
                                    if (next_next.next) |next_next_next| {
                                        if (next_next_next.data == .character and next_next_next.data.character == ']') {
                                            const new_token = try allocator.create(Token);
                                            const array_length = next_next.data.integer_literal;
                                            const _type = Types{ .char_array = array_length };
                                            new_token.data = .{ .type_keyword = _type };
                                            new_token.line = curr.line;
                                            new_token.prev = curr.prev;
                                            new_token.next = next_next_next.next;

                                            if (curr.prev) |prev| {
                                                prev.next = new_token;
                                            } else {
                                                token_list.head = new_token;
                                            }
                                            if (next_next_next.next) |nxt| {
                                                nxt.prev = new_token;
                                            } else {
                                                token_list.end = new_token;
                                            }
                                            if (token_list.current_token == curr or
                                                token_list.current_token == next or
                                                token_list.current_token == next_next or
                                                token_list.current_token == next_next_next)
                                            {
                                                token_list.current_token = new_token;
                                            }
                                            allocator.free(curr.data.identifier);
                                            allocator.destroy(curr);
                                            allocator.destroy(next);
                                            allocator.destroy(next_next);
                                            allocator.destroy(next_next_next);
                                            current = new_token.next;
                                            token_list.len -= 3; // We removed four tokens and added one
                                            break;
                                        }
                                    }
                                }
                            }
                        } else {
                            const _type = as_type(std.meta.stringToEnum(BaseTypes, curr.data.identifier) orelse return error.InvalidToken);
                            allocator.free(curr.data.identifier);
                            curr.data = .{ .type_keyword = _type };
                            break;
                        }
                    }
                }
            } else for (keywords) |keyword| {
                if (std.mem.eql(u8, curr.data.identifier, keyword)) {
                    allocator.free(curr.data.identifier);
                    curr.data = .{ .keyword = keyword };
                    break;
                }
            }
        }
        if (curr.data == .character) {
            const first_char = curr.data.character;
            if (first_char == '=' or first_char == '!' or first_char == '<' or first_char == '>') {
                if (curr.next) |next| {
                    if (next.data == .character and next.data.character == '=') {
                        const new_token = try allocator.create(Token);
                        new_token.data = .{ .double_character = [2]u8{ first_char, '=' } };
                        new_token.line = curr.line;
                        new_token.prev = curr.prev;
                        new_token.next = next.next;
                        if (curr.prev) |prev| {
                            prev.next = new_token;
                        } else {
                            token_list.head = new_token;
                        }
                        if (next.next) |next_next| {
                            next_next.prev = new_token;
                        } else {
                            token_list.end = new_token;
                        }
                        if (token_list.current_token == curr or token_list.current_token == next) {
                            token_list.current_token = new_token;
                        }
                        allocator.destroy(curr);
                        allocator.destroy(next);
                        current = new_token.next;
                        token_list.len -= 1; // We removed two tokens and added one
                        continue;
                    }
                }
            }
            if (first_char == '&') {
                if (curr.next) |next| {
                    if (next.data == .character and next.data.character == '&') {
                        const new_token = try allocator.create(Token);
                        new_token.data = .{ .double_character = [2]u8{ '&', '&' } };
                        new_token.prev = curr.prev;
                        new_token.next = next.next;
                        if (curr.prev) |prev| {
                            prev.next = new_token;
                        } else {
                            token_list.head = new_token;
                        }
                        if (next.next) |next_next| {
                            next_next.prev = new_token;
                        } else {
                            token_list.end = new_token;
                        }
                        if (token_list.current_token == curr or token_list.current_token == next) {
                            token_list.current_token = new_token;
                        }
                        allocator.destroy(curr);
                        allocator.destroy(next);
                        current = new_token.next;
                        token_list.len -= 1; // We removed two tokens and added one
                        continue;
                    }
                }
            }
            if (first_char == '|') {
                if (curr.next) |next| {
                    if (next.data == .character and next.data.character == '|') {
                        const new_token = try allocator.create(Token);
                        new_token.data = .{ .double_character = [2]u8{ '|', '|' } };
                        new_token.prev = curr.prev;
                        new_token.next = next.next;
                        if (curr.prev) |prev| {
                            prev.next = new_token;
                        } else {
                            token_list.head = new_token;
                        }
                        if (next.next) |next_next| {
                            next_next.prev = new_token;
                        } else {
                            token_list.end = new_token;
                        }
                        if (token_list.current_token == curr or token_list.current_token == next) {
                            token_list.current_token = new_token;
                        }
                        allocator.destroy(curr);
                        allocator.destroy(next);
                        current = new_token.next;
                        token_list.len -= 1; // We removed two tokens and added one
                        continue;
                    }
                }
            }
            if (first_char == '\'') {
                if (curr.next) |next| {
                    if (next.data == .identifier and next.data.identifier.len == 1) {
                        if (next.next) |next_next| {
                            if (next_next.data == .character and next_next.data.character == '\'') {
                                const new_token = try allocator.create(Token);
                                new_token.data = .{ .character_literal = next.data.identifier[0] };
                                new_token.prev = curr.prev;
                                new_token.next = next_next.next;
                                if (curr.prev) |prev| {
                                    prev.next = new_token;
                                } else {
                                    token_list.head = new_token;
                                }
                                if (next_next.next) |next_next_next| {
                                    next_next_next.prev = new_token;
                                } else {
                                    token_list.end = new_token;
                                }
                                if (token_list.current_token == curr or token_list.current_token == next or token_list.current_token == next_next) {
                                    token_list.current_token = new_token;
                                }
                                allocator.destroy(curr);
                                allocator.free(next.data.identifier);
                                allocator.destroy(next);
                                allocator.destroy(next_next);
                                current = new_token.next;
                                token_list.len -= 2; // We removed three tokens and added one
                                continue;
                            }
                        }
                    }
                }
            }
        }

        current = curr.next;
    }

    token_list.current_token = token_list.head;
    return token_list;
}
