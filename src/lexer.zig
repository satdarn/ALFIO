const std = @import("std");
const InputStream = @import("stream.zig").InputStream;

const type_keywords = [_][]const u8{ "int", "void", "bool", "char", "word" };
const keywords = [_][]const u8{ "break", "fn","syscall", "return", "if", "else", "while", "for" };
pub const BaseTypes = enum {
    void,
    bool,
    char,
    int,
    word,
    char_array,
};

fn as_type(base: BaseTypes) Types {
    return switch (base) {
        .int => Types{ .int = {} },
        .void => Types{ .void = {} },
        .bool => Types{ .bool = {} },
        .char => Types{ .char = {} },
        .word => Types{ .word = {} },
        .char_array => Types{ .char_array = 1 },
    };
}
pub const Types = union(enum) {
    void,
    bool,
    char,
    int,
    word,
    char_array: u64,

    pub const Void: Types = .{ .void = {} };
    pub const Bool: Types = .{ .bool = {} };
    pub const Char: Types = .{ .char = {} };
    pub const Int: Types = .{ .int = {} };
    pub const Word: Types = .{ .word = {} };

    pub fn print(self: Types) void {
        switch (self) {
            .void => std.debug.print("void", .{}),
            .bool => std.debug.print("bool", .{}),
            .char => std.debug.print("char", .{}),
            .word => std.debug.print("word", .{}),
            .int => std.debug.print("int", .{}),
            .char_array => |size| std.debug.print("char_array({})", .{size}),
        }
    }
    pub fn to_string(_type: Types, buffer: []u8) []const u8 {
        return switch (_type) {
            .void => {
                return "void";
            },
            .bool => {
                return "bool";
            },
            .char => {
                return "char";
            },
            .int => {
                return "int";
            },
            .word => {
                return "word";
            },
            .char_array => |size| {
                return std.fmt.bufPrint(buffer, "char[{d}]", .{size}) catch "char[?]";
            },
        };
    }
    pub fn size_of(self: Types) i64 {
        return switch (self) {
            .void => 0,
            .bool => 1,
            .char => 1,
            .int => 4,
            .word => 8,
            .char_array => |size| @intCast(size),
        };
    }
    pub fn eql(a: Types, b: Types) bool {
        var buf_a: [256]u8 = undefined;
        var buf_b: [256]u8 = undefined;

        const sa = a.to_string(&buf_a);
        const sb = b.to_string(&buf_b);

        return std.mem.eql(u8, sa, sb);
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
            continue;
        }
        // Handle string literals with escape sequences
        if (stream.current_char() == '"') {
            _ = stream.consume(); // consume opening quote
            // Use a temporary buffer to handle escape sequences
            var buffer = try std.ArrayList(u8).initCapacity(allocator, 16);
            defer buffer.deinit(allocator);
            while (true) {
                if (stream.current_char() == 0) {
                    return error.UnterminatedString;
                }
                if (stream.current_char() == '\\') {
                    _ = stream.consume(); // consume backslash
                    const escaped_char: u8 = switch (stream.current_char()) {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        '0' => 0,
                        else => return error.InvalidEscapeSequence,
                    };
                    try buffer.append(allocator, escaped_char);
                    _ = stream.consume(); // consume the escaped character
                } else if (stream.current_char() == '"') {
                    _ = stream.consume(); // consume closing quote
                    break;
                } else {
                    try buffer.append(allocator, stream.current_char());
                    _ = stream.consume();
                }
            }
            const string_copy = try buffer.toOwnedSlice(allocator);
            try token_list.append(allocator, .{ .string_literal = string_copy }, line);
            continue;
        }
        if (stream.current_char() == '0' and stream.next_char() == 'x') {
            const start = stream.pos;
            _ = stream.consume();
            _ = stream.consume();
            while (std.ascii.isHex(stream.current_char())) _ = stream.consume();
            const integer_literal = try std.fmt.parseInt(u64, stream.get_substring(start, stream.pos), 0);
            try token_list.append(allocator, .{ .integer_literal = integer_literal }, line);
            continue;
        }
        if (stream.current_char() == '0' and stream.next_char() == 'b') {
            const start = stream.pos;
            _ = stream.consume();
            _ = stream.consume();
            while (stream.current_char() == '1' or stream.current_char() == '0') _ = stream.consume();
            const integer_literal = try std.fmt.parseInt(u64, stream.get_substring(start, stream.pos), 0);
            try token_list.append(allocator, .{ .integer_literal = integer_literal }, line);
            continue;
        }
        if (stream.current_char() == '\'') {
            _ = stream.consume(); // consume opening quote
            if (stream.current_char() == '\\') {
                _ = stream.consume(); // consume backslash
                const escaped_char: u8 = switch (stream.current_char()) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '\'' => '\'',
                    '0' => 0,
                    else => return error.InvalidEscapeSequence,
                };
                _ = stream.consume(); // consume escaped character
                if (stream.current_char() != '\'') {
                    return error.UnterminatedCharacterLiteral;
                }
                _ = stream.consume(); // consume closing quote
                try token_list.append(allocator, .{ .character_literal = escaped_char }, line);
                continue;
            } else {
                // Regular character
                if (stream.current_char() == '\'' or stream.current_char() == 0) {
                    return error.EmptyCharacterLiteral;
                }
                const char_value = stream.current_char();
                _ = stream.consume(); // consume the character

                if (stream.current_char() != '\'') {
                    return error.UnterminatedCharacterLiteral;
                }
                _ = stream.consume(); // consume closing quote

                try token_list.append(allocator, .{ .character_literal = char_value }, line);
                continue;
            }
        }
        if (std.ascii.isDigit(stream.current_char())) {
            const start = stream.pos;
            while (std.ascii.isDigit(stream.current_char())) _ = stream.consume();
            const integer_literal = try std.fmt.parseInt(u64, stream.get_substring(start, stream.pos), 10);
            try token_list.append(allocator, .{ .integer_literal = integer_literal }, line);
            continue;
        }

        if (std.ascii.isWhitespace(stream.current_char())) {
            if (stream.current_char() == '\n' or stream.current_char() == '\r') {
                line += 1;
            }
            _ = stream.consume();
            continue;
        }
        if (std.ascii.isAlphabetic(stream.current_char()) or stream.current_char() == '_') {
            const start = stream.pos;
            while (std.ascii.isAlphabetic(stream.current_char()) or stream.current_char() == '_') _ = stream.consume();
            const ident_copy = try allocator.dupe(u8, stream.get_substring(start, stream.pos));
            try token_list.append(allocator, .{ .identifier = ident_copy }, line);
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
        }

        current = curr.next;
    }

    token_list.current_token = token_list.head;
    return token_list;
}
