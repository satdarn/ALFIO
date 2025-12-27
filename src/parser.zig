const std = @import("std");
const Token = @import("lexer.zig").Token;
const Types = @import("lexer.zig").Types;
const TokenList = @import("lexer.zig").TokenList;
const Node = @import("nodes.zig").Node;
const InputStream = @import("stream.zig").InputStream;

pub const ParserError = error{
    SyntaxError,
    UnexpectedToken,
    MissingSemicolon,
    MissingParenthesis,
    MissingBrace,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    token_list: *TokenList,
    had_error: bool = false,

    pub fn init(allocator: std.mem.Allocator, token_list: *TokenList) Parser {
        return Parser{
            .allocator = allocator,
            .token_list = token_list,
        };
    }

    pub fn fail(self: *Parser, comptime format: []const u8, args: anytype) void {
        if (self.had_error) return;
        self.had_error = true;

        const current_token = self.token_list.current_token;

        if (current_token == null) {
            // EOF error
            std.debug.print("Error at EOF: " ++ format ++ "\n", args);
            return;
        }

        const token = current_token.?;
        std.debug.print("Error", .{});

        std.debug.print(" on line {}: \n\t", .{token.line});

        switch (token.data) {
            .string_literal => std.debug.print("'{s}'", .{token.data.string_literal}),
            .integer_literal => std.debug.print("'{d}'", .{token.data.integer_literal}),
            .character_literal => std.debug.print("'{c}'", .{token.data.character_literal}),
            .character => std.debug.print("'{c}'", .{token.data.character}),
            .double_character => std.debug.print("'{s}'", .{token.data.double_character}),
            .identifier => std.debug.print("'{s}'", .{token.data.identifier}),
            .type_keyword => std.debug.print("{s}", .{@tagName(token.data.type_keyword)}),
            .keyword => std.debug.print("{s}", .{token.data.keyword}),
        }

        std.debug.print(" - " ++ format ++ "\n", args);
        return;
    }

    pub fn ok(self: *Parser) bool {
        return !self.had_error;
    }

    pub fn expectTypeKeyword(self: *Parser, expected: []const u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekTypeKeyword(expected)) {
            _ = self.token_list.consume();
            return true;
        }
        self.fail("Expected '{s}'", .{expected});
        return false;
    }

    pub fn expectKeyword(self: *Parser, expected: []const u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekKeyword(expected)) {
            _ = self.token_list.consume();
            return true;
        }
        self.fail("Expected '{s}'", .{expected});
        return false;
    }

    pub fn expectIdent(self: *Parser, expected: []const u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekIdent(expected)) {
            _ = self.token_list.consume();
            return true;
        }
        self.fail("Expected '{s}'", .{expected});
        return false;
    }

    pub fn expectChar(self: *Parser, char: u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekChar(char)) {
            _ = self.token_list.consume();
            return true;
        }
        self.fail("Expected '{c}'", .{char});
        return false;
    }

    pub fn expectAnyTypeKeyword(self: *Parser) ?Types {
        if (!self.ok()) return null;
        const token = self.token_list.peek() orelse {
            self.fail("Expected type_keyword", .{});
            return null;
        };
        if (token.data == .type_keyword) {
            _ = self.token_list.consume();
            return token.data.type_keyword;
        }
        self.fail("Expected type_keyword", .{});
        return null;
    }

    pub fn expectAnyIdent(self: *Parser) ?[]const u8 {
        if (!self.ok()) return null;
        const token = self.token_list.peek() orelse {
            self.fail("Expected identifier", .{});
            return null;
        };
        if (token.data == .identifier) {
            _ = self.token_list.consume();
            return token.data.identifier;
        }
        self.fail("Expected identifier", .{});
        return null;
    }

    pub fn consumeIfTypeKeyword(self: *Parser, ident: []const u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekTypeKeyword(ident)) {
            _ = self.token_list.consume();
            return true;
        }
        return false;
    }

    pub fn consumeIfKeyword(self: *Parser, ident: []const u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekKeyword(ident)) {
            _ = self.token_list.consume();
            return true;
        }
        return false;
    }

    pub fn consumeIfIdent(self: *Parser, ident: []const u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekIdent(ident)) {
            _ = self.token_list.consume();
            return true;
        }
        return false;
    }

    pub fn consumeIfChar(self: *Parser, char: u8) bool {
        if (!self.ok()) return false;
        if (self.token_list.isPeekChar(char)) {
            _ = self.token_list.consume();
            return true;
        }
        return false;
    }
};

pub fn parse(allocator: std.mem.Allocator, token_list: *TokenList) !*Node {
    var parser = Parser.init(allocator, token_list);
    const program_node: *Node = try Node.create_program_node(allocator);

    while (token_list.current_token != null) {
        if (parse_function_def(&parser)) |statement| {
            try program_node.program.append(allocator, statement);
        } else if (!parser.ok()) {
            parser.fail("Expected function definition", .{});
            break;
        }
    }

    if (!parser.ok()) {
        program_node.destroy(allocator);
        return error.SyntaxError;
    }

    return program_node;
}

fn parse_function_def(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("fn")) return null;

    const ident = parser.expectAnyIdent() orelse return null;

    if (!parser.expectChar('(')) return null;

    const parameters = parse_function_parameters(parser);
    const param = parameters orelse return null;

    if (!parser.expectChar(')')) return null;

    if (!parser.expectChar(':')) return null;

    const return_type = parser.expectAnyTypeKeyword() orelse return null;

    if (!parser.expectChar('{')) return null;

    const function_def_node = Node.create_function_def_node(
        parser.allocator,
        ident,
        param,
        return_type,
    ) catch {
        parser.fail("Failed to create function def node", .{});
        return null;
    };

    while (!parser.token_list.isPeekChar('}') and parser.ok()) {
        const statement = parse_statement(parser);
        if (statement) |stmt| {
            function_def_node.function_def.statement_list.append(
                parser.allocator,
                stmt,
            ) catch {
                parser.fail("Failed to add statement to function definition", .{});
                break;
            };
        } else if (parser.ok()) {
            parser.fail("Expected statement or }}", .{});
            break;
        }
    }
    if (!parser.expectChar('}')) {}
    return function_def_node;
}

fn parse_function_parameters(parser: *Parser) ?std.ArrayList(*Node) {
    var parameters: std.ArrayList(*Node) = std.ArrayList(*Node).initCapacity(parser.allocator, 10) catch {
        parser.fail("Failed to initalize parameter arraylist", .{});
        return null;
    };
    var first = true;
    while (!parser.token_list.isPeekChar(')')) {
        if (!first and !parser.consumeIfChar(',')) {
            parser.fail("Expected ',' between parameters", .{});
            return null;
        }
        const parameter_ident = parser.expectAnyIdent() orelse {
            parser.fail("Expected parameter name or ')'", .{});
            return null;
        };

        if (!parser.consumeIfChar(':')) {
            parser.fail("Expected ':' between parameter and type", .{});
            return null;
        }
        const parameter_type = parser.expectAnyTypeKeyword() orelse {
            parser.fail("Expected type after '{s}:'", .{parameter_ident});
            return null;
        };

        const parameter = Node.create_function_parameter(parser.allocator, parameter_ident, parameter_type) catch {
            parser.fail("Out of memory", .{});
            return null;
        };
        parameters.append(parser.allocator, parameter) catch {
            parser.fail("Failed to add parameter", .{});
            return null;
        };
        first = false;
    }
    return parameters;
}
fn parse_statement(parser: *Parser) ?*Node {
    if (parse_decleration(parser)) |decleration| {
        return decleration;
    }
    if (parse_print_statement(parser)) |print_statement| {
        return print_statement;
    }
    if (parse_byte_out_statement(parser)) |byte_out_statement| {
        return byte_out_statement;
    }
    if (parse_return_statement(parser)) |return_statement| {
        return return_statement;
    }
    if (parse_if_statement(parser)) |if_statement| {
        return if_statement;
    }
    if (parse_while_statement(parser)) |while_statement| {
        return while_statement;
    }
    if (parse_function_call_statement(parser)) |function_call_statement| {
        return function_call_statement;
    }
    if (parse_assignment(parser)) |assignment| {
        return assignment;
    }
    return null;
}

fn parse_decleration(parser: *Parser) ?*Node {
    if (!parser.token_list.isPeekAnyTypeKeyword()) return null;
    const _type = parser.expectAnyTypeKeyword() orelse return null;
    const ident = parser.expectAnyIdent() orelse return null;
    var expr: ?*Node = null;
    if (parser.consumeIfChar('=')) {
        expr = parse_expression(parser) orelse {
            parser.fail("Expected expression after '='", .{});
            return null;
        };
    }
    if (!parser.expectChar(';')) return null;
    return Node.create_decl_node(parser.allocator, ident, expr, _type) catch {
        parser.fail("Out of memory", .{});
        expr.?.destroy(parser.allocator);
        return null;
    };
}
fn parse_print_statement(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("print")) return null;
    if (!parser.expectChar('(')) return null;
    const expr = parse_expression(parser) orelse {
        parser.fail("Expected expression after 'print('", .{});
        return null;
    };
    if (!parser.expectChar(')')) return null;
    if (!parser.expectChar(';')) return null;
    return Node.create_print_node(parser.allocator, expr) catch {
        parser.fail("Out of memory", .{});
        expr.destroy(parser.allocator);
        return null;
    };
}
fn parse_byte_out_statement(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("byte_out")) return null;
    if (!parser.expectChar('(')) return null;
    const expr = parse_expression(parser) orelse {
        parser.fail("Expected expression after 'byte_out('", .{});
        return null;
    };
    if (!parser.expectChar(')')) return null;
    if (!parser.expectChar(';')) return null;
    return Node.create_byte_out_node(parser.allocator, expr) catch {
        parser.fail("Out of memory", .{});
        expr.destroy(parser.allocator);
        return null;
    };
}
fn parse_byte_in_statement(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("byte_in")) return null;
    if (!parser.expectChar('(')) return null;
    if (!parser.expectChar(')')) return null;
    return Node.create_byte_in_node(parser.allocator) catch {
        parser.fail("Out of memory", .{});
        return null;
    };
}
fn parse_return_statement(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("return")) return null;
    const expr = parse_expression(parser) orelse {
        parser.fail("Expected expression after 'return'", .{});
        return null;
    };
    if (!parser.expectChar(';')) return null;
    return Node.create_return_node(parser.allocator, expr) catch {
        parser.fail("Out of memory", .{});
        expr.destroy(parser.allocator);
        return null;
    };
}
fn parse_if_statement(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("if")) return null;
    if (!parser.expectChar('(')) return null;
    const expr = parse_expression(parser) orelse {
        parser.fail("Expected expression after 'if ('", .{});
        return null;
    };
    if (!parser.expectChar(')')) return null;
    if (!parser.expectChar('{')) return null;

    const if_statement_node = Node.create_if_node(parser.allocator, expr) catch {
        parser.fail("Out of memory", .{});
        return null;
    };
    while (!parser.token_list.isPeekChar('}') and parser.ok()) {
        const statement = parse_statement(parser);
        if (statement) |stmt| {
            if_statement_node.if_statement.statement_list.append(
                parser.allocator,
                stmt,
            ) catch {
                parser.fail("Failed to add statement to if statement", .{});
                break;
            };
        } else if (parser.ok()) {
            parser.fail("Expected statement or }}", .{});
            break;
        }
    }
    if (!parser.expectChar('}')) {
        if_statement_node.destroy(parser.allocator);
        return null;
    }
    if (!parser.consumeIfKeyword("else")) return if_statement_node;

    const else_if_statement = parse_if_statement(parser);
    if (else_if_statement) |else_if| {
        if_statement_node.if_statement.else_statement = else_if;
        return if_statement_node;
    } else {
        if (!parser.expectChar('{')) {
            if_statement_node.destroy(parser.allocator);
            return null;
        }
        const else_statement = Node.create_else_node(parser.allocator) catch {
            parser.fail("Out of memory", .{});
            if_statement_node.destroy(parser.allocator);
            return null;
        };

        while (!parser.token_list.isPeekChar('}') and parser.ok()) {
            const statement = parse_statement(parser);
            if (statement) |stmt| {
                else_statement.else_statement.append(
                    parser.allocator,
                    stmt,
                ) catch {
                    parser.fail("Failed to add statement to else statement", .{});
                    break;
                };
            } else if (parser.ok()) {
                parser.fail("Expected statement or }}", .{});
                break;
            }
        }

        if (!parser.expectChar('}')) {
            if_statement_node.destroy(parser.allocator);
            else_statement.destroy(parser.allocator);
            return null;
        }
        if_statement_node.if_statement.else_statement = else_statement;
        return if_statement_node;
    }
}
fn parse_while_statement(parser: *Parser) ?*Node {
    if (!parser.consumeIfKeyword("while")) return null;
    if (!parser.expectChar('(')) return null;
    const expr = parse_expression(parser) orelse {
        parser.fail("Expected expression after 'while('", .{});
        return null;
    };
    if (!parser.expectChar(')')) return null;
    if (!parser.expectChar('{')) return null;

    const while_statement_node = Node.create_while_node(parser.allocator, expr) catch {
        parser.fail("Out of memory", .{});
        return null;
    };

    while (!parser.token_list.isPeekChar('}') and parser.ok()) {
        const statement = parse_statement(parser);
        if (statement) |stmt| {
            while_statement_node.while_statement.statement_list.append(
                parser.allocator,
                stmt,
            ) catch {
                parser.fail("Failed to add statement to while statement", .{});
                break;
            };
        } else if (parser.ok()) {
            parser.fail("Expected statement or }}", .{});
            break;
        }
    }

    if (!parser.expectChar('}')) {
        while_statement_node.destroy(parser.allocator);
        return null;
    }
    return while_statement_node;
}
fn parse_assignment(parser: *Parser) ?*Node {
    const ident = parser.token_list.peek() orelse return null;
    if (ident.data != .identifier) return null;
    const lookahead = ident.next;
    if (lookahead != null and lookahead.?.data == .character and lookahead.?.data.character == '(') return null;
    if (lookahead != null and lookahead.?.data == .character and lookahead.?.data.character == '[') {
        _ = parser.token_list.consume();
        _ = parser.token_list.consume();
        const index_expr = parse_expression(parser) orelse {
            parser.fail("Expected expression after '['", .{});
            return null;
        };
        if (!parser.expectChar(']')) return null;
        if (!parser.expectChar('=')) return null;
        const expr = parse_expression(parser) orelse return null;
        if (!parser.expectChar(';')) return null;
        const identifier = Node.create_array_index_node(parser.allocator, ident.data.identifier, index_expr) catch {
            parser.fail("Out of memory", .{});
            expr.destroy(parser.allocator);
            return null;
        };
        return Node.create_assign_node(
            parser.allocator,
            identifier,
            expr,
        ) catch {
            parser.fail("Out of memory", .{});
            expr.destroy(parser.allocator);
            return null;
        };
    }
    if (lookahead != null and lookahead.?.data == .character and lookahead.?.data.character == '=') {
        _ = parser.token_list.consume();
        _ = parser.token_list.consume();
        const expr = parse_expression(parser) orelse {
            parser.fail("Expected expression after '='", .{});
            return null;
        };
        if (!parser.expectChar(';')) return null;
        const identifier = Node.create_identifier(parser.allocator, ident.data.identifier) catch {
            parser.fail("Out of memory", .{});
            return null;
        };
        return Node.create_assign_node(
            parser.allocator,
            identifier,
            expr,
        ) catch {
            parser.fail("Out of memory", .{});
            expr.destroy(parser.allocator);
            return null;
        };
    }
    return null;
}

fn parse_function_call(parser: *Parser) ?*Node {
    const start_token = parser.token_list.current_token;
    const ident = parser.token_list.peek() orelse return null;
    if (ident.data != .identifier) return null;

    const lookahead = ident.next orelse {
        parser.token_list.current_token = start_token;
        return null;
    };

    if (lookahead.data != .character or lookahead.data.character != '(') {
        parser.token_list.current_token = start_token;
        return null;
    }
    _ = parser.token_list.consume();
    _ = parser.token_list.consume();

    var function_call = Node.create_function_call_node(parser.allocator, ident.data.identifier) catch {
        parser.fail("Out of memory", .{});
        parser.token_list.current_token = start_token;
        return null;
    };

    if (!parser.token_list.isPeekChar(')')) {
        while (true) {
            const arg = parse_expression(parser) orelse {
                parser.fail("Expected argument expression", .{});
                function_call.destroy(parser.allocator);
                parser.token_list.current_token = start_token;
                return null;
            };

            function_call.function_call.parameter_expressions.append(parser.allocator, arg) catch {
                parser.fail("Failed to add argument", .{});
                function_call.destroy(parser.allocator);
                parser.token_list.current_token = start_token;
                return null;
            };

            if (parser.consumeIfChar(',')) {
                continue;
            } else if (parser.token_list.isPeekChar(')')) {
                break;
            } else {
                parser.fail("Expected ',' or ')'", .{});
                function_call.destroy(parser.allocator);
                parser.token_list.current_token = start_token;
                return null;
            }
        }
    }

    if (!parser.expectChar(')')) {
        function_call.destroy(parser.allocator);
        parser.token_list.current_token = start_token;
        return null;
    }

    return function_call;
}

fn parse_function_call_statement(parser: *Parser) ?*Node {
    const start_token = parser.token_list.current_token;

    const call = parse_function_call(parser);
    if (call == null) {
        parser.token_list.current_token = start_token;
        return null;
    }

    if (!parser.expectChar(';')) {
        call.?.destroy(parser.allocator);
        parser.token_list.current_token = start_token;
        return null;
    }

    return call;
}

fn parse_expression(parser: *Parser) ?*Node {
    const current_token = parser.token_list.current_token;
    if (parse_term(parser)) |term| {
        if (parser.token_list.isPeekChar('+')) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Add, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekChar('-')) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Sub, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekChar('%')) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Mod, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekChar('<')) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Lt, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekChar('>')) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Gt, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekDouble("==")) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Eql, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekDouble("!=")) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Neq, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekDouble("<=")) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Leq, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekDouble(">=")) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Geq, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekDouble("&&")) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .And, term, expression) catch null;
            }
        }
        if (parser.token_list.isPeekDouble("||")) {
            _ = parser.token_list.consume();
            if (parse_expression(parser)) |expression| {
                return Node.create_binary_op_node(parser.allocator, .Or, term, expression) catch null;
            }
        } else {
            return term;
        }
    }
    parser.token_list.current_token = current_token;
    return null;
}
fn parse_term(parser: *Parser) ?*Node {
    var current_token = parser.token_list.current_token;
    if (parse_factor(parser)) |factor| {
        if (parser.token_list.isPeekChar('*')) {
            current_token = parser.token_list.consume();
            if (parse_term(parser)) |term| {
                return Node.create_binary_op_node(parser.allocator, .Mult, factor, term) catch null;
            }
        }

        if (parser.token_list.isPeekChar('/')) {
            current_token = parser.token_list.consume();
            if (parse_term(parser)) |term| {
                return Node.create_binary_op_node(parser.allocator, .Div, factor, term) catch null;
            }
        } else {
            return factor;
        }
    }
    parser.token_list.current_token = current_token;
    return null;
}

fn parse_factor(parser: *Parser) ?*Node {
    const current_token = parser.token_list.current_token;
    if (parser.token_list.peek().?.data == .integer_literal) {
        return Node.create_integer_literal(parser.allocator, parser.token_list.consume().?.data.integer_literal) catch null;
    }

    if (parser.token_list.peek().?.data == .character_literal) {
        return Node.create_character_literal(parser.allocator, parser.token_list.consume().?.data.character_literal) catch null;
    }
    if (parser.token_list.peek().?.data == .string_literal) {
        return Node.create_string_literal(parser.allocator, parser.token_list.consume().?.data.string_literal) catch null;
    }

    if (parser.token_list.peek().?.data == .identifier) {
        if (parse_function_call(parser)) |function_call| {
            return function_call;
        }
        const identifier = parser.token_list.consume().?.data.identifier;

        if (parser.token_list.isPeekChar('[')) {
            _ = parser.token_list.consume(); // consume '['
            const index_expr = parse_expression(parser) orelse {
                parser.fail("Expected index expression", .{});
                return null;
            };
            if (!parser.expectChar(']')) {
                index_expr.destroy(parser.allocator);
                return null;
            }
            return Node.create_array_index_node(parser.allocator, identifier, index_expr) catch null;
        }
        return Node.create_identifier(parser.allocator, identifier) catch null;
    }

    if (parser.token_list.isPeekKeyword("byte_in")) {
        if (parse_byte_in_statement(parser)) |byte_in| {
            return byte_in;
        }
    }
    if (parser.token_list.isPeekChar('(')) {
        _ = parser.token_list.consume();
        if (parse_expression(parser)) |expression| {
            if (parser.token_list.isPeekChar(')')) {
                _ = parser.token_list.consume();
                return expression;
            }
        }
    }
    parser.token_list.current_token = current_token;
    return null;
}
