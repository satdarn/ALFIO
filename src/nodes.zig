const std = @import("std");
const Types = @import("lexer.zig").Types;
pub const Node = union(enum) {
    program: std.ArrayList(*Node),
    decleration: struct {
        type: Types,
        identifier: []const u8,
        expression: ?*Node,
    },
    assignment: struct {
        identifier: *Node,
        expression: *Node,
    },
    function_def: struct {
        name: []const u8,
        parameters: std.ArrayList(*Node),
        return_type: Types,
        statement_list: std.ArrayList(*Node),
    },
    function_parameter: struct {
        name: []const u8,
        type: Types,
    },
    function_call: struct {
        name: []const u8,
        parameter_expressions: std.ArrayList(*Node),
    },
    print_statement: *Node,
    byte_in_statement,
    byte_out_statement: *Node,
    return_statement: *Node,
    if_statement: struct {
        expression: *Node,
        statement_list: std.ArrayList(*Node),
        else_statement: ?*Node,
    },
    else_statement: std.ArrayList(*Node),
    while_statement: struct {
        expression: *Node,
        statement_list: std.ArrayList(*Node),
    },
    binary_op: struct {
        op: BinaryOpEnum,
        left: *Node,
        right: *Node,
    },
    array_index: struct {
        identifier: []const u8,
        expression: *Node,
    },
    integer_literal: u64,
    character_literal: u8,
    string_literal: []const u8,
    identifier: []u8,

    pub const BinaryOpEnum = enum { Add, Sub, Mult, Div, Mod, Eql, Neq, Leq, Geq, Lt, Gt, And, Or };
    pub fn create_program_node(allocator: std.mem.Allocator) !*Node {
        const new_program_node = try allocator.create(Node);
        const program_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_program_node.* = .{
            .program = program_list,
        };
        return new_program_node;
    }
    pub fn create_assign_node(allocator: std.mem.Allocator, name: *Node, expression: *Node) !*Node {
        const new_assign_node = try allocator.create(Node);
        new_assign_node.* = .{
            .assignment = .{
                .identifier = name,
                .expression = expression,
            },
        };
        return new_assign_node;
    }
    pub fn create_function_def_node(allocator: std.mem.Allocator, name: []const u8, parameters: std.ArrayList(*Node), return_type: Types) !*Node {
        const new_function_def_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_function_def_node.* = .{
            .function_def = .{
                .name = name_copy,
                .statement_list = statement_list,
                .parameters = parameters,
                .return_type = return_type,
            },
        };
        return new_function_def_node;
    }

    pub fn create_function_parameter(allocator: std.mem.Allocator, name: []const u8, _type: Types) !*Node {
        const new_function_parameter = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        new_function_parameter.* = .{
            .function_parameter = .{
                .name = name_copy,
                .type = _type,
            },
        };
        return new_function_parameter;
    }

    pub fn create_function_call_node(allocator: std.mem.Allocator, name: []const u8) !*Node {
        const new_function_call_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        const parameter_expressions = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_function_call_node.* = .{
            .function_call = .{
                .name = name_copy,
                .parameter_expressions = parameter_expressions,
            },
        };
        return new_function_call_node;
    }
    pub fn create_decl_node(allocator: std.mem.Allocator, name: []const u8, expression: ?*Node, _type: Types) !*Node {
        const new_decl_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        new_decl_node.* = .{
            .decleration = .{
                .identifier = name_copy,
                .expression = expression,
                .type = _type,
            },
        };
        return new_decl_node;
    }
    pub fn create_print_node(allocator: std.mem.Allocator, expression: *Node) !*Node {
        const new_print_node = try allocator.create(Node);
        new_print_node.* = .{
            .print_statement = expression,
        };
        return new_print_node;
    }
    pub fn create_byte_in_node(allocator: std.mem.Allocator) !*Node {
        const new_byte_in_node = try allocator.create(Node);
        new_byte_in_node.* = .{
            .byte_in_statement = {},
        };
        return new_byte_in_node;
    }
    pub fn create_byte_out_node(allocator: std.mem.Allocator, expression: *Node) !*Node {
        const new_byte_out_node = try allocator.create(Node);
        new_byte_out_node.* = .{
            .byte_out_statement = expression,
        };
        return new_byte_out_node;
    }
    pub fn create_return_node(allocator: std.mem.Allocator, expression: *Node) !*Node {
        const new_return_node = try allocator.create(Node);
        new_return_node.* = .{
            .return_statement = expression,
        };
        return new_return_node;
    }
    pub fn create_if_node(allocator: std.mem.Allocator, expression: *Node) !*Node {
        const new_if_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_if_node.* = .{ .if_statement = .{
            .expression = expression,
            .statement_list = statement_list,
            .else_statement = null,
        } };
        return new_if_node;
    }
    pub fn create_else_node(allocator: std.mem.Allocator) !*Node {
        const new_else_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_else_node.* = .{
            .else_statement = statement_list,
        };
        return new_else_node;
    }
    pub fn create_while_node(allocator: std.mem.Allocator, expression: *Node) !*Node {
        const new_while_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_while_node.* = .{
            .while_statement = .{
                .expression = expression,
                .statement_list = statement_list,
            },
        };
        return new_while_node;
    }
    pub fn create_binary_op_node(allocator: std.mem.Allocator, operator: Node.BinaryOpEnum, left: *Node, right: *Node) !*Node {
        const new_binary_op_node = try allocator.create(Node);
        new_binary_op_node.* = .{
            .binary_op = .{
                .op = operator,
                .left = left,
                .right = right,
            },
        };
        return new_binary_op_node;
    }
    pub fn create_array_index_node(allocator: std.mem.Allocator, identifer: []const u8, expression: *Node) !*Node {
        const new_array_index_node = try allocator.create(Node);
        const ident_copy = try allocator.dupe(u8, identifer);
        new_array_index_node.* = .{
            .array_index = .{
                .identifier = ident_copy,
                .expression = expression,
            },
        };
        return new_array_index_node;
    }

    pub fn create_integer_literal(allocator: std.mem.Allocator, integer_literal: u64) !*Node {
        const new_integer_literal_node = try allocator.create(Node);
        new_integer_literal_node.* = .{
            .integer_literal = integer_literal,
        };
        return new_integer_literal_node;
    }
    pub fn create_character_literal(allocator: std.mem.Allocator, character_literal: u8) !*Node {
        const new_character_literal_node = try allocator.create(Node);
        new_character_literal_node.* = .{
            .character_literal = character_literal,
        };
        return new_character_literal_node;
    }
    pub fn create_string_literal(allocator: std.mem.Allocator, string_literal: []const u8) !*Node {
        const new_string_literal_node = try allocator.create(Node);
        const ident_copy = try allocator.dupe(u8, string_literal);
        new_string_literal_node.* = .{
            .string_literal = ident_copy,
        };
        return new_string_literal_node;
    }
    pub fn create_identifier(allocator: std.mem.Allocator, identifier: []const u8) !*Node {
        const new_identifier_node = try allocator.create(Node);
        const ident_copy = try allocator.dupe(u8, identifier);
        new_identifier_node.* = .{
            .identifier = ident_copy,
        };
        return new_identifier_node;
    }
    pub fn destroy(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .program => {
                for (self.program.items) |node| {
                    node.destroy(allocator);
                }
                self.program.deinit(allocator);
                allocator.destroy(self);
                return;
            },
            .decleration => {
                allocator.free(self.decleration.identifier);
                if (self.decleration.expression) |expr| {
                    expr.destroy(allocator);
                }
                allocator.destroy(self);
                return;
            },
            .assignment => |ass| {
                ass.identifier.destroy(allocator);
                ass.expression.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .function_def => {
                allocator.free(self.function_def.name);
                for (self.function_def.statement_list.items) |statement| {
                    statement.destroy(allocator);
                }
                self.function_def.statement_list.deinit(allocator);
                for (self.function_def.parameters.items) |param| {
                    param.destroy(allocator);
                }
                self.function_def.parameters.deinit(allocator);
                allocator.destroy(self);
                return;
            },
            .function_parameter => {
                allocator.free(self.function_parameter.name);
                allocator.destroy(self);
            },
            .function_call => {
                allocator.free(self.function_call.name);
                for (self.function_call.parameter_expressions.items) |parameter_expression| {
                    parameter_expression.destroy(allocator);
                }
                self.function_call.parameter_expressions.deinit(allocator);
                allocator.destroy(self);
                return;
            },
            .print_statement => |expr| {
                expr.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .byte_in_statement => {
                allocator.destroy(self);
                return;
            },
            .byte_out_statement => |expr| {
                expr.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .return_statement => |expr| {
                expr.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .if_statement => {
                self.if_statement.expression.destroy(allocator);
                for (self.if_statement.statement_list.items) |statement| {
                    statement.destroy(allocator);
                }
                self.if_statement.statement_list.deinit(allocator);
                if (self.if_statement.else_statement) |statement| {
                    statement.destroy(allocator);
                }
                allocator.destroy(self);
                return;
            },
            .else_statement => {
                for (self.else_statement.items) |statement| {
                    statement.destroy(allocator);
                }
                self.else_statement.deinit(allocator);
                allocator.destroy(self);
            },
            .while_statement => {
                self.while_statement.expression.destroy(allocator);
                for (self.while_statement.statement_list.items) |statement| {
                    statement.destroy(allocator);
                }
                self.while_statement.statement_list.deinit(allocator);
                allocator.destroy(self);
                return;
            },
            .binary_op => |bin| {
                bin.left.destroy(allocator);
                bin.right.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .array_index => |index| {
                allocator.free(index.identifier);
                index.expression.destroy(allocator);
                allocator.destroy(self);
            },
            .integer_literal => {
                allocator.destroy(self);
                return;
            },
            .character_literal => {
                allocator.destroy(self);
                return;
            },
            .string_literal => |string| {
                allocator.free(string);
                allocator.destroy(self);
                return;
            },
            .identifier => |ident| {
                allocator.free(ident);
                allocator.destroy(self);
                return;
            },
        }
    }
    // Replace the print function in nodes.zig with this improved version

    fn printIndent(indent: usize) void {
        var i: usize = 0;
        while (i < indent) : (i += 1) {
            std.debug.print("│   ", .{});
        }
    }

    fn printBranch(indent: usize, is_last: bool) void {
        var i: usize = 0;
        while (i < indent) : (i += 1) {
            std.debug.print("│   ", .{});
        }
        if (is_last) {
            std.debug.print("└── ", .{});
        } else {
            std.debug.print("├── ", .{});
        }
    }

    pub fn print(self: *const Node, indent: usize) void {
        switch (self.*) {
            .program => {
                std.debug.print("Program\n", .{});
                for (self.program.items, 0..) |child, i| {
                    const is_last = (i == self.program.items.len - 1);
                    printBranch(0, is_last);
                    child.printWithContext(1, is_last);
                }
            },
            else => {
                self.printNode(indent);
            },
        }
    }

    fn printWithContext(self: *const Node, indent: usize, parent_is_last: bool) void {
        _ = parent_is_last;
        self.printNode(indent);
    }

    fn printNode(self: *const Node, indent: usize) void {
        var buffer: [256]u8 = undefined;
        switch (self.*) {
            .program => {
                // Handled in print()
            },

            .decleration => {
                std.debug.print("Declaration: {s} : {s}\n", .{
                    self.decleration.identifier,
                    typeToString(self.decleration.type, &buffer),
                });
                if (self.decleration.expression) |expr| {
                    printBranch(indent, true);
                    std.debug.print("value:\n", .{});
                    printBranch(indent + 1, true);
                    expr.printWithContext(indent + 2, true);
                }
            },

            .assignment => {
                std.debug.print("Assignment: \n", .{});
                printBranch(indent + 1, true);
                std.debug.print("variable:\n", .{});
                printBranch(indent + 1, true);
                self.assignment.identifier.printWithContext(indent + 2, true);
                printBranch(indent, true);
                std.debug.print("value:\n", .{});
                printBranch(indent + 1, true);
                self.assignment.expression.printWithContext(indent + 2, true);
            },

            .function_parameter => {
                std.debug.print("Param: {s} : {s}", .{
                    self.function_parameter.name,
                    typeToString(self.function_parameter.type, &buffer),
                });
            },

            .function_def => {
                std.debug.print("Function: {s}(", .{self.function_def.name});

                // Print parameters inline
                if (self.function_def.parameters.items.len > 0) {
                    for (self.function_def.parameters.items, 0..) |param, i| {
                        if (param.* == .function_parameter) {
                            std.debug.print("{s}: {s}", .{
                                param.function_parameter.name,
                                typeToString(param.function_parameter.type, &buffer),
                            });
                            if (i < self.function_def.parameters.items.len - 1) {
                                std.debug.print(", ", .{});
                            }
                        }
                    }
                }

                std.debug.print(") : {s}\n", .{typeToString(self.function_def.return_type, &buffer)});

                // Print body
                for (self.function_def.statement_list.items, 0..) |statement, i| {
                    const is_last = (i == self.function_def.statement_list.items.len - 1);
                    printBranch(indent, is_last);
                    statement.printWithContext(indent + 1, is_last);
                }
            },

            .function_call => {
                std.debug.print("Call: {s}()\n", .{self.function_call.name});
                if (self.function_call.parameter_expressions.items.len > 0) {
                    for (self.function_call.parameter_expressions.items, 0..) |param_expr, i| {
                        const is_last = (i == self.function_call.parameter_expressions.items.len - 1);
                        printBranch(indent, is_last);
                        param_expr.printWithContext(indent + 1, is_last);
                    }
                }
            },

            .print_statement => {
                std.debug.print("Print\n", .{});
                printBranch(indent, true);
                self.print_statement.printWithContext(indent + 1, true);
            },

            .byte_in_statement => {
                std.debug.print("ByteIn()\n", .{});
            },

            .byte_out_statement => {
                std.debug.print("ByteOut\n", .{});
                printBranch(indent, true);
                self.byte_out_statement.printWithContext(indent + 1, true);
            },

            .return_statement => {
                std.debug.print("Return\n", .{});
                printBranch(indent, true);
                self.return_statement.printWithContext(indent + 1, true);
            },

            .if_statement => {
                std.debug.print("If\n", .{});

                // Condition
                printBranch(indent, false);
                std.debug.print("condition:\n", .{});
                printBranch(indent + 1, true);
                self.if_statement.expression.printWithContext(indent + 2, true);

                // Then body
                const has_else = self.if_statement.else_statement != null;
                printBranch(indent, has_else);
                std.debug.print("then:\n", .{});
                for (self.if_statement.statement_list.items, 0..) |stmt, i| {
                    const is_last = (i == self.if_statement.statement_list.items.len - 1);
                    printBranch(indent + 1, is_last);
                    stmt.printWithContext(indent + 2, is_last);
                }

                // Else body
                if (self.if_statement.else_statement) |else_stmt| {
                    printBranch(indent, true);
                    std.debug.print("else:\n", .{});
                    printBranch(indent + 1, true);
                    else_stmt.printWithContext(indent + 2, true);
                }
            },

            .else_statement => {
                for (self.else_statement.items, 0..) |stmt, i| {
                    const is_last = (i == self.else_statement.items.len - 1);
                    if (i > 0) printBranch(indent - 1, is_last);
                    stmt.printWithContext(indent, is_last);
                }
            },

            .while_statement => {
                std.debug.print("While\n", .{});

                // Condition
                printBranch(indent, false);
                std.debug.print("condition:\n", .{});
                printBranch(indent + 1, true);
                self.while_statement.expression.printWithContext(indent + 2, true);

                // Body
                printBranch(indent, true);
                std.debug.print("body:\n", .{});
                for (self.while_statement.statement_list.items, 0..) |stmt, i| {
                    const is_last = (i == self.while_statement.statement_list.items.len - 1);
                    printBranch(indent + 1, is_last);
                    stmt.printWithContext(indent + 2, is_last);
                }
            },

            .binary_op => {
                std.debug.print("BinaryOp: {s}\n", .{binaryOpToString(self.binary_op.op)});
                printBranch(indent, false);
                std.debug.print("left:\n", .{});
                printBranch(indent + 1, true);
                self.binary_op.left.printWithContext(indent + 2, true);

                printBranch(indent, true);
                std.debug.print("right:\n", .{});
                printBranch(indent + 1, true);
                self.binary_op.right.printWithContext(indent + 2, true);
            },

            .array_index => {
                std.debug.print("ArrayIndex: {s}[]\n", .{self.array_index.identifier});
                printBranch(indent, true);
                std.debug.print("index:\n", .{});
                printBranch(indent + 1, true);
                self.array_index.expression.printWithContext(indent + 2, true);
            },
            .integer_literal => {
                std.debug.print("IntLit: {d}\n", .{self.integer_literal});
            },

            .character_literal => {
                std.debug.print("CharLit: '{c}' ({})\n", .{ self.character_literal, self.character_literal });
            },

            .string_literal => {
                std.debug.print("StrLit: {s}\n", .{self.string_literal});
            },

            .identifier => {
                std.debug.print("Ident: {s}\n", .{self.identifier});
            },
        }
    }

    fn typeToString(_type: Types, buffer: []u8) []const u8 {
        return switch (_type) {
            .int => "int",
            .void => "void",
            .bool => "bool",
            .char => "char",
            .char_array => |size| {
                const result = std.fmt.bufPrint(buffer, "char[{d}]", .{size}) catch "char[?]";
                return result;
            },
        };
    }

    fn binaryOpToString(op: BinaryOpEnum) []const u8 {
        return switch (op) {
            .Add => "+",
            .Sub => "-",
            .Mult => "*",
            .Div => "/",
            .Mod => "%",
            .Eql => "==",
            .Neq => "!=",
            .Leq => "<=",
            .Geq => ">=",
            .Lt => "<",
            .Gt => ">",
            .And => "&&",
            .Or => "||",
        };
    }
};
