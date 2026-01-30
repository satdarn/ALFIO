const std = @import("std");
const Types = @import("lexer.zig").Types;
pub const Node = union(enum) {
    program: std.ArrayList(*Node),
    decleration: struct {
        type: Types,
        identifier: []const u8,
        expression: ?*Node,
        line: u32,
    },
    const_decleration: struct {
        type: Types,
        identifier: []const u8,
        expression: *Node,
        line: u32,
    },
    assignment: struct {
        identifier: *Node,
        expression: *Node,
        line: u32,
    },
    deref_assignment: struct {
        identifier: *Node,
        expression: *Node,
        line: u32,
    },
    function_def: struct {
        name: []const u8,
        parameters: std.ArrayList(*Node),
        return_type: Types,
        statement_list: std.ArrayList(*Node),
        line: u32,
    },
    function_parameter: struct {
        name: []const u8,
        type: Types,
        line: u32,
    },
    function_call: struct {
        name: []const u8,
        parameter_expressions: std.ArrayList(*Node),
        line: u32,
    },
    syscall: struct {
        parameter_expressions: std.ArrayList(*Node),
        line: u32,
    },
    return_statement: struct {
        expression: ?*Node,
        line: u32,
    },
    break_statement: struct {
        line: u32,
    },
    if_statement: struct {
        expression: *Node,
        statement_list: std.ArrayList(*Node),
        else_statement: ?*Node,
        line: u32,
    },
    else_statement: struct {
        statement_list: std.ArrayList(*Node),
        line: u32,
    },
    while_statement: struct {
        expression: *Node,
        statement_list: std.ArrayList(*Node),
        line: u32,
    },
    for_statement: struct {
        decleration: *Node,
        condition: *Node,
        statement: *Node,
        statement_list: std.ArrayList(*Node),
        line: u32,
    },
    binary_op: struct {
        op: BinaryOpEnum,
        left: *Node,
        right: *Node,
        line: u32,
    },
    unary_op: struct {
        op: UnaryOpEnum,
        expression: *Node,
        line: u32,
    },
    cast: struct {
        target: Types,
        expression: *Node,
        line: u32,
    },
    array_index: struct {
        identifier: []const u8,
        expression: *Node,
        line: u32,
    },
    integer_literal: struct {
        value: u64,
        line: u32,
    },
    character_literal: struct {
        value: u8,
        line: u32,
    },
    string_literal: struct {
        value: []const u8,
        line: u32,
    },
    identifier: struct {
        name: []u8,
        line: u32,
    },

    pub const BinaryOpEnum = enum { Add, Sub, Mult, Div, Mod, Eql, Neq, Leq, Geq, Lt, Gt, And, Or, bAnd, bOr };
    pub const UnaryOpEnum = enum { AddrOf, Dref, Not, Neg, bNeg };
    pub fn create_program_node(allocator: std.mem.Allocator) !*Node {
        const new_program_node = try allocator.create(Node);
        const program_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_program_node.* = .{
            .program = program_list,
        };
        return new_program_node;
    }
    pub fn create_assign_node(allocator: std.mem.Allocator, name: *Node, expression: *Node, line: u32) !*Node {
        const new_assign_node = try allocator.create(Node);
        new_assign_node.* = .{
            .assignment = .{
                .identifier = name,
                .expression = expression,
                .line = line,
            },
        };
        return new_assign_node;
    }
    pub fn create_deref_assignment_node(allocator: std.mem.Allocator, name: *Node, expression: *Node, line: u32) !*Node {
        const new_deref_assign_node = try allocator.create(Node);
        new_deref_assign_node.* = .{
            .deref_assignment = .{
                .identifier = name,
                .expression = expression,
                .line = line,
            },
        };
        return new_deref_assign_node;
    }
    pub fn create_function_def_node(allocator: std.mem.Allocator, name: []const u8, parameters: std.ArrayList(*Node), return_type: Types, line: u32) !*Node {
        const new_function_def_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_function_def_node.* = .{
            .function_def = .{
                .name = name_copy,
                .statement_list = statement_list,
                .parameters = parameters,
                .return_type = return_type,
                .line = line,
            },
        };
        return new_function_def_node;
    }

    pub fn create_function_parameter(allocator: std.mem.Allocator, name: []const u8, _type: Types, line: u32) !*Node {
        const new_function_parameter = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        new_function_parameter.* = .{
            .function_parameter = .{
                .name = name_copy,
                .type = _type,
                .line = line,
            },
        };
        return new_function_parameter;
    }

    pub fn create_function_call_node(allocator: std.mem.Allocator, name: []const u8, line: u32) !*Node {
        const new_function_call_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        const parameter_expressions = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_function_call_node.* = .{
            .function_call = .{
                .name = name_copy,
                .parameter_expressions = parameter_expressions,
                .line = line,
            },
        };
        return new_function_call_node;
    }
    pub fn create_syscall_node(allocator: std.mem.Allocator, line: u32) !*Node {
        const new_syscall_node = try allocator.create(Node);
        const parameter_expressions = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_syscall_node.* = .{
            .syscall = .{
                .parameter_expressions = parameter_expressions,
                .line = line,
            },
        };
        return new_syscall_node;
    }
    pub fn create_decl_node(allocator: std.mem.Allocator, name: []const u8, expression: ?*Node, _type: Types, line: u32) !*Node {
        const new_decl_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        new_decl_node.* = .{
            .decleration = .{
                .identifier = name_copy,
                .expression = expression,
                .type = _type,
                .line = line,
            },
        };
        return new_decl_node;
    }
    pub fn create_const_decl_node(allocator: std.mem.Allocator, name: []const u8, expression: *Node, _type: Types, line: u32) !*Node {
        const new_decl_node = try allocator.create(Node);
        const name_copy = try allocator.dupe(u8, name);
        new_decl_node.* = .{
            .const_decleration = .{
                .identifier = name_copy,
                .expression = expression,
                .type = _type,
                .line = line,
            },
        };
        return new_decl_node;
    }
    pub fn create_return_node(allocator: std.mem.Allocator, expression: ?*Node, line: u32) !*Node {
        const new_return_node = try allocator.create(Node);
        new_return_node.* = .{
            .return_statement = .{
                .expression = expression,
                .line = line,
            },
        };
        return new_return_node;
    }
    pub fn create_break_node(allocator: std.mem.Allocator, line: u32) !*Node {
        const new_break_node = try allocator.create(Node);
        new_break_node.* = .{
            .break_statement = .{
                .line = line,
            },
        };
        return new_break_node;
    }
    pub fn create_if_node(allocator: std.mem.Allocator, expression: *Node, line: u32) !*Node {
        const new_if_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_if_node.* = .{
            .if_statement = .{
                .expression = expression,
                .statement_list = statement_list,
                .line = line,
                .else_statement = null,
            },
        };
        return new_if_node;
    }
    pub fn create_else_node(allocator: std.mem.Allocator, line: u32) !*Node {
        const new_else_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_else_node.* = .{
            .else_statement = .{
                .statement_list = statement_list,
                .line = line,
            },
        };
        return new_else_node;
    }
    pub fn create_while_node(allocator: std.mem.Allocator, expression: *Node, line: u32) !*Node {
        const new_while_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_while_node.* = .{
            .while_statement = .{
                .expression = expression,
                .statement_list = statement_list,
                .line = line,
            },
        };
        return new_while_node;
    }
    pub fn create_for_node(allocator: std.mem.Allocator, decleration: *Node, condition: *Node, statement: *Node, line: u32) !*Node {
        const new_for_node = try allocator.create(Node);
        const statement_list = try std.ArrayList(*Node).initCapacity(allocator, 10);
        new_for_node.* = .{
            .for_statement = .{
                .decleration = decleration,
                .condition = condition,
                .statement = statement,
                .statement_list = statement_list,
                .line = line,
            },
        };
        return new_for_node;
    }
    pub fn create_binary_op_node(allocator: std.mem.Allocator, operator: Node.BinaryOpEnum, left: *Node, right: *Node, line: u32) !*Node {
        const new_binary_op_node = try allocator.create(Node);
        new_binary_op_node.* = .{
            .binary_op = .{
                .op = operator,
                .left = left,
                .right = right,
                .line = line,
            },
        };
        return new_binary_op_node;
    }
    pub fn create_unary_op_node(allocator: std.mem.Allocator, operator: Node.UnaryOpEnum, expression: *Node, line: u32) !*Node {
        const new_unary_op_node = try allocator.create(Node);
        new_unary_op_node.* = .{
            .unary_op = .{
                .op = operator,
                .expression = expression,
                .line = line,
            },
        };
        return new_unary_op_node;
    }
    pub fn create_cast_node(allocator: std.mem.Allocator, target: Types, expression: *Node, line: u32) !*Node {
        const new_cast_node = try allocator.create(Node);
        new_cast_node.* = .{
            .cast = .{
                .target = target,
                .expression = expression,
                .line = line,
            },
        };
        return new_cast_node;
    }

    pub fn create_array_index_node(allocator: std.mem.Allocator, identifer: []const u8, expression: *Node, line: u32) !*Node {
        const new_array_index_node = try allocator.create(Node);
        const ident_copy = try allocator.dupe(u8, identifer);
        new_array_index_node.* = .{
            .array_index = .{
                .identifier = ident_copy,
                .expression = expression,
                .line = line,
            },
        };
        return new_array_index_node;
    }

    pub fn create_integer_literal(allocator: std.mem.Allocator, integer_literal: u64, line: u32) !*Node {
        const new_integer_literal_node = try allocator.create(Node);
        new_integer_literal_node.* = .{
            .integer_literal = .{
                .value = integer_literal,
                .line = line,
            },
        };
        return new_integer_literal_node;
    }
    pub fn create_character_literal(allocator: std.mem.Allocator, character_literal: u8, line: u32) !*Node {
        const new_character_literal_node = try allocator.create(Node);
        new_character_literal_node.* = .{
            .character_literal = .{
                .value = character_literal,
                .line = line,
            },
        };
        return new_character_literal_node;
    }
    pub fn create_string_literal(allocator: std.mem.Allocator, string_literal: []const u8, line: u32) !*Node {
        const new_string_literal_node = try allocator.create(Node);
        const ident_copy = try allocator.dupe(u8, string_literal);
        new_string_literal_node.* = .{
            .string_literal = .{
                .value = ident_copy,
                .line = line,
            },
        };
        return new_string_literal_node;
    }
    pub fn create_identifier(allocator: std.mem.Allocator, identifier: []const u8, line: u32) !*Node {
        const new_identifier_node = try allocator.create(Node);
        const ident_copy = try allocator.dupe(u8, identifier);
        new_identifier_node.* = .{
            .identifier = .{
                .name = ident_copy,
                .line = line,
            },
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
            .const_decleration => {
                allocator.free(self.const_decleration.identifier);
                self.const_decleration.expression.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .assignment => |ass| {
                ass.identifier.destroy(allocator);
                ass.expression.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .deref_assignment => |ass| {
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
            .syscall => {
                for (self.syscall.parameter_expressions.items) |parameter_expression| {
                    parameter_expression.destroy(allocator);
                }
                self.syscall.parameter_expressions.deinit(allocator);
                allocator.destroy(self);
                return;
            },
            .return_statement => {
                if (self.return_statement.expression) |expr| expr.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .break_statement => {
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
                for (self.else_statement.statement_list.items) |statement| {
                    statement.destroy(allocator);
                }
                self.else_statement.statement_list.deinit(allocator);
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
            .for_statement => {
                self.for_statement.decleration.destroy(allocator);
                self.for_statement.condition.destroy(allocator);
                self.for_statement.statement.destroy(allocator);
                for (self.for_statement.statement_list.items) |statement| {
                    statement.destroy(allocator);
                }
                self.for_statement.statement_list.deinit(allocator);
                allocator.destroy(self);
                return;
            },
            .binary_op => {
                self.binary_op.left.destroy(allocator);
                self.binary_op.right.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .unary_op => {
                self.unary_op.expression.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .cast => {
                self.cast.expression.destroy(allocator);
                allocator.destroy(self);
            },
            .array_index => {
                allocator.free(self.array_index.identifier);
                self.array_index.expression.destroy(allocator);
                allocator.destroy(self);
                return;
            },
            .integer_literal => {
                allocator.destroy(self);
                return;
            },
            .character_literal => {
                allocator.destroy(self);
                return;
            },
            .string_literal => {
                allocator.free(self.string_literal.value);
                allocator.destroy(self);
                return;
            },
            .identifier => {
                allocator.free(self.identifier.name);
                allocator.destroy(self);
                return;
            },
        }
    }
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
            .const_decleration => {
                std.debug.print("Const Declaration: {s} : {s}\n", .{
                    self.const_decleration.identifier,
                    typeToString(self.const_decleration.type, &buffer),
                });
                printBranch(indent, true);
                std.debug.print("value:\n", .{});
                printBranch(indent + 1, true);
                self.const_decleration.printWithContext(indent + 2, true);
            },

            .assignment => {
                std.debug.print("Assignment: \n", .{});
                printBranch(indent + 1, true);
                std.debug.print("variable:\n", .{});
                printBranch(indent + 2, true);
                self.assignment.identifier.printWithContext(indent + 2, true);
                printBranch(indent, true);
                std.debug.print("value:\n", .{});
                printBranch(indent + 2, true);
                self.assignment.expression.printWithContext(indent + 2, true);
            },
            .deref_assignment => {
                std.debug.print("Dereference Assignment: \n", .{});
                printBranch(indent + 1, true);
                std.debug.print("Pointer:\n", .{});
                printBranch(indent + 2, true);
                self.deref_assignment.identifier.printWithContext(indent + 2, true);
                printBranch(indent, true);
                std.debug.print("value:\n", .{});
                printBranch(indent + 2, true);
                self.deref_assignment.expression.printWithContext(indent + 2, true);
            },

            .function_parameter => {
                std.debug.print("Param: {s} : {s}", .{
                    self.function_parameter.name,
                    typeToString(self.function_parameter.type, &buffer),
                });
            },

            .function_def => {
                std.debug.print("Function: {s}(", .{self.function_def.name});
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
            .syscall => {
                std.debug.print("Syscall: \n", .{});
                if (self.syscall.parameter_expressions.items.len > 0) {
                    for (self.syscall.parameter_expressions.items, 0..) |param_expr, i| {
                        const is_last = (i == self.syscall.parameter_expressions.items.len - 1);
                        printBranch(indent, is_last);
                        param_expr.printWithContext(indent + 1, is_last);
                    }
                }
            },
            .return_statement => {
                std.debug.print("Return\n", .{});
                printBranch(indent, true);
                self.return_statement.expression.printWithContext(indent + 1, true);
            },

            .break_statement => {
                std.debug.print("Break\n", .{});
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
                for (self.else_statement.statement_list.items, 0..) |stmt, i| {
                    const is_last = (i == self.else_statement.statement_list.items.len - 1);
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
            .for_statement => {
                std.debug.print("While\n", .{});

                // Declaration
                printBranch(indent, false);
                std.debug.print("declaration:\n", .{});
                printBranch(indent + 1, true);
                self.for_statement.declaration.printWithContext(indent + 2, true);

                // Condition
                printBranch(indent, false);
                std.debug.print("condition:\n", .{});
                printBranch(indent + 1, true);
                self.for_statement.condition.printWithContext(indent + 2, true);

                // Expression`
                printBranch(indent, false);
                std.debug.print("expression:\n", .{});
                printBranch(indent + 1, true);
                self.for_statement.expression.printWithContext(indent + 2, true);

                // Body
                printBranch(indent, true);
                std.debug.print("body:\n", .{});
                for (self.for_statement.statement_list.items, 0..) |stmt, i| {
                    const is_last = (i == self.for_statement.statement_list.items.len - 1);
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
            .unary_op => {
                std.debug.print("UnaryOp: {s}\n", .{unaryOpToString(self.unary_op.op)});
                printBranch(indent, true);
                std.debug.print("expression:\n", .{});
                printBranch(indent + 1, true);
                self.unary_op.expression.printWithContext(indent + 2, true);
            },
            .cast => {
                std.debug.print("Cast: {s}\n", .{self.cast.target.to_string()});
                printBranch(indent, true);
                std.debug.print("expression:\n", .{});
                printBranch(indent + 1, true);
                self.cast.expression.printWithContext(indent + 2, true);
            },
            .array_index => {
                std.debug.print("ArrayIndex: {s}[]\n", .{self.array_index.identifier});
                printBranch(indent, true);
                std.debug.print("index:\n", .{});
                printBranch(indent + 1, true);
                self.array_index.expression.printWithContext(indent + 2, true);
            },
            .integer_literal => {
                std.debug.print("IntLit: {d}\n", .{self.integer_literal.value});
            },

            .character_literal => {
                std.debug.print("CharLit: '{c}' ({})\n", .{ self.character_literal.value, self.character_literal.value });
            },

            .string_literal => {
                std.debug.print("StrLit: {s}\n", .{self.string_literal.value});
            },

            .identifier => {
                std.debug.print("Ident: {s}\n", .{self.identifier.name});
            },
        }
    }

    fn typeToString(_type: Types, buffer: []u8) []const u8 {
        return switch (_type) {
            .void => "void",
            .bool => "bool",
            .char => "char",
            .int => "int",
            .word => "word",
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
            .bAnd => "&",
            .bOr => "|",
        };
    }
    fn unaryOpToString(op: UnaryOpEnum) []const u8 {
        return switch (op) {
            .AddrOf => "&",
            .Dref => "*",
            .Not => "!",
            .Neg => "-",
            .bNeg => "~",
        };
    }
};
