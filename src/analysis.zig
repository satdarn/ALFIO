const std = @import("std");
const Parser = @import("parser.zig");
const Node = @import("nodes.zig").Node;
const Types = @import("lexer.zig").Types;
const GlobalTable = @import("tables.zig").GlobalTable;
const FunctionTable = @import("tables.zig").FunctionTable;
const stdlib_functions = @import("stdlib_generated.zig").functions;
pub const TypeError = error{
    TypeMismatch,
    InvalidOperation,
    UndefinedVariable,
    UndefinedFunction,
    InvalidCast,
    CannotDereference,
    OutOfMemory,
};

pub const TypeErrorInfo = struct {
    line: u32,
    message: []const u8,

    pub fn print(self: TypeErrorInfo) void {
        std.debug.print("Type Error on line {}: {s}\n", .{ self.line, self.message });
    }
};

fn type_error(line: u32, comptime fmt: []const u8, args: anytype) TypeError {
    std.debug.print("Type Error on line {}: ", .{line});
    std.debug.print(fmt ++ "\n", args);
    return TypeError.TypeMismatch;
}

pub fn type_checking(symbol_table: *GlobalTable, ast: *Node) !void {
    for (ast.program.items) |function| {
        const function_table = symbol_table.get_function(function.function_def.name) orelse unreachable;
        for (function.function_def.statement_list.items) |statement| {
            try type_checking_pass(symbol_table, function_table, statement);
        }
    }
}

fn type_checking_pass(symbol_table: *GlobalTable, function_table: *FunctionTable, statement: *Node) !void {
    var buff: [64]u8 = undefined;
    switch (statement.*) {
        .decleration => |decl| {
            const lhs = try evaluate_expression_type(symbol_table, function_table, statement);
            if (statement.decleration.expression) |expr| {
                const rhs = try evaluate_expression_type(symbol_table, function_table, expr);
                if (!Types.eql(lhs, rhs)) return type_error(decl.line, "Type mismatch in declaration: expected {s}, got {s}", .{ lhs.to_string(&buff), rhs.to_string(&buff) });
            }
        },
        .assignment => |assign| {
            const lhs = try evaluate_expression_type(symbol_table, function_table, statement.assignment.identifier);
            const rhs = try evaluate_expression_type(symbol_table, function_table, statement.assignment.expression);
            if (!Types.eql(lhs, rhs)) return type_error(assign.line, "Type mismatch assignment expected {s}, got {s}", .{ lhs.to_string(&buff), rhs.to_string(&buff) });
        },
        .if_statement => |if_stmt| {
            const condition = try evaluate_expression_type(symbol_table, function_table, statement.if_statement.expression);
            if (!Types.eql(condition, Types.Bool)) return type_error(if_stmt.line, "If condition must be bool, got {s}", .{condition.to_string(&buff)});

            for (statement.if_statement.statement_list.items) |stmt| try type_checking_pass(symbol_table, function_table, stmt);
            if (statement.if_statement.else_statement) |stmt| try type_checking_pass(symbol_table, function_table, stmt);
        },
        .else_statement => {
            for (statement.else_statement.statement_list.items) |stmt| try type_checking_pass(symbol_table, function_table, stmt);
        },
        .while_statement => |while_stmt| {
            const condition = try evaluate_expression_type(symbol_table, function_table, statement.while_statement.expression);
            if (!Types.eql(condition, Types.Bool)) return type_error(while_stmt.line, "While condition must be bool, got {s}", .{condition.to_string(&buff)});
            for (statement.while_statement.statement_list.items) |stmt| try type_checking_pass(symbol_table, function_table, stmt);
        },
        else => {},
    }
}

pub fn evaluate_expression_type(global_table: *GlobalTable, function_table: *FunctionTable, expression: *Node) !Types {
    var _type = Types.Void;
    var buff: [64]u8 = undefined;
    switch (expression.*) {
        .decleration => {
            _type = if (function_table.get_parameter_or_variable(expression.decleration.identifier)) |variable| variable.type else Types.Void;
        },
        .identifier => {
            if (function_table.get_parameter_or_variable(expression.identifier.name)) |variable| {
                _type = variable.type;
            } else {
                return type_error(expression.identifier.line, "Undefined variable: {s}", .{expression.identifier.name});
            }
        },
        .array_index => {
            _type = Types.Char; //TODO if we implement int or word arrays this needs to be changed
        },
        .integer_literal => {
            _type = Types.Int;
        },
        .character_literal => {
            _type = Types.Char;
        },
        .string_literal => {
            _type = .{ .char_array = expression.string_literal.value.len + 1 };
        },
        .function_call => |call| {
            if (global_table.get_function(call.name)) |function| {
                // Check parameter count
                if (function.parameters.count() != call.parameter_expressions.items.len) {
                    return type_error(call.line, "Function '{s}' expects {d} arguments but got {d}", .{ call.name, function.parameters.count(), call.parameter_expressions.items.len });
                }

                // Check each parameter type
                for (call.parameter_expressions.items, function.parameters.values(), 0..) |call_expr, param, i| {
                    const call_type = try evaluate_expression_type(global_table, function_table, call_expr);
                    if (!can_cast_to(call_type, param.type) and !can_cast_to(param.type, call_type)) {
                        const param_name = function.parameters.keys()[i];
                        return type_error(
                            call.line,
                            "Function '{s}' parameter '{s}' expects type {s} but got {s}",
                            .{ call.name, param_name, param.type.to_string(&buff), call_type.to_string(&buff) },
                        );
                    }
                }

                return function.return_type;
            } else {
                for (stdlib_functions) |func| {
                    if (std.mem.eql(u8, func.name, call.name)) {
                        if (func.parameters.len != call.parameter_expressions.items.len) {
                            return type_error(
                                call.line,
                                "Function '{s}' expects {d} arguments but got {d}",
                                .{ call.name, func.parameters.len, call.parameter_expressions.items.len },
                            );
                        }
                        for (call.parameter_expressions.items, func.parameters) |call_expr, param| {
                            const call_type = try evaluate_expression_type(global_table, function_table, call_expr);
                            if (!can_cast_to(call_type, param.type) and !can_cast_to(param.type, call_type)) {
                                return type_error(
                                    call.line,
                                    "Function '{s}' parameter '{s}' expects type {s} but got {s}",
                                    .{ call.name, param.name, param.type.to_string(&buff), call_type.to_string(&buff) },
                                );
                            }
                        }
                        return func.return_type;
                    }
                }

                return type_error(call.line, "Undefined function: {s}", .{call.name});
            }
        },
        .byte_in_statement => {
            _type = Types.Char;
        },
        .binary_op => |binop| {
            const left_type = try evaluate_expression_type(global_table, function_table, binop.left);
            const right_type = try evaluate_expression_type(global_table, function_table, binop.right);

            switch (binop.op) {
                .Eql, .Neq, .Geq, .Leq, .Lt, .Gt => {
                    if (!can_cast_to(left_type, right_type) and !can_cast_to(right_type, left_type)) {
                        return type_error(binop.line, "Cannot compare {s} with {s}", .{ left_type.to_string(&buff), right_type.to_string(&buff) });
                    }
                    return Types.Bool;
                },
                .And, .Or => {
                    if (!Types.eql(left_type, Types.Bool)) {
                        return type_error(binop.line, "Logical operator requires bool operands, got {s}", .{left_type.to_string(&buff)});
                    }
                    if (!Types.eql(right_type, Types.Bool)) {
                        return type_error(binop.line, "Logical operator requires bool operands, got {s}", .{right_type.to_string(&buff)});
                    }
                    return Types.Bool;
                },
                else => {
                    if (can_cast_to(right_type, left_type)) {
                        return right_type;
                    } else if (can_cast_to(left_type, right_type)) {
                        return left_type;
                    } else {
                        return type_error(binop.line, "Type mismatch in binary operation: {s} and {s}", .{ left_type.to_string(&buff), right_type.to_string(&buff) });
                    }
                },
            }
        },
        .unary_op => |unop| {
            switch (unop.op) {
                .AddrOf => {
                    return Types.Word;
                },
                .Dref => {
                    const operand_type = try evaluate_expression_type(global_table, function_table, unop.expression);

                    if (unop.expression.* == .cast) {
                        return unop.expression.cast.target;
                    } else if (Types.eql(operand_type, Types.Word)) {
                        return type_error(unop.line, "Cannot dereference untyped pointer (word). Use a cast: *(type)ptr", .{});
                    } else {
                        return type_error(unop.line, "Cannot dereference non-pointer type: {s}", .{operand_type.to_string(&buff)});
                    }
                },
                .Neg => {
                    const operand_type = try evaluate_expression_type(global_table, function_table, unop.expression);
                    if (!Types.eql(operand_type, Types.Int) and !Types.eql(operand_type, Types.Char)) {
                        return type_error(unop.line, "Cannot negate non-numeric type: {s}", .{operand_type.to_string(&buff)});
                    }
                    return operand_type;
                },
                .Not => {
                    const operand_type = try evaluate_expression_type(global_table, function_table, unop.expression);
                    if (!Types.eql(operand_type, Types.Bool)) {
                        return type_error(unop.line, "Logical NOT requires bool operand, got {s}", .{operand_type.to_string(&buff)});
                    }
                    return Types.Bool;
                },
            }
        },
        .cast => {
            _type = expression.cast.target;
        },
        else => {
            _type = Types.Void;
        },
    }
    return _type;
}
// fn evaluate_expression_type(global_table =>{},
//     var _type =>{},
//     if (expression.* == .integer_literal) return min_literal_size(expression.integer_literal);
//     if (expression.* == .character_literal) return Types.char;
//     if (expression.* == .string_literal) return .{ .char_array = expression.string_literal.len - 1 };
//     if (expression.* == .byte_in_statement) return Types.char;
//     if (expression.* == .identifier) {
//         return if (function_table.get_parameter_or_variable(expression.identifier)) |variable| variable.type else unreachable;
//     }
//     if (expression.* == .array_index) return Types.char;
//     if (expression.* == .function_call) {
//         const function = global_table.get_function(expression.function_call.name) orelse unreachable;
//         if (function_table.parameters.count() != expression.function_call.parameter_expressions.items.len) unreachable;
//
//         // check that the parameters are of the correct type...
//         for (expression.function_call.parameter_expressions.items, 0..) |expr, i| {
//             const expr_type = evaluate_expression_type(global_table, function_table, expr);
//             const param_type = function.parameters.values()[i].type;
//             if (!std.meta.eql(expr_type, param_type)) unreachable;
//         }
//         // check the return value
//         return function.return_type;
//     }
//     if (expression.* == .binary_op) {
//         const left_type = evaluate_expression_type(global_table, function_table, expression.binary_op.left);
//         const right_type = evaluate_expression_type(global_table, function_table, expression.binary_op.right);
//         // as long as one can be cast to the other,
//         _type = right_type;
//         if (can_cast_to(left_type, right_type)) {
//             _type = left_type;
//         } else if (can_cast_to(right_type, left_type)) {
//             _type = right_type;
//         } else {
//             unreachable;
//         }
//         // and the operator isnt .Eq, .Neq, .Geq, .Leq, .Lt, .Gt, which casts the type to booleans
//         switch (expression.binary_op.op) {
//             .Eql, .Neq, .Geq, .Leq, .Lt, .Gt => {
//                 _type = Types.bool;
//             },
//             // and its an error to do the short circuit ops on non booleans
//             .And, .Or => {
//                 if (_type != .bool) unreachable;
//             },
//             else => {},
//         }
//     }
//     return _type;
// }
fn min_literal_size(literal: u64) Types {
    if (literal <= 255) return .char;
    if (literal <= 65535) {} // two byte integer.. not implemented
    if (literal <= 4294967295) return .int; // four byte integer.. not implemented
    if (literal > 18446744073709551615) unreachable;
    return .int;
}
fn can_cast_to(to: Types, from: Types) bool {
    if (from.size_of() <= to.size_of()) return true;
    return false;
}
