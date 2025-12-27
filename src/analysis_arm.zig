const std = @import("std");
const Parser = @import("parser.zig");
const Node = @import("nodes.zig").Node;
const Types = @import("lexer.zig").Types;

const VariableEntry = struct {
    type: Types,
    offset: i64,
};

pub const FunctionTable = struct {
    return_type: Types,
    parameters: std.StringArrayHashMap(VariableEntry),
    variables: std.StringHashMap(VariableEntry),
    frame_size: i64 = 0,
    pub fn init(allocator: std.mem.Allocator, return_type: Types) FunctionTable {
        return .{
            .parameters = std.StringArrayHashMap(VariableEntry).init(allocator),
            .variables = std.StringHashMap(VariableEntry).init(allocator),
            .return_type = return_type,
        };
    }
    pub fn deinit(self: *FunctionTable, allocator: std.mem.Allocator) void {
        var param_iter = self.parameters.iterator();
        while (param_iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.parameters.deinit();
        var vars_iter = self.variables.iterator();
        while (vars_iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();
    }
    pub fn insert_new_parameter(self: *FunctionTable, allocator: std.mem.Allocator, name: []const u8, _type: Types) !void {
        try self.parameters.put(try allocator.dupe(u8, name), .{ .type = _type, .offset = 0 });
    }
    pub fn insert_new_variable(self: *FunctionTable, allocator: std.mem.Allocator, name: []const u8, _type: Types) !void {
        try self.variables.put(try allocator.dupe(u8, name), .{ .type = _type, .offset = 0 });
    }
    pub fn get_parameter_or_variable(self: *FunctionTable, name: []const u8) ?*VariableEntry {
        if (self.parameters.getPtr(name)) |param| return param;
        if (self.variables.getPtr(name)) |variable| return variable else return null;
    }
    pub fn calculate_offsets(self: *FunctionTable) void {
        var frame_size: i64 = 0;
        var param_index: usize = 0;
        var param_iter = self.parameters.iterator();
        
        // AArch64 calling convention: first 8 parameters in x0-x7
        while (param_iter.next()) |entry| {
            const var_entry = entry.value_ptr;

            // Parameters beyond x0-x7 are on the stack
            if (param_index >= 8) {
                // Stack parameters are at positive offsets from the previous frame pointer
                // AArch64 stack layout: [previous FP][LR][param8][param9]...
                // After the function prolog, we have: sp points to current frame
                // The 9th parameter (index 8) is at [previous_fp + 16]
                var_entry.offset = 16 + @as(i64, @intCast(param_index - 8)) * 8;
            }

            param_index += 1;
        }
        
        // Allocate space for local variables
        var var_iter = self.variables.iterator();
        while (var_iter.next()) |entry| {
            const var_entry = entry.value_ptr;

            const size = size_of_type(var_entry.type);
            const _align = typeAlignment(size);

            frame_size = alignUp(frame_size, _align);
            frame_size += size;
            var_entry.offset = -frame_size;
        }
        
        // Now allocate space for the first 8 parameters on the stack
        // (we need to save them from registers)
        param_index = 0;
        param_iter = self.parameters.iterator();
        while (param_iter.next()) |entry| {
            const var_entry = entry.value_ptr;

            if (param_index < 8) {
                const size = size_of_type(var_entry.type);
                const _align = typeAlignment(size);

                frame_size = alignUp(frame_size, _align);
                frame_size += size;
                var_entry.offset = -frame_size;
            }

            param_index += 1;
        }
        
        // AArch64 requires 16-byte stack alignment
        frame_size = alignUp(frame_size, 16);
        self.frame_size = frame_size;
    }
};

fn alignUp(value: i64, alignment: i64) i64 {
    return (value + alignment - 1) & ~(alignment - 1);
}

fn typeAlignment(size: i64) i64 {
    return if (size >= 8) 8 else size;
}

fn size_of_type(_type: Types) i64 {
    return switch (_type) {
        .int => 8,
        .bool => 1,
        .char => 1,
        .char_array => @intCast(_type.char_array),
        else => unreachable,
    };
}

pub const GlobalTable = struct {
    table: std.StringHashMap(FunctionTable),
    pub fn init(allocator: std.mem.Allocator) !*GlobalTable {
        var global_table = try allocator.create(GlobalTable);
        global_table.table = std.StringHashMap(FunctionTable).init(allocator);
        return global_table;
    }
    pub fn deinit(self: *GlobalTable, allocator: std.mem.Allocator) void {
        var iter = self.table.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(allocator);
        }
        self.table.deinit();
        allocator.destroy(self);
    }
    pub fn insert_new_function(self: *GlobalTable, allocator: std.mem.Allocator, name: []const u8, return_type: Types) !void {
        const new_function = FunctionTable.init(allocator, return_type);
        try self.table.put(try allocator.dupe(u8, name), new_function);
    }
    pub fn get_function(self: *GlobalTable, name: []const u8) ?*FunctionTable {
        return self.table.getPtr(name);
    }
    pub fn calculate_offsets(self: *GlobalTable) void {
        var fn_iter = self.table.keyIterator();
        while (fn_iter.next()) |fn_name| {
            if (self.table.getPtr(fn_name.*)) |fn_tbl| {
                fn_tbl.calculate_offsets();
            }
        }
    }
};

pub fn analyize(allocator: std.mem.Allocator, ast: *Node, type_checking: bool) !*GlobalTable {
    const symbol_table = try GlobalTable.init(allocator);
    for (ast.program.items) |function| {
        try symbol_table.insert_new_function(allocator, function.function_def.name, function.function_def.return_type);
        const function_table = symbol_table.get_function(function.function_def.name) orelse unreachable;

        if (function.function_def.parameters.items.len == 0) {
            continue;
        }
        for (function.function_def.parameters.items) |parameter| {
            const param_type = parameter.function_parameter.type;
            const param_name = parameter.function_parameter.name;
            try function_table.insert_new_parameter(allocator, param_name, param_type);
        }
    }
    for (ast.program.items) |function| {
        const function_table = symbol_table.get_function(function.function_def.name) orelse unreachable;
        // variable declaration pass
        for (function.function_def.statement_list.items) |statement| {
            try variable_decleration_pass(allocator, symbol_table, function_table, statement);
        }
        for (function.function_def.statement_list.items) |statement| {
            if (type_checking) type_checking_pass(symbol_table, function_table, statement);
        }
    }
    return symbol_table;
}

fn variable_decleration_pass(allocator: std.mem.Allocator, symbol_table: *GlobalTable, function_table: *FunctionTable, statement: *Node) !void {
    if (statement.* == .decleration) {
        try function_table.insert_new_variable(allocator, statement.decleration.identifier, statement.decleration.type);
        if (statement.decleration.expression) |expr| {
            if (!can_cast_to(statement.decleration.type, evaluate_expression_type(symbol_table, function_table, expr))) unreachable;
        }
    }
    if (statement.* == .return_statement) {
        if (!std.meta.eql(function_table.return_type, evaluate_expression_type(symbol_table, function_table, statement.return_statement))) unreachable;
    }
    if (statement.* == .if_statement) {
        for (statement.if_statement.statement_list.items) |stmt| {
            try variable_decleration_pass(allocator, symbol_table, function_table, stmt);
        }
        if (statement.if_statement.else_statement) |else_stmt| {
            try variable_decleration_pass(allocator, symbol_table, function_table, else_stmt);
        }
    }
    if (statement.* == .while_statement) {
        for (statement.while_statement.statement_list.items) |stmt| {
            try variable_decleration_pass(allocator, symbol_table, function_table, stmt);
        }
    }
    if (statement.* == .else_statement) {
        for (statement.else_statement.items) |stmt| {
            try variable_decleration_pass(allocator, symbol_table, function_table, stmt);
        }
    }
}

fn type_checking_pass(symbol_table: *GlobalTable, function_table: *FunctionTable, statement: *Node) void {
    if (statement.* == .assignment) {
        if (statement.assignment.identifier.* == .identifier) {
            const variable = function_table.get_parameter_or_variable(statement.assignment.identifier.identifier) orelse unreachable;
            const variable_type = variable.type;
            if (!can_cast_to(variable_type, evaluate_expression_type(symbol_table, function_table, statement.assignment.expression))) unreachable;
        }
        if (statement.assignment.identifier.* == .array_index) {
            const variable = function_table.get_parameter_or_variable(statement.assignment.identifier.array_index.identifier) orelse unreachable;
            const variable_type = variable.type;
            if (!can_cast_to(variable_type, evaluate_expression_type(symbol_table, function_table, statement.assignment.expression))) unreachable;
        }
    }
    if (statement.* == .if_statement) {
        if (!std.meta.eql(Types.bool, evaluate_expression_type(symbol_table, function_table, statement.if_statement.expression))) unreachable;

        for (statement.if_statement.statement_list.items) |stmt| {
            type_checking_pass(symbol_table, function_table, stmt);
        }
        if (statement.if_statement.else_statement) |else_stmt| {
            type_checking_pass(symbol_table, function_table, else_stmt);
        }
    }
    if (statement.* == .while_statement) {
        if (!std.meta.eql(Types.bool, evaluate_expression_type(symbol_table, function_table, statement.while_statement.expression))) unreachable;
        for (statement.while_statement.statement_list.items) |stmt| {
            type_checking_pass(symbol_table, function_table, stmt);
        }
    }
    if (statement.* == .else_statement) {
        for (statement.else_statement.items) |stmt| {
            type_checking_pass(symbol_table, function_table, stmt);
        }
    }
}

fn evaluate_expression_type(global_table: *GlobalTable, function_table: *FunctionTable, expression: *Node) Types {
    var _type: Types = .void;
    if (expression.* == .integer_literal) return min_literal_size(expression.integer_literal);
    if (expression.* == .character_literal) return Types.char;
    if (expression.* == .string_literal) return .{ .char_array = expression.string_literal.len - 1 };
    if (expression.* == .byte_in_statement) return Types.char;
    if (expression.* == .identifier) {
        return if (function_table.get_parameter_or_variable(expression.identifier)) |variable| variable.type else unreachable;
    }
    if (expression.* == .array_index) return Types.char;
    if (expression.* == .function_call) {
        const function = global_table.get_function(expression.function_call.name) orelse unreachable;
        if (function_table.parameters.count() != expression.function_call.parameter_expressions.items.len) unreachable;

        // check that the parameters are of the correct type...
        for (expression.function_call.parameter_expressions.items, 0..) |expr, i| {
            const expr_type = evaluate_expression_type(global_table, function_table, expr);
            const param_type = function.parameters.values()[i].type;
            if (!std.meta.eql(expr_type, param_type)) unreachable;
        }
        // check the return value
        return function.return_type;
    }
    if (expression.* == .binary_op) {
        const left_type = evaluate_expression_type(global_table, function_table, expression.binary_op.left);
        const right_type = evaluate_expression_type(global_table, function_table, expression.binary_op.right);
        // as long as one can be cast to the other,
        _type = right_type;
        if (can_cast_to(left_type, right_type)) {
            _type = left_type;
        } else if (can_cast_to(right_type, left_type)) {
            _type = right_type;
        } else {
            unreachable;
        }
        // and the operator isnt .Eq, .Neq, .Geq, .Leq, .Lt, .Gt, which casts the type to booleans
        switch (expression.binary_op.op) {
            .Eql, .Neq, .Geq, .Leq, .Lt, .Gt => {
                _type = Types.bool;
            },
            // and its an error to do the short circuit ops on non booleans
            .And, .Or => {
                if (_type != .bool) unreachable;
            },
            else => {},
        }
    }
    return _type;
}

fn min_literal_size(literal: u64) Types {
    if (literal <= 255) return .char;
    if (literal <= 65535) {} // two byte integer.. not implemented
    if (literal <= 4294967295) {} // four byte integer.. not implemented
    if (literal > 18446744073709551615) unreachable;
    return .int;
}

fn can_cast_to(to: Types, from: Types) bool {
    if (size_of_type(from) <= size_of_type(to)) return true;
    return false;
}
