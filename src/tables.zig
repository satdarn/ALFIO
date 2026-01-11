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

    pub fn calculate_offsets(self: *FunctionTable) !void {
        var param_index: usize = 0;
        var param_iter = self.parameters.iterator();
        while (param_iter.next()) |entry| {
            const var_entry = entry.value_ptr;
            if (param_index >= 6) {
                var_entry.offset = 16 + @as(i64, @intCast(param_index)) * 8 - 48; // Since 6*8 = 48
            }
            param_index += 1;
        }
        var total_stack_size: i64 = 0;

        var var_iter = self.variables.iterator();
        while (var_iter.next()) |entry| {
            const var_entry = entry.value_ptr;
            const size = var_entry.type.size_of();
            const num_chucks: i64 = try std.math.divCeil(i64, (size), 8);
            total_stack_size += num_chucks * 8;
            var_entry.offset = -total_stack_size;
        }
        param_index = 0;
        param_iter = self.parameters.iterator();
        while (param_iter.next()) |entry| {
            const var_entry = entry.value_ptr;
            if (param_index < 6) {
                const size = var_entry.type.size_of();
                const num_chucks: i64 = try std.math.divCeil(i64, size, 8);
                total_stack_size += num_chucks * 8;
                var_entry.offset = -total_stack_size;
            }
            param_index += 1;
        }
        self.frame_size = (total_stack_size + 15) & ~@as(i64, 15);
    }
};

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

    pub fn calculate_offsets(self: *GlobalTable) !void {
        var fn_iter = self.table.keyIterator();
        while (fn_iter.next()) |fn_name| {
            if (self.table.getPtr(fn_name.*)) |fn_tbl| {
                try fn_tbl.calculate_offsets();
            }
        }
    }

    pub fn print_tables(global_table: *GlobalTable) void {
        var fn_iter = global_table.table.iterator();
        while (fn_iter.next()) |entry| {
            const fn_name = entry.key_ptr.*;
            const fn_table = entry.value_ptr;

            std.debug.print("\n{s}() -> {s} (frame: {d} bytes)\n", .{ fn_name, @tagName(fn_table.return_type), fn_table.frame_size });
            std.debug.print("{s:-<40}\n", .{""});

            if (fn_table.parameters.count() > 0) {
                std.debug.print("Parameters:\n", .{});
                var param_iter = fn_table.parameters.iterator();
                while (param_iter.next()) |param_entry| {
                    std.debug.print("  {s} {s} [rbp{d}]\n", .{ param_entry.key_ptr.*, @tagName(param_entry.value_ptr.type), param_entry.value_ptr.offset });
                }
            }

            if (fn_table.variables.count() > 0) {
                std.debug.print("Variables:\n", .{});
                var var_iter = fn_table.variables.iterator();
                while (var_iter.next()) |var_entry| {
                    std.debug.print("  {s} {s} [rbp{d}]\n", .{ var_entry.key_ptr.*, @tagName(var_entry.value_ptr.type), var_entry.value_ptr.offset });
                }
            }

            if (fn_table.parameters.count() == 0 and fn_table.variables.count() == 0) {
                std.debug.print("  (no parameters or variables)\n", .{});
            }
        }
    }
};

pub fn create_global_table(allocator: std.mem.Allocator, ast: *Node) !*GlobalTable {
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
        for (function.function_def.statement_list.items) |statement| {
            try check_for_variable_decleration(allocator, symbol_table, function_table, statement);
        }
    }
    return symbol_table;
}

fn check_for_variable_decleration(allocator: std.mem.Allocator, symbol_table: *GlobalTable, function_table: *FunctionTable, statement: *Node) !void {
    switch (statement.*) {
        .decleration => {
            try function_table.insert_new_variable(allocator, statement.decleration.identifier, statement.decleration.type);
        },
        .if_statement => {
            for (statement.if_statement.statement_list.items) |stmt| {
                try check_for_variable_decleration(allocator, symbol_table, function_table, stmt);
            }
            if (statement.if_statement.else_statement) |else_stmt| {
                try check_for_variable_decleration(allocator, symbol_table, function_table, else_stmt);
            }
        },
        .while_statement => {
            for (statement.while_statement.statement_list.items) |stmt| {
                try check_for_variable_decleration(allocator, symbol_table, function_table, stmt);
            }
        },
        .for_statement => {
            try check_for_variable_decleration(allocator, symbol_table, function_table, statement.for_statement.decleration);
            try check_for_variable_decleration(allocator, symbol_table, function_table, statement.for_statement.statement);

            for (statement.for_statement.statement_list.items) |stmt| {
                try check_for_variable_decleration(allocator, symbol_table, function_table, stmt);
            }
        },
        .else_statement => {
            for (statement.else_statement.statement_list.items) |stmt| {
                try check_for_variable_decleration(allocator, symbol_table, function_table, stmt);
            }
        },
        else => {},
    }
}
