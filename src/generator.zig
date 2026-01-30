const std = @import("std");
const Node = @import("nodes.zig").Node;
const GlobalTable = @import("tables.zig").GlobalTable;
const FunctionTable = @import("tables.zig").FunctionTable;
const Types = @import("lexer.zig").Types;
const evaluate_expression_type = @import("analysis.zig").evaluate_expression_type;
const stdlib_pregen = @import("stdlib_generated.zig").stdlib_assembly_x86_64;
const GeneratorError = error{
    OutOfMemory,
    NoSpaceLeft,
    Overflow,
};
const LoopContext = struct {
    start: u32,
    end: u32,
};

const StringLiteralEntry = struct {
    label: []const u8,
    value: []const u8,
};

const Generator = struct {
    output: *std.ArrayList(u8),
    symbol_table: *GlobalTable,
    current_function_table: *FunctionTable,
    scratch_allocator: *ScratchRegisters,
    label_count: *u32,
    loop_stack: std.ArrayList(LoopContext),
    string_literals: *std.ArrayList(StringLiteralEntry),
};
pub fn generate_code(allocator: std.mem.Allocator, ast: *Node, global_table: *GlobalTable, is_stdlib: bool) ![]const u8 {
    var output = try (std.ArrayList(u8)).initCapacity(allocator, 100);
    defer output.deinit(allocator);
    var string_literals = try std.ArrayList(StringLiteralEntry).initCapacity(allocator, 10);
    defer {
        for (string_literals.items) |entry| {
            allocator.free(entry.label);
            allocator.free(entry.value);
        }
        string_literals.deinit(allocator);
    }
    try generate_prolog(allocator, &output, global_table, is_stdlib);
    if (ast.* == .program) {
        var label_count: u32 = 0;
        for (ast.program.items) |node| {
            if (node.* == .function_def) {
                var scratch_allocator: ScratchRegisters = .{};
                var loop_stack = try (std.ArrayList(LoopContext)).initCapacity(allocator, 10);
                defer loop_stack.deinit(allocator);
                var generator: Generator = .{
                    .output = &output,
                    .symbol_table = global_table,
                    .current_function_table = global_table.get_function(node.function_def.name) orelse unreachable,
                    .label_count = &label_count,
                    .scratch_allocator = &scratch_allocator,
                    .loop_stack = loop_stack,
                    .string_literals = &string_literals,
                };
                try generate_function_prolog(allocator, &generator, node);
                try implict_zero_init(allocator, &generator);
                try generate_statements(allocator, &generator, node);
                try generate_function_epilog(allocator, &generator);
            }
        }
    }
    if (string_literals.items.len > 0) {
        try output.appendSlice(allocator, "\n.section .rodata\n");
        for (string_literals.items) |entry| {
            try output.appendSlice(allocator, try std.fmt.allocPrint(allocator, "{s}:\n    .string \"{s}\"\n", .{entry.label, entry.value}));
        }
    }

    return output.toOwnedSlice(allocator);
}

const ScratchRegistersEnum = enum { rcx, rdx, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 };
const _64BitRegistersEnum = enum { rax, rcx, rdx, rbx, rsi, rdi, rsp, rbp, r8, r9, r10, r11, r12, r13, r14, r15 };
const _32BitRegistersEnum = enum { eax, ecx, edx, ebx, esi, edi, esp, ebp, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d };
const _16BitRegistersEnum = enum { ax, cx, dx, bx, si, di, sp, bp, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w };
const _8BitRegistersEnum = enum { al, cl, dl, bl, sil, dil, spl, bpl, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b };

// Convert 64-bit register name to its 32-bit lower version
fn lower_32_reg(reg: []const u8) []const u8 {
    const map = std.StaticStringMap([]const u8).initComptime(.{
        .{ "rax", "eax" },
        .{ "rcx", "ecx" },
        .{ "rdx", "edx" },
        .{ "rbx", "ebx" },
        .{ "rsi", "esi" },
        .{ "rdi", "edi" },
        .{ "rsp", "esp" },
        .{ "rbp", "ebp" },
        .{ "r8", "r8d" },
        .{ "r9", "r9d" },
        .{ "r10", "r10d" },
        .{ "r11", "r11d" },
        .{ "r12", "r12d" },
        .{ "r13", "r13d" },
        .{ "r14", "r14d" },
        .{ "r15", "r15d" },
    });

    return map.get(reg) orelse reg;
}

// Convert 64-bit register name to its 8-bit lower version
fn lower_8_reg(reg: []const u8) []const u8 {
    const map = std.StaticStringMap([]const u8).initComptime(.{
        .{ "rax", "al" },
        .{ "rcx", "cl" },
        .{ "rdx", "dl" },
        .{ "rbx", "bl" },
        .{ "rsi", "sil" },
        .{ "rdi", "dil" },
        .{ "rsp", "spl" },
        .{ "rbp", "bpl" },
        .{ "r8", "r8b" },
        .{ "r9", "r9b" },
        .{ "r10", "r10b" },
        .{ "r11", "r11b" },
        .{ "r12", "r12b" },
        .{ "r13", "r13b" },
        .{ "r14", "r14b" },
        .{ "r15", "r15b" },
    });

    return map.get(reg) orelse reg;
}

fn upper_64_reg(reg: []const u8) []const u8 {
    const map = std.StaticStringMap([]const u8).initComptime(.{
        .{ "eax", "rax" },
        .{ "ecx", "rcx" },
        .{ "edx", "rdx" },
        .{ "ebx", "rbx" },
        .{ "esi", "rsi" },
        .{ "edi", "rdi" },
        .{ "esp", "rsp" },
        .{ "ebp", "rbp" },
        .{ "r8d", "r8" },
        .{ "r9d", "r9" },
        .{ "r10d", "r10" },
        .{ "r11d", "r11" },
        .{ "r12d", "r12" },
        .{ "r13d", "r13" },
        .{ "r14d", "r14" },
        .{ "r15d", "r15" },
        .{ "ax", "rax" },
        .{ "cx", "rcx" },
        .{ "dx", "rdx" },
        .{ "bx", "rbx" },
        .{ "si", "rsi" },
        .{ "di", "rdi" },
        .{ "sp", "rsp" },
        .{ "bp", "rbp" },
        .{ "r8w", "r8" },
        .{ "r9w", "r9" },
        .{ "r10w", "r10" },
        .{ "r11w", "r11" },
        .{ "r12w", "r12" },
        .{ "r13w", "r13" },
        .{ "r14w", "r14" },
        .{ "r15w", "r15" },
        .{ "al", "rax" },
        .{ "cl", "rcx" },
        .{ "dl", "rdx" },
        .{ "bl", "rbx" },
        .{ "sil", "rsi" },
        .{ "dil", "rdi" },
        .{ "spl", "rsp" },
        .{ "bpl", "rbp" },
        .{ "r8b", "r8" },
        .{ "r9b", "r9" },
        .{ "r10b", "r10" },
        .{ "r11b", "r11" },
        .{ "r12b", "r12" },
        .{ "r13b", "r13" },
        .{ "r14b", "r14" },
        .{ "r15b", "r15" },
    });

    return map.get(reg) orelse reg;
}
fn scratchRegisterCount() comptime_int {
    return @typeInfo(ScratchRegistersEnum).@"enum".fields.len;
}

const ScratchRegisters = struct {
    in_use: [scratchRegisterCount()]bool = .{false} ** scratchRegisterCount(),
    pub fn scratch_alloc(self: *ScratchRegisters) ?ScratchRegistersEnum {
        for (&self.in_use, 0..) |*reg, i| {
            if (!reg.*) {
                reg.* = true;
                const register: ScratchRegistersEnum = @enumFromInt(i);
                return register;
            }
        }
        unreachable;
    }

    pub fn alloc_for_func_call(self: *ScratchRegisters) void {
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdi)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rsi)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdx)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rcx)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r8)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r9)] = true;
    }
    pub fn alloc_for_syscall(self: *ScratchRegisters) void {
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdi)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rsi)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdx)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rcx)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r8)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r9)] = true;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r10)] = true;
    }

    pub fn free_for_func_call(self: *ScratchRegisters) void {
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdi)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rsi)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdx)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rcx)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r8)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r9)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r10)] = false;
    }
    pub fn free_for_syscall(self: *ScratchRegisters) void {
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdi)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rsi)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rdx)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.rcx)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r8)] = false;
        self.in_use[@intFromEnum(ScratchRegistersEnum.r9)] = false;
    }
    pub fn scratch_free_by_name(self: *ScratchRegisters, name: []const u8) void {
        const reg = std.meta.stringToEnum(ScratchRegistersEnum, upper_64_reg(name)) orelse {
            return;
        };
        self.scratch_free(reg);
    }
    pub fn scratch_free(self: *ScratchRegisters, reg: ScratchRegistersEnum) void {
        self.in_use[@intFromEnum(reg)] = false;
    }
    pub fn reset(self: *ScratchRegisters) void {
        for (&self.in_use) |*reg| {
            reg.* = false;
        }
    }
};

fn generate_prolog(allocator: std.mem.Allocator, output: *std.ArrayList(u8), global_table: *GlobalTable, is_stdlib: bool) !void {
    var buffer: [256]u8 = undefined;
    const prolog = if (!is_stdlib)
        \\.intel_syntax noprefix
        \\.global _start
        \\.text
        \\_start:
        \\      xor rbp, rbp
        \\      mov rdi, [rsp]           # argc
        \\      lea rsi, [rsp + 8]       # argv
        \\      and rsp, -16
        \\      call main                # main(argc, argv, envp)
        \\      mov edi, eax             # Move return value to exit argument (edi)
        \\      mov eax, 60              # syscall number for exit (60 on x86-64)
        \\      syscall                  # exit(main_return_value)
        \\      hlt
    else
        \\.intel_syntax noprefix
        \\.text
    ;
    if (is_stdlib) {
        var iter = global_table.table.keyIterator();
        while (iter.next()) |name| {
            try output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".global {s}\n", .{name.*}));
        }
    }
    try output.appendSlice(allocator, prolog);
    try output.append(allocator, '\n');
}

fn generate_function_prolog(allocator: std.mem.Allocator, generator: *Generator, function: *Node) !void {
    var buffer: [256]u8 = undefined;
    const registers = [_][]const u8{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };
    const prolog =
        \\{s}:
        \\     push rbp
        \\     mov rbp, rsp
        \\     sub rsp, {d}
    ;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, prolog, .{ function.function_def.name, generator.current_function_table.frame_size }));
    try generator.output.append(allocator, '\n');
    if (function.function_def.parameters.items.len > 5) {
        for (function.function_def.parameters.items[0..6], 0..) |param, i| {
            if (generator.current_function_table.get_parameter_or_variable(param.function_parameter.name)) |target| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp - {}], {s}\n", .{ @abs(target.offset), registers[i] }));
            }
        }
    } else {
        for (function.function_def.parameters.items, 0..) |param, i| {
            if (generator.current_function_table.get_parameter_or_variable(param.function_parameter.name)) |target| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp - {}], {s}\n", .{ @abs(target.offset), registers[i] }));
            }
        }
    }
}

fn generate_function_epilog(allocator: std.mem.Allocator, generator: *Generator) !void {
    const epilog =
        \\     xor rax, rax
        \\     mov rsp, rbp
        \\     pop rbp     
        \\     ret         
    ;
    try generator.output.appendSlice(allocator, epilog);
    try generator.output.append(allocator, '\n');
}

fn generate_statements(allocator: std.mem.Allocator, generator: *Generator, ast: *Node) !void {
    for (ast.function_def.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
}
fn generate_statement(allocator: std.mem.Allocator, generator: *Generator, node: *Node) GeneratorError!void {
    switch (node.*) {
        .function_call => try generate_function_call(allocator, generator, node),
        .syscall => try generate_syscall(allocator, generator, node),
        .decleration => try generate_decleration(allocator, generator, node),
        .assignment => try generate_assignment(allocator, generator, node),
        .deref_assignment => try generate_deref_assignment(allocator, generator, node),
        .if_statement => try generate_if(allocator, generator, node),
        .while_statement => try generate_while(allocator, generator, node),
        .for_statement => try generate_for(allocator, generator, node),
        .return_statement => try generate_return(allocator, generator, node),
        .break_statement => try generate_break(allocator, generator),
        .const_decleration => |const_decl| {
            // Store string literal for emission at end
            if (const_decl.expression.* == .string_literal) {
                const constant = generator.current_function_table.get_constant(const_decl.identifier) orelse unreachable;
                const label = try allocator.dupe(u8, constant.value.string_label);
                const value = try allocator.dupe(u8, const_decl.expression.string_literal.value);
                try generator.string_literals.append(allocator, .{.label = label, .value = value});
            }
        },
        else => {},
    }
}

fn generate_function_call(allocator: std.mem.Allocator, generator: *Generator, function_call: *Node) !void {
    var buffer: [256]u8 = undefined;
    const registers = [_][]const u8{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };
    generator.scratch_allocator.alloc_for_func_call();
    if (function_call.function_call.parameter_expressions.items.len > 6) {
        var i: usize = function_call.function_call.parameter_expressions.items.len - 1;
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub rsp, {d}\n", .{(i - 5) * 8}));
        while (i >= 6) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i], Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     push {s}\n", .{result_register}));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }

        while (i > 0) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i], Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
        if (i == 0) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i], Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     call {s}\n", .{function_call.function_call.name}));
        const stack_bytes = (function_call.function_call.parameter_expressions.items.len - 6) * 8;
        const padding = if ((stack_bytes) % 16 == 8) 8 + stack_bytes else stack_bytes;
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     add rsp, {d}\n", .{padding}));
        return;
    } else if (function_call.function_call.parameter_expressions.items.len != 0) {
        var i: usize = function_call.function_call.parameter_expressions.items.len - 1;
        while (i > 0) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i], Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
        if (i == 0) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i], Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     call {s}\n", .{function_call.function_call.name}));
    generator.scratch_allocator.free_for_func_call();
}
fn generate_syscall(allocator: std.mem.Allocator, generator: *Generator, syscall: *Node) !void {
    var buffer: [256]u8 = undefined;
    const registers = [_][]const u8{ "rax", "rdi", "rsi", "rdx", "r10", "r8", "r9" };

    var i: usize = syscall.syscall.parameter_expressions.items.len - 1;
    generator.scratch_allocator.alloc_for_syscall();
    while (i > 0) : (i -= 1) {
        const result_register = try evaluate_expression(allocator, generator, syscall.syscall.parameter_expressions.items[i], Types.Word);
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
        generator.scratch_allocator.scratch_free_by_name(result_register);
    }
    if (i == 0) {
        const result_register = try evaluate_expression(allocator, generator, syscall.syscall.parameter_expressions.items[i], Types.Word);
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
        generator.scratch_allocator.scratch_free_by_name(result_register);
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     syscall\n", .{}));
    generator.scratch_allocator.free_for_syscall();
}

fn generate_decleration(allocator: std.mem.Allocator, generator: *Generator, node: *Node) !void {
    var buffer: [66]u8 = undefined;
    if (node.decleration.expression) |expression| {
        if (expression.* == .string_literal) {
            const variable = generator.current_function_table.get_parameter_or_variable(node.decleration.identifier) orelse return;
            const str = expression.string_literal.value;
            for (str, 0..) |byte, i| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [rbp - {}], {}\n", .{ @abs(variable.offset) - i, byte }));
            }
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [rbp - {}], 0\n", .{@abs(variable.offset) - str.len}));
        } else {
            const result_register = try evaluate_expression(allocator, generator, expression, Types.Word);
            try store_variable(allocator, generator, node.decleration.identifier, result_register);
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
    }
}
fn generate_deref_assignment(allocator: std.mem.Allocator, generator: *Generator, node: *Node) !void {
    var buffer: [256]u8 = undefined;

    // Evaluate the value to store
    const _type = evaluate_expression_type(generator.symbol_table, generator.current_function_table, node.deref_assignment.expression) catch unreachable;
    const value_reg = try evaluate_expression(allocator, generator, node.deref_assignment.expression, _type);

    // Evaluate the pointer expression (could be cast or just identifier)
    var ptr_expr = node.deref_assignment.identifier;

    // Determine store size based on cast
    var store_size: i64 = 8; // default
    var value_suffix: []const u8 = value_reg;
    var ptr_type: []const u8 = "QWORD";

    if (ptr_expr.* == .cast) {
        store_size = ptr_expr.cast.target.size_of();
        ptr_type = switch (store_size) {
            1 => "BYTE",
            2 => "WORD",
            4 => "DWORD",
            else => "QWORD",
        };
        value_suffix = switch (store_size) {
            1 => lower_8_reg(value_reg),
            4 => lower_32_reg(value_reg),
            else => value_reg,
        };
        ptr_expr = ptr_expr.cast.expression;
    }

    // Get the pointer register
    const ptr_reg = try evaluate_expression(allocator, generator, ptr_expr, Types.Word);

    // Store through pointer
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s} PTR [{s}], {s}\n", .{ ptr_type, ptr_reg, value_suffix }));

    generator.scratch_allocator.scratch_free_by_name(ptr_reg);
    generator.scratch_allocator.scratch_free_by_name(value_reg);
}
fn generate_assignment(allocator: std.mem.Allocator, generator: *Generator, assignment: *Node) !void {
    var buffer: [66]u8 = undefined;
    if (assignment.assignment.identifier.* == .identifier) {
        const result_register = try evaluate_expression(
            allocator,
            generator,
            assignment.assignment.expression,
            Types.Word,
        );
        try store_variable(allocator, generator, assignment.assignment.identifier.identifier.name, result_register);
        generator.scratch_allocator.scratch_free_by_name(result_register);
    }
    if (assignment.assignment.identifier.* == .array_index) {
        const result_register = try evaluate_expression(
            allocator,
            generator,
            assignment.assignment.expression,
            Types.Char,
        );
        if (generator.current_function_table.get_parameter_or_variable(assignment.assignment.identifier.array_index.identifier)) |variable| {
            const index_reg = try evaluate_expression(
                allocator,
                generator,
                assignment.assignment.identifier.array_index.expression,
                Types.Word,
            );

            if (generator.scratch_allocator.scratch_alloc()) |target| {
                if (variable.offset < 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp - {}]\n", .{ @tagName(target), @abs(variable.offset) }));
                } else {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp + {}]\n", .{ @tagName(target), @abs(variable.offset) }));
                }

                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     add {s}, {s}\n", .{ @tagName(target), index_reg }));

                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [{s}], {s}\n", .{ @tagName(target), lower_8_reg(result_register) }));

                generator.scratch_allocator.scratch_free_by_name(index_reg);
            }
        }
        generator.scratch_allocator.scratch_free_by_name(result_register);
    }
}


fn generate_if(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    const false_lbl = generator.label_count.* + 1;
    generator.label_count.* += 1;
    const end_lbl = generator.label_count.* + 1;
    generator.label_count.* += 1;
    var buffer: [66]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.if_statement.expression, Types.Bool);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, 0\n     je .L_IF{d}\n", .{ result_register, false_lbl }));
    generator.scratch_allocator.scratch_free_by_name(result_register);
    for (statement.if_statement.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     jmp .L_IF{d}\n", .{end_lbl}));
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_IF{d}:\n", .{false_lbl}));
    if (statement.if_statement.else_statement) |else_statement| {
        if (else_statement.* == .if_statement) {
            try generate_if(allocator, generator, else_statement);
        }
        if (else_statement.* == .else_statement) {
            for (else_statement.else_statement.statement_list.items) |node| {
                try generate_statement(allocator, generator, node);
            }
        }
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_IF{d}:\n", .{end_lbl}));
}

fn generate_while(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    generator.label_count.* += 1;
    const start = generator.label_count.*;
    generator.label_count.* += 1;
    const end = generator.label_count.*;
    var buffer: [66]u8 = undefined;
    try generator.loop_stack.append(allocator, .{ .start = start, .end = end });
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_LOOP{d}:\n", .{start}));
    const result_register = try evaluate_expression(allocator, generator, statement.while_statement.expression, Types.Bool);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, 0\n     je .L_LOOP{d}\n", .{ result_register, end }));
    generator.scratch_allocator.scratch_free_by_name(result_register);
    for (statement.while_statement.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     jmp .L_LOOP{d}\n", .{start}));
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_LOOP{d}:\n", .{end}));
    _ = generator.loop_stack.pop();
}
fn generate_for(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    generator.label_count.* += 1;
    const start = generator.label_count.*;
    generator.label_count.* += 1;
    const end = generator.label_count.*;
    var buffer: [66]u8 = undefined;
    try generate_decleration(allocator, generator, statement.for_statement.decleration);
    try generator.loop_stack.append(allocator, .{ .start = start, .end = end });
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_LOOP{d}:\n", .{start}));
    const result_register = try evaluate_expression(allocator, generator, statement.for_statement.condition, Types.Bool);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, 0\n     je .L_LOOP{d}\n", .{ result_register, end }));
    generator.scratch_allocator.scratch_free_by_name(result_register);
    for (statement.for_statement.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
    try generate_statement(allocator, generator, statement.for_statement.statement);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     jmp .L_LOOP{d}\n", .{start}));
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_LOOP{d}:\n", .{end}));
    _ = generator.loop_stack.pop();
}

fn generate_return(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [1024]u8 = undefined;
    if (statement.return_statement.expression) |return_statement| {
        const result_register = upper_64_reg(try evaluate_expression(allocator, generator, return_statement, generator.current_function_table.return_type));
        switch (generator.current_function_table.return_type) {
            .bool => {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movzx rax, {s}\n", .{lower_8_reg(result_register)}));
            },
            .char => {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movzx rax, {s}\n", .{lower_8_reg(result_register)}));
            },
            .int => {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movsxd rax, {s}\n", .{lower_32_reg(result_register)}));
            },
            .word => {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov rax, {s}\n", .{result_register}));
            },
            else => unreachable,
        }
        generator.scratch_allocator.scratch_free_by_name(result_register);
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov rsp, rbp\n     pop rbp\n     ret\n", .{}));
}

fn generate_break(allocator: std.mem.Allocator, generator: *Generator) !void {
    if (generator.loop_stack.items.len == 0) unreachable;
    const current_loop = generator.loop_stack.getLast();
    var buffer: [1024]u8 = undefined;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     jmp .L_LOOP{d}\n", .{current_loop.end}));
}

fn evaluate_expression(allocator: std.mem.Allocator, generator: *Generator, expression: *Node, _type: Types) GeneratorError![]const u8 {
    var buffer: [1024]u8 = undefined;
    switch (expression.*) {
        .integer_literal => {
            if (generator.scratch_allocator.scratch_alloc()) |target| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {d}\n", .{ @tagName(target), expression.integer_literal.value }));
                return reg_from_type(@tagName(target), _type);
            }
        },
        .character_literal => {
            if (generator.scratch_allocator.scratch_alloc()) |target| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {d}\n", .{ @tagName(target), expression.character_literal.value }));
                return reg_from_type(@tagName(target), _type);
            }
        },
        .identifier => {
            if (generator.scratch_allocator.scratch_alloc()) |target| {
                return try load_variable_value_to_register(allocator, generator, expression.identifier.name, @tagName(target), Types.Word);
            }
        },
        .array_index => {
            if (generator.scratch_allocator.scratch_alloc()) |target| {
                const array_var = generator.current_function_table.get_parameter_or_variable(expression.array_index.identifier) orelse unreachable;
                const index_reg = try evaluate_expression(allocator, generator, expression.array_index.expression, Types.char); //TODO if you make int or word arrays... then this needs to change
                if (array_var.offset < 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp - {}]\n", .{ @tagName(target), @abs(array_var.offset) }));
                } else {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp + {}]\n", .{ @tagName(target), @abs(array_var.offset) }));
                }
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "    add {s}, {s}\n", .{ @tagName(target), index_reg }));
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movzx {s}, BYTE PTR [{s}]\n", .{ @tagName(target), @tagName(target) }));
                generator.scratch_allocator.scratch_free_by_name(index_reg);
                return reg_from_type(@tagName(target), _type);
            }
        },
        .function_call => {
            try generate_function_call(allocator, generator, expression);
            return reg_from_type("rax", _type);
        },
        .syscall => {
            try generate_syscall(allocator, generator, expression);
            return reg_from_type("rax", _type);
        },
        .unary_op => {
            const reg = try evaluate_unary(allocator, generator, expression);
            return reg_from_type(reg, _type);
        },
        .cast => {
            switch (expression.cast.target) {
                .void => unreachable,
                .bool => return lower_8_reg(upper_64_reg(try evaluate_expression(allocator, generator, expression.cast.expression, Types.Bool))),
                .char => return lower_8_reg(upper_64_reg(try evaluate_expression(allocator, generator, expression.cast.expression, Types.Char))),
                .int => return lower_32_reg(upper_64_reg(try evaluate_expression(allocator, generator, expression.cast.expression, Types.Int))),
                .word => return upper_64_reg(try evaluate_expression(allocator, generator, expression.cast.expression, Types.Word)),
                else => unreachable,
            }
        },
        .binary_op => {
            const reg = try evaluate_binary(allocator, generator, expression, _type);
            return reg_from_type(reg, _type);
        },
        else => unreachable,
    }
    unreachable;
}
fn evaluate_unary(allocator: std.mem.Allocator, generator: *Generator, expression: *Node) ![]const u8 {
    var buffer: [256]u8 = undefined;
    switch (expression.unary_op.op) {
        .AddrOf => {
            if (expression.unary_op.expression.* != .identifier) unreachable; // you cant just call the address of anything
            if (generator.scratch_allocator.scratch_alloc()) |target| {
                try load_variable_address_to_register(allocator, generator, expression.unary_op.expression.identifier.name, @tagName(target));
                return @tagName(target);
            }
        },
        .Dref => {
            if (generator.scratch_allocator.scratch_alloc()) |target| {

                // Check if we're dereferencing a cast
                var ptr_type: []const u8 = "QWORD";
                var target_reg: []const u8 = @tagName(target);

                var expr_reg = try evaluate_expression(allocator, generator, expression.unary_op.expression, Types.Word);
                if (expression.unary_op.expression.* == .cast) {
                    const size = expression.unary_op.expression.cast.target.size_of();
                    ptr_type = switch (size) {
                        1 => "BYTE",
                        2 => "WORD",
                        4 => "DWORD",
                        else => "QWORD",
                    };
                    target_reg = switch (size) {
                        1 => lower_8_reg(@tagName(target)),
                        4 => lower_32_reg(@tagName(target)),
                        else => @tagName(target),
                    };
                    expr_reg = try evaluate_expression(allocator, generator, expression.unary_op.expression.cast.expression, Types.Word);
                }

                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     xor {s}, {s}\n", .{ upper_64_reg(target_reg), upper_64_reg(target_reg) }));
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s} PTR [{s}]\n", .{ target_reg, ptr_type, expr_reg }));
                generator.scratch_allocator.scratch_free_by_name(expr_reg);

                return @tagName(target);
            }
        },
        .Not => {
            const expr_reg = try evaluate_expression(allocator, generator, expression.unary_op.expression, Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     not {s}\n", .{expr_reg}));
            return expr_reg;
        },
        .bNeg => {
            const expr_reg = try evaluate_expression(allocator, generator, expression.unary_op.expression, Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     not {s}\n", .{expr_reg}));
            return expr_reg;
        },
        .Neg => {
            const expr_reg = try evaluate_expression(allocator, generator, expression.unary_op.expression, Types.Word);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     neg {s}\n", .{expr_reg}));
            return expr_reg;
        },
    }
    unreachable;
}
fn evaluate_binary(allocator: std.mem.Allocator, generator: *Generator, expression: *Node, _type: Types) ![]const u8 {
    var buffer: [256]u8 = undefined;
    const left = try evaluate_expression(allocator, generator, expression.binary_op.left, _type);
    const right = try evaluate_expression(allocator, generator, expression.binary_op.right, _type);
    const left_target = upper_64_reg(left);
    const right_target = upper_64_reg(right);
    switch (expression.binary_op.op) {
        .Add => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     add {s}, {s}\n", .{ left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Sub => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, {s}\n", .{ left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Mult => {
            const mult =
                \\     push rax
                \\     push rbx 
                \\     mov rax, {s}
                \\     mov rbx, {s}
                \\     imul rax, rbx 
                \\     mov {s}, rax
                \\     pop rbx
                \\     pop rax
            ;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, mult, .{ left_target, right_target, left_target }));
            try generator.output.append(allocator, '\n');
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Div => {
            const div =
                \\     push rax
                \\     push rbx
                \\     push rdx
                \\     mov rax, {s}
                \\     mov rbx, {s}
                \\     mov edx, 0
                \\     div rbx
                \\     mov {s}, rax
                \\     pop rdx
                \\     pop rbx
                \\     pop rax
            ;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, div, .{ left_target, right_target, left_target }));
            try generator.output.append(allocator, '\n');
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Mod => {
            const mod =
                \\     push rax
                \\     push rbx
                \\     push rdx
                \\     mov rax, {s}
                \\     mov rbx, {s}
                \\     mov edx, 0
                \\     div rbx
                \\     mov {s}, rdx
                \\     pop rdx
                \\     pop rbx
                \\     pop rax
            ;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, mod, .{ left_target, right_target, left_target }));
            try generator.output.append(allocator, '\n');
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Eql => {
            const right_64 = upper_64_reg(right_target);
            const left_64 = upper_64_reg(left_target);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     sete al\n     movzx {s}, al\n", .{ left_64, right_64, upper_64_reg(left_64) }));
            generator.scratch_allocator.scratch_free_by_name(right_64);
        },
        .Neq => {
            const right_64 = upper_64_reg(right_target);
            const left_64 = upper_64_reg(left_target);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setne al\n     movzx {s}, al\n", .{ left_64, right_64, upper_64_reg(left_64) }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Leq => {
            const right_64 = upper_64_reg(right_target);
            const left_64 = upper_64_reg(left_target);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setbe al\n     movzx {s}, al\n", .{ left_64, right_64, upper_64_reg(left_64) }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Geq => {
            const right_64 = upper_64_reg(right_target);
            const left_64 = upper_64_reg(left_target);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setae al\n     movzx {s}, al\n", .{ left_64, right_64, upper_64_reg(left_64) }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Lt => {
            const right_64 = upper_64_reg(right_target);
            const left_64 = upper_64_reg(left_target);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setb al\n     movzx {s}, al\n", .{ left_64, right_64, upper_64_reg(left_64) }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Gt => {
            const right_64 = upper_64_reg(right_target);
            const left_64 = upper_64_reg(left_target);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     seta al\n     movzx {s}, al\n", .{ left_64, right_64, upper_64_reg(left_64) }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .And => {
            const and_asm =
                \\     cmp {s}, 0
                \\     je .L_AND{d}
                \\     cmp {s}, 0
                \\     je .L_AND{d}
                \\     mov {s}, 1 
                \\     jmp .L_AND{d}
                \\.L_AND{d}:
                \\     mov {s}, 0
                \\.L_AND{d}:
            ;
            generator.label_count.* += 2;
            const false_lbl = generator.label_count.* + 1;
            const end_lbl = generator.label_count.*;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, and_asm, .{
                left_target,
                false_lbl,
                right_target,
                false_lbl,
                left_target,
                end_lbl,
                false_lbl,
                left_target,
                end_lbl,
            }));
            try generator.output.append(allocator, '\n');
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Or => {
            const or_asm =
                \\     cmp {s}, 1
                \\     je .L_OR{d}
                \\     cmp {s}, 1
                \\     je .L_OR{d}
                \\     mov {s}, 0 
                \\     jmp .L_OR{d}
                \\.L_OR{d}:
                \\     mov {s}, 1
                \\.L_OR{d}:
            ;
            generator.label_count.* += 2;
            const false_lbl = generator.label_count.* - 1;
            const end_lbl = generator.label_count.*;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, or_asm, .{
                left_target,
                false_lbl,
                right_target,
                false_lbl,
                left_target,
                end_lbl,
                false_lbl,
                left_target,
                end_lbl,
            }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .bAnd => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     and {s}, {s}\n", .{ left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .bOr => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     or {s}, {s}\n", .{ left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
    }
    return left_target;
}

fn comptime_calculations(allocator: std.mem.Allocator, generator: *Generator, expression: *Node) []const u8 {
    var buffer: [256]u8 = undefined;
    if (expression.binary_op.left.* == .integer_literal and expression.binary_op.right.* == .integer_literal) {
        var value: u64 = 0;
        switch (expression.binary_op.op) {
            .Add => {
                value = expression.binary_op.left.integer_literal + expression.binary_op.right.integer_literal;
            },
            .Sub => {
                value = expression.binary_op.left.integer_literal - expression.binary_op.right.integer_literal;
            },
            .Mult => {
                value = expression.binary_op.left.integer_literal * expression.binary_op.right.integer_literal;
            },
            .Div => {
                value = expression.binary_op.left.integer_literal / expression.binary_op.right.integer_literal;
            },
            .Mod => {
                value = expression.binary_op.left.integer_literal % expression.binary_op.right.integer_literal;
            },
            .Eql => {
                value = @intFromBool((expression.binary_op.left.integer_literal == expression.binary_op.right.integer_literal));
            },
            .Neq => {
                value = @intFromBool((expression.binary_op.left.integer_literal != expression.binary_op.right.integer_literal));
            },
            .Leq => {
                value = @intFromBool((expression.binary_op.left.integer_literal <= expression.binary_op.right.integer_literal));
            },
            .Geq => {
                value = @intFromBool((expression.binary_op.left.integer_literal >= expression.binary_op.right.integer_literal));
            },
            .Lt => {
                value = @intFromBool((expression.binary_op.left.integer_literal < expression.binary_op.right.integer_literal));
            },
            .Gt => {
                value = @intFromBool((expression.binary_op.left.integer_literal > expression.binary_op.right.integer_literal));
            },
            .And => {
                value = expression.binary_op.left.integer_literal & expression.binary_op.right.integer_literal;
            },
            .Or => {
                value = expression.binary_op.left.integer_literal | expression.binary_op.right.integer_literal;
            },
        }
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {} \n", .{ @tagName(target), value }));
            return @tagName(target);
        }
    }
}

fn implict_zero_init(allocator: std.mem.Allocator, generator: *Generator) !void {
    if (generator.current_function_table.variables.count() > 0) {
        try generator.output.appendSlice(allocator, "     xor rax, rax\n");
        var var_iter = generator.current_function_table.variables.iterator();
        while (var_iter.next()) |variable| {
            try store_variable(allocator, generator, variable.key_ptr.*, "rax");
        }
    }
}

fn load_variable_value_to_register(allocator: std.mem.Allocator, generator: *Generator, variable: []const u8, register: []const u8, _type: Types) ![]const u8 {
    var buffer: [256]u8 = undefined;
    const target = upper_64_reg(register);
    if (generator.current_function_table.get_parameter_or_variable(variable)) |vari| {
        if (vari.type.size_of() == 1) {
            if (vari.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movzx {s}, BYTE PTR [rbp - {}]\n", .{ target, @abs(vari.offset) }));
            }
            if (vari.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movzx {s}, BYTE PTR [rbp + {}]\n", .{ target, @abs(vari.offset) }));
            }
        }
        if (vari.type.size_of() == 4) {
            if (vari.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movsxd {s}, DWORD PTR [rbp - {}]\n", .{ target, @abs(vari.offset) }));
            }
            if (vari.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movsxd {s}, DWORD PTR [rbp + {}]\n", .{ target, @abs(vari.offset) }));
            }
        }
        if (vari.type.size_of() == 8) {
            if (vari.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, QWORD PTR [rbp - {}]\n", .{ target, @abs(vari.offset) }));
            }
            if (vari.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, QWORD PTR [rbp + {}]\n", .{ target, @abs(vari.offset) }));
            }
        }
        return reg_from_type(target, _type);
    }
    unreachable;
}
fn load_variable_address_to_register(allocator: std.mem.Allocator, generator: *Generator, variable: []const u8, register: []const u8) !void {
    var buffer: [256]u8 = undefined;
    if (generator.current_function_table.get_parameter_or_variable(variable)) |vari| {
        if (vari.offset < 0) {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, QWORD PTR [rbp - {}]\n", .{ register, @abs(vari.offset) }));
        }
        if (vari.offset > 0) {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, QWORD PTR [rbp + {}]\n", .{ register, @abs(vari.offset) }));
        }
    }
}
fn store_variable(allocator: std.mem.Allocator, generator: *Generator, variable: []const u8, register: []const u8) !void {
    var buffer: [256]u8 = undefined;
    const target = upper_64_reg(register);
    if (generator.current_function_table.get_parameter_or_variable(variable)) |vari| {
        if (vari.type.size_of() == 1) {
            if (vari.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [rbp - {}], {s}\n", .{ @abs(vari.offset), lower_8_reg(target) }));
            }
            if (vari.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [rbp + {}], {s}\n", .{ @abs(vari.offset), lower_8_reg(target) }));
            }
        }
        if (vari.type.size_of() == 4) {
            if (vari.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov DWORD PTR [rbp - {}], {s}\n", .{ @abs(vari.offset), lower_32_reg(target) }));
            }
            if (vari.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov DWORD PTR [rbp + {}], {s}\n", .{ @abs(vari.offset), lower_32_reg(target) }));
            }
        }
        if (vari.type.size_of() == 8) {
            if (vari.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp - {}], {s}\n", .{ @abs(vari.offset), target }));
            }
            if (vari.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp + {}], {s}\n", .{ @abs(vari.offset), target }));
            }
        }
    }
}

pub fn saveAndCompileAssembly(allocator: std.mem.Allocator, code: []const u8, output_name: []const u8, clean_up_flag: bool) !void {
    // Save assembly code to main.s
    const file = try std.fs.cwd().createFile("main.s", .{});
    defer file.close();
    try file.writeAll(code);
    const stdlib = try std.fs.cwd().createFile("stdlib.o", .{});
    defer stdlib.close();
    try stdlib.writeAll(stdlib_pregen);
    const as_result = try std.process.Child.run(.{ .allocator = allocator, .argv = &[_][]const u8{ "as", "main.s", "-o", "main.o" } });
    std.debug.print("{s}", .{as_result.stdout});
    std.debug.print("{s}", .{as_result.stderr});

    defer allocator.free(as_result.stdout);
    defer allocator.free(as_result.stderr);
    const ld_result = try std.process.Child.run(.{ .allocator = allocator, .argv = &[_][]const u8{
        "gcc",
        "-nostartfiles",
        "main.o",
        "stdlib.o",
        "-o",
        output_name,
    } });
    std.debug.print("{s}", .{ld_result.stdout});
    std.debug.print("{s}", .{ld_result.stderr});

    defer allocator.free(ld_result.stdout);
    defer allocator.free(ld_result.stderr);
    if (clean_up_flag) std.fs.cwd().deleteFile("main.s") catch {};
    if (clean_up_flag) std.fs.cwd().deleteFile("main.o") catch {};
    std.fs.cwd().deleteFile("stdlib.o") catch {};
}

fn reg_from_type(reg: []const u8, _type: Types) []const u8 {
    return switch (_type) {
        .bool => lower_8_reg(upper_64_reg(reg)),
        .char => lower_8_reg(upper_64_reg(reg)),
        .int => lower_32_reg(upper_64_reg(reg)),
        .word => upper_64_reg(reg),
        else => unreachable,
    };
}
