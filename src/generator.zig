const std = @import("std");
const Node = @import("nodes.zig").Node;
const GlobalTable = @import("analysis.zig").GlobalTable;
const FunctionTable = @import("analysis.zig").FunctionTable;
const GeneratorError = error{
    OutOfMemory,
    NoSpaceLeft,
    Overflow,
};

const Generator = struct {
    output: *std.ArrayList(u8),
    symbol_table: *GlobalTable,
    current_function_table: *FunctionTable,
    scratch_allocator: *ScratchRegisters,
    label_count: *u32,
};
pub fn generate_code(allocator: std.mem.Allocator, ast: *Node, global_table: *GlobalTable) ![]const u8 {
    var output = try (std.ArrayList(u8)).initCapacity(allocator, 100);
    defer output.deinit(allocator);
    try generate_prolog(allocator, &output);
    if (ast.* == .program) {
        var label_count: u32 = 0;
        for (ast.program.items) |node| {
            if (node.* == .function_def) {
                var scratch_allocator: ScratchRegisters = .{};
                var generator: Generator = .{
                    .output = &output,
                    .symbol_table = global_table,
                    .current_function_table = global_table.get_function(node.function_def.name) orelse unreachable,
                    .label_count = &label_count,
                    .scratch_allocator = &scratch_allocator,
                };
                try generate_function_prolog(allocator, &generator, node);
                try implict_zero_init(allocator, &generator);
                try generate_statements(allocator, &generator, node);
                try generate_function_epilog(allocator, &generator);
            }
        }
    }

    return output.toOwnedSlice(allocator);
}

const ScratchRegistersEnum = enum { rcx, rdx, r8, r9, r10, r11 };
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

// Convert 64-bit register name to its 16-bit lower version
fn lower_16_reg(reg: []const u8) []const u8 {
    const map = std.StaticStringMap([]const u8).initComptime(.{
        .{ "rax", "ax" },
        .{ "rcx", "cx" },
        .{ "rdx", "dx" },
        .{ "rbx", "bx" },
        .{ "rsi", "si" },
        .{ "rdi", "di" },
        .{ "rsp", "sp" },
        .{ "rbp", "bp" },
        .{ "r8", "r8w" },
        .{ "r9", "r9w" },
        .{ "r10", "r10w" },
        .{ "r11", "r11w" },
        .{ "r12", "r12w" },
        .{ "r13", "r13w" },
        .{ "r14", "r14w" },
        .{ "r15", "r15w" },
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
const ScratchRegisters = struct {
    in_use: [6]bool = .{false} ** 6,
    pub fn scratch_alloc(self: *ScratchRegisters) ?ScratchRegistersEnum {
        for (&self.in_use, 0..) |*reg, i| {
            if (!reg.*) {
                reg.* = true;
                return @enumFromInt(i);
            }
        }
        return null;
    }
    pub fn scratch_free_by_name(self: *ScratchRegisters, name: []const u8) void {
        const reg = std.meta.stringToEnum(ScratchRegistersEnum, name) orelse {
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

fn generate_prolog(allocator: std.mem.Allocator, output: *std.ArrayList(u8)) !void {
    const prolog =
        \\.intel_syntax noprefix
        \\.section .rodata
        \\fmt_int:
        \\      .string "%ld\n"
        \\.text
        \\.globl main
        \\.extern printf
    ;
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
        for (function.function_def.parameters.items[0..5], 0..) |param, i| {
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
        .decleration => try generate_decleration(allocator, generator, node),
        .assignment => try generate_assignment(allocator, generator, node),
        .print_statement => try generate_print(allocator, generator, node),
        .byte_out_statement => try generator_byte_out_statement(allocator, generator, node),
        .if_statement => try generate_if(allocator, generator, node),
        .while_statement => try generate_while(allocator, generator, node),
        .return_statement => try generate_return(allocator, generator, node),
        else => {},
    }
}

fn generate_function_call(allocator: std.mem.Allocator, generator: *Generator, function_call: *Node) !void {
    var buffer: [256]u8 = undefined;
    const registers = [_][]const u8{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };
    if (function_call.function_call.parameter_expressions.items.len > 6) {
        var i: usize = function_call.function_call.parameter_expressions.items.len - 1;
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub rsp, {d}\n", .{(i - 5) * 8}));
        while (i >= 6) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     push {s}\n", .{result_register}));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }

        while (i > 0) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
        if (i == 0) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     call {s}\n", .{function_call.function_call.name}));
        const stack_bytes = (function_call.function_call.parameter_expressions.items.len - 6) * 8;
        std.debug.print("stack_bytes: {d}\n", .{stack_bytes});
        const padding = if ((stack_bytes) % 16 == 8) 8 + stack_bytes else stack_bytes;
        std.debug.print("padding: {d}\n", .{padding});
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     add rsp, {d}\n", .{padding}));
        return;
    } else {
        var i: usize = function_call.function_call.parameter_expressions.items.len - 1;
        while (i > 0) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s} {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
        if (i == 0) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     call {s}\n", .{function_call.function_call.name}));
}

fn generate_decleration(allocator: std.mem.Allocator, generator: *Generator, node: *Node) !void {
    var buffer: [66]u8 = undefined;
    if (node.decleration.expression) |expression| {
        if (expression.* == .string_literal) {
            const variable = generator.current_function_table.get_parameter_or_variable(node.decleration.identifier) orelse return;
            const str = expression.string_literal[1 .. expression.string_literal.len - 1];
            for (str, 0..) |byte, i| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [rbp - {}], {}\n", .{ @abs(variable.offset) + i, byte }));
            }
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [rbp - {}], 0\n", .{@abs(variable.offset) + str.len}));
        } else {
            const result_register = try evaluate_expression(allocator, generator, expression);
            if (generator.current_function_table.get_parameter_or_variable(node.decleration.identifier)) |variable| {
                if (variable.offset > 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp + {}], {s}\n", .{ @abs(variable.offset), result_register }));
                }
                if (variable.offset < 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp - {}], {s}\n", .{ @abs(variable.offset), result_register }));
                }
            }
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
    }
}
fn generate_assignment(allocator: std.mem.Allocator, generator: *Generator, assignment: *Node) !void {
    var buffer: [66]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, assignment.assignment.expression);
    if (assignment.assignment.identifier.* == .identifier) {
        if (generator.current_function_table.get_parameter_or_variable(assignment.assignment.identifier.identifier)) |variable| {
            if (variable.offset > 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp + {}], {s}\n", .{ @abs(variable.offset), result_register }));
            }
            if (variable.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp - {}], {s}\n", .{ @abs(variable.offset), result_register }));
            }
        }
    }
    if (assignment.assignment.identifier.* == .array_index) {
        if (generator.current_function_table.get_parameter_or_variable(assignment.assignment.identifier.array_index.identifier)) |variable| {
            const index_reg = try evaluate_expression(allocator, generator, assignment.assignment.identifier.array_index.expression);

            if (generator.scratch_allocator.scratch_alloc()) |target| {
                if (variable.offset < 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp - {}]\n", .{ @tagName(target), @abs(variable.offset) }));
                } else {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp + {}]\n", .{ @tagName(target), @abs(variable.offset) }));
                }

                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, {s}\n", .{ @tagName(target), index_reg }));

                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov BYTE PTR [{s}], {s}\n", .{ @tagName(target), lower_8_reg(result_register) }));

                generator.scratch_allocator.scratch_free_by_name(index_reg);
            }
        }
    }
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn generate_print(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [256]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.print_statement);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov rsi, {s}\n     lea rdi, [rip + fmt_int]\n     xor rax, rax\n     call printf\n", .{result_register}));
    generator.scratch_allocator.scratch_free_by_name(result_register);
}
fn generator_byte_out_statement(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [1024]u8 = undefined;

    const result_register = try evaluate_expression(allocator, generator, statement.byte_out_statement);

    const byte_out_asm =
        \\     push rax
        \\     push rdi
        \\     push rsi
        \\     push rdx
        \\     sub rsp, 8
        \\     mov BYTE PTR [rsp], {s}
        \\     mov rax, 1      # syscall: write
        \\     mov rdi, 1      # fd: stdout
        \\     mov rsi, rsp    # buf: pointer to our byte
        \\     mov rdx, 1      # count: 1 byte
        \\     syscall
        \\     add rsp, 8
        \\     pop rdx
        \\     pop rsi
        \\     pop rdi
        \\     pop rax
    ;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, byte_out_asm, .{lower_8_reg(result_register)}));
    try generator.output.append(allocator, '\n');
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn generate_if(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    const false_lbl = generator.label_count.* + 1;
    generator.label_count.* += 1;
    const end_lbl = generator.label_count.* + 1;
    generator.label_count.* += 1;
    var buffer: [66]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.if_statement.expression);
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
            for (else_statement.else_statement.items) |node| {
                try generate_statement(allocator, generator, node);
            }
        }
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_IF{d}:\n", .{end_lbl}));
}

fn generate_while(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    generator.label_count.* += 1;
    const start = generator.label_count.*;
    var buffer: [66]u8 = undefined;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_WHILE{d}:\n", .{start}));
    const result_register = try evaluate_expression(allocator, generator, statement.while_statement.expression);
    generator.label_count.* += 1;
    const end = generator.label_count.*;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, 0\n     je .L_WHILE{d}\n", .{ result_register, end }));
    generator.scratch_allocator.scratch_free_by_name(result_register);
    for (statement.while_statement.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     jmp .L_WHILE{d}\n", .{start}));
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_WHILE{d}:\n", .{end}));
}

fn generate_return(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [1024]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.return_statement);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov rax, {s}\n     mov rsp, rbp\n     pop rbp\n     ret\n", .{result_register}));
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn evaluate_expression(allocator: std.mem.Allocator, generator: *Generator, expression: *Node) GeneratorError![]const u8 {
    var buffer: [1024]u8 = undefined;
    if (expression.* == .integer_literal) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {d}\n", .{ @tagName(target), expression.integer_literal }));
            return @tagName(target);
        }
    }
    if (expression.* == .character_literal) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {d}\n", .{ @tagName(target), expression.character_literal }));
            return @tagName(target);
        }
    }
    if (expression.* == .identifier) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            if (generator.current_function_table.get_parameter_or_variable(expression.identifier)) |variable| {
                if (variable.offset < 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, QWORD PTR [rbp - {}]\n", .{ @tagName(target), @abs(variable.offset) }));
                }
                if (variable.offset > 0) {
                    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, QWORD PTR [rbp + {}]\n", .{ @tagName(target), @abs(variable.offset) }));
                }
            }
            return @tagName(target);
        }
    }
    if (expression.* == .byte_in_statement) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            const byte_in_asm =
                \\     push rax
                \\     push rdi
                \\     push rsi
                \\     push rdx
                \\     sub rsp, 8
                \\     mov rax, 0      # syscall: read
                \\     mov rdi, 0      # fd: stdin
                \\     mov rsi, rsp    # buf: pointer to stack space
                \\     mov rdx, 1      # count: 1 byte
                \\     syscall
                \\     mov {s}, BYTE PTR [rsp]
                \\     add rsp, 8
                \\     pop rdx
                \\     pop rsi
                \\     pop rdi
                \\     pop rax
            ;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, byte_in_asm, .{lower_8_reg(@tagName(target))}));
            try generator.output.append(allocator, '\n');
            return @tagName(target);
        }
    }
    if (expression.* == .array_index) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            const array_var = generator.current_function_table.get_parameter_or_variable(expression.array_index.identifier) orelse unreachable;
            // Evaluate the index expression
            const index_reg = try evaluate_expression(allocator, generator, expression.array_index.expression);

            // Calculate address and load byte
            if (array_var.offset < 0) {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp - {}]\n", .{ @tagName(target), @abs(array_var.offset) }));
            } else {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     lea {s}, [rbp + {}]\n", .{ @tagName(target), @abs(array_var.offset) }));
            }

            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, {s}\n", .{ @tagName(target), index_reg }));

            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     movzx {s}, BYTE PTR [{s}]\n", .{ @tagName(target), @tagName(target) }));

            generator.scratch_allocator.scratch_free_by_name(index_reg);
            return @tagName(target);
        }
    }

    if (expression.* == .function_call) {
        try generate_function_call(allocator, generator, expression);
        return "rax";
    }
    // if (expression.* == .binary_op) {
    //     if (expression.binary_op.left.* == .integer_literal and expression.binary_op.right.* == .integer_literal) {
    //         // todo: compute at compile time :)
    //     }
    // }
    const left_target = try evaluate_expression(allocator, generator, expression.binary_op.left);
    const right_target = try evaluate_expression(allocator, generator, expression.binary_op.right);
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
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     sete al\n     movzx {s}, al\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Neq => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setne al\n     movzx {s}, al\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Leq => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setbe al\n     movzx {s}, al\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Geq => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setae al\n     movzx {s}, al\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Lt => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     setb al\n     movzx {s}, al\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Gt => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     seta al\n     movzx {s}, al\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .And => {
            const and_asm =
                \\     cmp {s}, 0
                \\     je .L{d}
                \\     cmp {s}, 0
                \\     je .L{d}
                \\     mov {s}, 1 
                \\     jmp .L{d}
                \\.L{d}
                \\     mov {s}, 0
                \\.L{d}
            ;
            const false_lbl = generator.label_count.* + 1;
            const end_lbl = generator.label_count.* + 1;
            generator.label_count.* += 2;
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
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Or => {
            const or_asm =
                \\     cmp {s}, 1
                \\     je .L{d}
                \\     cmp {s}, 1
                \\     je .L{d}
                \\     mov {s}, 0 
                \\     jmp .L{d}
                \\.L{d}
                \\     mov {s}, 1
                \\.L{d}
            ;
            const false_lbl = generator.label_count.* + 1;
            const end_lbl = generator.label_count.* + 1;
            generator.label_count.* += 2;
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
    }
    return left_target;
}

fn implict_zero_init(allocator: std.mem.Allocator, generator: *Generator) !void {
    var buffer: [66]u8 = undefined;
    if (generator.current_function_table.variables.count() > 0) {
        try generator.output.appendSlice(allocator, "     xor rax, rax\n");
        var var_iter = generator.current_function_table.variables.iterator();
        while (var_iter.next()) |variable| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov QWORD PTR [rbp - {}], rax\n", .{@abs(variable.value_ptr.offset)}));
        }
    }
}

pub fn saveAndCompileAssembly(allocator: std.mem.Allocator, code: []const u8, clean_up_flag: bool) !void {
    // Save assembly code to main.s
    const file = try std.fs.cwd().createFile("main.s", .{});
    defer file.close();
    try file.writeAll(code);
    const result = try std.process.Child.run(.{ .allocator = allocator, .argv = &[_][]const u8{ "gcc", "-no-pie", "main.s", "-o", "main" } });
    std.debug.print("{s}", .{result.stdout});
    std.debug.print("{s}",.{ result.stderr});

    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    if (clean_up_flag) std.fs.cwd().deleteFile("main.s") catch {};
}
