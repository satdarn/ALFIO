const std = @import("std");
const Node = @import("nodes.zig").Node;
const GlobalTable = @import("analysis_arm.zig").GlobalTable;
const FunctionTable = @import("analysis_arm.zig").FunctionTable;
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

// AArch64 has 31 general-purpose registers (x0-x30) and SP
// x0-x7: argument/result registers
// x8: indirect result location
// x9-x15: temporary registers (caller-saved)
// x16-x17: intra-procedure-call temporary (IP0, IP1)
// x18: platform register
// x19-x28: callee-saved registers
// x29: frame pointer (FP)
// x30: link register (LR)
// SP: stack pointer

const ScratchRegistersEnum = enum { x9, x10, x11, x12, x13, x14, x15 };

const ScratchRegisters = struct {
    in_use: [7]bool = .{false} ** 7,
    
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
            // Handle x0 (return register) specially
            if (std.mem.eql(u8, name, "x0")) return;
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

// Convert 64-bit register (x) to 32-bit (w)
fn lower_32_reg(reg: []const u8) []const u8 {
    if (reg.len >= 2 and reg[0] == 'x') {
        var buf: [4]u8 = undefined;
        buf[0] = 'w';
        @memcpy(buf[1..reg.len], reg[1..]);
        return buf[0..reg.len];
    }
    return reg;
}

// For byte operations, we use the w register with appropriate masking
fn lower_8_reg(reg: []const u8) []const u8 {
    return lower_32_reg(reg);
}

fn generate_prolog(allocator: std.mem.Allocator, output: *std.ArrayList(u8)) !void {
    const prolog =
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
    const registers = [_][]const u8{ "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7" };
    
    // AArch64 requires 16-byte stack alignment
    const aligned_frame_size = (generator.current_function_table.frame_size + 15) & ~@as(i64, 15);
    
    const prolog =
        \\{s}:
        \\     stp x29, x30, [sp, #-16]!
        \\     mov x29, sp
        \\     sub sp, sp, #{d}
    ;

    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, prolog, .{ function.function_def.name, aligned_frame_size }));
    try generator.output.append(allocator, '\n');
    
    // Store parameters to stack
    for (function.function_def.parameters.items, 0..) |param, i| {
        if (i >= 8) break; // Only first 8 params in registers
        if (generator.current_function_table.get_parameter_or_variable(param.function_parameter.name)) |target| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     str {s}, [x29, #-{d}]\n", .{ registers[i], @abs(target.offset) }));
        }
    }
}

fn generate_function_epilog(allocator: std.mem.Allocator, generator: *Generator) !void {
    const epilog =
        \\     mov x0, #0
        \\     mov sp, x29
        \\     ldp x29, x30, [sp], #16
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
    const registers = [_][]const u8{ "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7" };
    
    const param_count = function_call.function_call.parameter_expressions.items.len;
    
    // Handle parameters beyond x0-x7 (push to stack in reverse order)
    if (param_count > 8) {
        var i: usize = param_count - 1;
        while (i >= 8) : (i -= 1) {
            const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     str {s}, [sp, #-16]!\n", .{result_register}));
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
    }
    
    // Load first 8 parameters into registers
    const max_reg_params = @min(param_count, 8);
    var i: usize = 0;
    while (i < max_reg_params) : (i += 1) {
        const result_register = try evaluate_expression(allocator, generator, function_call.function_call.parameter_expressions.items[i]);
        if (!std.mem.eql(u8, result_register, registers[i])) {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, {s}\n", .{ registers[i], result_register }));
        }
        generator.scratch_allocator.scratch_free_by_name(result_register);
    }
    
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     bl {s}\n", .{function_call.function_call.name}));
    
    // Clean up stack if we had extra parameters
    if (param_count > 8) {
        const stack_bytes = (param_count - 8) * 16;
        try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     add sp, sp, #{d}\n", .{stack_bytes}));
    }
}

fn generate_decleration(allocator: std.mem.Allocator, generator: *Generator, node: *Node) !void {
    var buffer: [128]u8 = undefined;
    if (node.decleration.expression) |expression| {
        if (expression.* == .string_literal) {
            const variable = generator.current_function_table.get_parameter_or_variable(node.decleration.identifier) orelse return;
            const str = expression.string_literal[1 .. expression.string_literal.len - 1];
            for (str, 0..) |byte, i| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov w8, #{d}\n", .{byte}));
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     strb w8, [x29, #-{d}]\n", .{@abs(variable.offset) + i}));
            }
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     strb wzr, [x29, #-{d}]\n", .{@abs(variable.offset) + str.len}));
        } else {
            const result_register = try evaluate_expression(allocator, generator, expression);
            if (generator.current_function_table.get_parameter_or_variable(node.decleration.identifier)) |variable| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     str {s}, [x29, #-{d}]\n", .{ result_register, @abs(variable.offset) }));
            }
            generator.scratch_allocator.scratch_free_by_name(result_register);
        }
    }
}

fn generate_assignment(allocator: std.mem.Allocator, generator: *Generator, assignment: *Node) !void {
    var buffer: [128]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, assignment.assignment.expression);
    
    if (assignment.assignment.identifier.* == .identifier) {
        if (generator.current_function_table.get_parameter_or_variable(assignment.assignment.identifier.identifier)) |variable| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     str {s}, [x29, #-{d}]\n", .{ result_register, @abs(variable.offset) }));
        }
    }
    
    if (assignment.assignment.identifier.* == .array_index) {
        if (generator.current_function_table.get_parameter_or_variable(assignment.assignment.identifier.array_index.identifier)) |variable| {
            const index_reg = try evaluate_expression(allocator, generator, assignment.assignment.identifier.array_index.expression);

            if (generator.scratch_allocator.scratch_alloc()) |target| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, x29, #{d}\n", .{ @tagName(target), @abs(variable.offset) }));
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, {s}, {s}\n", .{ @tagName(target), @tagName(target), index_reg }));
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     strb {s}, [{s}]\n", .{ lower_32_reg(result_register), @tagName(target) }));
                generator.scratch_allocator.scratch_free_by_name(index_reg);
                generator.scratch_allocator.scratch_free(target);
            }
        }
    }
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn generate_print(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [256]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.print_statement);
    
    // Move result to x1 (second argument for printf)
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov x1, {s}\n", .{result_register}));
    // Load format string address to x0
    try generator.output.appendSlice(allocator, "     adrp x0, fmt_int\n     add x0, x0, :lo12:fmt_int\n");
    try generator.output.appendSlice(allocator, "     bl printf\n");
    
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn generator_byte_out_statement(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [1024]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.byte_out_statement);

    // Linux syscall: write(1, buf, 1)
    // x8 = syscall number (64 for write)
    // x0 = fd (1 for stdout)
    // x1 = buf
    // x2 = count
    const byte_out_asm =
        \\     strb {s}, [sp, #-16]!
        \\     mov x0, #1
        \\     mov x1, sp
        \\     mov x2, #1
        \\     mov x8, #64
        \\     svc #0
        \\     add sp, sp, #16
    ;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, byte_out_asm, .{lower_32_reg(result_register)}));
    try generator.output.append(allocator, '\n');
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn generate_if(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    const false_lbl = generator.label_count.* + 1;
    generator.label_count.* += 1;
    const end_lbl = generator.label_count.* + 1;
    generator.label_count.* += 1;
    var buffer: [128]u8 = undefined;
    
    const result_register = try evaluate_expression(allocator, generator, statement.if_statement.expression);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, #0\n     b.eq .L_IF{d}\n", .{ result_register, false_lbl }));
    generator.scratch_allocator.scratch_free_by_name(result_register);
    
    for (statement.if_statement.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
    
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     b .L_IF{d}\n", .{end_lbl}));
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
    var buffer: [128]u8 = undefined;
    
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_WHILE{d}:\n", .{start}));
    const result_register = try evaluate_expression(allocator, generator, statement.while_statement.expression);
    
    generator.label_count.* += 1;
    const end = generator.label_count.*;
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, #0\n     b.eq .L_WHILE{d}\n", .{ result_register, end }));
    generator.scratch_allocator.scratch_free_by_name(result_register);
    
    for (statement.while_statement.statement_list.items) |node| {
        try generate_statement(allocator, generator, node);
    }
    
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     b .L_WHILE{d}\n", .{start}));
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, ".L_WHILE{d}:\n", .{end}));
}

fn generate_return(allocator: std.mem.Allocator, generator: *Generator, statement: *Node) !void {
    var buffer: [256]u8 = undefined;
    const result_register = try evaluate_expression(allocator, generator, statement.return_statement);
    try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov x0, {s}\n     mov sp, x29\n     ldp x29, x30, [sp], #16\n     ret\n", .{result_register}));
    generator.scratch_allocator.scratch_free_by_name(result_register);
}

fn evaluate_expression(allocator: std.mem.Allocator, generator: *Generator, expression: *Node) GeneratorError![]const u8 {
    var buffer: [1024]u8 = undefined;
    
    if (expression.* == .integer_literal) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, #{d}\n", .{ @tagName(target), expression.integer_literal }));
            return @tagName(target);
        }
    }
    
    if (expression.* == .character_literal) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mov {s}, #{d}\n", .{ @tagName(target), expression.character_literal }));
            return @tagName(target);
        }
    }
    
    if (expression.* == .identifier) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            if (generator.current_function_table.get_parameter_or_variable(expression.identifier)) |variable| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     ldr {s}, [x29, #-{d}]\n", .{ @tagName(target), @abs(variable.offset) }));
            }
            return @tagName(target);
        }
    }
    
    if (expression.* == .byte_in_statement) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            // Linux syscall: read(0, buf, 1)
            const byte_in_asm =
                \\     sub sp, sp, #16
                \\     mov x0, #0
                \\     mov x1, sp
                \\     mov x2, #1
                \\     mov x8, #63
                \\     svc #0
                \\     ldrb {s}, [sp]
                \\     add sp, sp, #16
            ;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, byte_in_asm, .{lower_32_reg(@tagName(target))}));
            try generator.output.append(allocator, '\n');
            return @tagName(target);
        }
    }
    
    if (expression.* == .array_index) {
        if (generator.scratch_allocator.scratch_alloc()) |target| {
            const array_var = generator.current_function_table.get_parameter_or_variable(expression.array_index.identifier) orelse unreachable;
            const index_reg = try evaluate_expression(allocator, generator, expression.array_index.expression);

            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, x29, #{d}\n", .{ @tagName(target), @abs(array_var.offset) }));
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, {s}, {s}\n", .{ @tagName(target), @tagName(target), index_reg }));
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     ldrb {s}, [{s}]\n", .{ lower_32_reg(@tagName(target)), @tagName(target) }));

            generator.scratch_allocator.scratch_free_by_name(index_reg);
            return @tagName(target);
        }
    }

    if (expression.* == .function_call) {
        try generate_function_call(allocator, generator, expression);
        return "x0";
    }
    
    const left_target = try evaluate_expression(allocator, generator, expression.binary_op.left);
    const right_target = try evaluate_expression(allocator, generator, expression.binary_op.right);
    
    switch (expression.binary_op.op) {
        .Add => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     add {s}, {s}, {s}\n", .{ left_target, left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Sub => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sub {s}, {s}, {s}\n", .{ left_target, left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Mult => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     mul {s}, {s}, {s}\n", .{ left_target, left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Div => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sdiv {s}, {s}, {s}\n", .{ left_target, left_target, right_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Mod => {
            // ARM doesn't have a modulo instruction, compute: a % b = a - (a/b)*b
            if (generator.scratch_allocator.scratch_alloc()) |temp| {
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     sdiv {s}, {s}, {s}\n", .{ @tagName(temp), left_target, right_target }));
                try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     msub {s}, {s}, {s}, {s}\n", .{ left_target, @tagName(temp), right_target, left_target }));
                generator.scratch_allocator.scratch_free(temp);
            }
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Eql => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     cset {s}, eq\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Neq => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     cset {s}, ne\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Leq => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     cset {s}, le\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Geq => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     cset {s}, ge\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Lt => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     cset {s}, lt\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .Gt => {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     cmp {s}, {s}\n     cset {s}, gt\n", .{ left_target, right_target, left_target }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
        .And => {
            const false_lbl = generator.label_count.* + 1;
            const end_lbl = generator.label_count.* + 2;
            generator.label_count.* += 2;
            
            const and_asm =
                \\     cmp {s}, #0
                \\     b.eq .L{d}
                \\     cmp {s}, #0
                \\     b.eq .L{d}
                \\     mov {s}, #1
                \\     b .L{d}
                \\.L{d}:
                \\     mov {s}, #0
                \\.L{d}:
            ;
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
            const true_lbl = generator.label_count.* + 1;
            const end_lbl = generator.label_count.* + 2;
            generator.label_count.* += 2;
            
            const or_asm =
                \\     cmp {s}, #0
                \\     b.ne .L{d}
                \\     cmp {s}, #0
                \\     b.ne .L{d}
                \\     mov {s}, #0
                \\     b .L{d}
                \\.L{d}:
                \\     mov {s}, #1
                \\.L{d}:
            ;
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, or_asm, .{
                left_target,
                true_lbl,
                right_target,
                true_lbl,
                left_target,
                end_lbl,
                true_lbl,
                left_target,
                end_lbl,
            }));
            generator.scratch_allocator.scratch_free_by_name(right_target);
        },
    }
    return left_target;
}

fn implict_zero_init(allocator: std.mem.Allocator, generator: *Generator) !void {
    var buffer: [128]u8 = undefined;
    if (generator.current_function_table.variables.count() > 0) {
        var var_iter = generator.current_function_table.variables.iterator();
        while (var_iter.next()) |variable| {
            try generator.output.appendSlice(allocator, try std.fmt.bufPrint(&buffer, "     str xzr, [x29, #-{d}]\n", .{@abs(variable.value_ptr.offset)}));
        }
    }
}
pub fn saveAndCompileAssembly(allocator: std.mem.Allocator, code: []const u8, output_name: []const u8, clean_up_flag: bool) !void {
    // Save assembly code to main.s
    const file = try std.fs.cwd().createFile("main.s", .{});
    defer file.close();
    try file.writeAll(code);
    const result = try std.process.Child.run(.{ 
        .allocator = allocator, 
        .argv = &[_][]const u8{ "gcc", "-no-pie", "main.s", "-o", output_name } 
    });
    std.debug.print("{s}", .{result.stdout});
    std.debug.print("{s}",.{ result.stderr});

    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    if (clean_up_flag) std.fs.cwd().deleteFile("main.s") catch {};
}
