const std = @import("std");
const builtin = @import("builtin");
const InputStream = @import("stream.zig").InputStream;
const tokenize = @import("lexer.zig").tokenize;
const parse = @import("parser.zig").parse;
const analyize_x86 = @import("analysis.zig").analyize;
const generate_code_x86 = @import("generator.zig").generate_code;
const saveAndCompileAssembly_x86 = @import("generator.zig").saveAndCompileAssembly;
const analyize_arm = @import("analysis_arm.zig").analyize;
const generate_code_arm = @import("generator_arm.zig").generate_code;
const saveAndCompileAssembly_arm = @import("generator_arm.zig").saveAndCompileAssembly;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();
    
    const file_path = args.next() orelse {
        std.debug.print("Usage: program <file_path> [options]\n", .{});
        std.debug.print("Options:\n", .{});
        std.debug.print("  --arch <x86_64|aarch64>  Target architecture (default: native)\n", .{});
        std.debug.print("  -o <output>              Output binary name (default: main)\n", .{});
        return error.NoFilePath;
    };

    // Parse optional arguments
    var target_arch: ?std.Target.Cpu.Arch = null;
    var output_name: []const u8 = "main";
    
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--arch")) {
            if (args.next()) |arch_name| {
                if (std.mem.eql(u8, arch_name, "x86_64")) {
                    target_arch = .x86_64;
                } else if (std.mem.eql(u8, arch_name, "aarch64")) {
                    target_arch = .aarch64;
                } else {
                    std.debug.print("Unknown architecture: {s}\n", .{arch_name});
                    std.debug.print("Supported: x86_64, aarch64\n", .{});
                    return error.UnknownArchitecture;
                }
            } else {
                std.debug.print("--arch requires an argument\n", .{});
                return error.MissingArchArgument;
            }
        } else if (std.mem.eql(u8, arg, "-o")) {
            if (args.next()) |name| {
                output_name = name;
            } else {
                std.debug.print("-o requires an argument\n", .{});
                return error.MissingOutputArgument;
            }
        } else {
            std.debug.print("Unknown option: {s}\n", .{arg});
            return error.UnknownOption;
        }
    }

    // Use provided architecture or detect current
    const arch = target_arch orelse builtin.cpu.arch;
    
    std.debug.print("Compiling for architecture: {s}\n", .{@tagName(arch)});
    std.debug.print("Output binary: {s}\n", .{output_name});

    var stream = try InputStream.fromFile(allocator, file_path);
    defer stream.deinit(allocator);

    var token_list = try tokenize(allocator, &stream);
    defer token_list.deinit(allocator);
    token_list.head.?.print_list();

    const ast = try parse(allocator, &token_list);
    defer ast.destroy(allocator);

    ast.print(0);

    // Select backend based on architecture
    switch (arch) {
        .x86_64 => {
            std.debug.print("Using x86_64 backend\n", .{});
            const symbol_table = try analyize_x86(allocator, ast, true);
            defer symbol_table.deinit(allocator);
            symbol_table.calculate_offsets();

            const assembly = try generate_code_x86(allocator, ast, symbol_table);
            defer allocator.free(assembly);

            try saveAndCompileAssembly_x86(allocator, assembly, output_name, false);
            std.debug.print("Compilation successful! Binary: ./{s}\n", .{output_name});
        },
        .aarch64, .aarch64_be => {
            std.debug.print("Using AArch64 backend\n", .{});
            const symbol_table = try analyize_arm(allocator, ast, true);
            defer symbol_table.deinit(allocator);
            symbol_table.calculate_offsets();

            const assembly = try generate_code_arm(allocator, ast, symbol_table);
            defer allocator.free(assembly);

            try saveAndCompileAssembly_arm(allocator, assembly, output_name, false);
            std.debug.print("Compilation successful! Binary: ./{s}\n", .{output_name});
        },
        else => {
            std.debug.print("Unsupported architecture: {s}\n", .{@tagName(arch)});
            std.debug.print("Supported architectures: x86_64, aarch64\n", .{});
            return error.UnsupportedArchitecture;
        },
    }
}
