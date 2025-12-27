const std = @import("std");
const InputStream = @import("stream.zig").InputStream;
const tokenize = @import("lexer.zig").tokenize;
const parse = @import("parser.zig").parse;
const analyize = @import("analysis.zig").analyize;
const generate_code = @import("generator.zig").generate_code;
const saveAndCompileAssembly = @import("generator.zig").saveAndCompileAssembly;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();
    
    const file_path = args.next() orelse {
        std.debug.print("Usage: program <file_path>\n", .{});
        return error.NoFilePath;
    };

    var stream = try InputStream.fromFile(allocator, file_path);
    defer stream.deinit(allocator);

    var token_list = try tokenize(allocator, &stream);
    defer token_list.deinit(allocator);
    token_list.head.?.print_list();

    const ast = try parse(allocator, &token_list);
    defer ast.destroy(allocator);

    ast.print(0);

    const symbol_table = try analyize(allocator, ast, true);
    defer symbol_table.deinit(allocator);
    symbol_table.calculate_offsets();

    const assembly = try generate_code(allocator, ast, symbol_table);
    defer allocator.free(assembly);

    try saveAndCompileAssembly(allocator, assembly, false);
    // std.debug.print("{s}", .{assembly});
}
