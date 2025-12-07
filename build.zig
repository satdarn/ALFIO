const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        // NAME THE EXE
        .name = "asc",
        .root_module = b.createModule(.{
            // ADD MAIN FILE
            .root_source_file = b.path("src/main.zig"),
            .target = b.graph.host,
        }),
    });
    b.installArtifact(exe);
    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
}
