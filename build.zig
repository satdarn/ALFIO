const std = @import("std");

pub fn build(b: *std.Build) void {
    // x86_64 target
    const x86_64_target = b.resolveTargetQuery(.{
        .cpu_arch = .x86_64,
        .os_tag = .linux,
        .abi = .gnu,
    });
    
    // ARM64 target
    const aarch64_target = b.resolveTargetQuery(.{
        .cpu_arch = .aarch64,
        .os_tag = .linux,
        .abi = .gnu,
    });

    // Build x86_64 binary
    const x86_exe = b.addExecutable(.{
        .name = "alc-x86_64",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = x86_64_target,
        }),
    });
    b.installArtifact(x86_exe);

    // Build ARM64 binary
    const arm64_exe = b.addExecutable(.{
        .name = "alc-aarch64",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = aarch64_target,
        }),
    });
    b.installArtifact(arm64_exe);

    // Create build steps
    const x86_step = b.step("x86_64", "Build for x86_64 Linux");
    x86_step.dependOn(&x86_exe.step);

    const arm64_step = b.step("arm64", "Build for ARM64 Linux");
    arm64_step.dependOn(&arm64_exe.step);

    const all_step = b.step("all", "Build for all architectures");
    all_step.dependOn(&x86_exe.step);
    all_step.dependOn(&arm64_exe.step);
}
