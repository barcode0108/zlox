const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const trace_execution =
        b.option(bool, "trace_execution", "trace execution") orelse false;
    const print_code =
        b.option(bool, "print_code", "print code") orelse true;
    const testing_allcator = false;

    const debug_flag = b.addOptions();
    debug_flag.addOption(bool, "trace_execution", if (optimize == .Debug) trace_execution else false);
    debug_flag.addOption(bool, "print_code", if (optimize == .Debug) print_code else false);
    debug_flag.addOption(bool, "testing_allocator", if (optimize == .Debug) testing_allcator else false);

    exe_mod.addOptions("debug", debug_flag);

    const exe = b.addExecutable(.{
        .name = "zlox",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
