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
        b.option(bool, "trace-execution", "Trace instruction execution and stack") orelse false;
    const print_code =
        b.option(bool, "print-code", "Print compiled code(default true on debug build)") orelse true;
    const stress_gc =
        b.option(bool, "stress-gc", "Stress GC. Runs every allocation") orelse false;

    const log_gc =
        b.option(bool, "log-gc", "Log GC information(default true on debug build)") orelse true;

    const testing_allcator = false;

    const debug_flag = b.addOptions();
    debug_flag.addOption(bool, "trace_execution", if (optimize == .Debug) trace_execution else false);
    debug_flag.addOption(bool, "print_code", if (optimize == .Debug) print_code else false);
    debug_flag.addOption(bool, "stress_gc", if (optimize == .Debug) stress_gc else false);
    debug_flag.addOption(bool, "log_gc", if (optimize == .Debug) log_gc else false);
    debug_flag.addOption(bool, "testing_allocator", if (optimize == .Debug) testing_allcator else false);

    exe_mod.addOptions("debug", debug_flag);

    const exe = b.addExecutable(.{
        .name = "zlox",
        .root_module = exe_mod,
    });

    switch (optimize) {
        .ReleaseFast, .ReleaseSmall => exe.linkLibC(),
        else => {},
    }

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
