// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::BoardKind;
use crate::plan::{BuildSpec, Label, Plan};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

pub type Manifest = BTreeMap<Label, PathBuf>;

/// The `uname -m` of each Treadmill host architecture, and the target the
/// harness is built for to run there. Linked statically with `rust-lld` (see
/// `.cargo/config.toml`), so it needs no system libraries or cross toolchain.
pub const HOST_TARGETS: &[(&str, &str)] = &[
    ("x86_64", "x86_64-unknown-linux-musl"),
    ("aarch64", "aarch64-unknown-linux-musl"),
];

pub fn prebuild(plan: &Plan, out_dir: &Path, libtock_c: &Path) -> Manifest {
    std::fs::create_dir_all(out_dir).expect("failed to create output directory");
    let mut manifest = Manifest::new();

    for (label, spec) in &plan.artifacts {
        let (built, ext) = match spec {
            BuildSpec::TockKernel { board } => (build_kernel(*board), "bin"),
            BuildSpec::LibtockCApp { name, tock_targets } => {
                (build_app(name, tock_targets, libtock_c), "tab")
            }
        };
        // Manifest paths are relative to the manifest's own directory, so the
        // output directory can be moved or archived and unpacked elsewhere.
        let name = PathBuf::from(format!("{label}.{ext}"));
        let dest = out_dir.join(&name);
        std::fs::copy(&built, &dest)
            .unwrap_or_else(|e| panic!("failed to copy {built:?} to {dest:?}: {e}"));
        manifest.insert(label.clone(), name);
    }

    for (arch, target) in HOST_TARGETS {
        let built = build_self(target);
        let dest = out_dir.join(format!("tock-harness-{arch}"));
        std::fs::copy(&built, &dest)
            .unwrap_or_else(|e| panic!("failed to copy {built:?} to {dest:?}: {e}"));
    }

    manifest
}

/// Build this harness for `target`, returning the path of the executable.
fn build_self(target: &str) -> PathBuf {
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
    let output = Command::new(cargo)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["build", "--release", "-p", env!("CARGO_PKG_NAME")])
        .args([
            "--target",
            target,
            "--message-format=json-render-diagnostics",
        ])
        .stderr(std::process::Stdio::inherit())
        .output()
        .expect("failed to run cargo");
    assert!(output.status.success(), "harness build failed for {target}");
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .find_map(|msg| Some(PathBuf::from(msg.get("executable")?.as_str()?)))
        .unwrap_or_else(|| panic!("cargo reported no executable for {target}"))
}

/// Pack `dir` into a tar archive at `archive`.
pub fn archive(dir: &Path, archive: &Path) {
    let status = Command::new("tar")
        .arg("-C")
        .arg(dir)
        .arg("-cf")
        .arg(archive)
        .arg(".")
        .status()
        .expect("failed to run tar");
    assert!(status.success(), "failed to create archive");
}

fn board_dir(board: BoardKind) -> &'static str {
    match board {
        BoardKind::QemuVirt => "boards/configurations/qemu_rv64_virt/qemu_rv64_virt-test-ci",
        BoardKind::Nrf52840Dk => "boards/nordic/nrf52840dk",
        BoardKind::NucleoF429zi => "boards/nucleo_f429zi",
        BoardKind::RaspberryPiPico => "boards/raspberry_pi_pico",
        BoardKind::RaspberryPiPico2 => "boards/raspberry_pi_pico_2",
        BoardKind::Esp32C3DevkitM1 => "boards/esp32-c3-devkitM-1",
    }
}

// TODO: derive triple/platform from the board's own Makefile instead of hardcoding them here.
fn board_kernel_bin(board: BoardKind) -> PathBuf {
    let (triple, platform) = match board {
        BoardKind::QemuVirt => ("riscv64imac-unknown-none-elf", "qemu_rv64_virt-test-ci"),
        BoardKind::Nrf52840Dk => ("thumbv7em-none-eabi", "nrf52840dk"),
        BoardKind::NucleoF429zi => ("thumbv7em-none-eabi", "nucleo_f429zi"),
        BoardKind::RaspberryPiPico => ("thumbv6m-none-eabi", "raspberry_pi_pico"),
        BoardKind::RaspberryPiPico2 => ("thumbv8m.main-none-eabi", "raspberry_pi_pico_2"),
        BoardKind::Esp32C3DevkitM1 => ("riscv32imc-unknown-none-elf", "esp32-c3-board"),
    };
    Path::new("target")
        .join(triple)
        .join("release")
        .join(format!("{platform}.bin"))
}

/// Run `make` with `args` in `dir`, panicking with `what` failed otherwise.
fn make(dir: &Path, args: &[&str], what: &str) {
    // A missing directory would otherwise surface as `make` not being found.
    assert!(dir.is_dir(), "{what}: no directory {}", dir.display());
    let status = Command::new("make")
        .current_dir(dir)
        .args(args)
        .status()
        .unwrap_or_else(|e| panic!("{what}: failed to run make: {e}"));
    assert!(status.success(), "{what} failed");
}

fn build_kernel(board: BoardKind) -> PathBuf {
    make(
        Path::new(board_dir(board)),
        &[],
        &format!("kernel build for {board}"),
    );
    board_kernel_bin(board)
}

fn build_app(name: &str, tock_targets: &[String], libtock_c: &Path) -> PathBuf {
    let dir = libtock_c.join("examples").join(name);
    // Remove the app's own build first, so that the TAB contains exactly
    // `tock_targets`, not those of an earlier build of the same app. Not
    // `make clean`: that also removes the libraries' (e.g. libtock's) builds,
    // which the next app would then rebuild.
    match std::fs::remove_dir_all(dir.join("build")) {
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => {
            panic!("failed to remove {}/build: {e}", dir.display())
        }
        _ => {}
    }
    let targets = format!("TOCK_TARGETS={}", tock_targets.join(" "));
    make(&dir, &["-j", &targets], &format!("app build for {name}"));
    let tabs: Vec<PathBuf> = std::fs::read_dir(dir.join("build"))
        .expect("failed to read app build directory")
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "tab"))
        .collect();
    assert_eq!(
        tabs.len(),
        1,
        "expected exactly one TAB for {name}, found {tabs:?}"
    );
    tabs.into_iter().next().unwrap()
}
