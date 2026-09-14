// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::BoardKind;
use crate::plan::{BuildSpec, Label, Plan};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

pub type Manifest = BTreeMap<Label, PathBuf>;

pub fn prebuild(plan: &Plan, out_dir: &Path, libtock_c: &Path) -> Manifest {
    std::fs::create_dir_all(out_dir).expect("failed to create output directory");
    let mut manifest = Manifest::new();

    for (label, spec) in &plan.artifacts {
        let (built, ext) = match spec {
            BuildSpec::Kernel { board } => (build_kernel(*board), "bin"),
            BuildSpec::App { name } => (build_app(name, libtock_c), "tab"),
        };
        let dest = out_dir.join(format!("{label}.{ext}"));
        std::fs::copy(&built, &dest)
            .unwrap_or_else(|e| panic!("failed to copy {built:?} to {dest:?}: {e}"));
        manifest.insert(label.clone(), dest);
    }

    manifest
}

fn board_dir(board: BoardKind) -> &'static str {
    match board {
        BoardKind::QemuVirt => "boards/configurations/qemu_rv64_virt/qemu_rv64_virt-test-ci",
        BoardKind::Nrf52840Dk => "boards/nordic/nrf52840dk",
        BoardKind::NucleoF429zi => "boards/nucleo_f429zi",
    }
}

// TODO: derive triple/platform from the board's own Makefile instead of hardcoding them here.
fn board_kernel_bin(board: BoardKind) -> PathBuf {
    let (triple, platform) = match board {
        BoardKind::QemuVirt => ("riscv64imac-unknown-none-elf", "qemu_rv64_virt-test-ci"),
        BoardKind::Nrf52840Dk => ("thumbv7em-none-eabi", "nrf52840dk"),
        BoardKind::NucleoF429zi => ("thumbv7em-none-eabi", "nucleo_f429zi"),
    };
    Path::new("target")
        .join(triple)
        .join("release")
        .join(format!("{platform}.bin"))
}

fn build_kernel(board: BoardKind) -> PathBuf {
    let status = Command::new("make")
        .current_dir(board_dir(board))
        .status()
        .expect("failed to run make");
    assert!(status.success(), "kernel build failed for {board:?}");
    board_kernel_bin(board)
}

fn build_app(name: &str, libtock_c: &Path) -> PathBuf {
    let dir = libtock_c.join("examples").join(name);
    let status = Command::new("make")
        .current_dir(&dir)
        .status()
        .expect("failed to run make");
    assert!(status.success(), "app build failed for {name}");
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
