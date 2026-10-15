// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::boards::Board;
use crate::boards::helpers::command;
use crate::plan::{BuildSpec, Plan};
use anyhow::{Context, Result, ensure};
use fs_err as fs;
use log::info;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// Architecture of each Treadmill host (as reported by `uname -m`), along with
/// the target of the harness built to run there.
///
/// The harness is linked statically with `rust-lld` (see
/// `.cargo/config.toml`), such that it needs no system libraries or cross
/// toolchain.
pub const HOST_TARGETS: &[(&str, &str)] = &[
    ("x86_64", "x86_64-unknown-linux-musl"),
    ("aarch64", "aarch64-unknown-linux-musl"),
];

#[derive(clap::Args)]
pub struct BuildArgs {
    /// Tock repository to build the kernels in
    #[arg(long)]
    pub tock: PathBuf,
    #[arg(long)]
    pub libtock_c: PathBuf,
    /// Directory to write the artifacts to
    #[arg(long)]
    pub artifacts_dir: PathBuf,
}

#[derive(clap::Args)]
pub struct Args {
    #[arg(long)]
    plan: PathBuf,
    #[command(flatten)]
    build: BuildArgs,
    /// Tar archive to pack the output directory into
    #[arg(long)]
    archive: Option<PathBuf>,
}

impl Args {
    /// Build the artifacts of the plan at `plan`, and the harness for each of
    /// the [`HOST_TARGETS`].
    pub fn run(self) -> Result<()> {
        build(&Plan::load(&self.plan)?, &self.build)?;
        for (arch, target) in HOST_TARGETS {
            let built = build_harness(target)?;
            fs::copy(
                &built,
                self.build
                    .artifacts_dir
                    .join(format!("tock-integration-test-harness-{arch}")),
            )?;
        }
        if let Some(archive) = &self.archive {
            pack(&self.build.artifacts_dir, archive)?;
            info!("wrote {}", archive.display());
        }
        Ok(())
    }
}

/// Build the harness for `target`, and return the path of the binary.
fn build_harness(target: &str) -> Result<PathBuf> {
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
    let output = Command::new(cargo)
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["build", "--release", "-p", env!("CARGO_PKG_NAME")])
        .args([
            "--target",
            target,
            "--message-format=json-render-diagnostics",
        ])
        .stderr(Stdio::inherit())
        .output()
        .context("failed to run cargo")?;
    ensure!(
        output.status.success(),
        "harness build failed for {target} ({})",
        output.status
    );
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str::<serde_json::Value>(line).ok())
        .find_map(|msg| Some(PathBuf::from(msg.get("executable")?.as_str()?)))
        .with_context(|| format!("cargo reported no executable for {target}"))
}

/// Pack the contents of `dir` into the tar archive `archive`.
pub fn pack(dir: &Path, archive: &Path) -> Result<()> {
    let status = Command::new("tar")
        .arg("-C")
        .arg(dir)
        .arg("-cf")
        .arg(archive)
        .arg(".")
        .status()
        .context("failed to run tar")?;
    ensure!(
        status.success(),
        "failed to create {} ({status})",
        archive.display()
    );
    Ok(())
}

/// Build the artifacts of `plan` into the artifacts directory.
///
/// The file name of each artifact is given by [`BuildSpec::file_name`].
pub fn build(plan: &Plan, args: &BuildArgs) -> Result<()> {
    let artifacts_dir = &args.artifacts_dir;
    fs::create_dir_all(artifacts_dir)?;

    for (label, spec) in &plan.artifacts {
        let what = match spec {
            BuildSpec::TockKernel { .. } => "kernel".to_string(),
            BuildSpec::LibtockCApp { name, .. } => format!("app {name}"),
        };
        let what = format!("{what} for {}", boards_using(plan, label).join(", "));
        info!("building {what}");
        let built = match spec {
            BuildSpec::TockKernel { board } => build_kernel(board, &args.tock),
            BuildSpec::LibtockCApp { name, tock_targets } => {
                build_app(name, tock_targets, &args.libtock_c)
            }
        };
        let built = built.with_context(|| format!("failed to build {what}"))?;
        fs::copy(&built, artifacts_dir.join(spec.file_name(label)))?;
    }
    Ok(())
}

/// Return the names of the boards whose tests in `plan` use the artifact with
/// `label`.
fn boards_using(plan: &Plan, label: &str) -> Vec<&'static str> {
    let mut boards: Vec<&str> = (plan.tests.iter())
        .filter(|t| t.kernel == label || t.apps.iter().any(|a| a == label))
        .map(|t| t.board.name)
        .collect();
    boards.sort();
    boards.dedup();
    boards
}

/// Run `make` in `dir` with `args`, and label its log lines with `label`.
fn make(dir: &Path, args: &[&str], label: &str) -> Result<()> {
    // Without this check, a missing `dir` is reported as `make` not being
    // found:
    ensure!(dir.is_dir(), "no directory {}", dir.display());
    command::run_labeled(Command::new("make").current_dir(dir).args(args), label)
}

fn build_kernel(board: &Board, tock: &Path) -> Result<PathBuf> {
    make(&tock.join(board.dir), &[], &format!("kernel:{board}"))?;
    Ok(tock.join(board.kernel))
}

fn build_app(name: &str, tock_targets: &[String], libtock_c: &Path) -> Result<PathBuf> {
    let dir = libtock_c.join("examples").join(name);
    // Remove the app's previous build directory, such that the TAB only
    // contains `tock_targets`.
    //
    // We avoid `make clean`, as it also removes the builds of libraries (e.g.,
    // libtock), which the next app would then rebuild:
    match fs::remove_dir_all(dir.join("build")) {
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(e.into()),
        _ => {}
    }

    let targets = format!("TOCK_TARGETS={}", tock_targets.join(" "));
    make(&dir, &["-j", &targets], &format!("app:{name}"))?;

    let mut tabs = Vec::new();
    for entry in fs::read_dir(dir.join("build"))? {
        let path = entry?.path();
        if path.extension().is_some_and(|e| e == "tab") {
            tabs.push(path);
        }
    }
    ensure!(
        tabs.len() == 1,
        "expected exactly one TAB for {name}, found {tabs:?}"
    );
    Ok(tabs.swap_remove(0))
}
