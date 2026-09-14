// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

mod board;
mod boards;
mod build;
mod hostspec;
mod plan;
mod requirement;
mod run;
mod testcase;
mod tests;

use std::path::{Path, PathBuf};
use std::process::Command;

const DEFAULT_HOST_SPEC: &str = "/run/tml/host-spec.json";

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("plan") => cmd_plan(&args[2..]),
        Some("prebuild") => cmd_prebuild(&args[2..]),
        Some("run") => cmd_run(&args[2..]),
        _ => {
            eprintln!("usage: tock-harness <plan|prebuild|run> [args]");
            std::process::exit(1);
        }
    }
}

fn cmd_plan(args: &[String]) {
    let out = arg_value(args, "--out").unwrap_or_else(|| "plan.json".to_string());
    let plan = plan::compute();
    std::fs::write(&out, serde_json::to_string_pretty(&plan).unwrap())
        .expect("failed to write plan");
    println!("wrote {out}");
}

fn cmd_prebuild(args: &[String]) {
    let plan_path = arg_value(args, "--plan").expect("--plan is required");
    let out_dir =
        PathBuf::from(arg_value(args, "--out-dir").unwrap_or_else(|| "artifacts".to_string()));
    let libtock_c = PathBuf::from(arg_value(args, "--libtock-c").expect("--libtock-c is required"));

    let plan: plan::Plan =
        serde_json::from_str(&std::fs::read_to_string(&plan_path).unwrap()).unwrap();
    let manifest = build::prebuild(&plan, &out_dir, &libtock_c);

    let manifest_path = out_dir.join("manifest.json");
    std::fs::write(
        &manifest_path,
        serde_json::to_string_pretty(&manifest).unwrap(),
    )
    .unwrap();
    println!("wrote {}", manifest_path.display());

    if let Some(archive) = arg_value(args, "--archive") {
        let status = Command::new("tar")
            .arg("-C")
            .arg(&out_dir)
            .arg("-cf")
            .arg(&archive)
            .arg(".")
            .status()
            .expect("failed to run tar");
        assert!(status.success(), "failed to create archive");
        println!("wrote {archive}");
    }
}

fn cmd_run(args: &[String]) {
    let plan_path = arg_value(args, "--plan").expect("--plan is required");
    let manifest_path = arg_value(args, "--manifest").expect("--manifest is required");
    let out = arg_value(args, "--out").unwrap_or_else(|| "results.json".to_string());
    let tests: Vec<String> = args
        .iter()
        .zip(args.iter().skip(1))
        .filter(|(flag, _)| *flag == "--test")
        .map(|(_, v)| v.clone())
        .collect();

    let plan: plan::Plan =
        serde_json::from_str(&std::fs::read_to_string(&plan_path).unwrap()).unwrap();
    let manifest: build::Manifest =
        serde_json::from_str(&std::fs::read_to_string(&manifest_path).unwrap()).unwrap();

    let host_spec = if args.iter().any(|a| a == "--no-host-spec") {
        None
    } else {
        let path = arg_value(args, "--host-spec").unwrap_or_else(|| DEFAULT_HOST_SPEC.to_string());
        Path::new(&path).exists().then(|| hostspec::load(Path::new(&path)))
    };

    let filter = if tests.is_empty() { None } else { Some(tests.as_slice()) };
    let reports = run::run(&plan, &manifest, run::RunOptions { host_spec, filter });

    std::fs::write(&out, serde_json::to_string_pretty(&reports).unwrap()).unwrap();

    let failed = reports
        .iter()
        .any(|r| matches!(r.outcome, run::TestOutcome::Failed { .. }));
    std::process::exit(if failed { 1 } else { 0 });
}

fn arg_value(args: &[String], name: &str) -> Option<String> {
    args.iter()
        .position(|a| a == name)
        .and_then(|i| args.get(i + 1))
        .cloned()
}
