// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

mod boards;
mod host_spec;
mod logging;
mod plan;
mod prebuild;
mod run;
mod test_cases;
mod treadmill;

use clap::Parser;
use std::process::ExitCode;

/// Tock integration test harness
#[derive(Parser)]
struct Cli {
    /// Increase log verbosity (`-v` for debug and `-vv` for trace)
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,
    #[command(subcommand)]
    command: Subcommand,
}

#[derive(clap::Subcommand)]
enum Subcommand {
    /// Step 1: generate a plan of tests and compute the artifacts they require
    Plan(plan::Args),
    /// Step 2: build the artifacts of a plan (kernels, applications, ...) and
    /// the harness for Treadmill hosts
    Prebuild(prebuild::Args),
    /// Step 3: run the tests of a plan on a board attached to this host
    Run(run::Args),
    /// Run the tests of a prebuilt plan on Treadmill jobs attached to their
    /// boards
    Treadmill(treadmill::Args),
}

fn main() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();

    logging::init(cli.verbose);

    match cli.command {
        Subcommand::Plan(args) => args.run().map(|()| ExitCode::SUCCESS),
        Subcommand::Prebuild(args) => args.run().map(|()| ExitCode::SUCCESS),
        Subcommand::Run(args) => args.run(),
        Subcommand::Treadmill(args) => args.run(),
    }
}
