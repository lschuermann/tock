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
mod treadmill;

use clap::Parser;
use log::{info, warn};
use std::path::{Path, PathBuf};
use std::process::Command;

/// Present only inside a Treadmill job, whose outcome `run` then reports.
const TML_JOB_ID: &str = "/run/tml/job-id";

/// Tock integration test harness
#[derive(Parser)]
struct Cli {
    /// Log more detail: `-v` for debug, `-vv` for trace. `RUST_LOG` also works.
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,
    #[command(subcommand)]
    command: Subcommand,
}

#[derive(clap::Subcommand)]
enum Subcommand {
    /// Select tests and compute what to build for them
    Plan {
        #[command(flatten)]
        selection: plan::Selection,
        #[arg(long, default_value = "plan.json")]
        out: PathBuf,
    },
    /// Build a plan's kernels and apps, and the harness for Treadmill hosts
    Prebuild {
        #[arg(long)]
        plan: PathBuf,
        #[arg(long)]
        libtock_c: PathBuf,
        #[arg(long, default_value = "artifacts")]
        out_dir: PathBuf,
        /// Also pack the output directory into this tar archive
        #[arg(long)]
        archive: Option<PathBuf>,
    },
    /// Run a plan's tests on this host's boards
    Run(RunArgs),
    /// Run a prebuilt plan, or part of it, on a Treadmill job attached to its board
    Treadmill(treadmill::TreadmillArgs),
}

#[derive(clap::Args)]
struct RunArgs {
    #[arg(long)]
    plan: PathBuf,
    /// The prebuilt artifacts' manifest; paths in it are relative to it
    #[arg(long)]
    manifest: PathBuf,
    #[arg(long, default_value = "/run/tml/host-spec.json")]
    host_spec: PathBuf,
    /// Ignore the host spec, even if it exists
    #[arg(long)]
    no_host_spec: bool,
    /// Run only part of the plan
    #[command(flatten)]
    selection: plan::Selection,
    #[arg(long, default_value = "results.json")]
    out: PathBuf,
    /// Directory to write each test's console transcript to
    #[arg(long, default_value = "logs")]
    logs: PathBuf,
    /// How `treadmill` came by the job this runs in, for the banner
    #[arg(long, value_enum, hide = true)]
    job_origin: Option<JobOrigin>,
    /// Who launched this run, for the banner
    #[arg(long, hide = true)]
    launched_by: Option<String>,
    /// Which of the job's `treadmill` runs this is, counting from 1
    #[arg(long, hide = true)]
    run: Option<u64>,
}

/// How `treadmill` came by the job a run is in.
#[derive(Clone, Copy, clap::ValueEnum)]
pub enum JobOrigin {
    /// Created for this run
    Fresh,
    /// An idle job of an earlier run, reused
    Reused,
    /// Given with `--job`
    Given,
}

fn main() {
    let cli = Cli::parse();
    init_logging(cli.verbose);
    match cli.command {
        Subcommand::Plan { selection, out } => {
            let plan = plan::compute(&selection);
            info!("{}", plan.summary().trim_end());
            write_json(&out, &plan);
            info!("wrote {}", out.display());
        }
        Subcommand::Prebuild {
            plan,
            libtock_c,
            out_dir,
            archive,
        } => {
            let manifest = build::prebuild(&read_json(&plan), &out_dir, &libtock_c);
            let manifest_path = out_dir.join("manifest.json");
            write_json(&manifest_path, &manifest);
            info!("wrote {}", manifest_path.display());
            if let Some(archive) = archive {
                build::archive(&out_dir, &archive);
                info!("wrote {}", archive.display());
            }
        }
        Subcommand::Run(args) => cmd_run(args),
        Subcommand::Treadmill(args) => std::process::exit(treadmill::treadmill(args)),
    }
}

/// Log to stderr, CLI-style: info messages as they are, others behind their
/// level (`warning: ...`, `debug: ...`).
fn init_logging(verbose: u8) {
    use std::io::Write;
    let level = match verbose {
        0 => log::LevelFilter::Info,
        1 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };
    env_logger::Builder::new()
        .filter_level(level)
        .parse_default_env()
        .format(|buf, record| match record.level() {
            log::Level::Info => writeln!(buf, "{}", record.args()),
            log::Level::Warn => writeln!(buf, "warning: {}", record.args()),
            level => writeln!(buf, "{}: {}", level.as_str().to_lowercase(), record.args()),
        })
        .init();
}

fn cmd_run(args: RunArgs) {
    let plan = read_json::<plan::Plan>(&args.plan).select(&args.selection);
    let mut manifest: build::Manifest = read_json(&args.manifest);
    let artifact_dir = args.manifest.parent().unwrap_or(Path::new(""));
    for path in manifest.values_mut() {
        *path = artifact_dir.join(&*path);
    }

    let host_spec =
        (!args.no_host_spec && args.host_spec.exists()).then(|| hostspec::load(&args.host_spec));
    print_banner(&plan, host_spec.as_ref(), &args);
    let reports = run::run(
        &plan,
        &manifest,
        run::RunOptions {
            host_spec,
            log_dir: &args.logs,
        },
    );
    write_json(&args.out, &reports);
    info!("{}", run::summary_table(&reports));

    let failed: Vec<&str> = reports
        .iter()
        .filter(|r| matches!(r.outcome, run::TestOutcome::Failed { .. }))
        .map(|r| r.id.as_str())
        .collect();
    if Path::new(TML_JOB_ID).exists() {
        report_exit_status(&reports, &failed, args.run);
    }
    std::process::exit(if failed.is_empty() { 0 } else { 1 });
}

/// Announce the run: after an empty line, so that the banner starts on a line
/// of its own even on a console the board's output left mid-line.
fn print_banner(plan: &plan::Plan, host_spec: Option<&hostspec::HostSpec>, args: &RunArgs) {
    let job = std::fs::read_to_string(TML_JOB_ID)
        .map(|id| id.trim().to_string())
        .ok();
    let origin = match (args.job_origin, args.run) {
        (Some(JobOrigin::Fresh), _) => " (created for this run)".to_string(),
        (Some(JobOrigin::Reused), Some(run)) => format!(" (reused: run {run} on this job)"),
        (Some(JobOrigin::Reused), None) => " (reused: it ran earlier runs)".to_string(),
        (Some(JobOrigin::Given), _) => " (given with --job)".to_string(),
        (None, _) => String::new(),
    };
    let host = host_spec.map(|spec| {
        let name = spec.name.as_deref().unwrap_or("unnamed host");
        match spec.platform.as_ref().and_then(|p| p.arch.as_deref()) {
            Some(arch) => format!("{name} ({arch})"),
            None => name.to_string(),
        }
    });

    let rule = "=".repeat(72);
    info!("");
    info!("{rule}");
    info!("tock-harness {} run", env!("CARGO_PKG_VERSION"));
    info!("  time:     {}", utc_now());
    info!(
        "  job:      {}{origin}",
        job.as_deref().unwrap_or("- (not in a Treadmill job)")
    );
    info!(
        "  host:     {}",
        host.as_deref().unwrap_or("- (no host spec)")
    );
    if let Some(launched_by) = &args.launched_by {
        info!("  launched: {launched_by}");
    }
    for line in plan.summary().lines() {
        info!("  {line}");
    }
    info!("{rule}");
}

/// The current time in UTC, as RFC 3339 to the second.
fn utc_now() -> String {
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_secs());
    utc_rfc3339(secs)
}

/// `secs` since the Unix epoch as an RFC 3339 UTC time, e.g.
/// `2026-09-29T01:55:12Z`.
fn utc_rfc3339(secs: u64) -> String {
    // Days to a proleptic Gregorian date, after Howard Hinnant's
    // `civil_from_days`, shifted to years starting on 1 March.
    let days = (secs / 86400) as i64 + 719_468;
    let era = days.div_euclid(146_097);
    let day_of_era = days.rem_euclid(146_097);
    let year_of_era =
        (day_of_era - day_of_era / 1460 + day_of_era / 36524 - day_of_era / 146_096) / 365;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let shifted_month = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * shifted_month + 2) / 5 + 1;
    let month = if shifted_month < 10 {
        shifted_month + 3
    } else {
        shifted_month - 9
    };
    let year = year_of_era + era * 400 + i64::from(month <= 2);
    let time = secs % 86400;
    format!(
        "{year:04}-{month:02}-{day:02}T{:02}:{:02}:{:02}Z",
        time / 3600,
        time % 3600 / 60,
        time % 60
    )
}

/// The file in the job's home, outside the directory `treadmill` replaces for
/// each run, where `run` records every run in a Treadmill job: a job that
/// `treadmill` reuses runs many.
const RUN_HISTORY: &str = ".tock-harness-runs.jsonl";

/// One run in a job, as recorded in [`RUN_HISTORY`].
#[derive(serde::Serialize, serde::Deserialize)]
struct RunRecord {
    /// The run's number among the job's `treadmill` runs; `None` for one
    /// started by hand.
    run: Option<u64>,
    time: String,
    passed: usize,
    failed: Vec<String>,
    skipped: usize,
}

/// Set the Treadmill job's exit status from all of its runs, as recorded in
/// [`RUN_HISTORY`] with this one: `failure` if any failed, with a summary of
/// this run and of the job's runs so far.
fn report_exit_status(reports: &[run::TestReport], failed: &[&str], run: Option<u64>) {
    let passed = reports
        .iter()
        .filter(|r| matches!(r.outcome, run::TestOutcome::Passed))
        .count();
    let record = RunRecord {
        run,
        time: utc_now(),
        passed,
        failed: failed.iter().map(|id| id.to_string()).collect(),
        skipped: reports.len() - passed - failed.len(),
    };
    let history = match std::env::var_os("HOME") {
        Some(home) => record_run(&Path::new(&home).join(RUN_HISTORY), record),
        None => {
            warn!("$HOME is not set, so this run is reported on its own");
            vec![record]
        }
    };
    let (outcome, message) = exit_status(&history);

    let status = Command::new("tml")
        .args(["job", "set-exit-status", outcome, &message])
        .status();
    if !status.is_ok_and(|s| s.success()) {
        warn!("failed to report the job's exit status");
    }
}

/// Append `record` to the run history at `path`, returning the whole history;
/// unreadable records of earlier runs are skipped.
fn record_run(path: &Path, record: RunRecord) -> Vec<RunRecord> {
    let mut history: Vec<RunRecord> = std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .filter_map(|line| serde_json::from_str(line).ok())
        .collect();
    let line = serde_json::to_string(&record).unwrap() + "\n";
    let appended = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .and_then(|mut file| std::io::Write::write_all(&mut file, line.as_bytes()));
    if let Err(e) = appended {
        warn!("failed to record this run in {}: {e}", path.display());
    }
    history.push(record);
    history
}

/// The exit status of a job with `history` (ending with the latest run): its
/// outcome, and e.g. `run 5: 2 passed, 0 failed, 0 skipped; 5 runs on this
/// job: 4 passed, 1 failed (run 3)`.
fn exit_status(history: &[RunRecord]) -> (&'static str, String) {
    fn first_three(items: &[String]) -> String {
        let mut list = items[..items.len().min(3)].join(", ");
        if items.len() > 3 {
            list += ", ...";
        }
        list
    }
    let name = |record: &RunRecord| match record.run {
        Some(run) => format!("run {run}"),
        None => "a manual run".to_string(),
    };

    let latest = history.last().expect("the history holds the latest run");
    let mut message = format!(
        "{}: {} passed, {} failed, {} skipped",
        if history.len() > 1 {
            name(latest)
        } else {
            "run".to_string()
        },
        latest.passed,
        latest.failed.len(),
        latest.skipped
    );
    if !latest.failed.is_empty() {
        message += &format!(" ({})", first_three(&latest.failed));
    }
    let failed_runs: Vec<String> = history
        .iter()
        .filter(|record| !record.failed.is_empty())
        .map(name)
        .collect();
    if history.len() > 1 {
        message += &format!(
            "; {} runs on this job: {} passed, {} failed",
            history.len(),
            history.len() - failed_runs.len(),
            failed_runs.len()
        );
        if !failed_runs.is_empty() {
            message += &format!(" ({})", first_three(&failed_runs));
        }
    }
    let outcome = if failed_runs.is_empty() {
        "success"
    } else {
        "failure"
    };
    (outcome, message)
}

fn read_json<T: serde::de::DeserializeOwned>(path: &Path) -> T {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
    serde_json::from_str(&text)
        .unwrap_or_else(|e| panic!("failed to parse {}: {e}", path.display()))
}

fn write_json(path: &Path, value: &impl serde::Serialize) {
    std::fs::write(path, serde_json::to_string_pretty(value).unwrap())
        .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
}

#[cfg(test)]
mod banner_tests {
    use super::{RunRecord, exit_status, record_run, utc_rfc3339};

    fn record(run: Option<u64>, failed: &[&str]) -> RunRecord {
        RunRecord {
            run,
            time: String::new(),
            passed: 2 - failed.len(),
            failed: failed.iter().map(|id| id.to_string()).collect(),
            skipped: 0,
        }
    }

    #[test]
    fn a_job_fails_if_any_of_its_runs_did() {
        assert_eq!(
            exit_status(&[record(Some(1), &[])]),
            ("success", "run: 2 passed, 0 failed, 0 skipped".to_string())
        );
        assert_eq!(
            exit_status(&[record(Some(1), &["blink@nrf52840dk"])]),
            (
                "failure",
                "run: 1 passed, 1 failed, 0 skipped (blink@nrf52840dk)".to_string()
            )
        );
        assert_eq!(
            exit_status(&[
                record(Some(1), &[]),
                record(Some(2), &["blink@nrf52840dk"]),
                record(None, &[]),
                record(Some(3), &[]),
            ]),
            (
                "failure",
                "run 3: 2 passed, 0 failed, 0 skipped; 4 runs on this job: 3 passed, 1 failed \
                 (run 2)"
                    .to_string()
            )
        );
        assert_eq!(
            exit_status(&[record(Some(1), &[]), record(Some(2), &[])]).0,
            "success"
        );
    }

    #[test]
    fn runs_accumulate_in_the_history() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("runs.jsonl");
        assert_eq!(record_run(&path, record(Some(1), &["a"])).len(), 1);
        std::fs::write(
            &path,
            std::fs::read_to_string(&path).unwrap() + "not a record\n",
        )
        .unwrap();
        let history = record_run(&path, record(Some(2), &[]));
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].failed, ["a"]);
        assert_eq!(history[1].run, Some(2));
    }

    #[test]
    fn formats_utc_times() {
        assert_eq!(utc_rfc3339(0), "1970-01-01T00:00:00Z");
        assert_eq!(utc_rfc3339(951_782_400), "2000-02-29T00:00:00Z");
        assert_eq!(utc_rfc3339(1_790_560_512), "2026-09-28T01:55:12Z");
        assert_eq!(utc_rfc3339(4_107_542_399), "2100-02-28T23:59:59Z");
    }
}
