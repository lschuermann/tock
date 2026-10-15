// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Support for runs inside a Treadmill job.
//!
//! Inside a job, `run` describes the job in its banner, and reports the outcome
//! of all of the job's runs as the job's exit status.

use crate::run::TestReport;
use crate::test_cases::TestOutcome;
use fs_err as fs;
use log::warn;
use std::path::Path;
use std::process::Command;
use std::time::SystemTime;

/// Path of the file holding the ID of the Treadmill job, which only exists
/// inside a job.
const TML_JOB_ID: &str = "/run/tml/job-id";

/// Path of the file, relative to the job's home, in which `run` records every
/// run in a Treadmill job.
///
/// A job which `treadmill` reuses runs many times. This file is outside of the
/// directory which `treadmill` replaces for each run.
const RUN_HISTORY: &str = ".tock-integration-test-runs.jsonl";

/// Arguments which `treadmill` passes to the `run` it starts in a job.
#[derive(clap::Args)]
pub struct JobArgs {
    /// Origin of the Treadmill job of this run
    #[arg(long, value_enum, hide = true)]
    job_origin: Option<JobOrigin>,
    /// Person or service which launched this run
    #[arg(long, hide = true)]
    pub launched_by: Option<String>,
    /// Number of this run among the runs of its job, counting from 1
    #[arg(long, hide = true)]
    run: Option<u64>,
}

/// Origin of the Treadmill job of a run.
#[derive(Clone, Copy, clap::ValueEnum)]
enum JobOrigin {
    /// Job created for this run
    Fresh,
    /// Idle job of an earlier run, which is reused
    Reused,
    /// Job given with `--job`
    Given,
}

impl JobArgs {
    /// Return the ID of the job of this run, and how `treadmill` came by it.
    pub fn describe_job(&self) -> String {
        let Ok(job) = fs::read_to_string(TML_JOB_ID) else {
            return "- (not in a Treadmill job)".to_string();
        };
        let origin = match (self.job_origin, self.run) {
            (Some(JobOrigin::Fresh), _) => " (created for this run)".to_string(),
            (Some(JobOrigin::Reused), Some(run)) => format!(" (reused, run {run} on this job)"),
            (Some(JobOrigin::Reused), None) => " (reused, it ran earlier runs)".to_string(),
            (Some(JobOrigin::Given), _) => " (given with --job)".to_string(),
            (None, _) => String::new(),
        };
        format!("{}{origin}", job.trim())
    }

    /// Set the exit status of the Treadmill job from all of its runs, if this
    /// run is inside a job.
    ///
    /// The runs are recorded in [`RUN_HISTORY`], along with this one. The
    /// status is `failure` if any run failed, and its message summarizes this
    /// run and all runs of the job so far.
    pub fn report(&self, reports: &[TestReport]) {
        if !Path::new(TML_JOB_ID).exists() {
            return;
        }
        let passed = (reports.iter())
            .filter(|r| r.outcome == TestOutcome::Passed)
            .count();
        let failed: Vec<String> = (reports.iter())
            .filter(|r| r.outcome.failed())
            .map(|r| r.id.clone())
            .collect();
        let record = RunRecord {
            run: self.run,
            time: humantime::format_rfc3339_seconds(SystemTime::now()).to_string(),
            passed,
            skipped: reports.len() - passed - failed.len(),
            failed,
        };
        let history = match std::env::var_os("HOME") {
            Some(home) => RunHistory::record(&Path::new(&home).join(RUN_HISTORY), record),
            None => {
                warn!("$HOME is not set, so this run is reported on its own");
                RunHistory(vec![record])
            }
        };
        let (outcome, message) = history.exit_status();

        let status = Command::new("tml")
            .args(["job", "set-exit-status", outcome, &message])
            .status();
        if !status.is_ok_and(|s| s.success()) {
            warn!("failed to report the job's exit status");
        }
    }
}

/// Run in a job, as recorded in [`RUN_HISTORY`].
#[derive(serde::Serialize, serde::Deserialize)]
struct RunRecord {
    /// Number of the run among the job's `treadmill` runs. This is `None` for
    /// a run which was started by hand.
    run: Option<u64>,
    time: String,
    passed: usize,
    failed: Vec<String>,
    skipped: usize,
}

/// Runs in a job, which end with the latest run.
struct RunHistory(Vec<RunRecord>);

impl RunHistory {
    /// Append `record` to the run history at `path`, and return the whole
    /// history.
    ///
    /// Unreadable records of earlier runs are skipped.
    fn record(path: &Path, record: RunRecord) -> RunHistory {
        let mut history: Vec<RunRecord> = fs::read_to_string(path)
            .unwrap_or_default()
            .lines()
            .filter_map(|line| serde_json::from_str(line).ok())
            .collect();
        let line = serde_json::to_string(&record).unwrap() + "\n";
        let appended = fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .and_then(|mut file| std::io::Write::write_all(&mut file, line.as_bytes()));
        if let Err(e) = appended {
            warn!("failed to record this run: {e}");
        }
        history.push(record);
        RunHistory(history)
    }

    /// Return the outcome and the message of the job's exit status (e.g.,
    /// `run 5: 2 passed, 0 failed, 0 skipped; 5 runs on this job: 4 passed, 1
    /// failed (run 3)`).
    fn exit_status(&self) -> (&'static str, String) {
        /// Join the first three of `items`, followed by an ellipsis if there
        /// are more.
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

        let history = &self.0;
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
        let failed_runs: Vec<String> = (history.iter())
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
}

#[cfg(test)]
mod tests {
    use super::{RunHistory, RunRecord};

    fn record(run: Option<u64>, failed: &[&str]) -> RunRecord {
        RunRecord {
            run,
            time: String::new(),
            passed: 2 - failed.len(),
            failed: failed.iter().map(|id| id.to_string()).collect(),
            skipped: 0,
        }
    }

    fn exit_status(history: Vec<RunRecord>) -> (&'static str, String) {
        RunHistory(history).exit_status()
    }

    #[test]
    fn a_job_fails_if_any_of_its_runs_did() {
        assert_eq!(
            exit_status(vec![record(Some(1), &[])]),
            ("success", "run: 2 passed, 0 failed, 0 skipped".to_string())
        );
        assert_eq!(
            exit_status(vec![record(Some(1), &["blink@nrf52840dk"])]),
            (
                "failure",
                "run: 1 passed, 1 failed, 0 skipped (blink@nrf52840dk)".to_string()
            )
        );
        assert_eq!(
            exit_status(vec![
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
            exit_status(vec![record(Some(1), &[]), record(Some(2), &[])]).0,
            "success"
        );
    }

    #[test]
    fn runs_accumulate_in_the_history() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("runs.jsonl");
        assert_eq!(
            RunHistory::record(&path, record(Some(1), &["a"])).0.len(),
            1
        );
        std::fs::write(
            &path,
            std::fs::read_to_string(&path).unwrap() + "not a record\n",
        )
        .unwrap();
        let history = RunHistory::record(&path, record(Some(2), &[])).0;
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].failed, ["a"]);
        assert_eq!(history[1].run, Some(2));
    }
}
