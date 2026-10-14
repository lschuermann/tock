// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Running tests in Treadmill jobs.
//!
//! The `treadmill` subcommand runs a prebuilt plan, or part of it, on Treadmill
//! jobs attached to its boards, as CI does. It uploads the plan and its
//! prebuilt artifacts, and runs the harness on each job. All interaction with
//! Treadmill goes through the user's `tml` CLI.

mod host;
mod in_job;
mod jobs;
mod tml;

pub use in_job::JobArgs;

use crate::boards::Board;
use crate::logging;
use crate::plan::{Plan, Selection, UnsupportedTest};
use crate::prebuild::{self, HOST_TARGETS};
use crate::run::{self, LOGS_DIR, RESULTS_FILE, TestReport};
use crate::test_cases::TestOutcome;
use anyhow::{Context, Result, bail, ensure};
use fs_err as fs;
use host::{fleet, host_predicate, host_requirements, unrunnable};
use jobs::{
    Acquired, CLAIM_ANNOTATION, claim_idle_job, claimant, create_job, hold_job, job_label,
    terminate_held_jobs, unhold_job,
};
use log::{Level, debug, error, info, warn};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::time::{Duration, Instant};
use tml::{tml, tml_stream};

/// Directory in the job's home into which the bundle is unpacked.
const REMOTE_DIR: &str = "tock-integration-test";
/// Output directory of the remote `run`, in [`REMOTE_DIR`].
const REMOTE_OUT_DIR: &str = "out";

/// Shell commands which install the tools of the board backends on a job.
///
/// TEMPORARY until a Tock image overlay ships these tools. This installs
/// `tockloader` at the revision of its master branch which Tock's `shell.nix`
/// pins, as the latest release lacks its flash file changes for the
/// NUCLEO-F429ZI, the Picos and the ESP32-C3. Reused jobs may have another
/// revision installed, so the installed revision is recorded, and replaced if
/// it differs. Likewise, this installs `esptool` v5 for the ESP32-C3, for its
/// `esptool` command and hyphenated options. The base images ship `pipx` and
/// the other tools which the board backends use (`openocd`).
const SETUP: &str = "\
    rev=b1b0cce04bd86fd524fb40a80bcda07a4cb39c63; \
    [ \"$(cat ~/.tockloader-rev 2>/dev/null)\" = \"$rev\" ] || { \
      pipx install --force \"git+https://github.com/tock/tockloader@$rev\" && \
      echo \"$rev\" > ~/.tockloader-rev; } && \
    { command -v esptool >/dev/null || pipx install 'esptool>=5,<6'; }";

/// Shell command for the end of a pipeline, which copies its input to the
/// job's serial console and passes it on.
///
/// The serial console is the last active non-VT kernel console (e.g., `ttyS0`
/// or `ttyAMA10`), which Treadmill records. Mirroring is best effort, and is
/// skipped if the console is not writable. A console which fails writes (e.g.,
/// `ttyS0` with EIO on the QEMU hosts) makes `tee` exit with status 1 after it
/// passed on all input. This status must not become the exit status of the
/// pipeline (with `pipefail`).
const MIRROR_TO_CONSOLE: &str = "\
    { c=/dev/null; for t in $(cat /sys/class/tty/console/active 2>/dev/null); do \
        case $t in tty[0-9]*) ;; *) c=/dev/$t ;; esac; done; \
      [ -w \"$c\" ] || c=/dev/null; tee -a \"$c\" 2>/dev/null || true; }";

#[derive(clap::Args)]
pub struct Args {
    /// Plan to run
    #[arg(long)]
    plan: PathBuf,
    /// Directory of the artifacts and harness binaries built by `prebuild`,
    /// which is uploaded
    #[arg(long)]
    artifacts_dir: PathBuf,
    /// Run only part of the plan
    #[command(flatten)]
    selection: Selection,
    /// Existing job to run on as it is, for tests of a single board
    #[arg(long, conflicts_with_all = ["new_job", "jobs_per_board"])]
    job: Option<String>,
    /// Maximum number of jobs per board, among which the board's tests are
    /// split
    #[arg(long, value_name = "N", default_value_t = 1,
          value_parser = clap::value_parser!(u32).range(1..))]
    jobs_per_board: u32,
    /// Always create a new job, without reusing an idle job
    #[arg(long)]
    new_job: bool,
    /// Keep the jobs claimed and leased after the run
    #[arg(long)]
    keep: bool,
    /// Lease of each job (e.g., `30m`), which defaults to the setup time plus
    /// the worst-case time of its tests
    #[arg(long)]
    lease: Option<String>,
    /// Directory to write the results of the tests to, and to download their
    /// console transcripts into
    #[arg(long)]
    out_dir: PathBuf,
}

/// Time which the lease of a new job allows for setting up a run.
///
/// The lease starts when the job is dispatched. Thus, this time covers booting
/// the job, installing tools and uploading the bundle.
const SETUP_NEW_JOB: Duration = Duration::from_secs(8 * 60);
/// Time which the lease of a reused job allows for setting up a run, which only
/// needs the upload and a check of the tools.
const SETUP_REUSED_JOB: Duration = Duration::from_secs(2 * 60);
/// Time which a lease allows per test beyond its worst-case time, for the
/// host's own work (e.g., resetting the board after the test).
///
/// The worst-case time already includes flashing the board and all of the
/// test's waits timing out.
const PER_TEST_MARGIN: Duration = Duration::from_secs(10);

/// Maximum length of the label of a job's log lines, without its `#<n>` suffix.
const MAX_LOG_LABEL: usize = 12;

/// Tests of one board, or a share of them, which run on a job of their own.
struct Group {
    /// Label of the group's log lines while several groups run at once.
    label: String,
    board: &'static Board,
    plan: Plan,
}

/// Split `plan` into the groups which run on a job each.
///
/// There is one group per board, or up to `jobs_per_board` groups for boards
/// with as many tests, each with a contiguous share of the board's tests. The
/// first group of a board reports the board's unsupported tests. The first
/// group overall reports the unsupported tests of boards without any tests.
fn groups(plan: &Plan, jobs_per_board: usize) -> Result<Vec<Group>> {
    let mut by_board: BTreeMap<&str, (&'static Board, Vec<usize>)> = BTreeMap::new();
    for (i, test) in plan.tests.iter().enumerate() {
        let (_, tests) = by_board
            .entry(test.board.name)
            .or_insert((test.board, Vec::new()));
        tests.push(i);
    }
    let mut groups = Vec::new();
    for (board, tests) in by_board.values() {
        ensure!(
            !board.is_virtual(),
            "{board} is virtual, so `run` runs it locally"
        );
        let jobs = jobs_per_board.clamp(1, tests.len());
        for share in 0..jobs {
            // The first `tests.len() % jobs` shares take one more test:
            let (size, extra) = (tests.len() / jobs, tests.len() % jobs);
            let begin = share * size + share.min(extra);
            let end = begin + size + usize::from(share < extra);
            let first = groups.is_empty();
            let unsupported = |u: &UnsupportedTest| {
                (share == 0 && u.board == *board) || (first && !by_board.contains_key(u.board.name))
            };
            groups.push(Group {
                label: log_label(board.name, share, jobs),
                board,
                plan: plan.part(|i, _| tests[begin..end].contains(&i), unsupported),
            });
        }
    }
    Ok(groups)
}

/// Return the label of the log lines of the share `share` among `shares` of
/// the tests of `board`.
///
/// The board's name is cut in the middle to [`MAX_LOG_LABEL`] characters, as
/// its ends tell boards apart. With several shares, the label ends in `#<n>`
/// for the `n`th share.
fn log_label(board: &str, share: usize, shares: usize) -> String {
    let chars: Vec<char> = board.chars().collect();
    let mut label = if chars.len() > MAX_LOG_LABEL {
        let head = (MAX_LOG_LABEL - 1) / 2;
        let tail = MAX_LOG_LABEL - 1 - head;
        let head: String = chars[..head].iter().collect();
        let tail: String = chars[chars.len() - tail..].iter().collect();
        format!("{head}~{tail}")
    } else {
        board.to_string()
    };
    if shares > 1 {
        label += &format!("#{}", share + 1);
    }
    label
}

/// Data which all groups of a run share.
struct Run<'a> {
    args: &'a Args,
    /// Archive of the artifacts directory, which is uploaded to each job.
    bundle: &'a Path,
    /// Temporary directory for the files of the run.
    work: &'a Path,
    /// Directory to download each test's console transcript into.
    logs: &'a Path,
}

/// Outcome of running a group.
struct Outcome {
    /// Exit code of `run` on the job, or 2 if Treadmill itself failed.
    code: i32,
    /// Job of the group, if one was acquired.
    job: Option<String>,
    /// Downloaded results of the group, if any.
    results: Option<PathBuf>,
}

/// Outcome of a group for which Treadmill itself failed.
const TREADMILL_FAILED: Outcome = Outcome {
    code: 2,
    job: None,
    results: None,
};

impl Args {
    /// Run the selected tests of the plan on Treadmill jobs, and write their
    /// reports to `out`.
    ///
    /// This returns the worst exit code of the remote `run`s, or 2 if Treadmill
    /// itself failed.
    pub fn run(self) -> Result<ExitCode> {
        // On Ctrl-C (which `tml` and `ssh` also receive) or SIGTERM, terminate
        // the held jobs, as the remote `run`s may still hold their boards.
        // `tml job create` also terminates a job which it is still waiting for
        // on Ctrl-C, but not on SIGTERM:
        ctrlc::set_handler(|| {
            warn!("interrupted");
            terminate_held_jobs("'interrupted'");
            std::process::exit(130);
        })
        .expect("failed to install the termination handler");
        // The threads which run each group spawn `tml` and cannot fail cleanly
        // if it is missing:
        Command::new("tml")
            .arg("--version")
            .output()
            .context("failed to run tml, which must be installed and on PATH")?;

        let (results, logs) = run::output_paths(&self.out_dir)?;
        fs::create_dir_all(&self.out_dir)?;

        let full = Plan::load(&self.plan)?;
        let planned: Vec<String> = full.tests.iter().map(|t| t.id()).collect();
        let selected = full.select(&self.selection)?;
        // Skip the tests which no host can run, like `run` skips the tests
        // which its host cannot run. With `--job`, the remote `run` skips them
        // itself:
        let mut unrunnable_reports = Vec::new();
        let plan = match self.job.is_none().then(fleet) {
            None => selected.part(|_, _| true, |_| true),
            Some(Err(e)) => {
                warn!("failed to list the Treadmill hosts, so running every test: {e:#}");
                selected.part(|_, _| true, |_| true)
            }
            Some(Ok(fleet)) => {
                let mut runnable = Vec::new();
                for planned in &selected.tests {
                    let reason = unrunnable(planned, &fleet);
                    runnable.push(reason.is_none());
                    if let Some(reason) = reason {
                        let outcome = TestOutcome::Skipped { reason };
                        unrunnable_reports.push(TestReport {
                            id: planned.id(),
                            outcome,
                        });
                    }
                }
                selected.part(|i, _| runnable[i], |_| true)
            }
        };
        let skipped: Vec<&str> = (planned.iter())
            .filter(|id| !plan.tests.iter().any(|t| t.id() == **id))
            .map(String::as_str)
            .collect();

        let groups = groups(&plan, self.jobs_per_board as usize)?;
        if self.job.is_some() && groups.len() > 1 {
            bail!(
                "--job runs the tests on one job, but these need {} (one per board)",
                groups.len()
            );
        }

        match groups.len() {
            1 => info!(
                "running {} of the plan's {} tests on a Treadmill job:",
                plan.tests.len(),
                planned.len()
            ),
            n => info!(
                "running {} of the plan's {} tests on {n} Treadmill jobs, in parallel:",
                plan.tests.len(),
                planned.len()
            ),
        }
        info!("{}", plan.summary().trim_end());
        if !skipped.is_empty() {
            info!("skipping {} tests: {}", skipped.len(), skipped.join(", "));
        }
        for report in &unrunnable_reports {
            if let TestOutcome::Skipped { reason } = &report.outcome {
                info!("skipping {}: {reason}", report.id);
            }
        }
        if groups.is_empty() {
            info!("no tests to run");
            write_results(&selected, &[], &unrunnable_reports, &results)?;
            return Ok(ExitCode::SUCCESS);
        }
        if groups.len() > 1 {
            info!("each job's lines are labeled:");
            for group in &groups {
                let tests: Vec<&str> = group.plan.tests.iter().map(|t| t.test.id).collect();
                info!(
                    "  {}: {} on {} ({})",
                    group.label,
                    count(tests.len(), "test"),
                    group.board,
                    tests.join(", ")
                );
            }
        }

        // Bundle the artifacts directory, which holds the artifacts and the
        // harness for each host architecture:
        for (arch, _) in HOST_TARGETS {
            let harness = self
                .artifacts_dir
                .join(format!("tock-integration-test-harness-{arch}"));
            ensure!(
                harness.exists(),
                "{} is missing, so {} is not the output of `prebuild`",
                harness.display(),
                self.artifacts_dir.display()
            );
        }
        let work = tempfile::tempdir()?;
        let bundle = work.path().join("bundle.tar");
        prebuild::pack(&self.artifacts_dir, &bundle)?;

        let run = Run {
            args: &self,
            bundle: &bundle,
            work: work.path(),
            logs: &logs,
        };
        let width = groups.iter().map(|g| g.label.len()).max().unwrap_or(0);
        let labeled = groups.len() > 1;
        let outcomes: Vec<Outcome> = std::thread::scope(|scope| {
            let threads: Vec<_> = (groups.iter().enumerate())
                .map(|(index, group)| {
                    let run = &run;
                    scope.spawn(move || {
                        if labeled {
                            logging::set_label(&group.label, width, index);
                        }
                        run_group(run, index, group)
                    })
                })
                .collect();
            (threads.into_iter())
                .map(|thread| thread.join().unwrap_or(TREADMILL_FAILED))
                .collect()
        });

        write_results(&selected, &outcomes, &unrunnable_reports, &results)?;
        if labeled {
            info!("summary:");
            for (group, outcome) in groups.iter().zip(&outcomes) {
                let on =
                    (outcome.job.as_ref()).map_or(String::new(), |job| format!(" on job {job}"));
                match outcome.code {
                    0 => info!("  {}: all tests passed{on}", group.label),
                    1 => warn!("  {}: some tests failed{on}", group.label),
                    _ => error!("  {}: running the tests failed{on}", group.label),
                }
            }
        }
        let code = outcomes.iter().map(|o| o.code).max().unwrap_or(2);
        match code {
            0 => info!("all tests passed"),
            1 => warn!("some tests failed"),
            _ if labeled => error!("running the tests failed on some jobs"),
            _ => {}
        }
        Ok(ExitCode::from(code as u8))
    }
}

/// Return `n` followed by `what`, in plural unless `n` is 1 (e.g., `1 test` or
/// `2 tests`).
fn count(n: usize, what: &str) -> String {
    format!("{n} {what}{}", if n == 1 { "" } else { "s" })
}

/// Write the results of all `outcomes`, and of the `skipped` tests which no
/// host can run, to `out`, in the order of `plan`.
///
/// Results of jobs whose results could not be downloaded are missing.
fn write_results(
    plan: &Plan,
    outcomes: &[Outcome],
    skipped: &[TestReport],
    out: &Path,
) -> Result<()> {
    let mut results: Vec<serde_json::Value> = (skipped.iter())
        .map(|report| serde_json::to_value(report).expect("a report serializes"))
        .collect();
    for path in outcomes.iter().filter_map(|o| o.results.as_ref()) {
        let part = fs::read_to_string(path)
            .map_err(anyhow::Error::from)
            .and_then(|json| {
                serde_json::from_str::<Vec<_>>(&json)
                    .with_context(|| format!("failed to parse {}", path.display()))
            });
        match part {
            Ok(part) => results.extend(part),
            Err(e) => warn!("{e:#}"),
        }
    }
    if results.is_empty() {
        return Ok(());
    }
    let position = |result: &serde_json::Value| {
        let id = result.get("id").and_then(|id| id.as_str());
        (plan.tests.iter())
            .position(|t| Some(t.id().as_str()) == id)
            .unwrap_or(usize::MAX)
    };
    results.sort_by_key(position);
    let json = serde_json::to_string_pretty(&results).unwrap();
    fs::write(out, json)?;
    info!("wrote {}", out.display());
    Ok(())
}

/// Run `group` on a job of its own.
///
/// This acquires the job, uploads the bundle and the group's plan, runs the
/// plan, downloads its results and logs, and releases the job.
fn run_group(run: &Run, index: usize, group: &Group) -> Outcome {
    // Upload the group's part of the plan, such that the remote `run` needs no
    // selection:
    let plan_file = run.work.join(format!("plan-{index}.json"));
    let json = serde_json::to_string_pretty(&group.plan).unwrap();
    if let Err(e) = fs::write(&plan_file, json) {
        error!("{e}");
        return TREADMILL_FAILED;
    }

    let Some((job, acquired)) = acquire_job(run.args, group) else {
        return TREADMILL_FAILED;
    };
    if acquired != Acquired::Given {
        hold_job(&job, true);
    }

    let size = fs::metadata(run.bundle).map_or(0, |m| m.len());
    info!(
        "uploading the artifacts ({:.1} MiB) and plan to job {job}",
        size as f64 / (1 << 20) as f64
    );
    let code = if upload(&job, run.bundle, "bundle.tar") && upload(&job, &plan_file, "plan.json") {
        run_tests(&job, acquired)
    } else {
        None
    };
    let results = (matches!(code, Some(0 | 1)))
        .then(|| download_results(run, index, &job))
        .flatten();

    // Release the job right away, also on failure. The job keeps running for
    // the next run to reuse, until Treadmill reclaims its host for another
    // job. The remote `run` has already set the job's exit status:
    let held = unhold_job(&job).is_some();
    if held {
        release_job(&job, run.args.keep);
    }
    report_outcome(code, &job, held && !run.args.keep);

    Outcome {
        code: code.filter(|code| matches!(code, 0 | 1)).unwrap_or(2),
        job: Some(job),
        results,
    }
}

/// Acquire the job to run `group` on.
///
/// This is the given job, an idle job to reuse, or a new job. This returns
/// `None` if creating a job failed.
fn acquire_job(args: &Args, group: &Group) -> Option<(String, Acquired)> {
    if let Some(job) = &args.job {
        info!("using the given job {job} as it is");
        return Some((job.clone(), Acquired::Given));
    }
    let plan = &group.plan;
    info!(
        "the job's host needs, for each of these, a {} DUT with:",
        group.board
    );
    for needs in host_requirements(plan) {
        info!("  {needs}");
    }
    let claim = format!("{CLAIM_ANNOTATION}={}", claimant());
    // Unless `--lease` is given, lease the job for `setup` plus the worst-case
    // time of the tests:
    let lease = |setup| match &args.lease {
        Some(lease) => lease.clone(),
        None => {
            let lease = lease_for(plan, setup);
            info!(
                "leasing the job for {}, {} to set up and the rest for the tests",
                human(lease),
                human(setup)
            );
            lease_arg(lease)
        }
    };
    let reused = if args.new_job {
        info!("not looking for an idle job to reuse, as --new-job is given");
        None
    } else {
        claim_idle_job(plan, &|| lease(SETUP_REUSED_JOB), &claim)
    };
    if let Some((job, run)) = reused {
        info!("reusing job {job}, for its run {run}");
        return Some((job, Acquired::Reused(run)));
    }
    let predicate = host_predicate(plan);
    debug!("host predicate: {predicate}");
    let lease = lease(SETUP_NEW_JOB);
    let label = job_label(plan, None);
    let Some(job) = create_job(&predicate, &lease, &label, &claim) else {
        error!("failed to create a job");
        return None;
    };
    Some((job, Acquired::Created))
}

/// Upload `local` to `remote` on `job`, and return `true` on success.
fn upload(job: &str, local: &Path, remote: &str) -> bool {
    let started = Instant::now();
    let status = tml(&["job", "upload", "--job", job, path_str(local), remote]);
    if status.success() {
        debug!("uploaded {remote} in {}", human(started.elapsed()));
    } else {
        error!(
            "failed to upload {remote} to job {job}: {}",
            status.reason()
        );
    }
    status.success()
}

/// Run the uploaded plan on `job`, and return the exit code of the remote
/// `run`.
fn run_tests(job: &str, acquired: Acquired) -> Option<i32> {
    let verbose = if log::log_enabled!(Level::Debug) {
        " -v"
    } else {
        ""
    };
    let origin = match acquired {
        Acquired::Given => "given".to_string(),
        Acquired::Reused(run) => format!("reused --run {run}"),
        Acquired::Created => "fresh --run 1".to_string(),
    };
    // Remove quotes from the claimant, which is quoted for the remote shell:
    let launched_by = claimant().replace('\'', "");
    // A reused job may still run the harness of an earlier run which lost its
    // connection. Stop that harness before taking over its boards:
    let script = format!(
        "export PATH=\"$HOME/.local/bin:$PATH\"; set -o pipefail; {{ \
         ({SETUP}) || {{ tml job set-exit-status failure 'tool setup failed'; exit 1; }}; \
         {{ pkill -f '^\\./tock-integration-test-harness-' && sleep 1; }}; \
         rm -rf {REMOTE_DIR} && mkdir {REMOTE_DIR} && tar -C {REMOTE_DIR} -xf bundle.tar && \
         mv plan.json {REMOTE_DIR}/ && cd {REMOTE_DIR} && \
         ./tock-integration-test-harness-$(uname -m){verbose} run --plan plan.json \
         --artifacts-dir . --out-dir {REMOTE_OUT_DIR} --host-spec /run/tml/host-spec.json \
         --job-origin {origin} --launched-by '{launched_by}'; \
         }} 2>&1 | {MIRROR_TO_CONSOLE}"
    );

    info!("running the tests on job {job}");
    if acquired == Acquired::Created {
        info!("the job first installs the tools the tests need, which takes a minute or two");
    }
    let started = Instant::now();
    let code = tml_stream(&["job", "exec", "--job", job, "--", &script]);
    info!("the remote run ended after {}", human(started.elapsed()));
    code
}

/// Download the results and console logs of the remote `run` on `job`, and
/// return the path of the results.
fn download_results(run: &Run, index: usize, job: &str) -> Option<PathBuf> {
    let remote = format!("{REMOTE_DIR}/{REMOTE_OUT_DIR}/{RESULTS_FILE}");
    let local = run.work.join(format!("results-{index}.json"));
    info!("downloading the results and console logs from job {job}");
    let status = tml(&["job", "download", "--job", job, &remote, path_str(&local)]);
    if !status.success() {
        warn!(
            "failed to download {remote} from job {job}: {}",
            status.reason()
        );
    }
    let logs = run.work.join(format!("logs-{index}.tar"));
    download_logs(job, &logs, run.logs);
    status.success().then_some(local)
}

/// Return the command which releases `job`.
fn release_command(job: &str) -> String {
    format!(
        "tml job update --job {job} --lease-until now --remove-annotation {CLAIM_ANNOTATION} --yes"
    )
}

/// Release `job` for later runs to reuse, unless `keep` is set.
fn release_job(job: &str, keep: bool) {
    if keep {
        info!("keeping job {job} claimed until its lease ends, it can be released with:");
        info!("  {}", release_command(job));
        return;
    }
    info!("releasing job {job}, for later runs to reuse");
    let status = tml(&[
        "job",
        "update",
        "--job",
        job,
        "--lease-until",
        "now",
        "--remove-annotation",
        CLAIM_ANNOTATION,
        "--yes",
    ]);
    if !status.success() {
        warn!(
            "failed to release job {job}: {}, it can be released with `{}`",
            status.reason(),
            release_command(job)
        );
    }
}

/// Log the outcome of running the tests on `job`, and the commands to debug a
/// failure.
fn report_outcome(code: Option<i32>, job: &str, released: bool) {
    match code {
        Some(0) => info!("all tests passed"),
        Some(1) => warn!("some tests failed"),
        // `ssh` exits with 255 when the connection fails:
        Some(code) => error!("running the tests on job {job} failed (exit status {code})"),
        None => error!("running the tests on job {job} failed"),
    }
    if code == Some(0) {
        return;
    }
    info!("commands to debug the run:");
    if released {
        info!(
            "  tml job update --job {job} --lease-until now+30m \
             --annotation {CLAIM_ANNOTATION}=debugging && tml job ssh --job {job}"
        );
    } else {
        info!("  tml job ssh --job {job}");
    }
    info!(
        "  cd {REMOTE_DIR} && rm -rf {REMOTE_OUT_DIR} && \
         ./tock-integration-test-harness-$(uname -m) run --plan plan.json --artifacts-dir . \
         --out-dir {REMOTE_OUT_DIR} --host-spec /run/tml/host-spec.json"
    );
    if released {
        info!("commands to release the job again afterwards:");
        info!("  {}", release_command(job));
        info!("commands to resume the job instead, if Treadmill has reclaimed its host by then:");
        info!("  tml job create --resume {job} && tml job ssh");
    }
}

/// Download the console transcripts which `run` wrote on `job` into `dir`,
/// such that failures can be debugged after the job is gone.
///
/// The transcripts are downloaded as one archive to `local`.
fn download_logs(job: &str, local: &Path, dir: &Path) {
    let archive = format!("{REMOTE_DIR}/logs.tar");
    let pack = format!("tar -C {REMOTE_DIR}/{REMOTE_OUT_DIR} -cf {archive} {LOGS_DIR}");
    let packed = tml(&["job", "exec", "--job", job, "--", &pack]);
    if !packed.success() {
        warn!(
            "failed to pack the console logs on job {job}: {}",
            packed.reason()
        );
        return;
    }
    let downloaded = tml(&["job", "download", "--job", job, &archive, path_str(local)]);
    if !downloaded.success() {
        warn!(
            "failed to download the console logs from job {job}: {}",
            downloaded.reason()
        );
        return;
    }
    // Unpack the contents of `logs/` in the archive into `dir`:
    let unpacked = fs::create_dir_all(dir).is_ok()
        && Command::new("tar")
            .args([
                "--strip-components=1",
                "-xf",
                path_str(local),
                "-C",
                path_str(dir),
            ])
            .status()
            .is_ok_and(|s| s.success());
    let _ = fs::remove_file(local);
    if unpacked {
        info!("wrote console logs to {}", dir.display());
    } else {
        warn!("failed to unpack the console logs into {}", dir.display());
    }
}

/// Return the lease of a job which runs `plan` after `setup`.
///
/// The lease covers the worst-case time of each test, plus
/// [`PER_TEST_MARGIN`].
fn lease_for(plan: &Plan, setup: Duration) -> Duration {
    let tests: Duration = (plan.tests.iter())
        .map(|planned| planned.test.worst_case(planned.board) + PER_TEST_MARGIN)
        .sum();
    setup + tests
}

/// Format `lease` as `tml` expects it, rounded up to the second (e.g., `90s`).
fn lease_arg(lease: Duration) -> String {
    format!("{}s", lease.as_secs() + u64::from(lease.subsec_nanos() > 0))
}

/// Format `d` coarsely for people (e.g., `45s`, `12m` or `12m 5s`).
fn human(d: Duration) -> String {
    let secs = d.as_secs();
    match secs {
        0..60 => format!("{secs}s"),
        _ if secs.is_multiple_of(60) => format!("{}m", secs / 60),
        _ => format!("{}m {}s", secs / 60, secs % 60),
    }
}

/// Return `path` as a string, and panic if it is not valid UTF-8.
fn path_str(path: &Path) -> &str {
    path.to_str().expect("path is not valid UTF-8")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan(tests: &[&str]) -> Plan {
        Plan::compute(&Selection {
            tests: tests
                .iter()
                .map(|id| format!("{id}@nrf52840dk").parse().unwrap())
                .collect(),
        })
        .unwrap()
    }

    #[test]
    fn leases_cover_setup_and_each_test_with_its_flashing() {
        // On the nRF52840DK, flashing takes up to 30 s and booting 5 s. Then,
        // `hello_world` waits up to 3 s for its output, and `blink` checks the
        // LEDs for up to 5 s:
        assert_eq!(
            lease_for(&plan(&["hello_world"]), SETUP_REUSED_JOB),
            SETUP_REUSED_JOB + Duration::from_secs(38) + PER_TEST_MARGIN
        );
        assert_eq!(
            lease_for(&plan(&["hello_world", "blink"]), SETUP_NEW_JOB),
            SETUP_NEW_JOB + Duration::from_secs(38 + 40) + 2 * PER_TEST_MARGIN
        );
        assert_eq!(lease_arg(Duration::from_millis(90_001)), "91s");
        assert_eq!(human(Duration::from_secs(725)), "12m 5s");
    }

    #[test]
    fn log_labels_are_short_and_tell_boards_and_shares_apart() {
        assert_eq!(log_label("nrf52840dk", 0, 1), "nrf52840dk");
        assert_eq!(log_label("nrf52840dk", 1, 3), "nrf52840dk#2");
        assert_eq!(log_label("nucleo_f429zi", 0, 1), "nucle~f429zi");
        assert_eq!(log_label("raspberry_pi_pico", 0, 1), "raspb~i_pico");
        assert_eq!(log_label("raspberry_pi_pico_2", 0, 1), "raspb~pico_2");
        assert_eq!(log_label("esp32-c3-devkitm-1", 0, 2), "esp32~kitm-1#1");
    }

    #[test]
    fn groups_split_boards_and_share_their_tests() {
        let plan = Plan::compute(&Selection {
            tests: [
                "hello_world@nrf52840dk",
                "blink@nrf52840dk",
                "hello_world@nucleo_f429zi",
            ]
            .iter()
            .map(|s| s.parse().unwrap())
            .collect(),
        })
        .unwrap();
        let tests = |g: &Group| -> Vec<String> { g.plan.tests.iter().map(|t| t.id()).collect() };

        let one = groups(&plan, 1).unwrap();
        assert_eq!(one.len(), 2);
        assert_eq!(one[0].label, "nrf52840dk");
        assert_eq!(tests(&one[0]).len(), 2);
        assert_eq!(one[1].label, "nucle~f429zi");
        // The plan of each group only holds the artifacts of its tests:
        assert!(one.iter().all(|g| {
            g.plan.artifacts.len() < plan.artifacts.len()
                && (g.plan.tests.iter()).all(|t| g.plan.artifacts.contains_key(&t.kernel))
        }));

        // With more jobs than tests, there is one job per test:
        let shared = groups(&plan, 3).unwrap();
        let labels: Vec<&str> = shared.iter().map(|g| g.label.as_str()).collect();
        assert_eq!(labels, ["nrf52840dk#1", "nrf52840dk#2", "nucle~f429zi"]);
        assert_eq!(
            shared.iter().flat_map(tests).collect::<Vec<_>>(),
            one.iter().flat_map(tests).collect::<Vec<_>>()
        );
    }
}
