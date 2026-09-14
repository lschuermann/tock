// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Run a prebuilt plan, or part of it, on a Treadmill job attached to its
//! board, as CI will: upload the plan and its prebuilt artifacts, and run the
//! harness there. All Treadmill interaction goes through the user's `tml` CLI.

use crate::build::{self, HOST_TARGETS};
use crate::plan::{self, Plan};
use crate::{hostspec, read_json, tests, write_json};
use log::{Level, debug, error, info, log, warn};
use std::collections::{BTreeMap, BTreeSet};
use std::io::{BufRead, BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

/// Directory in the job's home the bundle is unpacked into.
const REMOTE_DIR: &str = "tock-harness";

/// Image set jobs are created from.
const IMAGE_SET: &str = "linux";

/// TEMPORARY, until a Tock image overlay ships it: install `tockloader` at a
/// revision with flash-file support for the NUCLEO-F429ZI, the Picos and the
/// ESP32-C3 (newer than the one Tock's `shell.nix` pins; from the
/// `dev/ci-harness` branch of a fork, until that is merged upstream). Reused jobs may still have
/// another revision installed, so the installed one is recorded, and replaced
/// if it differs. Likewise `esptool` (v5, for its `esptool` command and
/// hyphenated options), for the ESP32-C3. The base images ship the other tools
/// the board backends invoke (`openocd`), and `pipx`.
const SETUP: &str = "\
    rev=d800023e78e617c467ef1bd71510d78d06e65ff6; \
    [ \"$(cat ~/.tockloader-rev 2>/dev/null)\" = \"$rev\" ] || { \
      pipx install --force \"git+https://github.com/lschuermann/tockloader@$rev\" && \
      echo \"$rev\" > ~/.tockloader-rev; } && \
    { command -v esptool >/dev/null || pipx install 'esptool>=5,<6'; }";

/// Shell for the end of a pipeline: copy its input to the job's serial console
/// (the last active non-VT kernel console, e.g. `ttyS0` or `ttyAMA10`), which
/// Treadmill records, if it is writable, and pass it on. Mirroring is best
/// effort: a console that fails writes (e.g. `ttyS0` with EIO on the QEMU
/// hosts) makes `tee` exit 1 after passing everything on, which must not
/// become the pipeline's (`pipefail`) exit status.
const MIRROR_TO_CONSOLE: &str = "\
    { c=/dev/null; for t in $(cat /sys/class/tty/console/active 2>/dev/null); do \
        case $t in tty[0-9]*) ;; *) c=/dev/$t ;; esac; done; \
      [ -w \"$c\" ] || c=/dev/null; tee -a \"$c\" 2>/dev/null || true; }";

#[derive(clap::Args)]
pub struct TreadmillArgs {
    #[arg(long)]
    plan: PathBuf,
    /// The manifest `prebuild` wrote for the plan; the artifacts and harness
    /// binaries next to it are uploaded
    #[arg(long)]
    manifest: PathBuf,
    /// Run only part of the plan
    #[command(flatten)]
    selection: plan::Selection,
    /// Run on this existing job as it is, instead of claiming or creating one
    #[arg(long, conflicts_with = "new_job")]
    job: Option<String>,
    /// Create a new job, rather than reusing an idle one of yours
    #[arg(long)]
    new_job: bool,
    /// Keep the job claimed, and its lease, after the run, instead of
    /// releasing it for other runs to reuse or Treadmill to reclaim
    #[arg(long)]
    keep: bool,
    /// How long to hold the job for the run (e.g. `30m`), instead of the time
    /// its tests may take, plus setup
    #[arg(long)]
    lease: Option<String>,
    #[arg(long, default_value = "results.json")]
    out: PathBuf,
    /// Directory to download each test's console transcript into
    #[arg(long, default_value = "logs")]
    logs: PathBuf,
}

/// Annotation marking the jobs `treadmill` creates, for later runs to reuse.
/// Its value is the image set they run.
const POOL_ANNOTATION: &str = "tock-harness.pool";

/// Annotation a run claims a job with, while it uses it. Its value only
/// records who: a job's revision, not the value, keeps two runs from claiming
/// it at once.
const CLAIM_ANNOTATION: &str = "tock-harness.claim";

/// Annotation counting the runs a job has had, to number the next one. Only
/// claims (conditional on the job's revision) change it.
const RUNS_ANNOTATION: &str = "tock-harness.runs";

/// Label prefix of the jobs `treadmill` runs in, and what separates it from
/// the run's description. Labels may not contain `:`.
const LABEL_PREFIX: &str = "tock-harness";
const LABEL_SEPARATOR: &str = " - ";

/// Longest label Treadmill accepts.
const MAX_LABEL: usize = 256;

/// What a lease allows for setting up a run in a new job, whose lease runs
/// from its dispatch: booting it, installing tools, and uploading the bundle.
const SETUP_NEW_JOB: Duration = Duration::from_secs(8 * 60);
/// ... and in a reused job, which only needs the upload and a tool check.
const SETUP_REUSED_JOB: Duration = Duration::from_secs(2 * 60);
/// What a lease allows per test beyond the test body's time limit, which
/// already assumes all of its waits time out: for flashing and resetting the
/// board, roughly constant at 10-15 s on the nRF52840DK hosts, and slack.
const PER_TEST_MARGIN: Duration = Duration::from_secs(30);

/// `tml job update`'s exit status when `--if-revision` no longer holds, and
/// when the job is stopping.
const TML_REVISION_MISMATCH: i32 = 3;
const TML_LEASE_REFUSED: i32 = 4;

/// The job this run claimed or created, while it holds it: terminated if the
/// run is interrupted.
static HELD_JOB: Mutex<Option<String>> = Mutex::new(None);

/// Terminate the job this run holds, if any, after setting its exit status to
/// `failure` with `message`.
fn terminate_held_job(message: &str) {
    let Some(job) = HELD_JOB.lock().unwrap().take() else {
        return;
    };
    let set_status = ["tml", "job", "set-exit-status", "failure", message].join(" ");
    tml(
        &["job", "exec", "--job", &job, "--", &set_status],
        Level::Debug,
        "tml | ",
    );
    info!("terminating job {job}");
    tml(&["job", "terminate", "--job", &job], Level::Debug, "tml | ");
}

/// How this run came by its job.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Acquired {
    /// Given with `--job`: used as it is, and left alone.
    Given,
    /// An idle job of earlier runs, claimed for this one, which is its run
    /// with this number.
    Reused(u64),
    /// Created, and claimed, for this run.
    Created,
}

/// Returns the process exit code: that of the remote `run`, or 2 if Treadmill
/// itself failed.
pub fn treadmill(opts: TreadmillArgs) -> i32 {
    // On Ctrl-C (which `tml` and `ssh` get too) or SIGTERM, don't leave the
    // job behind: the remote `run` may still hold its boards. `tml job create
    // --wait` terminates a job it is still waiting for itself.
    ctrlc::set_handler(|| {
        warn!("interrupted");
        terminate_held_job("'interrupted'");
        std::process::exit(130);
    })
    .expect("failed to install the termination handler");

    let full: Plan = read_json(&opts.plan);
    let planned: Vec<String> = full.tests.iter().map(|t| t.id.clone()).collect();
    let plan = full.select(&opts.selection);
    let skipped: Vec<&str> = planned
        .iter()
        .filter(|id| !plan.tests.iter().any(|t| &&t.id == id))
        .map(String::as_str)
        .collect();

    let mut boards: Vec<_> = plan.tests.iter().map(|t| t.board).collect();
    boards.sort();
    boards.dedup();
    let [board] = boards[..] else {
        panic!("tests must all target one board, found {boards:?}");
    };
    let host_board = board
        .host_spec_board()
        .unwrap_or_else(|| panic!("{board} is virtual; use `run` locally instead"));

    info!(
        "running {} of the plan's {} tests on a Treadmill job:",
        plan.tests.len(),
        planned.len()
    );
    info!("{}", plan.summary().trim_end());
    if !skipped.is_empty() {
        info!("skipping {} tests: {}", skipped.len(), skipped.join(", "));
    }

    // Bundle the prebuilt directory: the manifest, the artifacts and the
    // harness itself for each host architecture.
    let artifact_dir = opts.manifest.parent().unwrap_or(Path::new("."));
    for (arch, _) in HOST_TARGETS {
        let harness = artifact_dir.join(format!("tock-harness-{arch}"));
        assert!(
            harness.exists(),
            "{} is missing; is {} the output of `prebuild`?",
            harness.display(),
            opts.manifest.display()
        );
    }
    let manifest = opts
        .manifest
        .file_name()
        .and_then(|name| name.to_str())
        .expect("manifest path has no file name");
    let work = tempfile::tempdir().expect("failed to create a temporary directory");
    let bundle = work.path().join("bundle.tar");
    build::archive(artifact_dir, &bundle);
    // The selected part of the plan, so the remote `run` needs no filter.
    let plan_file = work.path().join("plan.json");
    write_json(&plan_file, &plan);

    let (job, acquired) = match opts.job {
        Some(job) => (job, Acquired::Given),
        None => {
            info!("the job's host needs, for each of these, a {board} DUT with:");
            for needs in host_requirements(&plan) {
                info!("  {needs}");
            }
            let claim = format!("{CLAIM_ANNOTATION}={}", claimant());
            // Unless given, as long as the tests may take once `setup` is done.
            let lease = |setup| match &opts.lease {
                Some(lease) => lease.clone(),
                None => {
                    let lease = lease_for(&plan, setup);
                    info!(
                        "leasing the job for {}: {} to set up, the rest for the tests",
                        human(lease),
                        human(setup)
                    );
                    lease_arg(lease)
                }
            };
            let reused = if opts.new_job {
                None
            } else {
                claim_idle_job(&plan, host_board, &|| lease(SETUP_REUSED_JOB), &claim)
            };
            match reused {
                Some((job, run)) => (job, Acquired::Reused(run)),
                None => {
                    let predicate = host_predicate(&plan, host_board);
                    debug!("host predicate: {predicate}");
                    let lease = lease(SETUP_NEW_JOB);
                    info!("creating a job, and waiting for it to start");
                    let label = job_label(&plan, None);
                    let Some(job) = create_job(&predicate, &lease, &label, &claim) else {
                        error!("failed to create a job");
                        return 2;
                    };
                    (job, Acquired::Created)
                }
            }
        }
    };
    if acquired != Acquired::Given {
        *HELD_JOB.lock().unwrap() = Some(job.clone());
    }

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
    // Quoted for the remote shell, which the claimant needs no quotes in.
    let launched_by = claimant().replace('\'', "");
    // A reused job may still run the harness of an earlier run that lost its
    // connection; stop it before taking over its boards.
    let script = format!(
        "export PATH=\"$HOME/.local/bin:$PATH\"; set -o pipefail; {{ \
         ({SETUP}) || {{ tml job set-exit-status failure 'tool setup failed'; exit 1; }}; \
         {{ pkill -f '^\\./tock-harness-' && sleep 1; }}; \
         rm -rf {REMOTE_DIR} && mkdir {REMOTE_DIR} && tar -C {REMOTE_DIR} -xf bundle.tar && \
         mv plan.json {REMOTE_DIR}/ && cd {REMOTE_DIR} && \
         ./tock-harness-$(uname -m){verbose} run --plan plan.json --manifest {manifest} \
         --job-origin {origin} --launched-by '{launched_by}'; \
         }} 2>&1 | {MIRROR_TO_CONSOLE}"
    );

    let size = std::fs::metadata(&bundle).map_or(0, |m| m.len());
    info!(
        "uploading the artifacts ({:.1} MiB) and plan to job {job}",
        size as f64 / (1 << 20) as f64
    );
    let upload = |local: &Path, remote: &str| {
        let args = ["job", "upload", "--job", &job, path_str(local), remote];
        tml(&args, Level::Debug, "tml | ") == Some(0)
    };
    let code = if upload(&bundle, "bundle.tar") && upload(&plan_file, "plan.json") {
        info!("running the tests on job {job}:");
        tml(
            &["job", "exec", "--job", &job, "--", &script],
            Level::Info,
            "job | ",
        )
    } else {
        error!("failed to upload to job {job}");
        None
    };

    if matches!(code, Some(0 | 1)) {
        let results = format!("{REMOTE_DIR}/results.json");
        let args = [
            "job",
            "download",
            "--job",
            &job,
            &results,
            path_str(&opts.out),
        ];
        if tml(&args, Level::Debug, "tml | ") == Some(0) {
            info!("wrote {}", opts.out.display());
        } else {
            warn!("failed to download {results} from job {job}");
        }
        download_logs(&job, &opts.logs);
    }

    // Release the job straight away, also on failure: it keeps running for
    // the next run to reuse, until Treadmill reclaims its host for another
    // job. The remote `run` has set its exit status.
    let release = format!(
        "tml job update --job {job} --lease-until now --remove-annotation {CLAIM_ANNOTATION} --yes"
    );
    let held = HELD_JOB.lock().unwrap().take();
    if held.is_some() && !opts.keep {
        info!("releasing job {job}");
        let args = [
            "job",
            "update",
            "--job",
            &job,
            "--lease-until",
            "now",
            "--remove-annotation",
            CLAIM_ANNOTATION,
            "--yes",
        ];
        if tml(&args, Level::Debug, "tml | ") != Some(0) {
            warn!("failed to release job {job}; release it with `{release}`");
        }
    } else if held.is_some() {
        info!("keeping job {job} claimed until its lease ends; release it with:");
        info!("  {release}");
    }

    match code {
        Some(0) => info!("all tests passed"),
        Some(1) => warn!("some tests failed"),
        // `ssh` exits with 255 when the connection fails, rather than the test.
        _ => error!("running the tests on job {job} failed"),
    }
    if code != Some(0) {
        info!("to debug:");
        if held.is_some() && !opts.keep {
            info!(
                "  tml job update --job {job} --lease-until now+30m \
                 --annotation {CLAIM_ANNOTATION}=debugging && tml job ssh --job {job}"
            );
        } else {
            info!("  tml job ssh --job {job}");
        }
        info!(
            "  cd {REMOTE_DIR} && ./tock-harness-$(uname -m) run --plan plan.json --manifest {manifest}"
        );
        if held.is_some() && !opts.keep {
            info!("and afterwards, to release it again:");
            info!("  {release}");
            info!("if Treadmill has reclaimed its host by then, resume it instead:");
            info!("  tml job create --resume {job} --wait --set-active && tml job ssh");
        }
    }

    code.filter(|code| matches!(code, 0 | 1)).unwrap_or(2)
}

/// How long to hold a job for running `plan` in it, after `setup`: as long as
/// its tests may take, each with [`PER_TEST_MARGIN`] for flashing.
/// Download the console transcripts `run` wrote on `job` into `dir`, as one
/// archive, so that failures can be debugged after the job is gone.
fn download_logs(job: &str, dir: &Path) {
    let archive = format!("{REMOTE_DIR}/logs.tar");
    let pack = format!("tar -C {REMOTE_DIR} -cf {archive} logs");
    let local = dir.with_extension("tar");
    let downloaded = tml(
        &["job", "exec", "--job", job, "--", &pack],
        Level::Debug,
        "job | ",
    ) == Some(0)
        && tml(
            &["job", "download", "--job", job, &archive, path_str(&local)],
            Level::Debug,
            "tml | ",
        ) == Some(0);
    if !downloaded {
        warn!("failed to download the console logs from job {job}");
        return;
    }
    // The archive holds `logs/`: unpack its contents into `dir`.
    let unpacked = std::fs::create_dir_all(dir).is_ok()
        && Command::new("tar")
            .args([
                "--strip-components=1",
                "-xf",
                path_str(&local),
                "-C",
                path_str(dir),
            ])
            .status()
            .is_ok_and(|s| s.success());
    let _ = std::fs::remove_file(&local);
    if unpacked {
        info!("wrote console logs to {}", dir.display());
    } else {
        warn!("failed to unpack the console logs into {}", dir.display());
    }
}

fn lease_for(plan: &Plan, setup: Duration) -> Duration {
    let tests: Duration = plan
        .tests
        .iter()
        .map(|planned| tests::lookup(&planned.id).body.time_limit() + PER_TEST_MARGIN)
        .sum();
    setup + tests
}

/// A lease in the form `tml` takes, rounded up to the second: `90s`.
fn lease_arg(lease: Duration) -> String {
    format!("{}s", lease.as_secs() + u64::from(lease.subsec_nanos() > 0))
}

/// `d` coarsely, for people: `45s`, `12m`, `12m 5s`.
fn human(d: Duration) -> String {
    let secs = d.as_secs();
    match secs {
        0..60 => format!("{secs}s"),
        _ if secs.is_multiple_of(60) => format!("{}m", secs / 60),
        _ => format!("{}m {}s", secs / 60, secs % 60),
    }
}

/// Who holds a job this run claims, for people looking at it:
/// `<user>@<host>, pid <pid>`.
fn claimant() -> String {
    let user = std::env::var("USER").unwrap_or_else(|_| "?".to_string());
    let host = std::fs::read_to_string("/proc/sys/kernel/hostname")
        .map_or_else(|_| "?".to_string(), |host| host.trim().to_string());
    format!("{user}@{host}, pid {}", std::process::id())
}

/// The parts of `tml job list`'s jobs this uses.
#[derive(serde::Deserialize)]
struct JobSummary {
    job_id: String,
    state: String,
    revision: i64,
    annotations: BTreeMap<String, String>,
    lease_expiry_action: String,
    dispatched_on_host_id: Option<String>,
    host_name: Option<String>,
}

/// The label of a job running `plan`: `tock-harness - <description>`, and
/// `tock-harness (run <n>) - <description>` once reused for its `n`th run.
/// The description names as many of the tests as fit.
fn job_label(plan: &Plan, run: Option<u64>) -> String {
    let prefix = match run {
        Some(run) => format!("{LABEL_PREFIX} (run {run}){LABEL_SEPARATOR}"),
        None => format!("{LABEL_PREFIX}{LABEL_SEPARATOR}"),
    };
    let mut boards: Vec<String> = plan.tests.iter().map(|t| t.board.to_string()).collect();
    boards.dedup();
    let tests: Vec<&str> = plan.tests.iter().map(|t| t.test_id()).collect();
    let [test] = tests[..] else {
        let mut label = format!("{prefix}{} tests on {} (", tests.len(), boards.join(", "));
        for (i, test) in tests.iter().enumerate() {
            let separator = if i == 0 { "" } else { ", " };
            // Room for this test, and for `, ...)` if others must be left out.
            let reserve = if i + 1 == tests.len() { 1 } else { 6 };
            if label.len() + separator.len() + test.len() + reserve > MAX_LABEL {
                label += if i == 0 { "...)" } else { ", ...)" };
                return label;
            }
            label += separator;
            label += test;
        }
        return label + ")";
    };
    format!("{prefix}{test} on {}", boards.join(", "))
}

/// Claim an idle job of an earlier run whose host can run `plan`, holding it
/// for `lease` under `claim`, and return its id and the number of the run
/// this is on it.
///
/// Only the caller's own jobs that `treadmill` created and no run has claimed
/// are candidates: idle, ready, and kept running (reclaimable) past their
/// lease. A job whose claim outlived its run (e.g. one that crashed) is never
/// reused; its lease has lapsed, so Treadmill reclaims its host when a job
/// needs it.
fn claim_idle_job(
    plan: &Plan,
    host_board: &str,
    lease: &dyn Fn() -> String,
    claim: &str,
) -> Option<(String, u64)> {
    let Some(jobs) = tml_json::<Vec<JobSummary>>(&["job", "list", "--mine"], Level::Debug) else {
        warn!("failed to list your jobs; creating a new one");
        return None;
    };
    let mut fits = BTreeMap::new();
    for job in jobs.iter().filter(|job| {
        job.state == "ready"
            && job.lease_expiry_action == "preempt"
            && job.annotations.get(POOL_ANNOTATION).map(String::as_str) == Some(IMAGE_SET)
            && !job.annotations.contains_key(CLAIM_ANNOTATION)
    }) {
        let Some(host) = &job.dispatched_on_host_id else {
            continue;
        };
        let host_name = job.host_name.as_deref().unwrap_or(host);
        if !*fits
            .entry(host.clone())
            .or_insert_with(|| host_fits(plan, host_board, host))
        {
            info!(
                "not reusing idle job {} on {host_name}, whose DUTs cannot run these tests",
                job.job_id
            );
            continue;
        }

        info!("claiming idle job {} on {host_name}", job.job_id);
        // A job from before runs were counted has had at least one.
        let run = job
            .annotations
            .get(RUNS_ANNOTATION)
            .and_then(|runs| runs.parse::<u64>().ok())
            .unwrap_or(1)
            + 1;
        let label = job_label(plan, Some(run));
        let runs = format!("{RUNS_ANNOTATION}={run}");
        let revision = job.revision.to_string();
        let lease_until = format!("now+{}", lease());
        let args = [
            "job",
            "update",
            "--job",
            &job.job_id,
            "--if-revision",
            &revision,
            "--lease-until",
            &lease_until,
            "--label",
            &label,
            "--annotation",
            claim,
            "--annotation",
            &runs,
            "--yes",
        ];
        match tml(&args, Level::Debug, "tml | ") {
            Some(0) => return Some((job.job_id.clone(), run)),
            Some(TML_REVISION_MISMATCH) => info!("another run claimed job {} first", job.job_id),
            Some(TML_LEASE_REFUSED) => info!("job {} is being stopped", job.job_id),
            _ => warn!("failed to claim job {}", job.job_id),
        }
    }
    info!("no idle job of yours can run these tests");
    None
}

/// Whether `host` (by id) can run every test of `plan`: has, for each, some
/// `host_board` DUT that satisfies all of its requirements, as the host
/// predicate of a new job asks.
fn host_fits(plan: &Plan, host_board: &str, host: &str) -> bool {
    #[derive(serde::Deserialize)]
    struct HostInfo {
        spec: Option<serde_json::Value>,
    }
    let spec = match tml_json::<HostInfo>(&["host", "show", host], Level::Debug) {
        Some(HostInfo { spec: Some(spec) }) => spec,
        _ => {
            warn!("failed to read the spec of host {host}");
            return false;
        }
    };
    let spec = match hostspec::from_value(spec) {
        Ok(spec) => spec,
        Err(e) => {
            warn!("host {host}: {e}");
            return false;
        }
    };
    plan.tests.iter().all(|planned| {
        let requires = tests::lookup(&planned.id).requires;
        spec.duts.iter().any(|dut| {
            dut.board == host_board && requires.iter().all(|r| r.satisfied_by(planned.board, dut))
        })
    })
}

/// What the host predicate asks of a host, one line per distinct set of
/// requirements: for each, some DUT must provide all of it.
fn host_requirements(plan: &Plan) -> BTreeSet<String> {
    plan.tests
        .iter()
        .map(|planned| {
            let needs: Vec<String> = tests::lookup(&planned.id)
                .requires
                .iter()
                .map(|r| r.describe(planned.board))
                .collect();
            needs.join(", ")
        })
        .collect()
}

/// A CEL predicate admitting hosts that can run every test of `plan`: for
/// each test, some DUT of the board satisfies all of its requirements.
fn host_predicate(plan: &Plan, host_board: &str) -> String {
    let clauses: BTreeSet<String> = plan
        .tests
        .iter()
        .map(|planned| {
            let conditions: Vec<String> = std::iter::once(format!("d.board == {host_board:?}"))
                .chain(
                    tests::lookup(&planned.id)
                        .requires
                        .iter()
                        .map(|r| r.cel(planned.board)),
                )
                .collect();
            format!("host.duts.exists(d, {})", conditions.join(" && "))
        })
        .collect();
    clauses.into_iter().collect::<Vec<_>>().join(" && ")
}

/// Create a job that later runs can reuse, claimed with `claim` for this one,
/// and wait for it to be reachable, returning its id.
fn create_job(predicate: &str, lease: &str, label: &str, claim: &str) -> Option<String> {
    #[derive(serde::Deserialize)]
    struct Created {
        job_id: String,
    }
    let pool = format!("{POOL_ANNOTATION}={IMAGE_SET}");
    let runs = format!("{RUNS_ANNOTATION}=1");
    let args = [
        "job",
        "create",
        "--wait",
        "--image",
        IMAGE_SET,
        "--host",
        predicate,
        "--label",
        label,
        "--lease",
        lease,
        "--reclaimable",
        "--annotation",
        &pool,
        "--annotation",
        claim,
        "--annotation",
        &runs,
    ];
    tml_json::<Created>(&args, Level::Info).map(|job| job.job_id)
}

/// Run `tml -o json`, logging each line it prints to stderr at `level`, and
/// parse its output; `None` if it fails.
fn tml_json<T: serde::de::DeserializeOwned>(args: &[&str], level: Level) -> Option<T> {
    let mut child = Command::new("tml")
        .args(["-o", "json"])
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run tml");
    let stderr = child.stderr.take().unwrap();
    let mut stdout = String::new();
    std::thread::scope(|scope| {
        scope.spawn(|| forward(stderr, level, "tml | "));
        child
            .stdout
            .take()
            .unwrap()
            .read_to_string(&mut stdout)
            .ok();
    });
    if !child.wait().expect("failed to wait for tml").success() {
        return None;
    }
    serde_json::from_str(&stdout)
        .map_err(|e| warn!("`tml {}` printed unexpected JSON: {e}", args.join(" ")))
        .ok()
}

/// Run `tml`, logging each line it prints at `level` after `prefix`. Returns
/// its exit code (`None` if killed by a signal).
fn tml(args: &[&str], level: Level, prefix: &str) -> Option<i32> {
    let mut child = Command::new("tml")
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run tml");
    let (stdout, stderr) = (child.stdout.take().unwrap(), child.stderr.take().unwrap());
    std::thread::scope(|scope| {
        scope.spawn(|| forward(stderr, level, prefix));
        forward(stdout, level, prefix);
    });
    child.wait().expect("failed to wait for tml").code()
}

/// Log each line read from `from` at `level`, after `prefix`.
fn forward(from: impl Read, level: Level, prefix: &str) {
    for line in BufReader::new(from).lines().map_while(Result::ok) {
        log!(level, "{prefix}{line}");
    }
}

fn path_str(path: &Path) -> &str {
    path.to_str().expect("path is not valid UTF-8")
}

#[cfg(test)]
mod label_tests {
    use super::*;

    fn plan(tests: &[&str]) -> Plan {
        plan::compute(&plan::Selection {
            tests: tests
                .iter()
                .map(|id| format!("{id}@nrf52840dk").parse().unwrap())
                .collect(),
        })
    }

    #[test]
    fn leases_cover_setup_and_each_test_with_its_flashing() {
        // hello_world waits up to 20s for its output, blink up to 14s.
        assert_eq!(
            lease_for(&plan(&["hello_world"]), SETUP_REUSED_JOB),
            SETUP_REUSED_JOB + Duration::from_secs(20) + PER_TEST_MARGIN
        );
        assert_eq!(
            lease_for(&plan(&["hello_world", "blink"]), SETUP_NEW_JOB),
            SETUP_NEW_JOB + Duration::from_secs(20 + 14) + 2 * PER_TEST_MARGIN
        );
        assert_eq!(lease_arg(Duration::from_millis(90_001)), "91s");
        assert_eq!(human(Duration::from_secs(725)), "12m 5s");
    }

    #[test]
    fn labels_describe_the_run_and_number_reuses() {
        assert_eq!(
            job_label(&plan(&["hello_world"]), None),
            "tock-harness - hello_world on nrf52840dk"
        );
        assert_eq!(
            job_label(&plan(&["hello_world", "blink"]), Some(3)),
            "tock-harness (run 3) - 2 tests on nrf52840dk (hello_world, blink)"
        );
    }

    #[test]
    fn long_labels_name_what_fits() {
        let all = plan(&["alltests"]);
        assert!(all.tests.len() > 10);
        let label = job_label(&all, Some(12));
        assert!(label.len() <= MAX_LABEL, "{} long: {label}", label.len());
        assert!(label.ends_with(", ...)"), "{label}");
        assert!(
            label
                .bytes()
                .all(|c| c.is_ascii_alphanumeric() || b" ()_,.#-".contains(&c))
        );
    }
}
