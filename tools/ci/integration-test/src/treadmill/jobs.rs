// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Claiming, creation and release of the Treadmill jobs which runs use.

use super::host::host_fits;
use super::human;
use super::tml::{TML_LEASE_REFUSED, TML_REVISION_MISMATCH, tml, tml_events, tml_json};
use crate::plan::Plan;
use log::{debug, error, info, warn};
use std::collections::BTreeMap;
use std::sync::Mutex;
use std::time::Instant;

/// Image set from which jobs are created.
const IMAGE_SET: &str = "linux";

/// Annotation which marks the jobs that `treadmill` creates, such that later
/// runs can reuse them. Its value is the image set which the jobs run.
const POOL_ANNOTATION: &str = "tock-integration-test.pool";

/// Annotation with which a run claims a job while it uses it.
///
/// Its value only records who claimed the job. The job's revision keeps two
/// runs from claiming it at once.
pub const CLAIM_ANNOTATION: &str = "tock-integration-test.claim";

/// Annotation which counts the runs of a job, to number the next run. Only
/// claims (conditional on the job's revision) change it.
const RUNS_ANNOTATION: &str = "tock-integration-test.runs";

/// Label prefix of the jobs in which `treadmill` runs tests. Labels may not
/// contain `:`.
const LABEL_PREFIX: &str = "tock-integration-test";
/// Separator between the label prefix and the description of a run.
const LABEL_SEPARATOR: &str = " - ";

/// Maximum length of a label which Treadmill accepts.
const MAX_LABEL: usize = 256;

/// Job which this run claimed or created, while the run holds it.
pub struct HeldJob {
    id: String,
    /// Whether the job is reachable over SSH. A job which is still starting is
    /// not.
    ready: bool,
}

/// Jobs which this run holds.
///
/// These jobs are terminated if the run is interrupted, or if they fail to
/// start.
static HELD_JOBS: Mutex<Vec<HeldJob>> = Mutex::new(Vec::new());

/// Lock and return [`HELD_JOBS`].
fn held_jobs() -> std::sync::MutexGuard<'static, Vec<HeldJob>> {
    HELD_JOBS.lock().unwrap_or_else(|e| e.into_inner())
}

/// Hold the job `id`, which is reachable over SSH if `ready`.
pub fn hold_job(id: &str, ready: bool) {
    let mut held = held_jobs();
    match held.iter_mut().find(|job| job.id == id) {
        Some(job) => job.ready = ready,
        None => held.push(HeldJob {
            id: id.to_string(),
            ready,
        }),
    }
}

/// Stop holding the job `id`, and return it if this run held it.
pub fn unhold_job(id: &str) -> Option<HeldJob> {
    let mut held = held_jobs();
    let index = held.iter().position(|job| job.id == id)?;
    Some(held.remove(index))
}

/// Terminate every job this run holds, as [`terminate_job`] does.
pub fn terminate_held_jobs(message: &str) {
    let jobs = std::mem::take(&mut *held_jobs());
    for job in jobs {
        terminate_job(&job, message);
    }
}

/// Terminate `job`.
///
/// If the job has started, this first sets its exit status to `failure` with
/// `message`.
fn terminate_job(job: &HeldJob, message: &str) {
    if job.ready {
        let set_status = ["tml", "job", "set-exit-status", "failure", message].join(" ");
        let status = tml(&["job", "exec", "--job", &job.id, "--", &set_status]);
        if !status.success() {
            debug!(
                "failed to set the exit status of job {}: {}",
                job.id,
                status.reason()
            );
        }
    }
    info!("terminating job {}", job.id);
    let terminated = tml(&["job", "terminate", "--job", &job.id, "--yes"]);
    if !terminated.success() {
        warn!(
            "failed to terminate job {}: {}, it can be terminated with \
             `tml job terminate --job {} --yes`",
            job.id,
            terminated.reason(),
            job.id
        );
    }
}

/// Way in which this run came by its job.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Acquired {
    /// Job given with `--job`, which is used as it is and left alone.
    Given,
    /// Idle job of earlier runs, which is claimed for this run. This run is
    /// the job's run with the given number.
    Reused(u64),
    /// Job created and claimed for this run.
    Created,
}

/// Return the holder of the jobs which this run claims, for people looking at
/// them (e.g., `<user>@<host>, pid <pid>`).
pub fn claimant() -> String {
    let user = std::env::var("USER").unwrap_or_else(|_| "?".to_string());
    let host = fs_err::read_to_string("/proc/sys/kernel/hostname")
        .map_or_else(|_| "?".to_string(), |host| host.trim().to_string());
    format!("{user}@{host}, pid {}", std::process::id())
}

/// Fields of the jobs in the output of `tml job list` which this module uses.
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

/// Return the label of a job which runs `plan`.
///
/// The label is `tock-integration-test - <description>`, or
/// `tock-integration-test (run <n>) - <description>` once the job is reused
/// for its `n`th run. The description names as many of the tests as fit.
pub fn job_label(plan: &Plan, run: Option<u64>) -> String {
    let prefix = match run {
        Some(run) => format!("{LABEL_PREFIX} (run {run}){LABEL_SEPARATOR}"),
        None => format!("{LABEL_PREFIX}{LABEL_SEPARATOR}"),
    };
    let mut boards: Vec<&str> = plan.tests.iter().map(|t| t.board.name).collect();
    boards.dedup();
    let tests: Vec<&str> = plan.tests.iter().map(|t| t.test.id).collect();
    let [test] = tests[..] else {
        let mut label = format!("{prefix}{} tests on {} (", tests.len(), boards.join(", "));
        for (i, test) in tests.iter().enumerate() {
            let separator = if i == 0 { "" } else { ", " };
            // Leave room for this test, and for `, ...)` if other tests are
            // left out:
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

/// Claim an idle job of an earlier run, whose host can run `plan`.
///
/// The job is held for `lease` under `claim`. This returns the job's ID, and
/// the number of this run on the job.
///
/// Candidates are the caller's own jobs which `treadmill` created and no run
/// has claimed. These jobs are idle, ready, and kept running (reclaimable)
/// past their lease. A job whose claim outlived its run (e.g., a crashed run)
/// is never reused. Its lease has lapsed, such that Treadmill reclaims its host
/// once another job needs it.
pub fn claim_idle_job(
    plan: &Plan,
    lease: &dyn Fn() -> String,
    claim: &str,
) -> Option<(String, u64)> {
    info!("looking for an idle job of yours to reuse");
    let jobs = match tml_json::<Vec<JobSummary>>(&["job", "list", "--mine"]) {
        Ok(jobs) => jobs,
        Err(e) => {
            warn!("failed to list your jobs ({e}); creating a new one");
            return None;
        }
    };
    let pooled: Vec<&JobSummary> = jobs
        .iter()
        .filter(|job| job.annotations.get(POOL_ANNOTATION).map(String::as_str) == Some(IMAGE_SET))
        .collect();
    let idle: Vec<&JobSummary> = pooled
        .iter()
        .copied()
        .filter(|job| {
            job.state == "ready"
                && job.lease_expiry_action == "preempt"
                && !job.annotations.contains_key(CLAIM_ANNOTATION)
        })
        .collect();
    info!(
        "found {} running jobs of yours, {} of earlier runs, {} of them idle",
        jobs.len(),
        pooled.len(),
        idle.len()
    );
    for job in pooled
        .iter()
        .filter(|job| !idle.iter().any(|i| i.job_id == job.job_id))
    {
        let claimed = job.annotations.get(CLAIM_ANNOTATION);
        debug!(
            "not reusing job {}: {}",
            job.job_id,
            match claimed {
                Some(by) => format!("claimed by {by}"),
                None => format!(
                    "{}, {} once its lease ends",
                    job.state, job.lease_expiry_action
                ),
            }
        );
    }
    let mut fits = BTreeMap::new();
    for job in idle {
        let Some(host) = &job.dispatched_on_host_id else {
            continue;
        };
        let host_name = job.host_name.as_deref().unwrap_or(host);
        if !*fits
            .entry(host.clone())
            .or_insert_with(|| host_fits(plan, host))
        {
            info!(
                "not reusing idle job {} on {host_name}, whose DUTs cannot run these tests",
                job.job_id
            );
            continue;
        }

        info!("claiming idle job {} on {host_name}", job.job_id);
        // A job from before runs were counted has had at least one run:
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
        let status = tml(&args);
        match status.code {
            Some(0) => return Some((job.job_id.clone(), run)),
            Some(TML_REVISION_MISMATCH) => info!("another run claimed job {} first", job.job_id),
            Some(TML_LEASE_REFUSED) => info!("job {} is being stopped", job.job_id),
            _ => warn!("failed to claim job {}: {}", job.job_id, status.reason()),
        }
    }
    info!("no idle job of yours can run these tests");
    None
}

/// Event which `tml -o jsonl job create` prints as the job starts, with one
/// event per line.
///
/// This only contains the fields which this module uses. Parsing is lenient,
/// such that `tml` can add event types and fields. Unknown types are
/// [`JobEvent::Other`], and unknown fields are ignored. Only the fields which
/// this module needs are required, and others take their default (`None` for
/// `Option` fields). Lines which still fail to parse are skipped.
#[derive(serde::Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum JobEvent {
    Enqueued {
        job_id: String,
        host_requirements: Option<HostCounts>,
    },
    Warning {
        message: String,
    },
    State {
        state: String,
        initializing_stage: Option<String>,
    },
    /// Compatible hosts of a queued job, counted by state.
    Hosts {
        #[serde(default)]
        compatible: BTreeMap<String, usize>,
    },
    Assigned {
        host_id: Option<String>,
        host_name: Option<String>,
        #[serde(default)]
        boards: Vec<String>,
    },
    Ready,
    Ended {
        termination_reason: Option<String>,
        job_error: Option<String>,
    },
    #[serde(other)]
    Other,
}

#[derive(serde::Deserialize)]
struct HostCounts {
    authorized: Option<u32>,
    predicate_matched: Option<u32>,
    schedulable: Option<usize>,
}

/// Return the phase of a job which is waiting to start, for people.
fn job_phase(state: &str, stage: Option<&str>) -> String {
    match (state, stage) {
        ("queued", _) => "queued, waiting for a host".to_string(),
        ("assigned", _) => "assigned to a host".to_string(),
        ("initializing", Some(stage)) => format!("initializing: {}", stage.replace('_', " ")),
        ("initializing", None) => "initializing".to_string(),
        ("ready", _) => "booted, waiting for SSH".to_string(),
        (state, _) => state.to_string(),
    }
}

/// Create a job which later runs can reuse, and which is claimed for this run
/// with `claim`. This waits until the job is reachable, and returns its ID.
///
/// The job is held from when it is enqueued, and terminated if it fails to
/// start. Starting the job can take minutes, as it waits for a host and boots
/// on it. This logs the progress of both.
pub fn create_job(predicate: &str, lease: &str, label: &str, claim: &str) -> Option<String> {
    let pool = format!("{POOL_ANNOTATION}={IMAGE_SET}");
    let runs = format!("{RUNS_ANNOTATION}=1");
    let args = [
        "job",
        "create",
        "--non-interactive",
        "--no-set-active",
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
    info!("creating a job, and waiting for it to start, which can take several minutes");
    let started = Instant::now();
    let since = || format!("+{}", human(started.elapsed()));
    let mut enqueued = None;
    let mut phase = String::new();
    let status = tml_events(&args, |event| match event {
        None => match &enqueued {
            Some(job) => info!("  still waiting for job {job}: {phase} ({})", since()),
            None => info!("  still waiting for the job to be enqueued ({})", since()),
        },
        Some(JobEvent::Enqueued {
            job_id,
            host_requirements: hosts,
        }) => {
            hold_job(&job_id, false);
            match hosts {
                Some(HostCounts {
                    authorized: Some(authorized),
                    schedulable: Some(schedulable),
                    predicate_matched,
                }) => {
                    info!(
                        "  enqueued job {job_id}: {schedulable} of the {authorized} hosts \
                         you may use can run it"
                    );
                    if let Some(matched) = predicate_matched {
                        debug!("  {matched} hosts matched the host predicate");
                    }
                }
                _ => info!("  enqueued job {job_id}"),
            }
            enqueued = Some(job_id);
        }
        Some(JobEvent::Warning { message }) => warn!("  tml: {message}"),
        Some(JobEvent::State {
            state,
            initializing_stage,
        }) => {
            let next = job_phase(&state, initializing_stage.as_deref());
            // Skip the `assigned` state, as the following `assigned` event
            // names the host:
            if next != phase && state != "assigned" {
                info!("  {next} ({})", since());
            }
            phase = next;
        }
        Some(JobEvent::Hosts { compatible }) if compatible.is_empty() => {
            warn!("  no compatible host is available, so the job waits for one")
        }
        Some(JobEvent::Hosts { compatible }) => {
            let counts: Vec<String> = compatible
                .iter()
                .map(|(state, n)| format!("{n} {state}"))
                .collect();
            info!("  compatible hosts: {}", counts.join(", "));
        }
        Some(JobEvent::Assigned {
            host_id,
            host_name,
            boards,
        }) => {
            let host = host_name.or(host_id).unwrap_or_else(|| "?".to_string());
            if boards.is_empty() {
                info!("  assigned to host {host} ({})", since());
            } else {
                info!(
                    "  assigned to host {host}, with {} ({})",
                    boards.join(", "),
                    since()
                );
            }
        }
        Some(JobEvent::Ready) => phase = "ready".to_string(),
        Some(JobEvent::Ended {
            termination_reason,
            job_error,
        }) => {
            // The job is ending already, so it needs no termination:
            if let Some(job) = &enqueued {
                unhold_job(job);
            }
            let reason =
                termination_reason.map_or("terminating".to_string(), |r| r.replace('_', " "));
            match job_error {
                Some(e) => error!("  the job ended before becoming ready: {reason} ({e})"),
                None => error!("  the job ended before becoming ready: {reason}"),
            }
        }
        Some(JobEvent::Other) => {}
    });
    match (status.success(), enqueued) {
        (true, Some(job)) => {
            info!("job {job} is ready, after {}", human(started.elapsed()));
            Some(job)
        }
        (true, None) => {
            error!("`tml job create` succeeded, but printed no job it created");
            None
        }
        (false, job) => {
            error!("`tml job create` failed: {}", status.reason());
            if let Some(held) = job.and_then(|job| unhold_job(&job)) {
                terminate_job(&held, "'failed to start'");
            }
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::Selection;

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
    fn labels_describe_the_run_and_number_reuses() {
        assert_eq!(
            job_label(&plan(&["hello_world"]), None),
            "tock-integration-test - hello_world on nrf52840dk"
        );
        assert_eq!(
            job_label(&plan(&["hello_world"]), Some(3)),
            "tock-integration-test (run 3) - hello_world on nrf52840dk"
        );
    }

    #[test]
    fn job_create_events_parse() {
        let parse = |line: &str| serde_json::from_str::<JobEvent>(line).unwrap();
        assert!(matches!(
            parse(
                r#"{"type":"enqueued","job_id":"j","active":false,"host_requirements":
                {"authorized":5,"predicate_matched":3,"image_matched":null,"schedulable":2,"errored":0}}"#
            ),
            JobEvent::Enqueued {
                host_requirements: Some(HostCounts {
                    schedulable: Some(2),
                    ..
                }),
                ..
            }
        ));
        assert!(matches!(
            parse(
                r#"{"type":"state","state":"initializing","initializing_stage":"fetching_image"}"#
            ),
            JobEvent::State {
                initializing_stage: Some(_),
                ..
            }
        ));
        assert!(matches!(
            parse(r#"{"type":"hosts","compatible":{"busy":1,"reclaimable":2}}"#),
            JobEvent::Hosts { compatible } if compatible.len() == 2
        ));
        assert!(matches!(
            parse(r#"{"type":"assigned","host_id":"h","host_name":null,"boards":["nrf52840dk"]}"#),
            JobEvent::Assigned {
                host_name: None,
                ..
            }
        ));
        assert!(matches!(parse(r#"{"type":"ready"}"#), JobEvent::Ready));
        assert!(matches!(
            parse(
                r#"{"type":"ended","state":"finalized","termination_reason":"queue_timeout","job_error":null}"#
            ),
            JobEvent::Ended {
                termination_reason: Some(_),
                job_error: None
            }
        ));
        assert!(matches!(
            parse(r#"{"type":"job","job":{"job_id":"j","state":"ready","revision":1}}"#),
            JobEvent::Other
        ));
        assert!(matches!(
            parse(r#"{"type":"cancelled","job_id":"j"}"#),
            JobEvent::Other
        ));
        // Types and fields of later versions are ignored, and missing fields
        // which this module does not need take their default:
        assert!(matches!(
            parse(r#"{"type":"queue_position","n":3}"#),
            JobEvent::Other
        ));
        assert!(matches!(
            parse(r#"{"type":"enqueued","job_id":"j","queue":{"position":3}}"#),
            JobEvent::Enqueued {
                host_requirements: None,
                ..
            }
        ));
        assert!(matches!(
            parse(r#"{"type":"enqueued","job_id":"j","host_requirements":{"authorized":1}}"#),
            JobEvent::Enqueued {
                host_requirements: Some(HostCounts {
                    schedulable: None,
                    ..
                }),
                ..
            }
        ));
        assert!(matches!(
            parse(r#"{"type":"assigned","site":"lab"}"#),
            JobEvent::Assigned { host_id: None, .. }
        ));
        assert_eq!(
            job_phase("initializing", Some("fetching_image")),
            "initializing: fetching image"
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
