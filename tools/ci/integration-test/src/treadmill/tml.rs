// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Invocations of the user's `tml` CLI, through which the harness accesses
//! Treadmill.

use crate::logging;
use anyhow::{Context, bail};
use log::{Level, debug, log, trace};
use std::io::{BufRead, BufReader, Read};
use std::process::{Child, ChildStdout, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

/// Exit status of `tml job update` when `--if-revision` no longer holds.
pub const TML_REVISION_MISMATCH: i32 = 3;
/// Exit status of `tml job update` when the job is stopping.
pub const TML_LEASE_REFUSED: i32 = 4;

/// Interval after which [`tml_events`] reports that it is still waiting for an
/// event.
const WAIT_HEARTBEAT: Duration = Duration::from_secs(60);

/// Exit status and error output of a `tml` invocation.
pub struct TmlStatus {
    /// Exit code of the invocation, or `None` if it was killed by a signal.
    pub code: Option<i32>,
    /// Lines which the invocation printed to stderr.
    stderr: Vec<String>,
}

impl TmlStatus {
    /// Check if the invocation succeeded.
    pub fn success(&self) -> bool {
        self.code == Some(0)
    }

    /// Return the reason the invocation failed, as given in its `error: ...`
    /// line, or its exit status if it printed no such line.
    pub fn reason(&self) -> String {
        let said = self
            .stderr
            .iter()
            .rev()
            .find_map(|line| line.strip_prefix("error: "));
        match (said, self.code) {
            (Some(said), _) => said.to_string(),
            (None, Some(code)) => format!("exit status {code}"),
            (None, None) => "killed by a signal".to_string(),
        }
    }
}

/// Start `tml` with the global options `global` and `args`, with its output
/// piped.
fn spawn_tml(global: &[&str], args: &[&str]) -> Child {
    debug!("$ tml {} {}", global.join(" "), args.join(" "));
    Command::new("tml")
        .args(["--color", "never"])
        .args(global)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to run tml, which must be installed and on PATH")
}

/// Run `tml` to completion, and consume its stdout with `read`.
///
/// Each line which `tml` prints to stderr is logged at debug level and
/// collected.
fn run_tml<R>(
    global: &[&str],
    args: &[&str],
    read: impl FnOnce(ChildStdout) -> R,
) -> (TmlStatus, R) {
    let mut child = spawn_tml(global, args);
    let stdout = child.stdout.take().unwrap();
    let stderr = child.stderr.take().unwrap();
    let (stderr, read) = std::thread::scope(|scope| {
        let stderr = scope.spawn(|| {
            BufReader::new(stderr)
                .lines()
                .map_while(Result::ok)
                .inspect(|line| debug!("tml | {line}"))
                .collect()
        });
        let read = read(stdout);
        (stderr.join().unwrap(), read)
    });
    let code = child.wait().expect("failed to wait for tml").code();
    (TmlStatus { code, stderr }, read)
}

/// Run `tml` with `args` to completion, and log its output at debug level.
pub fn tml(args: &[&str]) -> TmlStatus {
    let (status, ()) = run_tml(&[], args, |stdout| {
        for line in BufReader::new(stdout).lines().map_while(Result::ok) {
            debug!("tml | {line}");
        }
    });
    status
}

/// Run `tml -o json` with `args`, and parse its output.
///
/// If `tml` fails, this returns the reason.
pub fn tml_json<T: serde::de::DeserializeOwned>(args: &[&str]) -> anyhow::Result<T> {
    let (status, output) = run_tml(&["-o", "json"], args, |mut stdout| {
        let mut output = String::new();
        stdout.read_to_string(&mut output).ok();
        output
    });
    if !status.success() {
        bail!("{}", status.reason());
    }
    trace!("tml | {output}");
    serde_json::from_str(&output)
        .with_context(|| format!("`tml {}` printed unexpected JSON", args.join(" ")))
}

/// Run `tml -o jsonl` with `args`, and call `on_event` with each event as `tml`
/// prints it.
///
/// After each [`WAIT_HEARTBEAT`] without an event, this calls `on_event` with
/// `None`.
pub fn tml_events<T: serde::de::DeserializeOwned>(
    args: &[&str],
    mut on_event: impl FnMut(Option<T>),
) -> TmlStatus {
    let (status, ()) = run_tml(&["-o", "jsonl"], args, |stdout| {
        std::thread::scope(|scope| {
            let (lines, received) = mpsc::channel();
            scope.spawn(move || {
                for line in BufReader::new(stdout).lines().map_while(Result::ok) {
                    if lines.send(line).is_err() {
                        break;
                    }
                }
            });
            loop {
                match received.recv_timeout(WAIT_HEARTBEAT) {
                    Ok(line) => {
                        trace!("tml | {line}");
                        match serde_json::from_str(&line) {
                            Ok(event) => on_event(Some(event)),
                            Err(e) => debug!(
                                "skipping an event tml printed that did not parse ({e}): {line}"
                            ),
                        }
                    }
                    Err(mpsc::RecvTimeoutError::Timeout) => on_event(None),
                    Err(mpsc::RecvTimeoutError::Disconnected) => break,
                }
            }
        })
    });
    status
}

/// Run a command on a job with `tml` and `args`, log its output in `job | `
/// lines, and return its exit code (or `None` if it was killed by a signal).
///
/// Lines of a remote harness are logged at the level at which the harness
/// logged them. All other lines are logged at info level.
pub fn tml_stream(args: &[&str]) -> Option<i32> {
    let mut child = spawn_tml(&["--quiet"], args);
    let (stdout, stderr) = (child.stdout.take().unwrap(), child.stderr.take().unwrap());
    std::thread::scope(|scope| {
        scope.spawn(|| forward_remote(stderr));
        forward_remote(stdout);
    });
    child.wait().expect("failed to wait for tml").code()
}

/// Log each line read from `from` in a `job | ` line.
///
/// A line which the remote harness logged is logged at the same level. All
/// other lines are logged at info level.
fn forward_remote(from: impl Read) {
    let mut level = Level::Info;
    for line in BufReader::new(from).lines().map_while(Result::ok) {
        let message = match logging::split_tag(&line) {
            Some((Some(tagged), message)) => {
                level = tagged;
                message
            }
            // Log a continuation of the previous message at the same level,
            // and let this logger align it:
            Some((None, message)) => message,
            None => {
                level = Level::Info;
                &line
            }
        };
        log!(level, "job | {message}");
    }
}
