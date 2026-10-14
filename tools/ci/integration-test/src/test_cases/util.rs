// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use anyhow::{Context, Result, bail};
use std::time::Duration;

/// Time apps take to draw on the screen once they run.
pub const DRAW_TIME: Duration = Duration::from_millis(500);

/// Find the last row of `process` in the process console `list` output in
/// `out`, and return the process's PID and state.
///
/// A row starts with the PID, the short ID and the name of the process, and
/// ends with its state.
fn process_row<'a>(out: &'a str, process: &str) -> Result<(u32, &'a str)> {
    out.lines()
        .rev()
        .find_map(|line| {
            let row: Vec<&str> = line.split_whitespace().collect();
            match row[..] {
                [pid, _, name, .., state] if name == process => Some((pid.parse().ok()?, state)),
                _ => None,
            }
        })
        .with_context(|| format!("process {process} not in a process list in {out:?}"))
}

/// Return the PID of `process` in the last process console `list` output in
/// `out`.
pub fn process_pid(out: &str, process: &str) -> Result<u32> {
    process_row(out, process).map(|(pid, _)| pid)
}

/// Check that `process` is in `state` in the last process console `list`
/// output in `out`.
pub fn check_process_state(out: &str, process: &str, state: &str) -> Result<()> {
    match process_row(out, process)? {
        (_, s) if s == state => Ok(()),
        (_, s) => bail!("expected process {process} to be {state}, got {s}"),
    }
}

#[cfg(test)]
mod tests {
    use super::{check_process_state, process_pid};

    #[test]
    fn finds_the_last_row_of_a_process() {
        let out = "tock$ stop whileone\r\n\
                   Process whileone stopped\r\n\
                   tock$ list\r\n \
                   PID    ShortID    Name                Quanta  Syscalls  Restarts  Grants  State\r\n \
                   0      Unique     whileone                 0         5         0   1/16   Running\r\n\
                   tock$ list\r\n \
                   1      Unique     whileone                 0         5         0   1/16   Stopped(Running)\r\n";
        assert_eq!(process_pid(out, "whileone").unwrap(), 1);
        check_process_state(out, "whileone", "Stopped(Running)").unwrap();
        assert!(process_pid(out, "blink").is_err());
    }
}
