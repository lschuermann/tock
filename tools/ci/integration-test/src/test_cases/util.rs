// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::{Ctx, Requirement};
use crate::boards::GpioMode;
use anyhow::{Context, Result, bail, ensure};
use std::time::Duration;

/// Requirement of a button with the logical name `button0`.
pub const BUTTON0: Requirement = Requirement::Gpio {
    name: "button0",
    mode: GpioMode::DigitalOut,
};
/// Requirement of a button with the logical name `button1`.
pub const BUTTON1: Requirement = Requirement::Gpio {
    name: "button1",
    mode: GpioMode::DigitalOut,
};
/// Requirement of an LED with the logical name `led0`.
pub const LED0: Requirement = Requirement::Gpio {
    name: "led0",
    mode: GpioMode::DigitalIn,
};
/// Requirement of an LED with the logical name `led1`.
pub const LED1: Requirement = Requirement::Gpio {
    name: "led1",
    mode: GpioMode::DigitalIn,
};
/// Requirement of the userspace GPIO pin 0, which the host reads.
pub const GPIO0: Requirement = Requirement::Gpio {
    name: "gpio0",
    mode: GpioMode::DigitalIn,
};

/// Maximum deviation of an observed GPIO edge from its expected time.
pub const EDGE_TOLERANCE: Duration = Duration::from_millis(25);

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

/// Convert `times` to whole milliseconds, for messages.
pub fn millis(times: &[Duration]) -> Vec<u128> {
    times.iter().map(Duration::as_millis).collect()
}

/// Check that `edges` match `expected` one-to-one, within [`EDGE_TOLERANCE`].
pub fn check_edge_times(
    pin: &str,
    edges: &[Duration],
    expected: impl IntoIterator<Item = Duration>,
) -> Result<()> {
    let expected: Vec<Duration> = expected.into_iter().collect();
    let matches = edges.len() == expected.len()
        && (edges.iter().zip(&expected)).all(|(edge, want)| edge.abs_diff(*want) <= EDGE_TOLERANCE);
    log::debug!("  {pin}: edges at {:?} ms", millis(edges));
    ensure!(
        matches,
        "{pin}: expected edges at {:?} ms, observed {:?} ms",
        millis(&expected),
        millis(edges)
    );
    Ok(())
}

/// Make the `mpu_walk_region` app walk past the end of `region` (`flash` or
/// `memory`), and check that this faults.
///
/// The app walks both regions in turn, and overruns a region if `button0` is
/// held while it starts to walk it.
pub fn check_mpu_walk_region_overrun_fault(ctx: &mut Ctx, other: &str, region: &str) -> Result<()> {
    ctx.expect("[TEST] MPU Walk Regions")?;
    // The app reads the button before announcing a walk. Thus, pressing it
    // after the announcement only affects the following walk:
    ctx.expect(&format!("Walking {other}"))?;
    ctx.press("button0")?;
    let walk = ctx.expect("! Will overrun")?;
    if !walk.contains(&format!("Walking {region}")) || walk.contains(&format!("Walking {other}")) {
        bail!("the app did not overrun {region} after walking {other}");
    }
    ctx.expect("mpu_walk_region had a fault")?;
    ctx.expect("---| Cortex-M Fault Status |---")?;
    Ok(())
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
