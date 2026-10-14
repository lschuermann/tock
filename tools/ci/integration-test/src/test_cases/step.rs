// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Test steps and their execution.
//!
//! A test consists of [`Step`]s, which are executed in order. The cost of a
//! step ([`Step::cost`]) is the longest time it may take on a board when all of
//! its waits time out. It is computed from the board's [`Timing`].
//!
//! [`Step::Within`] runs procedural code, which declares its cost as a fixed
//! budget. Each primitive the code calls charges its cost on the board against
//! this budget before it runs, and fails the test if the budget does not cover
//! it. The charged cost only depends on the board and the path the code takes.
//! Thus, a budget which is too short fails every run on a board that takes this
//! path, independent of the speed of the device.

use super::{Requirement, TestCase};
use crate::boards::{Board, Dut, Feature, Timing, Uart};
use anyhow::{Context, Result, anyhow, bail, ensure};
use fs_err as fs;
use log::info;
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::mem;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Delay after each byte typed on the console. Consoles may drop input which
/// arrives faster than they process it.
const TYPING_DELAY: Duration = Duration::from_millis(10);

#[derive(Clone, Copy)]
pub enum Step {
    /// Wait for the given text on the console.
    Expect(&'static str),
    /// Type the given text on the console.
    Send(&'static str),
    /// Check the console output received by `Expect` steps since the last
    /// `Check`.
    Check(fn(&str, &Board) -> Result<()>),
    /// Check that the SHA-256 hash of the board's screen, as a PPM image,
    /// equals the hash given for the board.
    ///
    /// The image is saved next to the test's console log, such that it can be
    /// inspected and its hash recorded.
    Screen(&'static [(&'static Board, &'static str)]),
    /// Sleep for the given duration.
    Sleep(Duration),
    /// Run procedural code, which may charge up to the given budget.
    Within(Duration, fn(&mut Ctx) -> Result<()>),
}

impl Step {
    /// Return the longest time this step may take on a board with `timing`.
    pub fn cost(&self, timing: &Timing) -> Duration {
        match *self {
            Step::Expect(_) => timing.expect,
            Step::Send(text) => typing_time(text),
            Step::Check(_) | Step::Screen(_) => Duration::ZERO,
            Step::Sleep(duration) | Step::Within(duration, _) => duration,
        }
    }
}

/// Return the time it takes to type `text` on the console.
fn typing_time(text: &str) -> Duration {
    TYPING_DELAY * text.len() as u32
}

/// Return an error if the operation `what`, which started at `start`, took
/// longer than `bound`.
fn check_bound(what: &str, start: Instant, bound: Duration) -> Result<()> {
    let took = start.elapsed();
    ensure!(
        took <= bound,
        "{what} took {took:.1?}, longer than its {bound:?}"
    );
    Ok(())
}

/// Outcome of running a test.
#[derive(Debug, PartialEq, Serialize)]
#[serde(tag = "status")]
pub enum TestOutcome {
    Passed,
    /// The test failed with `reason`.
    Failed {
        reason: String,
    },
    /// The board or the test infrastructure failed with `reason` (e.g., when
    /// flashing exceeded [`Timing::flash`]).
    InfraFailed {
        reason: String,
    },
    /// The test was skipped for `reason`.
    Skipped {
        reason: String,
    },
}

impl TestOutcome {
    /// Check if the test or the infrastructure failed.
    pub fn failed(&self) -> bool {
        matches!(
            self,
            TestOutcome::Failed { .. } | TestOutcome::InfraFailed { .. }
        )
    }
}

/// Flash `kernel` and `apps` onto `dut` and run the steps of `case`.
///
/// Flashing failures are reported as infrastructure failures. Screens captured
/// by the test are saved to `log_dir`.
pub fn run(
    dut: &mut dyn Dut,
    case: &TestCase,
    kernel: &Path,
    apps: &[&Path],
    log_dir: &Path,
) -> TestOutcome {
    let timing = &dut.board().timing;
    let start = Instant::now();

    info!("  flashing...");
    let flashed =
        (dut.flash(kernel, apps)).and_then(|()| check_bound("flashing", start, timing.flash));
    if let Err(e) = flashed {
        let reason = format!("{e:#}");
        return TestOutcome::InfraFailed { reason };
    }

    info!("  running test steps...");
    let screen = log_dir.join(format!("{}@{}.ppm", case.id, dut.board()));
    let mut ctx = Ctx {
        dut,
        requires: case.requires,
        timing,
        output: String::new(),
        boot: timing.boot,
        budget: None,
        screen,
    };
    match case.steps.iter().try_for_each(|step| ctx.step(step)) {
        Ok(()) => TestOutcome::Passed,
        Err(e) => TestOutcome::Failed {
            reason: format!("{e:#}"),
        },
    }
}

/// State of a running test, which is passed to the code of [`Step::Within`].
pub struct Ctx<'a> {
    dut: &'a mut dyn Dut,
    requires: &'static [Requirement],
    timing: &'static Timing,
    /// Console output received by `Expect` steps since the last `Check`.
    output: String,
    /// Boot time which the test has not waited for yet. This time is added to
    /// the timeout of the next wait.
    boot: Duration,
    /// Budget remaining for the running `Within` step, if any.
    budget: Option<Duration>,
    /// Path to save the board's screen to.
    screen: PathBuf,
}

impl Ctx<'_> {
    fn step(&mut self, step: &Step) -> Result<()> {
        match *step {
            Step::Expect(needle) => self.expect(needle).map(drop),
            Step::Send(text) => self.send(text),
            Step::Check(check) => check(&mem::take(&mut self.output), self.board()),
            Step::Screen(hashes) => self.check_screen(hashes),
            Step::Sleep(duration) => self.sleep(duration),
            Step::Within(budget, run) => {
                self.budget = Some(budget);
                let result = run(self);
                self.budget = None;
                result
            }
        }
    }

    /// Charge `time` against the budget of the running `Within` step.
    ///
    /// Outside of a `Within` step, costs are known statically and nothing is
    /// charged.
    fn charge(&mut self, time: Duration) -> Result<()> {
        let Some(budget) = &mut self.budget else {
            return Ok(());
        };
        match budget.checked_sub(time) {
            Some(left) => {
                *budget = left;
                Ok(())
            }
            None => bail!(
                "Within budget too short on {}: needs {time:?} more, {budget:?} left",
                self.dut.board()
            ),
        }
    }

    /// Return the board the test runs on.
    pub fn board(&self) -> &'static Board {
        self.dut.board()
    }

    fn uart(&mut self) -> Result<&mut dyn Uart> {
        ensure!(
            self.requires.contains(&Requirement::Uart),
            "test used the console without requiring `Uart`"
        );
        (self.dut.uart()).ok_or_else(|| anyhow!("board has no console"))
    }

    /// Wait for `needle` on the console. This returns the output up to and
    /// including `needle`.
    pub fn expect(&mut self, needle: &str) -> Result<String> {
        self.charge(self.timing.expect)?;
        let timeout = self.timing.expect + mem::take(&mut self.boot);
        let out = self.uart()?.wait_for(needle, timeout)?;
        self.output.push_str(&out);
        Ok(out)
    }

    /// Check that the SHA-256 hash of the board's screen equals the entry for
    /// the board in `hashes`, and save the screen.
    fn check_screen(&mut self, hashes: &[(&Board, &str)]) -> Result<()> {
        ensure!(
            (self.requires).contains(&Requirement::Feature(Feature::Display)),
            "test checked the screen without requiring `Display`"
        );
        info!("  checking the screen");
        let image = self.dut.screen()?;
        let path = &self.screen;
        fs::write(path, &image).context("failed to save the screen")?;
        let hash = format!("{:x}", Sha256::digest(&image));
        match hashes.iter().find(|(board, _)| *board == self.board()) {
            Some((_, expected)) if *expected == hash => Ok(()),
            Some((_, expected)) => bail!(
                "the screen ({}) has SHA-256 {hash}, expected {expected}",
                path.display()
            ),
            None => bail!(
                "no screen hash for {}, the screen ({}) has SHA-256 {hash}",
                self.board(),
                path.display()
            ),
        }
    }

    /// Type `text` on the console.
    pub fn send(&mut self, text: &str) -> Result<()> {
        self.charge(typing_time(text))?;
        self.uart()?.write_slowly(text.as_bytes(), TYPING_DELAY)
    }

    /// Sleep for `duration`.
    pub fn sleep(&mut self, duration: Duration) -> Result<()> {
        self.charge(duration)?;
        std::thread::sleep(duration);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Step::*;
    use super::*;
    use crate::boards::helpers::console_buffer::ConsoleBuffer;
    use crate::boards::{Board, qemu};

    /// Mock board with `output` pending on its console.
    struct Console {
        output: ConsoleBuffer,
    }

    impl Uart for Console {
        fn send(&mut self, _: &[u8]) -> Result<()> {
            Ok(())
        }
        fn receive_until(&mut self, needle: &str, _: Duration) -> Result<String> {
            (self.output.take_through(needle)).ok_or_else(|| anyhow!("no {needle:?}"))
        }
        fn transcript(&mut self) -> &[u8] {
            self.output.transcript()
        }
    }

    impl Dut for Console {
        fn board(&self) -> &'static Board {
            &qemu::RV64_VIRT
        }
        fn flash(&mut self, _: &Path, _: &[&Path]) -> Result<()> {
            Ok(())
        }
        fn reset(&mut self) -> Result<()> {
            Ok(())
        }
        fn uart(&mut self) -> Option<&mut dyn Uart> {
            Some(self)
        }
        fn screen(&mut self) -> Result<Vec<u8>> {
            Ok(b"P6 1 1 255 abc".to_vec())
        }
    }

    fn timing() -> &'static Timing {
        &qemu::RV64_VIRT.timing
    }

    fn run_on(
        output: &str,
        requires: &'static [Requirement],
        steps: &'static [Step],
    ) -> TestOutcome {
        let case = TestCase {
            id: "test",
            requires,
            apps: &[],
            unsupported: &[],
            steps,
        };
        let mut console = Console {
            output: ConsoleBuffer::new(),
        };
        console.output.push(output.as_bytes());
        let logs = tempfile::tempdir().unwrap();
        run(&mut console, &case, Path::new("kernel"), &[], logs.path())
    }

    fn failed(reason: &str) -> TestOutcome {
        TestOutcome::Failed {
            reason: reason.into(),
        }
    }

    #[test]
    fn expects_wait_for_their_text() {
        let requires = &[Requirement::Uart];
        assert_eq!(Expect("a").cost(timing()), timing().expect);
        assert_eq!(run_on("xay", requires, &[Expect("a")]), TestOutcome::Passed);
        assert_eq!(run_on("b", requires, &[Expect("a")]), failed("no \"a\""));
    }

    #[test]
    fn costs_add_up() {
        const STEPS: &[Step] = &[
            Expect("a"),
            Send("ab"),
            Check(|_, _| Ok(())),
            Sleep(Duration::from_secs(1)),
            Expect("b"),
            Within(Duration::from_secs(7), |_| Ok(())),
        ];
        let t = timing();
        assert_eq!(
            STEPS.iter().map(|step| step.cost(t)).sum::<Duration>(),
            2 * t.expect + Duration::from_millis(20) + Duration::from_secs(8)
        );
    }

    #[test]
    fn check_sees_the_output_since_the_last_check() {
        const STEPS: &[Step] = &[
            Expect("a"),
            Check(|out, _| (out == "a").then_some(()).ok_or_else(|| anyhow!("{out}"))),
            Expect("b"),
            Expect("c"),
            Check(|out, _| (out == "bc").then_some(()).ok_or_else(|| anyhow!("{out}"))),
        ];
        assert_eq!(
            run_on("abc", &[Requirement::Uart], STEPS),
            TestOutcome::Passed
        );
    }

    #[test]
    fn within_charges_its_budget() {
        const STEPS: &[Step] = &[Within(Duration::from_secs(4), |ctx| {
            ctx.send("ab")?;
            ctx.expect("a")?;
            ctx.expect("b").map(drop)
        })];
        let expect = timing().expect;
        let left = Duration::from_secs(4) - Duration::from_millis(20) - expect;
        assert_eq!(
            run_on("ab", &[Requirement::Uart], STEPS),
            failed(&format!(
                "Within budget too short on qemu_rv64_virt: needs {expect:?} more, {left:?} left"
            ))
        );
    }

    #[test]
    fn screens_match_their_board_hash() {
        const DISPLAY: &[Requirement] = &[Requirement::Feature(Feature::Display)];
        // SHA-256 hash of the screen of the mock board:
        const HASH: &str = "6ef8e159c1ceb016f798e252c1146425e54f6f2b940e92713a9afa9493165dfc";
        const MATCHING: &[Step] = &[Screen(&[(&qemu::RV64_VIRT, HASH)])];
        assert_eq!(run_on("", DISPLAY, MATCHING), TestOutcome::Passed);
        let TestOutcome::Failed { reason } = run_on("", DISPLAY, &[Screen(&[])]) else {
            panic!("passed without a hash for the board");
        };
        assert!(reason.ends_with(&format!("has SHA-256 {HASH}")), "{reason}");
    }

    #[test]
    fn undeclared_resources_fail() {
        assert_eq!(
            run_on("a", &[], &[Expect("a")]),
            failed("test used the console without requiring `Uart`")
        );
    }
}
