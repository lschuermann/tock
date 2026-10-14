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
use crate::boards::{Board, Dut, Feature, Gpio, GpioMode, Timing, Uart};
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
/// Time for the device to react to a change of a GPIO input.
pub const SETTLE: Duration = Duration::from_millis(50);
/// Interval at which GPIOs are polled.
const POLL_INTERVAL: Duration = Duration::from_millis(1);

#[derive(Clone, Copy)]
pub enum Step {
    /// Wait for the given text on the console.
    Expect(&'static str),
    /// Type the given text on the console.
    Send(&'static str),
    /// Check the console output received by `Expect` steps since the last
    /// `Check`, `Discard` or `Reset`.
    Check(fn(&str, &Board) -> Result<()>),
    /// Discard the console output received by `Expect` steps since the last
    /// `Check`, `Discard` or `Reset`.
    Discard,
    /// Check that the SHA-256 hash of the board's screen, as a PPM image,
    /// equals the hash given for the board.
    ///
    /// The image is saved next to the test's console log, such that it can be
    /// inspected and its hash recorded.
    Screen(&'static [(&'static Board, &'static str)]),
    /// Restart the board, keeping what was flashed.
    Reset,
    /// Sleep for the given duration.
    Sleep(Duration),
    /// Run a sequence of steps in order.
    Steps(&'static [Step]),
    /// Run a step the given number of times.
    Repeat(u32, &'static Step),
    /// Run procedural code, which may charge up to the given budget.
    Within(Duration, fn(&mut Ctx) -> Result<()>),
}

impl Step {
    /// Return the longest time this step may take on a board with `timing`.
    pub fn cost(&self, timing: &Timing) -> Duration {
        match *self {
            Step::Expect(_) => timing.expect,
            Step::Send(text) => typing_time(text),
            Step::Check(_) | Step::Discard | Step::Screen(_) => Duration::ZERO,
            Step::Reset => timing.restart + timing.boot,
            Step::Sleep(duration) | Step::Within(duration, _) => duration,
            Step::Steps(steps) => steps.iter().map(|step| step.cost(timing)).sum(),
            Step::Repeat(n, step) => step.cost(timing) * n,
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
        retrying: false,
        infra: None,
    };
    let steps = |ctx: &mut Ctx| case.steps.iter().try_for_each(|step| ctx.step(step));
    let result = ctx.release_all().and_then(|()| steps(&mut ctx));
    match (ctx.infra, result) {
        (Some(reason), _) => TestOutcome::InfraFailed { reason },
        (None, Err(e)) => TestOutcome::Failed {
            reason: format!("{e:#}"),
        },
        (None, Ok(())) => TestOutcome::Passed,
    }
}

/// State of a running test, which is passed to the code of [`Step::Within`].
pub struct Ctx<'a> {
    dut: &'a mut dyn Dut,
    requires: &'static [Requirement],
    timing: &'static Timing,
    /// Console output received by `Expect` steps since the last `Check` or
    /// `Discard`.
    output: String,
    /// Boot time which the test has not waited for yet. This time is added to
    /// the timeout of the next wait.
    boot: Duration,
    /// Budget remaining for the running `Within` step or [`Ctx::retry`], if
    /// any.
    budget: Option<Duration>,
    /// Path to save the board's screen to.
    screen: PathBuf,
    /// Whether [`Ctx::retry`] is running, which reports an exhausted budget as
    /// a timeout of the retried action.
    retrying: bool,
    /// Reason for an infrastructure failure during the test, if any.
    infra: Option<String>,
}

impl Ctx<'_> {
    fn step(&mut self, step: &Step) -> Result<()> {
        match *step {
            Step::Expect(needle) => self.expect(needle).map(drop),
            Step::Send(text) => self.send(text),
            Step::Check(check) => check(&mem::take(&mut self.output), self.board()),
            Step::Discard => {
                self.output.clear();
                Ok(())
            }
            Step::Screen(hashes) => self.check_screen(hashes),
            Step::Reset => self.restart(),
            Step::Sleep(duration) => self.sleep(duration),
            Step::Steps(steps) => steps.iter().try_for_each(|step| self.step(step)),
            Step::Repeat(n, step) => (0..n).try_for_each(|_| self.step(step)),
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
            None if self.retrying => bail!("did not succeed within its time"),
            None => bail!(
                "Within budget too short on {}: needs {time:?} more, {budget:?} left",
                self.dut.board()
            ),
        }
    }

    /// Restart the board, keeping what was flashed.
    ///
    /// A failing restart, or one which takes longer than [`Timing::restart`],
    /// is an infrastructure failure.
    fn restart(&mut self) -> Result<()> {
        let start = Instant::now();
        let restarted = (self.dut.restart())
            .and_then(|()| check_bound("restarting", start, self.timing.restart));
        if let Err(e) = restarted {
            self.infra = Some(format!("{e:#}"));
            return Err(e);
        }
        self.output.clear();
        self.boot = self.timing.boot;
        Ok(())
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

    /// Call `attempt` until it returns `true`, for up to `time`.
    ///
    /// Right after the board was flashed or reset, the time also includes the
    /// boot time of the board.
    pub fn retry(
        &mut self,
        time: Duration,
        mut attempt: impl FnMut(&mut Ctx) -> Result<bool>,
    ) -> Result<()> {
        self.charge(time)?;
        let time = time + mem::take(&mut self.boot);
        let outer = self.budget.replace(time);
        self.retrying = true;
        let start = Instant::now();
        let result = loop {
            match attempt(self) {
                Ok(false) if start.elapsed() < time => {}
                Ok(false) => break Err(anyhow!("did not succeed within {time:?}")),
                done => break done.map(drop),
            }
        };
        self.retrying = false;
        self.budget = outer;
        result
    }

    /// Return the GPIO with the logical name `name`, which the test must
    /// require.
    fn gpio(&mut self, name: &str) -> Result<&mut dyn Gpio> {
        let required = (self.requires.iter())
            .any(|r| matches!(r, Requirement::Gpio { name: n, .. } if *n == name));
        ensure!(required, "test used GPIO {name:?} without requiring it");
        (self.dut.gpio(name)).with_context(|| format!("board has no GPIO {name:?}"))
    }

    /// Configure every GPIO the test requires as an input, which releases all
    /// buttons.
    fn release_all(&mut self) -> Result<()> {
        for requirement in self.requires {
            if let Requirement::Gpio { name, .. } = requirement {
                self.release(name)?;
            }
        }
        Ok(())
    }

    /// Read the level of `pin`.
    pub fn read(&mut self, pin: &str) -> Result<bool> {
        self.gpio(pin)?.read()
    }

    /// Read the levels of all `pins`.
    pub fn read_all<const N: usize>(&mut self, pins: [&str; N]) -> Result<[bool; N]> {
        let mut levels = [false; N];
        for (level, pin) in levels.iter_mut().zip(pins) {
            *level = self.read(pin)?;
        }
        Ok(levels)
    }

    /// Press `button` by driving its GPIO to its active level.
    pub fn press(&mut self, button: &str) -> Result<()> {
        info!("  pressing {button}");
        let gpio = self.gpio(button)?;
        let active = gpio.active_high();
        gpio.set_mode(GpioMode::DigitalOut)?;
        gpio.write(active)
    }

    /// Release `button` by configuring its GPIO as an input.
    ///
    /// The device's pull resistor then returns the GPIO to its inactive level.
    /// Thus, the host never drives an active-low GPIO high, as the device may
    /// use a lower I/O voltage than the host.
    pub fn release(&mut self, button: &str) -> Result<()> {
        self.gpio(button)?.set_mode(GpioMode::DigitalIn)
    }

    /// Press and release `button`, and let the device react to each.
    pub fn click(&mut self, button: &str) -> Result<()> {
        self.press(button)?;
        self.sleep(SETTLE)?;
        self.release(button)?;
        self.sleep(SETTLE)
    }

    /// Wait up to `timeout` for `pin` to change its level, and return the time
    /// this took.
    ///
    /// Right after the board was flashed or reset, the timeout also includes
    /// the boot time of the board.
    pub fn wait_edge(&mut self, pin: &str, timeout: Duration) -> Result<Duration> {
        self.charge(timeout)?;
        let timeout = timeout + mem::take(&mut self.boot);
        let start = Instant::now();
        let initial = self.read(pin)?;
        while self.read(pin)? == initial {
            if start.elapsed() > timeout {
                bail!("{pin} did not change within {timeout:?}");
            }
            std::thread::sleep(POLL_INTERVAL);
        }
        Ok(start.elapsed())
    }

    /// Sample `pins` for `duration`, and return the times (relative to the
    /// start) at which each pin changed its level.
    pub fn edges<const N: usize>(
        &mut self,
        pins: [&str; N],
        duration: Duration,
    ) -> Result<[Vec<Duration>; N]> {
        self.edges_until(pins, duration, |_| false)
    }

    /// Sample `pins` like [`Ctx::edges`], and stop early once `done` returns
    /// `true`.
    pub fn edges_until<const N: usize>(
        &mut self,
        pins: [&str; N],
        duration: Duration,
        done: impl Fn(&[Vec<Duration>; N]) -> bool,
    ) -> Result<[Vec<Duration>; N]> {
        self.charge(duration)?;
        info!(
            "  recording edges on {} for up to {duration:?}",
            pins.join(", ")
        );
        let mut last = self.read_all(pins)?;
        let mut edges = [const { Vec::new() }; N];
        let start = Instant::now();
        while start.elapsed() < duration && !done(&edges) {
            let levels = self.read_all(pins)?;
            let t = start.elapsed();
            for i in (0..N).filter(|&i| levels[i] != last[i]) {
                edges[i].push(t);
            }
            last = levels;
            std::thread::sleep(POLL_INTERVAL);
        }
        Ok(edges)
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
        fn restart(&mut self) -> Result<()> {
            Ok(())
        }
        fn uart(&mut self) -> Option<&mut dyn Uart> {
            Some(self)
        }
        fn gpio(&mut self, _: &str) -> Option<&mut dyn Gpio> {
            None
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
        const STEPS: Step = Steps(&[
            Expect("a"),
            Send("ab"),
            Check(|_, _| Ok(())),
            Sleep(Duration::from_secs(1)),
            Repeat(3, &Steps(&[Reset, Expect("b")])),
            Within(Duration::from_secs(7), |_| Ok(())),
        ]);
        let t = timing();
        let reset = t.restart + t.boot + t.expect;
        assert_eq!(
            STEPS.cost(t),
            t.expect + Duration::from_millis(20) + 3 * reset + Duration::from_secs(8)
        );
    }

    #[test]
    fn check_sees_the_output_since_the_last_check_discard_or_reset() {
        const STEPS: &[Step] = &[
            Expect("a"),
            Check(|out, _| (out == "a").then_some(()).ok_or_else(|| anyhow!("{out}"))),
            Expect("b"),
            Discard,
            Expect("c"),
            Reset,
            Expect("d"),
            Check(|out, _| (out == "d").then_some(()).ok_or_else(|| anyhow!("{out}"))),
        ];
        assert_eq!(
            run_on("abcd", &[Requirement::Uart], STEPS),
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
    fn retry_stops_at_its_time() {
        // Wait for "a" first, such that the retry does not include the boot
        // time:
        const STEPS: &[Step] = &[
            Expect("a"),
            Within(Duration::from_secs(1), |ctx| {
                ctx.retry(Duration::from_millis(30), |ctx| {
                    ctx.sleep(SETTLE).map(|()| false)
                })
            }),
        ];
        assert_eq!(
            run_on("a", &[Requirement::Uart], STEPS),
            failed("did not succeed within its time")
        );
    }

    #[test]
    fn undeclared_resources_fail() {
        assert_eq!(
            run_on("a", &[], &[Expect("a")]),
            failed("test used the console without requiring `Uart`")
        );
    }
}
