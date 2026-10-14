// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

mod step;
mod util;

pub use step::{Ctx, Step, TestOutcome, run};

use crate::boards::{BOARDS, Board, Feature, GpioMode};

/// Collection of tests run by this end-to-end harness.
///
/// Implemented through a [`distributed_slice`](linkme::distributed_slice), such
/// that each test module registers its own test and we don't need a central
/// registry.
#[linkme::distributed_slice]
pub static TESTS: [TestCase];

mod app_stack_overflow_then_kernel_hardfault;
mod blink;
mod blink_c_hello_buttons;
mod button_print;
mod buttons;
mod c_hello_and_printf_long;
mod console;
mod console_timeout;
mod exit;
mod gpio_original;
mod hello_world;
mod hello_world_process_console;
mod ipc_rot13;
mod led_odd;
mod led_odd_tock_logo;
mod lua_hello;
mod malloc_test01;
mod malloc_test02;
mod mpu_walk_region_flash;
mod mpu_walk_region_memory;
mod multi_alarm_test;
mod process_console_help;
mod process_console_list;
mod process_console_restart;
mod process_console_stop_start;
mod scheduler_whileone_blink;
mod sensors;
mod stack_size_test01;
mod stack_size_test02;
mod tock_logo;
mod tutorial_ipc_rng_led;
mod unexpected_rx;

/// Resource a test requires a board to provide.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    Uart,
    /// GPIO with the logical name `name` (see [`Board::pins`]), which the
    /// host uses in `mode`.
    Gpio {
        name: &'static str,
        mode: GpioMode,
    },
    Feature(Feature),
}

impl Requirement {
    /// Return a short description of what this requirement requires of
    /// `board`.
    pub fn describe(&self, board: &Board) -> String {
        match *self {
            Requirement::Uart => "console".to_string(),
            Requirement::Gpio { name, mode } => match board.pin(name) {
                Some(pin) => format!("{name} at {pin} ({})", mode.as_str()),
                None => format!("{name} (not mapped for {board})"),
            },
            Requirement::Feature(feature) => format!("{feature:?}"),
        }
    }
}

/// A test, which is planned on every board that provides all of its
/// requirements.
pub struct TestCase {
    /// Unique name of the test.
    pub id: &'static str,
    /// Requirements on the DUTs that this test case can operate on.
    pub requires: &'static [Requirement],
    /// libtock-c apps to install, as paths under libtock-c's `examples/`.
    pub apps: &'static [&'static str],
    /// Boards on which the test is unsupported despite providing all of its
    /// requirements (e.g., because of a known regression), along with the
    /// reason for each.
    pub unsupported: &'static [(&'static Board, &'static str)],
    /// Steps of the test, which are executed in order.
    pub steps: &'static [Step],
}

impl TestCase {
    /// Look up a test in [`TESTS`] by its id.
    ///
    /// Returns an error if no test with this id exists.
    pub fn from_id(id: &str) -> anyhow::Result<&'static TestCase> {
        (TESTS.iter())
            .find(|t| t.id == id)
            .ok_or_else(|| anyhow::anyhow!("unknown test {id:?}"))
    }

    /// Return the boards which provide all requirements of this test.
    pub fn boards(&self) -> impl Iterator<Item = &'static Board> + '_ {
        (BOARDS.iter()).filter(|board| self.requires.iter().all(|r| board.provides(*r)))
    }

    /// Return the longest time the test may take on `board`. This includes
    /// flashing and booting the board.
    pub fn worst_case(&self, board: &Board) -> std::time::Duration {
        let timing = &board.timing;
        let steps: std::time::Duration = self.steps.iter().map(|s| s.cost(timing)).sum();
        timing.flash + timing.boot + steps
    }

    /// Return a description of what this test requires of `board`, which lists
    /// each requirement once.
    pub fn describe_requirements(&self, board: &Board) -> String {
        let mut needs: Vec<String> = self.requires.iter().map(|r| r.describe(board)).collect();
        needs.sort();
        needs.dedup();
        needs.join(", ")
    }

    /// Return the reason this test is unsupported on `board`, if it is.
    pub fn unsupported_reason(&self, board: &Board) -> Option<&'static str> {
        (self.unsupported.iter())
            .find(|(b, _)| *b == board)
            .map(|(_, reason)| *reason)
    }
}

/// Serialization of a `&'static TestCase` as its [`id`](TestCase::id), for use
/// with `#[serde(with = "crate::test_cases::by_id")]`.
pub mod by_id {
    use super::TestCase;
    use serde::{Deserialize, Deserializer, Serializer};
    use std::borrow::Cow;

    pub fn serialize<S: Serializer>(test: &&'static TestCase, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(test.id)
    }

    /// Deserialize a test id and look up the test in [`TESTS`](super::TESTS).
    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<&'static TestCase, D::Error> {
        let id = Cow::<'de, str>::deserialize(d)?;
        TestCase::from_id(&id).map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::{BOARDS, Requirement, Step, TESTS};
    use crate::boards::qemu;

    #[test]
    fn every_test_file_is_registered() {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/src/test_cases");
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            let name = path.file_stem().unwrap().to_str().unwrap();
            if !["mod", "step", "util"].contains(&name) {
                assert!(
                    TESTS.iter().any(|t| t.id == name),
                    "{name}.rs registers no test with id {name:?}"
                );
            }
        }
    }

    #[test]
    fn boards_follow_from_requirements() {
        let boards = |id: &str| -> Vec<&str> {
            let tc = TESTS.iter().find(|t| t.id == id).unwrap();
            let mut names: Vec<&str> = tc.boards().map(|b| b.name).collect();
            names.sort();
            names
        };
        assert_eq!(boards("led_odd"), ["qemu_rv64_virt"]);
        assert_eq!(
            boards("button_print"),
            ["nrf52840dk", "nucleo_f429zi", "qemu_rv64_virt"]
        );
        // This test requires `led0` and `led1`, which only these boards map:
        assert_eq!(
            boards("scheduler_whileone_blink"),
            ["nrf52840dk", "nucleo_f429zi"]
        );
        assert_eq!(
            boards("app_stack_overflow_then_kernel_hardfault"),
            ["nucleo_f429zi"]
        );
        assert_eq!(boards("hello_world").len(), BOARDS.len());
    }

    #[test]
    fn test_ids_are_unique() {
        let mut ids: Vec<_> = TESTS.iter().map(|t| t.id).collect();
        ids.sort();
        let len = ids.len();
        ids.dedup();
        assert_eq!(ids.len(), len, "a test id is used twice");
    }

    /// Check that steps which use the console require `Uart`. `Within` steps
    /// check the resources they use while they run.
    #[test]
    fn console_steps_require_uart() {
        fn uses_console(step: &Step) -> bool {
            match *step {
                Step::Expect(_) | Step::Send(_) | Step::Check(_) => true,
                Step::Discard | Step::Screen(_) | Step::Sleep(_) | Step::Within(..) => false,
            }
        }
        for tc in TESTS {
            if tc.steps.iter().any(uses_console) {
                assert!(tc.requires.contains(&Requirement::Uart), "{}", tc.id);
            }
        }
    }

    #[test]
    fn unsupported_boards_are_boards_of_the_test() {
        for tc in TESTS {
            for (board, _) in tc.unsupported {
                assert!(tc.boards().any(|b| b == *board), "{}", tc.id);
            }
        }
    }

    #[test]
    fn worst_cases_include_flashing_and_booting() {
        let hello = TESTS.iter().find(|t| t.id == "hello_world").unwrap();
        let timing = &qemu::RV64_VIRT.timing;
        assert_eq!(
            hello.worst_case(&qemu::RV64_VIRT),
            timing.flash + timing.boot + timing.expect
        );
    }
}
