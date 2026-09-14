// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{BoardKind, GpioMode};
use crate::hostspec::DutSpec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    Uart,
    Gpio { name: &'static str, mode: GpioMode },
}

impl Requirement {
    /// A short description of what this requires of a `board` DUT.
    pub fn describe(self, board: BoardKind) -> String {
        match self {
            Requirement::Uart => "console".to_string(),
            Requirement::Gpio { name, mode } => match board.pin(name) {
                Some(pin) => format!("{name} at {pin} ({})", mode.as_str()),
                None => format!("{name} (not mapped for {board})"),
            },
        }
    }

    /// Whether a host-spec DUT (of `board`, with its pins' controllers
    /// resolved) satisfies this requirement: the same condition as
    /// [`cel`](Self::cel), for a host the harness has the spec of.
    pub fn satisfied_by(self, board: BoardKind, dut: &DutSpec) -> bool {
        match self {
            Requirement::Uart => dut.console.is_some(),
            Requirement::Gpio { name, mode } => board
                .pin(name)
                .and_then(|pin| dut.gpio.get(pin))
                .is_some_and(|pin| {
                    pin.modes.iter().any(|m| m == mode.as_str())
                        && pin
                            .controller_spec
                            .as_ref()
                            .is_some_and(|c| c.driver == "linux-gpiochip")
                }),
        }
    }

    /// A CEL condition on a host-spec DUT `d` (of `board`, on host `host`)
    /// that holds when that DUT satisfies this requirement.
    pub fn cel(self, board: BoardKind) -> String {
        match self {
            Requirement::Uart => "has(d.console)".to_string(),
            Requirement::Gpio { name, mode } => match board.pin(name) {
                // Matches what `boards::open_gpio` can drive.
                Some(pin) => format!(
                    "{pin:?} in d.gpio && {:?} in d.gpio[{pin:?}].modes && \
                     host.gpio_controllers[d.gpio[{pin:?}].controller].driver == \"linux-gpiochip\"",
                    mode.as_str()
                ),
                None => "false".to_string(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hostspec;

    /// An nRF52840DK DUT with a console, whose `button0` (`P0.11`) supports
    /// `digital_out` on a `linux-gpiochip` controller, `button1` (`P0.12`)
    /// only on another driver, and `led0` (`P0.13`) only `digital_in`.
    fn dut() -> DutSpec {
        let spec = hostspec::from_value(serde_json::json!({
            "spec_version": "v2",
            "gpio_controllers": {
                "rp1": {"driver": "linux-gpiochip", "config": {}},
                "exp": {"driver": "some-future-driver", "config": {}}
            },
            "duts": [{
                "board": "nrf52840dk",
                "console": {"kind": "uart", "device": "/dev/ttyACM0", "baud": 115200},
                "gpio": {
                    "P0.11": {"controller": "rp1", "modes": ["digital_out"]},
                    "P0.12": {"controller": "exp", "modes": ["digital_out"]},
                    "P0.13": {"controller": "rp1", "modes": ["digital_in"]}
                }
            }]
        }))
        .unwrap();
        spec.duts[0].clone()
    }

    #[test]
    fn duts_satisfy_what_the_cel_condition_admits() {
        let board = BoardKind::Nrf52840Dk;
        let gpio = |name, mode| Requirement::Gpio { name, mode };
        let dut = dut();
        assert!(Requirement::Uart.satisfied_by(board, &dut));
        assert!(gpio("button0", GpioMode::DigitalOut).satisfied_by(board, &dut));
        assert!(gpio("led0", GpioMode::DigitalIn).satisfied_by(board, &dut));
        // On a controller the harness cannot drive.
        assert!(!gpio("button1", GpioMode::DigitalOut).satisfied_by(board, &dut));
        // Not in a mode the host supports.
        assert!(!gpio("led0", GpioMode::DigitalOut).satisfied_by(board, &dut));
        // Not wired, and not mapped by the backend at all.
        assert!(!gpio("led1", GpioMode::DigitalIn).satisfied_by(board, &dut));
        assert!(!gpio("no-such-gpio", GpioMode::DigitalIn).satisfied_by(board, &dut));

        let mut bare = dut;
        bare.console = None;
        assert!(!Requirement::Uart.satisfied_by(board, &bare));
    }
}
