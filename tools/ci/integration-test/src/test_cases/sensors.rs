// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Check, Expect};
use super::{Requirement, TESTS, TestCase};
use crate::boards::{Board, nrf52, nucleo_f429zi, qemu, rp2xxx};
use anyhow::{Context, Result, ensure};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "sensors",
    requires: &[Requirement::Uart],
    apps: &["sensors"],
    unsupported: &[],
    // The app lists the sensors it samples, then prints a round of readings
    // every second, each ending with an empty line.
    steps: &[
        Expect("[Sensors] Starting Sensors App."),
        Expect("will be sampled."),
        Expect("\n\n"),
        Check(check_first_round),
    ],
};

/// Check that the first round of readings in `out` covers exactly the sensors
/// of `board`.
fn check_first_round(out: &str, board: &Board) -> Result<()> {
    let expected = board_sensors(board)?;
    let sampled: Vec<&str> = out
        .lines()
        .filter_map(|l| l.trim().strip_prefix("[Sensors]   Sampling "))
        .map(|l| l.trim_end_matches('.'))
        .collect();
    ensure!(
        sampled == expected,
        "expected the app to sample {expected:?}, but it samples {sampled:?}"
    );
    for sensor in expected {
        let label = SENSOR_READINGS
            .iter()
            .find(|(s, _)| s == sensor)
            .map_or_else(|| panic!("no reading label for {sensor:?}"), |(_, l)| *l);
        ensure!(
            out.contains(label),
            "no {label:?} reading in the first round"
        );
    }
    Ok(())
}

/// Sensors which the `sensors` app finds on each board's kernel, in the order
/// in which it lists them ("[Sensors]   Sampling <sensor>.").
static BOARD_SENSORS: &[(&Board, &[&str])] = &[
    (&qemu::RV64_VIRT, &[]),
    (&qemu::HIFIVE1, &[]),
    (&nrf52::NRF52840DK, &["Temperature sensor"]),
    (&nrf52::NRF52DK, &["Temperature sensor"]),
    (&nucleo_f429zi::BOARD, &["Temperature sensor"]),
    (&rp2xxx::PICO, &["Temperature sensor"]),
    (&rp2xxx::PICO_2, &[]),
];

/// Return the sensors of `board` from [`BOARD_SENSORS`].
fn board_sensors(board: &Board) -> Result<&'static [&'static str]> {
    (BOARD_SENSORS.iter())
        .find(|(b, _)| *b == board)
        .map(|(_, sensors)| *sensors)
        .with_context(|| format!("no sensors listed for {board}"))
}

/// Each sensor the `sensors` app lists, and the label of its readings.
const SENSOR_READINGS: &[(&str, &str)] = &[
    ("Ambient Light sensor", "Amb. Light:"),
    ("Temperature sensor", "Temperature:"),
    ("Humidity sensor", "Humidity:"),
    ("Accelerometer", "Acceleration:"),
    ("Magnetometer", "Magnetometer:"),
    ("Gyroscope", "Gyro:"),
    ("Pressure sensor", "Pressure:"),
    ("Proximity sensor", "Proximity:"),
    ("Sound Pressure sensor", "Sound Pressure:"),
    ("Moisture sensor", "Moisture:"),
    ("Rainfall sensor", "Rainfall:"),
];

#[cfg(test)]
mod tests {
    use super::{TEST, board_sensors};

    #[test]
    fn every_board_of_the_test_lists_its_sensors() {
        for board in TEST.boards() {
            assert!(board_sensors(board).is_ok(), "{board}");
        }
    }
}
