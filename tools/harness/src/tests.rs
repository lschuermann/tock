// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{BoardKind, GpioMode, Uart};
use crate::requirement::Requirement;
use crate::testcase::{TestBody, TestCase, TestCtx};
use std::time::{Duration, Instant};

const ALL: &[BoardKind] = &[
    BoardKind::QemuVirt,
    BoardKind::Nrf52840Dk,
    BoardKind::NucleoF429zi,
];
const NRF: &[BoardKind] = &[BoardKind::Nrf52840Dk];
const PHYSICAL: &[BoardKind] = &[BoardKind::Nrf52840Dk, BoardKind::NucleoF429zi];
const TIMEOUT: Duration = Duration::from_secs(10);

const LED0: Requirement = Requirement::Gpio {
    name: "led0",
    mode: GpioMode::DigitalIn,
};
const LED1: Requirement = Requirement::Gpio {
    name: "led1",
    mode: GpioMode::DigitalIn,
};
const BUTTON0: Requirement = Requirement::Gpio {
    name: "button0",
    mode: GpioMode::DigitalOut,
};
const BUTTON1: Requirement = Requirement::Gpio {
    name: "button1",
    mode: GpioMode::DigitalOut,
};
const GPIO0: Requirement = Requirement::Gpio {
    name: "gpio0",
    mode: GpioMode::DigitalIn,
};

/// Interval at which GPIO inputs are sampled.
const POLL_INTERVAL: Duration = Duration::from_millis(1);
/// Maximum deviation of an observed GPIO edge from its expected time.
const EDGE_TOLERANCE: Duration = Duration::from_millis(25);
/// Time for the DUT to react to a GPIO input change.
const SETTLE: Duration = Duration::from_millis(50);

pub const TESTS: &[TestCase] = &[
    TestCase {
        id: "hello_world",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["c_hello"],
        body: TestBody::UartSequence {
            needles: &["Hello World!"],
            timeout: Duration::from_secs(20),
        },
    },
    TestCase {
        id: "c_hello_and_printf_long",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["c_hello", "tests/printf_long"],
        body: TestBody::Run(|ctx| {
            let uart = ctx.uart();
            let out = uart.wait_for("And a short message.", TIMEOUT)?;
            if !out.contains("Hi welcome to Tock. This test makes sure that a greater than 64 byte message can be printed.") {
                return Err("long message missing or not printed before the short one".into());
            }
            if !out.contains("Hello World!") {
                uart.wait_for("Hello World!", TIMEOUT)?;
            }
            Ok(())
        }),
    },
    TestCase {
        id: "console_timeout",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["tests/console/console_timeout"],
        body: TestBody::Run(|ctx| {
            let uart = ctx.uart();
            uart.wait_for("tock$ ", TIMEOUT)?;
            uart.write(b"Hello, Tock!")?;
            uart.wait_for(
                "Userspace call to read console returned: Hello, Tock!",
                TIMEOUT,
            )?;
            Ok(())
        }),
    },
    TestCase {
        id: "ipc_rot13",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["rot13_client", "rot13_service"],
        body: TestBody::UartSequence {
            needles: &[
                "12: Hello World!",
                "12: Uryyb Jbeyq!",
                "12: Hello World!",
                "12: Uryyb Jbeyq!",
            ],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "lua_hello",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["lua-hello"],
        body: TestBody::UartSequence {
            needles: &["Hello from Lua!"],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "malloc_test01",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/malloc_test01"],
        body: TestBody::UartSequence {
            needles: &["malloc01: success"],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "malloc_test02",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["tests/malloc_test02"],
        body: TestBody::UartSequence {
            needles: &["malloc02: success"],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "stack_size_test01",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/stack_size_test01"],
        body: TestBody::UartSequence {
            needles: &["Stack Test App", "Current stack pointer: 0x"],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "stack_size_test02",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/stack_size_test02"],
        body: TestBody::UartSequence {
            needles: &["Stack Test App", "Current stack pointer: 0x"],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "sensors",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["sensors"],
        body: TestBody::UartSequence {
            needles: &[
                "[Sensors] Starting Sensors App.",
                "[Sensors] All available sensors on the platform will be sampled.",
                "Temperature:",
            ],
            timeout: TIMEOUT,
        },
    },
    TestCase {
        id: "process_console_restart",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/whileone"],
        body: TestBody::Run(|ctx| {
            let uart = ctx.uart();
            uart.wait_for("tock$ ", TIMEOUT)?;
            let before = process_pid(uart, "whileone")?;
            console_command(uart, "terminate whileone")?;
            console_command(uart, "boot whileone")?;
            let after = process_pid(uart, "whileone")?;
            if after <= before {
                return Err(format!(
                    "PID did not increase after restart: {before} -> {after}"
                ));
            }
            Ok(())
        }),
    },
    TestCase {
        id: "process_console_stop_start",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/whileone"],
        body: TestBody::Run(|ctx| {
            let uart = ctx.uart();
            uart.wait_for("tock$ ", TIMEOUT)?;
            expect_state(uart, "whileone", "Running")?;
            console_command(uart, "stop whileone")?;
            expect_state(uart, "whileone", "Stopped(Running)")?;
            console_command(uart, "start whileone")?;
            expect_state(uart, "whileone", "Running")
        }),
    },
    TestCase {
        id: "blink",
        boards: NRF,
        requires: &[LED0, LED1],
        apps: &["blink"],
        body: TestBody::Run(expect_blinking),
    },
    TestCase {
        id: "scheduler_whileone_blink",
        boards: NRF,
        requires: &[Requirement::Uart, LED0, LED1],
        apps: &["tests/whileone", "blink"],
        body: TestBody::Run(|ctx| {
            // blink must keep its timing even though whileone never yields.
            let uart = ctx.uart();
            uart.wait_for("tock$ ", TIMEOUT)?;
            expect_state(uart, "whileone", "Running")?;
            expect_blinking(ctx)
        }),
    },
    TestCase {
        id: "blink_c_hello_buttons",
        boards: NRF,
        requires: &[Requirement::Uart, LED0, LED1, BUTTON0, BUTTON1],
        apps: &["blink", "c_hello", "buttons"],
        body: TestBody::Run(|ctx| {
            ctx.uart().wait_for("Hello World!", TIMEOUT)?;
            expect_blinking(ctx)?;
            // Pressing a button makes the buttons app toggle the button's LED.
            // Press halfway between two blink steps, so blink does not
            // change the LEDs at the same time.
            for (button, leds) in [("button0", ["led0", "led1"]), ("button1", ["led1", "led0"])] {
                wait_for_edge(ctx, "led0", TIMEOUT)?;
                std::thread::sleep(ms(100));
                let before = read_all(ctx, &leds)?;
                press(ctx, button)?;
                std::thread::sleep(SETTLE);
                let after = read_all(ctx, &leds)?;
                release(ctx, button)?;
                if after != [!before[0], before[1]] {
                    return Err(format!(
                        "pressing {button} changed {leds:?} from {before:?} to {after:?}, \
                         expected only {} to toggle",
                        leds[0]
                    ));
                }
            }
            Ok(())
        }),
    },
    TestCase {
        id: "buttons",
        boards: NRF,
        requires: &[LED0, LED1, BUTTON0, BUTTON1],
        apps: &["buttons"],
        body: TestBody::Run(|ctx| {
            set_inputs(ctx, &["led0", "led1"])?;
            release(ctx, "button0")?;
            release(ctx, "button1")?;

            // The app does not announce when it is ready, so keep pressing
            // button0 until it responds.
            let start = Instant::now();
            loop {
                let before = ctx.gpio("led0").read()?;
                press(ctx, "button0")?;
                std::thread::sleep(SETTLE);
                release(ctx, "button0")?;
                std::thread::sleep(SETTLE);
                if ctx.gpio("led0").read()? != before {
                    break;
                }
                if start.elapsed() > TIMEOUT {
                    return Err("pressing button0 never toggled led0".into());
                }
            }

            // Each press, but not the release, toggles the button's own LED.
            for (button, leds) in [("button0", ["led0", "led1"]), ("button1", ["led1", "led0"])] {
                for _ in 0..2 {
                    let before = read_all(ctx, &leds)?;
                    press(ctx, button)?;
                    std::thread::sleep(SETTLE);
                    let pressed = read_all(ctx, &leds)?;
                    release(ctx, button)?;
                    std::thread::sleep(SETTLE);
                    let released = read_all(ctx, &leds)?;
                    if pressed != [!before[0], before[1]] || released != pressed {
                        return Err(format!(
                            "{button}: {leds:?} went from {before:?} to {pressed:?} on press \
                             and {released:?} on release, expected only {} to toggle on press",
                            leds[0]
                        ));
                    }
                }
            }
            Ok(())
        }),
    },
    TestCase {
        id: "button_print",
        boards: NRF,
        requires: &[Requirement::Uart, BUTTON0],
        apps: &["tests/button_print"],
        body: TestBody::Run(|ctx| {
            release(ctx, "button0")?;
            ctx.uart().wait_for("[TEST] Button Press", TIMEOUT)?;
            for _ in 0..2 {
                press(ctx, "button0")?;
                ctx.uart()
                    .wait_for("Button Press! Button: 0 Status: 0", TIMEOUT)?;
                release(ctx, "button0")?;
                std::thread::sleep(SETTLE);
            }
            Ok(())
        }),
    },
    TestCase {
        id: "gpio_original",
        boards: NRF,
        requires: &[Requirement::Uart, GPIO0],
        apps: &["tests/gpio/gpio_original"],
        body: TestBody::Run(|ctx| {
            // The app toggles userspace GPIO pin 0 every second.
            ctx.uart().wait_for("Periodically toggling pin", TIMEOUT)?;
            set_inputs(ctx, &["gpio0"])?;
            wait_for_edge(ctx, "gpio0", TIMEOUT)?;
            std::thread::sleep(ms(500));
            let edges = record_edges(ctx, &["gpio0"], ms(4000))?;
            expect_edges("gpio0", &edges[0], (500..4000).step_by(1000).map(ms))
        }),
    },
    TestCase {
        id: "multi_alarm_test",
        boards: NRF,
        requires: &[LED0, LED1],
        apps: &["tests/alarms/multi_alarm_test"],
        body: TestBody::Run(|ctx| {
            // Each of the board's four LEDs lights up for 300 ms every 4 s,
            // one second after the previous LED. Synchronize to the end of an
            // led0 pulse, the only case of two led0 edges less than 1 s apart.
            set_inputs(ctx, &["led0", "led1"])?;
            wait_for_edge(ctx, "led0", ms(6000))?;
            while wait_for_edge(ctx, "led0", ms(5000))? > ms(1000) {}
            let edges = record_edges(ctx, &["led0", "led1"], ms(8350))?;
            expect_edges("led0", &edges[0], [3700, 4000, 7700, 8000].map(ms))?;
            expect_edges("led1", &edges[1], [700, 1000, 4700, 5000].map(ms))
        }),
    },
    TestCase {
        id: "mpu_walk_region_flash",
        boards: NRF,
        requires: &[Requirement::Uart, BUTTON0],
        apps: &["tests/mpu/mpu_walk_region"],
        body: TestBody::Run(|ctx| expect_mpu_walk_fault(ctx, "memory", "flash")),
    },
    TestCase {
        id: "mpu_walk_region_memory",
        boards: NRF,
        requires: &[Requirement::Uart, BUTTON0],
        apps: &["tests/mpu/mpu_walk_region"],
        body: TestBody::Run(|ctx| expect_mpu_walk_fault(ctx, "flash", "memory")),
    },
    TestCase {
        id: "tutorial_ipc_rng_led",
        boards: NRF,
        requires: &[Requirement::Uart, LED0, LED1],
        apps: &[
            "tutorials/05_ipc/led",
            "tutorials/05_ipc/rng",
            "tutorials/05_ipc/logic",
        ],
        body: TestBody::Run(|ctx| {
            // The logic app queries the LED service for the number of LEDs,
            // then every 500 ms sets a random LED to a random state, using
            // bytes from the RNG service.
            ctx.uart().wait_for("Number of LEDs: 4", TIMEOUT)?;
            set_inputs(ctx, &["led0", "led1"])?;
            let edges = record_edges_until(ctx, &["led0", "led1"], ms(60_000), |edges| {
                edges.iter().all(|pin| !pin.is_empty())
            })?;
            if edges.iter().any(|pin| pin.is_empty()) {
                return Err(format!("not all LEDs changed within 60s: {edges:?}"));
            }
            let mut all = edges.concat();
            all.sort();
            if all.windows(2).any(|w| w[1] - w[0] < ms(450)) {
                return Err(format!("LEDs changed less than 500 ms apart: {edges:?}"));
            }
            Ok(())
        }),
    },
];

fn console_command(uart: &mut dyn Uart, cmd: &str) -> Result<String, String> {
    for byte in format!("{cmd}\r\n").bytes() {
        uart.write(&[byte])?;
        std::thread::sleep(Duration::from_millis(10));
    }
    uart.wait_for("tock$ ", TIMEOUT)
}

fn process_row(uart: &mut dyn Uart, name: &str) -> Result<String, String> {
    console_command(uart, "list")?
        .lines()
        .find(|line| line.contains(name))
        .map(str::to_string)
        .ok_or_else(|| format!("process {name} not found in process list"))
}

fn process_pid(uart: &mut dyn Uart, name: &str) -> Result<u32, String> {
    let row = process_row(uart, name)?;
    row.split_whitespace()
        .next()
        .and_then(|pid| pid.parse().ok())
        .ok_or_else(|| format!("failed to parse PID from {row:?}"))
}

fn expect_state(uart: &mut dyn Uart, name: &str, state: &str) -> Result<(), String> {
    let row = process_row(uart, name)?;
    match row.split_whitespace().last() {
        Some(s) if s == state => Ok(()),
        _ => Err(format!(
            "expected process {name} to be {state}, got {row:?}"
        )),
    }
}

fn ms(ms: u64) -> Duration {
    Duration::from_millis(ms)
}

fn set_inputs(ctx: &mut TestCtx, pins: &[&str]) -> Result<(), String> {
    pins.iter()
        .try_for_each(|pin| ctx.gpio(pin).set_mode(GpioMode::DigitalIn))
}

fn read_all(ctx: &mut TestCtx, pins: &[&str]) -> Result<Vec<bool>, String> {
    pins.iter().map(|pin| ctx.gpio(pin).read()).collect()
}

/// Presses an active-low button by pulling its line low.
fn press(ctx: &mut TestCtx, button: &str) -> Result<(), String> {
    let gpio = ctx.gpio(button);
    gpio.set_mode(GpioMode::DigitalOut)?;
    gpio.write(false)
}

/// Releases an active-low button by letting the DUT's pull-up raise the line.
/// The line is never driven high, as the DUT may use a lower I/O voltage than
/// the host.
fn release(ctx: &mut TestCtx, button: &str) -> Result<(), String> {
    ctx.gpio(button).set_mode(GpioMode::DigitalIn)
}

/// Waits for `pin` to change level, returning how long that took.
fn wait_for_edge(ctx: &mut TestCtx, pin: &str, timeout: Duration) -> Result<Duration, String> {
    let start = Instant::now();
    let initial = ctx.gpio(pin).read()?;
    while ctx.gpio(pin).read()? == initial {
        if start.elapsed() > timeout {
            return Err(format!("{pin} did not change within {timeout:?}"));
        }
        std::thread::sleep(POLL_INTERVAL);
    }
    Ok(start.elapsed())
}

/// Samples `pins` for `duration`, returning for each pin the times (relative
/// to the start) at which it changed level.
fn record_edges(
    ctx: &mut TestCtx,
    pins: &[&str],
    duration: Duration,
) -> Result<Vec<Vec<Duration>>, String> {
    record_edges_until(ctx, pins, duration, |_| false)
}

/// Like `record_edges`, but stops early once `done` returns true.
fn record_edges_until(
    ctx: &mut TestCtx,
    pins: &[&str],
    duration: Duration,
    done: impl Fn(&[Vec<Duration>]) -> bool,
) -> Result<Vec<Vec<Duration>>, String> {
    let mut last = read_all(ctx, pins)?;
    let mut edges = vec![Vec::new(); pins.len()];
    let start = Instant::now();
    while start.elapsed() < duration && !done(&edges) {
        let levels = read_all(ctx, pins)?;
        let t = start.elapsed();
        for (i, level) in levels.into_iter().enumerate() {
            if level != last[i] {
                edges[i].push(t);
                last[i] = level;
            }
        }
        std::thread::sleep(POLL_INTERVAL);
    }
    Ok(edges)
}

fn millis(times: &[Duration]) -> Vec<u128> {
    times.iter().map(Duration::as_millis).collect()
}

/// Fails unless `edges` match `expected` one-to-one, within `EDGE_TOLERANCE`.
fn expect_edges(
    pin: &str,
    edges: &[Duration],
    expected: impl IntoIterator<Item = Duration>,
) -> Result<(), String> {
    let expected: Vec<Duration> = expected.into_iter().collect();
    let matches = edges.len() == expected.len()
        && edges
            .iter()
            .zip(&expected)
            .all(|(edge, want)| edge.abs_diff(*want) <= EDGE_TOLERANCE);
    if matches {
        Ok(())
    } else {
        Err(format!(
            "{pin}: expected edges at {:?} ms, observed {:?} ms",
            millis(&expected),
            millis(edges)
        ))
    }
}

/// Checks the pattern of the blink app, a binary counter on the LEDs that
/// advances every 250 ms: led0 toggles on every step, led1 on every other.
fn expect_blinking(ctx: &mut TestCtx) -> Result<(), String> {
    set_inputs(ctx, &["led0", "led1"])?;
    // Start recording halfway between two steps, so that no edge coincides
    // with the start.
    wait_for_edge(ctx, "led0", TIMEOUT)?;
    std::thread::sleep(ms(125));
    let edges = record_edges(ctx, &["led0", "led1"], ms(3750))?;
    let (led0, led1) = (&edges[0], &edges[1]);

    // Each step waits 250 ms after the previous one, so scheduling latency
    // accumulates: check the length of each step rather than absolute times.
    let uneven = led0
        .windows(2)
        .any(|w| (w[1] - w[0]).abs_diff(ms(250)) > EDGE_TOLERANCE);
    if led0.len() < 14 || uneven {
        return Err(format!(
            "led0: expected a toggle every 250 ms, observed edges at {:?} ms",
            millis(led0)
        ));
    }
    let skip = match led1.first() {
        Some(&t) if t.abs_diff(led0[0]) <= EDGE_TOLERANCE => 0,
        _ => 1,
    };
    expect_edges("led1", led1, led0.iter().copied().skip(skip).step_by(2))
}

/// Makes the mpu_walk_region app walk past the end of its `region` ("flash"
/// or "memory"), and checks that this faults. The app walks both regions in
/// turn, overrunning a region if button0 is held when it starts walking it.
fn expect_mpu_walk_fault(ctx: &mut TestCtx, other: &str, region: &str) -> Result<(), String> {
    release(ctx, "button0")?;
    ctx.uart().wait_for("[TEST] MPU Walk Regions", TIMEOUT)?;
    // The app reads the button before announcing a walk, so pressing it
    // after the announcement only affects the walk that follows.
    ctx.uart().wait_for(&format!("Walking {other}"), TIMEOUT)?;
    press(ctx, "button0")?;
    let walk = ctx.uart().wait_for("! Will overrun", TIMEOUT)?;
    if !walk.contains(&format!("Walking {region}")) || walk.contains(&format!("Walking {other}")) {
        return Err(format!(
            "the app did not overrun {region} after walking {other}"
        ));
    }
    let uart = ctx.uart();
    uart.wait_for("mpu_walk_region had a fault", TIMEOUT)?;
    uart.wait_for("---| Cortex-M Fault Status |---", TIMEOUT)?;
    Ok(())
}
