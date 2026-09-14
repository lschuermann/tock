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
    BoardKind::RaspberryPiPico,
    BoardKind::Esp32C3DevkitM1,
];
const NRF: &[BoardKind] = &[BoardKind::Nrf52840Dk];
const NUCLEO: &[BoardKind] = &[BoardKind::NucleoF429zi];
/// Boards whose console is their kernel's USB CDC-ACM device.
const USB_CDC_CONSOLE: &[BoardKind] = &[BoardKind::RaspberryPiPico];
const PHYSICAL: &[BoardKind] = &[
    BoardKind::Nrf52840Dk,
    BoardKind::NucleoF429zi,
    BoardKind::RaspberryPiPico,
    BoardKind::Esp32C3DevkitM1,
];
/// Boards that can run the `switch_stress` app, which supports Thumb and RV32.
const SWITCH_STRESS: &[BoardKind] = &[
    BoardKind::Nrf52840Dk,
    BoardKind::NucleoF429zi,
    BoardKind::RaspberryPiPico,
    BoardKind::RaspberryPiPico2,
];
const TIMEOUT: Duration = Duration::from_secs(10);
/// How often `usb_enumeration_stress` restarts the board.
const USB_RESTARTS: u64 = 50;

/// The time limit of a test body that waits up to `n` [`TIMEOUT`]s, and for
/// up to `extra_ms` otherwise.
/// How many led0 edges `multi_alarm_test` waits for to synchronize to the end
/// of a pulse.
const MULTI_ALARM_SYNC_EDGES: u64 = 3;

const fn timeouts(n: u64, extra_ms: u64) -> Duration {
    Duration::from_millis(n * TIMEOUT.as_millis() as u64 + extra_ms)
}
/// How long to run the `switch_stress` app for. At about 5000 system calls per
/// second on the NUCLEO-F429ZI, this covers one full sweep of its alarm
/// deadlines (17 bands of at most 16384 calls each).
const SWITCH_STRESS_DURATION: Duration = Duration::from_secs(60);

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

/// The sensors the `sensors` app finds on each board's kernel, as it lists
/// them ("[Sensors]   Sampling <sensor>."), in its order.
fn board_sensors(board: BoardKind) -> &'static [&'static str] {
    match board {
        BoardKind::Nrf52840Dk | BoardKind::NucleoF429zi | BoardKind::RaspberryPiPico => {
            &["Temperature sensor"]
        }
        BoardKind::QemuVirt | BoardKind::RaspberryPiPico2 | BoardKind::Esp32C3DevkitM1 => &[],
    }
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

/// The test a `PlannedTest.id` (`"{test_id}@{board}"`) refers to.
pub fn lookup(planned_id: &str) -> &'static TestCase {
    TESTS
        .iter()
        .find(|t| planned_id.split('@').next() == Some(t.id))
        .unwrap_or_else(|| panic!("plan references an unknown test id {planned_id:?}"))
}

pub const TESTS: &[TestCase] = &[
    TestCase {
        id: "hello_world",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["c_hello"],
        unsupported: &[],
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
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                let uart = ctx.uart();
                let out = uart.wait_for("And a short message.", TIMEOUT)?;
                if !out.contains("Hi welcome to Tock. This test makes sure that a greater than 64 byte message can be printed.") {
                return Err("long message missing or not printed before the short one".into());
            }
                if !out.contains("Hello World!") {
                    uart.wait_for("Hello World!", TIMEOUT)?;
                }
                Ok(())
            },
            time_limit: timeouts(2, 0),
        },
    },
    TestCase {
        id: "console_timeout",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["tests/console/console_timeout"],
        unsupported: &[(
            BoardKind::Esp32C3DevkitM1,
            "the ESP32 UART driver does not implement receive_abort",
        )],
        body: TestBody::Run {
            run: |ctx| {
                let uart = ctx.uart();
                uart.wait_for("tock$ ", TIMEOUT)?;
                uart.write(b"Hello, Tock!")?;
                uart.wait_for(
                    "Userspace call to read console returned: Hello, Tock!",
                    TIMEOUT,
                )?;
                Ok(())
            },
            time_limit: timeouts(2, 500),
        },
    },
    TestCase {
        id: "ipc_rot13",
        boards: PHYSICAL,
        requires: &[Requirement::Uart],
        apps: &["rot13_client", "rot13_service"],
        unsupported: &[(BoardKind::Esp32C3DevkitM1, "the board's kernel has no IPC")],
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
        unsupported: &[],
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
        unsupported: &[],
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
        unsupported: &[],
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
        unsupported: &[],
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
        unsupported: &[],
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
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                let expected = board_sensors(ctx.board());
                let uart = ctx.uart();
                uart.wait_for("[Sensors] Starting Sensors App.", TIMEOUT)?;
                uart.wait_for("will be sampled.", TIMEOUT)?;
                // The app lists the sensors it samples, then prints a round of
                // readings every second, each ending with an empty line.
                let out = uart.wait_for("\n\n", TIMEOUT)?;
                let sampled: Vec<&str> = out
                    .lines()
                    .filter_map(|l| l.trim().strip_prefix("[Sensors]   Sampling "))
                    .map(|l| l.trim_end_matches('.'))
                    .collect();
                if sampled != expected {
                    return Err(format!(
                        "expected the app to sample {expected:?}, but it samples {sampled:?}"
                    ));
                }
                for sensor in expected {
                    let label = SENSOR_READINGS
                        .iter()
                        .find(|(s, _)| s == sensor)
                        .map_or_else(|| panic!("no reading label for {sensor:?}"), |(_, l)| *l);
                    if !out.contains(label) {
                        return Err(format!("no {label:?} reading in the first round"));
                    }
                }
                Ok(())
            },
            time_limit: timeouts(3, 0),
        },
    },
    TestCase {
        id: "process_console_restart",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/whileone"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
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
            },
            time_limit: timeouts(5, 1_000),
        },
    },
    TestCase {
        id: "process_console_stop_start",
        boards: ALL,
        requires: &[Requirement::Uart],
        apps: &["tests/whileone"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                let uart = ctx.uart();
                uart.wait_for("tock$ ", TIMEOUT)?;
                expect_state(uart, "whileone", "Running")?;
                console_command(uart, "stop whileone")?;
                expect_state(uart, "whileone", "Stopped(Running)")?;
                console_command(uart, "start whileone")?;
                expect_state(uart, "whileone", "Running")
            },
            time_limit: timeouts(6, 1_000),
        },
    },
    TestCase {
        // Restarts the board over and over, checking that each time, its USB
        // device enumerates and its CDC-ACM console works both ways: it must
        // print the output of the app, which only prints once, at boot, and
        // answer a process console command. Enumeration of
        // the RP2040's USB device used to fail intermittently (a timeout, or a
        // truncated configuration descriptor), and a failing enumeration may
        // take the host's USB controller down with it.
        id: "usb_enumeration_stress",
        boards: USB_CDC_CONSOLE,
        requires: &[Requirement::Uart],
        apps: &["c_hello"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                let check = |uart: &mut dyn Uart| {
                    uart.wait_for("Hello World!", TIMEOUT)?;
                    type_command(uart, "list")?;
                    uart.wait_for("c_hello", TIMEOUT).map(drop)
                };
                check(ctx.uart())?;
                for restart in 1..=USB_RESTARTS {
                    let at = |e: String| format!("restart {restart} of {USB_RESTARTS}: {e}");
                    ctx.restart().map_err(at)?;
                    check(ctx.uart()).map_err(at)?;
                }
                Ok(())
            },
            // Each restart waits up to one TIMEOUT for the USB device, and two
            // for the console, and takes a few seconds to reset the chip.
            time_limit: timeouts(2 + 3 * USB_RESTARTS, 3_000 * USB_RESTARTS),
        },
    },
    TestCase {
        // Regression test for https://github.com/tock/tock/pull/5194: the
        // ARMv7-M HardFault handler must clear the sticky CFSR/HFSR bits it
        // reports. Otherwise, the bits set by a process fault (here, a stack
        // overflow) leak into the report of a later kernel HardFault, and a
        // stale BFSR.STKERR makes the kernel misreport it as a kernel stack
        // overflow.
        id: "app_stack_overflow_then_kernel_hardfault",
        boards: NUCLEO,
        requires: &[Requirement::Uart],
        apps: &["tests/mpu/unit/mpu_stack_growth"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                let uart = ctx.uart();
                uart.wait_for("This test should recursively add stack frames", TIMEOUT)?;
                uart.wait_for("mpu_stack_growth faulted and was stopped.", TIMEOUT)?;

                // The `hardfault` command executes an undefined instruction in
                // the kernel, which should only set CFSR.UNDEFINSTR.
                //
                // The HardFault handler panics in handler mode, where the board's
                // `PANIC_RESOURCES` (a `SingleThreadValue` bound to thread mode)
                // are not accessible. The panic therefore does not print the chip
                // state (the "Cortex-M Fault Status" of the process) or the
                // process list, and we can only check the panic message, which
                // ends with the kernel version.
                type_command(uart, "hardfault")?;
                let report = uart.wait_for("\tKernel version ", TIMEOUT)?;
                if report.contains("kernel stack overflow") {
                    return Err("kernel HardFault was reported as a kernel stack overflow".into());
                }
                if !report.contains("Kernel HardFault.") {
                    return Err(format!("no kernel HardFault report in {report:?}"));
                }
                let cfsr = report
                    .lines()
                    .find_map(|line| line.trim().strip_prefix("CFSR  0x"))
                    .and_then(|hex| u32::from_str_radix(hex.trim(), 16).ok())
                    .ok_or_else(|| format!("no CFSR in kernel HardFault report {report:?}"))?;
                const UNDEFINSTR: u32 = 1 << 16;
                if cfsr != UNDEFINSTR {
                    return Err(format!(
                        "kernel HardFault CFSR is {cfsr:#010x}, expected only UNDEFINSTR \
                     ({UNDEFINSTR:#010x}); bits from the process fault leaked"
                    ));
                }
                Ok(())
            },
            time_limit: timeouts(3, 500),
        },
    },
    // TestCase {
    //     // Regression test for https://github.com/tock/tock/pull/5193 (see also
    //     // https://github.com/tock/tock/issues/3109): on ARMv7-M, an exception
    //     // tail-chained onto an app's `svc` could make the SVC handler switch
    //     // straight back to the app, without the kernel ever handling its
    //     // system call. The `switch_stress` app races alarm interrupts against
    //     // system calls to a nonexistent driver, and reports calls that return
    //     // their own arguments ("echoes") or other unexpected values ("bad").
    //     //
    //     // The alarm upcalls arriving while the app waits for its `printf`s
    //     // also exercise https://github.com/tock/tock/pull/5195, a race that
    //     // left a yield-wait-for'ing process stuck forever.
    //     id: "switch_stress",
    //     boards: SWITCH_STRESS,
    //     requires: &[Requirement::Uart],
    //     apps: &["tests/switch_stress"],
    //     unsupported: &[],
    //     body: TestBody::Run(|ctx| {
    //         let uart = ctx.uart();
    //         uart.wait_for("switch_stress: ", TIMEOUT)?;
    //         let start = Instant::now();
    //         let mut reports = 0;
    //         while start.elapsed() < SWITCH_STRESS_DURATION {
    //             let line = uart.wait_for("\n", TIMEOUT).map_err(|e| {
    //                 format!(
    //                     "switch_stress stopped printing after {:?} and {reports} progress \
    //                      reports; is it stuck yield-wait-for'ing its console write? {e}",
    //                     start.elapsed()
    //                 )
    //             })?;
    //             let line = line.trim();
    //             let failed = ["FAIL:", "fault", "panic", "switch_stress: "]
    //                 .iter()
    //                 .any(|needle| line.contains(needle));
    //             let counts = ["echoes=", "bad="].map(|name| report_field(line, name));
    //             if failed || counts.iter().any(|count| count.is_some_and(|c| c != 0)) {
    //                 return Err(format!(
    //                     "failed after {:?} and {reports} progress reports: {line:?}",
    //                     start.elapsed()
    //                 ));
    //             }
    //             if counts.iter().all(Option::is_some) {
    //                 reports += 1;
    //             }
    //         }
    //         if reports == 0 {
    //             return Err("switch_stress never reported progress".into());
    //         }
    //         Ok(())
    //     }),
    // },
    TestCase {
        id: "blink",
        boards: NRF,
        requires: &[LED0, LED1],
        apps: &["blink"],
        unsupported: &[],
        body: TestBody::Run {
            run: expect_blinking,
            time_limit: timeouts(1, 4_000),
        },
    },
    TestCase {
        id: "scheduler_whileone_blink",
        boards: NRF,
        requires: &[Requirement::Uart, LED0, LED1],
        apps: &["tests/whileone", "blink"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                // blink must keep its timing even though whileone never yields.
                let uart = ctx.uart();
                uart.wait_for("tock$ ", TIMEOUT)?;
                expect_state(uart, "whileone", "Running")?;
                expect_blinking(ctx)
            },
            time_limit: timeouts(3, 4_500),
        },
    },
    TestCase {
        id: "blink_c_hello_buttons",
        boards: NRF,
        requires: &[Requirement::Uart, LED0, LED1, BUTTON0, BUTTON1],
        apps: &["blink", "c_hello", "buttons"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                ctx.uart().wait_for("Hello World!", TIMEOUT)?;
                expect_blinking(ctx)?;
                // Pressing a button makes the buttons app toggle the button's LED.
                // Press halfway between two blink steps, so blink does not
                // change the LEDs at the same time.
                for (button, leds) in [("button0", ["led0", "led1"]), ("button1", ["led1", "led0"])]
                {
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
            },
            time_limit: timeouts(4, 4_500),
        },
    },
    TestCase {
        id: "buttons",
        boards: NRF,
        requires: &[LED0, LED1, BUTTON0, BUTTON1],
        apps: &["buttons"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
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
                for (button, leds) in [("button0", ["led0", "led1"]), ("button1", ["led1", "led0"])]
                {
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
            },
            time_limit: timeouts(1, 1_500),
        },
    },
    TestCase {
        id: "button_print",
        boards: NRF,
        requires: &[Requirement::Uart, BUTTON0],
        apps: &["tests/button_print"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
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
            },
            time_limit: timeouts(3, 500),
        },
    },
    TestCase {
        id: "gpio_original",
        boards: NRF,
        requires: &[Requirement::Uart, GPIO0],
        apps: &["tests/gpio/gpio_original"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                // The app toggles userspace GPIO pin 0 every second.
                ctx.uart().wait_for("Periodically toggling pin", TIMEOUT)?;
                set_inputs(ctx, &["gpio0"])?;
                wait_for_edge(ctx, "gpio0", TIMEOUT)?;
                std::thread::sleep(ms(500));
                let edges = record_edges(ctx, &["gpio0"], ms(4000))?;
                expect_edges("gpio0", &edges[0], (500..4000).step_by(1000).map(ms))
            },
            time_limit: timeouts(2, 5_000),
        },
    },
    TestCase {
        id: "multi_alarm_test",
        boards: NRF,
        requires: &[LED0, LED1],
        apps: &["tests/alarms/multi_alarm_test"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
                // Each of the board's four LEDs lights up for 300 ms every 4 s,
                // one second after the previous LED. Synchronize to the end of an
                // led0 pulse, the only case of two led0 edges less than 1 s apart.
                // At most two edges after any first one end a pulse; allow
                // one more before giving up on an LED that never pulses.
                set_inputs(ctx, &["led0", "led1"])?;
                wait_for_edge(ctx, "led0", ms(6000))?;
                let mut synced = false;
                for _ in 0..MULTI_ALARM_SYNC_EDGES {
                    if wait_for_edge(ctx, "led0", ms(5000))? <= ms(1000) {
                        synced = true;
                        break;
                    }
                }
                if !synced {
                    return Err(format!(
                        "led0 did not pulse: none of {MULTI_ALARM_SYNC_EDGES} edges followed \
                         the previous one within 1 s"
                    ));
                }
                let edges = record_edges(ctx, &["led0", "led1"], ms(8350))?;
                expect_edges("led0", &edges[0], [3700, 4000, 7700, 8000].map(ms))?;
                expect_edges("led1", &edges[1], [700, 1000, 4700, 5000].map(ms))
            },
            time_limit: Duration::from_millis(6_000 + MULTI_ALARM_SYNC_EDGES * 5_000 + 8_350),
        },
    },
    TestCase {
        id: "mpu_walk_region_flash",
        boards: NRF,
        requires: &[Requirement::Uart, BUTTON0],
        apps: &["tests/mpu/mpu_walk_region"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| expect_mpu_walk_fault(ctx, "memory", "flash"),
            time_limit: timeouts(5, 500),
        },
    },
    TestCase {
        id: "mpu_walk_region_memory",
        boards: NRF,
        requires: &[Requirement::Uart, BUTTON0],
        apps: &["tests/mpu/mpu_walk_region"],
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| expect_mpu_walk_fault(ctx, "flash", "memory"),
            time_limit: timeouts(5, 500),
        },
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
        unsupported: &[],
        body: TestBody::Run {
            run: |ctx| {
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
            },
            time_limit: timeouts(1, 60_500),
        },
    },
];

fn console_command(uart: &mut dyn Uart, cmd: &str) -> Result<String, String> {
    type_command(uart, cmd)?;
    uart.wait_for("tock$ ", TIMEOUT)
}

/// Types `cmd` into the process console, one byte at a time, without waiting
/// for it to complete.
fn type_command(uart: &mut dyn Uart, cmd: &str) -> Result<(), String> {
    uart.write_slowly(format!("{cmd}\r\n").as_bytes(), Duration::from_millis(10))
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

/// Parses the value of a `name=value` field in a `switch_stress` progress
/// report.
fn report_field(line: &str, name: &str) -> Option<u64> {
    line.split_whitespace()
        .find_map(|field| field.strip_prefix(name))
        .and_then(|value| value.parse().ok())
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
    log::info!("  pressing {button}");
    let gpio = ctx.gpio(button);
    gpio.set_mode(GpioMode::DigitalOut)?;
    gpio.write(false)
}

/// Releases an active-low button by letting the DUT's pull-up raise the line.
/// The line is never driven high, as the DUT may use a lower I/O voltage than
/// the host.
fn release(ctx: &mut TestCtx, button: &str) -> Result<(), String> {
    log::info!("  releasing {button}");
    ctx.gpio(button).set_mode(GpioMode::DigitalIn)
}

/// Waits for `pin` to change level, returning how long that took.
fn wait_for_edge(ctx: &mut TestCtx, pin: &str, timeout: Duration) -> Result<Duration, String> {
    log::info!("  waiting for {pin} to change (up to {timeout:?})");
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
    log::info!(
        "  recording edges on {} for up to {duration:?}",
        pins.join(", ")
    );
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
    log::debug!("  {pin}: edges at {:?} ms", millis(edges));
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
