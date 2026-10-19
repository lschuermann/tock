// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Frame transmission for single-wire RGB LEDs (WS2812 and SK68XX).
//!
//! These LEDs decode each bit from the width of a high pulse on their data
//! line: a short pulse is a `0`, a long pulse is a `1`. They take a 24-bit
//! frame in GRB order, MSB first. Once the line stays low for the LED's reset
//! time, the LED latches the frame. Bits that follow a frame without such a
//! pause are forwarded to the next LED in the chain.
//!
//! The pulse widths only tolerate about 150 ns of error. A cache miss on the
//! XIP flash, or a trap, stretches a pulse by much more. Therefore, we send
//! frames from code in `.iram` with interrupts disabled.

use esp32::gpio::GpioPin;

/// Send one 24-bit frame on a WS2812 or SK68XX LED and let the LED latch it.
///
/// `grb` holds the green, red and blue bytes in its lower 24 bits, with green
/// in the most significant byte. `pin` must be a GPIO output. This function
/// holds `pin` low for 300 us before the frame, which latches the previous
/// frame and separates it from this one. The LED latches this frame 300 us
/// after this function returns, unless another frame follows.
///
/// The pulse widths assume a CPU clock of 160 MHz.
pub fn write(pin: &GpioPin, grb: u32) {
    write_frame(pin.mask(), grb);
}

/// Send one 24-bit frame on the pins in `mask`, after holding them low for
/// 300 us.
///
/// This function times the pulses with Espressif's machine performance
/// counter `mpccr`, which it configures to count CPU cycles. It holds the line
/// low with interrupts enabled. It then clears `mstatus.MIE` for the frame
/// (about 30 us), and restores it afterwards.
///
/// A `0` bit is high for 300 ns, a `1` bit for 700 ns, and each bit takes
/// 1.25 us. The SK68XX-MINI-HS latches a frame once the line is low for 80 us,
/// but WS2812B-V5 and newer WS2812 parts require at least 280 us.
///
/// ## `naked`
///
/// This function must be implemented entirely within assembly. The compiler
/// could otherwise place parts of it (e.g., outlined prologues, or calls to
/// helpers that it chose not to inline) in flash, where a cache miss stretches
/// the pulses. It follows the standard C calling convention.
///
/// ## `link_section`
///
/// This function must be located entirely in SRAM and be executed from the
/// instruction bus alias of SRAM memory (IRAM). We use the `link_section`
/// attribute to place this function into `.iram.esp32_c3_ws2812_write_frame`.
/// The linker script is responsible for ensuring that this function is located
/// in IRAM. The chip's entry stub ([`crate::esp32_c3_direct_boot_entry`])
/// copies the `.iram` section into the `iram` region before any Rust code runs.
//
// ### Safety
//
// Both attributes are unsafe as the compiler cannot check their conditions.
//
// `naked`: the assembly follows the C calling convention. It only clobbers
// caller-saved registers, does not touch the stack, and returns with `ret`. It
// writes no memory other than the `GPIO_OUT_W1TS` and `GPIO_OUT_W1TC`
// registers, and it leaves `mstatus.MIE` as it found it.
//
// `link_section`: executing this function is sound only when the `iram` region
// holds a copy of the `.iram` section. The entry stub makes this copy before
// any Rust code runs, and no code writes the `iram` region afterwards: the
// board's linker script places it above the kernel stack, and the PMP denies
// the BLE sandbox access to it. Callers thus need not uphold any condition.
#[cfg(all(target_arch = "riscv32", target_os = "none"))]
#[unsafe(link_section = ".iram.esp32_c3_ws2812_write_frame")]
#[unsafe(naked)]
extern "C" fn write_frame(mask: u32, grb: u32) {
    core::arch::naked_asm!(
        "
    // Count CPU cycles in `mpccr`.
    li   t0, 1
    csrw 0x7e0, t0              // mpcer = 1: count cycles
    csrw 0x7e1, t0              // mpcmr = 1: enable the counter

    // Hold the line low for the reset time. Interrupts may extend this pause.
    // We compare cycle counts through their difference, which is correct when
    // the counter wraps around.
    csrr a2, 0x7e2
    li   t0, {reset}
    add  a2, a2, t0             // a2 = end of the pause
2:  csrr t0, 0x7e2
    sub  t0, t0, a2
    bltz t0, 2b

    li   t1, {w1ts}
    li   t2, {w1tc}
    slli a1, a1, 8              // a1 = frame, with its first bit in bit 31
    li   a3, 24                 // a3 = number of remaining bits
    csrrci t3, mstatus, 0x8     // t3 = mstatus, then clear mstatus.MIE
    csrr a2, 0x7e2              // a2 = start of the current bit

    // We derive all edges from the start of the frame, such that the loop's
    // own cycles do not accumulate.
3:  sw   a0, 0(t1)              // drive the line high
    li   t0, {t0h}
    bgez a1, 4f
    li   t0, {t1h}              // a `1` bit: the MSB of a1 is set
4:  add  a4, a2, t0             // a4 = end of the high pulse
5:  csrr t0, 0x7e2
    sub  t0, t0, a4
    bltz t0, 5b
    sw   a0, 0(t2)              // drive the line low
    addi a2, a2, {bit}          // a2 = start of the next bit
    slli a1, a1, 1
    addi a3, a3, -1
6:  csrr t0, 0x7e2
    sub  t0, t0, a2
    bltz t0, 6b
    bnez a3, 3b

    andi t3, t3, 0x8            // t3 = previous mstatus.MIE
    csrs mstatus, t3            // set mstatus.MIE if it was set
    ret
        ",
        // CPU cycles at 160 MHz: 300 us, 300 ns, 700 ns and 1.25 us.
        reset = const 48_000,
        t0h = const 48,
        t1h = const 112,
        bit = const 200,
        // `GPIO_OUT_W1TS` and `GPIO_OUT_W1TC`, which drive pins high and low.
        w1ts = const 0x6000_4008,
        w1tc = const 0x6000_400C,
    );
}

/// Mock implementation for static checks and CI.
#[cfg(not(all(target_arch = "riscv32", target_os = "none")))]
extern "C" fn write_frame(_mask: u32, _grb: u32) {
    unimplemented!()
}
