// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2022.

//! Chip specific configuration.
//!
//! This file includes a common configuration trait and pre-defined constants
//! values for different implementations and uses of the same earlgrey chip. For
//! example, running the chip on an FPGA requires different parameters from
//! running it in a verilog simulator.  Additionally, chips on different
//! platforms can be used differently, so this also permits changing values like
//! the UART baud rate to enable better debugging on platforms that can support
//! it.

/// Earlgrey configuration based on the target device.
pub trait EarlGreyConfig {
    /// Identifier for the platform. This is useful for debugging to confirm the
    /// correct configuration of the chip is being used.
    const NAME: &'static str;

    /// The clock speed of the CPU in Hz.
    const CPU_FREQ: u32;

    /// The clock speed of the peripherals in Hz.
    const PERIPHERAL_FREQ: u32;

    /// The clock of the AON Timer
    const AON_TIMER_FREQ: u32;

    /// The baud rate for UART. This allows for a version of the chip that can
    /// support a faster baud rate to use it to help with debugging.
    const UART_BAUDRATE: u32;
}

/// Config for running EarlGrey on the CW310 FPGA
pub enum EarlGreyConfigFPGACW310 {}
impl EarlGreyConfig for EarlGreyConfigFPGACW310 {
    const NAME: &'static str = "fpga_cw310";

    // Clock frequencies as of https://github.com/lowRISC/opentitan/pull/19368
    const CPU_FREQ: u32 = 10_000_000;
    const PERIPHERAL_FREQ: u32 = 2_500_000;
    const AON_TIMER_FREQ: u32 = 250_000;
    const UART_BAUDRATE: u32 = 115200;
}

/// Config for running EarlGrey in a verilog simulator.
pub enum EarlGreyConfigSimVerilator {}
impl EarlGreyConfig for EarlGreyConfigSimVerilator {
    const NAME: &'static str = "sim_verilator";

    // Clock frequencies as of https://github.com/lowRISC/opentitan/pull/19368
    const CPU_FREQ: u32 = 500_000;
    const PERIPHERAL_FREQ: u32 = 125_000;
    const AON_TIMER_FREQ: u32 = 125_000;
    const UART_BAUDRATE: u32 = 7200;
}
