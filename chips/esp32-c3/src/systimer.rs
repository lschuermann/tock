// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! System timer (SYSTIMER): read the free-running counter of unit 0.
//!
//! Unit 0 counts at 16 MHz from boot. The mask ROM and ESP-IDF's code use it
//! as their time base, so this driver only reads it.

use kernel::utilities::StaticRef;
use kernel::utilities::registers::interfaces::{Readable, Writeable};
use kernel::utilities::registers::{ReadOnly, ReadWrite, register_bitfields, register_structs};

pub const SYSTIMER_BASE: StaticRef<SysTimerRegisters> =
    unsafe { StaticRef::new(0x6002_3000 as *const SysTimerRegisters) };

/// Counter ticks per microsecond.
pub const TICKS_PER_US: u64 = 16;

register_structs! {
    pub SysTimerRegisters {
        (0x000 => _reserved0),
        (0x004 => unit0_op: ReadWrite<u32, UNIT_OP::Register>),
        (0x008 => _reserved1),
        (0x040 => unit0_value_hi: ReadOnly<u32>),
        (0x044 => unit0_value_lo: ReadOnly<u32>),
        (0x048 => @END),
    }
}

register_bitfields![u32,
    UNIT_OP [
        /// The latched value registers hold the counter's value.
        VALUE_VALID OFFSET(29) NUMBITS(1) [],
        /// Latch the counter into the value registers.
        UPDATE OFFSET(30) NUMBITS(1) [],
    ],
];

pub struct SysTimer {
    registers: StaticRef<SysTimerRegisters>,
}

impl SysTimer {
    pub const fn new() -> Self {
        SysTimer {
            registers: SYSTIMER_BASE,
        }
    }

    /// The current value of unit 0's counter, in ticks.
    pub fn unit0_ticks(&self) -> u64 {
        self.registers.unit0_op.write(UNIT_OP::UPDATE::SET);
        while !self.registers.unit0_op.is_set(UNIT_OP::VALUE_VALID) {}
        let hi = self.registers.unit0_value_hi.get() as u64;
        let lo = self.registers.unit0_value_lo.get() as u64;
        (hi << 32) | lo
    }

    /// Microseconds since boot, from unit 0.
    pub fn unit0_us(&self) -> u64 {
        self.unit0_ticks() / TICKS_PER_US
    }
}
