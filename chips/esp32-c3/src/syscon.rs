// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! SYSCON (APB control): clocks and resets of the Wi-Fi and Bluetooth modem
//! blocks.

use kernel::utilities::StaticRef;
use kernel::utilities::registers::interfaces::ReadWriteable;
use kernel::utilities::registers::{ReadWrite, register_bitfields, register_structs};

pub const SYSCON_BASE: StaticRef<SysconRegisters> =
    unsafe { StaticRef::new(0x6002_6000 as *const SysconRegisters) };

register_structs! {
    pub SysconRegisters {
        (0x000 => _reserved0),
        (0x018 => wifi_rst_en: ReadWrite<u32, WIFI_RST_EN::Register>),
        (0x01C => @END),
    }
}

register_bitfields![u32,
    WIFI_RST_EN [
        WIFIBB OFFSET(0) NUMBITS(1) [],
        FE OFFSET(1) NUMBITS(1) [],
        WIFIMAC OFFSET(2) NUMBITS(1) [],
        BTBB OFFSET(3) NUMBITS(1) [],
        BTMAC OFFSET(4) NUMBITS(1) [],
        RW_BTMAC OFFSET(9) NUMBITS(1) [],
        RW_BTMAC_REG OFFSET(11) NUMBITS(1) [],
        BTBB_REG OFFSET(13) NUMBITS(1) [],
    ],
];

pub struct Syscon {
    registers: StaticRef<SysconRegisters>,
}

impl Syscon {
    pub const fn new() -> Self {
        Syscon {
            registers: SYSCON_BASE,
        }
    }

    /// Hold the modem blocks that reset when the modem's power domain comes
    /// up in reset: the Wi-Fi and BT basebands and MACs and the RF front end.
    /// ESP-IDF calls this set `MODEM_RESET_FIELD_WHEN_PU`.
    pub fn hold_modem_in_reset(&self) {
        self.registers.wifi_rst_en.modify(
            WIFI_RST_EN::WIFIBB::SET
                + WIFI_RST_EN::FE::SET
                + WIFI_RST_EN::WIFIMAC::SET
                + WIFI_RST_EN::BTBB::SET
                + WIFI_RST_EN::BTMAC::SET
                + WIFI_RST_EN::RW_BTMAC::SET
                + WIFI_RST_EN::RW_BTMAC_REG::SET
                + WIFI_RST_EN::BTBB_REG::SET,
        );
    }
}
