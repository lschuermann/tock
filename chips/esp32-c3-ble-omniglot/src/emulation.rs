// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Emulation of the sandbox's loads and stores that fault on the PMP.
//!
//! The PMP gives the sandbox direct access only to the peripheral pages it
//! uses most (see `pmp_layout`). Its other peripheral accesses fault. The
//! Omniglot runtime then asks [`Emulation`] whether the kernel may perform
//! the access. If it may, the kernel performs it and the sandbox continues.
//! Otherwise the call into the sandbox ends with a fault.
//!
//! The kernel performs these accesses:
//!
//! - Any access to the peripheral ranges in `EMULATE_MMIO`.
//! - The sandbox's `interrupt_alloc` mapping a BT interrupt source to the
//!   CPU line of the same number.
//! - Stores to the baseband's exchange-memory (EM) mapping registers that
//!   keep every enabled window inside the sandbox's RAM (see below).
//!
//! Flash (SPI0/1), GDMA, permission control, the rest of the interrupt
//! matrix, the cache/MMU and the world controller are not on the list.
//!
//! # EM mapping registers
//!
//! The baseband reaches SRAM only through the EM mapping. The 16-bit EM
//! offset space is split into slots. Each slot's register holds the SRAM
//! address its window maps to, and a bit in one of three bitmaps enables the
//! slot. An enabled slot lets the baseband's DMA read and write its window.
//! The sandbox may read these registers directly. Its stores fault, and the
//! kernel performs a store only under these rules:
//!
//! - A store to a slot register is allowed if the slot is disabled, or if
//!   the new value maps a safe window.
//! - A store to an enable bitmap is allowed if every slot it newly enables
//!   already maps a safe window. Disabling slots is always allowed.
//! - Only 4-byte stores are allowed.
//!
//! A safe window belongs to a slot the controller uses (0-38). It is at the
//! slot's EM offset from the ROM's `em_base_reg_lut`, and lies entirely in
//! the sandbox's RAM.

use core::cell::Cell;

use kernel::utilities::StaticRef;
use kernel::utilities::registers::interfaces::{Readable, Writeable};
use kernel::utilities::registers::{
    LocalRegisterCopy, ReadWrite, register_bitfields, register_structs,
};

use omniglot_tock::rv32i_c_rt::AccessEmulation;

use super::{BT_SOURCES, IRAM_DBUS_OFFSET};

/// Peripheral ranges (start, end) whose accesses are emulated.
///
/// The blocks a static scan of the archives and the ROM's BT code found,
/// plus the glue's (see OMNIGLOT_BLE_PLAN.md, section 6). None of them is
/// accessed more than ~10 times/s after the bring-up.
const EMULATE_MMIO: &[(usize, usize)] = &[
    // GPIO (debug pin), FE2, FE (RF front end).
    (0x6000_4000, 0x6000_7000),
    // RTC_CNTL (modem power), eFuse (MAC).
    (0x6000_8000, 0x6000_9000),
    // RTC_I2C (analog registers).
    (0x6000_E000, 0x6000_F000),
    // AGC, NRX, BB (receive path).
    (0x6001_C000, 0x6001_E000),
    // SYSTIMER (`get_time_us`).
    (0x6002_3000, 0x6002_4000),
    // SYSCON (modem clocks and reset, RNG).
    (0x6002_6000, 0x6002_7000),
    // RWBT.
    (0x6003_0000, 0x6003_1000),
    // Wi-Fi MAC: TX channel offset (`rom_mac_tx_chan_offset`, PHY
    // bring-up) and the coexistence registers only.
    (0x6003_3400, 0x6003_3404),
    (0x6003_3C18, 0x6003_3C1C),
    (0x6003_3C68, 0x6003_3C70),
    // Coexistence.
    (0x6003_5000, 0x6003_6000),
    // APB_SARADC (temperature sensor).
    (0x6004_0000, 0x6004_1000),
    // Sleep/wakeup timer.
    (0x6004_2000, 0x6004_3000),
    // SYSTEM (low-power clock).
    (0x600C_0000, 0x600C_1000),
];

/// The interrupt matrix's map registers, one word per interrupt source.
const INTMATRIX_BASE: usize = 0x600C_2000;

/// The EM mapping registers, which the PMP makes read-only for the sandbox.
///
/// As (start, end): slot registers 0-47 with the enable bitmaps for slots
/// 0-31 and 32-47, and slot registers 48-55 with the bitmap for slots 48-55.
/// The RWBLE registers between and after the two banks are written per
/// packet and stay directly writable.
pub const EM_BANKS: [(usize, usize); 2] = [(0x6003_1204, 0x6003_12CC), (0x6003_12E0, 0x6003_1304)];

const EM_SLOT_LO: usize = 0x6003_1204;
const EM_ENABLE_LO: usize = 0x6003_12C4;
const EM_ENABLE_MID: usize = 0x6003_12C8;
const EM_SLOT_HI: usize = 0x6003_12E0;
const EM_ENABLE_HI: usize = 0x6003_1300;

register_structs! {
    EmRegisters {
        (0x000 => _reserved0),
        /// Slot registers 0-47.
        (0x204 => slot_lo: [ReadWrite<u32, SLOT::Register>; 48]),
        /// Enable bits for slots 0-31.
        (0x2C4 => enable_lo: ReadWrite<u32>),
        /// Enable bits for slots 32-47.
        (0x2C8 => enable_mid: ReadWrite<u32>),
        (0x2CC => _reserved1),
        /// Slot registers 48-55.
        (0x2E0 => slot_hi: [ReadWrite<u32, SLOT::Register>; 8]),
        /// Enable bits for slots 48-55.
        (0x300 => enable_hi: ReadWrite<u32>),
        (0x304 => @END),
    }
}

register_bitfields![u32,
    SLOT [
        /// SRAM word address of the window, relative to `SRAM_BASE`.
        ADDR OFFSET(0) NUMBITS(18) [],
        /// The slot's EM offset, in words.
        EM_OFFSET OFFSET(18) NUMBITS(14) []
    ]
];

// ### Safety
//
// The RWBLE block's EM mapping registers, at this fixed address on the
// ESP32-C3. The kernel writes them only in this module.
const EM_BASE: StaticRef<EmRegisters> =
    unsafe { StaticRef::new(0x6003_1000 as *const EmRegisters) };

/// Slots the controller maps (`em_base_init`).
const MAPPED_SLOTS: usize = 39;
/// What `ADDR` is relative to: the baseband reaches the 1 MiB from here.
const SRAM_BASE: usize = 0x3FC0_0000;

/// One entry of the ROM's `em_base_reg_lut`, for one 1 KiB of EM.
#[repr(C)]
struct EmLutEntry {
    /// The slot this 1 KiB of EM belongs to.
    slot: u8,
    _unused: u8,
    /// The EM offset of that slot.
    offset: u16,
}

const EM_GRANULES: usize = 51;
const EM_GRANULE: usize = 1024;

// ### Safety
//
// The ROM's `em_base_reg_lut`, in mask ROM data (readable through the DROM
// alias). It never changes.
const EM_BASE_REG_LUT: StaticRef<[EmLutEntry; EM_GRANULES]> =
    unsafe { StaticRef::new(0x3FF1_F518 as *const [EmLutEntry; EM_GRANULES]) };

/// (EM offset, window size) of slot `slot` < `MAPPED_SLOTS`, as in the ROM's
/// `em_base_reg_lut` (`Emulation::new` compares them): slots 1, 4 and 7-16
/// span 2 KiB, the others 1 KiB.
const fn slot_geometry(slot: usize) -> (usize, usize) {
    const K: usize = EM_GRANULE;
    match slot {
        0 => (0, K),
        1 => (K, 2 * K),
        2 => (3 * K, K),
        3 => (4 * K, K),
        4 => (5 * K, 2 * K),
        5 => (7 * K, K),
        6 => (8 * K, K),
        7..=16 => (9 * K + (slot - 7) * 2 * K, 2 * K),
        _ => (29 * K + (slot - 17) * K, K),
    }
}

/// Decides which of the sandbox's faulting accesses the kernel performs.
pub struct Emulation {
    em: StaticRef<EmRegisters>,
    /// The sandbox's `.iram1` code, (start, end), at its instruction-bus
    /// alias.
    iram: (usize, usize),
    /// The sandbox's RAM, (start, end), the only SRAM an EM window may
    /// cover.
    ram: (usize, usize),
    /// The last refused access: its address, and the value of a store.
    refused: Cell<Option<(usize, Option<usize>)>>,
}

impl Emulation {
    /// Fails if the ROM's EM slot table does not match `slot_geometry`.
    pub fn new(iram: (usize, usize), ram: (usize, usize)) -> Result<Self, &'static str> {
        let mut size = [0usize; MAPPED_SLOTS];
        for entry in EM_BASE_REG_LUT.iter() {
            let (slot, offset) = (entry.slot as usize, entry.offset as usize);
            if slot >= MAPPED_SLOTS || slot_geometry(slot).0 != offset {
                return Err("EM slot table does not match the ROM");
            }
            size[slot] += EM_GRANULE;
        }
        if (0..MAPPED_SLOTS).any(|slot| size[slot] != slot_geometry(slot).1) {
            return Err("EM slot sizes do not match the ROM");
        }
        Ok(Emulation {
            em: EM_BASE,
            iram,
            ram,
            refused: Cell::new(None),
        })
    }

    /// Take the last refused access: its address, and the value of a store.
    pub fn take_refused(&self) -> Option<(usize, Option<usize>)> {
        self.refused.take()
    }

    /// Disable every EM window, so that the baseband's DMA no longer reaches
    /// SRAM.
    pub fn disable_em_windows(&self) {
        self.em.enable_lo.set(0);
        self.em.enable_mid.set(0);
        self.em.enable_hi.set(0);
    }

    /// Whether to perform an access outside the EM mapping registers.
    fn allow_mmio(&self, addr: usize, width: usize, store: Option<usize>) -> bool {
        let listed = EMULATE_MMIO
            .iter()
            .any(|&(s, e)| addr >= s && addr + width <= e);
        let intmatrix = width == 4
            && BT_SOURCES.iter().any(|&src| {
                addr == INTMATRIX_BASE + 4 * src as usize && store.is_none_or(|v| v == src as usize)
            });
        listed || intmatrix
    }

    /// Whether to perform a 4-byte store of `value` to `addr`, one of the EM
    /// mapping registers.
    fn allow_em_store(&self, addr: usize, value: u32) -> bool {
        match addr {
            EM_SLOT_LO..EM_ENABLE_LO | EM_SLOT_HI..EM_ENABLE_HI if addr.is_multiple_of(4) => {
                let slot = if addr < EM_ENABLE_LO {
                    (addr - EM_SLOT_LO) / 4
                } else {
                    48 + (addr - EM_SLOT_HI) / 4
                };
                !self.enabled(slot) || self.safe(slot, value)
            }
            EM_ENABLE_LO | EM_ENABLE_MID | EM_ENABLE_HI => {
                let (bitmap, first, count) = match addr {
                    EM_ENABLE_LO => (&self.em.enable_lo, 0, 32),
                    EM_ENABLE_MID => (&self.em.enable_mid, 32, 16),
                    _ => (&self.em.enable_hi, 48, 8),
                };
                let newly_enabled = value & !bitmap.get();
                (0..32)
                    .filter(|bit| newly_enabled & (1 << bit) != 0)
                    .all(|bit| bit < count && self.safe(first + bit, self.slot(first + bit).get()))
            }
            _ => false,
        }
    }

    fn slot(&self, slot: usize) -> &ReadWrite<u32, SLOT::Register> {
        if slot < 48 {
            &self.em.slot_lo[slot]
        } else {
            &self.em.slot_hi[slot - 48]
        }
    }

    fn enabled(&self, slot: usize) -> bool {
        let (bitmap, first) = match slot {
            0..32 => (&self.em.enable_lo, 0),
            32..48 => (&self.em.enable_mid, 32),
            _ => (&self.em.enable_hi, 48),
        };
        bitmap.get() & (1 << (slot - first)) != 0
    }

    /// Whether slot register value `value` maps a safe window for `slot`.
    fn safe(&self, slot: usize, value: u32) -> bool {
        if slot >= MAPPED_SLOTS {
            return false;
        }
        let (em_offset, size) = slot_geometry(slot);
        let value = LocalRegisterCopy::<u32, SLOT::Register>::new(value);
        let start = SRAM_BASE + 4 * value.read(SLOT::ADDR) as usize;
        4 * value.read(SLOT::EM_OFFSET) as usize == em_offset
            && start >= self.ram.0
            && start + size <= self.ram.1
    }
}

impl AccessEmulation for Emulation {
    fn allow(&self, addr: usize, width: usize, store: Option<usize>) -> bool {
        let em = EM_BANKS.iter().any(|&(s, e)| (s..e).contains(&addr));
        let allowed = if em {
            width == 4 && store.is_some_and(|value| self.allow_em_store(addr, value as u32))
        } else {
            self.allow_mmio(addr, width, store)
        };
        if !allowed {
            self.refused.set(Some((addr, store)));
        }
        allowed
    }

    /// The sandbox's `.iram1` runs through SRAM's instruction-bus alias;
    /// the kernel reads it through the data-bus alias.
    fn code_address(&self, pc: usize) -> usize {
        if (self.iram.0..self.iram.1).contains(&pc) {
            pc - IRAM_DBUS_OFFSET
        } else {
            pc
        }
    }
}
