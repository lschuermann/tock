use core::cell::Cell;
use core::fmt;
use kernel::utilities::registers::FieldValue;
use rv32i::csr;
use rv32i::pmp::{format_pmp_entries, pmpcfg_octet, TORUserPMP, TORUserPMPCFG};

pub const PMP_ENTRIES_OVER_TWO: usize = 8;
pub const TOR_USER_REGIONS: usize = 5;
pub const TOR_USER_ENTRIES_OFFSET: usize = 0;

/// RISC-V ePMP memory protection implementation for the EarlGrey SoC.
///
/// The EarlGrey ePMP implementation hard-codes many assumptions about the
/// behavior and state of the underlying hardware, to reduce complexity of this
/// codebase, and improve security and auditability.
///
/// Namely, it makes and checks assumptions about the machine security policy
/// prior to its initialization routine, locks down the hardware through a
/// static set of PMP configuration steps, and then exposes a subset of regions
/// for user-mode protection through the `PMPUserMPU` trait.
///
/// ## ePMP Region Layout
///
/// The EarlGrey ePMP driver attempts to set up the following memory protection
/// rules and layout:
///
/// - `msseccfg` CSR:
///
///   ```
///   |-----+--------------------------------------+-------|
///   | BIT | LABEL                                | STATE |
///   |-----+--------------------------------------+-------|
///   |   0 | Machine-Mode Lockdown (MML)          |     1 |
///   |   1 | Machine-Mode Whitelist Policy (MMWP) |     1 |
///   |   2 | Rule-Lock Bypass (RLB)               |     0 |
///   |-----+--------------------------------------+-------|
///   ```
///
/// - `pmpcfgX` / `pmpaddrX` CSRs:
///
///   ```
///   |-------+--------------------------------+-------+---+-------|
///   | ENTRY | REGION / ADDR                  | MODE  | L | PERMS |
///   |-------+--------------------------------+-------+---+-------|
///   |     0 | /                         \    |       |   |       |
///   |     1 | \ Userspace TOR region #0 /    |       |   |       |
///   |     2 | /                         \    |       |   |       |
///   |     3 | \ Userspace TOR region #1 /    |       |   |       |
///   |     4 | /                         \    |       |   |       |
///   |     5 | \ Userspace TOR region #2 /    |       |   |       |
///   |     6 | /                         \    |       |   |       |
///   |     7 | \ Userspace TOR region #3 /    |       |   |       |
///   |     8 | /                         \    |       |   |       |
///   |     9 | \ Userspace TOR region #4 /    |       |   |       |
///   |    10 | free                           |       |   |       |
///   |    11 | ------------------------------ | OFF   | X | ----- |
///   |    12 | Kernel .text section           | TOR   | X | R/X   |
///   |    13 | FLASH (spanning kernel & apps) | NAPOT | X | R     |
///   |    14 | RAM (spanning kernel & apps)   | NAPOT | X | R/W   |
///   |    15 | MMIO                           | NAPOT | X | R/W   |
///   |-------+--------------------------------+-------+---+-------|
///   ```
/// - 2nd flash bank
/// - retention RAM
pub struct EarlGreyEPMP {
    user_pmp_enabled: Cell<bool>,
    // TODO: maybe optimized way
    shadow_user_pmpcfgs: [Cell<TORUserPMPCFG>; TOR_USER_REGIONS],
}

pub struct FlashNAPOTRegion {
    pub start: *const u8,
    pub size: usize,
}

pub struct RAMNAPOTRegion {
    pub start: *const u8,
    pub size: usize,
}

impl EarlGreyEPMP {
    pub unsafe fn new(
        flash: (*const u8, usize),
        ram: (*const u8, usize),
        mmio: (*const u8, usize),
        kernel_text: (*const u8, *const u8),
	debug_memory: Option<(*const u8, usize)>,
    ) -> Result<Self, ()> {
        use kernel::utilities::registers::interfaces::{Readable, Writeable};

        // This initialization code is written to work with 16 PMP entries. Add
        // an explicit assertion such that things break when the constant above
        // is changed:
        const _: () = assert!(
            PMP_ENTRIES_OVER_TWO == 8,
            "EarlGrey ePMP initialization is written for 16 PMP entries.",
        );

        // Perform all parameter validation before touching any PMP CSRs.
        //
        // The main flash section will be configured as a NAPOT region,
        // requiring its size to be a power of two, its base address to be
        // aligned to its size, and it must be at least 8 bytes in size.
        if !flash.1.is_power_of_two() || (flash.0 as usize) % flash.1 != 0 || flash.1 < 8 {
            return Err(());
        }

        // The RAM section will be configured as a NAPOT region, requiring its
        // size to be a power of two, its base address to be aligned to its
        // size, and it must be at least 8 bytes long.
        if !ram.1.is_power_of_two() || (ram.0 as usize) % ram.1 != 0 || ram.1 < 8 {
            return Err(());
        }

        // The MMIO section will be configured as a NAPOT region, requiring its
        // size to be a power of two, its base address to be aligned to its
        // size, and it must be at least 8 bytes long.
        if !mmio.1.is_power_of_two() || (mmio.0 as usize) % mmio.1 != 0 || mmio.1 < 8 {
            return Err(());
        }

        // The kernel flash text section will be configured as a TOR region,
        // requiring 4-byte alignment on its start and end address, and it being
        // at least 4 bytes long:
        if (kernel_text.0 as usize) % 4 != 0
            || (kernel_text.1 as usize) % 4 != 0
            || (kernel_text.1 as usize)
                .checked_sub(kernel_text.0 as usize)
                .map_or(true, |size| size < 4)
        {
            return Err(());
        }

        // ---------- Parameters validated, check current HW config

        // Ensure that the `mseccfg` CSR has the expected value, namely that
        // we're in "machine-mode whitelist policy" and have "rule-lock bypass"
        // enabled. If this register has an unexpected value, we risk
        // accidentally revoking important permissions for the Tock kernel
        // itself.
        if csr::CSR.mseccfg.get() != 0x00000006 {
            return Err(());
        }

        // We assume the very last PMP region is set to provide us RXW access to
        // the entirety of memory, and all other regions are disabled. Check the
        // CSRs to make sure that this is indeed the case.
        for i in 0..(PMP_ENTRIES_OVER_TWO / 2 - 1) {
            // 0x98 = 0b10011000, extracting L(7) and A(4-3) bits.
            if csr::CSR.pmpconfig_get(i) & 0x98989898 != 0x00000000 {
                return Err(());
            }
        }

        // The last CSR is special, as we expect it to contain the NAPOT region
        // which currently gives us memory access.
        //
        // 0x98 = 0b10011000, extracting L(7) and A(4-3) bits.
        // 0x9F = 0b10011111, extracing L(7), A(4-3), X(2), W(1), R(0) bits.
        if csr::CSR.pmpconfig_get(PMP_ENTRIES_OVER_TWO / 2 - 1) & 0x9F989898 != 0x9F000000 {
            return Err(());
        }

        // ---------- HW configured as expected, start setting PMP CSRs

        // This helper requires that the address and size are already compliant
        // to NAPOT alignment constraints.
        #[inline(always)]
        fn napot_addr(start: *const u8, size: usize) -> usize {
            ((start as usize) + (size - 1).overflowing_shr(1).0)
                .overflowing_shr(2)
                .0
        }

        // The below instructions are an intricate dance to achieve our desired
        // ePMP configuration. For correctness sake, we -- at no intermediate
        // point -- want to lose access to RAM, FLASH or MMIO.
        //
        // This is challenging, as the last section currently provides us access
        // to all of these regions, and we can't atomically change both its
        // pmpaddrX and pmpcfgX CSRs to limit it to a subset of its address
        // range. Thus, we first utilize another higher-priority CSR to provide
        // us access to one of the memory regions we'd lose access to, namely
        // we use the PMP entry 10 to provide us access to MMIO.
        //
        // Thus, configure this address here. Delay writing the pmpcfgX CSR
        // until after we've written the start of the kernel text section to
        // pmpaddr11.
        csr::CSR.pmpaddr10.set(napot_addr(mmio.0, mmio.1));

        // Now, provide R/X access to the kernel flash section, as passed to us
        // above. We already validated that it meets PMP alignment constraints.
        // Allocate a TOR region in PMP entries 11-12.
        csr::CSR.pmpaddr11.set((kernel_text.0 as usize) >> 2);
        csr::CSR.pmpaddr12.set((kernel_text.1 as usize) >> 2);

        // Set the pmpcfg2 CSR to temporarily give us MMIO access. We delay
        // setting the pmpcfg3 CSR until we have set all later regions, as we
        // would invalidate the last region which is currently giving us R/W on
        // RAM.
        //
        // 0x80 = 0b10000000, for start address of the kernel .text TOR entry
        //        setting L(7) = 1, A(4-3) = OFF, X(2) = 0, W(1) = 0, R(0) = 0
        //
        // 0x9B = 0b10011011, for MMIO NAPOT region
        //        setting L(7) = 1, A(4-3) = NAPOT, X(2) = 0, W(1) = 1, R(0) = 1
        csr::CSR.pmpcfg2.set(0x809B0000);

        // Configure a Read-Only NAPOT region for the entire flash (spanning
        // kernel & apps, but overlayed by the R/X kernel text TOR section)
        csr::CSR.pmpaddr13.set(napot_addr(flash.0, flash.1));

        // Configure a Read-Write NAPOT region for the entire RAM (spanning
        // kernel & apps)
        csr::CSR.pmpaddr14.set(napot_addr(ram.0, ram.1));

        // Now, change the pmpcfg3 to activate the kernel text, flash and RAM
        // sections. We further set R/W permissions on the last region, spanning
        // the entire address space, as its address is going to be set to a
        // NAPOT MMIO region. This should be fine, as we just now configured RAM
        // & FLASH, and still have MMIO accessible through PMP entry 10.
        //
        // 0x8d = 0b10001101, for kernel .text TOR region
        //        setting L(7) = 1, A(4-3) = TOR,   X(2) = 1, W(1) = 0, R(0) = 1
        //
        // 0x99 = 0b10011001, for FLASH NAPOT region
        //        setting L(7) = 1, A(4-3) = NAPOT, X(2) = 0, W(1) = 0, R(0) = 1
        //
        // 0x9B = 0b10011011, for RAM & MMIO NAPOT regions
        //        setting L(7) = 1, A(4-3) = NAPOT, X(2) = 0, W(1) = 1, R(0) = 1
        csr::CSR.pmpcfg3.set(0x9B9B998D);

        // Finally, set the last region's address to MMIO...
        csr::CSR.pmpaddr15.set(napot_addr(mmio.0, mmio.1));

	if let Some((debug_mem_addr, debug_mem_size)) = debug_memory {
	    // ...and reconfigure the temporary MMIO region from PMP entry 10 to
	    // give permissions to the debug module memory:
	    csr::CSR.pmpaddr10.set(napot_addr(debug_mem_addr, debug_mem_size));

	    // 0x80 = 0b10000000, for kernel .text TOR region start address
            //        setting L(7) = 1, A(4-3) = OFF,   X(2) = 0, W(1) = 0, R(0) = 0
            //
            // 0x9F = 0b10011111, for FLASH NAPOT region
            //        setting L(7) = 1, A(4-3) = NAPOT, X(2) = 1, W(1) = 1, R(0) = 1
	    csr::CSR.pmpcfg2.set(0x809F0000);
	} else {
            // ...and remove the temporary MMIO region from PMP entry 10:
	    //
	    // 0x80 = 0b10000000, for kernel .text TOR region start address
            //        setting L(7) = 1, A(4-3) = OFF,   X(2) = 0, W(1) = 0, R(0) = 0
            csr::CSR.pmpcfg2.set(0x80000000);
	}

        // Ensure that the other pmpcfgX CSRs are cleared:
        csr::CSR.pmpcfg1.set(0x00000000);
        csr::CSR.pmpcfg0.set(0x00000000);

        // ---------- PMP machine CSRs configured, lock down the system

        // Finally, unset the rule-lock bypass (RLB) bit. If we don't have a
        // debug memory region provided, further set machine-mode lockdown (we
        // can't enable MML and also have an RWX region). We also set MMWP, but
        // it can't be cleared anyways as it is a sticky bit.
        //
        // Unsetting RLB with at least one locked region will mean that we can't
        // set it again, thus actually enforcing the region lock bits.
	if debug_memory.is_some() {
	    csr::CSR.mseccfg.set(0x00000002);
	} else {
            csr::CSR.mseccfg.set(0x00000003);
	}

        // ---------- System locked down, cross-check config

        // Now, cross-check that the CSRs have the expected values. This acts as
        // a sanity check, and can also help to protect against some set of
        // fault-injection attacks. These checks can't be optimized out by the
        // compiler, as they invoke assembly underneath which is not marked as
        // ["pure"](https://doc.rust-lang.org/reference/inline-assembly.html).
        // if csr::CSR.mseccfg.get() != 0x00000003
        //     || csr::CSR.pmpcfg0.get() != 0x00000000
        //     || csr::CSR.pmpcfg1.get() != 0x00000000
        //     || csr::CSR.pmpcfg2.get() != 0x80000000
        //     || csr::CSR.pmpcfg3.get() != 0x9B9B998D
        //     || csr::CSR.pmpaddr11.get() != (kernel_text.0 as usize) >> 2
        //     || csr::CSR.pmpaddr12.get() != (kernel_text.1 as usize) >> 2
        //     || csr::CSR.pmpaddr13.get() != napot_addr(flash.0, flash.1)
        //     || csr::CSR.pmpaddr14.get() != napot_addr(ram.0, ram.1)
        //     || csr::CSR.pmpaddr15.get() != napot_addr(mmio.0, mmio.1)
        // {
        //     return Err(());
        // }

        // The ePMP hardware was correctly configured, return the ePMP struct:

        const DEFAULT_USER_PMPCFG_OCTET: Cell<TORUserPMPCFG> = Cell::new(TORUserPMPCFG::OFF);
        Ok(EarlGreyEPMP {
            user_pmp_enabled: Cell::new(false),
            shadow_user_pmpcfgs: [DEFAULT_USER_PMPCFG_OCTET; TOR_USER_REGIONS],
        })
    }
}

impl fmt::Display for EarlGreyEPMP {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use kernel::utilities::registers::interfaces::Readable;

        write!(f, " EarlGrey ePMP configuration:\r\n")?;
        write!(
            f,
            "  mseccfg: {:#08X}, user-mode PMP active: {:?}\r\n",
            csr::CSR.mseccfg.get(),
            self.user_pmp_enabled.get()
        )?;
        unsafe { format_pmp_entries::<PMP_ENTRIES_OVER_TWO>(f) }?;

        write!(f, "  Shadow PMP entries for user-mode:\r\n")?;
        for (i, shadowed_pmpcfg) in self.shadow_user_pmpcfgs.iter().enumerate() {
            let (start_pmpaddr_label, startaddr_pmpaddr, endaddr, mode) =
                if shadowed_pmpcfg.get() == TORUserPMPCFG::OFF {
                    (
                        "pmpaddr",
                        csr::CSR.pmpaddr_get(TOR_USER_ENTRIES_OFFSET + (i * 2)),
                        0,
                        "OFF",
                    )
                } else {
                    (
                        "  start",
                        csr::CSR
                            .pmpaddr_get(TOR_USER_ENTRIES_OFFSET + (i * 2))
                            .overflowing_shl(2)
                            .0,
                        csr::CSR
                            .pmpaddr_get(TOR_USER_ENTRIES_OFFSET + (i * 2) + 1)
                            .overflowing_shl(2)
                            .0
                            | 0b11,
                        "TOR",
                    )
                };

            write!(
                f,
                "  [{:02}]: {}={:#010X}, end={:#010X}, cfg={:#04X} ({}) ({}{}{}{})\r\n",
                TOR_USER_ENTRIES_OFFSET + (i * 2) + 1,
                start_pmpaddr_label,
                startaddr_pmpaddr,
                endaddr,
                shadowed_pmpcfg.get().get(),
                mode,
                if shadowed_pmpcfg.get().get_reg().is_set(pmpcfg_octet::l) {
                    "l"
                } else {
                    "-"
                },
                if shadowed_pmpcfg.get().get_reg().is_set(pmpcfg_octet::r) {
                    "r"
                } else {
                    "-"
                },
                if shadowed_pmpcfg.get().get_reg().is_set(pmpcfg_octet::w) {
                    "w"
                } else {
                    "-"
                },
                if shadowed_pmpcfg.get().get_reg().is_set(pmpcfg_octet::x) {
                    "x"
                } else {
                    "-"
                },
            )?;
        }

        Ok(())
    }
}

impl TORUserPMP<{ TOR_USER_REGIONS }> for EarlGreyEPMP {
    // Don't require any const-assertions in the EarlGreyEPMP.
    const CONST_ASSERT_CHECK: () = ();

    fn available_regions(&self) -> usize {
        // Always assume to have `MPU_REGIONS` usable TOR regions. We have a
        // fixed number of kernel memory protection regions, and a fixed mapping
        // of user regions to hardware PMP entries.
        TOR_USER_REGIONS
    }

    fn configure_pmp(
        &self,
        regions: &[(TORUserPMPCFG, *const u8, *const u8); TOR_USER_REGIONS],
    ) -> Result<(), ()> {
        // Configure all of the regions' addresses and store their pmpcfg octets
        // in our shadow storage. If the user PMP is already enabled, we further
        // apply this configuration (set the pmpcfgX CSRs) by running
        // `enable_user_pmp`:
        for (i, (region, shadow_user_pmpcfg)) in regions
            .iter()
            .zip(self.shadow_user_pmpcfgs.iter())
            .enumerate()
        {
            // Set the CSR addresses for this region (if its not OFF, in which
            // case the hardware-configured addresses are irrelevant):
            if region.0 != TORUserPMPCFG::OFF {
                csr::CSR.pmpaddr_set(
                    TOR_USER_ENTRIES_OFFSET + (i * 2) + 0,
                    (region.1 as usize).overflowing_shr(2).0,
                );
                csr::CSR.pmpaddr_set(
                    TOR_USER_ENTRIES_OFFSET + (i * 2) + 1,
                    (region.2 as usize).overflowing_shr(2).0,
                );
            }

            // Store the region's pmpcfg octet:
            shadow_user_pmpcfg.set(region.0);
        }

        // If the PMP is currently active, apply the changes to the CSRs:
        if self.user_pmp_enabled.get() {
            self.enable_user_pmp()?;
        }

        Ok(())
    }

    fn enable_user_pmp(&self) -> Result<(), ()> {
        // Currently, this code requires the TOR regions to start at an even PMP
        // region index. Assert that this is indeed the case:
        let _: () = assert!(TOR_USER_ENTRIES_OFFSET % 2 == 0);

        // We store the "enabled" PMPCFG octets of user regions in the
        // `shadow_user_pmpcfg` field, such that we can re-enable the PMP
        // without a call to `configure_pmp` (where the `TORUserPMPCFG`s are
        // provided by the caller).

        // Could use `iter_array_chunks` once that's stable.
        let mut shadow_user_pmpcfgs_iter = self.shadow_user_pmpcfgs.iter();
        let mut i = TOR_USER_ENTRIES_OFFSET / 2;

        while let Some(first_region_pmpcfg) = shadow_user_pmpcfgs_iter.next() {
            let second_region_opt = if i % 2 == 0 {
                shadow_user_pmpcfgs_iter.next()
            } else {
                None
            };

            if let Some(second_region_pmpcfg) = second_region_opt {
                // We're at an even index and have two regions to configure, so
                // do that with a single CSR write:
                csr::CSR.pmpconfig_set(
                    i / 2,
                    u32::from_be_bytes([
                        second_region_pmpcfg.get().get(),
                        TORUserPMPCFG::OFF.get(),
                        first_region_pmpcfg.get().get(),
                        TORUserPMPCFG::OFF.get(),
                    ]) as usize,
                );

                i += 2;
            } else if i % 2 == 0 {
                // This is a single region at an even index. Thus, modify the
                // first two pmpcfgX octets for this region.
                csr::CSR.pmpconfig_modify(
                    i / 2,
                    FieldValue::<usize, csr::pmpconfig::pmpcfg::Register>::new(
                        0x0000FFFF,
                        0, // lower two octets
                        u32::from_be_bytes([
                            0,
                            0,
                            first_region_pmpcfg.get().get(),
                            TORUserPMPCFG::OFF.get(),
                        ]) as usize,
                    ),
                );

                i += 1;
            } else {
                // This is a single region at an odd index. Thus, modify the
                // latter two pmpcfgX octets for this region.
                csr::CSR.pmpconfig_modify(
                    i / 2,
                    FieldValue::<usize, csr::pmpconfig::pmpcfg::Register>::new(
                        0x0000FFFF,
                        16, // higher two octets
                        u32::from_be_bytes([
                            0,
                            0,
                            first_region_pmpcfg.get().get(),
                            TORUserPMPCFG::OFF.get(),
                        ]) as usize,
                    ),
                );

                i += 1;
            }
        }

        self.user_pmp_enabled.set(true);

        Ok(())
    }

    fn disable_user_pmp(&self) {
        // Simply set all of the user-region pmpcfg octets to OFF:
        let mut user_region_pmpcfg_octet_pairs = (TOR_USER_ENTRIES_OFFSET / 2)
            ..((TOR_USER_ENTRIES_OFFSET / 2) + self.shadow_user_pmpcfgs.len());

        while let Some(first_region_idx) = user_region_pmpcfg_octet_pairs.next() {
            let second_region_opt = if first_region_idx % 2 == 0 {
                user_region_pmpcfg_octet_pairs.next()
            } else {
                None
            };

            if let Some(_second_region_idx) = second_region_opt {
                // We're at an even index and have two regions to configure, so
                // do that with a single CSR write:
                csr::CSR.pmpconfig_set(
                    first_region_idx / 2,
                    u32::from_be_bytes([
                        TORUserPMPCFG::OFF.get(),
                        TORUserPMPCFG::OFF.get(),
                        TORUserPMPCFG::OFF.get(),
                        TORUserPMPCFG::OFF.get(),
                    ]) as usize,
                );
            } else if first_region_idx % 2 == 0 {
                // This is a single region at an even index. Thus, modify the
                // first two pmpcfgX octets for this region.
                csr::CSR.pmpconfig_modify(
                    first_region_idx / 2,
                    FieldValue::<usize, csr::pmpconfig::pmpcfg::Register>::new(
                        0x0000FFFF,
                        0, // lower two octets
                        u32::from_be_bytes([
                            0,
                            0,
                            TORUserPMPCFG::OFF.get(),
                            TORUserPMPCFG::OFF.get(),
                        ]) as usize,
                    ),
                );
            } else {
                // This is a single region at an odd index. Thus, modify the
                // latter two pmpcfgX octets for this region.
                csr::CSR.pmpconfig_modify(
                    first_region_idx / 2,
                    FieldValue::<usize, csr::pmpconfig::pmpcfg::Register>::new(
                        0x0000FFFF,
                        16, // higher two octets
                        u32::from_be_bytes([
                            0,
                            0,
                            TORUserPMPCFG::OFF.get(),
                            TORUserPMPCFG::OFF.get(),
                        ]) as usize,
                    ),
                );
            }
        }

        self.user_pmp_enabled.set(false);
    }
}
