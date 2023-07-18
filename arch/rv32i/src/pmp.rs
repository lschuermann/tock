use crate::csr;
use core::cell::Cell;
use core::num::NonZeroUsize;
use core::ops::Range;
use core::{cmp, fmt};
use kernel::platform::mpu;
use kernel::utilities::cells::OptionalCell;
use kernel::utilities::registers::{register_bitfields, LocalRegisterCopy};

// Generic PMP config
register_bitfields![u8,
    pub pmpcfg_octet [
        r OFFSET(0) NUMBITS(1) [],
        w OFFSET(1) NUMBITS(1) [],
        x OFFSET(2) NUMBITS(1) [],
        a OFFSET(3) NUMBITS(2) [
            OFF = 0,
            TOR = 1,
            NA4 = 2,
            NAPOT = 3
        ],
        l OFFSET(7) NUMBITS(1) []
    ]
];

pub unsafe fn format_pmp_entries<const PHYSICAL_ENTRIES: usize>(
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    // TODO: use proper Tock registers types
    for i in 0..(PHYSICAL_ENTRIES * 2) {
        // Extract the entry's pmpcfgX register value. The pmpcfgX CSRs are
        // tightly packed and contain 4 octets beloging to individual
        // entries. Convert this into a u8-wide LocalRegisterCopy<u8,
        // pmpcfg_octet> as a generic register type, independent of the entry's
        // offset.
        let pmpcfg: LocalRegisterCopy<u8, pmpcfg_octet::Register> = LocalRegisterCopy::new(
            csr::CSR
                .pmpconfig_get(i / 4)
                .overflowing_shr(((i % 4) * 8) as u32)
                .0 as u8,
        );

        // The address interpretation is different for every mode. Return both a
        // string indicating the PMP entry's mode, as well as the effective
        // start and end address (inclusive) affected by the region. For regions
        // that are OFF, we still want to expose the pmpaddrX register value --
        // thus return the raw unshifted value as the addr, and 0 as the
        // region's end.
        let (start_label, start, end, mode) = match pmpcfg.read_as_enum(pmpcfg_octet::a) {
            Some(pmpcfg_octet::a::Value::OFF) => {
                let addr = csr::CSR.pmpaddr_get(i);
                ("pmpaddr", addr, 0, "OFF")
            }

            Some(pmpcfg_octet::a::Value::TOR) => {
                let start = if i > 0 {
                    csr::CSR.pmpaddr_get(i - 1)
                } else {
                    0
                };

                (
                    "  start",
                    start.overflowing_shl(2).0,
                    csr::CSR.pmpaddr_get(i).overflowing_shl(2).0.wrapping_sub(1),
                    "TOR",
                )
            }

            Some(pmpcfg_octet::a::Value::NA4) => {
                let addr = csr::CSR.pmpaddr_get(i).overflowing_shl(2).0;
                ("  start", addr, addr | 0b11, "NA4")
            }

            Some(pmpcfg_octet::a::Value::NAPOT) => {
                let pmpaddr = csr::CSR.pmpaddr_get(i);
                let encoded_size = pmpaddr.trailing_ones();
                if (encoded_size as usize) < (core::mem::size_of_val(&pmpaddr) * 8 - 1) {
                    let start = pmpaddr - ((1 << encoded_size) - 1);
                    let end = start + (1 << (encoded_size + 1)) - 1;
                    (
                        "  start",
                        start.overflowing_shl(2).0,
                        end.overflowing_shl(2).0 | 0b11,
                        "NAPOT",
                    )
                } else {
                    ("  start", usize::MIN, usize::MAX, "NAPOT")
                }
            }

            None => {
                // We match on a 2-bit value with 4 variants, so this is
                // unreachable. However, don't insert a panic in case this
                // doesn't get optimized away:
                ("", 0, 0, "")
            }
        };

        // Ternary operator shortcut function, to avoid bulky formatting...
        fn t<T>(cond: bool, a: T, b: T) -> T {
            if cond {
                a
            } else {
                b
            }
        }

        write!(
            f,
            "  [{:02}]: {}={:#010X}, end={:#010X}, cfg={:#04X} ({}) ({}{}{}{})\r\n",
            i,
            start_label,
            start,
            end,
            pmpcfg.get(),
            mode,
            t(pmpcfg.is_set(pmpcfg_octet::l), "l", "-"),
            t(pmpcfg.is_set(pmpcfg_octet::r), "r", "-"),
            t(pmpcfg.is_set(pmpcfg_octet::w), "w", "-"),
            t(pmpcfg.is_set(pmpcfg_octet::x), "x", "-"),
        )?;
    }

    Ok(())
}

// TODO: comments
#[derive(Copy, Clone, Debug)]
pub struct TORUserPMPCFG(LocalRegisterCopy<u8, pmpcfg_octet::Register>);

impl TORUserPMPCFG {
    pub const OFF: TORUserPMPCFG = TORUserPMPCFG(LocalRegisterCopy::new(0));

    pub fn get(&self) -> u8 {
        self.0.get()
    }

    pub fn get_reg(&self) -> LocalRegisterCopy<u8, pmpcfg_octet::Register> {
        self.0
    }
}

impl PartialEq<TORUserPMPCFG> for TORUserPMPCFG {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}

impl Eq for TORUserPMPCFG {}

impl From<mpu::Permissions> for TORUserPMPCFG {
    fn from(p: mpu::Permissions) -> Self {
        let fv = match p {
            mpu::Permissions::ReadWriteExecute => {
                pmpcfg_octet::r::SET + pmpcfg_octet::w::SET + pmpcfg_octet::x::SET
            }
            mpu::Permissions::ReadWriteOnly => {
                pmpcfg_octet::r::SET + pmpcfg_octet::w::SET + pmpcfg_octet::x::CLEAR
            }
            mpu::Permissions::ReadExecuteOnly => {
                pmpcfg_octet::r::SET + pmpcfg_octet::w::CLEAR + pmpcfg_octet::x::SET
            }
            mpu::Permissions::ReadOnly => {
                pmpcfg_octet::r::SET + pmpcfg_octet::w::CLEAR + pmpcfg_octet::x::CLEAR
            }
            mpu::Permissions::ExecuteOnly => {
                pmpcfg_octet::r::CLEAR + pmpcfg_octet::w::CLEAR + pmpcfg_octet::x::SET
            }
        };

        TORUserPMPCFG(LocalRegisterCopy::new(
            (fv + pmpcfg_octet::l::CLEAR + pmpcfg_octet::a::TOR).value,
        ))
    }
}

/// A RISC-V PMP implementation exposing a number of TOR memory protection
/// regions to the [`PMPUserMPU`].
///
/// The RISC-V PMP is complex and can be used for enforcing memory protection in
/// various modes (Machine, Supervisor and User mode). Depending on the exact
/// extension set present (e.g., ePMP) and the machine's security configuration
/// bits, it may expose a vastly different set of constraints and application
/// semantics.
///
/// Because we can't possibly capture all of this in a single readable,
/// maintainable and efficient implementation, we implement a two-layer system:
///
/// - a [`TORUserPMP`] is a simple abstraction over some underlying PMP hardware
///   implementation, which exposes an interface to configure regions that are
///   active (enforced) in user-mode and can be configured for arbitrary
///   addresses on a 4-byte granularity.
///
/// - the [`PMPUserMPU`] takes this abstraction and implements the Tock kernel's
///   [`mpu::MPU`] trait. It worries about re-configuring memory protection when
///   switching processes, allocating memory regions of an appropriate size,
///   etc.
///
/// Implementors of a chip are free to define their own [`TORUserPMP`]
/// implementations, adhering to their specific PMP layout & constraints,
/// provided they implement this trait.
///
/// The `MAX_REGIONS` const generic is used to indicate the maximum number of
/// TOR PMP regions available to the [`PMPUserMPU`]. The PMP implementation may
/// provide less regions than indicated through `MAX_REGIONS`, for instance when
/// entries are enforced (locked) in machine mode. The number of available
/// regions may change at runtime. The current number of regions available to
/// the [`PMPUserMPU`] is indicated by the [`TORUserPMP::available_regions`]
/// method. However, when it is known that a number of regions are not available
/// for userspace protection, `MAX_REGIONS` can be used to reduce the memory
/// footprint allocated by stored PMP configurations, as well as the
/// re-configuration overhead.
pub trait TORUserPMP<const MAX_REGIONS: usize> {
    /// A placeholder to define const-assertions which are evaluated in
    /// [`PMPUserMPU::new`]. This can be used to, for instance, assert that the
    /// number of userspace regions doees not exceed the number of hardware
    /// regions.
    const CONST_ASSERT_CHECK: ();

    /// The number of TOR regions currently available for userspace memory
    /// protection. Within `[0; MAX_REGIONS]`.
    ///
    /// The PMP implementation may provide less regions than indicated through
    /// `MAX_REGIONS`, for instance when entries are enforced (locked) in
    /// machine mode. The number of available regions may change at runtime. The
    /// implementation is free to map these regions to arbitrary PMP entries
    /// (and change this mapping at runtime), provided that they are enforced
    /// when the hart is in user-mode, and other memory regions are generally
    /// inaccessible when in user-mode.
    ///
    /// When allocating regions for kernel-mode protection, and thus reducing
    /// the number of regions available to userspace, re-configuring the PMP may
    /// fail. This is allowed behavior. However, the PMP must not remove any
    /// regions from the user-mode current configuration while it is active
    /// ([`TORUserPMP::enable_user_pmp`] has been called, and it has not been
    /// disabled through [`TORUserPMP::disable_user_pmp`]).
    fn available_regions(&self) -> usize;

    /// Configure the user-mode memory protection.
    ///
    /// This method configures the user-mode memory protection, to be enforced
    /// on a call to [`TORUserPMP::enable_user_pmp`].
    ///
    /// PMP implementations where configured regions are only enforced in
    /// user-mode may re-configure the PMP on this function invocation and
    /// implement [`TORUserPMP::enable_user_pmp`] as a no-op. If configured
    /// regions are enforced in machine-mode (for instance when using an ePMP
    /// with the machine-mode whitelist policy), the new configuration rules
    /// must not apply until [`TORUserPMP::enable_user_pmp`].
    ///
    /// The tuples as passed in the `regions` parameter are defined as follows:
    ///
    /// - first value ([`TORUserPMPCFG`]): the memory protection mode as
    ///   enforced on the region. A `TORUserPMPCFG` can be created from the
    ///   [`mpu::Permissions`] type. It is in a format compatible to the pmpcfgX
    ///   register, guaranteed to not have the lock (`L`) bit set, and
    ///   configured either as a TOR region (`A = 0b01`), or disabled (all bits
    ///   set to `0`).
    ///
    /// - second value (`*const u8`): the region's start addres. As a PMP TOR
    ///   region has a 4-byte address granularity, this address is rounded down
    ///   to the next 4-byte boundary.
    ///
    /// - third value (`*const u8`): the region's end addres. As a PMP TOR
    ///   region has a 4-byte address granularity, this address is rounded down
    ///   to the next 4-byte boundary.
    ///
    /// To disable a region, set its configuration to [`TORUserPMPCFG::OFF`]. In
    /// this case, the start and end addresses are ignored and can be set to
    /// arbitrary values.
    fn configure_pmp(
        &self,
        regions: &[(TORUserPMPCFG, *const u8, *const u8); MAX_REGIONS],
    ) -> Result<(), ()>;

    /// Enable the user-mode memory protection.
    ///
    /// Enables the memory protection for user-mode, as configured through
    /// [`TORUserPMP::configure_pmp`]. Enabling the PMP for user-mode may make
    /// the user-mode accessible regions inaccessible to the kernel. For PMP
    /// implementations where configured regions are only enforced in user-mode,
    /// this method may be implemented as a no-op.
    ///
    /// If enabling the current configuration is not possible (e.g., because
    /// regions have been allocated to the kernel), this function must return
    /// `Err(())`. Otherwise, this function returns `Ok(())`.
    fn enable_user_pmp(&self) -> Result<(), ()>;

    /// Disable the user-mode memory protection.
    ///
    /// Disables the memory protection for user-mode. If enabling the user-mode
    /// memory protetion made user-mode accessible regions inaccessible to
    /// machine-mode, this method should make these regions accessible again.
    ///
    /// For PMP implementations where configured regions are only enforced in
    /// user-mode, this method may be implemented as a no-op. This method is not
    /// responsible for making regions inaccessible to user-mode. If previously
    /// configured regions must be made inaccessible,
    /// [`TORUserPMP::configure_pmp`] must be used to re-configure the PMP
    /// accordingly.
    fn disable_user_pmp(&self);
}

pub mod simple {
    use super::{pmpcfg_octet, TORUserPMP, TORUserPMPCFG};
    use crate::csr;
    use core::fmt;
    use kernel::utilities::registers::{FieldValue, LocalRegisterCopy};

    pub struct SimplePMP<const AVAILABLE_ENTRIES_OVER_TWO: usize>;

    impl<const AVAILABLE_ENTRIES_OVER_TWO: usize> SimplePMP<AVAILABLE_ENTRIES_OVER_TWO> {
        pub unsafe fn new() -> Result<Self, ()> {
            // The SimplePMP does not support locked regions, kernel memory
            // protection, or any ePMP features (using the mseccfg CSR). Ensure
            // that we don't find any locked regions. If we don't have locked
            // regions and can still successfully execute code, this means that
            // we're not in the ePMP machine-mode lockdown mode, and can treat
            // our hardware as a regular PMP.
            //
            // Furthermore, we test whether we can use each entry (i.e. whether
            // it actually exists in HW) by flipping the RWX bits. If we can't
            // flip them, then `AVAILABLE_ENTRIES_OVER_TWO` is incorrect.
            // However, this is not sufficient to check for locked regions,
            // because of the ePMP's rule-lock-bypass bit. If a rule is locked,
            // it might be the reason why we can execute code or read-write data
            // in machine mode right now. Thus, never try to touch a locked
            // region, as we might well revoke access to a kernel region!
            for i in 0..(AVAILABLE_ENTRIES_OVER_TWO * 2) {
                // Read the entry's CSR:
                let pmpcfg_csr = csr::CSR.pmpconfig_get(i / 4);

                // Extract the entry's pmpcfg octet:
                let pmpcfg: LocalRegisterCopy<u8, pmpcfg_octet::Register> = LocalRegisterCopy::new(
                    pmpcfg_csr.overflowing_shr(((i % 4) * 8) as u32).0 as u8,
                );

                // As outlined above, we never touch a locked region. Thus, bail
                // out if it's locked:
                if pmpcfg.is_set(pmpcfg_octet::l) {
                    return Err(());
                }

                // Now that it's not locked, we can be sure that regardless of
                // any ePMP bits, this region is either ignored or entirely
                // denied for machine-mode access. Hence, we can change it in
                // arbitrary ways without breaking our own memory access. Try to
                // flip the R/W/X bits:
                csr::CSR.pmpconfig_set(i / 4, pmpcfg_csr ^ (7 << ((i % 4) * 8)));

                // Check if the CSR changed:
                if pmpcfg_csr == csr::CSR.pmpconfig_get(i / 4) {
                    // Didn't change! This means that this region is not backed
                    // by HW. Return an error as `AVAILABLE_ENTRIES_OVER_TWO` is
                    // incorrect:
                    return Err(());
                }

                // Finally, turn the region off:
                csr::CSR.pmpconfig_set(i / 4, pmpcfg_csr & !(0x18 << ((i % 4) * 8)));
            }

            // Hardware PMP is verified to be in a compatible mode / state, and
            // has at least `AVAILABLE_ENTRIES_OVER_TWO` regions.
            Ok(SimplePMP)
        }
    }

    impl<const AVAILABLE_ENTRIES_OVER_TWO: usize, const MPU_REGIONS: usize> TORUserPMP<MPU_REGIONS>
        for SimplePMP<AVAILABLE_ENTRIES_OVER_TWO>
    {
        // Don't require any const-assertions in the SimplePMP.
        const CONST_ASSERT_CHECK: () = assert!(MPU_REGIONS <= AVAILABLE_ENTRIES_OVER_TWO);

        fn available_regions(&self) -> usize {
            // Always assume to have `MPU_REGIONS` usable TOR regions. We don't
            // support locked regions, or kernel protection.
            MPU_REGIONS
        }

        // This implementation is specific for 32-bit systems. We use
        // `u32::from_be_bytes` and then cast to usize, as it manages to compile
        // on 64-bit systems as well. However, this implementation will not work
        // on RV64I systems, due to the changed pmpcfgX CSR layout.
        fn configure_pmp(
            &self,
            regions: &[(TORUserPMPCFG, *const u8, *const u8); MPU_REGIONS],
        ) -> Result<(), ()> {
            // Could use `iter_array_chunks` once that's stable.
            let mut regions_iter = regions.iter();
            let mut i = 0;

            while let Some(even_region) = regions_iter.next() {
                let odd_region_opt = regions_iter.next();

                if let Some(odd_region) = odd_region_opt {
                    // We can configure two regions at once which, given that we
                    // start at index 0 (an even offset), translates to a single
                    // CSR write for the pmpcfgX register:
                    csr::CSR.pmpconfig_set(
                        i / 2,
                        u32::from_be_bytes([
                            odd_region.0.get(),
                            TORUserPMPCFG::OFF.get(),
                            even_region.0.get(),
                            TORUserPMPCFG::OFF.get(),
                        ]) as usize,
                    );

                    // Now, set the addresses of the respective regions, if they
                    // are enabled, respectively:
                    if even_region.0 != TORUserPMPCFG::OFF {
                        csr::CSR.pmpaddr_set(i * 2 + 0, even_region.1 as usize);
                        csr::CSR.pmpaddr_set(i * 2 + 1, even_region.2 as usize);
                    }

                    if odd_region.0 != TORUserPMPCFG::OFF {
                        csr::CSR.pmpaddr_set(i * 2 + 2, odd_region.1 as usize);
                        csr::CSR.pmpaddr_set(i * 2 + 3, odd_region.2 as usize);
                    }

                    i += 2;
                } else {
                    // TODO: check overhead of code
                    // Modify the first two pmpcfgX octets for this region:
                    csr::CSR.pmpconfig_modify(
                        i / 2,
                        FieldValue::<usize, csr::pmpconfig::pmpcfg::Register>::new(
                            0x0000FFFF,
                            0,
                            u32::from_be_bytes([
                                0,
                                0,
                                even_region.0.get(),
                                TORUserPMPCFG::OFF.get(),
                            ]) as usize,
                        ),
                    );

                    // Set the addresses if the region is enabled:
                    if even_region.0 != TORUserPMPCFG::OFF {
                        csr::CSR.pmpaddr_set(i * 2 + 0, even_region.1 as usize);
                        csr::CSR.pmpaddr_set(i * 2 + 1, even_region.2 as usize);
                    }

                    i += 1;
                }
            }

            Ok(())
        }

        fn enable_user_pmp(&self) -> Result<(), ()> {
            // No-op. The SimplePMP does not have any kernel-enforced regions.
            Ok(())
        }

        fn disable_user_pmp(&self) {
            // No-op. The SimplePMP does not have any kernel-enforced regions.
        }
    }

    impl<const AVAILABLE_ENTRIES_OVER_TWO: usize> fmt::Display
        for SimplePMP<AVAILABLE_ENTRIES_OVER_TWO>
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, " PMP hardware configuration -- entries: \r\n")?;
            unsafe { super::format_pmp_entries::<AVAILABLE_ENTRIES_OVER_TWO>(f) }
        }
    }
}

/// Struct storing userspace memory protection regions for the [`PMPUserMPU`].
pub struct PMPUserMPUConfig<const MAX_REGIONS: usize> {
    /// PMP config identifier, as generated by the issuing PMP implementation.
    id: NonZeroUsize,
    /// Indicates if the configuration has changed since the last time it was
    /// written to hardware.
    is_dirty: Cell<bool>,
    /// Array of MPU regions. Each region requires two physical PMP entries.
    regions: [(TORUserPMPCFG, *const u8, *const u8); MAX_REGIONS],
    /// Which region index (into the `regions` array above) is used
    /// for app memory (if it has been configured).
    app_memory_region: OptionalCell<usize>,
}

impl<const MAX_REGIONS: usize> fmt::Display for PMPUserMPUConfig<MAX_REGIONS> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " PMP regions: TODO\r\n")?;
        Ok(())
    }
}

/// Adaptor from a generic PMP implementation exposing TOR-type regions to the
/// Tock [`mpu::MPU`] trait. See [`TORUserPMP`].
///
///
pub struct PMPUserMPU<const MAX_REGIONS: usize, P: TORUserPMP<MAX_REGIONS> + 'static> {
    /// Monotonically increasing counter for allocated configurations, used to
    /// assign unique IDs to `PMPUserMPUConfig` instances.
    config_count: Cell<NonZeroUsize>,
    /// The configuration that the PMP was last configured for. Used (along with
    /// the `is_dirty` flag) to determine if PMP can skip writing the
    /// configuration to hardware.
    last_configured_for: OptionalCell<NonZeroUsize>,
    /// Underlying hardware PMP implementation, exposing a number (up to
    /// `P::MAX_REGIONS`) of memory protection regions with a 4-byte enforcement
    /// granularity.
    pub pmp: P,
}

impl<const MAX_REGIONS: usize, P: TORUserPMP<MAX_REGIONS> + 'static> PMPUserMPU<MAX_REGIONS, P> {
    pub fn new(pmp: P) -> Self {
        let _: () = P::CONST_ASSERT_CHECK;

        PMPUserMPU {
            config_count: Cell::new(NonZeroUsize::MIN),
            last_configured_for: OptionalCell::empty(),
            pmp,
        }
    }
}

/// Check if a [`PMPUserMPUConfig`] region overlaps with a region specified by
/// `other_start` and `other_size`.
///
/// Matching the RISC-V spec this checks pmpaddr[i-i] <= y < pmpaddr[i] for TOR
/// ranges.
fn region_overlaps(
    region: &(TORUserPMPCFG, *const u8, *const u8),
    other_start: *const u8,
    other_size: usize,
) -> bool {
    // PMP TOR regions are not inclusive on the high end, that is
    //     pmpaddr[i-i] <= y < pmpaddr[i].
    //
    // This happens to coincide with the definition of the Rust half-open Range
    // type, which provides a convenient `.contains()` method:
    let region_range = Range {
        start: region.1 as usize,
        end: region.2 as usize,
    };

    let other_range = Range {
        start: other_start as usize,
        end: other_start as usize + other_size,
    };

    // For a range A to overlap with a range B, either B's first or B's last
    // element must be contained in A, or A's first or A's last element must be
    // contained in B. As we deal with half-open ranges, ensure that neither
    // range is empty.
    //
    // This implementation is simple and stupid, and can be optimized. We leave
    // that as an exercise to the compiler.
    !region_range.is_empty()
        && !other_range.is_empty()
        && (region_range.contains(&other_range.start)
            || region_range.contains(&other_range.end)
            || other_range.contains(&region_range.start)
            || other_range.contains(&region_range.end))
}

impl<const MAX_REGIONS: usize, P: TORUserPMP<MAX_REGIONS> + 'static> kernel::platform::mpu::MPU
    for PMPUserMPU<MAX_REGIONS, P>
{
    type MpuConfig = PMPUserMPUConfig<MAX_REGIONS>;

    fn clear_mpu(&self) {
        self.disable_app_mpu();
        self.pmp
            .configure_pmp(&[(TORUserPMPCFG::OFF, 0 as *const u8, 0 as *const u8); MAX_REGIONS])
            .unwrap()
    }

    // TODO: actually handle errors
    fn enable_app_mpu(&self) {
        // TODO: how to handle errors?
        self.pmp.enable_user_pmp().unwrap()
    }

    fn disable_app_mpu(&self) {
        self.pmp.disable_user_pmp()
    }

    fn number_total_regions(&self) -> usize {
        self.pmp.available_regions()
    }

    // TODO: return an error
    fn new_config(&self) -> Self::MpuConfig {
        let id = self.config_count.get();
        self.config_count.set(id.checked_add(1).unwrap());

        // Start with an empty config, with some default FieldValue
        // copies into the regions array:
        PMPUserMPUConfig {
            id,
            regions: [(TORUserPMPCFG::OFF, 0 as *const u8, 0 as *const u8); MAX_REGIONS],
            is_dirty: Cell::new(true),
            app_memory_region: OptionalCell::empty(),
        }
    }

    fn allocate_region(
        &self,
        unallocated_memory_start: *const u8,
        unallocated_memory_size: usize,
        min_region_size: usize,
        permissions: mpu::Permissions,
        config: &mut Self::MpuConfig,
    ) -> Option<mpu::Region> {
        // Find a free region slot. If we don't have one, abort early:
        let region_num = config
            .regions
            .iter()
            .enumerate()
            .find(|(_i, (pmpcfg, _, _))| *pmpcfg == TORUserPMPCFG::OFF)
            .map(|(i, _)| i)?;

        // Now, meet the PMP TOR region constraints. For this, start with the
        // provided start address and size, transform them to meet the
        // constraints, and then check that we're still within the bounds of the
        // provided values:
        let mut start = unallocated_memory_start as usize;
        let mut size = min_region_size;

        // Region start always has to align to 4 bytes. Round up to a 4 byte
        // boundary if required:
        if start % 4 != 0 {
            start += 4 - (start % 4);
        }

        // Region size always has to align to 4 bytes. Round up to a 4 byte
        // boundary if required:
        if size % 4 != 0 {
            size += 4 - (size % 4);
        }

        // Regions must be at least 4 bytes in size.
        if size < 4 {
            size = 4;
        }

        // Now, check to see whether the adjusted start and size still meet the
        // allocation constraints, namely ensure that
        //
        //     start + size <= unallocated_memory_start + unallocated_memory_size
        if start + size > (unallocated_memory_start as usize) + unallocated_memory_size {
            // We're overflowing the provided memory region, can't make
            // allocation. Normally, we'd abort here.
            //
            // However, a previous implementation of this code was incorrect in
            // that performed this check before adjusting the requested region
            // size to meet PMP region layout constraints (4 byte alignment for
            // start and end address). Existing applications whose end-address
            // is aligned on a less than 4-byte bondary would thus be given
            // access to additional memory which should be inaccessible.
            // Unfortunately, we can't fix this without breaking existing
            // applications. Thus, we perform the same insecure hack here, and
            // give the apps at most an extra 3 bytes of memory, as long as the
            // requested region as no write privileges.
            //
            // TODO: Remove this logic with as part of
            // https://github.com/tock/tock/issues/3544
            let writeable = match permissions {
                mpu::Permissions::ReadWriteExecute => true,
                mpu::Permissions::ReadWriteOnly => true,
                mpu::Permissions::ReadExecuteOnly => false,
                mpu::Permissions::ReadOnly => false,
                mpu::Permissions::ExecuteOnly => false,
            };

            if writeable
                || (start + size
                    > (unallocated_memory_start as usize) + unallocated_memory_size + 3)
            {
                return None;
            }
        }

        // Finally, check that this new region does not overlap with any
        // existing configured userspace region:
        for region in config.regions.iter() {
            if region.0 != TORUserPMPCFG::OFF && region_overlaps(region, start as *const u8, size) {
                return None;
            }
        }

        // All checks passed, store region allocation and mark config as dirty:
        config.regions[region_num] = (
            permissions.into(),
            start as *const u8,
            (start + size) as *const u8,
        );
        config.is_dirty.set(true);

        Some(mpu::Region::new(start as *const u8, size))
    }

    fn remove_memory_region(
        &self,
        region: mpu::Region,
        config: &mut Self::MpuConfig,
    ) -> Result<(), ()> {
        let index = config
            .regions
            .iter()
            .enumerate()
            .find(|(_i, r)| {
                // `start as usize + size` in lieu of a safe pointer offset method
                r.0 != TORUserPMPCFG::OFF
                    && r.1 == region.start_address()
                    && r.2 == (region.start_address() as usize + region.size()) as *const u8
            })
            .map(|(i, _)| i)
            .ok_or(())?;

        config.regions[index].0 = TORUserPMPCFG::OFF;
        config.is_dirty.set(true);

        Ok(())
    }

    fn allocate_app_memory_region(
        &self,
        unallocated_memory_start: *const u8,
        unallocated_memory_size: usize,
        min_memory_size: usize,
        initial_app_memory_size: usize,
        initial_kernel_memory_size: usize,
        permissions: mpu::Permissions,
        config: &mut Self::MpuConfig,
    ) -> Option<(*const u8, usize)> {
        // An app memory region can only be allocated once per `MpuConfig`.
        // If we already have one, abort:
        if config.app_memory_region.is_some() {
            return None;
        }

        // Find a free region slot. If we don't have one, abort early:
        let region_num = config
            .regions
            .iter()
            .enumerate()
            .find(|(_i, (pmpcfg, _, _))| *pmpcfg == TORUserPMPCFG::OFF)
            .map(|(i, _)| i)?;

        // Now, meet the PMP TOR region constraints for the region specified by
        // `initial_app_memory_size` (which is the part of the region actually
        // protected by the PMP). For this, start with the provided start
        // address and size, transform them to meet the constraints, and then
        // check that we're still within the bounds of the provided values:
        let mut start = unallocated_memory_start as usize;
        let mut pmp_region_size = initial_app_memory_size;

        // Region start always has to align to 4 bytes. Round up to a 4 byte
        // boundary if required:
        if start % 4 != 0 {
            start += 4 - (start % 4);
        }

        // Region size always has to align to 4 bytes. Round up to a 4 byte
        // boundary if required:
        if pmp_region_size % 4 != 0 {
            pmp_region_size += 4 - (pmp_region_size % 4);
        }

        // Regions must be at least 4 bytes in size.
        if pmp_region_size < 4 {
            pmp_region_size = 4;
        }

        // We need to provide a memory block that fits both the initial app and
        // kernel memory sections, and is `min_memory_size` bytes
        // long. Calculate the length of this block with our new PMP-aliged
        // size:
        let memory_block_size = cmp::max(
            min_memory_size,
            pmp_region_size + initial_kernel_memory_size,
        );

        // Now, check to see whether the adjusted start and size still meet the
        // allocation constraints, namely ensure that
        //
        //     start + memory_block_size
        //         <= unallocated_memory_start + unallocated_memory_size
        //
        // , which ensures the PMP constraints didn't push us over the bounds of
        // the provided memory region, and we can fit the entire allocation as
        // requested by the kernel:
        if start + memory_block_size > (unallocated_memory_start as usize) + unallocated_memory_size
        {
            // Overflowing the provided memory region, can't make allocation:
            return None;
        }

        // Finally, check that this new region does not overlap with any
        // existing configured userspace region:
        for region in config.regions.iter() {
            if region.0 != TORUserPMPCFG::OFF
                && region_overlaps(region, start as *const u8, memory_block_size)
            {
                return None;
            }
        }

        // All checks passed, store region allocation, indicate the
        // app_memory_region, and mark config as dirty:
        config.regions[region_num] = (
            permissions.into(),
            start as *const u8,
            (start + pmp_region_size) as *const u8,
        );
        config.is_dirty.set(true);
        config.app_memory_region.replace(region_num);

        Some((start as *const u8, memory_block_size))
    }

    fn update_app_memory_region(
        &self,
        app_memory_break: *const u8,
        kernel_memory_break: *const u8,
        permissions: mpu::Permissions,
        config: &mut Self::MpuConfig,
    ) -> Result<(), ()> {
        let region_num = config.app_memory_region.get().ok_or(())?;

        let mut app_memory_break = app_memory_break as usize;
        let kernel_memory_break = kernel_memory_break as usize;

        // Ensure that the requested app_memory_break complies with PMP
        // alignment constraints, namely that the region's end address is 4 byte
        // aligned:
        if app_memory_break % 4 != 0 {
            app_memory_break += 4 - (app_memory_break % 4);
        }

        // Check if the app has run out of memory:
        if app_memory_break > kernel_memory_break {
            return Err(());
        }

        // If we're not out of memory, update the region configuration
        // accordingly:
        config.regions[region_num].0 = permissions.into();
        config.regions[region_num].2 = app_memory_break as *const u8;
        config.is_dirty.set(true);

        Ok(())
    }

    fn configure_mpu(&self, config: &Self::MpuConfig) {
        if !self.last_configured_for.contains(&config.id) || config.is_dirty.get() {
            self.pmp.configure_pmp(&config.regions).unwrap()
        }
    }
}
