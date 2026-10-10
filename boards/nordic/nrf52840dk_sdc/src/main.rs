// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

#![no_std]
#![no_main]

use kernel::component::Component;
use kernel::deferred_call::DeferredCallClient;
use kernel::platform::chip::InterruptService;
use kernel::platform::{KernelResources, SyscallDriverLookup};
use kernel::{capabilities, create_capability, debug, static_init};
use nrf52840::interrupt_service::Nrf52840DefaultPeripherals;
use nrf52840dk_lib::ChipHw as BaseChipHw;

// Linked for the MPSL and SoftDevice Controller archives its build script
// pulls in; none of its Rust items are used.
extern crate tock_firmware_nrf_sdc;

#[allow(
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    dead_code
)]
mod bindings;
mod hci_transport;
mod sdc;

use hci_transport::SdcHciTransport;

type HciDriver = components::bluetooth_hci::BluetoothHciComponentType<SdcHciTransport<'static>>;

/// Extends the stock nRF52840DK interrupt service with the software interrupt
/// MPSL uses to request low-priority processing.
struct SdcInterruptService {
    base: &'static Nrf52840DefaultPeripherals<'static>,
    hci: &'static SdcHciTransport<'static>,
}

impl InterruptService for SdcInterruptService {
    fn service_interrupt(&self, interrupt: u32) -> bool {
        if interrupt == sdc::IRQ_SWI5_LOW_PRIO {
            sdc::low_priority_process();
            self.hci.rx_ready();
            return true;
        }
        self.base.service_interrupt(interrupt)
    }
}

type ChipHw = nrf52840::chip::NRF52<'static, SdcInterruptService>;

struct Platform {
    base: nrf52840dk_lib::Platform,
    hci: &'static HciDriver,
}

impl SyscallDriverLookup for Platform {
    fn with_driver<F, R>(&self, driver_num: usize, f: F) -> R
    where
        F: FnOnce(Option<&dyn kernel::syscall::SyscallDriver>) -> R,
    {
        match driver_num {
            capsules_extra::bluetooth_hci::DRIVER_NUM => f(Some(self.hci)),
            _ => self.base.with_driver(driver_num, f),
        }
    }
}

impl KernelResources<ChipHw> for Platform {
    type SyscallDriverLookup = Self;
    type SyscallFilter = ();
    type ProcessFault = ();
    type Scheduler = components::sched::round_robin::RoundRobinComponentType;
    type SchedulerTimer = cortexm4::systick::SysTick;
    type WatchDog = ();
    type ContextSwitchCallback = ();

    fn syscall_driver_lookup(&self) -> &Self::SyscallDriverLookup {
        self
    }
    fn syscall_filter(&self) -> &Self::SyscallFilter {
        &()
    }
    fn process_fault(&self) -> &Self::ProcessFault {
        &()
    }
    fn scheduler(&self) -> &Self::Scheduler {
        <nrf52840dk_lib::Platform as KernelResources<BaseChipHw>>::scheduler(&self.base)
    }
    fn scheduler_timer(&self) -> &Self::SchedulerTimer {
        <nrf52840dk_lib::Platform as KernelResources<BaseChipHw>>::scheduler_timer(&self.base)
    }
    fn watchdog(&self) -> &Self::WatchDog {
        &()
    }
    fn context_switch_callback(&self) -> &Self::ContextSwitchCallback {
        &()
    }
}

#[no_mangle]
pub unsafe fn main() {
    let (board_kernel, base_platform, _base_chip, peripherals, _mux_alarm) =
        nrf52840dk_lib::start();

    let transport = static_init!(SdcHciTransport<'static>, SdcHciTransport::new());
    transport.register();
    let hci = components::bluetooth_hci::BluetoothHciComponent::new(
        board_kernel,
        capsules_extra::bluetooth_hci::DRIVER_NUM,
        transport,
        create_capability!(capabilities::MemoryAllocationCapability),
    )
    .finalize(components::bluetooth_hci_component_static!(
        SdcHciTransport<'static>
    ));

    let interrupt_service = static_init!(
        SdcInterruptService,
        SdcInterruptService {
            base: peripherals,
            hci: transport,
        }
    );
    let chip = static_init!(ChipHw, nrf52840::chip::NRF52::new(interrupt_service));

    let platform = Platform {
        base: base_platform,
        hci,
    };

    debug!("Tock + Nordic SoftDevice Controller (HCI to userspace)");
    // The controller stays disabled until a process uses the HCI driver.
    if !sdc::init() {
        debug!("sdc: initialization failed");
    }

    extern "C" {
        static _sapps: u8;
        static _eapps: u8;
        static mut _sappmem: u8;
        static _eappmem: u8;
    }

    kernel::process::load_processes(
        board_kernel,
        chip,
        core::slice::from_raw_parts(
            core::ptr::addr_of!(_sapps),
            core::ptr::addr_of!(_eapps) as usize - core::ptr::addr_of!(_sapps) as usize,
        ),
        core::slice::from_raw_parts_mut(
            core::ptr::addr_of_mut!(_sappmem),
            core::ptr::addr_of!(_eappmem) as usize - core::ptr::addr_of!(_sappmem) as usize,
        ),
        &capsules_system::process_policies::PanicFaultPolicy {},
        &create_capability!(capabilities::ProcessManagementCapability),
    )
    .unwrap_or_else(|err| {
        debug!("Error loading processes: {:?}", err);
    });

    board_kernel.kernel_loop(
        &platform,
        chip,
        Some(&platform.base.ipc),
        &create_capability!(capabilities::MainLoopCapability),
    );
}
