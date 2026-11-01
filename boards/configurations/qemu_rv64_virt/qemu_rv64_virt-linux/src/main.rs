// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Board configuration for the QEMU RISC-V 64-bit `virt` machine, running a
//! Linux kernel as an SBI process next to regular Tock processes.

#![no_std]
#![no_main]

use capsules_core::virtualizers::virtual_alarm::VirtualMuxAlarm;
use capsules_core::virtualizers::virtual_uart::UartDevice;
use kernel::capabilities;
use kernel::component::Component;
use kernel::hil::time::Alarm;
use kernel::hil::uart::Transmit;
use kernel::platform::KernelResources;
use kernel::platform::chip::Chip;
use kernel::{create_capability, debug, static_init};
use qemu_rv64_virt_lib::{ChipHw, NUM_PROCS, QemuRv64VirtPlatform};
use rv64i::sbi::{SbiGuestInterrupts, SbiProcess, basic::BasicSbi, boundary::RiscvSbiBoundary};

const FAULT_RESPONSE: capsules_system::process_policies::PanicFaultPolicy =
    capsules_system::process_policies::PanicFaultPolicy {};

/// Memory region of the Linux guest. Must match the device tree in
/// `linux/guest.dts` and the load addresses in the `Makefile`.
const LINUX_MEMORY_START: usize = 0x8040_0000;
const LINUX_MEMORY_SIZE: usize = 0x07c0_0000;
const LINUX_DTB: usize = 0x8700_0000;

type AlarmHw = qemu_rv64_virt_chip::chip::QemuRv64VirtClint<'static>;
type LinuxSbi = BasicSbi<'static, VirtualMuxAlarm<'static, AlarmHw>, UartDevice<'static>>;
type LinuxBoundary = RiscvSbiBoundary<'static, <ChipHw as Chip>::MPU>;
type LinuxProcess = SbiProcess<'static, LinuxBoundary, LinuxSbi>;
type SchedulerInUse = components::sched::round_robin::RoundRobinComponentType;

struct Platform {
    base: QemuRv64VirtPlatform,
    // Use a preemptive scheduler: Linux never yields.
    scheduler: &'static SchedulerInUse,
}

impl KernelResources<ChipHw> for Platform {
    type SyscallDriverLookup = QemuRv64VirtPlatform;
    type SyscallFilter = <QemuRv64VirtPlatform as KernelResources<ChipHw>>::SyscallFilter;
    type ProcessFault = <QemuRv64VirtPlatform as KernelResources<ChipHw>>::ProcessFault;
    type Scheduler = SchedulerInUse;
    type SchedulerTimer = <QemuRv64VirtPlatform as KernelResources<ChipHw>>::SchedulerTimer;
    type WatchDog = <QemuRv64VirtPlatform as KernelResources<ChipHw>>::WatchDog;
    type ContextSwitchCallback =
        <QemuRv64VirtPlatform as KernelResources<ChipHw>>::ContextSwitchCallback;

    fn syscall_driver_lookup(&self) -> &Self::SyscallDriverLookup {
        &self.base
    }
    fn syscall_filter(&self) -> &Self::SyscallFilter {
        self.base.syscall_filter()
    }
    fn process_fault(&self) -> &Self::ProcessFault {
        self.base.process_fault()
    }
    fn scheduler(&self) -> &Self::Scheduler {
        self.scheduler
    }
    fn scheduler_timer(&self) -> &Self::SchedulerTimer {
        self.base.scheduler_timer()
    }
    fn watchdog(&self) -> &Self::WatchDog {
        self.base.watchdog()
    }
    fn context_switch_callback(&self) -> &Self::ContextSwitchCallback {
        self.base.context_switch_callback()
    }
}

#[unsafe(no_mangle)]
pub unsafe fn main() {
    let main_loop_capability = create_capability!(capabilities::MainLoopCapability);

    let (board_kernel, base_platform, chip) = qemu_rv64_virt_lib::start();

    let scheduler =
        components::sched::round_robin::RoundRobinComponent::new(base_platform.processes)
            .finalize(components::round_robin_component_static!(NUM_PROCS));

    let platform = Platform {
        base: base_platform,
        scheduler,
    };

    let _ = platform.base.process_console_start();

    // These symbols are defined in the linker script.
    extern "C" {
        static _sapps: u8;
        static _eapps: u8;
        static mut _sappmem: u8;
        static _eappmem: u8;
    }
    let process_mgmt_cap = create_capability!(capabilities::ProcessManagementCapability);

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
        &FAULT_RESPONSE,
        &process_mgmt_cap,
    )
    .unwrap_or_else(|err| {
        debug!("Error loading processes!");
        debug!("{:?}", err);
    });

    // --- Linux SBI guest ---

    let interrupts = static_init!(SbiGuestInterrupts, SbiGuestInterrupts::new());

    // The guest's timer.
    let sbi_alarm = static_init!(
        VirtualMuxAlarm<'static, AlarmHw>,
        VirtualMuxAlarm::new(platform.base.mux_alarm)
    );
    sbi_alarm.setup();

    // The guest's console, sharing the UART with Tock's console.
    let sbi_uart = static_init!(
        UartDevice<'static>,
        UartDevice::new(platform.base.uart_mux, false)
    );
    sbi_uart.setup();
    let sbi_tx_buffer = static_init!([u8; 128], [0; 128]);

    let sbi = static_init!(
        LinuxSbi,
        BasicSbi::new(interrupts, sbi_alarm, sbi_uart, sbi_tx_buffer)
    );
    sbi_alarm.set_alarm_client(sbi);
    sbi_uart.set_transmit_client(sbi);

    let boundary = RiscvSbiBoundary::new(
        chip.mpu(),
        LINUX_MEMORY_START as *const u8,
        LINUX_MEMORY_SIZE,
        LINUX_MEMORY_START,
        0,
        LINUX_DTB,
    );
    match boundary {
        Some(boundary) => {
            let boundary = static_init!(LinuxBoundary, boundary);
            let external_process_cap = create_capability!(capabilities::ExternalProcessCapability);
            let _ = board_kernel
                .add_external_process(
                    |processid| {
                        static_init!(
                            LinuxProcess,
                            SbiProcess::new(processid, boundary, sbi, interrupts)
                        )
                    },
                    &external_process_cap,
                )
                .inspect(|processid| debug!("Added Linux SBI guest as process {:?}", processid))
                .inspect_err(|()| debug!("Error adding Linux guest: no free process slot"));
        }
        None => debug!("Error adding Linux guest: cannot protect its memory region"),
    }

    debug!("Entering main loop.");

    board_kernel.kernel_loop(&platform, chip, &main_loop_capability);
}
