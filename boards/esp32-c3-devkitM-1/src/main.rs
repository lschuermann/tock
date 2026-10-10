// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2022.

//! Board file for ESP32-C3 RISC-V development platform, with Espressif's BLE
//! controller running in an Omniglot sandbox and exposed to userspace as an
//! H4 HCI transport (driver 0x30001).
//!
//! The controller (the mask ROM's link layer plus Espressif's `libbtdm_app`,
//! `libbtbb`, `libphy` and `libcoexist`) and its C glue are linked into a
//! separate sandbox, which runs in user mode under the PMP. See
//! `chips/esp32-c3-ble-omniglot` and `OMNIGLOT_BLE_PLAN.md`.

#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(test_runner)]
#![reexport_test_harness_main = "test_main"]

use capsules_core::virtualizers::virtual_alarm::{MuxAlarm, VirtualMuxAlarm};
use esp32_c3::chip::Esp32C3DefaultPeripherals;
use esp32_c3_ble_omniglot::BleOmniglot;
use kernel::capabilities;
use kernel::component::Component;
use kernel::debug::PanicResources;
use kernel::deferred_call::DeferredCallClient;
use kernel::platform::chip::InterruptService;
use kernel::platform::{KernelResources, SyscallDriverLookup};
use kernel::utilities::cells::OptionalCell;
use kernel::utilities::registers::interfaces::ReadWriteable;
use kernel::utilities::single_thread_value::SingleThreadValue;
use kernel::{create_capability, debug, hil, static_buf, static_init};
use rv32i::csr;

pub mod io;

type HciDriver = components::bluetooth_hci::BluetoothHciComponentType<BleOmniglot>;

#[cfg(test)]
mod tests;

const NUM_PROCS: usize = 4;

/// Services the BT interrupts in Tock's deferred interrupt path, ahead of the
/// stock peripherals.
struct BtInterruptService {
    base: &'static Esp32C3DefaultPeripherals<'static>,
    bt: OptionalCell<&'static BleOmniglot>,
}

impl InterruptService for BtInterruptService {
    fn service_interrupt(&self, interrupt: u32) -> bool {
        self.bt.map_or(false, |bt| bt.service_interrupt(interrupt))
            || self.base.service_interrupt(interrupt)
    }
}

type ChipHw = esp32_c3::chip::Esp32C3<'static, BtInterruptService>;
type AlarmHw = esp32_c3::timg::TimG<'static>;
type SchedulerTimerHw =
    components::virtual_scheduler_timer::VirtualSchedulerTimerNoMuxComponentType<AlarmHw>;
type ProcessPrinterInUse = capsules_system::process_printer::ProcessPrinterText;

/// Resources for when a board panics used by io.rs.
static PANIC_RESOURCES: SingleThreadValue<PanicResources<ChipHw, ProcessPrinterInUse>> =
    SingleThreadValue::new();

// How should the kernel respond when a process faults.
const FAULT_RESPONSE: capsules_system::process_policies::PanicFaultPolicy =
    capsules_system::process_policies::PanicFaultPolicy {};

// Test access to the peripherals
#[cfg(test)]
static mut PERIPHERALS: Option<&'static Esp32C3DefaultPeripherals> = None;
// Test access to scheduler
#[cfg(test)]
static mut SCHEDULER: Option<
    &capsules_system::scheduler::priority::PrioritySched<ProcessManagementCapabilityObj>,
> = None;
// Test access to board
#[cfg(test)]
static mut BOARD: Option<&'static kernel::Kernel> = None;
// Test access to platform
#[cfg(test)]
static mut PLATFORM: Option<&'static Esp32C3Board> = None;
// Test access to main loop capability
#[cfg(test)]
static mut MAIN_CAP: Option<&dyn kernel::capabilities::MainLoopCapability> = None;
// Test access to alarm
static mut ALARM: Option<&'static MuxAlarm<'static, esp32_c3::timg::TimG<'static>>> = None;

// Omniglot callbacks run in the trap handler, on the kernel stack.
kernel::stack_size! {0x1000}

type RngDriver = components::rng::RngComponentType<esp32_c3::rng::Rng<'static>>;
type GpioHw = esp32::gpio::GpioPin<'static>;
type LedHw = components::sk68xx::Sk68xxLedComponentType<GpioHw, 3>;
type LedDriver = components::led::LedsComponentType<LedHw, 3>;
type ButtonDriver = components::button::ButtonComponentType<GpioHw>;

struct ProcessManagementCapabilityObj {}
unsafe impl capabilities::ProcessManagementCapability for ProcessManagementCapabilityObj {}

type SchedulerInUse =
    components::sched::priority::PriorityComponentType<ProcessManagementCapabilityObj>;

/// A structure representing this platform that holds references to all
/// capsules for this platform. We've included an alarm and console.
struct Esp32C3Board {
    gpio: &'static capsules_core::gpio::GPIO<'static, esp32::gpio::GpioPin<'static>>,
    console: &'static capsules_core::console::Console<'static>,
    alarm: &'static capsules_core::alarm::AlarmDriver<
        'static,
        VirtualMuxAlarm<'static, esp32_c3::timg::TimG<'static>>,
    >,
    scheduler: &'static SchedulerInUse,
    scheduler_timer: &'static SchedulerTimerHw,
    rng: &'static RngDriver,
    led: &'static LedDriver,
    button: &'static ButtonDriver,
    hci: Option<&'static HciDriver>,
}

/// Mapping of integer syscalls to objects that implement syscalls.
impl SyscallDriverLookup for Esp32C3Board {
    fn with_driver<F, R>(&self, driver_num: usize, f: F) -> R
    where
        F: FnOnce(Option<&dyn kernel::syscall::SyscallDriver>) -> R,
    {
        match driver_num {
            capsules_core::gpio::DRIVER_NUM => f(Some(self.gpio)),
            capsules_core::console::DRIVER_NUM => f(Some(self.console)),
            capsules_core::alarm::DRIVER_NUM => f(Some(self.alarm)),
            capsules_core::rng::DRIVER_NUM => f(Some(self.rng)),
            capsules_core::led::DRIVER_NUM => f(Some(self.led)),
            capsules_core::button::DRIVER_NUM => f(Some(self.button)),
            capsules_extra::bluetooth_hci::DRIVER_NUM => {
                f(self.hci.map(|h| h as &dyn kernel::syscall::SyscallDriver))
            }
            _ => f(None),
        }
    }
}

impl KernelResources<ChipHw> for Esp32C3Board {
    type SyscallDriverLookup = Self;
    type SyscallFilter = ();
    type ProcessFault = ();
    type ContextSwitchCallback = ();
    type Scheduler = SchedulerInUse;
    type SchedulerTimer = SchedulerTimerHw;
    type WatchDog = ();

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
        self.scheduler
    }
    fn scheduler_timer(&self) -> &Self::SchedulerTimer {
        self.scheduler_timer
    }
    fn watchdog(&self) -> &Self::WatchDog {
        &()
    }
    fn context_switch_callback(&self) -> &Self::ContextSwitchCallback {
        &()
    }
}

unsafe fn setup() -> (
    &'static kernel::Kernel,
    &'static Esp32C3Board,
    &'static ChipHw,
    &'static Esp32C3DefaultPeripherals<'static>,
) {
    use esp32_c3::sysreg::{CpuFrequency, PllFrequency};

    // only machine mode
    esp32_c3::chip::configure_trap_handler();

    // Initialize deferred calls very early.
    kernel::deferred_call::initialize_deferred_call_state_unsafe::<
        <ChipHw as kernel::platform::chip::Chip>::ThreadIdProvider,
    >();

    // Bind global variables to this thread.
    let _ = PANIC_RESOURCES
        .bind_to_thread_unsafe::<<ChipHw as kernel::platform::chip::Chip>::ThreadIdProvider>(
            PanicResources::new(),
        );

    //
    // PERIPHERALS
    //

    let peripherals = static_init!(Esp32C3DefaultPeripherals, Esp32C3DefaultPeripherals::new());

    peripherals.timg0.disable_wdt();
    peripherals.rtc_cntl.disable_wdt();
    peripherals.rtc_cntl.disable_super_wdt();
    peripherals.rtc_cntl.enable_fosc();
    peripherals.sysreg.disable_timg0();
    peripherals.sysreg.enable_timg0();

    peripherals
        .sysreg
        .use_pll_clock_source(PllFrequency::MHz320, CpuFrequency::MHz160);

    // The mask ROM leaves flash in single-bit mode at 20 MHz. Switch flash
    // reads to a faster mode (DIO or QIO at 80MHz).
    esp32_c3::flash::configure();

    // initialise capabilities
    let process_mgmt_cap = create_capability!(capabilities::ProcessManagementCapability);
    let memory_allocation_cap = create_capability!(capabilities::MemoryAllocationCapability);

    //
    // BOARD SETUP AND PROCESSES
    //

    // Create an array to hold process references.
    let processes = components::process_array::ProcessArrayComponent::new()
        .finalize(components::process_array_component_static!(NUM_PROCS));
    PANIC_RESOURCES.get().map(|resources| {
        resources.processes.put(processes.as_slice());
    });

    // Setup space to store the core kernel data structure.
    let board_kernel = static_init!(kernel::Kernel, kernel::Kernel::new(processes.as_slice()));

    //
    // UART
    //

    // Create a shared UART channel for the console and for kernel debug.
    let uart_mux = components::console::UartMuxComponent::new(&peripherals.uart0, 115200)
        .finalize(components::uart_mux_component_static!());

    // Setup the console.
    let console = components::console::ConsoleComponent::new(
        board_kernel,
        capsules_core::console::DRIVER_NUM,
        uart_mux,
        create_capability!(capabilities::MemoryAllocationCapability),
    )
    .finalize(components::console_component_static!());
    // Create the debugger object that handles calls to `debug!()`.
    components::debug_writer::DebugWriterComponent::new_unsafe(
        uart_mux,
        create_capability!(capabilities::SetDebugWriterCapability),
        || unsafe {
            kernel::debug::initialize_debug_writer_wrapper_unsafe::<
                <ChipHw as kernel::platform::chip::Chip>::ThreadIdProvider,
            >();
        },
    )
    .finalize(components::debug_writer_component_static!());

    // Create process printer for panic.
    let process_printer = components::process_printer::ProcessPrinterTextComponent::new()
        .finalize(components::process_printer_text_component_static!());
    PANIC_RESOURCES.get().map(|resources| {
        resources.printer.put(process_printer);
    });

    //
    // GPIO
    //

    let gpio = components::gpio::GpioComponent::new(
        board_kernel,
        capsules_core::gpio::DRIVER_NUM,
        components::gpio_component_helper!(
            esp32::gpio::GpioPin,
            0 => &peripherals.gpio[0],
            1 => &peripherals.gpio[1],
            2 => &peripherals.gpio[2],
            3 => &peripherals.gpio[3],
            4 => &peripherals.gpio[4],
            5 => &peripherals.gpio[5],
            6 => &peripherals.gpio[6],
            7 => &peripherals.gpio[7],
            8 => &peripherals.gpio[15]
        ),
        create_capability!(capabilities::MemoryAllocationCapability),
    )
    .finalize(components::gpio_component_static!(esp32::gpio::GpioPin));

    //
    // ALARM
    //

    // Create a shared virtualization mux layer on top of a single hardware
    // alarm.
    let mux_alarm = static_init!(
        MuxAlarm<'static, esp32_c3::timg::TimG>,
        MuxAlarm::new(&peripherals.timg0)
    );
    hil::time::Alarm::set_alarm_client(&peripherals.timg0, mux_alarm);

    ALARM = Some(mux_alarm);

    // Alarm
    let virtual_alarm_user = static_init!(
        VirtualMuxAlarm<'static, esp32_c3::timg::TimG>,
        VirtualMuxAlarm::new(mux_alarm)
    );
    virtual_alarm_user.setup();

    let alarm = static_init!(
        capsules_core::alarm::AlarmDriver<'static, VirtualMuxAlarm<'static, esp32_c3::timg::TimG>>,
        capsules_core::alarm::AlarmDriver::new(
            virtual_alarm_user,
            board_kernel.create_grant(capsules_core::alarm::DRIVER_NUM, &memory_allocation_cap)
        )
    );
    hil::time::Alarm::set_alarm_client(virtual_alarm_user, alarm);

    //
    // LED
    //

    let led_gpio = &peripherals.gpio[8];
    let sk68xx = components::sk68xx::Sk68xxComponent::new(led_gpio, rv32i::support::nop)
        .finalize(components::sk68xx_component_static_esp32c3_160mhz!(GpioHw,));

    let led = components::led::LedsComponent::new().finalize(components::led_component_static!(
        LedHw,
        capsules_extra::sk68xx::Sk68xxLed::new(sk68xx, 0), // red
        capsules_extra::sk68xx::Sk68xxLed::new(sk68xx, 1), // green
        capsules_extra::sk68xx::Sk68xxLed::new(sk68xx, 2), // blue
    ));

    //
    // BUTTONS
    //

    let button_boot_gpio = &peripherals.gpio[9];
    let button = components::button::ButtonComponent::new(
        board_kernel,
        capsules_core::button::DRIVER_NUM,
        components::button_component_helper!(
            GpioHw,
            (
                button_boot_gpio,
                kernel::hil::gpio::ActivationMode::ActiveLow,
                kernel::hil::gpio::FloatingState::PullUp
            )
        ),
        create_capability!(capabilities::MemoryAllocationCapability),
    )
    .finalize(components::button_component_static!(GpioHw));

    //
    // SCHEDULER
    //

    let scheduler_timer_alarm = &peripherals.timg1;
    let scheduler_timer =
        components::virtual_scheduler_timer::VirtualSchedulerTimerNoMuxComponent::new(
            scheduler_timer_alarm,
        )
        .finalize(components::virtual_scheduler_timer_no_mux_component_static!(AlarmHw));

    let scheduler = components::sched::priority::PriorityComponent::new(
        board_kernel,
        ProcessManagementCapabilityObj {},
    )
    .finalize(components::priority_component_static!(
        ProcessManagementCapabilityObj
    ));

    //
    // PROCESS CONSOLE
    //

    kernel::create_typed_capability!(process_console_cap, ProcessConsoleCap:
        kernel::capabilities::ProcessManagementCapability,
        kernel::capabilities::ProcessStartCapability
    );
    let process_console = components::process_console::ProcessConsoleComponent::new(
        board_kernel,
        uart_mux,
        mux_alarm,
        process_printer,
        None,
        process_console_cap,
    )
    .finalize(components::process_console_component_static!(
        esp32_c3::timg::TimG,
        ProcessConsoleCap
    ));
    let _ = process_console.start();

    //
    // RNG
    //

    let rng = components::rng::RngComponent::new(
        board_kernel,
        capsules_core::rng::DRIVER_NUM,
        &peripherals.rng,
        create_capability!(capabilities::MemoryAllocationCapability),
    )
    .finalize(components::rng_component_static!(esp32_c3::rng::Rng));

    //
    // CHIP AND INTERRUPTS
    //

    let interrupt_service = static_init!(
        BtInterruptService,
        BtInterruptService {
            base: peripherals,
            bt: OptionalCell::empty(),
        }
    );
    let chip = static_init!(ChipHw, esp32_c3::chip::Esp32C3::new(interrupt_service));
    PANIC_RESOURCES.get().map(|resources| {
        resources.chip.put(chip);
    });

    //
    // BLE CONTROLLER
    //

    // Only if its sandbox was written to flash (`make flash`); otherwise the
    // board runs without it.

    // These symbols are defined in the linker script.
    extern "C" {
        static _ble_load_start: u8;
        static _ble_load_end: u8;
        static _ble_flash_start: u8;
        static _ble_flash_end: u8;
        static _ble_iram_start: u8;
        static _ble_iram_end: u8;
        static _ble_ram_start: u8;
        static _ble_ram_end: u8;
    }
    let region = |start: *const u8, end: *const u8| (start as usize, end as usize);
    let ble_regions = esp32_c3_ble_omniglot::Regions {
        load: region(&raw const _ble_load_start, &raw const _ble_load_end),
        flash: region(&raw const _ble_flash_start, &raw const _ble_flash_end),
        iram: region(&raw const _ble_iram_start, &raw const _ble_iram_end),
        ram: region(&raw const _ble_ram_start, &raw const _ble_ram_end),
    };
    // ### Safety
    //
    // Called once, before processes run; layout.ld reserves the sandbox's
    // regions.
    let bt = match unsafe {
        BleOmniglot::create(
            &chip.pmp.pmp,
            chip.intc(),
            ble_regions,
            static_buf!(esp32_c3_ble_omniglot::SandboxPmp),
            static_buf!(esp32_c3_ble_omniglot::Emulation),
            static_buf!(esp32_c3_ble_omniglot::SandboxRuntime),
            static_buf!(esp32_c3_ble_omniglot::RomRamSnapshot),
        )
    } {
        Ok(bt) => Some(&*static_init!(BleOmniglot, bt)),
        Err(e) => {
            debug!("ble: no BLE controller: {}", e);
            None
        }
    };
    let hci = match bt {
        Some(bt) => {
            bt.register();
            interrupt_service.bt.set(bt);
            let hci = components::bluetooth_hci::BluetoothHciComponent::new(
                board_kernel,
                capsules_extra::bluetooth_hci::DRIVER_NUM,
                bt,
                create_capability!(capabilities::MemoryAllocationCapability),
            )
            .finalize(components::bluetooth_hci_component_static!(BleOmniglot));
            Some(hci)
        }
        None => None,
    };

    // Need to enable all interrupts for Tock Kernel
    chip.map_pic_interrupts();
    chip.enable_pic_interrupts();

    // enable interrupts globally
    csr::CSR.mstatus.modify(csr::mstatus::mstatus::mie::SET);

    esp32_c3::flash::print_active_flash_mode();

    // The controller starts when a process first uses the HCI driver.

    debug!("ESP32-C3 initialisation complete.");
    debug!("Entering main loop.");

    //
    // LOAD PROCESSES
    //

    // These symbols are defined in the linker script.
    extern "C" {
        /// Beginning of the ROM region containing app images.
        static _sapps: u8;
        /// End of the ROM region containing app images.
        static _eapps: u8;
        /// Beginning of the RAM region for app memory.
        static mut _sappmem: u8;
        /// End of the RAM region for app memory.
        static _eappmem: u8;
    }

    let esp32_c3_board = static_init!(
        Esp32C3Board,
        Esp32C3Board {
            gpio,
            console,
            alarm,
            scheduler,
            scheduler_timer,
            rng,
            led,
            button,
            hci,
        }
    );

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

    peripherals.init();

    (board_kernel, esp32_c3_board, chip, peripherals)
}

/// Main function.
///
/// This function is called from the arch crate after some very basic RISC-V
/// setup and RAM initialization.
///
/// # Safety
///
/// ## `no_mangle`
///
/// We use `main` as a symbol in the arch crate's initialization routine to jump
/// here. We guarantee no other symbol has the same name.
///
/// Note: this requires a global/uniqueness guarantee.
/// See <https://github.com/tock/tock/issues/5250>.
#[unsafe(no_mangle)]
pub unsafe fn main() {
    #[cfg(test)]
    test_main();

    #[cfg(not(test))]
    {
        let (board_kernel, esp32_c3_board, chip, _peripherals) = setup();

        let main_loop_cap = create_capability!(capabilities::MainLoopCapability);

        board_kernel.kernel_loop(
            esp32_c3_board,
            chip,
            None::<&kernel::ipc::IPC<0>>,
            &main_loop_cap,
        );
    }
}

#[cfg(test)]
use kernel::platform::watchdog::WatchDog;

#[cfg(test)]
fn test_runner(tests: &[&dyn Fn()]) {
    unsafe {
        let (board_kernel, esp32_c3_board, _chip, peripherals) = setup();

        BOARD = Some(board_kernel);
        PLATFORM = Some(&esp32_c3_board);
        PERIPHERALS = Some(peripherals);
        SCHEDULER = Some(
            components::sched::priority::PriorityComponent::new(
                board_kernel,
                ProcessManagementCapabilityObj {},
            )
            .finalize(components::priority_component_static!(
                ProcessManagementCapabilityObj
            )),
        );
        MAIN_CAP = Some(&create_capability!(capabilities::MainLoopCapability));

        PLATFORM.map(|p| {
            p.watchdog().setup();
        });

        for test in tests {
            test();
        }
    }

    loop {}
}
