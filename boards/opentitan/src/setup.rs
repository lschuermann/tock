//! Board file for LowRISC OpenTitan RISC-V development platform.
//!
//! - <https://opentitan.org/>

use capsules_core::virtualizers::virtual_aes_ccm;
use capsules_core::virtualizers::virtual_alarm::{MuxAlarm, VirtualMuxAlarm};
use capsules_core::virtualizers::virtual_hmac::VirtualMuxHmac;
// use capsules_core::virtualizers::virtual_sha::VirtualMuxSha;
use earlgrey::chip::EarlGreyDefaultPeripherals;
use kernel::capabilities;
use kernel::component::Component;
//use kernel::dynamic_deferred_call::{DynamicDeferredCall, DynamicDeferredCallClientState};
use kernel::hil;
use kernel::hil::digest::Digest;
use kernel::hil::entropy::Entropy32;
use kernel::hil::hasher::Hasher;
use kernel::hil::i2c::I2CMaster;
use kernel::hil::kv_system::KVSystem;
use kernel::hil::led::LedHigh;
use kernel::hil::rng::Rng;
use kernel::hil::symmetric_encryption::AES128;
use kernel::hil::symmetric_encryption::AES128_BLOCK_SIZE;
use kernel::platform::scheduler_timer::VirtualSchedulerTimer;
use kernel::platform::{KernelResources, SyscallDriverLookup, TbfHeaderFilterDefaultAllow};
use kernel::scheduler::priority::PrioritySched;
use kernel::syscall::SyscallDriver;
use kernel::utilities::registers::interfaces::{ReadWriteable, Readable};
use kernel::{create_capability, debug, static_init};
use rv32i::csr;

use crate::otbn::OtbnComponent;
use crate::otbn_mux_component_static;

pub(crate) const NUM_PROCS: usize = 4;

//
// Actual memory for holding the active process structures. Need an empty list
// at least.
pub(crate) static mut PROCESSES: [Option<&'static dyn kernel::process::Process>; 4] =
    [None; NUM_PROCS];

// Test access to the peripherals
#[cfg(test)]
pub(crate) static mut PERIPHERALS: Option<&'static EarlGreyDefaultPeripherals> = None;
// Test access to board
#[cfg(test)]
pub(crate) static mut BOARD: Option<&'static kernel::Kernel> = None;
// Test access to platform
#[cfg(test)]
pub(crate) static mut PLATFORM: Option<&'static EarlGrey> = None;
// Test access to main loop capability
#[cfg(test)]
pub(crate) static mut MAIN_CAP: Option<&dyn kernel::capabilities::MainLoopCapability> = None;
// Test access to alarm
pub(crate) static mut ALARM: Option<&'static MuxAlarm<'static, earlgrey::timer::RvTimer<'static>>> =
    None;
// Test access to TicKV
pub(crate) static mut TICKV: Option<
    &capsules_extra::tickv::TicKVStore<
        'static,
        capsules_core::virtualizers::virtual_flash::FlashUser<
            'static,
            lowrisc::flash_ctrl::FlashCtrl<'static>,
        >,
        capsules_extra::sip_hash::SipHasher24<'static>,
    >,
> = None;
// Test access to AES CCM
pub(crate) static mut AES: Option<
    &virtual_aes_ccm::VirtualAES128CCM<'static, earlgrey::aes::Aes<'static>>,
> = None;
// Test access to SipHash
pub(crate) static mut SIPHASH: Option<&capsules_extra::sip_hash::SipHasher24<'static>> = None;
// Test access to RSA
pub(crate) static mut RSA_HARDWARE: Option<&lowrisc::rsa::OtbnRsa<'static>> = None;

pub(crate) static mut CHIP: Option<&'static earlgrey::chip::EarlGrey<EarlGreyDefaultPeripherals>> =
    None;
pub(crate) static mut PROCESS_PRINTER: Option<&'static kernel::process::ProcessPrinterText> = None;

// How should the kernel respond when a process faults.
pub(crate) const FAULT_RESPONSE: kernel::process::PanicFaultPolicy =
    kernel::process::PanicFaultPolicy {};

/// Dummy buffer that causes the linker to reserve enough space for the stack.
#[no_mangle]
#[link_section = ".stack_buffer"]
pub static mut STACK_MEMORY: [u8; 0x2000] = [0; 0x2000];

/// A structure representing this platform that holds references to all
/// capsules for this platform. We've included an alarm and console.
pub struct EarlGrey {
    led: &'static capsules_core::led::LedDriver<
        'static,
        LedHigh<'static, earlgrey::gpio::GpioPin<'static>>,
        8,
    >,
    gpio: &'static capsules_core::gpio::GPIO<'static, earlgrey::gpio::GpioPin<'static>>,
    console: &'static capsules_core::console::Console<'static>,
    alarm: &'static capsules_core::alarm::AlarmDriver<
        'static,
        VirtualMuxAlarm<'static, earlgrey::timer::RvTimer<'static>>,
    >,
    // hmac: &'static capsules_extra::hmac::HmacDriver<
    //     'static,
    //     VirtualMuxHmac<
    //         'static,
    //         capsules_core::virtualizers::virtual_digest::VirtualMuxDigest<
    //             'static,
    //             lowrisc::hmac::Hmac<'static>,
    //             32,
    //         >,
    //         32,
    //     >,
    //     32,
    // >,
    // sha: &'static capsules_extra::sha::ShaDriver<
    //     'static,
    //     VirtualMuxSha<
    //         'static,
    //         capsules_core::virtualizers::virtual_digest::VirtualMuxDigest<
    //             'static,
    //             lowrisc::hmac::Hmac<'static>,
    //             32,
    //         >,
    //         32,
    //     >,
    //     32,
    // >,
    lldb: &'static capsules_core::low_level_debug::LowLevelDebug<
        'static,
        capsules_core::virtualizers::virtual_uart::UartDevice<'static>,
    >,
    i2c_master:
        &'static capsules_core::i2c_master::I2CMasterDriver<'static, lowrisc::i2c::I2c<'static>>,
    spi_controller: &'static capsules_core::spi_controller::Spi<
        'static,
        capsules_core::virtualizers::virtual_spi::VirtualSpiMasterDevice<
            'static,
            lowrisc::spi_host::SpiHost,
        >,
    >,
    rng: &'static capsules_core::rng::RngDriver<'static>,
    aes: &'static capsules_extra::symmetric_encryption::aes::AesDriver<
        'static,
        virtual_aes_ccm::VirtualAES128CCM<'static, earlgrey::aes::Aes<'static>>,
    >,
    kv_driver: Option<
        &'static capsules_extra::kv_driver::KVSystemDriver<
            'static,
            capsules_extra::tickv::TicKVStore<
                'static,
                capsules_core::virtualizers::virtual_flash::FlashUser<
                    'static,
                    lowrisc::flash_ctrl::FlashCtrl<'static>,
                >,
                capsules_extra::sip_hash::SipHasher24<'static>,
            >,
            [u8; 8],
        >,
	>,
        hmac: &'static capsules_extra::hmac::HmacDriver<
		'static,
	    VirtualMuxHmac<'static,
        crate::cryptolib_hmac::CryptolibHmac<'static, 'static, earlgrey::chip::EarlGrey<'static, EarlGreyDefaultPeripherals<'static>>>, 32>, 32>,
    syscall_filter: &'static TbfHeaderFilterDefaultAllow,
    scheduler: &'static PrioritySched,
    scheduler_timer:
        &'static VirtualSchedulerTimer<VirtualMuxAlarm<'static, earlgrey::timer::RvTimer<'static>>>,
}

/// Mapping of integer syscalls to objects that implement syscalls.
impl SyscallDriverLookup for EarlGrey {
    fn with_driver<F, R>(&self, driver_num: usize, f: F) -> R
    where
        F: FnOnce(Option<&dyn kernel::syscall::SyscallDriver>) -> R,
    {
        match driver_num {
            capsules_core::led::DRIVER_NUM => f(Some(self.led)),
            // capsules_extra::hmac::DRIVER_NUM => f(Some(self.hmac)),
            // capsules_extra::sha::DRIVER_NUM => f(Some(self.sha)),
            capsules_core::gpio::DRIVER_NUM => f(Some(self.gpio)),
            capsules_core::console::DRIVER_NUM => f(Some(self.console)),
            capsules_core::alarm::DRIVER_NUM => f(Some(self.alarm)),
            capsules_core::low_level_debug::DRIVER_NUM => f(Some(self.lldb)),
            capsules_core::i2c_master::DRIVER_NUM => f(Some(self.i2c_master)),
            capsules_core::spi_controller::DRIVER_NUM => f(Some(self.spi_controller)),
            capsules_core::rng::DRIVER_NUM => f(Some(self.rng)),
            capsules_extra::symmetric_encryption::aes::DRIVER_NUM => f(Some(self.aes)),
	    capsules_extra::hmac::DRIVER_NUM => f(Some(self.hmac)),
            capsules_extra::kv_driver::DRIVER_NUM => {
                f(self.kv_driver.map(|d| d as &dyn SyscallDriver))
            }
            _ => f(None),
        }
    }
}

impl KernelResources<earlgrey::chip::EarlGrey<'static, EarlGreyDefaultPeripherals<'static>>>
    for EarlGrey
{
    type SyscallDriverLookup = Self;
    type SyscallFilter = TbfHeaderFilterDefaultAllow;
    type ProcessFault = ();
    type CredentialsCheckingPolicy = ();
    type Scheduler = PrioritySched;
    type SchedulerTimer =
        VirtualSchedulerTimer<VirtualMuxAlarm<'static, earlgrey::timer::RvTimer<'static>>>;
    type WatchDog = ();
    type ContextSwitchCallback = ();

    fn syscall_driver_lookup(&self) -> &Self::SyscallDriverLookup {
        &self
    }
    fn syscall_filter(&self) -> &Self::SyscallFilter {
        &self.syscall_filter
    }
    fn process_fault(&self) -> &Self::ProcessFault {
        &()
    }
    fn credentials_checking_policy(&self) -> &'static Self::CredentialsCheckingPolicy {
        &()
    }
    fn scheduler(&self) -> &Self::Scheduler {
        self.scheduler
    }
    fn scheduler_timer(&self) -> &Self::SchedulerTimer {
        &self.scheduler_timer
    }
    fn watchdog(&self) -> &Self::WatchDog {
        &()
    }
    fn context_switch_callback(&self) -> &Self::ContextSwitchCallback {
        &()
    }
}

#[cfg(feature = "want_tickv")]
unsafe fn configure_tickv(
    peripherals: &'static EarlGreyDefaultPeripherals<'static>,
    board_kernel: &'static kernel::Kernel,
) -> Option<
    &'static capsules_extra::kv_driver::KVSystemDriver<
        'static,
        capsules_extra::tickv::TicKVStore<
            'static,
            capsules_core::virtualizers::virtual_flash::FlashUser<
                'static,
                lowrisc::flash_ctrl::FlashCtrl<'static>,
            >,
            capsules_extra::sip_hash::SipHasher24<'static>,
        >,
        [u8; 8],
    >,
> {
    // Flash
    let flash_ctrl_read_buf = static_init!(
        [u8; lowrisc::flash_ctrl::PAGE_SIZE],
        [0; lowrisc::flash_ctrl::PAGE_SIZE]
    );
    let page_buffer = static_init!(
        lowrisc::flash_ctrl::LowRiscPage,
        lowrisc::flash_ctrl::LowRiscPage::default()
    );

    let mux_flash = components::flash::FlashMuxComponent::new(&peripherals.flash_ctrl).finalize(
        components::flash_mux_component_static!(lowrisc::flash_ctrl::FlashCtrl),
    );

    // SipHash
    let sip_hash = static_init!(
        capsules_extra::sip_hash::SipHasher24,
        capsules_extra::sip_hash::SipHasher24::new()
    );

    kernel::deferred_call::DeferredCallClient::register(sip_hash);
    SIPHASH = Some(sip_hash);

    // TicKV
    let tickv = components::tickv::TicKVComponent::new(
        sip_hash,
        &mux_flash,                                  // Flash controller
        0x20060000 / lowrisc::flash_ctrl::PAGE_SIZE, // Region offset (size / page_size)
        0x20000,                                     // Region size
        flash_ctrl_read_buf,                         // Buffer used internally in TicKV
        page_buffer,                                 // Buffer used with the flash controller
    )
    .finalize(components::tickv_component_static!(
        lowrisc::flash_ctrl::FlashCtrl,
        capsules_extra::sip_hash::SipHasher24
    ));
    hil::flash::HasClient::set_client(&peripherals.flash_ctrl, mux_flash);
    sip_hash.set_client(tickv);
    TICKV = Some(tickv);

    let mux_kv = components::kv_system::KVStoreMuxComponent::new(tickv).finalize(
        components::kv_store_mux_component_static!(
            capsules_extra::tickv::TicKVStore<
                capsules_core::virtualizers::virtual_flash::FlashUser<
                    lowrisc::flash_ctrl::FlashCtrl,
                >,
                capsules_extra::sip_hash::SipHasher24<'static>,
            >,
            capsules_extra::tickv::TicKVKeyType,
        ),
    );

    let kv_store = components::kv_system::KVStoreComponent::new(mux_kv).finalize(
        components::kv_store_component_static!(
            capsules_extra::tickv::TicKVStore<
                capsules_core::virtualizers::virtual_flash::FlashUser<
                    lowrisc::flash_ctrl::FlashCtrl,
                >,
                capsules_extra::sip_hash::SipHasher24<'static>,
            >,
            capsules_extra::tickv::TicKVKeyType,
        ),
    );
    tickv.set_client(kv_store);

    // TODO missing paramters
    let kv_driver = components::kv_system::KVDriverComponent::new(
        kv_store,
        board_kernel,
        capsules_extra::kv_driver::DRIVER_NUM,
    )
    .finalize(components::kv_driver_component_static!(
        capsules_extra::tickv::TicKVStore<
            capsules_core::virtualizers::virtual_flash::FlashUser<lowrisc::flash_ctrl::FlashCtrl>,
            capsules_extra::sip_hash::SipHasher24<'static>,
        >,
        capsules_extra::tickv::TicKVKeyType,
    ));
    Some(kv_driver)
}

#[cfg(not(feature = "want_tickv"))]
unsafe fn configure_tickv(
    peripherals: &'static EarlGreyDefaultPeripherals<'static>,
    board_kernel: &'static kernel::Kernel,
) -> Option<
    &'static capsules_extra::kv_driver::KVSystemDriver<
        'static,
        capsules_extra::tickv::TicKVStore<
            'static,
            capsules_core::virtualizers::virtual_flash::FlashUser<
                'static,
                lowrisc::flash_ctrl::FlashCtrl<'static>,
            >,
            capsules_extra::sip_hash::SipHasher24<'static>,
        >,
        [u8; 8],
    >,
> {
    None
}

pub unsafe fn setup() -> (
    &'static kernel::Kernel,
    &'static EarlGrey,
    &'static earlgrey::chip::EarlGrey<'static, EarlGreyDefaultPeripherals<'static>>,
    &'static EarlGreyDefaultPeripherals<'static>,
) {
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
        /// The start of the kernel text (Included only for kernel PMP)
        static _stext: u8;
        /// The end of the kernel text (Included only for kernel PMP)
        static _etext: u8;
        /// The start of the kernel / app / storage flash (Included only for kernel PMP)
        static _sflash: u8;
        /// The end of the kernel / app / storage flash (Included only for kernel PMP)
        static _eflash: u8;
        /// The start of the kernel / app RAM (Included only for kernel PMP)
        static _ssram: u8;
        /// The end of the kernel / app RAM (Included only for kernel PMP)
        static _esram: u8;
        /// The start of the OpenTitan manifest
        static _manifest: u8;
    }

    // Ibex-specific handler
    earlgrey::chip::configure_trap_handler();

    // Set up memory protection immediately after setting the trap handler, to
    // ensure that much of the board initialization routine runs with ePMP
    // protection.
    let earlgrey_epmp = unsafe {
        earlgrey::epmp::EarlGreyEPMP::new(
            // flash
            (
                &_sflash as *const u8,
                &_eflash as *const u8 as usize - &_sflash as *const u8 as usize,
            ),
            // ram
            (
                &_ssram as *const u8,
                &_esram as *const u8 as usize - &_ssram as *const u8 as usize + 0x8000,
            ),
            // mmio
            (0x40000000 as *const u8, 0x10000000),
            // kernel .text
            (&_stext as *const u8, &_etext as *const u8),
        )
        .unwrap()
    };

    // initialize capabilities
    let process_mgmt_cap = create_capability!(capabilities::ProcessManagementCapability);
    let memory_allocation_cap = create_capability!(capabilities::MemoryAllocationCapability);

    let board_kernel = static_init!(kernel::Kernel, kernel::Kernel::new(&PROCESSES));

    let peripherals = static_init!(
        EarlGreyDefaultPeripherals,
        EarlGreyDefaultPeripherals::new()
    );
    peripherals.init();

    // Configure kernel debug gpios as early as possible
    kernel::debug::assign_gpios(
        Some(&peripherals.gpio_port[7]), // First LED
        None,
        None,
    );

    // Create a shared UART channel for the console and for kernel debug.
    let uart_mux = components::console::UartMuxComponent::new(
        &peripherals.uart0,
        earlgrey::uart::UART0_BAUDRATE,
    )
    .finalize(components::uart_mux_component_static!());

    // LEDs
    // Start with half on and half off
    let led = components::led::LedsComponent::new().finalize(components::led_component_static!(
        LedHigh<'static, earlgrey::gpio::GpioPin>,
        LedHigh::new(&peripherals.gpio_port[8]),
        LedHigh::new(&peripherals.gpio_port[9]),
        LedHigh::new(&peripherals.gpio_port[10]),
        LedHigh::new(&peripherals.gpio_port[11]),
        LedHigh::new(&peripherals.gpio_port[12]),
        LedHigh::new(&peripherals.gpio_port[13]),
        LedHigh::new(&peripherals.gpio_port[14]),
        LedHigh::new(&peripherals.gpio_port[15]),
    ));

    let gpio = components::gpio::GpioComponent::new(
        board_kernel,
        capsules_core::gpio::DRIVER_NUM,
        components::gpio_component_helper!(
            earlgrey::gpio::GpioPin,
            0 => &peripherals.gpio_port[0],
            1 => &peripherals.gpio_port[1],
            2 => &peripherals.gpio_port[2],
            3 => &peripherals.gpio_port[3],
            4 => &peripherals.gpio_port[4],
            5 => &peripherals.gpio_port[5],
            6 => &peripherals.gpio_port[6],
            7 => &peripherals.gpio_port[15]
        ),
    )
    .finalize(components::gpio_component_static!(earlgrey::gpio::GpioPin));

    let hardware_alarm = static_init!(earlgrey::timer::RvTimer, earlgrey::timer::RvTimer::new());
    hardware_alarm.setup();

    // Create a shared virtualization mux layer on top of a single hardware
    // alarm.
    let mux_alarm = static_init!(
        MuxAlarm<'static, earlgrey::timer::RvTimer>,
        MuxAlarm::new(hardware_alarm)
    );
    hil::time::Alarm::set_alarm_client(hardware_alarm, mux_alarm);

    ALARM = Some(mux_alarm);

    // Alarm
    let virtual_alarm_user = static_init!(
        VirtualMuxAlarm<'static, earlgrey::timer::RvTimer>,
        VirtualMuxAlarm::new(mux_alarm)
    );
    virtual_alarm_user.setup();

    let scheduler_timer_virtual_alarm = static_init!(
        VirtualMuxAlarm<'static, earlgrey::timer::RvTimer>,
        VirtualMuxAlarm::new(mux_alarm)
    );
    scheduler_timer_virtual_alarm.setup();

    let alarm = static_init!(
        capsules_core::alarm::AlarmDriver<
            'static,
            VirtualMuxAlarm<'static, earlgrey::timer::RvTimer>,
        >,
        capsules_core::alarm::AlarmDriver::new(
            virtual_alarm_user,
            board_kernel.create_grant(capsules_core::alarm::DRIVER_NUM, &memory_allocation_cap)
        )
    );
    hil::time::Alarm::set_alarm_client(virtual_alarm_user, alarm);

    let scheduler_timer = static_init!(
        VirtualSchedulerTimer<VirtualMuxAlarm<'static, earlgrey::timer::RvTimer<'static>>>,
        VirtualSchedulerTimer::new(scheduler_timer_virtual_alarm)
    );

    let chip = static_init!(
        earlgrey::chip::EarlGrey<
            EarlGreyDefaultPeripherals,
        >,
        earlgrey::chip::EarlGrey::new(peripherals, hardware_alarm, earlgrey_epmp)
    );
    CHIP = Some(chip);

    // Need to enable all interrupts for Tock Kernel
    chip.enable_plic_interrupts();
    // enable interrupts globally
    csr::CSR.mie.modify(
        csr::mie::mie::msoft::SET + csr::mie::mie::mtimer::CLEAR + csr::mie::mie::mext::SET,
    );
    csr::CSR.mstatus.modify(csr::mstatus::mstatus::mie::SET);

    // Setup the console.
    let console = components::console::ConsoleComponent::new(
        board_kernel,
        capsules_core::console::DRIVER_NUM,
        uart_mux,
    )
    .finalize(components::console_component_static!());
    // Create the debugger object that handles calls to `debug!()`.
    components::debug_writer::DebugWriterComponent::new(uart_mux)
        .finalize(components::debug_writer_component_static!());

    let lldb = components::lldb::LowLevelDebugComponent::new(
        board_kernel,
        capsules_core::low_level_debug::DRIVER_NUM,
        uart_mux,
    )
    .finalize(components::low_level_debug_component_static!());

    // let mux_digest = components::digest::DigestMuxComponent::new(&peripherals.hmac).finalize(
    //     components::digest_mux_component_static!(lowrisc::hmac::Hmac, 32),
    // );

    // let digest = components::digest::DigestComponent::new(&mux_digest).finalize(
    //     components::digest_component_static!(lowrisc::hmac::Hmac, 32,),
    // );

    // peripherals.hmac.set_client(digest);

    // let mux_hmac = components::hmac::HmacMuxComponent::new(digest).finalize(
    //     components::hmac_xmux_component_static!(capsules_core::virtualizers::virtual_digest::VirtualMuxDigest<lowrisc::hmac::Hmac, 32>, 32),
    // );

    // let hmac = components::hmac::HmacComponent::new(
    //     board_kernel,
    //     capsules_extra::hmac::DRIVER_NUM,
    //     &mux_hmac,
    // )
    // .finalize(components::hmac_component_static!(
    //     capsules_core::virtualizers::virtual_digest::VirtualMuxDigest<lowrisc::hmac::Hmac, 32>,
    //     32,
    // ));

    // digest.set_hmac_client(hmac);

    // let mux_sha = components::sha::ShaMuxComponent::new(digest).finalize(
    //     components::sha_mux_component_static!(capsules_core::virtualizers::virtual_digest::VirtualMuxDigest<lowrisc::hmac::Hmac, 32>, 32),
    // );

    // let sha = components::sha::ShaComponent::new(
    //     board_kernel,
    //     capsules_extra::sha::DRIVER_NUM,
    //     &mux_sha,
    // )
    // .finalize(components::sha_component_static!(capsules_core::virtualizers::virtual_digest::VirtualMuxDigest<lowrisc::hmac::Hmac, 32>, 32));

    // digest.set_sha_client(sha);

    let i2c_master = static_init!(
        capsules_core::i2c_master::I2CMasterDriver<'static, lowrisc::i2c::I2c<'static>>,
        capsules_core::i2c_master::I2CMasterDriver::new(
            &peripherals.i2c0,
            &mut capsules_core::i2c_master::BUF,
            board_kernel.create_grant(
                capsules_core::i2c_master::DRIVER_NUM,
                &memory_allocation_cap
            )
        )
    );

    peripherals.i2c0.set_master_client(i2c_master);

    //SPI
    let mux_spi = components::spi::SpiMuxComponent::new(&peripherals.spi_host0).finalize(
        components::spi_mux_component_static!(lowrisc::spi_host::SpiHost),
    );

    let spi_controller = components::spi::SpiSyscallComponent::new(
        board_kernel,
        mux_spi,
        0,
        capsules_core::spi_controller::DRIVER_NUM,
    )
    .finalize(components::spi_syscall_component_static!(
        lowrisc::spi_host::SpiHost
    ));

    let process_printer = components::process_printer::ProcessPrinterTextComponent::new()
        .finalize(components::process_printer_text_component_static!());
    PROCESS_PRINTER = Some(process_printer);

    // USB support is currently broken in the OpenTitan hardware
    // See https://github.com/lowRISC/opentitan/issues/2598 for more details
    // let usb = usb::UsbComponent::new(board_kernel).finalize(());

    // Kernel storage region, allocated with the storage_volume!
    // macro in common/utils.rs
    extern "C" {
        /// Beginning on the ROM region containing app images.
        static _sstorage: u8;
        static _estorage: u8;
    }

    let mux_otbn = crate::otbn::AccelMuxComponent::new(&peripherals.otbn)
        .finalize(otbn_mux_component_static!());

    let otbn = OtbnComponent::new(&mux_otbn).finalize(crate::otbn_component_static!());

    let otbn_rsa_internal_buf = static_init!([u8; 512], [0; 512]);

    // Use the OTBN to create an RSA engine
    if let Ok((rsa_imem_start, rsa_imem_length, rsa_dmem_start, rsa_dmem_length)) =
        crate::otbn::find_app(
            "otbn-rsa",
            core::slice::from_raw_parts(
                &_sapps as *const u8,
                &_eapps as *const u8 as usize - &_sapps as *const u8 as usize,
            ),
        )
    {
        let rsa_hardware = static_init!(
            lowrisc::rsa::OtbnRsa<'static>,
            lowrisc::rsa::OtbnRsa::new(
                otbn,
                lowrisc::rsa::AppAddresses {
                    imem_start: rsa_imem_start,
                    imem_size: rsa_imem_length,
                    dmem_start: rsa_dmem_start,
                    dmem_size: rsa_dmem_length
                },
                otbn_rsa_internal_buf,
            )
        );
        peripherals.otbn.set_client(rsa_hardware);
        RSA_HARDWARE = Some(rsa_hardware);
    } else {
        debug!("Unable to find otbn-rsa, disabling RSA support");
    }

    // Convert hardware RNG to the Random interface.
    let entropy_to_random = static_init!(
        capsules_core::rng::Entropy32ToRandom<'static>,
        capsules_core::rng::Entropy32ToRandom::new(&peripherals.rng)
    );
    peripherals.rng.set_client(entropy_to_random);
    // Setup RNG for userspace
    let rng = static_init!(
        capsules_core::rng::RngDriver<'static>,
        capsules_core::rng::RngDriver::new(
            entropy_to_random,
            board_kernel.create_grant(capsules_core::rng::DRIVER_NUM, &memory_allocation_cap)
        )
    );
    entropy_to_random.set_client(rng);

    const CRYPT_SIZE: usize = 7 * AES128_BLOCK_SIZE;

    let aes_source_buffer = static_init!([u8; 16], [0; 16]);
    let aes_dest_buffer = static_init!([u8; CRYPT_SIZE], [0; CRYPT_SIZE]);

    let ccm_mux = static_init!(
        virtual_aes_ccm::MuxAES128CCM<'static, earlgrey::aes::Aes<'static>>,
        virtual_aes_ccm::MuxAES128CCM::new(&peripherals.aes)
    );
    kernel::deferred_call::DeferredCallClient::register(ccm_mux);
    peripherals.aes.set_client(ccm_mux);

    let crypt_buf1 = static_init!([u8; CRYPT_SIZE], [0x00; CRYPT_SIZE]);
    let ccm_client1 = static_init!(
        virtual_aes_ccm::VirtualAES128CCM<'static, earlgrey::aes::Aes<'static>>,
        virtual_aes_ccm::VirtualAES128CCM::new(ccm_mux, crypt_buf1)
    );

    ccm_client1.setup();
    // ccm_mux.set_client(ccm_client1);

    let aes = static_init!(
        capsules_extra::symmetric_encryption::aes::AesDriver<
            'static,
            virtual_aes_ccm::VirtualAES128CCM<'static, earlgrey::aes::Aes<'static>>,
        >,
        capsules_extra::symmetric_encryption::aes::AesDriver::new(
            ccm_client1,
            aes_source_buffer,
            aes_dest_buffer,
            board_kernel.create_grant(
                capsules_extra::symmetric_encryption::aes::DRIVER_NUM,
                &memory_allocation_cap
            )
        )
    );

    AES = Some(ccm_client1);

    hil::symmetric_encryption::AES128CCM::set_client(ccm_client1, aes);
    hil::symmetric_encryption::AES128::set_client(ccm_client1, aes);

    // These symbols are defined in the linker script.
    extern "C" {
        static _dsvcram_start: u8;
        static _dsvcram_end: u8;
    }

    let cryptosvc_binary = contsvc::ContSvcBinary::find(
        "cryptolib_cmpsvc",
        core::slice::from_raw_parts(
            &_sapps as *const u8,
            &_eapps as *const u8 as usize - &_sapps as *const u8 as usize,
        ),
    )
    .unwrap();

    let cryptosvc = contsvc::ContSvc::new(
        chip,
        cryptosvc_binary,
        &_dsvcram_start as *const u8 as *mut u8,
        &_dsvcram_end as *const u8 as usize - &_dsvcram_start as *const u8 as usize,
    )
	.unwrap();

    type CryptolibHmacInst = crate::cryptolib_hmac::CryptolibHmac<'static, 'static, earlgrey::chip::EarlGrey<'static, EarlGreyDefaultPeripherals<'static>>>;

    let hmac_sha256_cryptolib = static_init!(
	CryptolibHmacInst,
	crate::cryptolib_hmac::CryptolibHmac::new(cryptosvc),
    );


    // let hmac = components::hmac::HmacComponent::new(
    //     board_kernel,
    //     capsules_extra::hmac::DRIVER_NUM,
    //     hmac_sha256_cryptolib,
    // )
    // .finalize(components::hmac_component_static!(
    //     CryptolibHmacInst,
    //     32
    // ));


    let mux_hmac = components::hmac::HmacMuxComponent::new(hmac_sha256_cryptolib).finalize(
        components::hmac_mux_component_static!(CryptolibHmacInst, 32),
    );

    let hmac = components::hmac::HmacComponent::new(
        board_kernel,
        capsules_extra::hmac::DRIVER_NUM,
        &mux_hmac,
    )
    .finalize(components::hmac_component_static!(
	CryptolibHmacInst,
        32,
    ));


    let syscall_filter = static_init!(TbfHeaderFilterDefaultAllow, TbfHeaderFilterDefaultAllow {});
    let scheduler = components::sched::priority::PriorityComponent::new(board_kernel)
        .finalize(components::priority_component_static!());

    let earlgrey = static_init!(
        EarlGrey,
        EarlGrey {
            gpio: gpio,
            led: led,
            console: console,
            alarm: alarm,
            // hmac,
            // sha,
            rng,
            lldb: lldb,
            i2c_master,
            spi_controller,
            aes,
            kv_driver: configure_tickv(peripherals, board_kernel),
	    hmac,
            syscall_filter,
            scheduler,
            scheduler_timer,
        }
    );

    kernel::process::load_processes(
        board_kernel,
        chip,
        core::slice::from_raw_parts(
            &_sapps as *const u8,
            &_eapps as *const u8 as usize - &_sapps as *const u8 as usize,
        ),
        core::slice::from_raw_parts_mut(
            &mut _sappmem as *mut u8,
            &_eappmem as *const u8 as usize - &_sappmem as *const u8 as usize,
        ),
        &mut PROCESSES,
        &FAULT_RESPONSE,
        &process_mgmt_cap,
    )
    .unwrap_or_else(|err| {
        debug!("Error loading processes!");
        debug!("{:?}", err);
    });

    // let (fnptr, res) =
    //     dummysvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_blinded_key_t, _, _>(|blinded_key| {
    //         dummysvc.allocate_stacked(32, 4, |tagblob| {
    //  // {
    //  //     let bk: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key };
    //  //     bk.keyblob = keyblob as *mut u32;
    //  //     bk.keyblob_length = 64;
    //  // }

    //  let fnptr = dummysvc.resolve_function_pointer(7).unwrap();
    //  let res = dummysvc.invoke_service(fnptr, tagblob as usize, 0, 0, 0, 0, 0, 0, 0, false);
    //  tag.copy_from_slice(unsafe { core::slice::from_raw_parts(tagblob, 32) });
    //  (fnptr, res)
    //         }).unwrap()
    //     }).unwrap();

    // debug!("OpenTitan initialisation complete. Entering main loop: {:p} {:?} {:02x?}", fnptr, res, tag);

    // let res =
    //     cryptosvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_blinded_key_t, _, _>(|blinded_key_ptr| {
    //         // Set the blinded key config prior to calling the `keyblob_num_words` helper:
    //         let config_ptr = {
    //     let blinded_key: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key_ptr };

    //     // Set the blinded key config first:
    //     blinded_key.config = crate::cryptolib_mac::crypto_key_config {
    //         version: crate::cryptolib_mac::crypto_lib_version_kCryptoLibVersion1,
    //         key_mode: crate::cryptolib_mac::key_mode_kKeyModeHmacSha256,
    //         key_length: 32, // HMAC-SHA256
    //         hw_backed: crate::cryptolib_mac::hardened_bool_kHardenedBoolFalse,
    //         diversification_hw_backed: crate::cryptolib_mac::crypto_const_uint8_buf_t {
    //             data: core::ptr::null(),
    //             len: 0,
    //         },
    //         exportable: crate::cryptolib_mac::hardened_bool_kHardenedBoolFalse,
    //         security_level: crate::cryptolib_mac::crypto_key_security_level_kSecurityLevelLow,
    //     };

    //     &mut blinded_key.config as *mut crate::cryptolib_mac::crypto_key_config
    //         };

    //         let (keyblob_words, _) = cryptosvc.invoke_service(
    //     // keyblob_num_words (wrapped)
    //     cryptosvc.resolve_function_pointer(1).unwrap(),
    //     blinded_key_ptr as usize, 0, 0, 0, 0, 0, 0, 0, false
    //         ).unwrap();

    //         cryptosvc.allocate_stacked_array::<17, u32, _, _>(|test_mask_ptr| {
    //     {
    //         (unsafe { &mut *test_mask_ptr }).copy_from_slice(&[
    //             0x8cb847c3, 0xc6d34f36, 0x72edbf7b, 0x9bc0317f, 0x8f003c7f, 0x1d7ba049,
    //             0xfd463b63, 0xbb720c44, 0x784c215e, 0xeb101d65, 0x35beb911, 0xab481345,
    //             0xa7ebc3e3, 0x04b2a1b9, 0x764a9630, 0x78b8f9c5, 0x3f2a1d8e,
    //         ]);
    //     }

    //     cryptosvc.allocate_stacked_array::<8, u32, _, _>(|test_key_ptr| {
    //         {
    //             (unsafe { &mut *test_key_ptr }).copy_from_slice(&[
    //                 0xea10ff1b, 0x04b2b9a5, 0x2a23f3d6, 0x518e3e57,
    //                 0xc3687ba2, 0xea6d3619, 0xb0916bf2, 0x347a2f71,
    //             ]);
    //         }

    //         cryptosvc.allocate_stacked_slice::<u32, _, _>(keyblob_words, |keyblob_slice_ptr| {
    //             let keyblob_res =  cryptosvc.invoke_service(
    //                 // keyblob_from_key_and_mask (wrapped)
    //                 cryptosvc.resolve_function_pointer(5).unwrap(),
    //                 test_key_ptr as *mut u32 as usize,
    //                 test_mask_ptr as *mut u32 as usize,
    //                 config_ptr as usize,
    //                 keyblob_slice_ptr as *mut u32 as usize,
    //                 0, 0, 0, 0, false
    //             ).unwrap();

    //             {
    //                 let blinded_key: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key_ptr };
    //                 blinded_key.keyblob = keyblob_slice_ptr as *mut u32;
    //                 blinded_key.keyblob_length = keyblob_words * core::mem::size_of::<u32>();
    //                 blinded_key.checksum = 0;
    //             }

    //             let (checksum, _) =  cryptosvc.invoke_service(
    //                 // integrity_blinded_checksum
    //                 cryptosvc.resolve_function_pointer(14).unwrap(),
    //                 blinded_key_ptr as usize,
    //                 0, 0, 0, 0, 0, 0, 0, false
    //             ).unwrap();

    //             {
    //                 let blinded_key: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key_ptr };
    //                 blinded_key.checksum = checksum as u32;
    //             }

    //             cryptosvc.allocate_stacked_array::<{256 / 32}, u32, _, _>(|act_tag_ptr| {
    //                 cryptosvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_uint8_buf_t, _, _>(|tag_buf_ptr| {
    //                     {
    //                         let tag_buf: &mut crate::cryptolib_mac::crypto_uint8_buf_t = unsafe { &mut *tag_buf_ptr };
    //                         tag_buf.data = act_tag_ptr as *mut u32 as *mut u8;
    //                         tag_buf.len = 256 / 8;
    //                     }

    //                     cryptosvc.allocate_stacked_array::<13, u8, _, _>(|msg_ptr| {
    //                         (unsafe { &mut *msg_ptr }).copy_from_slice(b"Test message.");

    //                         cryptosvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_uint8_buf_t, _, _>(|msg_buf_ptr| {
    //                             {
    //                                 let msg_buf: &mut crate::cryptolib_mac::crypto_uint8_buf_t = unsafe { &mut *msg_buf_ptr };
    //                                 msg_buf.data = msg_ptr as *mut u8;
    //                                 msg_buf.len = 13;
    //                             }

    //                             let keyblob_ptr_0 = (unsafe { &* blinded_key_ptr}).keyblob;

    //                             let res = cryptosvc.invoke_service(
    //                                 // otcrypto_hmac_wrapped
    //                                 cryptosvc.resolve_function_pointer(8).unwrap(),
    //                                 blinded_key_ptr as usize,
    //                                 msg_buf_ptr as usize,
    //                                 tag_buf_ptr as usize, 0, 0, 0, 0, 0, false
    //                             ).unwrap();

    //                             let keyblob_ptr_1 = (unsafe { &* blinded_key_ptr}).keyblob;

    //                             // panic!("Keyblob res: {:x?} {:x?}, Checksum: {:x?}, hmac res {:x?}, {:p} {:p} [{:p} {:p} {:p}], key mode {:?}", keyblob_res, {&*keyblob_slice_ptr}, checksum, res, blinded_key_ptr, (unsafe { &* blinded_key_ptr}).keyblob, keyblob_ptr_0, keyblob_ptr_1, keyblob_slice_ptr, &(unsafe { &* blinded_key_ptr}).config.key_mode);

    //                             tag.copy_from_slice(unsafe { &*act_tag_ptr });

    //                             res
    //                                 }).unwrap()
    //                             }).unwrap()
    //                         }).unwrap()
    //                     }).unwrap()
    //                 }).unwrap()
    //             }).unwrap()
    //         }).unwrap()
    //     }).unwrap();

    // dummysvc.allocate_stacked(64, 4, |tagblob| { //
    //  // {
    //  //     let bk: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key };
    //  //     bk.keyblob = keyblob as *mut u32;
    //  //     bk.keyblob_length = 64;
    //  // }

    //  let fnptr = dummysvc.resolve_function_pointer(7).unwrap();
    //  let res = dummysvc.invoke_service(fnptr, tagblob as usize, 0, 0, 0, 0, 0, 0, 0, false);
    //  tag.copy_from_slice(unsafe { core::slice::from_raw_parts(tagblob, 32) });
    //  (fnptr, res)
    // }).unwrap()

    // let mut tag = [0_u8; 32];

    // hmac_sha256_cryptolib.hmac_update(b"Test message.").unwrap();
    // hmac_sha256_cryptolib.hmac_finalize(&mut tag);
    


    // debug!(
    //     "OpenTitan initialisation complete. Entering main loop: {:02x?}",
    //     tag
    // );

    (board_kernel, earlgrey, chip, peripherals)
}
