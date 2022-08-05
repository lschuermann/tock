use core::cell::Cell;
use kernel::utilities::cells::{TakeCell, OptionalCell, MapCell};
use kernel::hil::spi::{SpiMasterDevice, SpiMasterClient};
use kernel::ErrorCode;
use kernel::debug;
use kernel::hil::gpio;
use kernel::dynamic_deferred_call::{
    DeferredCallHandle, DynamicDeferredCall, DynamicDeferredCallClient,
};

macro_rules! enc_bank {
    ($bank:expr, $reg:expr) => {
	($bank << 5) | $reg
    }
}

pub static mut TXBUFFER: [u8; 512] = [0; 512];
pub static mut RXBUFFER: [u8; 512] = [0; 512];
pub static mut BUFMEMBUFFER: [u8; 16] = [0; 16];

// Using a common panic message reference for buffers not present
// errors to reduce binary size
const BUFFER_ERROR: &'static str = "enc28j60 buffer not present";

const OPCODE_BITMASK: u8 = 0x07 << 5;
const PARAMETER_BITMASK: u8 = 0x1F;

const OPCODE_RESET: u8 = 0x07 << 5;
const CONSTP_RESET: u8 = 0x1F;
const OPCODE_READCONTROLREG: u8 = 0x00 << 5;
const OPCODE_WRITECONTROLREG: u8 = 0x02 << 5;
const OPCODE_READBUFMEM: u8 = 0x01 << 5;
const CONSTP_READBUFMEM: u8 = 0x1A;
const OPCODE_WRITEBUFMEM: u8 = 0x03 << 5;
const CONSTP_WRITEBUFMEM: u8 = 0x1A;
const OPCODE_BITFIELDSET: u8 = 0x04 << 5;
const OPCODE_BITFIELDCLEAR: u8 = 0x05 << 5;

const ETHERNET_PKT_MAXLEN: u16 = 1518;
// Should be an even number
const ENC28J60_TX_PACKET_LEN: u16 = 1 + ETHERNET_PKT_MAXLEN + 7;

const ENC28J60_BUFFER_LEN: u16 = 0x1FFF;
const ENC28J60_RXBUF_START: u16 = 0x0000;
// End address must be an odd number, see Errata 11: Memory (Ethernet Buffer)
const ENC28J60_RXBUF_END: u16 = ENC28J60_BUFFER_LEN - ENC28J60_TX_PACKET_LEN - 2;
const ENC28J60_TXBUF_START: u16 = ENC28J60_RXBUF_END + 2;
const ENC28J60_TXBUF_END: u16 = ENC28J60_BUFFER_LEN;

// Next packet pointer must be an odd number, function from errata
// 11: Memory (Ethernet Buffer)
fn next_packet_workaround(ptr: u16) -> u16 {
    if ((ptr as i16) - 1) < ENC28J60_RXBUF_START as i16
	|| ((ptr as i16) - 1) > ENC28J60_RXBUF_END as i16 {
	    ENC28J60_RXBUF_END
	} else {
	    ptr - 1
    }
}

pub type MacAddr = [u8; 6];

#[repr(usize)]
#[derive(Debug, Copy, Clone, PartialEq)]
enum ENC28J60Bank {
    Bank0 = 0,
    Bank1 = 1,
    Bank2 = 2,
    Bank3 = 3,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
enum ENC28J60Register {
    // all banks (encoded as bank 4)
    EIE = enc_bank!(4, 0x1B),
    EIR = enc_bank!(4, 0x1C),
    ESTAT = enc_bank!(4, 0x1D),
    ECON2 = enc_bank!(4, 0x1E),
    ECON1 = enc_bank!(4, 0x1F),

    // bank 0
    ERDPTL = enc_bank!(0, 0x00),
    ERDPTH = enc_bank!(0, 0x01),
    EWRPTL = enc_bank!(0, 0x02),
    EWRPTH = enc_bank!(0, 0x03),
    ETXSTL = enc_bank!(0, 0x04),
    ETXSTH = enc_bank!(0, 0x05),
    ETXNDL = enc_bank!(0, 0x06),
    ETXNDH = enc_bank!(0, 0x07),
    ERXSTL = enc_bank!(0, 0x08),
    ERXSTH = enc_bank!(0, 0x09),
    ERXNDL = enc_bank!(0, 0x0A),
    ERXNDH = enc_bank!(0, 0x0B),
    ERXRDPTL = enc_bank!(0, 0x0C),
    ERXRDPTH = enc_bank!(0, 0x0D),
    ERXWRPTL = enc_bank!(0, 0x0E),
    ERXWRPTH = enc_bank!(0, 0x0F),
    EDMASTL = enc_bank!(0, 0x10),
    EDMASTH = enc_bank!(0, 0x11),
    EDMANDL = enc_bank!(0, 0x12),
    EDMANDH = enc_bank!(0, 0x13),
    EDMADSTL = enc_bank!(0, 0x14),
    EDMADSTH = enc_bank!(0, 0x15),
    EDMACSL = enc_bank!(0, 0x16),
    EDMACSH = enc_bank!(0, 0x17),

    // bank 1
    EHT0 = enc_bank!(1, 0x00),
    EHT1 = enc_bank!(1, 0x01),
    EHT2 = enc_bank!(1, 0x02),
    EHT3 = enc_bank!(1, 0x03),
    EHT4 = enc_bank!(1, 0x04),
    EHT5 = enc_bank!(1, 0x05),
    EHT6 = enc_bank!(1, 0x06),
    EHT7 = enc_bank!(1, 0x07),
    EPMM0 = enc_bank!(1, 0x08),
    EPMM1 = enc_bank!(1, 0x09),
    EPMM2 = enc_bank!(1, 0x0A),
    EPMM3 = enc_bank!(1, 0x0B),
    EPMM4 = enc_bank!(1, 0x0C),
    EPMM5 = enc_bank!(1, 0x0D),
    EPMM6 = enc_bank!(1, 0x0E),
    EPMM7 = enc_bank!(1, 0x0F),
    EPMCSL = enc_bank!(1, 0x10),
    EPMCSH = enc_bank!(1, 0x11),
    EPMOL = enc_bank!(1, 0x14),
    EPMOH = enc_bank!(1, 0x15),
    ERXFCON = enc_bank!(1, 0x18),
    EPKTCNT = enc_bank!(1, 0x19),

    // bank 2
    MACON1 = enc_bank!(2, 0x00),
    MACON3 = enc_bank!(2, 0x02),
    MACON4 = enc_bank!(2, 0x03),
    MABBIPG = enc_bank!(2, 0x04),
    MAIPGL = enc_bank!(2, 0x06),
    MAIPGH = enc_bank!(2, 0x07),
    MACLCON1 = enc_bank!(2, 0x08),
    MACLCON2 = enc_bank!(2, 0x09),
    MAMXFLL = enc_bank!(2, 0x0A),
    MAMXFLH = enc_bank!(2, 0x0B),
    MICMD = enc_bank!(2, 0x12),
    MIREGADR = enc_bank!(2, 0x14),
    MIWRL = enc_bank!(2, 0x16),
    MIWRH = enc_bank!(2, 0x17),
    MIRDL = enc_bank!(2, 0x18),
    MIRDH = enc_bank!(2, 0x19),

    // bank 3
    MAADR5 = enc_bank!(3, 0x00),
    MAADR6 = enc_bank!(3, 0x01),
    MAADR3 = enc_bank!(3, 0x02),
    MAADR4 = enc_bank!(3, 0x03),
    MAADR1 = enc_bank!(3, 0x04),
    MAADR2 = enc_bank!(3, 0x05),
    EBSTSD = enc_bank!(3, 0x06),
    EBSTCON = enc_bank!(3, 0x07),
    EBSTCSL = enc_bank!(3, 0x08),
    EBSTCSH = enc_bank!(3, 0x09),
    MISTAT = enc_bank!(3, 0x0A),
    EREVID = enc_bank!(3, 0x12),
    ECOCON = enc_bank!(3, 0x15),
    EFLOCON = enc_bank!(3, 0x17),
    EPAUSL = enc_bank!(3, 0x18),
    EPAUSH = enc_bank!(3, 0x19),
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
enum ENC28J60PHYRegister {
    PHCON1 = 0x00,
    PHSTAT1 = 0x01,
    PHID1 = 0x02,
    PHID2 = 0x03,
    PHCON2 = 0x10,
    PHSTAT2 = 0x11,
    PHIE = 0x12,
    PHIR = 0x13,
    PHLCON = 0x14,
}

impl ENC28J60Register {
    pub fn to_bank_address(&self) -> (Option<ENC28J60Bank>, u8) {
	let numeric = *self as u8;
	let numeric_bank = numeric >> 5;
	let register = numeric & 0x1F;

	let bank =
	    match numeric_bank {
		0 => Some(ENC28J60Bank::Bank0),
		1 => Some(ENC28J60Bank::Bank1),
		2 => Some(ENC28J60Bank::Bank2),
		3 => Some(ENC28J60Bank::Bank3),
		4 => None,
		_ => panic!("unknown enc28j60 bank"),
	    };

	(bank, register)
    }

    pub fn check_bank(&self, bank: Option<ENC28J60Bank>) -> bool {
	if let (Some(dst_bank), _) = self.to_bank_address() {
	    // Register specific to one bank
	    if let Some(cur_bank) = bank {
		// Current bank known, compare
		cur_bank == dst_bank
	    } else {
		// Current bank unknown, must set explicitly
		false
	    }
	} else {
	    // Register available on every bank
	    true
	}
    }

    pub fn wide_reg(&self) -> Option<(ENC28J60Register, ENC28J60Register)> {
	use ENC28J60Register as R;

	match *self {
	    R::ERDPTL | R::ERDPTH => Some((R::ERDPTL, R::ERDPTH)),
	    R::EWRPTL | R::EWRPTH => Some((R::EWRPTL, R::EWRPTH)),
	    R::ETXSTL | R::ETXSTH => Some((R::ETXSTL, R::ETXSTH)),
	    R::ETXNDL | R::ETXNDH => Some((R::ETXNDL, R::ETXNDH)),
	    R::ERXSTL | R::ERXSTH => Some((R::ERXSTL, R::ERXSTH)),
	    R::ERXNDL | R::ERXNDH => Some((R::ERXNDL, R::ERXNDH)),
	    R::ERXRDPTL | R::ERXRDPTH => Some((R::ERXRDPTL, R::ERXRDPTH)),
	    R::ERXWRPTL | R::ERXWRPTH => Some((R::ERXWRPTL, R::ERXWRPTH)),
	    R::EDMASTL | R::EDMASTH => Some((R::EDMASTL, R::EDMASTH)),
	    R::EDMANDL | R::EDMANDH => Some((R::EDMANDL, R::EDMANDH)),
	    R::EDMADSTL | R::EDMADSTH => Some((R::EDMADSTL, R::EDMADSTH)),
	    R::EDMACSL | R::EDMACSH => Some((R::EDMACSL, R::EDMACSH)),
	    R::EPMCSL | R::EPMCSH => Some((R::EPMCSL, R::EPMCSH)),
	    R::EPMOL | R::EPMOH => Some((R::EPMOL, R::EPMOH)),
	    R::MAIPGL | R::MAIPGH => Some((R::MAIPGL, R::MAIPGH)),
	    R::MAMXFLL | R::MAMXFLH => Some((R::MAMXFLL, R::MAMXFLH)),
	    R::MIWRL | R::MIWRH => Some((R::MIWRL, R::MIWRH)),
	    R::MIRDL | R::MIRDH => Some((R::MIRDL, R::MIRDH)),
	    R::EBSTCSL | R::EBSTCSH => Some((R::EBSTCSL, R::EBSTCSH)),
	    R::EPAUSL | R::EPAUSH => Some((R::EPAUSL, R::EPAUSH)),
	    _ => None,
	}
    }
}

#[derive(Debug, Copy, Clone)]
enum ENC28J60SpiOperation {
    SystemReset,
    ReadControlRegister(ENC28J60Register),
    ReadWideControlRegister(ENC28J60Register),
    ReadBufferMemory(Option<u16>, usize),
    WriteControlRegister(ENC28J60Register, u8),
    WriteWideControlRegister(ENC28J60Register, u16),
    WriteBufferMemory(u16, usize, usize),
    BitFieldSet(ENC28J60Register, u8),
    BitFieldClear(ENC28J60Register, u8),
    ReadPHYRegister(ENC28J60PHYRegister),
    WritePHYRegister(ENC28J60PHYRegister, u16),
}

#[derive(Debug, Copy, Clone)]
enum ENC28J60SpiOperationResult {
    ResetDone,
    ReadControlRegister(ENC28J60Register, u8),
    ReadWideControlRegister(ENC28J60Register, u16),
    ReadBufferMemory(Option<u16>, usize),
    WroteControlRegister(ENC28J60Register, u8),
    WroteWideControlRegister(ENC28J60Register, u16),
    WroteBufferMemory(u16, usize, usize),
    BitFieldSet(ENC28J60Register, u8),
    BitFieldCleared(ENC28J60Register, u8),
    ReadPHYRegister(ENC28J60PHYRegister, u16),
    WritePHYRegister(ENC28J60PHYRegister, u16),
}

#[derive(Debug, Copy, Clone)]
enum ENC28J60SpiState {
    /// Intermediate stage for selecting the correct register
    /// bank. This is required for banks 1 and 2, where both a BIT
    /// FIELD SET and BIT FIELD CLEAR operation are required.
    ///
    /// Next possible states:
    /// - BankSelected
    BankSelectTwoStage(ENC28J60Bank),

    /// The bank for reading / writing a bank has been
    /// selected. Continue with the original operation.
    ///
    /// Next possible states (reconstructed from the
    /// ENC28J60SpiOperation):
    /// - ReadControlRegister
    /// - WriteControlRegister
    /// - BitFieldSet
    /// - BitFieldClear
    /// - ReadPHYRegister (only after selecting bank 2)
    /// - WritePHYRegister (only after selecting bank 2)
    BankSelected(ENC28J60Bank),

    /// Perform a soft system reset command (data sheet 11.2)
    ///
    /// Next possible states:
    /// - ResetDone
    SystemReset,

    /// Soft system reset done
    ///
    /// Return to the caller (callback) with ResetDone result
    ResetDone,
    ReadControlRegister(ENC28J60Register),
    ReadHighControlRegister(ENC28J60Register),
    ControlRegisterRead(ENC28J60Register, Option<u8>),
    WriteControlRegister(ENC28J60Register, Result<u8, u16>),
    WriteHighControlRegister(ENC28J60Register, Result<u8, u16>),
    ControlRegisterWritten(ENC28J60Register, Result<u8, u16>),
    BufferMemoryWritten(u16, usize, usize),
    BitFieldSet(ENC28J60Register, u8),
    BitFieldSetDone(ENC28J60Register, u8),
    BitFieldClear(ENC28J60Register, u8),
    BitFieldClearDone(ENC28J60Register, u8),

    /// Read contents of a PHY register
    ///
    /// Depending on the currently selected bank, the next state is
    /// either:
    /// - BankSelectTwoStage
    /// - BankSelected
    /// - ReadPHYRegExec
    ReadPHYRegAddr(ENC28J60PHYRegister),

    /// Read contents of a PHY register (execute the read operation)
    ///
    /// This assumes that bank 2 has been selected
    ///
    /// The next state will be ReadPHYRegWait
    ReadPHYRegExec(ENC28J60PHYRegister),

    /// Read contents of a PHY register (wait for the operation to finish)
    ///
    /// This assumes that bank 2 has been selected
    ///
    /// The next state will be ReadPHYRegWaitClear
    ReadPHYRegWait(ENC28J60PHYRegister),

    /// Read contents of a PHY register (wait for the operation to
    /// finish or reset the PHY operation)
    ///
    /// This assumes that bank 2 has been selected
    ///
    /// Depending on the result of ReadPHYRegWait, this state will
    /// loop waiting for the operation to be done, and finally jump to
    /// ReadPHYRegReadLow
    ReadPHYRegWaitClear(ENC28J60PHYRegister),

    /// Read contents of a PHY register (read the low byte)
    ///
    /// This assumes that bank 2 has been selected
    ///
    /// The next state will be ReadPHYRegReadHigh
    ReadPHYRegReadLow(ENC28J60PHYRegister),

    /// Read contents of a PHY register (read the high byte)
    ///
    /// This assumes that bank 2 has been selected
    ///
    /// The next state will be ReadPHYRegDone
    ReadPHYRegReadHigh(ENC28J60PHYRegister),

    /// Reading PHY register done
    ///
    /// Return to the caller (callback) with ReadPHYRegister result
    ReadPHYRegDone(ENC28J60PHYRegister, u8),

    WritePHYRegAddr(ENC28J60PHYRegister, u16),
    WritePHYRegWriteLow(ENC28J60PHYRegister, u16),
    WritePHYRegWriteHigh(ENC28J60PHYRegister, u16),
    WritePHYRegWait(ENC28J60PHYRegister, u16),
    WritePHYRegWaitDone(ENC28J60PHYRegister, u16),

    WriteBufferMemorySetAutoinc(u16, usize, usize),
    WriteBufferMemorySetBank(u16, usize, usize),
    WriteBufferMemoryStartAddrLow(u16, usize, usize),
    WriteBufferMemoryStartAddrHigh(u16, usize, usize),
    WriteBufferMemoryWrite(u16, usize, usize, usize),


    ReadBufferMemorySetBank(u16, usize),
    ReadBufferMemoryStartAddrLow(u16, usize),
    ReadBufferMemoryStartAddrHigh(u16, usize),
    ReadBufferMemorySetAutoinc(Option<u16>, usize),
    ReadBufferMemoryRead(Option<u16>, usize, usize, usize),
}

impl ENC28J60SpiOperation {
    fn to_init_state(&self) -> ENC28J60SpiState {
	use ENC28J60SpiOperation as Op;
	use ENC28J60SpiState as State;

	match *self {
	    Op::SystemReset => State::SystemReset,
	    Op::ReadControlRegister(reg) => State::ReadControlRegister(reg),
	    Op::ReadWideControlRegister(reg) => State::ReadControlRegister(reg),
	    Op::ReadBufferMemory(Some(addr), len) => State::ReadBufferMemorySetBank(addr, len),
	    Op::ReadBufferMemory(None, len) => State::ReadBufferMemorySetAutoinc(None, len),
	    Op::WriteControlRegister(reg, val) => State::WriteControlRegister(reg, Ok(val)),
	    Op::WriteWideControlRegister(reg, val) => State::WriteControlRegister(reg, Err(val)),
	    Op::WriteBufferMemory(addr, start, end) => State::WriteBufferMemorySetAutoinc(addr, start, end),
	    Op::BitFieldSet(reg, bf) => State::BitFieldSet(reg, bf),
	    Op::BitFieldClear(reg, bf) => State::BitFieldClear(reg, bf),
	    Op::ReadPHYRegister(reg) => State::ReadPHYRegAddr(reg),
	    Op::WritePHYRegister(reg, val) => State::WritePHYRegAddr(reg, val),
	}
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ENC28J60ReceivePacketError {
    NoPacketQueued,
}

#[derive(Debug, Copy, Clone)]
struct ENC28J60RXStatusVector {
    next_packet: u16,
    frame_length: usize,
    pending_packets: usize,
}

#[derive(Debug, Copy, Clone)]
enum ENC28J60State {
    Uninitialized,
    ChipReset,
    DetermineHWRev,
    SetRXBufferStart,
    SetRXBufferEnd,
    SetInitRXReadPointer,
    ConfigureMac0,
    ConfigureMaxFrameLength,
    ConfigureBtBInterPacketGap,
    ConfigureNBtBInterPacketGap,
    SetMacAddress(usize),
    ConfigureMac2,
    SetPHYFullDuplex,
    EnableInterrupts,
    ConfigureReceiveFilters,
    EnableRX,
    InitializeDone,
    Initialized,

    TransmitSetStartPointer(usize, bool),
    TransmitWriteControlByte(usize, bool),
    TransmitWritePacket(usize),
    TransmitSetEndPointer(u16),
    TransmitClearInterrupts,
    TransmitEnableInterrupts,
    TransmitStart,
    TransmitStarted,

    ReceivePacketGetCount,
    ReceivePacketReadStatusVector,
    ReceivePacketReadPkt(usize),
    ReceivePacketAdvRXPtr(ENC28J60RXStatusVector),
    ReceivePacketDecCount(ENC28J60RXStatusVector),
    ReceivePacketReenableInterrupt(ENC28J60RXStatusVector),
    ReceivePacketDone(Result<ENC28J60RXStatusVector, ENC28J60ReceivePacketError>),

    HandleInterrupt,
    HandleInterruptRead,
    HandleInterruptLoop(u8),
}

impl ENC28J60State {
    pub fn is_initialized(&self) -> bool {
	use ENC28J60State as S;

	match *self {
	    S::Uninitialized => false,
	    S::ChipReset => false,
	    S::SetMacAddress(_) => false,
	    S::SetRXBufferStart => false,
	    S::SetRXBufferEnd => false,
	    S::InitializeDone => false,
	    _ => true,
	}
    }

    pub fn is_busy(&self) -> bool {
	match *self {
	    ENC28J60State::Uninitialized => false,
	    ENC28J60State::Initialized => false,
	    _ => true,
	}
    }

    pub fn is_ready(&self) -> bool {
	self.is_initialized() && !self.is_busy()
    }
}

#[derive(Debug, Default)]
struct ENC28J60DeferredCalls {
    device_initialized: Option<usize>,
    packet_sent: Option<(&'static mut [u8], Result<(), ErrorCode>)>,
    packet_pending: Option<()>,
    packet_received: Option<(&'static mut [u8], Result<usize, ENC28J60ReceivePacketError>)>,
}

pub trait ENC28J60Client {
    fn device_initialized(&self, hw_rev: usize);
    fn packet_sent(&self, buffer: &'static mut [u8], res: Result<(), ErrorCode>);
    fn packet_pending(&self);
    fn packet_received(&self, buffer: &'static mut [u8], res: Result<usize, ENC28J60ReceivePacketError>);
}

pub struct ENC28J60<'a, S: SpiMasterDevice> {
    spi: &'a S,
    int_pin: &'static dyn gpio::InterruptPin<'static>,
    txbuffer: TakeCell<'static, [u8]>,
    rxbuffer: TakeCell<'static, [u8]>,
    bufmembuffer: TakeCell<'static, [u8]>,

    mac_addr: MacAddr,
    current_bank: Cell<Option<ENC28J60Bank>>,

    deferred_calls: MapCell<ENC28J60DeferredCalls>,
    deferred_caller: &'a DynamicDeferredCall,
    deferred_call_handle: OptionalCell<DeferredCallHandle>,
    interrupt_pending: Cell<bool>,
    hw_rev: OptionalCell<usize>,
    current_spi_operation: OptionalCell<(ENC28J60SpiOperation, ENC28J60SpiState)>,
    state: Cell<ENC28J60State>,
    buffer_memory_buf: TakeCell<'static, [u8]>,
    current_transmission: MapCell<Result<&'static mut [u8], ()>>,
    current_reception: MapCell<Result<&'static mut [u8], ()>>,
    suppress_rx_interrupt: Cell<bool>,
    next_packet_pointer: Cell<u16>,

    device_client: OptionalCell<&'a dyn ENC28J60Client>,
}

impl<'a, S: SpiMasterDevice> ENC28J60<'a, S> {
    pub fn new(
	spi: &'a S,
	int_pin: &'static dyn gpio::InterruptPin,
	deferred_caller: &'a DynamicDeferredCall,
	txbuffer: &'static mut [u8],
	rxbuffer: &'static mut [u8],
	bufmembuffer: &'static mut [u8],
	mac_addr: MacAddr
    ) -> ENC28J60<'a, S> {
	if txbuffer.len() != rxbuffer.len() {
	    panic!("enc28j60: buffers have different lengths");
	}

	int_pin.enable_interrupts(gpio::InterruptEdge::FallingEdge);

	ENC28J60 {
	    spi: spi,
	    int_pin: int_pin,
	    txbuffer: TakeCell::new(txbuffer),
	    rxbuffer: TakeCell::new(rxbuffer),
	    bufmembuffer: TakeCell::new(bufmembuffer),
	    mac_addr: mac_addr,
	    current_bank: Cell::new(None),
	    deferred_calls: MapCell::new(ENC28J60DeferredCalls::default()),
	    deferred_caller: deferred_caller,
	    deferred_call_handle: OptionalCell::empty(),
	    interrupt_pending: Cell::new(false),
	    hw_rev: OptionalCell::empty(),
	    current_spi_operation: OptionalCell::empty(),
	    state: Cell::new(ENC28J60State::Uninitialized),
	    current_transmission: MapCell::empty(),
	    current_reception: MapCell::empty(),
	    buffer_memory_buf: TakeCell::empty(),
	    suppress_rx_interrupt: Cell::new(false),
	    device_client: OptionalCell::empty(),
	    next_packet_pointer: Cell::new(ENC28J60_RXBUF_START),
	}
    }

    pub fn initialize_deferred_call_handle(&self, handle: DeferredCallHandle) {
        self.deferred_call_handle.replace(handle);
    }

    pub fn set_device_client(&self, c: &'a dyn ENC28J60Client) {
	self.device_client.replace(c);
    }


    /// Peform a new ENC28J60 SPI transaction, automatically dealing
    /// with multiple underlying transactions or large buffers
    fn spi_operation(&self, op: ENC28J60SpiOperation) -> Result<(), ErrorCode> {
	use ENC28J60SpiOperation as Op;

	let op =
	    match op {
		Op::ReadWideControlRegister(reg) => {
		    // The next states expect the lower half of a wide register
		    Op::ReadWideControlRegister(
			reg.wide_reg().expect("enc28j60 no wide reg").0
		    )
		},
		Op::WriteWideControlRegister(reg, val) => {
		    // The next states expect the lower half of a wide register
		    Op::WriteWideControlRegister(
			reg.wide_reg().expect("enc28j60 no wide reg").0, val
		    )
		},
		a => a,
	    };

	if self.current_spi_operation.is_some() {
	    Err(ErrorCode::BUSY)
	} else {
	    let init_state = op.to_init_state();
	    self.current_spi_operation.replace((op, init_state));
	    self.spi_state_advance(None);
	    Ok(())
	}
    }

    fn spi_state_advance(&self, transmit_res: Option<(&'static mut [u8], Option<&'static mut [u8]>, usize)>) {
	use ENC28J60SpiState as State;
	use ENC28J60Bank as Bank;
	use ENC28J60SpiOperation as Op;
	use ENC28J60SpiOperationResult as OpRes;

	let select_bank = |original_op: Op, bank: Option<ENC28J60Bank>| {
	    // This is a bit tricky. We want to set some contents in
	    // ECON1 while not overwriting others. The most efficient
	    // solution is to use a BIT FIELD SET, followed by a BIT
	    // FIELD CLEAR. However, that's not always required, as
	    // bank 0 can use only a BIT FIELD CLEAR, while bank 3 can
	    // use only a BIT FIELD SET.

	    let bank = bank.unwrap_or(Bank::Bank0);

   	    let (_, econ1_addr) = ENC28J60Register::ECON1.to_bank_address();
	    let txbuf = self.txbuffer.take().expect(BUFFER_ERROR);

	    match bank {
		Bank::Bank0 => {
		    // Use only a BIT FIELD CLEAR on the first two bits
		    txbuf[0] = OPCODE_BITFIELDCLEAR | (econ1_addr & PARAMETER_BITMASK);
		    txbuf[1] = 0x03;

		    // Advance to the next state (select bank finished)
		    self.current_spi_operation.replace((original_op, State::BankSelected(bank)));

		    self.spi.read_write_bytes(txbuf, None, 2);
		},
		Bank::Bank3 => {
		    // Use only a BIT FIELD SET on the first two bits
		    txbuf[0] = OPCODE_BITFIELDSET | (econ1_addr & PARAMETER_BITMASK);
		    txbuf[1] = 0x03;

		    // Advance to the next state (select bank finished)
		    self.current_spi_operation.replace((original_op, State::BankSelected(bank)));

		    self.spi.read_write_bytes(txbuf, None, 2);
		},
		_ => {
		    // Use a BIT FIELD CLEAR on the first two bits
		    txbuf[0] = OPCODE_BITFIELDCLEAR | (econ1_addr & PARAMETER_BITMASK);
		    txbuf[1] = 0x03;

		    // Advance to the next state (select bank finished)
		    self.current_spi_operation.replace((original_op, State::BankSelectTwoStage(bank)));

		    self.spi.read_write_bytes(txbuf, None, 2);
		}
	    }
	};

	// Read control register code to be run when the correct bank has been selected
	let cb_read_control_register = |original_op: ENC28J60SpiOperation, reg: ENC28J60Register| {
	    let (_, addr) = reg.to_bank_address();

	    let txbuf = self.txbuffer.take().expect(BUFFER_ERROR);
	    txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
	    txbuf[1] = 0x00;

	    let rxbuf = self.rxbuffer.take().expect(BUFFER_ERROR);

	    // Advance to next state
	    self.current_spi_operation.replace((original_op, State::ControlRegisterRead(reg, None)));

	    self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
	};

	// Write control register code to be run when the correct bank has been selected
	let cb_write_control_register = |original_op: ENC28J60SpiOperation, reg: ENC28J60Register, val: Result<u8, u16>| {
	    let (_, addr) = reg.to_bank_address();

	    let txbuf = self.txbuffer.take().expect(BUFFER_ERROR);
	    txbuf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
	    txbuf[1] = val.map_or_else(
		|high_low| high_low.to_be_bytes()[1],
		|byte| byte,
	    );

	    // Advance to next state
	    self.current_spi_operation.replace(
		(
		    original_op,
		    if let Err(_) = val {
			// Wide register write
			State::WriteHighControlRegister(reg, val)
		    } else {
			State::ControlRegisterWritten(reg, val)
		    }
		)
	    );

	    self.spi.read_write_bytes(txbuf, None, 2);
	};

	// Read or write PHY control register, step 1: Write the destination addr
	//
	// This must be run when the correct bank is set
	let cb_write_phy_addr = |original_op: ENC28J60SpiOperation, reg: ENC28J60PHYRegister, write: Option<u16>| {
	    let (_, addr) = ENC28J60Register::MIREGADR.to_bank_address();

	    let txbuf = self.txbuffer.take().expect(BUFFER_ERROR);
	    txbuf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
	    txbuf[1] = reg as u8;

	    // Advance to next state
	    self.current_spi_operation.replace(
		(
		    original_op,
		    if let Some(val) = write {
			State::WritePHYRegWriteLow(reg, val)
		    } else {
			State::ReadPHYRegExec(reg)
		    }
		)
	    );

	    self.spi.read_write_bytes(txbuf, None, 2);
	};

	// BIT FIELD SET code to be run when the correct bank has been selected
	let cb_bit_field_set = |original_op: ENC28J60SpiOperation, reg: ENC28J60Register, bf: u8| {
	    let (_, addr) = reg.to_bank_address();

	    let txbuf = self.txbuffer.take().expect(BUFFER_ERROR);
	    txbuf[0] = OPCODE_BITFIELDSET | (addr & PARAMETER_BITMASK);
	    txbuf[1] = bf;

	    // Advance to next state
	    self.current_spi_operation.replace((original_op, State::BitFieldSetDone(reg, bf)));

	    self.spi.read_write_bytes(txbuf, None, 2);
	};

	// BIT FIELD CLEAR code to be run when the correct bank has been selected
	let cb_bit_field_clear = |original_op: ENC28J60SpiOperation, reg: ENC28J60Register, bf: u8| {
	    let (_, addr) = reg.to_bank_address();

	    let txbuf = self.txbuffer.take().expect(BUFFER_ERROR);
	    txbuf[0] = OPCODE_BITFIELDCLEAR | (addr & PARAMETER_BITMASK);
	    txbuf[1] = bf;

	    // Advance to next state
	    self.current_spi_operation.replace((original_op, State::BitFieldClearDone(reg, bf)));

	    self.spi.read_write_bytes(txbuf, None, 2);
	};

	let (op, current_state) =
	    self.current_spi_operation.take().expect("enc28j60 spiopadv no op");

	match current_state {
	    State::SystemReset => {
		// Perfom a soft reset

		let buf = self.txbuffer.take().expect(BUFFER_ERROR);
		buf[0] = OPCODE_RESET | CONSTP_RESET;

		// Advance to next state
		self.current_spi_operation.replace((op, State::ResetDone));

		self.spi.read_write_bytes(buf, None, 1);
	    },
	    State::ResetDone => {
		// Soft reset has been performed, normally we'd have
		// to wait for at least 50us prior to writing any PHY
		// registers

		// This must be the result of a SPI transmission
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		// Return the txbuffer, rx has not been taken
		self.txbuffer.replace(buf);

		let res = Ok(OpRes::ResetDone);
		self.spi_operation_finished(res);
	    },
	    State::BankSelectTwoStage(bank) => {
		// A BIT FIELD CLEAR was already executed, now to a
		// BIT FIELD SET

		// We can reuse the txbuffer from the previous transfer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);
		let (_, econ1_addr) = ENC28J60Register::ECON1.to_bank_address();

		buf[0] = OPCODE_BITFIELDSET | (econ1_addr & PARAMETER_BITMASK);
		buf[1] = (bank as u8) & 0x03;

		// Advance to next state
		self.current_spi_operation.replace((op, State::BankSelected(bank)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::BankSelected(bank) => {
		// A bank has been selected. Continue the previous
		// operation
		self.current_bank.set(Some(bank));

		// Put back the txbuffer we used for selecting the bank
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);
		self.txbuffer.replace(buf);

		// Continue with whatever we were doing
		match op {
		    Op::ReadControlRegister(reg) => {
			cb_read_control_register(op, reg);
		    },
		    Op::WriteWideControlRegister(reg, val) => {
			cb_write_control_register(op, reg, Err(val));
		    },
		    Op::WriteControlRegister(reg, val) => {
			cb_write_control_register(op, reg, Ok(val));
		    },
		    Op::ReadPHYRegister(reg) => {
			cb_write_phy_addr(op, reg, None);
		    },
		    Op::WritePHYRegister(reg, val) => {
			cb_write_phy_addr(op, reg, Some(val));
		    },
		    Op::BitFieldSet(reg, bf) => {
			cb_bit_field_set(op, reg, bf);
		    },
		    Op::BitFieldClear(reg, bf) => {
			cb_bit_field_clear(op, reg, bf);
		    },
		    _ => panic!("enc28j60 unknown banksel cont op"),
		}
	    },
	    State::BitFieldSet(reg, bf) => {
		// Execute a bit field set

		// First check if the bank is set correctly
		if reg.check_bank(self.current_bank.get()) {
		    // Bank is correct, read immediately
		    cb_bit_field_set(op, reg, bf);
		} else {
		    let (bank, _) = reg.to_bank_address();
		    select_bank(op, bank);
		}
	    },
	    State::BitFieldSetDone(reg, bf) => {
		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		self.txbuffer.replace(txbuf);

		self.spi_operation_finished(Ok(OpRes::BitFieldSet(reg, bf)));
	    },
	    State::BitFieldClear(reg, bf) => {
		// Execute a bit field clear

		// First check if the bank is set correctly
		if reg.check_bank(self.current_bank.get()) {
		    // Bank is correct, read immediately
		    cb_bit_field_clear(op, reg, bf);
		} else {
		    let (bank, _) = reg.to_bank_address();
		    select_bank(op, bank);
		}
	    },
	    State::BitFieldClearDone(reg, bf) => {
		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		self.txbuffer.replace(txbuf);

		self.spi_operation_finished(Ok(OpRes::BitFieldCleared(reg, bf)));
	    },
	    State::ReadControlRegister(reg) => {
		// Read a control register

		// First check if the bank is set correctly
		if reg.check_bank(self.current_bank.get()) {
		    // Bank is correct, read immediately
		    cb_read_control_register(op, reg);
		} else {
		    let (bank, _) = reg.to_bank_address();
		    select_bank(op, bank);
		}
	    },
	    State::ReadHighControlRegister(reg) => {
		// This must be the result of a SPI transmission with read buffer
		let (txbuf, rxbuf, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = rxbuf.expect(BUFFER_ERROR);

		let low_byte = rxbuf[1];

		let (_, addr) = reg.to_bank_address();

		txbuf[0] = OPCODE_READCONTROLREG | ((addr + 1) & PARAMETER_BITMASK);
		txbuf[1] = 0x00;

		// Advance to next state
		self.current_spi_operation.replace((op, State::ControlRegisterRead(reg, Some(low_byte))));

		self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
	    },
	    State::ControlRegisterRead(reg, low_byte) => {
		// This must be the result of a SPI transmission with read buffer
		let (txbuf, rxbuf, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = rxbuf.expect(BUFFER_ERROR);

		let val = rxbuf[1];

		self.txbuffer.replace(txbuf);
		self.rxbuffer.replace(rxbuf);

		if let Some(low) = low_byte {
		    let wide_val = u16::from_be_bytes([val, low]);
		    let res = Ok(OpRes::ReadWideControlRegister(reg, wide_val));
		    self.spi_operation_finished(res);
		} else {
		    let res = Ok(ENC28J60SpiOperationResult::ReadControlRegister(reg, val));
		    self.spi_operation_finished(res);
		}
	    },
	    State::WriteControlRegister(reg, val) => {
		// Write a control register

		// First check if the bank is set correctly
		if reg.check_bank(self.current_bank.get()) {
		    // Bank is correct, write immediately
		    cb_write_control_register(op, reg, val);
		} else {
		    let (bank, _) = reg.to_bank_address();
		    select_bank(op, bank);
		}
	    },
	    State::WriteHighControlRegister(reg, val) => {
		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		let (_, addr) = reg.to_bank_address();

		txbuf[0] = OPCODE_WRITECONTROLREG | ((addr + 1) & PARAMETER_BITMASK);
		txbuf[1] = val.err().expect("enc28j60 writehigh u8").to_be_bytes()[0];

		// Advance to next state
		self.current_spi_operation.replace((op, State::ControlRegisterWritten(reg, val)));

		self.spi.read_write_bytes(txbuf, None, 2);
	    },
	    State::ControlRegisterWritten(reg, val) => {
		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		self.txbuffer.replace(txbuf);

		let res = match val {
		    Ok(byte) => Ok(OpRes::WroteControlRegister(reg, byte)),
		    Err(high_low) => Ok(OpRes::WroteWideControlRegister(reg, high_low)),
		};
		self.spi_operation_finished(res);
	    },
	    State::ReadPHYRegAddr(reg) => {
		// Read a PHY register, step 1: set the address to
		// read

		let control_reg = ENC28J60Register::MIREGADR;

		// In this step we don't know whether we are on the
		// correct bank, so set it
		if control_reg.check_bank(self.current_bank.get()) {
		    // Bank is correct, write the address immediately
		    cb_write_phy_addr(op, reg, None);
		} else {
		    let (bank, _) = control_reg.to_bank_address();
		    select_bank(op, bank);
		}
	    },
	    State::ReadPHYRegExec(reg) => {
		// Read a PHY register, step 2: set the MICMD.MIIRD
		// bit to start the PHY read operation
		// This assumes that bank 2 has already been selected

		// We can't use BIT FIELD SET here, but the register
		// is safe to be overwritten (data sheet 4.2.5)

		let (_, addr) = ENC28J60Register::MICMD.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = 0x01; // Set the MICMD.MIIRD bit

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadPHYRegWait(reg)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::ReadPHYRegWait(reg) => {
		// Read a PHY register, step 3: wait for MISTAT.BUSY
		// to be cleared

		let (_, addr) = ENC28J60Register::MISTAT.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = self.rxbuffer.take().expect(BUFFER_ERROR);

		txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
		txbuf[1] = 0x00;

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadPHYRegWaitClear(reg)));

		self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
	    },
	    State::ReadPHYRegWaitClear(reg) => {
		// Read a PHY register, step 4: wait for MISTAT.BUSY
		// (depending on last operations result) or clear the
		// MICMD.MIIRD bit

		// This must be the result of a SPI transmission with read buffer
		let (txbuf, rxbuf, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = rxbuf.expect(BUFFER_ERROR);

		let was_busy: bool = (rxbuf[2] & 0x01) == 0x01;

		if was_busy {
		    // Operation was busy, try again now
		    let (_, addr) = ENC28J60Register::MISTAT.to_bank_address();

		    txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
		    txbuf[1] = 0x00;

		    // Loop back to this state
		    self.current_spi_operation.replace((op, State::ReadPHYRegWaitClear(reg)));

		    self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
		} else {
		    // Operation has finished, clear the MICMD.MIIRD bit

		    // Put back the read buffer first
		    self.rxbuffer.replace(rxbuf);

		    let (_, addr) = ENC28J60Register::MICMD.to_bank_address();

		    txbuf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		    txbuf[1] = 0x00; // Disable both MICMD.MIISCAN and MICMD.MIIRD

		    // Advance to next state
		    self.current_spi_operation.replace((op, State::ReadPHYRegReadLow(reg)));

		    self.spi.read_write_bytes(txbuf, None, 2);
		}
	    },
	    State::ReadPHYRegReadLow(reg) => {
		// Read a PHY register, step 5: read the PHY register low byte

		let (_, addr) = ENC28J60Register::MIRDL.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = self.rxbuffer.take().expect(BUFFER_ERROR);

		txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
		txbuf[1] = 0x00;

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadPHYRegReadHigh(reg)));

		self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
	    },
	    State::ReadPHYRegReadHigh(reg) => {
		// Read a PHY register, step 6: read the PHY register high byte

		let (_, addr) = ENC28J60Register::MIRDH.to_bank_address();

		// This must be the result of a SPI transmission with read buffer
		let (txbuf, rxbuf, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = rxbuf.expect(BUFFER_ERROR);

		// First, recover the low byte from the read buffer
		let low_byte = rxbuf[1];

		txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
		txbuf[1] = 0x00;

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadPHYRegDone(reg, low_byte)));

		self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
	    },
	    State::ReadPHYRegDone(reg, low_byte) => {
		// Read a PHY register, step 7: recover the high byte from the read buffer

		// This must be the result of a SPI transmission with read buffer
		let (txbuf, rxbuf, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = rxbuf.expect(BUFFER_ERROR);

		let high_byte = rxbuf[1];
		let val: u16 = u16::from_be_bytes([high_byte, low_byte]);

		// Return both buffers
		self.txbuffer.replace(txbuf);
		self.rxbuffer.replace(rxbuf);

		let res = Ok(ENC28J60SpiOperationResult::ReadPHYRegister(reg, val));
		self.spi_operation_finished(res);
	    },
    	    State::WritePHYRegAddr(reg, val) => {
		// Write a PHY register, step 1: set the address to
		// write to

		let control_reg = ENC28J60Register::MIREGADR;

		// In this step we don't know whether we are on the
		// correct bank, so set it
		if control_reg.check_bank(self.current_bank.get()) {
		    // Bank is correct, write the address immediately
		    cb_write_phy_addr(op, reg, Some(val));
		} else {
		    let (bank, _) = control_reg.to_bank_address();
		    select_bank(op, bank);
		}
	    },
	    State::WritePHYRegWriteLow(reg, val) => {
		// Write a PHY register, step 2: write the low byte

		let (_, addr) = ENC28J60Register::MIWRL.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = val.to_be_bytes()[1];

		// Advance to next state
		self.current_spi_operation.replace((op, State::WritePHYRegWriteHigh(reg, val)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::WritePHYRegWriteHigh(reg, val) => {
		// Write a PHY register, step 2: write the high byte

		let (_, addr) = ENC28J60Register::MIWRH.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = val.to_be_bytes()[0];

		// Advance to next state
		self.current_spi_operation.replace((op, State::WritePHYRegWait(reg, val)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::WritePHYRegWait(reg, val) => {
		// Write a PHY register, step 3: Read the BUSY bit

		let (_, addr) = ENC28J60Register::MISTAT.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = self.rxbuffer.take().expect(BUFFER_ERROR);

		txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
		txbuf[1] = 0x00;

		// Advance to next state
		self.current_spi_operation.replace((op, State::WritePHYRegWaitDone(reg, val)));

		self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
	    },
	    State::WritePHYRegWaitDone(reg, val) => {
		// Read a PHY register, step 4: wait for MISTAT.BUSY
		// If unset, return to the caller (callback)

		// This must be the result of a SPI transmission with read buffer
		let (txbuf, rxbuf, _) = transmit_res.expect(BUFFER_ERROR);
		let rxbuf = rxbuf.expect(BUFFER_ERROR);

		let was_busy: bool = (rxbuf[2] & 0x01) == 0x01;

		if was_busy {
		    // Operation was busy, try again now
		    let (_, addr) = ENC28J60Register::MISTAT.to_bank_address();

		    txbuf[0] = OPCODE_READCONTROLREG | (addr & PARAMETER_BITMASK);
		    txbuf[1] = 0x00;

		    // Loop back to this state
		    self.current_spi_operation.replace((op, State::WritePHYRegWaitDone(reg, val)));

		    self.spi.read_write_bytes(txbuf, Some(rxbuf), 2);
		} else {
		    // Operation has finished return to the caller

		    // Put back both buffers first
		    self.txbuffer.replace(txbuf);
		    self.rxbuffer.replace(rxbuf);

		    let res = Ok(ENC28J60SpiOperationResult::WritePHYRegister(reg, val));
		    self.spi_operation_finished(res);
		}
	    },
	    State::WriteBufferMemorySetAutoinc(wbmaddr, offset, len) => {
		// Set the AUTOINC bit in ECON2 for WBM to work
		let buf = self.txbuffer.take().expect(BUFFER_ERROR);

		let (_, addr) = ENC28J60Register::ECON2.to_bank_address();
		buf[0] = OPCODE_BITFIELDSET | (addr & PARAMETER_BITMASK);
		buf[1] = 1 << 7; // ECON2.AUTOINC

		// Advance to next state
		self.current_spi_operation.replace((op, State::WriteBufferMemorySetBank(wbmaddr, offset, len)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::WriteBufferMemorySetBank(wbmaddr, offset, len) => {
		// TODO: This should be done conditionally
		// Set bank 0
		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		let (_, econ1_addr) = ENC28J60Register::ECON1.to_bank_address();

		buf[0] = OPCODE_BITFIELDCLEAR | (econ1_addr & PARAMETER_BITMASK);
		buf[1] = 0x03;

		// Advance to next state
		self.current_spi_operation.replace((op, State::WriteBufferMemoryStartAddrLow(wbmaddr, offset, len)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::WriteBufferMemoryStartAddrLow(wbmaddr, offset, len) => {
		// Set the WBM start address
		let (_, addr) = ENC28J60Register::EWRPTL.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = wbmaddr.to_be_bytes()[1];

		// Advance to next state
		self.current_spi_operation.replace((op, State::WriteBufferMemoryStartAddrHigh(wbmaddr, offset, len)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::WriteBufferMemoryStartAddrHigh(wbmaddr, offset, len) => {
		// Set the WBM start address
		let (_, addr) = ENC28J60Register::EWRPTH.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = wbmaddr.to_be_bytes()[0];

		// Advance to next state
		self.current_spi_operation.replace((op, State::WriteBufferMemoryWrite(wbmaddr, offset, len, 0)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::WriteBufferMemoryWrite(wbmaddr, offset, len, progress) => {
		// This must be the result of a SPI transmission without read buffer
		let (txbuf, _, _) = transmit_res.expect(BUFFER_ERROR);

		let buffer_elements =
		    self.buffer_memory_buf.map(|wbmbuf| {
			wbmbuf.iter()
			    .skip(offset + progress)
			    .take(len - progress)
			    .zip(txbuf[1..].iter_mut())
			    .fold(0, |i, (src, dst)| {
				*dst = *src;
				i + 1
			    })
		    }).expect(BUFFER_ERROR);

		txbuf[0] = OPCODE_WRITEBUFMEM | CONSTP_WRITEBUFMEM;

		let progress = progress + buffer_elements;

		if progress > len {
		    panic!("Wrote more than requested");
		} else if progress == len {
		    // Advance to next state
		    self.current_spi_operation.replace((op, State::BufferMemoryWritten(wbmaddr, offset, len)));
		} else {
		    // Advance to next state
		    self.current_spi_operation.replace((op, State::WriteBufferMemoryWrite(wbmaddr, offset, len, progress)));
		}

		self.spi.read_write_bytes(txbuf, None, 1 + buffer_elements);
	    },
	    State::BufferMemoryWritten(wbmaddr, offset, len) => {
		// Operation has finished, return to the caller

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);
		self.txbuffer.replace(buf);

		let res = Ok(ENC28J60SpiOperationResult::WroteBufferMemory(wbmaddr, offset, len));
		self.spi_operation_finished(res);
	    },
	    State::ReadBufferMemorySetBank(rbmaddr, len) => {
		// TODO: There is a chance the correct bank is already selected
		// Set bank 0
		// This can not be the result of a SPI transaction
		let buf = self.txbuffer.take().expect(BUFFER_ERROR);

		let (_, econ1_addr) = ENC28J60Register::ECON1.to_bank_address();

		buf[0] = OPCODE_BITFIELDCLEAR | (econ1_addr & PARAMETER_BITMASK);
		buf[1] = 0x03;

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadBufferMemoryStartAddrLow(rbmaddr, len)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::ReadBufferMemoryStartAddrLow(rbmaddr, len) => {
		// Set the RBM start address
		let (_, addr) = ENC28J60Register::ERDPTL.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = rbmaddr.to_be_bytes()[1];

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadBufferMemoryStartAddrHigh(rbmaddr, len)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::ReadBufferMemoryStartAddrHigh(rbmaddr, len) => {
		// Set the RBM start address
		let (_, addr) = ENC28J60Register::ERDPTH.to_bank_address();

		// This must be the result of a SPI transmission without read buffer
		let (buf, _, _) = transmit_res.expect(BUFFER_ERROR);

		buf[0] = OPCODE_WRITECONTROLREG | (addr & PARAMETER_BITMASK);
		buf[1] = rbmaddr.to_be_bytes()[0];

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadBufferMemorySetAutoinc(Some(rbmaddr), len)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::ReadBufferMemorySetAutoinc(rbmaddr, len) => {
		// Set the AUTOINC bit in ECON2 for RBM to work

		// This can either be called from the ReadBufferMemoryStartAddrHigh or directly
		let buf =
		    if let Some((txbuf, _, _)) = transmit_res {
			// Only the TX buffer was taken
			txbuf
		    } else {
			self.txbuffer.take().expect(BUFFER_ERROR)
		    };

		let (_, addr) = ENC28J60Register::ECON2.to_bank_address();
		buf[0] = OPCODE_BITFIELDSET | (addr & PARAMETER_BITMASK);
		buf[1] = 1 << 7; // ECON2.AUTOINC

		// Advance to next state
		self.current_spi_operation.replace((op, State::ReadBufferMemoryRead(rbmaddr, len, 0, 0)));

		self.spi.read_write_bytes(buf, None, 2);
	    },
	    State::ReadBufferMemoryRead(rbmaddr, len, mut progress, last_read) => {
		let (txbuf, mayberx, spilen) = transmit_res.expect(BUFFER_ERROR);

		// Make sure we have access to the read buffer
		let rxbuf =
		    if let Some(rxbuf) = mayberx {
			rxbuf
		    } else {
			self.rxbuffer.take().expect(BUFFER_ERROR)
		    };

		if last_read != 0 {
		    // Copy last_read bytes from the rxbuf to the target, starting at progress
		    self.buffer_memory_buf.map(|target| {
			target.iter_mut()
			    .zip(rxbuf.iter().skip(1))
			    .take(last_read)
			    .for_each(|(dst, src)| *dst = *src);
		    }).expect(BUFFER_ERROR);

		    // Advance progress
		    progress += last_read;
		}

		// Prepare the next read
		txbuf[0] = OPCODE_READBUFMEM | CONSTP_READBUFMEM;

		if progress > len {
		    panic!("rbm read to much");
		} else if progress == len {
		    // We are done.
		    self.rxbuffer.replace(rxbuf);
		    self.txbuffer.replace(txbuf);
		    let res = Ok(ENC28J60SpiOperationResult::ReadBufferMemory(rbmaddr, len));
		    self.spi_operation_finished(res);
		    // Don't replace the current_spi_operation
		} else {
		    let to_read = len - progress;
		    self.current_spi_operation.replace((op, State::ReadBufferMemoryRead(rbmaddr, len, progress, to_read)));
		    // Potentially try to read more than the rxbuf allows
		    self.spi.read_write_bytes(txbuf, Some(rxbuf), 1 + to_read);
		}
	    },
	}
    }

    fn spi_operation_finished(&self, result: Result<ENC28J60SpiOperationResult, ErrorCode>) {
	self.device_state_advance(Some(result))
	    .expect("enc28j60 device state error")
    }

    fn device_state_advance(&self, spi_res: Option<Result<ENC28J60SpiOperationResult, ErrorCode>>) -> Result<(), ErrorCode> {
	use ENC28J60State as State;
	use ENC28J60SpiOperation as Op;
	use ENC28J60SpiOperationResult as OpRes;
	use ENC28J60Register as Reg;

	let current_state = self.state.get();

	match current_state {
	    State::Uninitialized
		| State::Initialized => panic!("enc28j60 advance while idle"),
	    State::ChipReset => {
		self.state.set(State::DetermineHWRev);
		self.spi_operation(Op::SystemReset)
	    },
	    State::DetermineHWRev => {
		self.state.set(State::SetRXBufferStart);
		self.spi_operation(Op::ReadControlRegister(Reg::EREVID))
	    },
	    State::SetRXBufferStart => {
		// TODO: This is a hack, this needs to be in some post processing fn
		// Store the HW revision determined in the last step
		let opres = spi_res.expect("enc28j60 non spi adv")?;
		let hw_rev =
		    if let OpRes::ReadControlRegister(Reg::EREVID, val) = opres {
			val & 0x1F
		    } else {
			panic!("enc28j60 wrong opres");
		    };
		self.hw_rev.replace(hw_rev as usize);

		self.state.set(State::SetRXBufferEnd);
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::ERXSTH, ENC28J60_RXBUF_START))
	    },
	    State::SetRXBufferEnd => {
		self.state.set(State::SetInitRXReadPointer);
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::ERXNDL, ENC28J60_RXBUF_END))
	    },
	    State::SetInitRXReadPointer => {
		// For tracking purposes, as per data sheet 6.1
		self.state.set(State::ConfigureMac0);
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::ERXRDPTL, ENC28J60_RXBUF_START))
	    },
	    State::ConfigureMac0 => {
		// Configure MACON3
		let macon3: u8 =
		// - Pad all short frames to 64 bytes and append a valid CRC
		    0b111 << 5 |
		// - Respect padding setting above
		    0b0 << 4 |
		// - Frames don't contain proprietary 4 byte header
		    0b0 << 3 |
		// - Don't allow hugeframes
		    0b0 << 2 |
		// - Transmitted and received frames will have their length field validated
		    0b1 << 1 |
		// - Operate in full duplex mode
		0b1;

		let macon3: u8 = 0b00010101;

		self.state.set(State::ConfigureMaxFrameLength);
		self.spi_operation(Op::WriteControlRegister(ENC28J60Register::MACON3, macon3))
	    },
	    State::ConfigureMaxFrameLength => {
		// Configure max Ethernet frame length
		self.state.set(State::ConfigureBtBInterPacketGap);
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::MAMXFLL, ETHERNET_PKT_MAXLEN))
	    },
	    State::ConfigureBtBInterPacketGap => {
		// Recommendation as per datasheet 6.5.5
		self.state.set(State::ConfigureNBtBInterPacketGap);
		self.spi_operation(Op::WriteControlRegister(ENC28J60Register::MABBIPG, 0x15))
	    },
	    State::ConfigureNBtBInterPacketGap => {
		// Recommendation as per datasheet 6.5.7 and 6.5.8
		self.state.set(State::SetMacAddress(0));
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::MAIPGH, 0x0012))
	    },
	    State::SetMacAddress(byte) => {
		// Set the MAC Address
		let mac_byte_reg =
		    match byte {
			0 => Reg::MAADR1,
			1 => Reg::MAADR2,
			2 => Reg::MAADR3,
			3 => Reg::MAADR4,
			4 => Reg::MAADR5,
			5 => Reg::MAADR6,
			_ => panic!("enc28j60 mac loop"),
		    };

		// Advance to next state
		self.state.set(
		    if byte < 5 {
			State::SetMacAddress(byte + 1)
		    } else {
			State::ConfigureMac2
		    }
		);

		self.spi_operation(Op::WriteControlRegister(mac_byte_reg, self.mac_addr[byte]))
	    },
	    State::ConfigureMac2 => {
		// Configure MACON1
		let macon1: u8 =
		// - Allow transmitting pause frames
		    0b1 << 3 |
		// - Respect received pause frames
		    0b1 << 2 |
		// - Discard control frames after processing by the MAC
		    0b0 << 1 |
		// - MAC enable RX
		    0b1;

		self.state.set(State::SetPHYFullDuplex);
		self.spi_operation(Op::WriteControlRegister(ENC28J60Register::MACON1, macon1))
	    },
	    State::SetPHYFullDuplex => {
		self.state.set(State::EnableInterrupts);
		self.spi_operation(Op::WritePHYRegister(ENC28J60PHYRegister::PHCON1, 0x0100))
	    },
	    State::EnableInterrupts => {
		self.state.set(State::ConfigureReceiveFilters);
		self.spi_operation(Op::BitFieldSet(
		    ENC28J60Register::EIE,
		    1 << 7 // Drive interrupt pin
			| 1 << 6 // Packet pending interrupt
		))
	    },
	    State::ConfigureReceiveFilters => {
		self.state.set(State::EnableRX);
		self.spi_operation(Op::WriteControlRegister(
		    ENC28J60Register::ERXFCON,
		    1 << 7 // Unicast enable
			| 0 << 6 // OR-mode (only one filter has to match)
			| 1 << 5 // Reject invalid CRC
			| 0 << 4 // Disable pattern matching
			| 0 << 3 // Disable Magic Packet
			| 0 << 2 // Disable hash table filter criteria
			| 1 << 1 // Enable Multicast reception
			| 1 << 0 // Enable broadcast reception
		))
	    },
	    State::EnableRX => {
		self.state.set(State::InitializeDone);
		// ECON1.RXEN
		self.spi_operation(Op::BitFieldSet(Reg::ECON1, 1 << 2))
	    },
	    State::InitializeDone => {
		// Set to idle
		if self.interrupt_pending.get() {
		    self.state.set(State::HandleInterrupt);
		    self.device_state_advance(None);
		} else {
		    self.state.set(State::Initialized);
		}

		// Inform the device client
		debug!("Device initialized!, {}", self.hw_rev.extract().expect("no hwrev") as usize);
		self.device_client.map(|dc| dc.device_initialized(self.hw_rev.extract().expect("no hwrev") as usize));

		Ok(())
	    },
	    State::TransmitSetStartPointer(len, has_crc) => {
		self.state.set(State::TransmitWriteControlByte(len, has_crc));
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::ETXSTL, ENC28J60_TXBUF_START))
	    },
	    State::TransmitWriteControlByte(len, has_crc) => {
		let buffer = self.bufmembuffer.take().expect(BUFFER_ERROR);
		buffer[0] = // per packet control byte
		// don't add padding
		    (1 << 2) |
		    // generate CRC if required
		    (if has_crc { 0 } else { 1 }) << 1 |
		    // override active
		1;
		self.buffer_memory_buf.replace(buffer);

		self.state.set(State::TransmitWritePacket(len));
		self.spi_operation(Op::WriteBufferMemory(ENC28J60_TXBUF_START, 0, 1))
	    },
	    State::TransmitWritePacket(len) => {
		self.bufmembuffer.replace(self.buffer_memory_buf.take().expect(BUFFER_ERROR));

		let packet = self
		    .current_transmission
		    .replace(Err(()))
		    .expect(BUFFER_ERROR)
		    .expect(BUFFER_ERROR);
		self.buffer_memory_buf.replace(packet);

		// TODO: Why not +1 here?
		self.state.set(State::TransmitSetEndPointer((ENC28J60_TXBUF_START as usize + len) as u16));

		self.spi_operation(Op::WriteBufferMemory(ENC28J60_TXBUF_START + 1, 0, len))
	    },
	    State::TransmitSetEndPointer(pos) => {
		self.current_transmission.replace(Ok(self.buffer_memory_buf.take().expect(BUFFER_ERROR)));

		self.state.set(State::TransmitClearInterrupts);
		self.spi_operation(Op::WriteWideControlRegister(ENC28J60Register::ETXNDL, pos))
	    },
	    State::TransmitClearInterrupts => {
		self.state.set(State::TransmitEnableInterrupts);
		self.spi_operation(Op::BitFieldClear(ENC28J60Register::EIR, 1 << 3))
	    },
	    State::TransmitEnableInterrupts => {
		self.state.set(State::TransmitStart);
		self.spi_operation(Op::BitFieldSet(ENC28J60Register::EIE, 1 << 3 | 1 << 1))
	    },
	    State::TransmitStart => {
		self.state.set(State::TransmitStarted);
		self.spi_operation(Op::BitFieldSet(ENC28J60Register::ECON1, 1 << 3))
	    },
	    State::TransmitStarted => {
		if self.interrupt_pending.get() {
		    self.state.set(State::HandleInterrupt);
		    self.device_state_advance(None);
		} else {
		    self.state.set(State::Initialized);
		}
		Ok(())
	    },
	    State::ReceivePacketGetCount => {
		self.state.set(State::ReceivePacketReadStatusVector);
		self.spi_operation(Op::ReadControlRegister(Reg::EPKTCNT))
	    },
	    State::ReceivePacketReadStatusVector => {
		// Post process the packet count
		let opres = spi_res.expect("enc28j60 non spi adv")?;
		let pktcnt =
		    if let OpRes::ReadControlRegister(Reg::EPKTCNT, val) = opres {
			val
		    } else {
			panic!("enc28j60 wrong opres");
		    };

		debug!("pktcnt: {}", pktcnt);
		if pktcnt == 0x00 {
		    self.state.set(State::ReceivePacketDone(Err(ENC28J60ReceivePacketError::NoPacketQueued)));
		    // Calling this might break things?
		    self.device_state_advance(None);
		    Ok(())
		} else {
		    // There's at least one packet to read, proceed reading its status vector
		    let buffer = self.bufmembuffer.take().expect(BUFFER_ERROR);
		    self.buffer_memory_buf.replace(buffer);
		    self.state.set(State::ReceivePacketReadPkt(pktcnt as usize));
		    self.spi_operation(Op::ReadBufferMemory(Some(self.next_packet_pointer.get()), 6))
		}
	    },
	    State::ReceivePacketReadPkt(pktcnt) => {
		// Post process the status vector
		let rxbuf = self.current_reception
		    .replace(Err(()))
		    .expect(BUFFER_ERROR)
		    .expect(BUFFER_ERROR);

		let statbuf = self.buffer_memory_buf.replace(rxbuf).expect(BUFFER_ERROR);
		let statvec = ENC28J60RXStatusVector {
		    next_packet: u16::from_le_bytes([statbuf[0], statbuf[1]]),
		    frame_length: u16::from_le_bytes([statbuf[2], statbuf[3]]) as usize,
		    pending_packets: pktcnt - 1,
		};

		self.bufmembuffer.replace(statbuf);

		// Imediately advance the internal next packet reference
		self.next_packet_pointer.set(statvec.next_packet);
		debug!("npkt: {:02x}", statvec.next_packet);

		self.state.set(State::ReceivePacketAdvRXPtr(statvec));
		self.spi_operation(Op::ReadBufferMemory(None, statvec.frame_length))
	    },
	    State::ReceivePacketAdvRXPtr(statvec) => {
		self.current_reception.replace(Ok(self.buffer_memory_buf.take().expect(BUFFER_ERROR)));

		self.state.set(State::ReceivePacketDecCount(statvec));
		self.spi_operation(Op::WriteWideControlRegister(Reg::ERXRDPTL, next_packet_workaround(statvec.next_packet)))
	    },
	    State::ReceivePacketDecCount(statvec) => {
		self.state.set(State::ReceivePacketReenableInterrupt(statvec));
		// Set ECON2.PKTDEC
		self.spi_operation(Op::BitFieldSet(Reg::ECON2, 1 << 6))
	    },
	    State::ReceivePacketReenableInterrupt(statvec) => {
		self.state.set(State::ReceivePacketDone(Ok(statvec)));
		self.spi_operation(Op::BitFieldSet(Reg::EIE, 1 << 6))
	    },
	    State::ReceivePacketDone(rxres) => {
		// Packet received either successfully or with error
		// Inform the device lcient
		self.deferred_calls.map(|mut calls| {
		    calls.packet_received = Some((
			self.current_reception.take().expect(BUFFER_ERROR).expect(BUFFER_ERROR),
			rxres.map(|statvec| statvec.frame_length),
		    ));

		    // We need to check here again, as interrupts for
		    // RX are not very reliable
		    rxres.map(|statvec| {
			debug!("more pending: {}", statvec.pending_packets);
			if statvec.pending_packets > 0 {
			    calls.packet_pending = Some(())
			}
		    });
		});

		// Now that we've got the packet, don't suppress RX interrupts anymore
		self.suppress_rx_interrupt.set(false);

		//if self.interrupt_pending.get() {
		    // HandleInterrupt will call the deferred calls
		// after all interrupts have been taken care of
		//self.deferred_call_handle.map(|h| self.deferred_caller.set(*h));
		    self.state.set(State::HandleInterrupt);
		    self.device_state_advance(None);
		// } else {
		//     self.state.set(State::Initialized);
		//     self.deferred_call_handle.map(|h| self.deferred_caller.set(*h));
		//}
		Ok(())
	    },

	    State::HandleInterrupt => {
		self.state.set(State::HandleInterruptRead);
		self.spi_operation(Op::ReadControlRegister(ENC28J60Register::EIR))
	    },
	    State::HandleInterruptRead => {
		// This must have been called by HandleInterrupt
		let opres = spi_res.as_ref().expect("enc28j60 non spi adv").expect("TODO");
		let eir =
		    if let OpRes::ReadControlRegister(Reg::EIR, val) = opres {
			val
		    } else {
			panic!("enc28j60 wrong opres");
		    };

		self.state.set(State::HandleInterruptLoop(eir));
		self.device_state_advance(spi_res)
	    },
	    State::HandleInterruptLoop(mut eir) => {
		if self.suppress_rx_interrupt.get() {
		    eir &= !(1 << 6);
		}

		// Suppress receive errors
		eir &= !(1 << 0);

		if eir == 0x00 {
		    // All interrupts handeled
		    self.interrupt_pending.set(false);
		    self.state.set(State::Initialized);

		    // Now that all interrupts are handled, call
		    // deferred call clients if they were set
		    self.deferred_call_handle.map(|h| self.deferred_caller.set(*h));
		} else if (eir & (1 << 3)) != 0x00 {
		    // Transfer finished interrupt
		    // Inform the device client
		    self.deferred_calls.map(|mut calls| {
			calls.packet_sent = Some((
			    self.current_transmission.take().expect(BUFFER_ERROR).expect(BUFFER_ERROR),
			    Ok(())
			));
		    });
		    self.state.set(State::HandleInterrupt);
		    self.spi_operation(Op::BitFieldClear(Reg::EIR, 1 << 3));
		} else if (eir & (1 << 6)) != 0x00 {
		    // Receive packet pending
		    // Inform the device client
		    self.deferred_calls.map(|mut calls| {
			calls.packet_pending = Some(());
		    });
		    self.state.set(State::HandleInterrupt);

		    // The interrupt value itself can't be cleared, so
		    // rather suppress it in software
		    self.suppress_rx_interrupt.set(true);

		    // Clear the enable bit, as we have to wait for
		    // the client to fetch the packet anyways
		    self.spi_operation(Op::BitFieldClear(Reg::EIE, 1 << 6));
		} else {
		    debug!("Unknown EIR value: {:02x}", eir);
		}

		Ok(())
	    },
	}
    }

    pub fn initialize(&self) -> Result<(), ErrorCode> {
	use ENC28J60State as State;

	if self.state.get().is_busy() {
	    Err(ErrorCode::BUSY)
	} else {
	    self.state.set(State::ChipReset);
	    self.device_state_advance(None)
	}
    }

    pub fn transmit_packet(&self, packet: &'static mut [u8], len: usize, has_crc: bool) -> Result<(), (&'a mut [u8], ErrorCode)> {
	if !self.state.get().is_ready() {
	    return Err((packet, ErrorCode::BUSY));
	}

	if len > packet.len() || len > (ETHERNET_PKT_MAXLEN as usize) || 1 + len + 7 > (ENC28J60_BUFFER_LEN as usize) {
	    return Err((packet, ErrorCode::INVAL));
	}

	if self.current_transmission.is_some() {
	    return Err((packet, ErrorCode::BUSY));
	}

	self.current_transmission.replace(Ok(packet));

	self.state.set(ENC28J60State::TransmitSetStartPointer(len, has_crc));
	self.device_state_advance(None);

	Ok(())
    }

    pub fn fetch_packet(&self, packet: &'static mut [u8]) -> Result<(), (&'a mut [u8], ErrorCode)> {
	if !self.state.get().is_ready() {
	    return Err((packet, ErrorCode::BUSY));
	}

	if self.current_reception.is_some() {
	    return Err((packet, ErrorCode::BUSY));
	}

	self.current_reception.replace(Ok(packet));

	self.state.set(ENC28J60State::ReceivePacketGetCount);
	self.device_state_advance(None);

	Ok(())
    }
}

impl<'a, S: SpiMasterDevice> SpiMasterClient for ENC28J60<'a, S> {
    fn read_write_done(
        &self,
        write_buffer: &'static mut [u8],
        read_buffer: Option<&'static mut [u8]>,
        len: usize,
	status: Result<(), ErrorCode>,
    ) {
	assert!(status.is_ok());
	self.spi_state_advance(Some((write_buffer, read_buffer, len)))
    }
}

impl<'a, S: SpiMasterDevice> gpio::Client for ENC28J60<'a, S> {
    fn fired(&self) {
	// Interrupt occured

	// If we are idle, transition to the interrupt handling state
	// immediately, otherwise set the pending flag
	if self.state.get().is_busy() {
	    self.interrupt_pending.set(true);
	} else {
	    self.state.set(ENC28J60State::HandleInterrupt);
	    self.device_state_advance(None);
	}
    }
}

impl<'a, S: SpiMasterDevice> DynamicDeferredCallClient for ENC28J60<'a, S> {
    fn call(&self, _handle: DeferredCallHandle) {
	self.deferred_calls.map(|mut calls| {
	    if let Some(hwrev) = calls.device_initialized.take() {
		self.device_client.map(|dc| dc.device_initialized(hwrev));
	    }

	    if let Some((buf, res)) = calls.packet_sent.take() {
		self.device_client.map(move |dc| dc.packet_sent(buf, res));
	    }

	    if let Some(_) = calls.packet_pending.take() {
		self.device_client.map(|dc| dc.packet_pending());
	    }

	    if let Some((buf, res)) = calls.packet_received.take() {
		self.device_client.map(move |dc| dc.packet_received(buf, res));
	    }
	});
    }
}
