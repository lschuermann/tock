// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! USB_SERIAL_JTAG driver.
//!
//! The ESP32-C3 includes a `USB_SERIAL_JTAG` peripheral that implements a USB
//! device exposing a JTAG interface and a CDC-ACM virtual serial port, wired
//! directly to the chip's native USB D+/D- pins.
//!
//! This driver only implements the CDC-ACM ("serial port") half of the
//! peripheral.
//!
//! The hardware has no interrupt for a host that disappears (e.g., an unplugged
//! cable). A host that is gone or does not read the serial port never picks up
//! a flushed packet, which would stall a transmission forever. Therefore, this
//! driver follows the approach of ESP-IDF's `usb_serial_jtag` driver:
//!
//! - A host sends a start-of-frame (SOF) packet every 1 ms, which sets the
//!   `SOF` bit in `INT_RAW`. When this bit stays clear for
//!   [`DISCONNECT_TIMEOUT_MS`], the driver considers the host disconnected.
//!   `transmit_buffer` then fails with [`ErrorCode::OFF`] until the next SOF.
//! - When the host does not pick up a packet within [`TX_TIMEOUT_MS`], the
//!   transmission completes with [`ErrorCode::FAIL`]. `transmit_buffer` then
//!   fails with [`ErrorCode::OFF`] until the host picks up the packet.
//!
//! The driver measures these timeouts with an alarm that the board provides
//! through [`UsbSerialJtag::set_alarm`].

use core::cell::Cell;
use core::cmp::min;

use kernel::ErrorCode;
use kernel::deferred_call::{DeferredCall, DeferredCallClient};
use kernel::hil;
use kernel::hil::time::{Alarm, AlarmClient, ConvertTicks, Ticks};
use kernel::utilities::StaticRef;
use kernel::utilities::cells::OptionalCell;
use kernel::utilities::cells::TakeCell;
use kernel::utilities::registers::interfaces::{ReadWriteable, Readable, Writeable};
use kernel::utilities::registers::register_structs;
use kernel::utilities::registers::{ReadOnly, ReadWrite, WriteOnly, register_bitfields};

/// Maximum number of bytes the hardware will accept per IN/OUT USB packet.
const USB_SERIAL_JTAG_PACKET_SIZE: usize = 64;

/// Time without an SOF packet after which the driver considers the host
/// disconnected.
///
/// A host sends an SOF packet every 1 ms. ESP-IDF tolerates up to 3 ms.
pub const DISCONNECT_TIMEOUT_MS: u32 = 3;

/// Maximum time for the host to pick up a flushed packet.
///
/// An idle host polls for data every 1 ms. Like ESP-IDF, we allow for a busy
/// bus with a longer timeout.
pub const TX_TIMEOUT_MS: u32 = 50;

/// Number of polls of the panic writer for the host to pick up a packet.
///
/// Each poll reads a register and executes a `nop`. At 160 MHz, this takes on
/// the order of [`TX_TIMEOUT_MS`].
const PANIC_TX_TIMEOUT_POLLS: usize = 800_000;

register_structs! {
    pub UsbSerialJtagRegisters {
        (0x000 => ep1: ReadWrite<u32, EP1::Register>),
        (0x004 => ep1_conf: ReadWrite<u32, EP1_CONF::Register>),
        (0x008 => int_raw: ReadWrite<u32, INT::Register>),
        (0x00C => int_st: ReadOnly<u32, INT::Register>),
        (0x010 => int_ena: ReadWrite<u32, INT::Register>),
        (0x014 => int_clr: WriteOnly<u32, INT::Register>),
        (0x018 => conf0: ReadWrite<u32, CONF0::Register>),
        (0x01C => test: ReadWrite<u32, TEST::Register>),
        (0x020 => jfifo_st: ReadWrite<u32, JFIFO_ST::Register>),
        (0x024 => fram_num: ReadOnly<u32, FRAM_NUM::Register>),
        (0x028 => in_ep0_st: ReadOnly<u32, IN_EP_ST::Register>),
        (0x02C => in_ep1_st: ReadOnly<u32, IN_EP_ST::Register>),
        (0x030 => in_ep2_st: ReadOnly<u32, IN_EP_ST::Register>),
        (0x034 => in_ep3_st: ReadOnly<u32, IN_EP_ST::Register>),
        (0x038 => out_ep0_st: ReadOnly<u32, OUT_EP_ST::Register>),
        (0x03C => out_ep1_st: ReadOnly<u32, OUT_EP1_ST::Register>),
        (0x040 => out_ep2_st: ReadOnly<u32, OUT_EP_ST::Register>),
        (0x044 => misc_conf: ReadWrite<u32, MISC_CONF::Register>),
        (0x048 => mem_conf: ReadWrite<u32, MEM_CONF::Register>),
        (0x04C => _reserved0),
        (0x080 => date: ReadWrite<u32>),
        (0x084 => @END),
    }
}

register_bitfields![u32,
    EP1 [
        RDWR_BYTE OFFSET(0) NUMBITS(8) [],
    ],
    EP1_CONF [
        // Write-only: set to signal that a byte (or bytes) written through
        // EP1 should be flushed out as a USB packet.
        WR_DONE OFFSET(0) NUMBITS(1) [],
        // Read-only: 1 if the Tx hardware buffer is free (not full and not
        // waiting on the host to pick up a previously flushed packet).
        SERIAL_IN_EP_DATA_FREE OFFSET(1) NUMBITS(1) [],
        // Read-only: 1 if there are unread bytes in the Rx hardware buffer.
        SERIAL_OUT_EP_DATA_AVAIL OFFSET(2) NUMBITS(1) [],
    ],
    INT [
        JTAG_IN_FLUSH OFFSET(0) NUMBITS(1) [],
        SOF OFFSET(1) NUMBITS(1) [],
        SERIAL_OUT_RECV_PKT OFFSET(2) NUMBITS(1) [],
        SERIAL_IN_EMPTY OFFSET(3) NUMBITS(1) [],
        PID_ERR OFFSET(4) NUMBITS(1) [],
        CRC5_ERR OFFSET(5) NUMBITS(1) [],
        CRC16_ERR OFFSET(6) NUMBITS(1) [],
        STUFF_ERR OFFSET(7) NUMBITS(1) [],
        IN_TOKEN_REC_IN_EP1 OFFSET(8) NUMBITS(1) [],
        USB_BUS_RESET OFFSET(9) NUMBITS(1) [],
        OUT_EP1_ZERO_PAYLOAD OFFSET(10) NUMBITS(1) [],
        OUT_EP2_ZERO_PAYLOAD OFFSET(11) NUMBITS(1) [],
    ],
    CONF0 [
        // Select internal/external USB PHY. 0: internal, 1: external.
        PHY_SEL OFFSET(0) NUMBITS(1) [],
        // Enable software control of the USB D+/D- pin exchange below,
        // rather than it being fixed by the PHY.
        EXCHG_PINS_OVERRIDE OFFSET(1) NUMBITS(1) [],
        // Swap the USB D+/D- pins.
        EXCHG_PINS OFFSET(2) NUMBITS(1) [],
        // Single-ended input high threshold: 1.76V to 2V, 80mV/step.
        VREFH OFFSET(5) NUMBITS(2) [],
        // Single-ended input low threshold: 0.8V to 1.04V, 80mV/step.
        VREFL OFFSET(3) NUMBITS(2) [],
        // Enable software control of the input thresholds above, rather
        // than them being fixed by the PHY.
        VREF_OVERRIDE OFFSET(7) NUMBITS(1) [],
        // Enable software control of the D+/D- pull up/down resistors
        // below, rather than them being fixed by the PHY.
        PAD_PULL_OVERRIDE OFFSET(8) NUMBITS(1) [],
        DP_PULLUP OFFSET(9) NUMBITS(1) [],
        DP_PULLDOWN OFFSET(10) NUMBITS(1) [],
        DM_PULLUP OFFSET(11) NUMBITS(1) [],
        DM_PULLDOWN OFFSET(12) NUMBITS(1) [],
        // Pull-up strength: 0 = ~2.4kΩ, 1 = ~1.4kΩ.
        PULLUP_VALUE OFFSET(13) NUMBITS(1) [],
        // Enable the USB FSLS PHY pads.
        USB_PAD_ENABLE OFFSET(14) NUMBITS(1) [],
    ],
    TEST [
        // Enable USB pad test mode.
        TEST_ENABLE OFFSET(0) NUMBITS(1) [],
        // USB pad output-enable value while in test mode.
        TEST_USB_OE OFFSET(1) NUMBITS(1) [],
        // USB D+ drive value while in test mode.
        TEST_TX_DP OFFSET(2) NUMBITS(1) [],
        // USB D- drive value while in test mode.
        TEST_TX_DM OFFSET(3) NUMBITS(1) [],
    ],
    JFIFO_ST [
        // Occupancy of the JTAG IN (device-to-host) FIFO.
        IN_FIFO_CNT OFFSET(0) NUMBITS(2) [],
        IN_FIFO_EMPTY OFFSET(2) NUMBITS(1) [],
        IN_FIFO_FULL OFFSET(3) NUMBITS(1) [],
        // Occupancy of the JTAG OUT (host-to-device) FIFO.
        OUT_FIFO_CNT OFFSET(4) NUMBITS(2) [],
        OUT_FIFO_EMPTY OFFSET(6) NUMBITS(1) [],
        OUT_FIFO_FULL OFFSET(7) NUMBITS(1) [],
        // Write 1 to reset the JTAG IN FIFO.
        IN_FIFO_RESET OFFSET(8) NUMBITS(1) [],
        // Write 1 to reset the JTAG OUT FIFO.
        OUT_FIFO_RESET OFFSET(9) NUMBITS(1) [],
    ],
    FRAM_NUM [
        // Frame index of the most recently received SOF frame.
        SOF_FRAME_INDEX OFFSET(0) NUMBITS(11) [],
    ],
    // Shared layout for `in_ep0_st`..`in_ep3_st`: status of each IN
    // (device-to-host) endpoint.
    IN_EP_ST [
        STATE OFFSET(0) NUMBITS(2) [],
        WR_ADDR OFFSET(2) NUMBITS(7) [],
        RD_ADDR OFFSET(9) NUMBITS(7) [],
    ],
    // Shared layout for `out_ep0_st` and `out_ep2_st`: status of an OUT
    // (host-to-device) endpoint that isn't the serial port's (see
    // `OUT_EP1_ST`).
    OUT_EP_ST [
        STATE OFFSET(0) NUMBITS(2) [],
        WR_ADDR OFFSET(2) NUMBITS(7) [],
        RD_ADDR OFFSET(9) NUMBITS(7) [],
    ],
    // `out_ep1_st`: status of OUT endpoint 1, the serial port's, which
    // additionally reports how many bytes the most recently received
    // packet had.
    OUT_EP1_ST [
        STATE OFFSET(0) NUMBITS(2) [],
        WR_ADDR OFFSET(2) NUMBITS(7) [],
        RD_ADDR OFFSET(9) NUMBITS(7) [],
        REC_DATA_CNT OFFSET(16) NUMBITS(7) [],
    ],
    MISC_CONF [
        // 1: force the register clock on. 0: only clock registers while
        // software is actively accessing them.
        CLK_EN OFFSET(0) NUMBITS(1) [],
    ],
    MEM_CONF [
        // Write 1 to power down the USB SRAM.
        USB_MEM_PD OFFSET(0) NUMBITS(1) [],
        // 1: force the USB SRAM's clock on.
        USB_MEM_CLK_EN OFFSET(1) NUMBITS(1) [],
    ],
];

/// Write as much of one `USB_SERIAL_JTAG_PACKET_SIZE`-sized chunk as the
/// hardware currently has room for, then flush it. Returns the number
/// of bytes actually written.
fn write_chunk(regs: &UsbSerialJtagRegisters, bytes: &[u8]) -> usize {
    let mut written = 0;

    for &b in bytes.iter().take(USB_SERIAL_JTAG_PACKET_SIZE) {
        if !regs.ep1_conf.is_set(EP1_CONF::SERIAL_IN_EP_DATA_FREE) {
            break;
        }
        regs.ep1.write(EP1::RDWR_BYTE.val(b as u32));
        written += 1;
    }

    // Flush unconditionally, even if `written == 0`. A flush with an
    // empty buffer sends a zero-length packet, which is how a
    // full-size packet's transfer gets terminated (see the module
    // documentation).
    regs.ep1_conf.modify(EP1_CONF::WR_DONE::SET);

    written
}

pub struct UsbSerialJtag<'a, A: Alarm<'a>> {
    registers: StaticRef<UsbSerialJtagRegisters>,
    alarm: OptionalCell<&'a A>,
    tx_client: OptionalCell<&'a dyn hil::uart::TransmitClient>,
    rx_client: OptionalCell<&'a dyn hil::uart::ReceiveClient>,

    tx_buffer: TakeCell<'static, [u8]>,
    tx_len: Cell<usize>,
    tx_index: Cell<usize>,
    // Set once the most recently flushed packet was exactly
    // `USB_SERIAL_JTAG_PACKET_SIZE` bytes: a following zero-length packet
    // is then required to terminate the transfer once there is nothing
    // left to send. See the module documentation.
    tx_needs_zero_length_packet: Cell<bool>,
    // Set once a transmission timed out with the host's packet still
    // pending. Cleared once the host picks up that packet.
    tx_stalled: Cell<bool>,

    // Whether the host sent an SOF packet recently, as of the last call to
    // `host_connected`.
    host_connected: Cell<bool>,
    // Time at which `host_connected` last cleared the raw SOF interrupt.
    sof_cleared_at: Cell<A::Ticks>,

    rx_buffer: TakeCell<'static, [u8]>,
    rx_index: Cell<usize>,
    rx_len: Cell<usize>,

    // Used to invoke the `received_buffer` callback when there's already some
    // data to receive in the hardware buffers.
    deferred_call: DeferredCall,
}

impl<'a, A: Alarm<'a>> UsbSerialJtag<'a, A> {
    pub fn new(base: StaticRef<UsbSerialJtagRegisters>) -> Self {
        let usb_serial_jtag = Self {
            registers: base,
            alarm: OptionalCell::empty(),
            tx_client: OptionalCell::empty(),
            rx_client: OptionalCell::empty(),
            tx_buffer: TakeCell::empty(),
            tx_len: Cell::new(0),
            tx_index: Cell::new(0),
            tx_needs_zero_length_packet: Cell::new(false),
            tx_stalled: Cell::new(false),
            // Like ESP-IDF, assume a host until no SOF arrives.
            host_connected: Cell::new(true),
            sof_cleared_at: Cell::new(A::Ticks::from(0)),
            rx_buffer: TakeCell::empty(),
            rx_index: Cell::new(0),
            rx_len: Cell::new(0),
            deferred_call: DeferredCall::new(),
        };

        usb_serial_jtag.disable_rx_interrupt();
        usb_serial_jtag.disable_tx_interrupt();

        usb_serial_jtag
    }

    /// Set the alarm that times out a host that is gone or does not read.
    ///
    /// Without an alarm, the driver considers a host always connected and
    /// waits on it indefinitely. A transmission that is already pending gets
    /// its timeout from this call.
    pub fn set_alarm(&'a self, alarm: &'a A) {
        alarm.set_alarm_client(self);
        self.alarm.set(alarm);
        if self.tx_buffer.is_some() {
            self.arm_tx_timeout();
        }
    }

    /// Check if a host sent an SOF packet within the last
    /// [`DISCONNECT_TIMEOUT_MS`].
    ///
    /// The raw SOF interrupt bit records whether an SOF packet arrived since
    /// this function last cleared it. When this was less than
    /// [`DISCONNECT_TIMEOUT_MS`] ago, a clear bit is no evidence of a lost
    /// host, and the previous result stands.
    fn host_connected(&self) -> bool {
        self.alarm.map_or(true, |alarm| {
            let now = alarm.now();
            if self.registers.int_raw.is_set(INT::SOF) {
                self.registers.int_clr.write(INT::SOF::SET);
                self.sof_cleared_at.set(now);
                self.host_connected.set(true);
            } else if now.wrapping_sub(self.sof_cleared_at.get())
                >= alarm.ticks_from_ms(DISCONNECT_TIMEOUT_MS)
            {
                self.host_connected.set(false);
            }
            self.host_connected.get()
        })
    }

    /// Give the host [`TX_TIMEOUT_MS`] from now to pick up the flushed packet.
    fn arm_tx_timeout(&self) {
        self.alarm.map(|alarm| {
            alarm.set_alarm(alarm.now(), alarm.ticks_from_ms(TX_TIMEOUT_MS));
        });
    }

    fn enable_tx_interrupt(&self) {
        self.registers.int_ena.modify(INT::SERIAL_IN_EMPTY::SET);
    }

    fn disable_tx_interrupt(&self) {
        self.registers.int_ena.modify(INT::SERIAL_IN_EMPTY::CLEAR);
    }

    fn enable_rx_interrupt(&self) {
        self.registers.int_ena.modify(INT::SERIAL_OUT_RECV_PKT::SET);
    }

    fn disable_rx_interrupt(&self) {
        self.registers
            .int_ena
            .modify(INT::SERIAL_OUT_RECV_PKT::CLEAR);
    }

    /// Advance an in-progress `transmit_buffer` as far as the hardware will
    /// currently allow, completing (and issuing the client callback) once
    /// everything, including any required terminating zero-length packet,
    /// has been flushed.
    fn tx_progress(&self) {
        let regs = self.registers;

        if !regs.ep1_conf.is_set(EP1_CONF::SERIAL_IN_EP_DATA_FREE) {
            // Not writable yet; wait for the next SERIAL_IN_EMPTY
            // interrupt.
            return;
        }

        let idx = self.tx_index.get();
        let len = self.tx_len.get();

        if idx < len {
            self.tx_buffer.map(|tx_buf| {
                let written = write_chunk(&regs, &tx_buf[idx..len]);
                self.arm_tx_timeout();
                self.tx_index.set(idx + written);

                // If we wrote a full buffer we may need to send an empty
                // buffer so the host knows the transaction has completed.
                // From: https://github.com/espressif/esp-idf/blob/4d59230ddff16327812782151ef0afef202dc6d7/components/esp_driver_usb_serial_jtag/src/usb_serial_jtag.c#L113
                self.tx_needs_zero_length_packet
                    .set(written == USB_SERIAL_JTAG_PACKET_SIZE);
            });
        } else if self.tx_needs_zero_length_packet.take() {
            write_chunk(&regs, &[]);
            self.arm_tx_timeout();
        } else {
            self.disable_tx_interrupt();
            self.alarm.map(|alarm| alarm.disarm());
            self.tx_client.map(|client| {
                self.tx_buffer.take().map(|tx_buf| {
                    client.transmitted_buffer(tx_buf, len, Ok(()));
                });
            });
        }
    }

    /// Read bytes out of the RX hardware FIFO and invoke the `received_buffer`
    /// callback once it's full.
    ///
    /// This must only be called from `handle_interrupt` or a deferred call
    /// context.
    fn rx_progress(&self) {
        let regs = self.registers;
        let idx = self.rx_index.get();
        let len = self.rx_len.get();

        if idx < len {
            self.rx_buffer.map(|rx_buf| {
                for i in idx..len {
                    if !regs.ep1_conf.is_set(EP1_CONF::SERIAL_OUT_EP_DATA_AVAIL) {
                        break;
                    }
                    rx_buf[i] = regs.ep1.read(EP1::RDWR_BYTE) as u8;
                    self.rx_index.set(i + 1);
                }
            });
        }

        if self.rx_buffer.is_some() && self.rx_index.get() == len {
            self.disable_rx_interrupt();
            self.rx_client.map(|client| {
                self.rx_buffer.take().map(|rx_buf| {
                    client.received_buffer(rx_buf, len, Ok(()), hil::uart::Error::None);
                });
            });
        }
    }

    pub fn handle_interrupt(&self) {
        let regs = self.registers;
        let intrs = regs.int_st.extract();

        if intrs.is_set(INT::SERIAL_IN_EMPTY) {
            regs.int_clr.write(INT::SERIAL_IN_EMPTY::SET);
            self.tx_progress();
        }

        if intrs.is_set(INT::SERIAL_OUT_RECV_PKT) {
            regs.int_clr.write(INT::SERIAL_OUT_RECV_PKT::SET);
            self.rx_progress();
        }
    }
}

impl<'a, A: Alarm<'a>> AlarmClient for UsbSerialJtag<'a, A> {
    fn alarm(&self) {
        if self.tx_buffer.is_none() {
            return;
        }

        if self
            .registers
            .ep1_conf
            .is_set(EP1_CONF::SERIAL_IN_EP_DATA_FREE)
        {
            // The host picked up the packet, and the interrupt for it is
            // pending.
            self.tx_progress();
            return;
        }

        // The host did not pick up the packet in time. Until it does,
        // `transmit_buffer` fails fast. We also check for SOF packets, such
        // that a lost host makes `transmit_buffer` fail even once it picks up
        // the packet.
        self.host_connected();
        self.tx_stalled.set(true);
        self.disable_tx_interrupt();
        let written = self.tx_index.get();
        self.tx_client.map(|client| {
            self.tx_buffer.take().map(|tx_buf| {
                client.transmitted_buffer(tx_buf, written, Err(ErrorCode::FAIL));
            });
        });
    }
}

impl<'a, A: Alarm<'a>> DeferredCallClient for UsbSerialJtag<'a, A> {
    fn register(&'static self) {
        self.deferred_call.register(self);
    }

    fn handle_deferred_call(&self) {
        self.rx_progress();
    }
}

impl<'a, A: Alarm<'a>> hil::uart::Configure for UsbSerialJtag<'a, A> {
    fn configure(&self, _params: hil::uart::Parameters) -> Result<(), ErrorCode> {
        // Because this is USB CDC-ACM there is nothing to configure.
        Ok(())
    }
}

impl<'a, A: Alarm<'a>> hil::uart::Transmit<'a> for UsbSerialJtag<'a, A> {
    fn set_transmit_client(&self, client: &'a dyn hil::uart::TransmitClient) {
        self.tx_client.set(client);
    }

    fn transmit_buffer(
        &self,
        tx_data: &'static mut [u8],
        tx_len: usize,
    ) -> Result<(), (ErrorCode, &'static mut [u8])> {
        if tx_len == 0 || tx_len > tx_data.len() {
            Err((ErrorCode::SIZE, tx_data))
        } else if self.tx_buffer.is_some() {
            Err((ErrorCode::BUSY, tx_data))
        } else if !self.host_connected() {
            Err((ErrorCode::OFF, tx_data))
        } else if self.tx_stalled.get()
            && !self
                .registers
                .ep1_conf
                .is_set(EP1_CONF::SERIAL_IN_EP_DATA_FREE)
        {
            Err((ErrorCode::OFF, tx_data))
        } else {
            self.tx_stalled.set(false);
            self.tx_buffer.replace(tx_data);
            self.tx_len.set(tx_len);
            self.tx_index.set(0);
            self.tx_needs_zero_length_packet.set(false);

            self.arm_tx_timeout();
            self.enable_tx_interrupt();

            self.tx_progress();
            Ok(())
        }
    }

    fn transmit_word(&self, _word: u32) -> Result<(), ErrorCode> {
        Err(ErrorCode::FAIL)
    }

    fn transmit_abort(&self) -> Result<(), ErrorCode> {
        Err(ErrorCode::FAIL)
    }
}

impl<'a, A: Alarm<'a>> hil::uart::Receive<'a> for UsbSerialJtag<'a, A> {
    fn set_receive_client(&self, client: &'a dyn hil::uart::ReceiveClient) {
        self.rx_client.set(client);
    }

    fn receive_buffer(
        &self,
        rx_buffer: &'static mut [u8],
        rx_len: usize,
    ) -> Result<(), (ErrorCode, &'static mut [u8])> {
        if rx_len == 0 || rx_len > rx_buffer.len() {
            return Err((ErrorCode::SIZE, rx_buffer));
        } else if self.rx_buffer.is_some() {
            return Err((ErrorCode::BUSY, rx_buffer));
        }

        self.rx_buffer.replace(rx_buffer);
        self.rx_index.set(0);
        self.rx_len.set(rx_len);

        self.enable_rx_interrupt();

        // The UART hardware receives input on the granularity of USB packets,
        // which may contain one or more characters. If this operation reads
        // fewer than the available bytes of this packet, we won't re-raise an
        // interrupt for the next few bytes, but also don't have a place to
        // stash the pending bytes of the current packet for the next reader.
        // So, if we already have some bytes to read, schedule a deferred call,
        // which will read the current packet either partially or fully.
        //
        // (This check is allowed to race with an actual interrupt, in which
        // case either the deferred call or interrupt will win, one of which
        // might just find no available data to receive.)
        if self
            .registers
            .ep1_conf
            .is_set(EP1_CONF::SERIAL_OUT_EP_DATA_AVAIL)
        {
            self.deferred_call.set();
        }

        Ok(())
    }

    fn receive_word(&self) -> Result<(), ErrorCode> {
        Err(ErrorCode::FAIL)
    }

    fn receive_abort(&self) -> Result<(), ErrorCode> {
        Err(ErrorCode::FAIL)
    }
}

use kernel::utilities::io_write::IoWrite;

/// A synchronous writer for the USB_SERIAL_JTAG CDC-ACM port, useful for
/// panics.
///
/// This is only to be used by panic messages and is not used within the normal
/// operation of the Tock kernel. It does not hold a full [`UsbSerialJtag`]
/// instance, as creating that one invokes [`DeferredCall::new`]. That fails for
/// panics raised in a trap handler, where the deferred call state is not
/// accessible.
struct UsbSerialJtagPanicWriter {
    registers: StaticRef<UsbSerialJtagRegisters>,
    // Set once the host did not pick up a packet in time.
    stalled: bool,
}

impl UsbSerialJtagPanicWriter {
    /// Wait until the host picked up the last flushed packet.
    ///
    /// Return `false` if the host did not pick it up within
    /// [`PANIC_TX_TIMEOUT_POLLS`]. Once this happens, the writer only checks
    /// for room and no longer waits.
    fn wait_writable(&mut self) -> bool {
        let polls = if self.stalled {
            1
        } else {
            PANIC_TX_TIMEOUT_POLLS
        };
        for _ in 0..polls {
            if self
                .registers
                .ep1_conf
                .is_set(EP1_CONF::SERIAL_IN_EP_DATA_FREE)
            {
                self.stalled = false;
                return true;
            }
            rv32i::support::nop();
        }
        self.stalled = true;
        false
    }

    /// Synchronously write `bytes` out the CDC-ACM serial port, including any
    /// terminating zero-length packet required.
    ///
    /// When the host does not pick up a packet in time, this function drops
    /// the remaining bytes.
    fn transmit_sync(&mut self, bytes: &[u8]) {
        let mut idx = 0;

        while idx < bytes.len() {
            let end = min(idx + USB_SERIAL_JTAG_PACKET_SIZE, bytes.len());
            if !self.wait_writable() {
                return;
            }
            idx += write_chunk(&self.registers, &bytes[idx..end]);
        }

        if !bytes.is_empty()
            && bytes.len().is_multiple_of(USB_SERIAL_JTAG_PACKET_SIZE)
            && self.wait_writable()
        {
            write_chunk(&self.registers, &[]);
        }
    }
}

impl IoWrite for UsbSerialJtagPanicWriter {
    fn write(&mut self, buf: &[u8]) -> usize {
        self.transmit_sync(buf);
        buf.len()
    }
}

impl core::fmt::Write for UsbSerialJtagPanicWriter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        self.write(s.as_bytes());
        Ok(())
    }
}

/// Configuration for the synchronous USB_SERIAL_JTAG panic writer.
pub struct UsbSerialJtagPanicWriterConfig {
    pub registers: StaticRef<UsbSerialJtagRegisters>,
}

impl<'a, A: Alarm<'a>> kernel::platform::chip::PanicWriter for UsbSerialJtag<'a, A> {
    type Config = UsbSerialJtagPanicWriterConfig;

    fn create_panic_writer(
        config: Self::Config,
        _panic: &core::panic::PanicInfo,
    ) -> impl IoWrite + core::fmt::Write {
        UsbSerialJtagPanicWriter {
            registers: config.registers,
            stalled: false,
        }
    }
}
