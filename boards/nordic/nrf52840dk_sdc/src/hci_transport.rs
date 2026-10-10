// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Bluetooth HCI transport over the SoftDevice Controller.
//!
//! The SoftDevice Controller queues the packets for the host internally. The
//! transport pulls them from that queue only while receive is enabled. The
//! controller applies link-layer flow control to the peer while the host
//! does not take packets.
//!
//! The SoftDevice Controller answers a command during the call that submits
//! it. The transport stores this reply and delivers it before the next packet
//! from the controller's queue. A host waits for this reply before it sends
//! the next command, because HCI command flow control requires it. If a
//! command arrives while a reply is stored, the transport holds the command
//! as the outstanding packet. It submits the command after it delivered the
//! reply.

use core::cell::Cell;

use kernel::ErrorCode;
use kernel::deferred_call::{DeferredCall, DeferredCallClient};
use kernel::hil::bluetooth_hci::{HciTransport, HciTransportClient, MAX_PACKET_LEN};
use kernel::utilities::cells::{MapCell, OptionalCell, TakeCell};

use crate::sdc;

const _: () = assert!(sdc::PACKET_MAX == MAX_PACKET_LEN);

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    Stopped,
    Resetting,
    Ready,
}

pub struct SdcHciTransport<'a> {
    client: OptionalCell<&'a dyn HciTransportClient>,
    deferred_call: DeferredCall,
    state: Cell<State>,
    /// Whether the SoftDevice Controller is enabled.
    enabled: Cell<bool>,
    receiving: Cell<bool>,
    /// The buffer of the outstanding packet, until `transmit_done` returns
    /// it.
    tx_buffer: TakeCell<'static, [u8]>,
    tx_len: Cell<usize>,
    /// The packet in `tx_buffer` is a command that waits for the delivery of
    /// the stored reply.
    tx_queued: Cell<bool>,
    /// The result for `transmit_done`.
    tx_result: Cell<Result<(), ErrorCode>>,
    /// The result of the last reset, until `reset_done` reports it.
    reset_result: OptionalCell<Result<(), ErrorCode>>,
    /// The reply to the last command and its length, until the client
    /// receives it.
    reply: MapCell<([u8; sdc::PACKET_MAX], usize)>,
}

impl SdcHciTransport<'_> {
    pub fn new() -> Self {
        Self {
            client: OptionalCell::empty(),
            deferred_call: DeferredCall::new(),
            state: Cell::new(State::Stopped),
            enabled: Cell::new(false),
            receiving: Cell::new(false),
            tx_buffer: TakeCell::empty(),
            tx_len: Cell::new(0),
            tx_queued: Cell::new(false),
            tx_result: Cell::new(Ok(())),
            reset_result: OptionalCell::empty(),
            reply: MapCell::new(([0; sdc::PACKET_MAX], 0)),
        }
    }

    fn reply_pending(&self) -> bool {
        self.reply.map_or(false, |(_, len)| *len > 0)
    }

    /// Submit `packet` to the controller, and store the reply if it has one.
    fn submit(&self, packet: &[u8]) -> Result<(), ErrorCode> {
        self.reply
            .map(|(reply, reply_len)| sdc::hci_tx(packet, reply).map(|n| *reply_len = n))
            .unwrap_or(Err(()))
            .map_err(|()| ErrorCode::INVAL)
    }

    /// Submit the command that waited for the delivery of the stored reply.
    fn submit_queued(&self) {
        if !self.tx_queued.take() {
            return;
        }
        let len = self.tx_len.get();
        if let Some(result) = self.tx_buffer.map(|buffer| self.submit(&buffer[..len])) {
            // The command was checked in `transmit`, so `submit` only fails if
            // the controller rejects it.
            self.tx_result.set(result.map_err(|_| ErrorCode::FAIL));
        }
        self.deferred_call.set();
    }

    /// Deliver packets to the client while receive is enabled.
    ///
    /// Runs after MPSL's low-priority processing and from the deferred call.
    pub fn rx_ready(&self) {
        while self.state.get() == State::Ready && self.receiving.get() {
            let mut packet = [0u8; sdc::PACKET_MAX];
            let reply_len = self
                .reply
                .map(|(reply, len)| {
                    let n = core::mem::replace(len, 0);
                    packet[..n].copy_from_slice(&reply[..n]);
                    n
                })
                .unwrap_or(0);
            let len = if reply_len > 0 {
                reply_len
            } else {
                sdc::hci_rx_get(&mut packet)
            };
            if len == 0 {
                break;
            }
            self.client
                .map(|client| client.packet_received(&packet[..len]));
            if reply_len > 0 {
                self.submit_queued();
            }
        }
    }
}

impl<'a> HciTransport<'a> for SdcHciTransport<'a> {
    fn set_client(&self, client: &'a dyn HciTransportClient) {
        self.client.set(client);
    }

    fn reset(&self) -> Result<(), ErrorCode> {
        if self.state.get() == State::Resetting {
            return Err(ErrorCode::BUSY);
        }
        self.state.set(State::Resetting);
        // A queued command has not reached the controller. Any other
        // outstanding packet has reached it during `transmit`.
        if self.tx_queued.take() {
            self.tx_result.set(Err(ErrorCode::CANCEL));
        }
        if self.enabled.get() {
            sdc::disable();
        }
        self.reply.map(|(_, len)| *len = 0);
        let started = sdc::enable();
        self.enabled.set(started);
        self.reset_result.set(if started {
            Ok(())
        } else {
            Err(ErrorCode::FAIL)
        });
        self.deferred_call.set();
        Ok(())
    }

    fn transmit(
        &self,
        buffer: &'static mut [u8],
        len: usize,
    ) -> Result<(), (ErrorCode, &'static mut [u8])> {
        if self.state.get() != State::Ready {
            return Err((ErrorCode::OFF, buffer));
        }
        if self.tx_buffer.is_some() {
            return Err((ErrorCode::BUSY, buffer));
        }
        if len > buffer.len() || len > sdc::PACKET_MAX {
            return Err((ErrorCode::SIZE, buffer));
        }
        let packet = &buffer[..len];
        let is_command = packet.first() == Some(&sdc::H4_CMD);
        if is_command && self.reply_pending() {
            if !(len >= 4 && 4 + packet[3] as usize == len) {
                return Err((ErrorCode::INVAL, buffer));
            }
            self.tx_queued.set(true);
        } else if let Err(e) = self.submit(packet) {
            return Err((e, buffer));
        }
        self.tx_len.set(len);
        self.tx_result.set(Ok(()));
        self.tx_buffer.replace(buffer);
        self.deferred_call.set();
        Ok(())
    }

    fn enable_receive(&self) {
        self.receiving.set(true);
        self.deferred_call.set();
    }

    fn disable_receive(&self) {
        self.receiving.set(false);
    }
}

impl DeferredCallClient for SdcHciTransport<'_> {
    fn handle_deferred_call(&self) {
        if !self.tx_queued.get() {
            if let Some(buffer) = self.tx_buffer.take() {
                let result = self.tx_result.get();
                self.client
                    .map(|client| client.transmit_done(buffer, result));
            }
        }
        if let Some(result) = self.reset_result.take() {
            self.state.set(if result.is_ok() {
                State::Ready
            } else {
                State::Stopped
            });
            self.client.map(|client| client.reset_done(result));
        }
        self.rx_ready();
    }

    fn register(&'static self) {
        self.deferred_call.register(self);
    }
}
