// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Leon Schuermann <leon@is.currently.online> 2023.
// Copyright Tock Contributors 2023.

//! Ethernet network cards
use crate::ErrorCode;
use core::fmt;
use core::ops::Range;

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct MacAddress([u8; 6]);

impl MacAddress {
    pub const BROADCAST_MAC_ADDRESS: MacAddress = MacAddress([0xFF; 6]);

    pub const fn new(bytes: [u8; 6]) -> Self {
        Self(bytes)
    }

    pub fn set_address(&mut self, bytes: &[u8; 6]) {
        // Can't panic
        self.0.copy_from_slice(bytes);
    }

    pub const fn get_address(&self) -> [u8; 6] {
        self.0
    }

    pub const fn is_broadcast(&self) -> bool {
        self.0[0] == 0xFF &&
        self.0[1] == 0xFF &&
        self.0[2] == 0xFF &&
        self.0[3] == 0xFF &&
        self.0[4] == 0xFF &&
        self.0[5] == 0xFF
    }

    pub const fn is_multicast(&self) -> bool {
        self.0[0] & 0x1 != 0
    }

    pub const fn is_unicast(&self) -> bool {
        !self.is_multicast() && !self.is_broadcast()
    }
}

impl Default for MacAddress {
    fn default() -> Self {
        Self {
            0: [0; 6]
        }
    }
}

impl From<u64> for MacAddress {
    fn from(value: u64) -> Self {
        // Can't panic
        MacAddress(value.to_be_bytes()[2..8].try_into().unwrap())
    }
}

impl From<MacAddress> for u64 {
    fn from(mac_address: MacAddress) -> Self {
        // Can't panic
        let high: u16 = u16::from_be_bytes(mac_address.get_address()[0..2].try_into().unwrap());
        let low: u32 = u32::from_be_bytes(mac_address.get_address()[2..6].try_into().unwrap());

        ((high as u64) << 32) + (low as u64)
    }
}

impl fmt::Display for MacAddress {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{:02x}-{:02x}-{:02x}-{:02x}-{:02x}-{:02x}",
            self.get_address()[0], self.get_address()[1], self.get_address()[2],
            self.get_address()[3], self.get_address()[4], self.get_address()[5]
        )
    }
}

pub const MAX_FRAME_LENGTH: usize = 1536;
pub const DESTINATION_FIELD: Range<usize> = 0..6;
pub const SOURCE_FIELD: Range<usize> = 6..12;
pub const LENGTH_OR_TYPE_NO_VLAN_FIELD: Range<usize> = 12..14;
pub const HEADER_NO_VLAN_FIELD: Range<usize> = 0..14;

#[derive(PartialEq, Debug)]
pub struct EthernetFrame([u8; MAX_FRAME_LENGTH]);

#[repr(u16)]
#[derive(PartialEq, Debug)]
pub enum EthernetType {
    RawFrame = 0,
    Unknown = 1501,
}

// No method panics
impl EthernetFrame {
    pub fn set_destination(&mut self, destination_mac_address: MacAddress) {
        self.0[DESTINATION_FIELD].copy_from_slice(&destination_mac_address.get_address());
    }

    pub fn get_destination(&self) -> MacAddress {
        MacAddress::new(self.0[DESTINATION_FIELD].try_into().unwrap())
    }

    pub fn set_source(&mut self, source_mac_address: MacAddress) {
        self.0[SOURCE_FIELD].copy_from_slice(&source_mac_address.get_address());
    }

    pub fn get_source(&self) -> MacAddress {
        MacAddress::new(self.0[SOURCE_FIELD].try_into().unwrap())
    }

    pub fn set_payload_length_no_vlan(&mut self, length: u16) {
        self.0[LENGTH_OR_TYPE_NO_VLAN_FIELD].copy_from_slice(&length.to_be_bytes());
    }

    pub fn get_payload_length_no_vlan(&self) -> u16 {
        u16::from_be_bytes(self.0[LENGTH_OR_TYPE_NO_VLAN_FIELD].try_into().unwrap())
    }

    pub fn get_type_no_vlan(&self) -> EthernetType {
        match u16::from_be_bytes(self.0[LENGTH_OR_TYPE_NO_VLAN_FIELD].try_into().unwrap()) {
            x if x <= 1500 => EthernetType::RawFrame,
            _ => EthernetType::Unknown
        }
    }

    pub fn get_header_no_vlan(&self) -> [u8; HEADER_NO_VLAN_FIELD.end] {
        self.0[HEADER_NO_VLAN_FIELD].try_into().unwrap()
    }

    pub fn set_payload_no_vlan(&mut self, payload: &[u8]) -> Result<(), ErrorCode> {
        if payload.len() > MAX_FRAME_LENGTH - HEADER_NO_VLAN_FIELD.end {
            return Err(ErrorCode::SIZE);
        }

        self.0[HEADER_NO_VLAN_FIELD.end..(HEADER_NO_VLAN_FIELD.end + payload.len())].copy_from_slice(payload);

        Ok(())
    }

    pub fn get_payload_no_vlan(&self) -> &[u8; MAX_FRAME_LENGTH - HEADER_NO_VLAN_FIELD.end] {
        (&self.0[HEADER_NO_VLAN_FIELD.end..]).try_into().unwrap()
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.0.as_mut_ptr()
    }

    pub fn len(&self) -> usize {
        match self.get_type_no_vlan() {
            EthernetType::RawFrame => self.get_payload_length_no_vlan() as usize + HEADER_NO_VLAN_FIELD.end,
            EthernetType::Unknown => 0
        }
    }
}

impl Default for EthernetFrame {
    fn default() -> Self {
        Self {
            0: [0; MAX_FRAME_LENGTH]
        }
    }
}

impl fmt::Display for EthernetFrame {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "Type: {:?}\nDestination: {}\nSource: {}\nLength: {}\nPayload: {:?}",
            self.get_type_no_vlan(), self.get_destination(), self.get_source(),
            self.get_payload_length_no_vlan(), self.get_payload_no_vlan()
        )
    }
}

#[derive(PartialEq, Debug)]
pub enum OperationMode {
    HalfDuplex = 0b0,
    FullDuplex = 0b1,
}

#[derive(PartialEq, Debug)]
pub enum EthernetSpeed {
    Speed10Mbs = 0b0,
    Speed100Mbs = 0b1,
}

pub trait Configure {
    fn init(&self) -> Result<(), ErrorCode>;

    fn set_operation_mode(&self, _operation_mode: OperationMode) -> Result<(), ErrorCode> {
        Err(ErrorCode::NOSUPPORT)
    }

    fn get_operation_mode(&self) -> OperationMode;

    fn set_speed(&self, _speed: EthernetSpeed) -> Result<(), ErrorCode> {
        Err(ErrorCode::NOSUPPORT)
    }

    fn get_speed(&self) -> EthernetSpeed;

    fn set_loopback_mode(&self, _enable: bool) -> Result<(), ErrorCode> {
        Err(ErrorCode::NOSUPPORT)
    }

    fn is_loopback_mode_enabled(&self) -> bool;

    fn set_mac_address(&self, _mac_address: MacAddress) -> Result<(), ErrorCode> {
        Err(ErrorCode::NOSUPPORT)
    }

    fn get_mac_address(&self) -> MacAddress;

    // TODO: Move this into the Transmit trait
    fn start_transmit(&self) -> Result<(), ErrorCode>;

    // TODO: Move this into the Receive trait
    fn start_receive(&self) -> Result<(), ErrorCode>;
}

pub trait Transmit<'a> {
    fn set_transmit_client(&self, transmit_client: &'a dyn TransmitClient);
    fn start_transmitter(&self) -> Result<(), ErrorCode>;
    fn stop_transmitter(&self) -> Result<(), ErrorCode>;
    fn is_transmitter_up(&self) -> bool;
    fn transmit_raw_frame(
        &self,
        destination_address: MacAddress,
        payload: &[u8]
    ) -> Result<(), ErrorCode>;
}

pub trait Receive<'a> {
    fn set_receive_client(&self, receive_client: &'a dyn ReceiveClient);
    fn start_receiver(&self) -> Result<(), ErrorCode>;
    fn stop_receiver(&self) -> Result<(), ErrorCode>;
    fn is_receiver_up(&self) -> bool;
    fn receive_raw_frame(
        &self,
        frame: &'static mut EthernetFrame
    ) -> Result<(), (ErrorCode, &'static mut EthernetFrame)>;
}

pub trait TransmitClient {
    fn transmitted_frame(&self, transmit_status: Result<(), ErrorCode>);
}

pub trait ReceiveClient {
    fn received_frame(&self,
        receive_status: Result<(), ErrorCode>,
        received_frame_length: usize,
        received_frame: &'static mut EthernetFrame
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mac_address() {
        let mut mac_address = MacAddress::default();
        assert_eq!([0; 6], mac_address.get_address());
        assert_eq!(0x0 as u64, mac_address.into());

        mac_address = MacAddress::from(0x112233445566);
        assert_eq!([0x11, 0x22, 0x33, 0x44, 0x55, 0x66], mac_address.get_address());
        assert_eq!(0x112233445566 as u64, mac_address.into());

        mac_address.set_address(&[0x12, 0x34, 0x56, 0x78, 0x90, 0xAB]);
        assert_eq!([0x12, 0x34, 0x56, 0x78, 0x90, 0xAB], mac_address.get_address());
        assert_eq!(0x1234567890AB as u64, mac_address.into());

        assert_eq!(false, mac_address.is_broadcast());
        assert_eq!(false, mac_address.is_multicast());
        assert_eq!(true, mac_address.is_unicast());

        mac_address = MacAddress([0xFF; 6]);
        assert_eq!(true, mac_address.is_broadcast());
        assert_eq!(true, mac_address.is_multicast());
        assert_eq!(false, mac_address.is_unicast());

        mac_address = MacAddress::new([0x13, 0x34, 0x56, 0x78, 0x90, 0xAB]);
        assert_eq!(false, mac_address.is_broadcast());
        assert_eq!(true, mac_address.is_multicast());
        assert_eq!(false, mac_address.is_unicast());
    }

    #[test]
    fn test_frame() {
        let mut ethernet_frame = EthernetFrame::default();
        assert_eq!(MacAddress::from(0x0), ethernet_frame.get_destination());
        assert_eq!(MacAddress::from(0x0), ethernet_frame.get_source());
        assert_eq!(0x0, ethernet_frame.get_payload_length_no_vlan());

        let destination_mac_address = MacAddress::new([0x11, 0x22, 0x33, 0x44, 0x55, 0x66]);
        ethernet_frame.set_destination(destination_mac_address);
        assert_eq!(destination_mac_address, ethernet_frame.get_destination());

        let source_mac_address = MacAddress::new([0x12, 0x34, 0x56, 0x78, 0x90, 0xAB]);
        ethernet_frame.set_source(source_mac_address);
        assert_eq!(source_mac_address, ethernet_frame.get_source());

        ethernet_frame.set_payload_length_no_vlan(123);
        assert_eq!(123, ethernet_frame.get_payload_length_no_vlan());

        assert_eq!(EthernetType::RawFrame, ethernet_frame.get_type_no_vlan());
        ethernet_frame.set_payload_length_no_vlan(1500);
        assert_eq!(EthernetType::RawFrame, ethernet_frame.get_type_no_vlan());
        ethernet_frame.set_payload_length_no_vlan(1501);
        assert_eq!(EthernetType::Unknown, ethernet_frame.get_type_no_vlan());

        assert_eq!([0x11, 0x22, 0x33, 0x44, 0x55, 0x66,
                    0x12, 0x34, 0x56, 0x78, 0x90, 0xAB,
                    0x05, 0xDD], ethernet_frame.get_header_no_vlan());

        let payload = b"TockOS is great!";
        assert_eq!(Ok(()), ethernet_frame.set_payload_no_vlan(payload));
        assert_eq!(payload,
            &ethernet_frame.get_payload_no_vlan()[0..payload.len()]);

        ethernet_frame.set_payload_length_no_vlan(payload.len() as u16);
        assert_eq!(payload.len() + HEADER_NO_VLAN_FIELD.end, ethernet_frame.len());
    }
}

pub trait EthernetAdapterClient {
    fn tx_done(
        &self,
        err: Result<(), ErrorCode>,
        packet_buffer: &'static mut [u8],
        len: u16,
        packet_identifier: usize,
        timestamp: Option<u64>,
    );
    fn rx_packet(&self, packet: &[u8], timestamp: Option<u64>);
}

pub trait EthernetAdapter<'a> {
    fn set_client(&self, client: &'a dyn EthernetAdapterClient);
    fn transmit(
        &self,
        packet: &'static mut [u8],
        len: u16,
        packet_identifier: usize,
    ) -> Result<(), (ErrorCode, &'static mut [u8])>;
}