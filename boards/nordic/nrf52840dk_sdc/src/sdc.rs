// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use core::mem::{align_of, size_of};
use core::ptr::{addr_of_mut, read_volatile, write_volatile};
use kernel::debug;

// The generated bindings are one flat namespace of ~90 nrfxlib items.
#[allow(clippy::wildcard_imports)]
use crate::bindings::*;

pub const IRQ_POWER_CLOCK: usize = 0;
pub const IRQ_RADIO: usize = 1;
pub const IRQ_TIMER0: usize = 8;
pub const IRQ_RTC0: usize = 11;
pub const IRQ_SWI5_LOW_PRIO: u32 = 25;

const NUM_IRQS: usize = 48;
const VECTOR_TABLE_LEN: usize = 16 + NUM_IRQS;

const SCB_VTOR: *mut u32 = 0xE000_ED08 as *mut u32;
const NVIC_IPR: *mut u8 = 0xE000_E400 as *mut u8;
const SCB_SHPR2: *mut u32 = 0xE000_ED1C as *mut u32;
const SCB_SHPR3: *mut u32 = 0xE000_ED20 as *mut u32;

const RNG_BASE: usize = 0x4000_D000;

pub const H4_CMD: u8 = 0x01;
pub const H4_ACL: u8 = 0x02;
pub const H4_EVT: u8 = 0x04;

pub const PACKET_MAX: usize = 259;

#[repr(align(512))]
struct RamVectors([u32; VECTOR_TABLE_LEN]);
static mut RAM_VECTORS: RamVectors = RamVectors([0; VECTOR_TABLE_LEN]);

#[repr(align(8))]
struct SdcMem([u8; 6144]);
static mut SDC_MEM: SdcMem = SdcMem([0; 6144]);

unsafe extern "C" fn mpsl_assert(_file: *const core::ffi::c_char, line: u32) {
    panic!("MPSL assert at line {}", line);
}

unsafe extern "C" fn sdc_fault(_file: *const core::ffi::c_char, line: u32) {
    panic!("SDC fault at line {}", line);
}

unsafe extern "C" fn sdc_callback() {}

unsafe extern "C" fn rand_poll(buf: *mut u8, len: u8) {
    unsafe {
        let task_start = RNG_BASE as *mut u32;
        let task_stop = (RNG_BASE + 0x004) as *mut u32;
        let events_valrdy = (RNG_BASE + 0x100) as *mut u32;
        let config = (RNG_BASE + 0x504) as *mut u32;
        let value = (RNG_BASE + 0x508) as *const u32;

        write_volatile(config, 1); // CONFIG.DERCEN
        write_volatile(task_start, 1);
        for i in 0..len as usize {
            while read_volatile(events_valrdy) == 0 {}
            write_volatile(events_valrdy, 0);
            *buf.add(i) = read_volatile(value) as u8;
        }
        write_volatile(task_stop, 1);
    }
}

unsafe fn set_irq_priority(irq: usize, prio: u8) {
    write_volatile(NVIC_IPR.add(irq), prio << 5);
}

unsafe fn install_vectors() {
    let src = read_volatile(SCB_VTOR) as *const u32;
    let dst = addr_of_mut!(RAM_VECTORS.0).cast::<u32>();
    core::ptr::copy_nonoverlapping(src, dst, VECTOR_TABLE_LEN);

    *dst.add(16 + IRQ_POWER_CLOCK) = MPSL_IRQ_CLOCK_Handler as *const () as usize as u32;
    *dst.add(16 + IRQ_RADIO) = MPSL_IRQ_RADIO_Handler as *const () as usize as u32;
    *dst.add(16 + IRQ_TIMER0) = MPSL_IRQ_TIMER0_Handler as *const () as usize as u32;
    *dst.add(16 + IRQ_RTC0) = MPSL_IRQ_RTC0_Handler as *const () as usize as u32;

    cortexm4::support::with_interrupts_disabled(|| {
        write_volatile(SCB_VTOR, dst as u32);
        core::arch::asm!("dsb", "isb");
    });
}

unsafe fn configure_priorities() {
    for irq in 0..NUM_IRQS {
        set_irq_priority(irq, 4);
    }
    let shpr2 = read_volatile(SCB_SHPR2) & 0x00FF_FFFF;
    write_volatile(SCB_SHPR2, shpr2 | (4u32 << 5) << 24);
    write_volatile(SCB_SHPR3, ((4u32 << 5) << 16) | ((4u32 << 5) << 24));

    set_irq_priority(IRQ_RADIO, 0);
    set_irq_priority(IRQ_TIMER0, 0);
    set_irq_priority(IRQ_RTC0, 0);
    set_irq_priority(IRQ_POWER_CLOCK, 4);
    set_irq_priority(IRQ_SWI5_LOW_PRIO as usize, 6);
}

unsafe fn release_clock() {
    const CLOCK: usize = 0x4000_0000;
    write_volatile((CLOCK + 0x308) as *mut u32, 0xffff_ffff);
    write_volatile((CLOCK + 0x004) as *mut u32, 1);
    write_volatile((CLOCK + 0x00c) as *mut u32, 1);
    write_volatile((CLOCK + 0x100) as *mut u32, 0);
    write_volatile((CLOCK + 0x104) as *mut u32, 0);
}

/// Bring up MPSL and initialize the SoftDevice Controller.
///
/// The controller stays disabled until [`enable`] is called.
pub unsafe fn init() -> bool {
    install_vectors();
    configure_priorities();
    release_clock();

    let clock_cfg = mpsl_clock_lfclk_cfg_t {
        source: MPSL_CLOCK_LF_SRC::MPSL_CLOCK_LF_SRC_XTAL as u8,
        rc_ctiv: 0,
        rc_temp_ctiv: 0,
        accuracy_ppm: 20,
        skip_wait_lfclk_started: false,
    };

    if mpsl_init(
        &clock_cfg,
        IRQ_SWI5_LOW_PRIO as IRQn_Type,
        Some(mpsl_assert),
    ) != 0
    {
        debug!("mpsl_init failed");
        return false;
    }
    if sdc_init(Some(sdc_fault)) != 0 {
        debug!("sdc_init failed");
        return false;
    }

    let rand_source = sdc_rand_source_t {
        rand_poll: Some(rand_poll),
    };
    if sdc_rand_source_register(&rand_source) != 0 {
        debug!("sdc_rand_source_register failed");
        return false;
    }
    true
}

/// Enable the SoftDevice Controller as a peripheral-only controller.
///
/// The controller starts in its power-on state. Requires [`init`].
pub fn enable() -> bool {
    // ### Safety
    //
    // `init` registered the random source that `sdc_enable` requires. The
    // controller is disabled, so it does not use `SDC_MEM` yet.
    unsafe {
        // The feature set is fixed here, before sdc_enable(). Userspace
        // cannot widen it afterwards: sdc_support_* returns -NRF_EPERM once
        // enabled. Scanning, the central role and direct test mode are
        // deliberately absent.
        sdc_support_adv();
        sdc_support_peripheral();
        sdc_support_dle_peripheral();
        sdc_support_le_2m_phy();
        sdc_support_phy_update_peripheral();

        let required = sdc_cfg_set(0, 0, core::ptr::null());
        if required < 0 || required as usize > 6144 {
            debug!("sdc memory pool too small: need {}", required);
            return false;
        }

        if sdc_enable(Some(sdc_callback), addr_of_mut!(SDC_MEM.0).cast::<u8>()) != 0 {
            debug!("sdc_enable failed");
            return false;
        }
    }
    true
}

/// Disable the SoftDevice Controller. It drops all of its state, including
/// its queued HCI packets.
pub fn disable() {
    // ### Safety
    //
    // `sdc_disable` is synchronous and has no preconditions.
    unsafe {
        sdc_disable();
    }
}

/// Write a Command Complete event for `opcode` into `resp`, returning its length.
unsafe fn cmd_complete(
    opcode: u16,
    status: u8,
    ret: *const u8,
    ret_len: usize,
    resp: &mut [u8; PACKET_MAX],
) -> usize {
    // `ret_len` is at most `RET_MAX` for every command in the table. The
    // assertion next to `RET_MAX` checks that such an event fits into one
    // packet.
    let ret_len = ret_len.min(RET_MAX);
    resp[0] = H4_EVT;
    resp[1] = 0x0e;
    resp[2] = (4 + ret_len) as u8;
    resp[3] = 1;
    resp[4] = opcode as u8;
    resp[5] = (opcode >> 8) as u8;
    resp[6] = status;
    if ret_len > 0 && !ret.is_null() {
        core::ptr::copy_nonoverlapping(ret, resp.as_mut_ptr().add(7), ret_len);
    }
    7 + ret_len
}

/// Write a Command Status event for `opcode` into `resp`, returning its length.
fn cmd_status(opcode: u16, status: u8, resp: &mut [u8; PACKET_MAX]) -> usize {
    resp[..7].copy_from_slice(&[
        H4_EVT,
        0x0f,
        0x04,
        status,
        1,
        opcode as u8,
        (opcode >> 8) as u8,
    ]);
    7
}

/// Pull the oldest packet the controller has queued into `dst`, H4-framed.
///
/// Returns its length, or 0 when the controller has nothing. Anything not
/// pulled stays inside the SoftDevice, which is where the queue lives; the
/// kernel keeps no packet buffers of its own. Packets of other types, and
/// packets longer than `PACKET_MAX`, are discarded.
pub fn hci_rx_get(dst: &mut [u8; PACKET_MAX]) -> usize {
    loop {
        let mut msg_type: u8 = 0;
        // sdc_hci_get needs HCI_MSG_BUFFER_MAX_SIZE (258) bytes of room,
        // which is exactly what is left after the H4 type byte.
        const _: () = assert!(PACKET_MAX > 258);
        // ### Safety
        //
        // Byte 0 is reserved for the H4 type prefix, so `sdc_hci_get` writes
        // into the 258 bytes after it.
        if unsafe { sdc_hci_get(dst.as_mut_ptr().add(1), &mut msg_type) } != 0 {
            return 0;
        }
        let len = match msg_type {
            0x04 => 2 + dst[2] as usize,
            0x02 => 4 + u16::from_le_bytes([dst[3], dst[4]]) as usize,
            _ => continue,
        };
        if len + 1 > PACKET_MAX {
            debug!("sdc: discarding {} B HCI packet", len + 1);
            continue;
        }
        dst[0] = if msg_type == 0x04 { H4_EVT } else { H4_ACL };
        return len + 1;
    }
}

/// Run MPSL's low-priority work. Packets are collected by `hci_rx_get`, which
/// writes into a buffer the caller supplies.
pub fn low_priority_process() {
    unsafe { mpsl_low_priority_process() }
}

/// Submit one H4 packet to the controller.
///
/// `data` is staged into a fixed `PACKET_MAX` buffer, zeroed first: the
/// SoftDevice reads a fixed number of bytes per opcode without consulting the
/// host's declared parameter length, so it must never be handed a shorter
/// buffer, and must never see bytes from a previous packet.
///
/// Any synthesized Command Complete / Command Status is written into `resp`,
/// whose length is returned (0 for ACL data).
pub fn hci_tx(data: &[u8], resp: &mut [u8; PACKET_MAX]) -> Result<usize, ()> {
    unsafe {
        if data.is_empty() || data.len() > PACKET_MAX {
            return Err(());
        }
        let mut stage = [0u8; PACKET_MAX];
        stage[..data.len()].copy_from_slice(data);
        let len = data.len();

        match stage[0] {
            H4_ACL => {
                if len < 5 {
                    return Err(());
                }
                // The packet must carry exactly the payload it declares.
                let dlen = u16::from_le_bytes([stage[3], stage[4]]) as usize;
                if 5 + dlen != len {
                    return Err(());
                }
                if sdc_hci_data_put(stage.as_ptr().add(1)) != 0 {
                    return Err(());
                }
                Ok(0)
            }
            H4_CMD => {
                if len < 4 {
                    return Err(());
                }
                let opcode = u16::from_le_bytes([stage[1], stage[2]]);
                let plen = stage[3] as usize;
                // The packet must carry exactly the parameters it declares.
                if 4 + plen != len {
                    return Err(());
                }
                Ok(dispatch_command(opcode, stage.as_ptr().add(4), plen, resp))
            }
            _ => Err(()),
        }
    }
}

/// Read Local Supported Commands. nrfxlib declares it, but the
/// peripheral-only library does not provide it. An all-zero bitmap is only
/// consulted by hosts for extended advertising v2.
unsafe extern "C" fn read_local_supported_commands(
    p_return: *mut sdc_hci_cmd_ip_read_local_supported_commands_return_t,
) -> u8 {
    p_return.write_bytes(0, 1);
    0x00
}

/// Largest return parameters any command below produces.
const RET_MAX: usize = 64;

// A Command Complete event with `RET_MAX` bytes of return parameters fits
// into one H4 packet, and its parameter length fits into one byte.
const _: () = assert!(7 + RET_MAX <= PACKET_MAX && 4 + RET_MAX <= u8::MAX as usize);

/// Parameter length of a table entry, from the nrfxlib struct.
macro_rules! param_len {
    () => {
        0
    };
    (p: $pt:ty) => {
        size_of::<$pt>()
    };
    (r: $rt:ty) => {
        0
    };
    (p: $pt:ty, r: $rt:ty) => {
        size_of::<$pt>()
    };
}

/// Return-parameter length of a table entry, from the nrfxlib struct.
macro_rules! ret_len {
    () => {
        0
    };
    (p: $pt:ty) => {
        0
    };
    (r: $rt:ty) => {
        size_of::<$rt>()
    };
    (p: $pt:ty, r: $rt:ty) => {
        size_of::<$rt>()
    };
}

/// Strictest alignment a table entry's structs require.
macro_rules! align_req {
    () => {
        1
    };
    (p: $pt:ty) => {
        align_of::<$pt>()
    };
    (r: $rt:ty) => {
        align_of::<$rt>()
    };
    (p: $pt:ty, r: $rt:ty) => {
        if align_of::<$pt>() > align_of::<$rt>() {
            align_of::<$pt>()
        } else {
            align_of::<$rt>()
        }
    };
}

/// Calls a table entry's SoftDevice function with the pointers it takes.
macro_rules! invoke {
    ($f:ident, $p:expr, $r:expr,) => {
        $f()
    };
    ($f:ident, $p:expr, $r:expr, p: $pt:ty) => {
        $f($p.cast::<$pt>())
    };
    ($f:ident, $p:expr, $r:expr, r: $rt:ty) => {
        $f($r.cast::<$rt>())
    };
    ($f:ident, $p:expr, $r:expr, p: $pt:ty, r: $rt:ty) => {
        $f($p.cast::<$pt>(), $r.cast::<$rt>())
    };
}

/// Builds the event a table entry answers with.
macro_rules! reply {
    (complete, $op:expr, $st:expr, $r:expr, $rl:expr, $resp:expr) => {
        cmd_complete($op, $st, $r, $rl, $resp)
    };
    (status, $op:expr, $st:expr, $r:expr, $rl:expr, $resp:expr) => {{
        // A Command Status event carries no return parameters.
        let _ = ($r, $rl);
        cmd_status($op, $st, $resp)
    }};
}

/// Generates `dispatch_command` from the opcode table below.
///
/// Each entry names the opcode, whether it answers with Command `complete` or
/// Command `status`, the SoftDevice function, and that function's parameter
/// (`p`) and return (`r`) structs. Both lengths are taken from those structs,
/// so the length the command is checked against is the number of bytes the
/// SoftDevice reads, and the response carries the number it writes. Naming the
/// wrong struct for a function does not compile.
macro_rules! hci_commands {
    (
        $( $opcode:literal $kind:tt $f:ident ( $($args:tt)* ) ; )*
    ) => {
        /// Dispatch one command and return the length of the event answering it.
        ///
        /// `p` points at `plen` readable bytes.
        unsafe fn dispatch_command(
            opcode: u16,
            p: *const u8,
            plen: usize,
            resp: &mut [u8; PACKET_MAX],
        ) -> usize {
            match opcode {
                $(
                    $opcode => {
                        // Parameters are read straight out of a byte buffer,
                        // and return values written into one.
                        const _: () = assert!(align_req!($($args)*) == 1);
                        #[allow(unused_comparisons)]
                        const _: () = assert!(ret_len!($($args)*) <= RET_MAX);

                        // The SoftDevice reads its struct's worth of bytes
                        // without consulting the length the host declared.
                        if plen != param_len!($($args)*) {
                            // Invalid HCI Command Parameters
                            return reply!($kind, opcode, 0x12, core::ptr::null::<u8>(), 0, resp);
                        }

                        let mut ret = [0u8; RET_MAX];
                        let r = ret.as_mut_ptr();
                        let status = invoke!($f, p, r, $($args)*);
                        // Return parameters are always present; their values
                        // are undefined when the status is non-zero.
                        reply!($kind, opcode, status, r, ret_len!($($args)*), resp)
                    }
                )*
                // Unknown HCI Command.
                _ => cmd_complete(opcode, 0x01, core::ptr::null(), 0, resp),
            }
        }
    };
}

hci_commands! {
    0x0c03 complete sdc_hci_cmd_cb_reset();
    0x0c01 complete sdc_hci_cmd_cb_set_event_mask(p: sdc_hci_cmd_cb_set_event_mask_t);
    0x0c63 complete sdc_hci_cmd_cb_set_event_mask_page_2(p: sdc_hci_cmd_cb_set_event_mask_page_2_t);
    0x1001 complete sdc_hci_cmd_ip_read_local_version_information(r: sdc_hci_cmd_ip_read_local_version_information_return_t);
    0x1002 complete read_local_supported_commands(r: sdc_hci_cmd_ip_read_local_supported_commands_return_t);
    0x1003 complete sdc_hci_cmd_ip_read_local_supported_features(r: sdc_hci_cmd_ip_read_local_supported_features_return_t);
    0x1009 complete sdc_hci_cmd_ip_read_bd_addr(r: sdc_hci_cmd_ip_read_bd_addr_return_t);
    0x2001 complete sdc_hci_cmd_le_set_event_mask(p: sdc_hci_cmd_le_set_event_mask_t);
    0x2002 complete sdc_hci_cmd_le_read_buffer_size(r: sdc_hci_cmd_le_read_buffer_size_return_t);
    0x2003 complete sdc_hci_cmd_le_read_local_supported_features(r: sdc_hci_cmd_le_read_local_supported_features_return_t);
    0x2005 complete sdc_hci_cmd_le_set_random_address(p: sdc_hci_cmd_le_set_random_address_t);
    0x2006 complete sdc_hci_cmd_le_set_adv_params(p: sdc_hci_cmd_le_set_adv_params_t);
    0x2007 complete sdc_hci_cmd_le_read_adv_physical_channel_tx_power(r: sdc_hci_cmd_le_read_adv_physical_channel_tx_power_return_t);
    0x2008 complete sdc_hci_cmd_le_set_adv_data(p: sdc_hci_cmd_le_set_adv_data_t);
    0x2009 complete sdc_hci_cmd_le_set_scan_response_data(p: sdc_hci_cmd_le_set_scan_response_data_t);
    0x200a complete sdc_hci_cmd_le_set_adv_enable(p: sdc_hci_cmd_le_set_adv_enable_t);
    0x2015 complete sdc_hci_cmd_le_read_channel_map(p: sdc_hci_cmd_le_read_channel_map_t, r: sdc_hci_cmd_le_read_channel_map_return_t);
    0x2017 complete sdc_hci_cmd_le_encrypt(p: sdc_hci_cmd_le_encrypt_t, r: sdc_hci_cmd_le_encrypt_return_t);
    0x2018 complete sdc_hci_cmd_le_rand(r: sdc_hci_cmd_le_rand_return_t);
    0x201a complete sdc_hci_cmd_le_long_term_key_request_reply(p: sdc_hci_cmd_le_long_term_key_request_reply_t, r: sdc_hci_cmd_le_long_term_key_request_reply_return_t);
    0x201b complete sdc_hci_cmd_le_long_term_key_request_negative_reply(p: sdc_hci_cmd_le_long_term_key_request_negative_reply_t, r: sdc_hci_cmd_le_long_term_key_request_negative_reply_return_t);
    0x2022 complete sdc_hci_cmd_le_set_data_length(p: sdc_hci_cmd_le_set_data_length_t, r: sdc_hci_cmd_le_set_data_length_return_t);
    0x2023 complete sdc_hci_cmd_le_read_suggested_default_data_length(r: sdc_hci_cmd_le_read_suggested_default_data_length_return_t);
    0x2024 complete sdc_hci_cmd_le_write_suggested_default_data_length(p: sdc_hci_cmd_le_write_suggested_default_data_length_t);
    0x202f complete sdc_hci_cmd_le_read_max_data_length(r: sdc_hci_cmd_le_read_max_data_length_return_t);
    0x2030 complete sdc_hci_cmd_le_read_phy(p: sdc_hci_cmd_le_read_phy_t, r: sdc_hci_cmd_le_read_phy_return_t);
    0x2031 complete sdc_hci_cmd_le_set_default_phy(p: sdc_hci_cmd_le_set_default_phy_t);
    0x204b complete sdc_hci_cmd_le_read_transmit_power(r: sdc_hci_cmd_le_read_transmit_power_return_t);

    0x0406 status sdc_hci_cmd_lc_disconnect(p: sdc_hci_cmd_lc_disconnect_t);
    0x041d status sdc_hci_cmd_lc_read_remote_version_information(p: sdc_hci_cmd_lc_read_remote_version_information_t);
    0x2016 status sdc_hci_cmd_le_read_remote_features(p: sdc_hci_cmd_le_read_remote_features_t);
    0x2032 status sdc_hci_cmd_le_set_phy(p: sdc_hci_cmd_le_set_phy_t);
}
