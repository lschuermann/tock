// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2022.

use core::panic::PanicInfo;

use kernel::debug;
use kernel::hil::gpio::ActivationMode;
use kernel::hil::uart;

use stm32f429zi::gpio::{PanicLed, PinId};
use stm32f429zi::usart::{UsartId, UsartPanicWriterConfig};

/// Panic handler.
#[panic_handler]
pub unsafe fn panic_fmt(info: &PanicInfo) -> ! {
    // User LD4 is connected to PG14
    let led = &PanicLed::new(PinId::PG14, ActivationMode::ActiveHigh);

    debug::panic::<_, stm32f429zi::usart::Usart<stm32f429zi::dma::Dma2>, _, _>(
        &mut [led],
        UsartPanicWriterConfig {
            usart: UsartId::Usart1,
            params: uart::Parameters {
                baud_rate: 115200,
                stop_bits: uart::StopBits::One,
                parity: uart::Parity::None,
                hw_flow_control: false,
                width: uart::Width::Eight,
            },
            hse_frequency_mhz: None,
        },
        info,
        &cortexm4::support::nop,
        crate::PANIC_RESOURCES.get(),
    )
}
