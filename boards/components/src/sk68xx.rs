// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2025.

//! Component for the sk68xx LED.

use core::mem::MaybeUninit;
use kernel::component::Component;
use kernel::hil;

#[macro_export]
macro_rules! sk68xx_component_static {
    ($P:ty $(,)?) => {
        kernel::static_buf!(capsules_extra::sk68xx::Sk68xx<'static, $P>)
    };
}

pub struct Sk68xxComponent<P: 'static + hil::gpio::Pin> {
    led_pin: &'static P,
    write_frame: fn(&P, u32),
}

impl<P: 'static + hil::gpio::Pin> Sk68xxComponent<P> {
    pub fn new(led_pin: &'static P, write_frame: fn(&P, u32)) -> Self {
        Self {
            led_pin,
            write_frame,
        }
    }
}

impl<P: 'static + hil::gpio::Pin> Component for Sk68xxComponent<P> {
    type StaticInput = &'static mut MaybeUninit<capsules_extra::sk68xx::Sk68xx<'static, P>>;
    type Output = &'static capsules_extra::sk68xx::Sk68xx<'static, P>;

    fn finalize(self, static_buffer: Self::StaticInput) -> Self::Output {
        let sk68xx = static_buffer.write(capsules_extra::sk68xx::Sk68xx::new(
            self.led_pin,
            self.write_frame,
        ));
        sk68xx.init();
        sk68xx
    }
}

#[macro_export]
macro_rules! sk68xx_led_component_static {
    ($P:ty $(,)?) => {
        kernel::static_buf!(capsules_extra::sk68xx::Sk68xxLed<'static, $P>)
    };
}

pub type Sk68xxLedComponentType<P> = capsules_extra::sk68xx::Sk68xxLed<'static, P>;

pub struct Sk68xxLedComponent<P: 'static + hil::gpio::Pin> {
    sk68xx: &'static capsules_extra::sk68xx::Sk68xx<'static, P>,
    index: usize,
}

impl<P: 'static + hil::gpio::Pin> Sk68xxLedComponent<P> {
    pub fn new(sk68xx: &'static capsules_extra::sk68xx::Sk68xx<'static, P>, index: usize) -> Self {
        Self { sk68xx, index }
    }
}

impl<P: 'static + hil::gpio::Pin> Component for Sk68xxLedComponent<P> {
    type StaticInput = &'static mut MaybeUninit<capsules_extra::sk68xx::Sk68xxLed<'static, P>>;
    type Output = &'static capsules_extra::sk68xx::Sk68xxLed<'static, P>;

    fn finalize(self, static_buffer: Self::StaticInput) -> Self::Output {
        let sk68xx_led = static_buffer.write(capsules_extra::sk68xx::Sk68xxLed::new(
            self.sk68xx,
            self.index,
        ));
        sk68xx_led
    }
}
