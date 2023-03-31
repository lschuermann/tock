#![deny(missing_docs)]
//! STM32F4xx clock driver
//!
//! This crate provides drivers for various clocks: HSI, PLL, system, AHB, APB1 and APB2.
//! This documentation applies to the system clock, AHB, APB1 and APB2. For in-detail documentation
//! for HSI and PLL, check their documentation.
//!
//! # Features
//!
//! - [x] Dynamic system source
//! - [x] Hardware limits verification for AHB, APB1 and APB2.
//! - [x] Prescaler configuration for AHB, APB1 and APB2.
//!
//! # Limitations
//!
//! - [ ] Precision of 1MHz
//! - [ ] No support for MCO
//!
//! # Usage [^usage_note]
//!
//! First, import the following enums:
//!
//! ```rust,ignore
//! // Assuming a STM32F429 chip. Change this to correspond to the chip model.
//! use stm32f429zi::rcc::APBPrescaler;
//! use stm32f429zi::rcc::AHBPrescaler;
//! use stm32f429zi::rcc::SysClockSource;
//! ```
//!
//! A reference to the [crate::clocks::Clocks] is needed:
//!
//! ```rust,ignore
//! // Add this in board main.rs
//! let clocks = &peripherals.stm32f4.clocks;
//! ```
//!
//! ## Retrieve the AHB frequency:
//!
//! ```rust,ignore
//! let ahb_frequency = clocks.get_ahb_frequency();
//! debug!("Current AHB frequency is {}MHz", ahb_frequency);
//! ```
//!
//! ## Retrieve the AHB prescaler:
//!
//! ```rust,ignore
//! let ahb_prescaler = clocks.get_ahb_prescaler();
//! debug!("Current AHB prescaler is {:?}", ahb_prescaler);
//! ```
//!
//! NOTE: If one wishes to get the usize equivalent value of [crate::clocks::Clocks::get_ahb_prescaler], to use in
//! computations for example, they must use [crate::rcc::AHBPrescaler].into() method:
//!
//! ```rust,ignore
//! let ahb_prescaler_usize: usize = clocks.get_ahb_prescaler().into();
//! if ahb_prescaler_usize > 8 {
//!     /* Do something */
//! }
//! ```
//!
//! ## Set the AHB prescaler
//!
//! ```rust,ignore
//! clocks.set_ahb_prescaler(AHBPrescaler::DivideBy4);
//! ```
//!
//! ## APB1 and APB2 prescalers
//!
//! APB1 and APB2 prescalers are configured in a similar way as AHB prescaler, except that the
//! corresponding enum is APBPrescaler.
//!
//! ## Retrieve the system clock frequency:
//!
//! ```rust,ignore
//! let sys_frequency = clocks.get_sys_clock_frequency();
//! debug!("Current system clock frequency is {}MHz", sys_frequency);
//! ```
//!
//! ## Retrieve the system clock source:
//!
//! ```rust,ignore
//! let sys_source = clocks.get_sys_clock_source();
//! debug!("Current system clock source is {:?}", sys_source);
//! ```
//!
//! ## Change the system clock source to PLL:
//!
//! Changing the system clock source is a fastidious task because of AHB, APB1 and APB2 limits,
//! which are chip-dependent. This example assumes a STM32F429 chip.
//!
//! First, get a reference to the PLL
//!
//! ```rust,ignore
//! let pll = &peripherals.stm32f4.clocks.pll;
//! ```
//!
//! Then, configure its frequency and enable it
//! ```rust,ignore
//! pll.set_frequency(50);
//! pll.enable();
//! ```
//!
//! STM32F429 maximum APB1 frequency is 45MHz, which is computed as following:
//! freq_APB1 = freq_sys / AHB_prescaler / APB1_prescaler
//! Default prescaler values are 1, which gives an frequency of 50MHz without modifying the
//! APB1 prescaler. As such, the APB1 prescaler must be changed.
//!
//! ```rust,ignore
//! clocks.set_apb1_prescaler(APBPrescaler::DivideBy2);
//! ```
//!
//! Since the APB1 frequency limit is satisfied now, the system clock source can be safely changed.
//!
//! ```rust,ignore
//! clocks.set_sys_clock_source(SysClockSource::PLLCLK);
//! ```
//!
//! ## Another example of changing the system clock to PLL for STM32F429:
//!
//! As before, Pll clock is configured and enabled.
//!
//! ```rust,ignore
//! pll.set_frequency(100);
//! pll.enable();
//! ```
//!
//! Because of the high frequency of the PLL clock, both APB1 and APB2 prescalers must be
//! configured.
//!
//! ```rust,ignore
//! clocks.set_apb1_prescaler(APBPrescaler::DivideBy4);
//! clocks.set_apb2_prescaler(APBPrescaler::DivideBy2);
//! ```
//!
//! As an alternative, the AHB prescaler could be configured to change both APB1 and APB2
//! frequencies.
//!
//! ```rust,ignore
//! // Changing it to 2 wouldn't work, because it would give a frequency of 50MHz for the APB1.
//! clocks.set_ahb_prescaler(APBPrescaler::DivideBy4);
//! ```
//!
//! Now, it's safe to change the system clock source:
//!
//! ```rust,ignore
//! clocks.set_sys_clock_source(SysClockSource::PLLCLK);
//! ```
//!
//! [^usage_note]: For the purpose of brievity, any error checking has been removed.

pub mod hsi;
pub mod pll;

use pll::Pll;
use hsi::Hsi;
use hsi::HSI_FREQUENCY_MHZ;
use crate::rcc::Rcc;
use crate::rcc::SysClockSource;
use crate::rcc::APBPrescaler;
use crate::rcc::AHBPrescaler;
use crate::flash::Flash;

use kernel::debug;
use kernel::ErrorCode;
use kernel::utilities::cells::OptionalCell;

/// Main struct for configuring on-board clocks.
pub struct Clocks<'a> {
    rcc: &'a Rcc,
    flash: OptionalCell<&'a Flash>,
    /// High speed internal clock
    pub hsi: Hsi<'a>,
    /// Main phase loop-lock clock
    pub pll: Pll<'a>,
}

#[cfg(any(
    feature = "stm32f410",
    feature = "stm32f411",
    feature = "stm32f412",
    feature = "stm32f413",
    feature = "stm32f423"
))]
const APB1_FREQUENCY_LIMIT_MHZ: usize = 50;

#[cfg(any(
    feature = "stm32f427",
    feature = "stm32f429",
    feature = "stm32f437",
    feature = "stm32f439",
    feature = "stm32f446",
    feature = "stm32f469",
    feature = "stm32f479"
))]
const APB1_FREQUENCY_LIMIT_MHZ: usize = 45;

#[cfg(any(
    feature = "stm32f401",
    feature = "stm32f405",
    feature = "stm32f407",
    feature = "stm32f415",
    feature = "stm32f417",
))]
const APB1_FREQUENCY_LIMIT_MHZ: usize = 42;

// APB2 frequency limit is twice the APB1 frequency limit
const APB2_FREQUENCY_LIMIT_MHZ: usize = APB1_FREQUENCY_LIMIT_MHZ << 1;

#[cfg(any(
    feature = "stm32f410",
    feature = "stm32f411",
    feature = "stm32f412",
    feature = "stm32f413",
    feature = "stm32f423"
))]
const SYS_CLOCK_FREQUENCY_LIMIT_MHZ: usize = 100;

// TODO: Some of these models support overdrive model. Change this constant when overdrive support
// is added.
#[cfg(any(
    feature = "stm32f405",
    feature = "stm32f407",
    feature = "stm32f415",
    feature = "stm32f417",
    feature = "stm32f427",
    feature = "stm32f429",
    feature = "stm32f437",
    feature = "stm32f439",
    feature = "stm32f446",
    feature = "stm32f469",
    feature = "stm32f479"
))]
const SYS_CLOCK_FREQUENCY_LIMIT_MHZ: usize = 168;

#[cfg(any(
    feature = "stm32f401",
))]
const SYS_CLOCK_FREQUENCY_LIMIT_MHZ: usize = 84;

impl<'a> Clocks<'a> {
    // The constructor must be called when the default peripherals are created
    pub(crate) fn new(rcc: &'a Rcc) -> Self {
        Self {
            rcc,
            flash: OptionalCell::empty(),
            hsi: Hsi::new(rcc),
            pll: Pll::new(rcc),
        }
    }

    // This method should be called when the dependencies are resolved
    pub(crate) fn set_flash(&self, flash: &'a Flash) {
        self.flash.set(flash);
    }

    /// Set the AHB prescaler
    ///
    /// AHB bus, core, memory, DMA, Cortex System timer and FCLK Cortex free-running clock
    /// frequencies are equal to the system clock frequency divided by the AHB prescaler.
    ///
    /// # Errors:
    ///
    /// + [Err]\([ErrorCode::FAIL]\) if changing the AHB prescaler doesn't preserve APB frequency
    /// constraints
    /// + [Err]\([ErrorCode::BUSY]\) if changing the AHB prescaler took too long. Retry.
    pub fn set_ahb_prescaler(&self, prescaler: AHBPrescaler) -> Result<(), ErrorCode> {
        // Changing the AHB prescaler affects the APB frequencies. A check must be done to ensure
        // that the constraints are still valid
        let divider: usize = prescaler.into();
        let new_ahb_frequency = self.get_sys_clock_frequency() / divider;
        if !self.check_apb1_frequency_limit(new_ahb_frequency) || !self.check_apb2_frequency_limit(new_ahb_frequency) {
            return Err(ErrorCode::FAIL);
        }

        self.rcc.set_ahb_prescaler(prescaler);

        for _ in 0..16 {
            if self.get_ahb_prescaler() == prescaler {
                return Ok(());
            }
        }

        Err(ErrorCode::BUSY)
    }

    /// Get the current configured AHB prescaler
    pub fn get_ahb_prescaler(&self) -> AHBPrescaler {
        self.rcc.get_ahb_prescaler()
    }

    /// Get the frequency of the AHB
    pub fn get_ahb_frequency(&self) -> usize {
        let ahb_divider: usize = self.get_ahb_prescaler().into();
        self.get_sys_clock_frequency() / ahb_divider
    }

    // APB1 frequency must not be higher than the maximum allowable frequency. This method is
    // called when the system clock source is changed. The sys_clk_frequency_mhz is the
    // hypothetical future frequency.
    fn check_apb1_frequency_limit(&self, sys_clk_frequency_mhz: usize) -> bool {
        match self.rcc.get_apb1_prescaler()  {
            APBPrescaler::DivideBy1 => sys_clk_frequency_mhz <= APB1_FREQUENCY_LIMIT_MHZ,
            APBPrescaler::DivideBy2 => sys_clk_frequency_mhz <= APB1_FREQUENCY_LIMIT_MHZ * 2,
            // For all models 4 * APB1_MAX_FREQUENCY >= SYS_MAX_FREQUENCY
            _ => true,
        }
    }

    /// Set the APB1 prescaler.
    ///
    /// The APB1 peripheral clock frequency is equal to the AHB frequency divided by the APB1
    /// prescaler.
    ///
    /// # Errors:
    ///
    /// + [Err]\([ErrorCode::FAIL]\) if the desired prescaler would break the APB1 frequency limit
    /// + [Err]\([ErrorCode::BUSY]\) if setting the prescaler took too long. Retry.
    pub fn set_apb1_prescaler(&self, prescaler: APBPrescaler) -> Result<(), ErrorCode> {
        let ahb_frequency = self.get_ahb_frequency();
        let divider: usize = prescaler.into();
        if ahb_frequency / divider > APB1_FREQUENCY_LIMIT_MHZ {
            return Err(ErrorCode::FAIL);
        }

        self.rcc.set_apb1_prescaler(prescaler);

        for _ in 0..16 {
            if self.rcc.get_apb1_prescaler() == prescaler {
                return Ok(());
            }
        }

        Err(ErrorCode::BUSY)
    }

    /// Get the current configured APB1 prescaler
    pub fn get_apb1_prescaler(&self) -> APBPrescaler {
        self.rcc.get_apb1_prescaler()
    }

    /// Get the current APB1 frequency
    pub fn get_apb1_frequency(&self) -> usize {
        // Every enum variant can be converted into a usize
        let divider: usize = self.rcc.get_apb1_prescaler().try_into().unwrap();
        self.get_ahb_frequency() / divider
    }

    // Same as for APB1, APB2 has a frequency limit that must be enforced by software
    fn check_apb2_frequency_limit(&self, sys_clk_frequency_mhz: usize) -> bool {
        match self.rcc.get_apb2_prescaler() {
            APBPrescaler::DivideBy1 => sys_clk_frequency_mhz <= APB2_FREQUENCY_LIMIT_MHZ,
            // For all models 2 * APB2_MAX_FREQUENCY >= SYS_MAX_FREQUENCY
            _ => true,
        }
    }

    /// Set the APB2 prescaler.
    ///
    /// The APB2 peripheral clock frequency is equal to the AHB frequency divided by the APB2
    /// prescaler.
    ///
    /// # Errors:
    ///
    /// + [Err]\([ErrorCode::FAIL]\) if the desired prescaler would break the APB2 frequency limit
    /// + [Err]\([ErrorCode::BUSY]\) if setting the prescaler took too long. Retry.
    pub fn set_apb2_prescaler(&self, prescaler: APBPrescaler) -> Result<(), ErrorCode> {
        let current_ahb_frequency = self.get_ahb_frequency();
        let divider: usize = prescaler.into();
        if current_ahb_frequency / divider > APB2_FREQUENCY_LIMIT_MHZ {
            return Err(ErrorCode::FAIL);
        }

        self.rcc.set_apb2_prescaler(prescaler);

        for _ in 0..16 {
            if self.rcc.get_apb2_prescaler() == prescaler {
                return  Ok(());
            }
        }

        Err(ErrorCode::BUSY)
    }

    /// Get the current configured APB2 prescaler
    pub fn get_apb2_prescaler(&self) -> APBPrescaler {
        self.rcc.get_apb2_prescaler()
    }

    /// Get the current APB2 frequency
    pub fn get_apb2_frequency(&self) -> usize {
        // Every enum variant can be converted into a usize
        let divider: usize = self.rcc.get_apb2_prescaler().try_into().unwrap();
        self.get_ahb_frequency() / divider
    }

    /// Set the system clock source
    ///
    /// # Errors:
    ///
    /// + [Err]\([ErrorCode::FAIL]\) if the source is not enabled.
    /// + [Err]\([ErrorCode::SIZE]\) if the source frequency surpasses the system clock frequency
    /// limit, or the APB1 and APB2 limits are not satisfied.
    /// + [Err]\([ErrorCode::BUSY]\) if the source switching took too long. Retry.
    pub fn set_sys_clock_source(&self, source: SysClockSource) -> Result<(), ErrorCode> {
        // Immediatelly return if the required source is already configured as the system clock
        // source. Should this maybe be Err(ErrorCode::ALREADY)?
        if source == self.get_sys_clock_source() {
            return Ok(());
        }

        // Ensure the source is enabled before configuring it as the system clock source
        if let false = match source {
            SysClockSource::HSI => self.hsi.is_enabled(),
            SysClockSource::PLLCLK => self.pll.is_enabled(),
        } {
            return Err(ErrorCode::FAIL);
        }

        let current_frequency = self.get_sys_clock_frequency();

        // Get the frequency of the source to be configured
        let alternate_frequency = match source {
            // The unwrap can't failed because the source clock status was checked before
            SysClockSource::HSI => self.hsi.get_frequency().unwrap(),
            SysClockSource::PLLCLK => self.pll.get_frequency().unwrap(),
        };

        // HELP: Confusing point. The PLL clock can output a frequency up to 216MHz, but the doc
        // warns that the output must not surpass system clock frequency limit. My assumption is
        // that if the PLL clock is used as a system clock source, then its frequency has to be
        // checked. Otherwise, if it is used as a microcontroller clock output (MCO), the entire
        // frequency range is available.
        //
        // Check the alternate frequency is not higher than the system clock limit
        if alternate_frequency > SYS_CLOCK_FREQUENCY_LIMIT_MHZ {
            return Err(ErrorCode::SIZE);
        }

        // Retrieve the currently configured AHB prescaler
        let ahb_divider: usize = self.get_ahb_prescaler().into();
        // Compute the possible future AHB frequency
        let ahb_frequency = alternate_frequency / ahb_divider;

        // APB1 frequency must not exceed APB1_FREQUENCY_LIMIT_MHZ
        if let false = self.check_apb1_frequency_limit(ahb_frequency) {
            return Err(ErrorCode::SIZE);
        }

        // APB2 frequency must not exceed APB2_FREQUENCY_LIMIT_MHZ
        if let false = self.check_apb2_frequency_limit(ahb_frequency) {
            return Err(ErrorCode::SIZE);
        }

        // The documentation recommends the following sequence when changing the system clock
        // frequency:
        //
        // + if the desired frequency is higher than the current frequency, first change flash
        // latency, then set the new system clock source.
        // + if the desired frequency is lower than the current frequency, first change the system
        // clock source, then set the flash latency
        if alternate_frequency > current_frequency {
            self.flash.unwrap_or_panic().set_latency(alternate_frequency)?;
        }
        self.rcc.set_sys_clock_source(source);
        if alternate_frequency < current_frequency {
            self.flash.unwrap_or_panic().set_latency(alternate_frequency)?;
        }

        // If this point is reached, everything worked as expected
        Ok(())
    }

    /// Get the current system clock source
    pub fn get_sys_clock_source(&self) -> SysClockSource {
        self.rcc.get_sys_clock_source()
    }

    /// Get the current system clock frequency
    pub fn get_sys_clock_frequency(&self) -> usize {
        match self.get_sys_clock_source() {
            // These unwraps can't panic because set_sys_clock_frequency ensures that the source is
            // enabled. Also, Hsi and Pll structs ensure that the clocks can't be disabled when
            // they are configured as the system clock
            SysClockSource::HSI => self.hsi.get_frequency().unwrap(),
            SysClockSource::PLLCLK => self.pll.get_frequency().unwrap(),
        }
    }
}

/// Tests for clocks functionalities
///
/// Run these tests to ensure that system clock and AHB, APB1 and APB2 prescalers work as expected.
/// To test HSI or PLL, see their documentation.
pub mod tests {
    use super::*;

    const LOW_FREQUENCY: usize = 25;
    #[cfg(not(any(
        feature = "stm32f401",
        feature = "stm32f410",
        feature = "stm32f411",
        feature = "stm32f412",
        feature = "stm32f413",
        feature = "stm32f423"
    )))]
    const HIGH_FREQUENCY: usize = 112;
    #[cfg(any(
        feature = "stm32f401",
        feature = "stm32f410",
        feature = "stm32f411",
        feature = "stm32f412",
        feature = "stm32f413",
        feature = "stm32f423"
    ))]
    const HIGH_FREQUENCY: usize = 80;

    fn set_default_configuration(clocks: &Clocks) {
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::HSI));
        assert_eq!(Ok(()), clocks.pll.disable());
        assert_eq!(Ok(()), clocks.set_ahb_prescaler(AHBPrescaler::DivideBy1));
        assert_eq!(Ok(()), clocks.set_apb1_prescaler(APBPrescaler::DivideBy1));
        assert_eq!(Ok(()), clocks.set_apb2_prescaler(APBPrescaler::DivideBy1));
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_sys_clock_frequency());
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_apb1_frequency());
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_apb2_frequency());
    }

    fn test_prescalers(clocks: &Clocks) {
        // This test requires a bit of setup. A system clock running at 160MHz is configured.
        assert_eq!(Ok(()), clocks.pll.set_frequency(HIGH_FREQUENCY));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Ok(()), clocks.set_apb1_prescaler(APBPrescaler::DivideBy4));
        assert_eq!(Ok(()), clocks.set_apb2_prescaler(APBPrescaler::DivideBy2));
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Trying to reduce the APB scaler to an invalid value should fail
        assert_eq!(Err(ErrorCode::FAIL), clocks.set_apb1_prescaler(APBPrescaler::DivideBy1));
        // The following assert will pass on these models because of the low system clock
        // frequency limit
        #[cfg(not(any(
            feature = "stm32f401",
            feature = "stm32f410",
            feature = "stm32f411",
            feature = "stm32f412",
            feature = "stm32f413",
            feature = "stm32f423"
        )))]
        assert_eq!(Err(ErrorCode::FAIL), clocks.set_apb2_prescaler(APBPrescaler::DivideBy1));

        // Increasing the AHB prescaler should allow decreasing APB prescalers
        assert_eq!(Ok(()), clocks.set_ahb_prescaler(AHBPrescaler::DivideBy4));
        assert_eq!(Ok(()), clocks.set_apb1_prescaler(APBPrescaler::DivideBy1));
        assert_eq!(Ok(()), clocks.set_apb2_prescaler(APBPrescaler::DivideBy1));

        // Now, decreasing the AHB prescaler would result in the violation of APB constraints
        assert_eq!(Err(ErrorCode::FAIL), clocks.set_ahb_prescaler(AHBPrescaler::DivideBy1));

        // Revert to default configuration
        set_default_configuration(clocks);
    }

    #[cfg(any(
        feature = "stm32f401",
        feature = "stm32f410",
        feature = "stm32f411",
        feature = "stm32f412",
        feature = "stm32f413",
        feature = "stm32f423"
    ))]
    /// Test for the [crate::clk::clocks::Clocks] struct
    pub fn test_clocks_struct(clocks: &Clocks) {
        debug!("");
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        debug!("Testing clocks struct...");

        // By default, the HSI clock is the system clock
        assert_eq!(SysClockSource::HSI, clocks.get_sys_clock_source());

        // HSI frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_sys_clock_frequency());

        // AHB default prescaler is 1
        assert_eq!(AHBPrescaler::DivideBy1, clocks.get_ahb_prescaler());

        // AHB default frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_ahb_frequency());

        // APB1 default prescaler is 1
        assert_eq!(APBPrescaler::DivideBy1, clocks.get_apb1_prescaler());

        // APB1 default frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_apb1_frequency());

        // APB2 default prescaler is 1
        assert_eq!(APBPrescaler::DivideBy1, clocks.get_apb1_prescaler());

        // APB2 default frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_apb2_frequency());

        // Attempting to change the system clock source with a disabled source
        assert_eq!(Err(ErrorCode::FAIL), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Attempting to set twice the same system clock source is fine
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::HSI));

        // Change the system clock source to a low frequency so that APB prescalers don't need to be
        // changed
        assert_eq!(Ok(()), clocks.pll.set_frequency(LOW_FREQUENCY));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::PLLCLK));
        assert_eq!(SysClockSource::PLLCLK, clocks.get_sys_clock_source());

        // Now the system clock frequency is equal to 25MHz
        assert_eq!(LOW_FREQUENCY, clocks.get_sys_clock_frequency());

        // APB1 and APB2 frequencies must also be 25MHz
        assert_eq!(LOW_FREQUENCY, clocks.get_apb1_frequency());
        assert_eq!(LOW_FREQUENCY, clocks.get_apb2_frequency());

        // Attempting to disable PLL when it is configured as the system clock must fail
        assert_eq!(Err(ErrorCode::FAIL), clocks.pll.disable());
        // Same for the HSI since it is used indirectly as a system clock through PLL
        assert_eq!(Err(ErrorCode::FAIL), clocks.hsi.disable());

        // Revert to default system clock configuration
        set_default_configuration(clocks);

        // Trying to configure a high frequency for the system clock without configuring the APB1
        // prescaler must fail
        assert_eq!(Ok(()), clocks.pll.set_frequency(HIGH_FREQUENCY));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Err(ErrorCode::SIZE), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Configuring APB1 prescaler to 4
        assert_eq!(Ok(()), clocks.set_apb1_prescaler(APBPrescaler::DivideBy4));

        // Now, PLL can be set as the system clock source
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Configuring APB2 prescaler to 2
        assert_eq!(Ok(()), clocks.set_apb2_prescaler(APBPrescaler::DivideBy2));

        // Check new AHB frequency
        assert_eq!(HIGH_FREQUENCY, clocks.get_ahb_frequency());

        // Check new APB frequencies
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_apb1_frequency());
        assert_eq!(HIGH_FREQUENCY / 2, clocks.get_apb2_frequency());

        // Revert to default system clock configuration
        set_default_configuration(clocks);

        // Doing the same thing as before, except that this time the AHB prescaler is configured
        // instead of individual APB prescalers
        assert_eq!(Ok(()), clocks.set_ahb_prescaler(AHBPrescaler::DivideBy4));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::HSI));
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_ahb_frequency());
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_apb1_frequency());
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_apb1_frequency());


        // Revert to default configuration
        set_default_configuration(clocks);

        debug!("Finished testing clocks struct. Everything is alright!");
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        debug!("");
    }

    #[cfg(not(any(
        feature = "stm32f401",
        feature = "stm32f410",
        feature = "stm32f411",
        feature = "stm32f412",
        feature = "stm32f413",
        feature = "stm32f423"
    )))]
    /// Test for the [crate::clocks::Clocks] struct
    pub fn test_clocks_struct(clocks: &Clocks) {
        debug!("");
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        debug!("Testing clocks struct...");

        // By default, the HSI clock is the system clock
        assert_eq!(SysClockSource::HSI, clocks.get_sys_clock_source());

        // HSI frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_sys_clock_frequency());

        // APB1 default prescaler is 1
        assert_eq!(APBPrescaler::DivideBy1, clocks.get_apb1_prescaler());

        // APB1 default frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_apb1_frequency());

        // APB2 default prescaler is 1
        assert_eq!(APBPrescaler::DivideBy1, clocks.get_apb1_prescaler());

        // APB2 default frequency is 16MHz
        assert_eq!(HSI_FREQUENCY_MHZ, clocks.get_apb2_frequency());

        // Attempting to change the system clock source with a disabled source
        assert_eq!(Err(ErrorCode::FAIL), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Attempting to set twice the same system clock source is fine
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::HSI));

        // Change the system clock source to a low frequency so that APB prescalers don't need to be
        // changed
        assert_eq!(Ok(()), clocks.pll.set_frequency(LOW_FREQUENCY));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::PLLCLK));
        assert_eq!(SysClockSource::PLLCLK, clocks.get_sys_clock_source());

        // Now the system clock frequency is equal to 25MHz
        assert_eq!(LOW_FREQUENCY, clocks.get_sys_clock_frequency());

        // APB1 and APB2 frequencies must also be 25MHz
        assert_eq!(LOW_FREQUENCY, clocks.get_apb1_frequency());
        assert_eq!(LOW_FREQUENCY, clocks.get_apb2_frequency());

        // Attempting to disable PLL when it is configured as the system clock must fail
        assert_eq!(Err(ErrorCode::FAIL), clocks.pll.disable());
        // Same for the HSI since it is used indirectly as a system clock through PLL
        assert_eq!(Err(ErrorCode::FAIL), clocks.hsi.disable());

        // Revert to default system clock configuration
        set_default_configuration(clocks);

        // Attempting to change the system clock frequency without correctly configuring the APB1
        // prescaler (freq_APB1 <= APB1_FREQUENCY_LIMIT_MHZ) and APB2 prescaler
        // (freq_APB2 <= APB2_FREQUENCY_LIMIT_MHZ) must fail
        assert_eq!(Ok(()), clocks.pll.disable());
        assert_eq!(Ok(()), clocks.pll.set_frequency(HIGH_FREQUENCY));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Err(ErrorCode::SIZE), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Even if the APB1 prescaler is changed to 2, it must fail
        // (HIGH_FREQUENCY / 2 > APB1_FREQUENCY_LIMIT_MHZ)
        assert_eq!(Ok(()), clocks.set_apb1_prescaler(APBPrescaler::DivideBy2));
        assert_eq!(Err(ErrorCode::SIZE), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Configuring APB1 prescaler to 4 is fine, but APB2 prescaler is still wrong
        assert_eq!(Ok(()), clocks.set_apb1_prescaler(APBPrescaler::DivideBy4));
        assert_eq!(Err(ErrorCode::SIZE), clocks.set_sys_clock_source(SysClockSource::PLLCLK));

        // Configuring APB2 prescaler to 2
        assert_eq!(Ok(()), clocks.set_apb2_prescaler(APBPrescaler::DivideBy2));

        // Now the system clock source can be changed
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::PLLCLK));
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_apb1_frequency());
        assert_eq!(HIGH_FREQUENCY / 2, clocks.get_apb2_frequency());

        // Revert to default system clock configuration
        set_default_configuration(clocks);

        // This time, configure the AHB prescaler instead of APB prescalers
        assert_eq!(Ok(()), clocks.set_ahb_prescaler(AHBPrescaler::DivideBy4));
        assert_eq!(Ok(()), clocks.pll.enable());
        assert_eq!(Ok(()), clocks.set_sys_clock_source(SysClockSource::PLLCLK));
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_ahb_frequency());
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_apb1_frequency());
        assert_eq!(HIGH_FREQUENCY / 4, clocks.get_apb2_frequency());

        // Revert to default configuration
        set_default_configuration(clocks);

        debug!("Finished testing clocks struct. Everything is alright!");
        debug!("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        debug!("");
    }

    /// Run the entire test suite
    pub fn run_all(clocks: &Clocks) {
        debug!("");
        debug!("===============================================");
        debug!("Testing clocks...");

        hsi::tests::run(&clocks.hsi);
        pll::tests::run(&clocks.pll);
        test_clocks_struct(clocks);
        test_prescalers(clocks);

        debug!("Finished testing clocks. Everything is alright!");
        debug!("===============================================");
        debug!("");
    }
}
