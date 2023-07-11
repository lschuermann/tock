// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2022.

use crate::top_earlgrey::{TOP_EARLGREY_SPI_HOST0_BASE_ADDR, TOP_EARLGREY_SPI_HOST1_BASE_ADDR};
use kernel::utilities::StaticRef;
use lowrisc::spi_host::SpiHostRegisters;

//Refer: https://github.com/lowRISC/opentitan/blob/c4f342b9349ba033a5f22fba9349999299a1b2bf/hw/top_earlgrey/sw/autogen/top_earlgrey_memory.h#L179
pub const SPIHOST0_BASE: StaticRef<SpiHostRegisters> =
    unsafe { StaticRef::new(TOP_EARLGREY_SPI_HOST0_BASE_ADDR as *const SpiHostRegisters) };
//Refer: https://github.com/lowRISC/opentitan/blob/c4f342b9349ba033a5f22fba9349999299a1b2bf/hw/top_earlgrey/sw/autogen/top_earlgrey_memory.h#L184
pub const SPIHOST1_BASE: StaticRef<SpiHostRegisters> =
    unsafe { StaticRef::new(TOP_EARLGREY_SPI_HOST1_BASE_ADDR as *const SpiHostRegisters) };
