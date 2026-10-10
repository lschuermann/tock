/* Licensed under the Apache License, Version 2.0 or the MIT License. */
/* SPDX-License-Identifier: Apache-2.0 OR MIT */
/* Copyright Tock Contributors 2026. */

/* The nrfxlib interfaces `make bindings` generates src/bindings.rs from. */

#include "mpsl.h"
#include "mpsl_clock.h"

#include "sdc.h"
#include "sdc_hci.h"
#include "sdc_hci_cmd_controller_baseband.h"
#include "sdc_hci_cmd_info_params.h"
#include "sdc_hci_cmd_le.h"
#include "sdc_hci_cmd_link_control.h"
#include "sdc_hci_cmd_status_params.h"
#include "sdc_soc.h"
