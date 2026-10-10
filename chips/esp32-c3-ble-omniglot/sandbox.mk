# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2026.

# Builds the sandbox of `esp32-c3-ble-omniglot`: Espressif's BLE controller
# (the mask ROM's link layer plus the archives from tock-firmware's
# esp32c3-bt crate) and its C glue in `sandbox/`, linked to run at fixed
# addresses. A board includes this after `Makefile.common` and writes
# `$(BLE_SANDBOX_BIN)` into its kernel binary at the address its layout.ld
# reserves for it.

BLE_OMNIGLOT_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
BLE_SANDBOX_SRC := $(BLE_OMNIGLOT_DIR)sandbox
BLE_SANDBOX_OUT := $(TARGET_PATH)/release/ble-omniglot-sandbox
BLE_SANDBOX_ELF := $(BLE_SANDBOX_OUT)/sandbox.elf
BLE_SANDBOX_BIN := $(BLE_SANDBOX_OUT)/sandbox.bin

# The controller's archives and ROM linker scripts, from the
# tock-firmware-esp32c3-bt crate, a Cargo dependency of
# `esp32-c3-ble-omniglot`. Its manifest path is taken from the package entry
# in `cargo metadata`, which starts with its name and version; the first
# `manifest_path` after that is the package's own. Expanded only when the
# sandbox is built, and then once.
BLE_FIRMWARE_MANIFEST = $(eval BLE_FIRMWARE_MANIFEST := $$(shell \
  $(CARGO) metadata --format-version 1 \
  | grep -o '{"name":"tock-firmware-esp32c3-bt","version":.*' \
  | grep -o '"manifest_path":"[^"]*"' | head -n 1 | cut -d '"' -f 4))$(BLE_FIRMWARE_MANIFEST)
FIRMWARE_DIR ?= $(patsubst %/Cargo.toml,%,$(BLE_FIRMWARE_MANIFEST))/resources
BLE_FIRMWARE_MISSING = BLE controller firmware not found in "$(FIRMWARE_DIR)" \
  (tock-firmware-esp32c3-bt manifest from `cargo metadata`: "$(BLE_FIRMWARE_MANIFEST)")

# Bare-metal RISC-V GCC toolchain, as provided by Tock's `shell.nix`.
SANDBOX_PREFIX ?= riscv64-none-elf-
SANDBOX_CC     ?= $(SANDBOX_PREFIX)gcc
SANDBOX_CFLAGS := -march=rv32imc_zicsr -mabi=ilp32 -mcmodel=medlow -Os -g \
  -ffreestanding -nostdlib -ffunction-sections -fdata-sections \
  -fno-tree-loop-distribute-patterns -Wall -Wextra

# ROM entry points of the BLE controller, PHY and libgcc.
SANDBOX_ROM_LD := esp32c3.rom.ld esp32c3.rom.api.ld esp32c3.rom.eco3.ld \
  esp32c3.rom.libgcc.ld esp32c3.rom.version.ld esp32c3.rom.bt_funcs.ld \
  esp32c3.rom.eco3_bt_funcs.ld

$(BLE_SANDBOX_ELF): $(wildcard $(BLE_SANDBOX_SRC)/*)
	$(if $(wildcard $(FIRMWARE_DIR)/lib/libbtdm_app.a),,$(error $(BLE_FIRMWARE_MISSING)))
	@mkdir -p $(BLE_SANDBOX_OUT)
	$(SANDBOX_CC) $(SANDBOX_CFLAGS) -T$(BLE_SANDBOX_SRC)/layout.ld \
	  $(addprefix -T$(FIRMWARE_DIR)/ld/,$(SANDBOX_ROM_LD)) \
	  -Wl,--no-relax -Wl,--gc-sections \
	  -Wl,-Map=$(BLE_SANDBOX_OUT)/sandbox.map -o $@ \
	  $(filter %.c %.S,$(wildcard $(BLE_SANDBOX_SRC)/*)) \
	  -L$(FIRMWARE_DIR)/lib -Wl,--start-group -lbtdm_app -lbtbb -lphy -lcoexist -Wl,--end-group
	$(SANDBOX_PREFIX)size -A $@ | awk '$$2 > 0'

$(BLE_SANDBOX_BIN): $(BLE_SANDBOX_ELF)
	$(SANDBOX_PREFIX)objcopy --output-target=binary $< $@

.PHONY: ble-sandbox
ble-sandbox: $(BLE_SANDBOX_BIN)

# Regenerate the checked-in Omniglot bindings (`src/bindings.rs`) after
# changing `sandbox/sandbox.h` or `sandbox/sandbox.omniglot.toml`. Needs
# libclang.
.PHONY: ble-omniglot-bindings
ble-omniglot-bindings:
	cd $(TOCK_ROOT_DIRECTORY)tools && cargo run -p ble-omniglot-bindgen
