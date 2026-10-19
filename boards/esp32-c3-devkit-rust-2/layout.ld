/* Licensed under the Apache License, Version 2.0 or the MIT License. */
/* SPDX-License-Identifier: Apache-2.0 OR MIT                         */
/* Copyright Tock Contributors 2026.                                  */

/* The ESP32-C3 supports multiple differnet boot-modes, implemented in the
 * integrated ROM bootloader. For Tock, we configure it as follows:
 *
 * The kernel and apps execute in place from flash. We use the mask ROM's
 * direct boot mode by starting our binary image with two magic words
 * (`0xaedb041d`, `0xaedb041d`). This causes the mask ROM to map the external
 * SPI flash chip at address 0x42000000 and then jumps to 0x42000008.
 * The mask ROM reserves the top of SRAM for its own internal data (starting
 * from 0x3FCDF000); we are free to use any SRAM up until that address.
 *
 * The `.iram` section holds code that must not execute from flash. Instead,
 * this code is copied from flash into SRAM during early boot. The ESP32-C3
 * implements a modified harvard architecture where SRAM addresses cannot be
 * executed directly, but the CPU can execute instructions in SRAM at an alias
 * mapped at `SRAM_BASE (0x3FC8_0000) + 0x700000 = IRAM_BASE (0x4000_0000)`.
 * Similar to the kernel's `.data` section, we re-locate it on boot through a
 * loader stub that is linked at `.esp_direct_boot.entry` and thus executed as
 * the first function boot. This then chains the remaining, shared RISC-V start
 * assembly.
 *
 * The kernel stack is at the bottom of the `ram` region. The `iram` region is
 * at the top of the usable SRAM because a kernel stack overflow must not
 * overwrite code in the `iram` region.
 *
 * Espressif's BLE controller runs in an Omniglot sandbox, linked separately
 * (`chips/esp32-c3-ble-omniglot/sandbox/layout.ld`, which must match) and
 * merged into the kernel binary by `make`:
 *
 * - in `rom`, between the kernel and the kernel attributes at its end:
 *   `ble_load` (the sandbox's header and the load copies of its .iram1 and
 *   .data, read by the kernel) and `ble_flash` (its .text and .rodata, the
 *   sandbox's code window, a 256 KiB NAPOT PMP region);
 * - `ble_iram`: its .iram1 code, run from the bottom of SRAM through the
 *   instruction-bus alias (0x40380000 = 0x3FC80000 + 0x700000);
 * - `ble_ram`: its data, stacks and heap, directly below `iram`.
 *
 * Nothing of the kernel may execute from these regions, and the sandbox gets
 * no access to `iram`. libtock-c's ESP32-C3 targets (RAM at 0x3FCA0000..
 * 0x3FCD0000) still fit in `ram`. Without the sandbox, the kernel runs
 * without BLE.
 */

MEMORY
{
  rom  (rx)  : ORIGIN = 0x42000000, LENGTH = 0x00100000
  prog (rx)  : ORIGIN = 0x42100000, LENGTH = 0x00300000
  ram  (rw)  : ORIGIN = 0x3FC88000, LENGTH = 0x00048000
  iram (rx)  : ORIGIN = 0x403DEDB0, LENGTH = 0x00000250

  ble_iram (rx) : ORIGIN = 0x40380000, LENGTH = 0x8000
  ble_ram  (rw) : ORIGIN = 0x3FCD0000, LENGTH = 0xEDB0
}

/* Inside `rom`; nothing of the kernel link is placed there. */
_ble_load_start = 0x42070000;
_ble_load_end = 0x42080000;
_ble_flash_start = 0x42080000;
_ble_flash_end = 0x420C0000;
_ble_iram_start = ORIGIN(ble_iram);
_ble_iram_end = ORIGIN(ble_iram) + LENGTH(ble_iram);
_ble_ram_start = ORIGIN(ble_ram);
_ble_ram_end = ORIGIN(ble_ram) + LENGTH(ble_ram);

SECTIONS
{
  .esp_direct_boot :
  {
    LONG(0xaedb041d)
    LONG(0xaedb041d)
    KEEP(*(.esp_direct_boot.entry))
  } > rom
}

/* Align `.storage` (`kernel::storage_volume!`) to the 4 KiB sectors of the SPI
 * flash, the unit in which the flash erases. The board erases and writes its
 * storage volumes at run time, so no sector may also hold kernel code or
 * data. */
PAGE_SIZE = 4096;

/* We override Tock's default entry point (`initialize_ram_jump_to_main`) here,
 * as we need to first bootstrap the IRAM section. `esp32_c3_direct_boot_entry`
 * is run by the hardware and will in turn jump to Tock's entrypoint. */
ENTRY(esp32_c3_direct_boot_entry)

INCLUDE tock_kernel_layout.ld

SECTIONS
{
  .iram :
  {
    . = ALIGN(4);
    *(.iram .iram.*)
    . = ALIGN(4);
  } > iram AT> rom
}
INSERT AFTER .text;

/* The instruction bus is read-only. The `esp32_c3_direct_boot_entry` chip crate
 * entry stub writes the `.iram` section through the data bus. */
_siram_load = LOADADDR(.iram);
_siram_dbus = ADDR(.iram) - 0x700000;
_eiram_dbus = ADDR(.iram) + SIZEOF(.iram) - 0x700000;

/* On LLVM-LLD, the first ENTRY wins (above, before the include of
 * `tock_kernel_layout.ld`). On GNU LD, however, the last ENTRY wins. We do both
 * so this function will be set as the entrypoint no matter what linker we're
 * using */
ENTRY(esp32_c3_direct_boot_entry)

ASSERT(LOADADDR(.relocate) + SIZEOF(.relocate) <= _ble_load_start,
       "the kernel overlaps the BLE sandbox")
