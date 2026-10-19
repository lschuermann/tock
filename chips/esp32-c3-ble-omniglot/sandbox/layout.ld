/* Licensed under the Apache License, Version 2.0 or the MIT License. */
/* SPDX-License-Identifier: Apache-2.0 OR MIT                         */
/* Copyright Tock Contributors 2026.                                  */

/* Layout of the sandboxed BLE controller, linked to run at fixed addresses.
 *
 * The sandbox is merged into the kernel binary, inside the kernel's flash
 * region (see the board's layout.ld, whose `ble_*` regions this must match):
 *
 * - LOAD (0x42070000): the header the kernel reads to load the sandbox, the
 *   function table, and the load copies of .iram and .data. The kernel
 *   copies .data and .iram, zeroes the rest of the sandbox's RAM, and hands
 *   the Omniglot runtime the function table and the stack top.
 * - FLASH (0x42080000): .text and .rodata, the sandbox's code window.
 * - `.iram1`: code ESP-IDF keeps in SRAM. It runs from the SRAM's
 *   instruction-bus alias at 0x40380000; the kernel copies it there through
 *   the data-bus alias. The sandbox cannot write it.
 * - `.dram1`: initialised data (mostly strings), part of `.data`.
 * - The mask ROM's libc routines, which the archives call.
 *
 * RAM ends below the kernel's `.iram` code at 0x3FCDEEB0; the kernel also
 * grants the sandbox the ROM's own RAM (0x3FCDF000..0x3FCE0000).
 */

MEMORY
{
  /* Read by the kernel only: header, function table, load copies. */
  LOAD  (r)  : ORIGIN = 0x42070000, LENGTH = 0x10000
  /* The sandbox's code window. */
  FLASH (rx) : ORIGIN = 0x42080000, LENGTH = 0x40000
  IRAM  (rx) : ORIGIN = 0x40380000, LENGTH = 0x8000
  RAM   (rw) : ORIGIN = 0x3FCD0000, LENGTH = 0xEEB0
}

/* Omniglot stack: entry points, interrupt handlers and the scheduler run on
 * it. The green threads have their own stacks in .bss. */
STACK_SIZE = 4 * 1024;

memcpy  = 0x40058bdc;
memset  = 0x40058d00;
memmove = 0x40058c92;
memcmp  = 0x40058b94;
strlen  = 0x400592ae;
strncmp = 0x400593c8;

/* The ROM's printf core, which formats through a putc callback; it has no
 * jump-table entry, so this is its address in the ECO7 ROM (esp-rom-elfs
 * `esp32c3_rev101_rom.elf`). `ets_printf` is overridden by the glue. */
ets_vprintf = 0x40049e2e;

ENTRY(sandbox_start)

SECTIONS
{
    .header : {
        /* Header the kernel reads to load the sandbox (`SandboxHeader` in
         * `esp32-c3-ble-omniglot`): magic "BLEC", the function table and
         * its length in pointers, the top of the Omniglot stack, and the
         * load address, run address and size of .data and of .iram, then
         * .bss. */
        LONG(0x43454C42);
        LONG(omniglot_fntab);
        LONG((omniglot_fntab_end - omniglot_fntab) / 4);
        LONG(_stack_top);
        LONG(LOADADDR(.data));
        LONG(ADDR(.data));
        LONG(SIZEOF(.data));
        LONG(LOADADDR(.iram));
        LONG(ADDR(.iram));
        LONG(SIZEOF(.iram));
        LONG(ADDR(.bss));
        LONG(SIZEOF(.bss));

        . = ALIGN(4);
        omniglot_fntab = .;
        KEEP(*(.omniglot_fntab));
        omniglot_fntab_end = .;
    } > LOAD

    .text : {
        *(.text .text.*)
        . = ALIGN(4);
    } > FLASH

    .rodata ALIGN(4) : {
        *(.rodata .rodata.* .srodata .srodata.*)
        . = ALIGN(4);
    } > FLASH

    .iram : {
        *(.iram1 .iram1.*)
        . = ALIGN(4);
    } > IRAM AT > LOAD

    .stack (NOLOAD) : {
        . = ALIGN(16);
        _stack_bottom = .;
        . = _stack_bottom + STACK_SIZE;
        . = ALIGN(16);
        _stack_top = .;
    } > RAM

    .data ALIGN(4) : {
        *(.sdata .sdata.* .data .data.* .dram1 .dram1.*)
        . = ALIGN(4);
    } > RAM AT > LOAD

    .bss ALIGN(4) (NOLOAD) : {
        *(.sbss .sbss.* .bss .bss.* COMMON)
        . = ALIGN(4);
    } > RAM

    /DISCARD/ : {
        *(.eh_frame .comment)
    }
}

ASSERT(SIZEOF(.iram) > 0, "no .iram1 code: check the archives")
