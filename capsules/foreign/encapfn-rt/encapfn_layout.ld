
MEMORY {
  FLASH (X) : ORIGIN = FLASH_START + TBF_HEADER_SIZE, LENGTH = FLASH_LENGTH - TBF_HEADER_SIZE
  RAM   (W) : ORIGIN = RAM_START                    , LENGTH = RAM_LENGTH
}

/* GNU LD looks for `start` as an entry point by default, while LLVM's LLD looks
 * for `_start`. To be compatible with both, we manually specify an entry point.
 */
ENTRY(start)

SECTIONS {
    /* The FLASH memory section is placed at a TBF_HEADER_SIZE offset, to give
     * elf2tab room to prepend the TBF headers. Communicate this reservation to
     * elf2tab, such that it fills up the space after the TBF headers (if any)
     * as part of the protected region trailer:
     */
    tbf_protected_region_size = TBF_HEADER_SIZE;

    /* Sections located in FLASH.
     */

    /* Containerized service header.
     */
    . = ALIGN(4);
    .contsvc_header : {
        /* We define start here, in the first section to be included
         * in FLASH. This makes the linker and elf2tab happy, but we
         * don't actually use this symbol. */
        start = .;

        /* MAGIC: CSVC */
        LONG(0x43535643);
        LONG(contsvc_rthdr - ORIGIN(FLASH));
        LONG(contsvc_init - ORIGIN(FLASH));
        LONG(contsvc_fntab - ORIGIN(FLASH));

        KEEP(*(.contsvc_hdr));
    } > FLASH

    /* Text section -- the application's code. */
    .text ALIGN(4) : {
        contsvc_rthdr = .;

        LONG(LOADADDR(.data));
        LONG(SIZEOF(.data));
        LONG(ADDR(.data));
        LONG(SIZEOF(.bss));
        LONG(ADDR(.bss));

        KEEP(*(contsvc_init))

        *(.text.*)
    } > FLASH

    /* Read-only data section. Contains strings and other global constants. */
    .rodata ALIGN(4) : {
        *(.rodata.*)
    } > FLASH

    /* Sections located in RAM at runtime.
     */

    /* Read-write data section. This is deployed as part of FLASH but is copied
     * into RAM at runtime by the loader (i.e. Tock).
     */
    .data ALIGN(4) : {
        /* .sdata is the RISC-V small data section */
        *(.sdata .data)
    } > RAM AT > FLASH

    /* BSS section. These are zero-initialized static variables. */
    .bss ALIGN(4) (NOLOAD) : {
        /* .sbss is the RISC-V small data section */
        *(.sbss .bss.*)
    } > RAM AT > FLASH

    /* Sections we do not need. */
    /DISCARD/ :
    {
      *(.ARM.exidx .eh_frame)
    }
}
