/* RAM starts at 0x40000000, the binary is loaded into ROM at 0x0 */

MEMORY
{
  rom (rx)  : ORIGIN = 0x00000000, LENGTH = 0x00080000
  prog (rx) : ORIGIN = 0x00080000, LENGTH = 0x00080000
  ram (rwx) : ORIGIN = 0x40000000, LENGTH = 0x08000000
  dsvcram (rwx) : ORIGIN = 0x48000000, LENGTH = 0x100000
}

INCLUDE ../../kernel_layout.ld

_dsvcram_start = ORIGIN(dsvcram);
_dsvcram_end = ORIGIN(dsvcram) + LENGTH(dsvcram);
