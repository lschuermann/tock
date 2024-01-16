/* Licensed under the Apache License, Version 2.0 or the MIT License. */
/* SPDX-License-Identifier: Apache-2.0 OR MIT                         */
/* Copyright Tock Contributors 2023.                                  */

/* Memory Space Definitions, 1M flash, 256K ram */
MEMORY
{
  rom (rx)  : ORIGIN = 0x00000000, LENGTH = 256K
  prog (rx) : ORIGIN = 0x00040000, LENGTH = 768K
  ram (rwx) : ORIGIN = 0x20000000, LENGTH = 128K
  dsvcram (rwx) : ORIGIN = 0x20020000, LENGTH = 128K
}

PAGE_SIZE = 4K;

_dsvcram_start = ORIGIN(dsvcram);
_dsvcram_end = ORIGIN(dsvcram) + LENGTH(dsvcram);
