// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// Minimal init for the Linux SBI guest, built against the kernel's nolibc
// (tools/include/nolibc), so no C library or busybox is required.

int main(void) {
  struct utsname u;

  if (uname(&u) == 0) {
    printf("Hello from Linux userspace (%s %s %s), running as a Tock SBI process!\n",
           u.sysname, u.release, u.machine);
  }

  for (int i = 0;; i++) {
    sleep(2);
    printf("[linux init] tick %d\n", i);
  }

  return 0;
}
