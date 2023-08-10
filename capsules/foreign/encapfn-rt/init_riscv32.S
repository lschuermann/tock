/* contsvc_rtheader is defined by the service linker script (contsvc_layout.ld).
 * It has the following layout:
 *
 *     Field                       | Offset
 *     ------------------------------------
 *     Start of .data in flash     |      0
 *     Size of .data               |      4
 *     Start of .data in RAM       |      8
 *     Size of .bss                |     12
 *     Start of .bss in RAM        |     16
 */

/* contsvc_init is executed by the kernel to have it initialize its memory.
 * The kernel passes the following arguments:
 *
 *     a0  Pointer to the contsvc_rtheader.
 *
 * After initialization, we are expected to return to the kernel by invoking
 * `ret` (`jalr x0, x1, 0`), where `a0` is used to indicate the initialization
 * status. `a0` = 0 is interpreted as successful initialization, all other
 * values indicate an error.
 */

/* .section .contsvc_init_text */

.globl contsvc_init
contsvc_init:
        /* Make sure all of the provided parameters are word-aligned: */
        lw   t0, 0*4(a0)             /* src = contsvc_rtheader.data_start */
        andi t3, t0, 3
        bnez t3, .Linit_error

        lw   t1, 1*4(a0)             /* remaining = contsvc_rtheader.data_size */
        andi t3, t1, 3
        bnez t3, .Linit_error

        lw   t2, 2*4(a0)             /* dest = contsvc_rtheader.data_start */
        andi t3, t2, 3
        bnez t3, .Linit_error

                /* Copy data */
        beqz t1, .Lzero_bss          /* Jump to zero_bss if remaining is zero */

.Ldata_loop_body:
        lw   t3, 0(t0)               /* t3 = *src */
        sw   t3, 0(t2)               /* *dest = t3 */
        addi t0, t0, 4               /* src += 4 */
        addi t1, t1, -4              /* remaining -= 4 */
        addi t2, t2, 4               /* dest += 4 */
        bnez t1, .Ldata_loop_body    /* Loop if there's still data remaining */

.Lzero_bss:
        lw   t0, 3*4(a0)             /* remaining = rt_contsvc_rtheader.bss_size */
        lw   t1, 4*4(a0)             /* dest = rt_contsvc_rtheader.bss_start */
        add  t2, t1, t0              /* end = dest + remaining */

        /* Zero BSS */
        beq  t1, t2, .Linit_done     /* Jump to init_done if no data to copy */

.Lbss_loop_body:
        sb   zero, 0(t1)             /* *dest = zero */
        addi t1, a0, 1               /* dest += 1 */
        beq  t1, t2, .Lbss_loop_body /* Iterate again if dest != end */

.Linit_done:
        li   a0, 0
        ret

.Linit_error:
        li   a0, 1
        ret
