/* param.h: These definitions are not intended to be tunable. Do not change
 * them unless you _must_.
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
 *
 * This software was derived from Elk 1.2, which was Copyright 1987, 1988,
 * 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
 * by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
 * between TELES and Nixdorf Microprocessor Engineering, Berlin).
 *
 * Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
 * owners or individual owners of copyright in this software, grant to any
 * person or company a worldwide, royalty free, license to
 *
 *    i) copy this software,
 *   ii) prepare derivative works based on this software,
 *  iii) distribute copies of this software or derivative works,
 *   iv) perform this software, or
 *    v) display this software,
 *
 * provided that this notice is not removed and that neither Oliver Laumann
 * nor Teles nor Nixdorf are deemed to have made any representations as to
 * the suitability of this software for any purpose nor are held responsible
 * for any defects of this software.
 *
 * THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

/* Name of Scheme file which is always loaded on startup:
 */
#define INITFILE             "initscheme.scm"


/* Name of environment variable holding initial load-path:
 */
#define LOADPATH_ENV         "ELK_LOADPATH"


/* Size of `obarray' (symbol hash table):
 */
#define OBARRAY_SIZE         1009


/* Approximate size of gap between beginning of stack and `stkbase'
 * (subtracted from stack limit on startup):
 */
#define STACK_MARGIN         (64*1024)


/* Minimum number of bytes that must be reclaimed by a run of the
 * stop-and-copy garbage collector:
 */
#define HEAP_MARGIN          (HEAP_SIZE/10*1024)


/* The buffers maintained by output string ports grow in increments
 * of STRING_GROW_SIZE when written:
 */
#define STRING_GROW_SIZE     64


/* Initial print depth and print length:
 */
#define DEF_PRINT_DEPTH      20
#define DEF_PRINT_LEN        1000


/* Offset to compensate for differences in argv/environment between base
 * a.out and dumped a.out on startup (see src/main.c):
 */
#ifdef CAN_DUMP
#  define INITIAL_STK_OFFSET (20*1024)       /* 2*NCARGS */
#endif


/* Number of static string buffers cyclically reused by Get_String()
 * and Get_Strsym():
 */
#define NUMSTRBUFS           3
