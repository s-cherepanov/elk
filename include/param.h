/* These definitions are not intended to be tunable.  Do not change
 * them unless you _must_.
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
