/* cstring.c: Convert Scheme strings to C strings. The contents of strings
 * has to be copied, because strings in Elk do not have a terminating null-
 * byte (strings may _contain_ null-bytes).
 *
 * Get_String() and Get_Strsym() allocate memory in NUMSTRBUFS cyclically
 * reused buffers in the C heap.
 * The macros Get_String_Stack() and Get_Strsym_Stack() in include/cstring.h
 * allocate memory on the stack.  They have to be used whenever more than
 * NUMSTRBUFS strings are active in a function at the same time.
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

#include "config.h"

#include <string.h>

#include "kernel.h"

static char *heapstr[NUMSTRBUFS];
static int heaplen[NUMSTRBUFS];
static int nextstr;

void Init_Cstring() {  /* Preallocate memory to avoid fragmentation */
    int i;

    for (i = 0; i < NUMSTRBUFS; i++)
        heapstr[i] = Safe_Malloc (heaplen[i] = 512);
}

char *Get_String (Object str) {
    char **pp = &heapstr[nextstr];
    int len;

    Check_Type (str, T_String);
    if ((len = STRING(str)->size+1) > heaplen[nextstr]) {
        Disable_Interrupts;
        *pp = Safe_Realloc (*pp, len);
        heaplen[nextstr] = len;
        Enable_Interrupts;
    }
    memcpy (*pp, STRING(str)->data, --len);
    (*pp)[len] = '\0';
    if (++nextstr == NUMSTRBUFS) nextstr = 0;
    return *pp;
}

char *Get_Strsym (Object str) {
    if (TYPE(str) == T_Symbol)
        str = SYMBOL(str)->name;
    else if (TYPE(str) != T_String)
        Wrong_Type_Combination (str, "string or symbol");
    return Get_String (str);
}
