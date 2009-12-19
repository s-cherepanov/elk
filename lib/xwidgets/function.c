/* function.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

#include "xt.h"

static int max_functions = 512;
static Object Functions;

int Register_Function (Object x) {
    register int i;
    Object v;
    GC_Node;

    for (i = 0; i < max_functions; i++)
        if (Nullp (VECTOR(Functions)->data[i])) break;
    if (i == max_functions) {
        max_functions *= 2;
        GC_Link (x);
        v = Make_Vector (max_functions, Null);
        GC_Unlink;
        memcpy ((char *)VECTOR(v)->data, (char *)VECTOR(Functions)->data,
                i * sizeof (Object));
        Functions = v;
    }
    VECTOR(Functions)->data[i] = x;
    return i;
}

Object Get_Function (int i) {
    return VECTOR(Functions)->data[i];
}

void Deregister_Function (int i) {
    VECTOR(Functions)->data[i] = Null;
}

void elk_init_xt_function () {
    Functions = Make_Vector (max_functions, Null);
    Global_GC_Link (Functions);
}
