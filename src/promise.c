/* promise.c: Delay and force.
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

#include "kernel.h"

Object P_Promisep (Object x) {
    return TYPE(x) == T_Promise ? True : False;
}

Object P_Delay (Object argl) {
    Object d;
    GC_Node;

    GC_Link (argl);
    d = Alloc_Object (sizeof (struct S_Promise), T_Promise, 0);
    GC_Unlink;
    PROMISE(d)->done = 0;
    PROMISE(d)->env = The_Environment;
    PROMISE(d)->thunk = Car (argl);
    return d;
}

Object P_Force (Object d) {
    Object ret, a[2];
    GC_Node;
    TC_Prolog;

    Check_Type (d, T_Promise);
    if (PROMISE(d)->done)
        return PROMISE(d)->thunk;
    GC_Link (d);
    a[0] = PROMISE(d)->thunk; a[1] = PROMISE(d)->env;
    TC_Disable;
    ret = P_Eval (2, a);
    TC_Enable;
    GC_Unlink;
    if (PROMISE(d)->done)    /* take care of recursive force calls */
        return PROMISE(d)->thunk;
    PROMISE(d)->thunk = ret;
    PROMISE(d)->done = 1;
    return ret;
}

Object P_Promise_Environment (Object p) {
    Check_Type (p, T_Promise);
    return PROMISE(p)->env;
}
