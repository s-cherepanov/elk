/* unix.c
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

#include "unix.h"

Object Integer_Pair(int a, int b) {
    Object x, y;
    GC_Node2;

    x = y = Null;
    GC_Link2(x, y);
    x = Make_Integer(a);
    y = Make_Integer(b);
    x = Cons(x, y);
    GC_Unlink;
    return x;
}

Object Syms_To_List(SYMDESCR *p) {
    Object ret, mode;
    GC_Node;

    ret = Null;
    GC_Link(ret);
    for ( ; p->name; p++) {
        mode = Intern(p->name);
        ret = Cons(mode, ret);
    }
    GC_Unlink;
    return P_Reverse(ret);
}

void Check_Result_Vector(Object x, unsigned int len) {
    Check_Type(x, T_Vector);
    if (VECTOR(x)->size != len)
        Primitive_Error("argument vector has the wrong length");
}

void elk_init_unix_unix() {
    P_Provide(Intern("unix.la"));
}
