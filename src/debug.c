/* debug.c: The primitive `backtrace-list'.
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

Object P_Backtrace_List (int argc, Object *argv) {
    register GCNODE *p, *gp = GC_List;
    register int delta = 0;
    Object cp, list, tail, cell, vec;
    GC_Node3;

    if (argc > 0) {
	cp = argv[0];
	Check_Type (cp, T_Control_Point);
	delta = CONTROL(cp)->delta;
	gp = CONTROL(cp)->gclist;
    }
    vec = list = tail = Null;
    GC_Link3 (vec, list, tail);
    for ( ; gp; gp = p->next) {
	p = (GCNODE *)NORM(gp);
	switch (p->gclen) {
	case TAG_ENV:
	    vec = Make_Vector (3, Null);
	    VECTOR(vec)->data[2] = *(Object *)NORM(p->gcobj);
	    break;
	case TAG_FUN: case TAG_TCFUN:
	    VECTOR(vec)->data[0] = *(Object *)NORM(p->gcobj);
	    break;
	case TAG_ARGS:
	    VECTOR(vec)->data[1] = *(Object *)NORM(p->gcobj);
	    cell = Cons (vec, Null);
	    if (Nullp (list))
		list = cell;
	    else
		(void)P_Set_Cdr (tail, cell);
	    tail = cell;
	}
    }
    GC_Unlink;
    return list;
}
