/* identifier.c
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

#include "xt.h"

Generic_Predicate (Identifier)

static Identifier_Equal (x, y) Object x, y; {
    register struct S_Identifier *p = IDENTIFIER(x), *q = IDENTIFIER(y);
    return p->type == q->type && p->val == q->val && !p->free && !q->free;
}

Generic_Print (Identifier, "#[identifier %lu]", POINTER(x))

Object Make_Id (type, val, num) XtPointer val; {
    Object i;

    i = Find_Object (T_Identifier, (GENERIC)0, Match_Xt_Obj, type, val);
    if (Nullp (i)) {
        i = Alloc_Object (sizeof (struct S_Identifier), T_Identifier, 0);
        IDENTIFIER(i)->tag = Null;
        IDENTIFIER(i)->type = type;
        IDENTIFIER(i)->val = val;
        IDENTIFIER(i)->num = num;
        IDENTIFIER(i)->free = 0;
        Register_Object (i, (GENERIC)0, (PFO)0, 0);
    }
    return i;
}

XtPointer Use_Id (x, type) Object x; {
    Check_Type (x, T_Identifier);
    if (IDENTIFIER(x)->type != type || IDENTIFIER(x)->free)
        Primitive_Error ("invalid identifier");
    IDENTIFIER(x)->free = 1;
    Deregister_Object (x);
    return IDENTIFIER(x)->val;
}

elk_init_xt_identifier () {
    Generic_Define (Identifier, "identifier", "identifier?");
}
