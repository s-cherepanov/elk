/* extension.c
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

#include "xlib.h"

static Object P_List_Extensions (d) Object d; {
    Object ret;
    int n;
    register i;
    register char **p;
    GC_Node;

    Check_Type (d, T_Display);
    Disable_Interrupts;
    p = XListExtensions (DISPLAY(d)->dpy, &n);
    Enable_Interrupts;
    ret = Make_Vector (n, Null);
    GC_Link (ret);
    for (i = 0; i < n; i++) {
        Object e;

        e = Make_String (p[i], strlen (p[i]));
        VECTOR(ret)->data[i] = e;
    }
    GC_Unlink;
    XFreeExtensionList (p);
    return ret;
}

static Object P_Query_Extension (d, name) Object d, name; {
    int opcode, event, error;
    Object ret, t;
    GC_Node2;

    Check_Type (d, T_Display);
    if (!XQueryExtension (DISPLAY(d)->dpy, Get_Strsym (name), &opcode,
            &event, &error))
        return False;
    t = ret = P_Make_List (Make_Integer (3), Null);
    GC_Link2 (ret, t);
    Car (t) = (opcode ? Make_Integer (opcode) : False); t = Cdr (t);
    Car (t) = (event ? Make_Integer (event) : False); t = Cdr (t);
    Car (t) = (error ? Make_Integer (error) : False);
    GC_Unlink;
    return ret;
}

elk_init_xlib_extension () {
    Define_Primitive (P_List_Extensions,    "list-extensions",   1, 1, EVAL);
    Define_Primitive (P_Query_Extension,    "query-extension",   2, 2, EVAL);
}
