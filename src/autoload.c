/* autoload.c: Autoloading support.
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

Object V_Autoload_Notifyp;

void Init_Auto (void) {
    Define_Variable (&V_Autoload_Notifyp, "autoload-notify?", False);
}

Object P_Autoload (Object sym, Object files) {
    Object al, ret;
    GC_Node3;

    al = Null;
    Check_Type (sym, T_Symbol);
    Check_Loadarg (files);
    GC_Link3 (al, sym, files);
    al = Alloc_Object (sizeof (struct S_Autoload), T_Autoload, 0);
    AUTOLOAD(al)->files = files;
    AUTOLOAD(al)->env = The_Environment;
    al = Cons (al, Null);
    al = Cons (sym, al);
    ret = P_Define (al);
    GC_Unlink;
    return ret;
}

Object Do_Autoload (Object sym, Object al) {
    Object val, a[1];
    GC_Node;

    if (Var_Is_True (V_Autoload_Notifyp)) {
        a[0] = AUTOLOAD(al)->files;
        Format (Standard_Output_Port, "[Autoloading ~a]~%", 18, 1, a);
    }
    GC_Link (sym);
    (void)General_Load (AUTOLOAD(al)->files, AUTOLOAD(al)->env);
    GC_Unlink;
    val = SYMBOL(sym)->value;
    if (TYPE(val) == T_Autoload)
        Primitive_Error ("autoloading failed to define ~s", sym);
    return val;
}
