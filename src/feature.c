/* feature.c: provide, require, and related primitives.
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

static Object Features;

void Init_Features () {
    Features = Null;
    Global_GC_Link (Features);
#ifdef CAN_DUMP
    P_Provide (Intern ("elk:dump"));
#endif
#ifdef CAN_LOAD_LIB
    P_Provide (Intern ("elk:load-lib"));
#endif
}

Object P_Features () {
    return Features;
}

Object P_Featurep (Object sym) {
    Object member;

    Check_Type (sym, T_Symbol);
    member = P_Memq (sym, Features);
    return Truep (member) ? True : False;
}

Object P_Provide (Object sym) {
    Object member;

    Check_Type (sym, T_Symbol);
    member = P_Memq (sym, Features);
    if (!Truep (member))
        Features = Cons (sym, Features);
    return Void;
}

static Object Feature_Filename (Object str) {
    struct S_String *sp = STRING(str);
    int len = sp->size;
    char *p;
    Object s;
    GC_Node;

    for (p = sp->data+len-1; p >= sp->data && *p != '.'; p--)
        ;
    if (p >= sp->data)
       return str;
    GC_Link (str);
    s = Make_String ((char *)0, len+4);
    memcpy (STRING(s)->data, STRING(str)->data, len);
    memcpy (STRING(s)->data+len, ".scm", 4);
    GC_Unlink;
    return s;
}

Object P_Require (int argc, Object *argv) {
    Object sym, a[1], isfeature;
    GC_Node;

    sym = argv[0];
    GC_Link (sym);
    isfeature = P_Featurep (sym);
    if (!Truep (isfeature)) {
        if (argc == 3)
            Check_Type (argv[2], T_Environment);
        a[0] = argc == 1 ? Feature_Filename (SYMBOL(sym)->name) : argv[1];
        if (Var_Is_True (V_Autoload_Notifyp))
            Format (Standard_Output_Port, "[Autoloading ~a]~%", 18, 1, a);
        (void)General_Load (a[0], argc == 3 ? argv[2] : The_Environment);
        isfeature = P_Featurep (sym);
        if (!Truep (isfeature))
            Primitive_Error ("feature ~s was not provided", sym);
    }
    GC_Unlink;
    return Void;
}
