/* struct.c: The `structure' extension is obsolete and should not be used
 * in applications any longer; it has been replaced by the more powerful
 * `record' extension.
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

#include "scheme.h"

#define STRUCT(x)  ((struct S_Struct *)POINTER(x))

struct S_Struct {
    Object name;
    Object slots;
    Object values;
};

int T_Struct;

static Object P_Structurep (Object x) {
    return TYPE(x) == T_Struct ? True : False;
}

static Object P_Structure_Name (Object x) {
    Check_Type (x, T_Struct);
    return STRUCT(x)->name;
}

static Object P_Structure_Slots (Object x) {
    Check_Type (x, T_Struct);
    return P_Vector_To_List (STRUCT(x)->slots);
}

static Object P_Structure_Values (Object x) {
    Check_Type (x, T_Struct);
    return P_Vector_To_List (STRUCT(x)->values);
}

static void Check_Structure_Type (Object x, Object t) {
    Check_Type (x, T_Struct);
    Check_Type (t, T_Symbol);
    if (!EQ(STRUCT(x)->name, t))
        Primitive_Error ("wrong structure type ~s (expected ~s)",
            STRUCT(x)->name, t);
}

static Object P_Structure_Ref (Object x, Object t, Object n) {
    Check_Structure_Type (x, t);
    return P_Vector_Ref (STRUCT(x)->values, n);
}

static Object P_Structure_Set (Object x, Object t, Object n, Object obj) {
    Check_Structure_Type (x, t);
    return P_Vector_Set (STRUCT(x)->values, n, obj);
}

static Object P_Make_Structure (Object name, Object slots) {
    register int n;
    Object s, vec, *vp;
    GC_Node3;

    Check_Type (name, T_Symbol);
    Check_List (slots);
    s = Null;
    GC_Link3 (s, name, slots);
    s = Alloc_Object (sizeof (struct S_Struct), T_Struct, 0);
    STRUCT(s)->name = name;
    STRUCT(s)->values = STRUCT(s)->slots = Null;
    n = Fast_Length (slots);
    vec = Make_Vector (n, Null);
    STRUCT(s)->values = vec;
    vec = Make_Vector (n, Null);
    STRUCT(s)->slots = vec;
    GC_Unlink;
    for (vp = VECTOR(vec)->data; n--; slots = Cdr (slots)) {
        Check_Type (Car (slots), T_Symbol);
        *vp++ = Car (slots);
    }
    return s;
}

static int Structure_Eqv (Object a, Object b) {
    return EQ(a,b);
}

static int Structure_Equal (Object a, Object b) {
    return EQ(STRUCT(a)->name,STRUCT(b)->name) &&
           Equal (STRUCT(a)->slots, STRUCT(b)->slots) &&
           Equal (STRUCT(a)->values, STRUCT(b)->values);
}

static int Structure_Print (Object x, Object port,
                            int raw, int depth, int length) {
    struct S_String *s = STRING(SYMBOL(STRUCT(x)->name)->name);
    Printf (port, "#[%.*s-structure %lu]", s->size, s->data, POINTER(x));
    return 0;
}

static int Structure_Visit (register Object *sp, register int (*f)()) {
    (*f)(&STRUCT(*sp)->name);
    (*f)(&STRUCT(*sp)->slots);
    (*f)(&STRUCT(*sp)->values);
    return 0;
}

void elk_init_lib_struct () {
    T_Struct = Define_Type (0, "structure", NOFUNC, sizeof (struct S_Struct),
        Structure_Eqv, Structure_Equal, Structure_Print, Structure_Visit);
    Define_Primitive (P_Structurep,       "structure?",       1, 1, EVAL);
    Define_Primitive (P_Structure_Name,   "structure-name",   1, 1, EVAL);
    Define_Primitive (P_Structure_Slots,  "structure-slots",  1, 1, EVAL);
    Define_Primitive (P_Structure_Values, "structure-values", 1, 1, EVAL);
    Define_Primitive (P_Structure_Ref,    "structure-ref",    3, 3, EVAL);
    Define_Primitive (P_Structure_Set,    "structure-set!",   4, 4, EVAL);
    Define_Primitive (P_Make_Structure,   "make-structure",   2, 2, EVAL);
    P_Provide (Intern ("struct.la"));
}
