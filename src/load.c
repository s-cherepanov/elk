/* load.c
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

Object V_Load_Path, V_Load_Noisilyp, V_Load_Libraries;

char *Loader_Input;  /* tmp file name used by load.xx.c */

extern void Switch_Environment (Object);
#ifdef CAN_LOAD_LIB
extern void Load_Library (Object libs);
#endif
void Load_Source (Object);

void Init_Load () {
    Define_Variable (&V_Load_Path, "load-path",
        Cons (Make_String (".", 1),
        Cons (Make_String (Scm_Dir, strlen (Scm_Dir)),
        Cons (Make_String (Lib_Dir, strlen (Lib_Dir)), Null))));
    Define_Variable (&V_Load_Noisilyp, "load-noisily?", False);
    Define_Variable (&V_Load_Libraries, "load-libraries", Make_String ("", 0));
}

void Init_Loadpath (char *s) {     /* No GC possible here */
    register char *p;
    Object path;

    path = Null;
    if (s[0] == '\0')
        return;
    while (1) {
        for (p = s; *p && *p != ':'; p++)
            ;
        path = Cons (Make_String (s, p-s), path);
        if (*p == '\0')
            break;
        s = ++p;
    }
    Var_Set (V_Load_Path, P_Reverse (path));
}

int Has_Suffix (Object name, char const *suffix) {
    register char *p;
    register int len = strlen(suffix);
    register struct S_String *str;

    if (TYPE(name) == T_Symbol)
        name = SYMBOL(name)->name;
    str = STRING(name);
    p = str->data + str->size - len;
    return len <= str->size && !strncasecmp(p, suffix, len);
}

void Check_Loadarg (Object x) {
    Object tail;
    register int t = TYPE(x);

    if (t == T_Symbol || t == T_String)
        return;
    if (t != T_Pair)
        Wrong_Type_Combination (x, "string, symbol, or list");
    for (tail = x; !Nullp (tail); tail = Cdr (tail)) {
        Object f;

        f = Car (tail);
        if (TYPE(f) != T_Symbol && TYPE(f) != T_String)
            Wrong_Type_Combination (f, "string or symbol");
        if (!Has_Suffix (f, ".so"))
            Primitive_Error ("~s: not an object file", f);
    }
}

Object General_Load (Object what, Object env) {
    Object oldenv;
    GC_Node;

    Check_Type (env, T_Environment);
    oldenv = The_Environment;
    GC_Link (oldenv);
    Switch_Environment (env);
    Check_Loadarg (what);
    if (TYPE(what) == T_Pair) {
        if (Has_Suffix (Car (what), ".so"))
#ifdef CAN_LOAD_LIB
            Load_Library (what)
#endif
            ;
    }
    else if (Has_Suffix (what, ".so"))
#ifdef CAN_LOAD_LIB
        Load_Library (Cons (what, Null))
#endif
        ;
    else
        Load_Source (what);
    Switch_Environment (oldenv);
    GC_Unlink;
    return Void;
}

Object P_Load (int argc, Object *argv) {
    return General_Load (argv[0], argc == 1 ? The_Environment : argv[1]);
}

void Load_Source_Port (Object port) {
    Object val;
    GC_Node;
    TC_Prolog;

    GC_Link (port);
    while (1) {
        val = General_Read (port, 1);
        if (TYPE(val) == T_End_Of_File)
            break;
        TC_Disable;
        val = Eval (val);
        TC_Enable;
        if (Var_Is_True (V_Load_Noisilyp)) {
            Print (val);
            (void)P_Newline (0, (Object *)0);
        }
    }
    GC_Unlink;
}

void Load_Source (Object name) {
    Object port;
    GC_Node;

    port = General_Open_File (name, P_INPUT, Var_Get (V_Load_Path));
    GC_Link (port);
    Load_Source_Port (port);
    (void)P_Close_Input_Port (port);
    GC_Unlink;
}

/* Interface to P_Load() for use by applications.
 */
void Load_File (char *name) {
    Object arg;

    arg = Make_String(name, strlen(name));
    (void)P_Load(1, &arg);
}
