/* load-shl.c
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

#include <dl.h>
#include <string.h>

extern void Free_Symbols (SYMTAB *);
extern void Call_Initializers (SYMTAB *, char *, int);

extern int errno;

static void Load_Them (Object names) {
    char *fn;
    shl_t handle;
    SYM *sp;
    static struct obj_loaded {
        struct obj_loaded *next;
        char *name;
    } *loaded, *lp;
    GC_Node;
    Alloca_Begin;

    GC_Link(names);
    for ( ; !Nullp (names); names = Cdr (names)) {
        Get_Strsym_Stack (Car (names), fn);
        for (lp = loaded; lp; lp = lp->next)
            if (strcmp (lp->name, fn) == 0) break;
        if (lp) continue;
        lp = (struct obj_loaded *)Safe_Malloc (sizeof (*lp));
        lp->name = strdup (fn);
        lp->next = loaded;
        loaded = lp;
        if (Verb_Load)
            printf ("[shl_load %s]\n", fn);
        if ((handle = shl_load (fn, BIND_IMMEDIATE|BIND_VERBOSE, 0L)) == 0) {
            Saved_Errno = errno;
            Primitive_Error ("shl_load of ~s failed: ~E", Car (names));
        }
        if (The_Symbols)
            Free_Symbols (The_Symbols);
        The_Symbols = Open_File_And_Snarf_Symbols (fn);
        for (sp = The_Symbols->first; sp; sp = sp->next)
            if (shl_findsym (&handle, sp->name, TYPE_UNDEFINED, &sp->value)) {
                Saved_Errno = errno;
                Primitive_Error ("~s: shl_findsym on ~s failed: ~E",
                    Car (names),
                    Make_String (sp->name, strlen (sp->name)));
        }
        Call_Initializers (The_Symbols, 0, PR_CONSTRUCTOR);
        Call_Initializers (The_Symbols, 0, PR_EXTENSION);
    }
    GC_Unlink;
    Alloca_End;
}

void Load_Object (Object names) {
    Object port, tail, fullnames, str;
    char *p, *libs = "";
    GC_Node3;
    Alloca_Begin;

    port = tail = fullnames = Null;
    GC_Link3 (port, tail, fullnames);
    for (tail = names; !Nullp (tail); tail = Cdr (tail)) {
        port = General_Open_File (Car (tail), P_INPUT, Var_Get (V_Load_Path));
        fullnames = Cons (PORT(port)->name, fullnames);
        (void)P_Close_Input_Port (port);
    }
    tail = Var_Get (V_Load_Libraries);
    if (TYPE(tail) == T_String)
        Get_Strsym_Stack (tail, libs);
    Disable_Interrupts;
    for (tail = Null; (p = strtok (libs, " \t")) != 0; libs = 0) {
        str = Make_String (p, strlen (p));
        tail = Cons (str, tail);
    }
    Load_Them (tail);
    Load_Them (fullnames);
    Enable_Interrupts;
    GC_Unlink;
    Alloca_End;
}

void Finit_Load () {
}

void Fork_Load () {
}
