/* error.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>

#include "kernel.h"

void Reset () elk_attribute(__noreturn__);
void Err_Handler () elk_attribute(__noreturn__);

Object Arg_True;

static Object V_Error_Handler, V_Top_Level_Control_Point;

/* Error_Tag should be static and users should only use the functions
 * Set_Error_Tag() and Get_Error_Tag().  However, in older versions
 * the variable was manipulated directly, therefore it will remain global
 * for some time for backwards compatibility.
 */
char const *Error_Tag;

char *appname;

void Init_Error () {
    Arg_True = Cons (True, Null);
    Global_GC_Link (Arg_True);
    Define_Variable (&V_Error_Handler, "error-handler", Null);
    Define_Variable (&V_Top_Level_Control_Point, "top-level-control-point",
        Null);
}

char const *Get_Error_Tag () {
    return Error_Tag;
}

void Set_Error_Tag (char const *tag) {
    Error_Tag = tag;
}

void Set_App_Name (char *name) {
    appname = name;
}

#ifdef lint
/*VARARGS1*/
void Fatal_Error (const char *foo) { foo = foo; }
#else
void Fatal_Error (const char *fmt, ...) {
    va_list args;

    Disable_Interrupts;
    va_start (args, fmt);
    (void)fflush (stdout);
    if (appname)
        fprintf (stderr, "\n%s: fatal error: ", appname);
    else
        fprintf (stderr, "\nFatal error: ");
    vfprintf (stderr, fmt, args);
    fprintf (stderr, ".\n");
    va_end (args);
    exit (1);
}
#endif

void Panic (char const *msg) {
    Disable_Interrupts;
    (void)fflush (stdout);
    if (appname)
        fprintf (stderr, "\n%s: panic: ", appname);
    else
        fprintf (stderr, "\nPanic: ");
    fprintf (stderr, "%s (dumping core).\n", msg);
    abort ();
}

void Uncatchable_Error (char *errmsg) {
    Disable_Interrupts;
    Reset_IO (0);
    /*
     * The message can be sent to stdout, as Reset_IO() resets the
     * current output port back to the Standard_Output_Port:
     */
    if (appname) {
        printf ("%s: %c", appname, tolower (errmsg[0]));
        errmsg++;
    }
    printf("%s\n", errmsg);
    Reset ();
}

#ifdef lint
/*VARARGS1*/
void Primitive_Error (const char *foo) { foo = foo; }
#else
void Primitive_Error (const char *fmt, ...) {
    va_list args;
    register const char *p;
    register int i, n;
    Object msg, sym, argv[10];
    GC_Node; GCNODE gcv;

    va_start (args, fmt);
    for (n = 0, p = fmt; *p; p++)
        if (*p == '~' && p[1] != '~' && p[1] != '%'
                && p[1] != 'E' && p[1] != 'e')
            n++;
    if (n > 10)
        Panic ("Primitive_Error args");
    for (i = 0; i < n; i++)
        argv[i] = va_arg (args, Object);
    sym = Null;
    GC_Link (sym);
    gcv.gclen = 1 + i; gcv.gcobj = argv; gcv.next = &gc1; GC_List = &gcv;
    sym = Intern (Error_Tag);
    msg = Make_String (fmt, p - fmt);
    Err_Handler (sym, msg, i, argv);
    /*NOTREACHED*/
}
#endif

Object P_Error (int argc, Object *argv) {
    Check_Type (argv[1], T_String);
    Err_Handler (argv[0], argv[1], argc-2, argv+2);
    /*NOTREACHED*/
}

void Err_Handler (Object sym, Object fmt, int argc, Object *argv) {
    Object fun, args, a[1];
    GC_Node3;

    Reset_IO (0);
    args = Null;
    GC_Link3 (args, sym, fmt);
    args = P_List (argc, argv);
    args = Cons (fmt, args);
    args = Cons (sym, args);
    fun = Var_Get (V_Error_Handler);
    if (TYPE(fun) == T_Compound)
        (void)Funcall (fun, args, 0);
    a[0] = sym;
    Format (Curr_Output_Port, "~s: ", 4, 1, a);
    Format (Curr_Output_Port, STRING(fmt)->data, STRING(fmt)->size,
        argc, argv);
    (void)P_Newline (0, (Object *)0);
    GC_Unlink;
    Reset ();
    /*NOTREACHED*/
}

void Reset () {
    Object cp;

    cp = Var_Get (V_Top_Level_Control_Point);
    if (TYPE(cp) == T_Control_Point)
        (void)Funcall_Control_Point (cp, Arg_True, 0);
    (void)fflush (stdout);
    exit (1);
}

Object P_Reset () {
    Reset_IO (0);
    Reset ();
    /*NOTREACHED*/
}

void Range_Error (Object i) {
    Primitive_Error ("argument out of range: ~s", i);
}
