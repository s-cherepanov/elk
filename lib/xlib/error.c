/* error.c
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

#include <stdlib.h>

static Object V_X_Error_Handler, V_X_Fatal_Error_Handler;

/* Default error handlers of the Xlib */
extern int _XDefaultIOError();
extern int _XDefaultError();

static int X_Fatal_Error (Display *d) {
    Object args, fun;
    GC_Node;

    Reset_IO (0);
    args = Make_Display (0, d);
    GC_Link (args);
    args = Cons (args, Null);
    GC_Unlink;
    fun = Var_Get (V_X_Fatal_Error_Handler);
    if (TYPE(fun) == T_Compound)
        (void)Funcall (fun, args, 0);
    _XDefaultIOError (d);
    exit (1);         /* In case the default handler doesn't exit() */
    /*NOTREACHED*/
    return 0;
}

static int X_Error (Display *d, XErrorEvent *ep) {
    Object args, a, fun;
    GC_Node;

    Reset_IO (0);
    args = Make_Unsigned_Long ((unsigned long)ep->resourceid);
    GC_Link (args);
    args = Cons (args, Null);
    a = Make_Unsigned (ep->minor_code);
    args = Cons (a, args);
    a = Make_Unsigned (ep->request_code);
    args = Cons (a, args);
    a = Bits_To_Symbols ((unsigned long)ep->error_code, 0, Error_Syms);
    if (Nullp (a))
        a = Make_Unsigned (ep->error_code);
    args = Cons (a, args);
    a = Make_Unsigned_Long (ep->serial);
    args = Cons (a, args);
    a = Make_Display (0, ep->display);
    args = Cons (a, args);
    GC_Unlink;
    fun = Var_Get (V_X_Error_Handler);
    if (TYPE(fun) == T_Compound)
        (void)Funcall (fun, args, 0);
    else
        _XDefaultError (d, ep);
    return 0;
}

static int X_After_Function (Display *d) {
    Object args;
    GC_Node;

    args = Make_Display (0, d);
    GC_Link (args);
    args = Cons (args, Null);
    GC_Unlink;
    (void)Funcall (DISPLAY(Car (args))->after, args, 0);
    return 0;
}

static Object P_Set_After_Function (Object d, Object f) {
    Object old;

    Check_Type (d, T_Display);
    if (EQ(f, False)) {
        (void)XSetAfterFunction (DISPLAY(d)->dpy, (int (*)())0);
    } else {
        Check_Procedure (f);
        (void)XSetAfterFunction (DISPLAY(d)->dpy, X_After_Function);
    }
    old = DISPLAY(d)->after;
    DISPLAY(d)->after = f;
    return old;
}

static Object P_After_Function (Object d) {
    Check_Type (d, T_Display);
    return DISPLAY(d)->after;
}

void elk_init_xlib_error () {
    Define_Variable (&V_X_Fatal_Error_Handler, "x-fatal-error-handler", Null);
    Define_Variable (&V_X_Error_Handler, "x-error-handler", Null);
    (void)XSetIOErrorHandler (X_Fatal_Error);
    (void)XSetErrorHandler (X_Error);
    Define_Primitive (P_Set_After_Function, "set-after-function!", 2, 2, EVAL);
    Define_Primitive (P_After_Function,     "after-function",      1, 1, EVAL);
}
