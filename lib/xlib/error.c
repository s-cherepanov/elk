#include "xlib.h"

static Object V_X_Error_Handler, V_X_Fatal_Error_Handler;

/* Default error handlers of the Xlib */
extern int _XDefaultIOError();   
extern int _XDefaultError();

static X_Fatal_Error (d) Display *d; {
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
}

static X_Error (d, ep) Display *d; XErrorEvent *ep; {
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
}

static X_After_Function (d) Display *d; {
    Object args;
    GC_Node;

    args = Make_Display (0, d);
    GC_Link (args);
    args = Cons (args, Null);
    GC_Unlink;
    (void)Funcall (DISPLAY(Car (args))->after, args, 0);
}

static Object P_Set_After_Function (d, f) Object d, f; {
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

static Object P_After_Function (d) Object d; {
    Check_Type (d, T_Display);
    return DISPLAY(d)->after;
}
    
elk_init_xlib_error () {
    Define_Variable (&V_X_Fatal_Error_Handler, "x-fatal-error-handler", Null);
    Define_Variable (&V_X_Error_Handler, "x-error-handler", Null);
    (void)XSetIOErrorHandler (X_Fatal_Error);
    (void)XSetErrorHandler (X_Error);
    Define_Primitive (P_Set_After_Function, "set-after-function!", 2, 2, EVAL);
    Define_Primitive (P_After_Function,     "after-function",      1, 1, EVAL);
}
