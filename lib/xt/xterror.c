#include "xt.h"

static Object V_Xt_Warning_Handler;

void Xt_Warning (msg) char *msg; {
    Object args, fun;

    args = Cons (Make_String (msg, strlen (msg)), Null);
    fun = Var_Get (V_Xt_Warning_Handler);
    if (TYPE(fun) == T_Compound)
	(void)Funcall (fun, args, 0);
    Format (Curr_Output_Port, msg, strlen (msg), 0, (Object *)0);
    (void)P_Newline (0, (Object *)0);
}

elk_init_xt_error () {
    Define_Variable (&V_Xt_Warning_Handler, "xt-warning-handler", Null);
    XtSetWarningHandler (Xt_Warning);
}
