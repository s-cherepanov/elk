/* The function
 * 
 *     char *Elk_Eval(char *expr);
 * 
 * is similar to Tcl_Eval() in Tcl.  It is called with a Scheme expression
 * encoded as a C string and returns the result of evaluating the expression
 * (as another C string), or a null pointer if an error has occured
 * during evaluation.
 * 
 * Elk_Eval() stores its result in a static buffer of fixed size; this
 * can be improved easily by passing a buffer and a length as additional
 * arguments.
 */

#include "scheme.h"

static Object in, out;

static char *String_Eval(expr) char *expr; {
    Object str, res;
    char *p;
    GC_Node;
    static char buf[1024];

    str = Make_String(expr, strlen(expr));
    PORT(in)->name = str;
    PORT(in)->ptr = 0;
    res = General_Read(in, 0);
    GC_Link(res);
    res = Eval(res);
    (void)General_Print_Object(res, out, 1);
    str = P_Get_Output_String(out);
    p = Get_String(str);
    if (strlen(p) > sizeof buf - 1)
	p = "too long";
    strcpy(buf, p);
    GC_Unlink;
    return buf;
}

char *Elk_Eval(expr) char *expr; {
    char *s;

    s = String_Eval("\
	(call-with-current-continuation (lambda (c)\
	  (set! error-handler (lambda a (print a) (c #f))) #t))\
    ");
    if (strcmp(s, "#f") == 0)
	return 0;
    return String_Eval(expr);
}

elk_init_eval() {
    in = P_Open_Input_String(Make_String("", 0));
    Global_GC_Link(in);
    out = P_Open_Output_String();
    Global_GC_Link(out);
}
