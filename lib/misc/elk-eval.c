/* elk-eval.c
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

#include <string.h>

#include "scheme.h"

static Object in, out;

static char *String_Eval(char *expr) {
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

char *Elk_Eval(char *expr) {
    char *s;

    s = String_Eval("\
        (call-with-current-continuation (lambda (c)\
          (set! error-handler (lambda a (print a) (c #f))) #t))\
    ");
    if (strcmp(s, "#f") == 0)
        return 0;
    return String_Eval(expr);
}

void elk_init_eval() {
    in = P_Open_Input_String(Make_String("", 0));
    Global_GC_Link(in);
    out = P_Open_Output_String();
    Global_GC_Link(out);
}
