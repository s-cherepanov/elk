#include <stdarg.h>

#include "xt.h"

Match_Xt_Obj (x, v) Object x; va_list v; {
    register type = TYPE(x);

    if (type == T_Context) {
	return va_arg (v, XtAppContext) == CONTEXT(x)->context;
    } else if (type == T_Class) {
	return va_arg (v, WidgetClass) == CLASS(x)->wclass;
    } else if (type == T_Widget) {
	return va_arg (v, Widget) == WIDGET(x)->widget;
    } else if (type == T_Identifier) {
	return va_arg (v, int) == IDENTIFIER(x)->type
	    && va_arg (v, XtPointer) == IDENTIFIER(x)->val;
    } else Panic ("Match_Xt_Obj");
    return 0;
}
