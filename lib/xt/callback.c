#include "xt.h"

typedef struct {
    PFX2S converter;
    int num;
} CLIENT_DATA;

Object Get_Callbackfun (c) XtPointer c; {
    register CLIENT_DATA *cd = (CLIENT_DATA *)c;
    return cd ? Get_Function (cd->num) : False;
}

static void Callback_Proc (w, client_data, call_data) Widget w;
	XtPointer client_data, call_data; {
    register CLIENT_DATA *cd = (CLIENT_DATA *)client_data;
    Object args;
    GC_Node;
 
    args = Null;
    GC_Link (args);
    if (cd->converter)
	args = Cons ((cd->converter)((XtArgVal)call_data), args);
    args = Cons (Make_Widget_Foreign (w), args);
    GC_Unlink;
    (void)Funcall (Get_Callbackfun (client_data), args, 0);
}

/*ARGSUSED*/
void Destroy_Callback_Proc (w, client_data, call_data) Widget w;
	XtPointer client_data, call_data; {
    Object x;

    x = Find_Object (T_Widget, (GENERIC)0, Match_Xt_Obj, w);
    if (Nullp (x) || WIDGET(x)->free)
	return;
    WIDGET(x)->free = 1;
    Remove_All_Callbacks (w);
    Deregister_Object (x);
}

/* The code assumes that callbacks are called in the order they
 * have been added.  The Destroy_Callback_Proc() must always be
 * the last callback in the destroy callback list of each widget.
 *
 * When the destroy callback list of a widget is modified
 * (via P_Add_Callbacks or P_Set_Values), Fiddle_Destroy_Callback()
 * must be called to remove the Destroy_Callback_Proc() and put
 * it back to the end of the callback list.
 */
void Fiddle_Destroy_Callback (w) Widget w; {
    XtRemoveCallback (w, XtNdestroyCallback, Destroy_Callback_Proc,
	(XtPointer)0);
    XtAddCallback (w, XtNdestroyCallback, Destroy_Callback_Proc, (XtPointer)0);
}

void Check_Callback_List (x) Object x; {
    Object tail;

    Check_List (x);
    for (tail = x; !Nullp (tail); tail = Cdr (tail))
	Check_Procedure (Car (tail));
}

static Object P_Add_Callbacks (w, name, cbl) Object w, name, cbl; {
    register char *s;
    register n;
    XtCallbackList callbacks;
    Alloca_Begin;

    Check_Widget (w);
    Check_Callback_List (cbl);
    s = Get_Strsym (name);
    Make_Resource_Name (s);
    n = Fast_Length (cbl);
    Alloca (callbacks, XtCallbackRec*, (n+1) * sizeof (XtCallbackRec));
    callbacks[n].callback = 0;
    callbacks[n].closure = 0;
    Fill_Callbacks (cbl, callbacks, n,
	Find_Callback_Converter (XtClass (WIDGET(w)->widget), s, name));
    XtAddCallbacks (WIDGET(w)->widget, s, callbacks);
    if (streq (s, XtNdestroyCallback))
	Fiddle_Destroy_Callback (WIDGET(w)->widget);
    Alloca_End;
    return Void;
}

void Fill_Callbacks (src, dst, n, conv) Object src; XtCallbackList dst;
	register n; PFX2S conv; {
    register CLIENT_DATA *cd;
    register i, j;
    Object tail;

    for (i = 0, tail = src; i < n; i++, tail = Cdr (tail)) {
	j = Register_Function (Car (tail));
	cd = (CLIENT_DATA *)XtMalloc (sizeof (CLIENT_DATA));
	cd->converter = conv;
	cd->num = j;
	dst[i].callback = (XtCallbackProc)Callback_Proc;
	dst[i].closure = (XtPointer)cd;
    }
}

Remove_All_Callbacks (w) Widget w; {
    Arg a[1];
    XtCallbackList c;
    XtResource *r;
    int nr, nc;
    register i, j;

    Get_All_Resources (0, w, XtClass (w), &r, &nr, &nc);
    for (j = 0; j < nr; j++) {
	if (streq (r[j].resource_type, XtRCallback)) {
	    XtSetArg (a[0], r[j].resource_name, &c);
	    XtGetValues (w, a, 1);
	    for (i = 0; c[i].callback; i++) {
		register CLIENT_DATA *cd = (CLIENT_DATA *)c[i].closure;
		if (c[i].callback == (XtCallbackProc)Callback_Proc && cd) {
		    Deregister_Function (cd->num);
		    XtFree ((char *)cd);
		}
	    }
	}
    }
    XtFree ((char *)r);
}

elk_init_xt_callback () {
    Define_Primitive (P_Add_Callbacks, "add-callbacks", 3, 3, EVAL);
}
