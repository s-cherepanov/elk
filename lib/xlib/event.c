#include "xlib.h"

#define MAX_ARGS 14

static Object Argl, Argv;

static struct event_desc {
    char *name;
    int argc;
} Event_Table[] = {
    { "event-0",	     1 },
    { "event-1",             1 },
    { "key-press",          12 },
    { "key-release",        12 },
    { "button-press",       12 },
    { "button-release",     12 },
    { "motion-notify",      12 },
    { "enter-notify",       14 },
    { "leave-notify",       14 },
    { "focus-in",            4 },
    { "focus-out",           4 },
    { "keymap-notify",       3 },
    { "expose",              7 },
    { "graphics-expose",     9 },
    { "no-expose",           4 },
    { "visibility-notify",   3 },
    { "create-notify",       9 },
    { "destroy-notify",      3 },
    { "unmap-notify",        4 },
    { "map-notify",          4 },
    { "map-request",         3 },
    { "reparent-notify",     7 },
    { "configure-notify",   10 },
    { "configure-request",  11 },
    { "gravity-notify",      5 },
    { "resize-request",      4 },
    { "circulate-notify",    4 },
    { "circulate-request",   4 },
    { "property-notify",     5 },
    { "selection-clear",     4 },
    { "selection-request",   7 },
    { "selection-notify",    6 },
    { "colormap-notify",     5 },
    { "client-message",      4 },
    { "mapping-notify",      4 },
    { 0,                     0 }
};

struct predicate_arg {
    Object *funcs;
    Object *ret;
};

/*ARGSUSED*/
static Event_Predicate (dpy, ep, ptr) Display *dpy; XEvent *ep;
#ifdef XLIB_RELEASE_5_OR_LATER
		XPointer ptr; {
#else
		char *ptr; {
#endif
    struct predicate_arg *ap = (struct predicate_arg *)ptr;
    register i;
    Object args;
    GC_Node;

    if ((i = ep->type) < LASTEvent && !Nullp (ap->funcs[i])) {
	args = Get_Event_Args (ep);
	GC_Link (args);
	*ap->ret = Funcall (ap->funcs[i], args, 0);
	Destroy_Event_Args (args);
	GC_Unlink;
    }
    return Truep (*ap->ret);
}

/* (handle-events display discard? peek? clause...)
 * clause = (event function) or ((event...) function) or (else function)
 * loops/blocks until a function returns x != #f, then returns x.
 * discard?: discard unprocessed events.
 * peek?: don't discard processed events.
 */

static Object P_Handle_Events (argl) Object argl; {
    Object next, clause, func, ret, funcs[LASTEvent], args;
    register i, discard, peek;
    Display *dpy;
    char *errmsg = "event occurs more than once";
    GC_Node3; struct gcnode gcv;
    TC_Prolog;

    TC_Disable;
    clause = args = Null;
    GC_Link3 (argl, clause, args);
    next = Eval (Car (argl));
    Check_Type (next, T_Display);
    dpy = DISPLAY(next)->dpy;
    argl = Cdr (argl);
    next = Eval (Car (argl));
    Check_Type (next, T_Boolean);
    discard = Truep (next);
    argl = Cdr (argl);
    next = Eval (Car (argl));
    Check_Type (next, T_Boolean);
    peek = Truep (next);
    for (i = 0; i < LASTEvent; i++)
	funcs[i] = Null;
    gcv.gclen = 1+LASTEvent; gcv.gcobj = funcs; gcv.next = &gc3; GC_List = &gcv;
    for (argl = Cdr (argl); !Nullp (argl); argl = Cdr (argl)) {
	clause = Car (argl);
	Check_List (clause);
	if (Fast_Length (clause) != 2)
	    Primitive_Error ("badly formed event clause");
	func = Eval (Car (Cdr (clause)));
	Check_Procedure (func);
	clause = Car (clause);
	if (EQ(clause, Sym_Else)) {
	    for (i = 0; i < LASTEvent; i++)
		if (Nullp (funcs[i])) funcs[i] = func;
	} else {
	    if (TYPE(clause) == T_Pair) {
		for (; !Nullp (clause); clause = Cdr (clause)) {
		    i = Encode_Event (Car (clause));
		    if (!Nullp (funcs[i]))
			Primitive_Error (errmsg);
		    funcs[i] = func;
		}
	    } else {
		i = Encode_Event (clause);
		if (!Nullp (funcs[i]))
		    Primitive_Error (errmsg);
		funcs[i] = func;
	    }
	}
    }
    ret = False;
    while (!Truep (ret)) {
	XEvent e;
	if (discard) {
	    (peek ? XPeekEvent : XNextEvent) (dpy, &e);
	    if ((i = e.type) < LASTEvent && !Nullp (funcs[i])) {
		args = Get_Event_Args (&e);
		ret = Funcall (funcs[i], args, 0);
		Destroy_Event_Args (args);
	    } else {
		if (peek)
		    XNextEvent (dpy, &e);  /* discard it */
	    }
	} else {
	    struct predicate_arg a;
	    a.funcs = funcs;
	    a.ret = &ret;
	    (peek ? XPeekIfEvent : XIfEvent) (dpy, &e, Event_Predicate,
#ifdef XLIB_RELEASE_5_OR_LATER
		(XPointer)&a);
#else
		(char *)&a);
#endif
	}
    }
    GC_Unlink;
    TC_Enable;
    return ret;
}

static Object Get_Time_Arg (t) Time t; {
    return t == CurrentTime ? Sym_Now : Make_Unsigned_Long ((unsigned long)t);
}

Object Get_Event_Args (ep) XEvent *ep; {
    Object tmpargs[MAX_ARGS];
    register e, i;
    register Object *a, *vp;
    struct gcnode gcv;
    Object dummy;
    GC_Node;

    e = ep->type;
    dummy = Null;
    a = tmpargs;
    for (i = 0; i < MAX_ARGS; i++)
	a[i] = Null;
    GC_Link (dummy);
    gcv.gclen = 1 + MAX_ARGS; gcv.gcobj = a; gcv.next = &gc1; GC_List = &gcv;
    switch (e) {
    case KeyPress: case KeyRelease:
    case ButtonPress: case ButtonRelease:
    case MotionNotify:
    case EnterNotify: case LeaveNotify: {
	register XKeyEvent *p = (XKeyEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Window (0, p->display, p->root);
	a[3] = Make_Window (0, p->display, p->subwindow);
	a[4] = Get_Time_Arg (p->time);
	a[5] = Make_Integer (p->x);
	a[6] = Make_Integer (p->y);
	a[7] = Make_Integer (p->x_root);
	a[8] = Make_Integer (p->y_root);
	if (e == KeyPress || e == KeyRelease) {
	    a[9] = Bits_To_Symbols ((unsigned long)p->state, 1, State_Syms);
	    a[10] = Make_Integer (p->keycode);
	    a[11] = p->same_screen ? True : False;
	} else if (e == ButtonPress || e == ButtonRelease) {
	    register XButtonEvent *q = (XButtonEvent *)ep;
	    a[9] = Bits_To_Symbols ((unsigned long)q->state, 1, State_Syms);
	    a[10] = Bits_To_Symbols ((unsigned long)q->button, 0, Button_Syms);
	    a[11] = q->same_screen ? True : False;
	} else if (e == MotionNotify) {
	    register XMotionEvent *q = (XMotionEvent *)ep;
	    a[9] = Bits_To_Symbols ((unsigned long)q->state, 1, State_Syms);
	    a[10] = q->is_hint ? True : False;
	    a[11] = q->same_screen ? True : False;
	} else {
	    register XCrossingEvent *q = (XCrossingEvent *)ep;
	    a[9] = Bits_To_Symbols ((unsigned long)q->mode, 0, Cross_Mode_Syms);
	    a[10] = Bits_To_Symbols ((unsigned long)q->detail, 0,
		Cross_Detail_Syms);
	    a[11] = q->same_screen ? True : False;
	    a[12] = q->focus ? True : False;
	    a[13] = Bits_To_Symbols ((unsigned long)q->state, 1, Button_Syms);
	}
    } break;
    case FocusIn: case FocusOut: {
	register XFocusChangeEvent *p = (XFocusChangeEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Bits_To_Symbols ((unsigned long)p->mode, 0, Cross_Mode_Syms);
	a[3] = Bits_To_Symbols ((unsigned long)p->detail, 0, Focus_Detail_Syms);
    } break;
    case KeymapNotify: {
	register XKeymapEvent *p = (XKeymapEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_String (p->key_vector, 32);
    } break;
    case Expose: {
	register XExposeEvent *p = (XExposeEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Integer (p->x);
	a[3] = Make_Integer (p->y);
	a[4] = Make_Integer (p->width);
	a[5] = Make_Integer (p->height);
	a[6] = Make_Integer (p->count);
    } break;
    case GraphicsExpose: {
	register XGraphicsExposeEvent *p = (XGraphicsExposeEvent *)ep;
	a[1] = Make_Window (0, p->display, p->drawable);
	a[2] = Make_Integer (p->x);
	a[3] = Make_Integer (p->y);
	a[4] = Make_Integer (p->width);
	a[5] = Make_Integer (p->height);
	a[6] = Make_Integer (p->count);
	a[7] = Make_Integer (p->major_code);
	a[8] = Make_Integer (p->minor_code);
    } break;
    case NoExpose: {
	register XNoExposeEvent *p = (XNoExposeEvent *)ep;
	a[1] = Make_Window (0, p->display, p->drawable);
	a[2] = Make_Integer (p->major_code);
	a[3] = Make_Integer (p->minor_code);
    } break;
    case VisibilityNotify: {
	register XVisibilityEvent *p = (XVisibilityEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Bits_To_Symbols ((unsigned long)p->state, 0, Visibility_Syms);
    } break;
    case CreateNotify: {
	register XCreateWindowEvent *p = (XCreateWindowEvent *)ep;
	a[1] = Make_Window (0, p->display, p->parent);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Make_Integer (p->x);
	a[4] = Make_Integer (p->y);
	a[5] = Make_Integer (p->width);
	a[6] = Make_Integer (p->height);
	a[7] = Make_Integer (p->border_width);
	a[8] = p->override_redirect ? True : False;
    } break;
    case DestroyNotify: {
	register XDestroyWindowEvent *p = (XDestroyWindowEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
    } break;
    case UnmapNotify: {
	register XUnmapEvent *p = (XUnmapEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = p->from_configure ? True : False;
    } break;
    case MapNotify: {
	register XMapEvent *p = (XMapEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = p->override_redirect ? True : False;
    } break;
    case MapRequest: {
	register XMapRequestEvent *p = (XMapRequestEvent *)ep;
	a[1] = Make_Window (0, p->display, p->parent);
	a[2] = Make_Window (0, p->display, p->window);
    } break;
    case ReparentNotify: {
	register XReparentEvent *p = (XReparentEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Make_Window (0, p->display, p->parent);
	a[4] = Make_Integer (p->x);
	a[5] = Make_Integer (p->y);
	a[6] = p->override_redirect ? True : False;
    } break;
    case ConfigureNotify: {
	register XConfigureEvent *p = (XConfigureEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Make_Integer (p->x);
	a[4] = Make_Integer (p->y);
	a[5] = Make_Integer (p->width);
	a[6] = Make_Integer (p->height);
	a[7] = Make_Integer (p->border_width);
	a[8] = Make_Window (0, p->display, p->above);
	a[9] = p->override_redirect ? True : False;
    } break;
    case ConfigureRequest: {
	register XConfigureRequestEvent *p = (XConfigureRequestEvent *)ep;
	a[1] = Make_Window (0, p->display, p->parent);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Make_Integer (p->x);
	a[4] = Make_Integer (p->y);
	a[5] = Make_Integer (p->width);
	a[6] = Make_Integer (p->height);
	a[7] = Make_Integer (p->border_width);
	a[8] = Make_Window (0, p->display, p->above);
	a[9] = Bits_To_Symbols ((unsigned long)p->detail, 0, Stack_Mode_Syms);
	a[10] = Make_Unsigned_Long (p->value_mask);
    } break;
    case GravityNotify: {
	register XGravityEvent *p = (XGravityEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Make_Integer (p->x);
	a[4] = Make_Integer (p->y);
    } break;
    case ResizeRequest: {
	register XResizeRequestEvent *p = (XResizeRequestEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Integer (p->width);
	a[3] = Make_Integer (p->height);
    } break;
    case CirculateNotify: {
	register XCirculateEvent *p = (XCirculateEvent *)ep;
	a[1] = Make_Window (0, p->display, p->event);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Bits_To_Symbols ((unsigned long)p->place, 0, Place_Syms);
    } break;
    case CirculateRequest: {
	register XCirculateRequestEvent *p = (XCirculateRequestEvent *)ep;
	a[1] = Make_Window (0, p->display, p->parent);
	a[2] = Make_Window (0, p->display, p->window);
	a[3] = Bits_To_Symbols ((unsigned long)p->place, 0, Place_Syms);
    } break;
    case PropertyNotify: {
	register XPropertyEvent *p = (XPropertyEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Atom (p->atom);
	a[3] = Get_Time_Arg (p->time);
	a[4] = Bits_To_Symbols ((unsigned long)p->state, 0, Prop_Syms);
    } break;
    case SelectionClear: {
	register XSelectionClearEvent *p = (XSelectionClearEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Atom (p->selection);
	a[3] = Get_Time_Arg (p->time);
    } break;
    case SelectionRequest: {
	register XSelectionRequestEvent *p = (XSelectionRequestEvent *)ep;
	a[1] = Make_Window (0, p->display, p->owner);
	a[2] = Make_Window (0, p->display, p->requestor);
	a[3] = Make_Atom (p->selection);
	a[4] = Make_Atom (p->target);
	a[5] = Make_Atom (p->property);
	a[6] = Get_Time_Arg (p->time);
    } break;
    case SelectionNotify: {
	register XSelectionEvent *p = (XSelectionEvent *)ep;
	a[1] = Make_Window (0, p->display, p->requestor);
	a[2] = Make_Atom (p->selection);
	a[3] = Make_Atom (p->target);
	a[4] = Make_Atom (p->property);
	a[5] = Get_Time_Arg (p->time);
    } break;
    case ColormapNotify: {
	register XColormapEvent *p = (XColormapEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Colormap (0, p->display, p->colormap);
	a[3] = p->new ? True : False;
	a[4] = p->state == ColormapInstalled ? True : False;
    } break;
    case ClientMessage: {
	register XClientMessageEvent *p = (XClientMessageEvent *)ep;
	register i;

	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Make_Atom (p->message_type);
	switch (p->format) {
	case 8:
	    a[3] = Make_String (p->data.b, 20);
	    break;
	case 16:
	    a[3] = Make_Vector (10, Null);
	    for (i = 0; i < 10; i++)
		VECTOR(a[3])->data[i] = Make_Integer (p->data.s[i]);
	    break;
	case 32:
	    a[3] = Make_Vector (5, Null);
	    for (i = 0; i < 5; i++)
		VECTOR(a[3])->data[i] = Make_Long (p->data.l[i]);
	    break;
	default:
	    a[3] = Make_Integer (p->format);   /* ??? */
	}
    } break;
    case MappingNotify: {
	register XMappingEvent *p = (XMappingEvent *)ep;
	a[1] = Make_Window (0, p->display, p->window);
	a[2] = Bits_To_Symbols ((unsigned long)p->request, 0, Mapping_Syms);
	a[3] = Make_Integer (p->first_keycode);
	a[4] = Make_Integer (p->count);
    } break;
    }
    a[0] = Intern (Event_Table[e].name);
    for (vp = VECTOR(Argv)->data, i = 0; i < Event_Table[e].argc; i++) {
	if (i) vp++;
	Car (*vp) = a[i];
	Cdr (*vp) = vp[1];
    }
    Cdr (*vp) = Null;
    GC_Unlink;
    return Argl;
}

void Destroy_Event_Args (args) Object args; {
    Object t;

    for (t = args; !Nullp (t); t = Cdr (t))
	Car (t) = Null;
}

Encode_Event (e) Object e; {
    Object s;
    register char *p;
    register struct event_desc *ep;
    register n;

    Check_Type (e, T_Symbol);
    s = SYMBOL(e)->name;
    p = STRING(s)->data;
    n = STRING(s)->size;
    for (ep = Event_Table; ep->name; ep++)
	if (n && strncmp (ep->name, p, n) == 0) break;
    if (ep->name == 0)
	Primitive_Error ("no such event: ~s", e);
    return ep-Event_Table;
}

static Object P_Get_Motion_Events (w, from, to) Object w, from, to; {
    XTimeCoord *p;
    int n;
    register i;
    Object e, ret;
    GC_Node2;

    Check_Type (w, T_Window);
    p = XGetMotionEvents (WINDOW(w)->dpy, WINDOW(w)->win, Get_Time (from),
	Get_Time (to), &n);
    e = ret = Make_Vector (n, Null);
    GC_Link2 (ret, e);
    for (i = 0; i < n; i++) {
	e = P_Make_List (Make_Integer (3), Null);
	VECTOR(ret)->data[i] = e;
	Car (e) = Get_Time_Arg (p[i].time); e = Cdr (e);
	Car (e) = Make_Integer (p[i].x); e = Cdr (e);
	Car (e) = Make_Integer (p[i].y);
    }
    GC_Unlink;
    XFree ((char *)p);
    return ret;
}

static Object P_Event_Listen (d, wait_flag) Object d, wait_flag; {
    Display *dpy;
    register n;
    XEvent e;

    Check_Type (d, T_Display);
    Check_Type (wait_flag, T_Boolean);
    dpy = DISPLAY(d)->dpy;
    n = XPending (dpy);
    if (n == 0 && EQ(wait_flag, True)) {
	XPeekEvent (dpy, &e);
	n = XPending (dpy);
    }
    return Make_Integer (n);
}

elk_init_xlib_event () {
    Object t;
    register i;

    Argl = P_Make_List (Make_Integer (MAX_ARGS), Null);
    Global_GC_Link (Argl);
    Argv = Make_Vector (MAX_ARGS, Null);
    Global_GC_Link (Argv);
    for (i = 0, t = Argl; i < MAX_ARGS; i++, t = Cdr (t))
	VECTOR(Argv)->data[i] = t;
    Define_Primitive (P_Handle_Events,   "handle-events",     3, MANY, NOEVAL);
    Define_Primitive (P_Get_Motion_Events,
			"get-motion-events",                  3, 3, EVAL);
    Define_Primitive (P_Event_Listen,    "event-listen",      2, 2, EVAL);
}
