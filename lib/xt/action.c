#include "xt.h"

typedef struct action {
    char *name;
    int num;
    XtAppContext con;
    struct action *next;
} ACTION;

ACTION *actions;

/*ARGSUSED*/
static void Dummy_Action (w, ep, argv, argc) Widget w; XEvent *ep;
	String *argv; int *argc; {
}

void Action_Hook (w, client_data, name, ep, argv, argc)
	Widget w; XtPointer client_data; char *name; XEvent *ep;
	char **argv; int *argc; {
    ACTION *ap;
    Object args, params, tail;
    register i;
    GC_Node3;

    for (ap = actions; ap; ap = ap->next) {
	if (strcmp (ap->name, name))
	    continue;
	args = params = tail = Null;
	GC_Link3 (args, params, tail);
	params = P_Make_List (Make_Integer (*argc), Null);
	for (i = 0, tail = params; i < *argc; tail = Cdr (tail), i++) {
	    Object tmp;

	    tmp = Make_String (argv[i], strlen (argv[i]));
	    Car (tail) = tmp;
	}
	args = Cons (params, Null);
	params = Get_Event_Args (ep);
	args = Cons (Copy_List (params), args);
	Destroy_Event_Args (params);
	args = Cons (Make_Widget_Foreign (w), args);
	(void)Funcall (Get_Function (ap->num), args, 0);
	GC_Unlink;
    }
}

static Object P_Context_Add_Action (c, s, p) Object c, s, p; {
    ACTION *ap;
    XtActionsRec a;

    Check_Context (c);
    Check_Procedure (p);
    ap = (ACTION *)XtMalloc (sizeof (ACTION));
    ap->num = Register_Function (p);
    ap->name = XtNewString (Get_Strsym (s));
    ap->con = CONTEXT(c)->context;
    ap->next = actions;
    actions = ap;
    a.string = ap->name;
    a.proc = (XtActionProc)Dummy_Action;
    XtAppAddActions (ap->con, &a, 1);
    return Void;
}

void Free_Actions (con) XtAppContext con; {
    register ACTION *p, **pp;

    for (pp = &actions; p = *pp; ) {
	if (p->con == con) {
	    Deregister_Function (p->num);
	    XtFree (p->name);
	    *pp = p->next;
	    XtFree ((char *)p);
	} else pp = &p->next;
    }
}

elk_init_xt_action () {
    Define_Primitive (P_Context_Add_Action, "context-add-action", 3, 3, EVAL);
}
