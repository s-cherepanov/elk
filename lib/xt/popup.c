#include "xt.h"

static SYMDESCR Grab_Kind_Syms[] = {
    { "grab-none",         XtGrabNone },
    { "grab-nonexclusive", XtGrabNonexclusive },
    { "grab-exclusive",    XtGrabExclusive },
    { 0, 0 }
};

static Object P_Create_Popup_Shell (argc, argv) Object *argv; {
    ArgList a;
    char *name = 0;
    Object x, class, parent, ret;
    Alloca_Begin;

    x = argv[0];
    if (TYPE(x) != T_Class) {
	name = Get_Strsym (x);
	argv++; argc--;
    }
    class = argv[0];
    parent = argv[1];
    Check_Type (class, T_Class);
    Check_Widget (parent);
    if (name == 0)
	name = CLASS(class)->name;
    Encode_Arglist (argc-2, argv+2, a, (Widget)0, CLASS(class)->wclass);
    ret = Make_Widget (XtCreatePopupShell (name, CLASS(class)->wclass,
	WIDGET(parent)->widget, a, (Cardinal)(argc-2)/2));
    Alloca_End;
    return ret;
}

static Object P_Popup (shell, grab_kind) Object shell, grab_kind; {
    Check_Widget (shell);
    XtPopup (WIDGET(shell)->widget, Symbols_To_Bits (grab_kind, 0,
	Grab_Kind_Syms));
    return Void;
}

static Object P_Popdown (shell) Object shell; {
    Check_Widget (shell);
    XtPopdown (WIDGET(shell)->widget);
    return Void;
}

elk_init_xt_popup () {
    Define_Primitive (P_Create_Popup_Shell, "create-popup-shell",
					    2, MANY, VARARGS);
    Define_Primitive (P_Popup,   "popup",   2, 2, EVAL);
    Define_Primitive (P_Popdown, "popdown", 1, 1, EVAL);
}
