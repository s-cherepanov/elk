/* popup.c
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
