/* client.c
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

#include "xlib.h"

static Object Sym_Wm_Hints, Sym_Size_Hints;

static Object P_Iconify_Window (Object w, Object scr) {
    Check_Type (w, T_Window);
    if (!XIconifyWindow (WINDOW(w)->dpy, WINDOW(w)->win,
            Get_Screen_Number (WINDOW(w)->dpy, scr)))
        Primitive_Error ("cannot iconify window");
    return Void;
}

static Object P_Withdraw_Window (Object w, Object scr) {
    Check_Type (w, T_Window);
    if (!XWithdrawWindow (WINDOW(w)->dpy, WINDOW(w)->win,
            Get_Screen_Number (WINDOW(w)->dpy, scr)))
        Primitive_Error ("cannot withdraw window");
    return Void;
}

static Object P_Reconfigure_Wm_Window (Object w, Object scr, Object conf) {
    unsigned long mask;

    Check_Type (w, T_Window);
    mask = Vector_To_Record (conf, Conf_Size, Sym_Conf, Conf_Rec);
    if (!XReconfigureWMWindow (WINDOW(w)->dpy, WINDOW(w)->win,
            Get_Screen_Number (WINDOW(w)->dpy, scr), mask, &WC))
        Primitive_Error ("cannot reconfigure window");
    return Void;
}

static Object P_Wm_Command (Object w) {
    int i, ac;
    char **av;
    Object s, ret, t;
    GC_Node2;

    Check_Type (w, T_Window);
    Disable_Interrupts;
    if (!XGetCommand (WINDOW(w)->dpy, WINDOW(w)->win, &av, &ac))
        ac = 0;
    Enable_Interrupts;
    ret = t = P_Make_List (Make_Integer (ac), Null);
    GC_Link2 (ret, t);
    for (i = 0; i < ac; i++, t = Cdr (t)) {
        s = Make_String (av[i], strlen (av[i]));
        Car (t) = s;
    }
    GC_Unlink;
    if (ac) XFreeStringList (av);
    return ret;
}

static String_List_To_Text_Property (Object x, XTextProperty *ret) {
    register i, n;
    register char **s;
    Object t;
    Alloca_Begin;

    Check_List (x);
    n = Fast_Length (x);
    Alloca (s, char**, n * sizeof (char *));
    for (i = 0; i < n; i++, x = Cdr (x)) {
        t = Car (x);
        Get_Strsym_Stack (t, s[i]);
    }
    if (!XStringListToTextProperty (s, n, ret))
        Primitive_Error ("cannot create text property");
    Alloca_End;
}

static Object Text_Property_To_String_List (XTextProperty *p) {
    int n;
    register i;
    char **s;
    Object x, ret, t;
    GC_Node2;

    if (!XTextPropertyToStringList (p, &s, &n))
        Primitive_Error ("cannot convert from text property");
    ret = t = P_Make_List (Make_Integer (n), Null);
    GC_Link2 (ret, t);
    for (i = 0; i < n; i++, t = Cdr (t)) {
        x = Make_String (s[i], strlen (s[i]));
        Car (t) = x;
    }
    GC_Unlink;
    XFreeStringList (s);
    return ret;
}

static Object P_Get_Text_Property (Object w, Object a) {
    XTextProperty ret;

    Check_Type (w, T_Window);
    Check_Type (a, T_Atom);
    Disable_Interrupts;
    if (!XGetTextProperty (WINDOW(w)->dpy, WINDOW(w)->win, &ret,
            ATOM(a)->atom)) {
        Enable_Interrupts;
        return False;
    }
    Enable_Interrupts;
    return Text_Property_To_String_List (&ret);
}

static Object P_Set_Text_Property (Object w, Object prop, Object a) {
    XTextProperty p;

    Check_Type (w, T_Window);
    Check_Type (a, T_Atom);
    String_List_To_Text_Property (prop, &p);
    XSetTextProperty (WINDOW(w)->dpy, WINDOW(w)->win, &p, ATOM(a)->atom);
    XFree ((char *)p.value);
    return Void;
}

static Object P_Wm_Protocols (Object w) {
    Atom *p;
    int i, n;
    Object ret;
    GC_Node;

    Check_Type (w, T_Window);
    Disable_Interrupts;
    if (!XGetWMProtocols (WINDOW(w)->dpy, WINDOW(w)->win, &p, &n))
        Primitive_Error ("cannot get WM protocols");
    Enable_Interrupts;
    ret = Make_Vector (n, Null);
    GC_Link (ret);
    for (i = 0; i < n; i++) {
        Object a;

        a = Make_Atom (p[i]);
        VECTOR(ret)->data[i] = a;
    }
    XFree ((char *)p);
    GC_Unlink;
    return ret;
}

static Object P_Set_Wm_Protocols (Object w, Object v) {
    Atom *p;
    int i, n;
    Alloca_Begin;

    Check_Type (w, T_Window);
    Check_Type (v, T_Vector);
    n = VECTOR(v)->size;
    Alloca (p, Atom*, n * sizeof (Atom));
    for (i = 0; i < n; i++) {
        Object a;
        a = VECTOR(v)->data[i];
        Check_Type (a, T_Atom);
        p[i] = ATOM(a)->atom;
    }
    if (!XSetWMProtocols (WINDOW(w)->dpy, WINDOW(w)->win, p, n))
        Primitive_Error ("cannot set WM protocols");
    Alloca_End;
    return Void;
}

static Object P_Wm_Class (Object w) {
    Object ret, x;
    XClassHint c;
    GC_Node;

    Check_Type (w, T_Window);
    /*
     * In X11.2 XGetClassHint() returns either 0 or Success, which happens
     * to be defined as 0.  So until this bug is fixed, we must
     * explicitly check whether the XClassHint structure has been filled.
     */
    c.res_name = c.res_class = 0;
    Disable_Interrupts;
    (void)XGetClassHint (WINDOW(w)->dpy, WINDOW(w)->win, &c);
    Enable_Interrupts;
    ret = Cons (False, False);
    GC_Link (ret);
    if (c.res_name) {
        x = Make_String (c.res_name, strlen (c.res_name));
        Car (ret) = x;
        XFree (c.res_name);
    }
    if (c.res_class) {
        x = Make_String (c.res_class, strlen (c.res_class));
        Cdr (ret) = x;
        XFree (c.res_class);
    }
    GC_Unlink;
    return ret;
}

static Object P_Set_Wm_Class (Object w, Object name, Object class) {
    XClassHint c;

    Check_Type (w, T_Window);
    c.res_name = Get_Strsym (name);
    c.res_class = Get_Strsym (class);
    XSetClassHint (WINDOW(w)->dpy, WINDOW(w)->win, &c);
    return Void;
}

static Object P_Set_Wm_Command (Object w, Object cmd) {
    register i, n;
    register char **argv;
    Object c;
    Alloca_Begin;

    Check_Type (w, T_Window);
    Check_List (cmd);
    n = Fast_Length (cmd);
    Alloca (argv, char**, n * sizeof (char *));
    for (i = 0; i < n; i++, cmd = Cdr (cmd)) {
        c = Car (cmd);
        Get_Strsym_Stack (c, argv[i]);
    }
    XSetCommand (WINDOW(w)->dpy, WINDOW(w)->win, argv, n);
    Alloca_End;
    return Void;
}

static Object P_Wm_Hints (Object w) {
    XWMHints *p;

    Check_Type (w, T_Window);
    Disable_Interrupts;
    p = XGetWMHints (WINDOW(w)->dpy, WINDOW(w)->win);
    Enable_Interrupts;
    if (p) {
        WMH = *p;
        XFree ((char *)p);
    } else {
        WMH.flags = 0;
    }
    return Record_To_Vector (Wm_Hints_Rec, Wm_Hints_Size, Sym_Wm_Hints,
        WINDOW(w)->dpy, (unsigned long)WMH.flags);
}

static Object P_Set_Wm_Hints (Object w, Object h) {
    unsigned long mask;

    Check_Type (w, T_Window);
    mask = Vector_To_Record (h, Wm_Hints_Size, Sym_Wm_Hints, Wm_Hints_Rec);
    WMH.flags = mask;
    XSetWMHints (WINDOW(w)->dpy, WINDOW(w)->win, &WMH);
    return Void;
}

static Object P_Size_Hints (Object w, Object a) {
    long supplied;

    Check_Type (w, T_Window);
    Check_Type (a, T_Atom);
    Disable_Interrupts;
    if (!XGetWMSizeHints (WINDOW(w)->dpy, WINDOW(w)->win, &SZH, &supplied,
            ATOM(a)->atom))
        SZH.flags = 0;
    if (!(supplied & PBaseSize))
        SZH.flags &= ~PBaseSize;
    if (!(supplied & PWinGravity))
        SZH.flags &= ~PWinGravity;
    Enable_Interrupts;
    if ((SZH.flags & (PPosition|USPosition)) == (PPosition|USPosition))
        SZH.flags &= ~PPosition;
    if ((SZH.flags & (PSize|USSize)) == (PSize|USSize))
        SZH.flags &= ~PSize;
    return Record_To_Vector (Size_Hints_Rec, Size_Hints_Size, Sym_Size_Hints,
        WINDOW(w)->dpy, (unsigned long)SZH.flags);
}

static Object P_Set_Size_Hints (Object w, Object a, Object h) {
    unsigned long mask;

    Check_Type (w, T_Window);
    Check_Type (a, T_Atom);
    bzero ((char *)&SZH, sizeof (SZH));        /* Not portable? */
    mask = Vector_To_Record (h, Size_Hints_Size, Sym_Size_Hints,
        Size_Hints_Rec);
    if ((mask & (PPosition|USPosition)) == (PPosition|USPosition))
        mask &= ~PPosition;
    if ((mask & (PSize|USSize)) == (PSize|USSize))
        mask &= ~PSize;
    SZH.flags = mask;
    XSetWMSizeHints (WINDOW(w)->dpy, WINDOW(w)->win, &SZH, ATOM(a)->atom);
    return Void;
}

static Object P_Icon_Sizes (Object w) {
    XIconSize *p;
    int i, n;
    Object v;
    GC_Node;

    Check_Type (w, T_Window);
    Disable_Interrupts;
    if (!XGetIconSizes (WINDOW(w)->dpy, WINDOW(w)->win, &p, &n))
        n = 0;
    Enable_Interrupts;
    v = Make_Vector (n, Null);
    GC_Link (v);
    for (i = 0; i < n; i++) {
        register XIconSize *q = &p[i];
        Object t;

        t = P_Make_List (Make_Integer (6), Null);
        VECTOR(v)->data[i] = t;
        Car (t) = Make_Integer (q->min_width); t = Cdr (t);
        Car (t) = Make_Integer (q->min_height); t = Cdr (t);
        Car (t) = Make_Integer (q->max_width); t = Cdr (t);
        Car (t) = Make_Integer (q->max_height); t = Cdr (t);
        Car (t) = Make_Integer (q->width_inc); t = Cdr (t);
        Car (t) = Make_Integer (q->height_inc);
    }
    GC_Unlink;
    if (n > 0)
        XFree ((char *)p);
    return v;
}

static Object P_Set_Icon_Sizes (Object w, Object v) {
    register i, n;
    XIconSize *p;
    Alloca_Begin;

    Check_Type (w, T_Window);
    Check_Type (v, T_Vector);
    n = VECTOR(v)->size;
    Alloca (p, XIconSize*, n * sizeof (XIconSize));
    for (i = 0; i < n; i++) {
        register XIconSize *q = &p[i];
        Object t;

        t = VECTOR(v)->data[i];
        Check_List (t);
        if (Fast_Length (t) != 6)
            Primitive_Error ("invalid argument: ~s", t);
        q->min_width = Get_Integer (Car (t)); t = Cdr (t);
        q->min_height = Get_Integer (Car (t)); t = Cdr (t);
        q->max_width = Get_Integer (Car (t)); t = Cdr (t);
        q->max_height = Get_Integer (Car (t)); t = Cdr (t);
        q->width_inc = Get_Integer (Car (t)); t = Cdr (t);
        q->height_inc = Get_Integer (Car (t));
    }
    XSetIconSizes (WINDOW(w)->dpy, WINDOW(w)->win, p, n);
    Alloca_End;
    return Void;
}

static Object P_Transient_For (Object w) {
    Window win;

    Disable_Interrupts;
    if (!XGetTransientForHint (WINDOW(w)->dpy, WINDOW(w)->win, &win))
        win = None;
    Enable_Interrupts;
    return Make_Window (0, WINDOW(w)->dpy, win);
}

static Object P_Set_Transient_For (Object w, Object pw) {
    Check_Type (w, T_Window);
    XSetTransientForHint (WINDOW(w)->dpy, WINDOW(w)->win, Get_Window (pw));
    return Void;
}

elk_init_xlib_client () {
    Define_Symbol (&Sym_Wm_Hints, "wm-hints");
    Define_Symbol (&Sym_Size_Hints, "size-hints");
    Define_Primitive (P_Iconify_Window,   "iconify-window",    2, 2, EVAL);
    Define_Primitive (P_Withdraw_Window,  "withdraw-window",   2, 2, EVAL);
    Define_Primitive (P_Reconfigure_Wm_Window,
                        "xlib-reconfigure-wm-window",          3, 3, EVAL);
    Define_Primitive (P_Wm_Command,       "wm-command",        1, 1, EVAL);
    Define_Primitive (P_Get_Text_Property,"get-text-property", 2, 2, EVAL);
    Define_Primitive (P_Set_Text_Property,"set-text-property!",3, 3, EVAL);
    Define_Primitive (P_Wm_Protocols,     "wm-protocols",      1, 1, EVAL);
    Define_Primitive (P_Set_Wm_Protocols, "set-wm-protocols!", 2, 2, EVAL);
    Define_Primitive (P_Wm_Class,         "wm-class",          1, 1, EVAL);
    Define_Primitive (P_Set_Wm_Class,     "set-wm-class!",     3, 3, EVAL);
    Define_Primitive (P_Set_Wm_Command,   "set-wm-command!",   2, 2, EVAL);
    Define_Primitive (P_Wm_Hints,         "xlib-wm-hints",     1, 1, EVAL);
    Define_Primitive (P_Set_Wm_Hints,     "xlib-set-wm-hints!",2, 2, EVAL);
    Define_Primitive (P_Size_Hints,       "xlib-wm-size-hints",2, 2, EVAL);
    Define_Primitive (P_Set_Size_Hints,
                        "xlib-set-wm-size-hints!",             3, 3, EVAL);
    Define_Primitive (P_Icon_Sizes,       "icon-sizes",        1, 1, EVAL);
    Define_Primitive (P_Set_Icon_Sizes,   "set-icon-sizes!",   2, 2, EVAL);
    Define_Primitive (P_Transient_For,    "transient-for",     1, 1, EVAL);
    Define_Primitive (P_Set_Transient_For,"set-transient-for!",2, 2, EVAL);
}
