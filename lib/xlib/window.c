#include "xlib.h"

static Object Sym_Set_Attr, Sym_Get_Attr, Sym_Geo;
Object Sym_Conf;

Generic_Predicate (Window)

Generic_Equal_Dpy (Window, WINDOW, win)

Generic_Print (Window, "#[window %lu]", WINDOW(x)->win)

Generic_Get_Display (Window, WINDOW)

Object Make_Window (finalize, dpy, win) Display *dpy; Window win; {
    Object w;

    if (win == None)
        return Sym_None;
    if (win == PointerRoot)
        return Intern ("pointer-root");
    w = Find_Object (T_Window, (GENERIC)dpy, Match_X_Obj, win);
    if (Nullp (w)) {
        w = Alloc_Object (sizeof (struct S_Window), T_Window, 0);
        WINDOW(w)->tag = Null;
        WINDOW(w)->win = win;
        WINDOW(w)->dpy = dpy;
        WINDOW(w)->free = 0;
        WINDOW(w)->finalize = finalize;
        Register_Object (w, (GENERIC)dpy, finalize ? P_Destroy_Window :
            (PFO)0, 0);
    }
    return w;
}

Window Get_Window (w) Object w; {
    if (EQ(w, Sym_None))
        return None;
    Check_Type (w, T_Window);
    return WINDOW(w)->win;
}

Drawable Get_Drawable (d, dpyp) Object d; Display **dpyp; {
    if (TYPE(d) == T_Window) {
        *dpyp = WINDOW(d)->dpy;
        return (Drawable)WINDOW(d)->win;
    } else if (TYPE(d) == T_Pixmap) {
        *dpyp = PIXMAP(d)->dpy;
        return (Drawable)PIXMAP(d)->pm;
    }
    Wrong_Type_Combination (d, "drawable");
    /*NOTREACHED*/
}

static Object P_Create_Window (parent, x, y, width, height, border_width, attr)
        Object parent, x, y, width, height, border_width, attr; {
    unsigned long mask;
    Window win;

    Check_Type (parent, T_Window);
    mask = Vector_To_Record (attr, Set_Attr_Size, Sym_Set_Attr, Set_Attr_Rec);
    if ((win = XCreateWindow (WINDOW(parent)->dpy, WINDOW(parent)->win,
            Get_Integer (x), Get_Integer (y), Get_Integer (width),
            Get_Integer (height), Get_Integer (border_width),
            CopyFromParent, CopyFromParent, CopyFromParent, mask, &SWA)) == 0)
        Primitive_Error ("cannot create window");
    return Make_Window (1, WINDOW(parent)->dpy, win);
}

static Object P_Configure_Window (w, conf) Object w, conf; {
    unsigned long mask;

    Check_Type (w, T_Window);
    mask = Vector_To_Record (conf, Conf_Size, Sym_Conf, Conf_Rec);
    XConfigureWindow (WINDOW(w)->dpy, WINDOW(w)->win, mask, &WC);
    return Void;
}

static Object P_Change_Window_Attributes (w, attr) Object w, attr; {
    unsigned long mask;

    Check_Type (w, T_Window);
    mask = Vector_To_Record (attr, Set_Attr_Size, Sym_Set_Attr, Set_Attr_Rec);
    XChangeWindowAttributes (WINDOW(w)->dpy, WINDOW(w)->win, mask, &SWA);
    return Void;
}

static Object P_Get_Window_Attributes (w) Object w; {
    Check_Type (w, T_Window);
    XGetWindowAttributes (WINDOW(w)->dpy, WINDOW(w)->win, &WA);
    return Record_To_Vector (Win_Attr_Rec, Win_Attr_Size, Sym_Get_Attr,
        WINDOW(w)->dpy, ~0L);
}

static Object P_Get_Geometry (d) Object d; {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    /* GEO.width, GEO.height, etc. should really be unsigned, not int.
     */
    XGetGeometry (dpy, dr, &GEO.root, &GEO.x, &GEO.y, (unsigned *)&GEO.width,
        (unsigned *)&GEO.height, (unsigned *)&GEO.border_width,
        (unsigned *)&GEO.depth);
    return Record_To_Vector (Geometry_Rec, Geometry_Size, Sym_Geo, dpy, ~0L);
}

static Object P_Map_Window (w) Object w; {
    Check_Type (w, T_Window);
    XMapWindow (WINDOW(w)->dpy, WINDOW(w)->win);
    return Void;
}

static Object P_Unmap_Window (w) Object w; {
    Check_Type (w, T_Window);
    XUnmapWindow (WINDOW(w)->dpy, WINDOW(w)->win);
    return Void;
}

Object P_Destroy_Window (w) Object w; {
    Check_Type (w, T_Window);
    if (!WINDOW(w)->free)
        XDestroyWindow (WINDOW(w)->dpy, WINDOW(w)->win);
    Deregister_Object (w);
    WINDOW(w)->free = 1;
    return Void;
}

static Object P_Destroy_Subwindows (w) Object w; {
    Check_Type (w, T_Window);
    XDestroySubwindows (WINDOW(w)->dpy, WINDOW(w)->win);
    return Void;
}

static Object P_Map_Subwindows (w) Object w; {
    Check_Type (w, T_Window);
    XMapSubwindows (WINDOW(w)->dpy, WINDOW(w)->win);
    return Void;
}

static Object P_Unmap_Subwindows (w) Object w; {
    Check_Type (w, T_Window);
    XUnmapSubwindows (WINDOW(w)->dpy, WINDOW(w)->win);
    return Void;
}

static Object P_Circulate_Subwindows (w, dir) Object w, dir; {
    Check_Type (w, T_Window);
    XCirculateSubwindows (WINDOW(w)->dpy, WINDOW(w)->win,
        Symbols_To_Bits (dir, 0, Circulate_Syms));
    return Void;
}

static Object P_Query_Tree (w) Object w; {
    Window root, parent, *children;
    Display *dpy;
    int i;
    unsigned n;
    Object v, ret;
    GC_Node2;

    Check_Type (w, T_Window);
    dpy = WINDOW(w)->dpy;
    Disable_Interrupts;
    XQueryTree (dpy, WINDOW(w)->win, &root, &parent, &children, &n);
    Enable_Interrupts;
    v = ret = Null;
    GC_Link2 (v, ret);
    v = Make_Window (0, dpy, root);
    ret = Cons (v, Null);
    v = Make_Window (0, dpy, parent);
    ret = Cons (v, ret);
    v = Make_Vector (n, Null);
    for (i = 0; i < n; i++) {
        Object x;

        x = Make_Window (0, dpy, children[i]);
        VECTOR(v)->data[i] = x;
    }
    ret = Cons (v, ret);
    GC_Unlink;
    return ret;
}

static Object P_Translate_Coordinates (src, x, y, dst) Object src, x, y, dst; {
    int rx, ry;
    Window child;
    Object l, t, z;
    GC_Node3;

    Check_Type (src, T_Window);
    Check_Type (dst, T_Window);
    if (!XTranslateCoordinates (WINDOW(src)->dpy, WINDOW(src)->win,
            WINDOW(dst)->win, Get_Integer (x), Get_Integer (y), &rx, &ry,
            &child))
        return False;
    l = t = P_Make_List (Make_Integer (3), Null);
    GC_Link3 (l, t, dst);
    Car (t) = Make_Integer (rx); t = Cdr (t);
    Car (t) = Make_Integer (ry), t = Cdr (t);
    z = Make_Window (0, WINDOW(dst)->dpy, child);
    Car (t) = z;
    GC_Unlink;
    return l;
}

static Object P_Query_Pointer (win) Object win; {
    Object l, t, z;
    Bool ret;
    Window root, child;
    int r_x, r_y, x, y;
    unsigned int mask;
    GC_Node3;

    Check_Type (win, T_Window);
    ret = XQueryPointer (WINDOW(win)->dpy, WINDOW(win)->win, &root, &child,
        &r_x, &r_y, &x, &y, &mask);
    t = l = P_Make_List (Make_Integer (8), Null);
    GC_Link3 (l, t, win);
    Car (t) = Make_Integer (x); t = Cdr (t);
    Car (t) = Make_Integer (y); t = Cdr (t);
    Car (t) = ret ? True : False; t = Cdr (t);
    z = Make_Window (0, WINDOW(win)->dpy, root);
    Car (t) = z; t = Cdr (t);
    Car (t) = Make_Integer (r_x); t = Cdr (t);
    Car (t) = Make_Integer (r_y); t = Cdr (t);
    z = Make_Window (0, WINDOW(win)->dpy, child);
    Car (t) = z; t = Cdr (t);
    z = Bits_To_Symbols ((unsigned long)mask, 1, State_Syms);
    Car (t) = z;
    GC_Unlink;
    return l;
}

elk_init_xlib_window () {
    Define_Symbol (&Sym_Set_Attr, "set-window-attributes");
    Define_Symbol (&Sym_Get_Attr, "get-window-attributes");
    Define_Symbol (&Sym_Conf, "window-configuration");
    Define_Symbol (&Sym_Geo, "geometry");
    Generic_Define (Window, "window", "window?");
    Define_Primitive (P_Window_Display,   "window-display",   1, 1, EVAL);
    Define_Primitive (P_Create_Window,
                        "xlib-create-window",                 7, 7, EVAL);
    Define_Primitive (P_Configure_Window,
                        "xlib-configure-window",              2, 2, EVAL);
    Define_Primitive (P_Change_Window_Attributes,
                        "xlib-change-window-attributes",      2, 2, EVAL);
    Define_Primitive (P_Get_Window_Attributes,
                        "xlib-get-window-attributes",         1, 1, EVAL);
    Define_Primitive (P_Get_Geometry,     "xlib-get-geometry",1, 1, EVAL);
    Define_Primitive (P_Map_Window,       "map-window",       1, 1, EVAL);
    Define_Primitive (P_Unmap_Window,     "unmap-window",     1, 1, EVAL);
    Define_Primitive (P_Circulate_Subwindows,
                        "circulate-subwindows",               2, 2, EVAL);
    Define_Primitive (P_Destroy_Window,   "destroy-window",   1, 1, EVAL);
    Define_Primitive (P_Destroy_Subwindows,
                        "destroy-subwindows",                 1, 1, EVAL);
    Define_Primitive (P_Map_Subwindows,   "map-subwindows",   1, 1, EVAL);
    Define_Primitive (P_Unmap_Subwindows, "unmap-subwindows", 1, 1, EVAL);
    Define_Primitive (P_Query_Tree,       "query-tree",       1, 1, EVAL);
    Define_Primitive (P_Translate_Coordinates,
                        "translate-coordinates",              4, 4, EVAL);
    Define_Primitive (P_Query_Pointer,    "query-pointer",    1, 1, EVAL);
}
