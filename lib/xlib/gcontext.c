/* gcontext.c
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

static Object Sym_Gc;

Generic_Predicate (Gc)

Generic_Equal_Dpy (Gc, GCONTEXT, gc)

Generic_Print (Gc, "#[gcontext %lu]", (unsigned int)(uintptr_t)GCONTEXT(x)->gc)

Generic_Get_Display (Gc, GCONTEXT)

Object Make_Gc (int finalize, Display *dpy, GC g) {
    Object gc;

    if (g == None)
        return Sym_None;
    gc = Find_Object (T_Gc, (GENERIC)dpy, Match_X_Obj, g);
    if (Nullp (gc)) {
        gc = Alloc_Object (sizeof (struct S_Gc), T_Gc, 0);
        GCONTEXT(gc)->tag = Null;
        GCONTEXT(gc)->gc = g;
        GCONTEXT(gc)->dpy = dpy;
        GCONTEXT(gc)->free = 0;
        Register_Object (gc, (GENERIC)dpy, finalize ? P_Free_Gc :
            (PFO)0, 0);
    }
    return gc;
}

static Object P_Create_Gc (Object w, Object g) {
    unsigned long mask;
    Display *dpy;
    Drawable dr;

    dr = Get_Drawable (w, &dpy);
    mask = Vector_To_Record (g, GC_Size, Sym_Gc, GC_Rec);
    return Make_Gc (1, dpy, XCreateGC (dpy, dr, mask, &GCV));
}

static Object P_Copy_Gc (Object gc, Object w) {
    GC dst;
    Display *dpy;
    Drawable dr;

    Check_Type (gc, T_Gc);
    dr = Get_Drawable (w, &dpy);
    dst = XCreateGC (dpy, dr, 0L, &GCV);
    XCopyGC (dpy, GCONTEXT(gc)->gc, ~0L, dst);
    return Make_Gc (1, dpy, dst);
}

static Object P_Change_Gc (Object gc, Object g) {
    unsigned long mask;

    Check_Type (gc, T_Gc);
    mask = Vector_To_Record (g, GC_Size, Sym_Gc, GC_Rec);
    XChangeGC (GCONTEXT(gc)->dpy, GCONTEXT(gc)->gc, mask, &GCV);
    return Void;
}

Object P_Free_Gc (Object g) {
    Check_Type (g, T_Gc);
    if (!GCONTEXT(g)->free)
        XFreeGC (GCONTEXT(g)->dpy, GCONTEXT(g)->gc);
    Deregister_Object (g);
    GCONTEXT(g)->free = 1;
    return Void;
}

static Object P_Query_Best_Size (Object d, Object w, Object h, Object shape) {
    unsigned int rw, rh;

    Check_Type (d, T_Display);
    if (!XQueryBestSize (DISPLAY(d)->dpy, Symbols_To_Bits (shape, 0,
            Shape_Syms), DefaultRootWindow (DISPLAY(d)->dpy),
            Get_Integer (w), Get_Integer (h), &rw, &rh))
        Primitive_Error ("cannot query best shape");
    return Cons (Make_Integer (rw), Make_Integer (rh));
}

static Object P_Set_Gcontext_Clip_Rectangles (Object gc, Object x, Object y,
                                              Object v, Object ord) {
    register XRectangle *p;
    register int i, n;
    Alloca_Begin;

    Check_Type (gc, T_Gc);
    Check_Type (v, T_Vector);
    n = VECTOR(v)->size;
    Alloca (p, XRectangle*, n * sizeof (XRectangle));
    for (i = 0; i < n; i++) {
        Object rect;

        rect = VECTOR(v)->data[i];
        Check_Type (rect, T_Pair);
        if (Fast_Length (rect) != 4)
            Primitive_Error ("invalid rectangle: ~s", rect);
        p[i].x = Get_Integer (Car (rect)); rect = Cdr (rect);
        p[i].y = Get_Integer (Car (rect)); rect = Cdr (rect);
        p[i].width = Get_Integer (Car (rect)); rect = Cdr (rect);
        p[i].height = Get_Integer (Car (rect));
    }
    XSetClipRectangles (GCONTEXT(gc)->dpy, GCONTEXT(gc)->gc, Get_Integer (x),
        Get_Integer (y), p, n, Symbols_To_Bits (ord, 0, Ordering_Syms));
    Alloca_End;
    return Void;
}

static Object P_Set_Gcontext_Dashlist (Object gc, Object off, Object v) {
    register char *p;
    register int i, n, d;
    Alloca_Begin;

    Check_Type (gc, T_Gc);
    Check_Type (v, T_Vector);
    n = VECTOR(v)->size;
    Alloca (p, char*, n);
    for (i = 0; i < n; i++) {
        d = Get_Integer (VECTOR(v)->data[i]);
        if (d < 0 || d > 255)
            Range_Error (VECTOR(v)->data[i]);
        p[i] = d;
    }
    XSetDashes (GCONTEXT(gc)->dpy, GCONTEXT(gc)->gc, Get_Integer (off), p, n);
    Alloca_End;
    return Void;
}

#define ValidGCValuesBits \
    (GCFunction | GCPlaneMask | GCForeground | GCBackground | GCLineWidth |\
    GCLineStyle | GCCapStyle | GCJoinStyle | GCFillStyle | GCFillRule |\
    GCTile | GCStipple | GCTileStipXOrigin | GCTileStipYOrigin | GCFont |\
    GCSubwindowMode | GCGraphicsExposures | GCClipXOrigin | GCClipYOrigin |\
    GCDashOffset | GCArcMode)

static Object P_Get_Gc_Values (Object gc) {
    unsigned long mask = ValidGCValuesBits;

    Check_Type (gc, T_Gc);
    if (!XGetGCValues (GCONTEXT(gc)->dpy, GCONTEXT(gc)->gc, mask, &GCV))
        Primitive_Error ("cannot get gcontext values");
    return Record_To_Vector (GC_Rec, GC_Size, Sym_Gc, GCONTEXT(gc)->dpy,
        mask);
}

void elk_init_xlib_gcontext () {
    Define_Symbol (&Sym_Gc, "gcontext");
    Generic_Define (Gc, "gcontext", "gcontext?");
    Define_Primitive (P_Gc_Display,      "gcontext-display",    1, 1, EVAL);
    Define_Primitive (P_Create_Gc,       "xlib-create-gcontext",2, 2, EVAL);
    Define_Primitive (P_Copy_Gc,         "copy-gcontext",       2, 2, EVAL);
    Define_Primitive (P_Change_Gc,       "xlib-change-gcontext",2, 2, EVAL);
    Define_Primitive (P_Free_Gc,         "free-gcontext",       1, 1, EVAL);
    Define_Primitive (P_Query_Best_Size, "query-best-size",     4, 4, EVAL);
    Define_Primitive (P_Set_Gcontext_Clip_Rectangles,
        "set-gcontext-clip-rectangles!",                        5, 5, EVAL);
    Define_Primitive (P_Set_Gcontext_Dashlist,
        "set-gcontext-dashlist!",                               3, 3, EVAL);
    Define_Primitive (P_Get_Gc_Values,
                        "xlib-get-gcontext-values",             1, 1, EVAL);
}
