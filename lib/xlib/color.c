/* color.c
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

Generic_Predicate (Color)

static int Color_Equal (Object x, Object y) {
    register XColor *p = &COLOR(x)->c, *q = &COLOR(y)->c;
    return p->red == q->red && p->green == q->green && p->blue == q->blue;
}

Generic_Print (Color, "#[color %lu]", (unsigned int)(uintptr_t)POINTER(x))

Object Make_Color (unsigned int r, unsigned int g, unsigned int b) {
    Object c;

    c = Find_Object (T_Color, (GENERIC)0, Match_X_Obj, r, g, b);
    if (Nullp (c)) {
        c = Alloc_Object (sizeof (struct S_Color), T_Color, 0);
        COLOR(c)->tag = Null;
        COLOR(c)->c.red = r;
        COLOR(c)->c.green = g;
        COLOR(c)->c.blue = b;
        Register_Object (c, (GENERIC)0, (PFO)0, 0);
    }
    return c;
}

XColor *Get_Color (Object c) {
    Check_Type (c, T_Color);
    return &COLOR(c)->c;
}

static unsigned short Get_RGB_Value (Object x) {
    double d;

    d = Get_Double (x);
    if (d < 0.0 || d > 1.0)
        Primitive_Error ("bad RGB value: ~s", x);
    return (unsigned short)(d * 65535);
}

static Object P_Make_Color (Object r, Object g, Object b) {
    return Make_Color (Get_RGB_Value (r), Get_RGB_Value (g), Get_RGB_Value (b));
}

static Object P_Color_Rgb_Values (Object c) {
    Object ret, t, x;
    GC_Node3;

    Check_Type (c, T_Color);
    ret = t = Null;
    GC_Link3 (c, ret, t);
    t = ret = P_Make_List (Make_Integer (3), Null);
    GC_Unlink;
    x = Make_Reduced_Flonum ((double)COLOR(c)->c.red / 65535.0);
    Car (t) = x; t = Cdr (t);
    x = Make_Reduced_Flonum ((double)COLOR(c)->c.green / 65535.0);
    Car (t) = x; t = Cdr (t);
    x = Make_Reduced_Flonum ((double)COLOR(c)->c.blue / 65535.0);
    Car (t) = x;
    return ret;
}

static Object P_Query_Color (Object cmap, Object p) {
    XColor c;
    Colormap cm = Get_Colormap (cmap);

    c.pixel = Get_Pixel (p);
    Disable_Interrupts;
    XQueryColor (COLORMAP(cmap)->dpy, cm, &c);
    Enable_Interrupts;
    return Make_Color (c.red, c.green, c.blue);
}

static Object P_Query_Colors (Object cmap, Object v) {
    Colormap cm = Get_Colormap (cmap);
    register int i, n;
    Object ret;
    register XColor *p;
    GC_Node;
    Alloca_Begin;

    Check_Type (v, T_Vector);
    n = VECTOR(v)->size;
    Alloca (p, XColor*, n * sizeof (XColor));
    for (i = 0; i < n; i++)
        p[i].pixel = Get_Pixel (VECTOR(v)->data[i]);
    Disable_Interrupts;
    XQueryColors (COLORMAP(cmap)->dpy, cm, p, n);
    Enable_Interrupts;
    ret = Make_Vector (n, Null);
    GC_Link (ret);
    for (i = 0; i < n; i++, p++) {
        Object x;

        x = Make_Color (p->red, p->green, p->blue);
        VECTOR(ret)->data[i] = x;
    }
    GC_Unlink;
    Alloca_End;
    return ret;
}

static Object P_Lookup_Color (Object cmap, Object name) {
    XColor visual, exact;
    Colormap cm = Get_Colormap (cmap);
    Object ret, x;
    GC_Node;

    if (!XLookupColor (COLORMAP(cmap)->dpy, cm, Get_Strsym (name),
            &visual, &exact))
        Primitive_Error ("no such color: ~s", name);
    ret = Cons (Null, Null);
    GC_Link (ret);
    x = Make_Color (visual.red, visual.green, visual.blue);
    Car (ret) = x;
    x = Make_Color (exact.red, exact.green, exact.blue);
    Cdr (ret) = x;
    GC_Unlink;
    return ret;
}

void elk_init_xlib_color () {
    Generic_Define (Color, "color", "color?");
    Define_Primitive (P_Make_Color,       "make-color",       3, 3, EVAL);
    Define_Primitive (P_Color_Rgb_Values, "color-rgb-values", 1, 1, EVAL);
    Define_Primitive (P_Query_Color,      "query-color",      2, 2, EVAL);
    Define_Primitive (P_Query_Colors,     "query-colors",     2, 2, EVAL);
    Define_Primitive (P_Lookup_Color,     "lookup-color",     2, 2, EVAL);
}
