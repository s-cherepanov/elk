/* graphics.c
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

extern int XDrawPoints(), XDrawLines(), XDrawRectangle(), XFillRectangle();
extern int XDrawRectangles(), XFillRectangles(), XDrawArc(), XFillArc();
extern int XDrawArcs(), XFillArcs(), XFillPolygon();

static Object P_Clear_Area (Object win, Object x, Object y, Object w, Object h,
                            Object e) {
    Check_Type (win, T_Window);
    Check_Type (e, T_Boolean);
    XClearArea (WINDOW(win)->dpy, WINDOW(win)->win, Get_Integer (x),
        Get_Integer (y), Get_Integer (w), Get_Integer (h), EQ(e, True));
    return Void;
}

static Object P_Copy_Area (Object src, Object gc, Object sx, Object sy,
                           Object w, Object h, Object dst, Object dx,
                           Object dy) {
    Display *dpy;
    Drawable ddst = Get_Drawable (dst, &dpy), dsrc = Get_Drawable (src, &dpy);

    Check_Type (gc, T_Gc);
    XCopyArea (dpy, dsrc, ddst, GCONTEXT(gc)->gc, Get_Integer (sx),
        Get_Integer (sy), Get_Integer (w), Get_Integer (h),
        Get_Integer (dx), Get_Integer (dy));
    return Void;
}

static Object P_Copy_Plane (Object src, Object gc, Object plane, Object sx,
                            Object sy, Object w, Object h, Object dst,
                            Object dx, Object dy) {
    Display *dpy;
    Drawable ddst = Get_Drawable (dst, &dpy), dsrc = Get_Drawable (src, &dpy);
    register unsigned long p;

    Check_Type (gc, T_Gc);
    p = (unsigned long)Get_Long (plane);
    if (p & (p-1))
        Primitive_Error ("invalid plane: ~s", plane);
    XCopyPlane (dpy, dsrc, ddst, GCONTEXT(gc)->gc, Get_Integer (sx),
        Get_Integer (sy), Get_Integer (w), Get_Integer (h),
        Get_Integer (dx), Get_Integer (dy), p);
    return Void;
}

static Object P_Draw_Point (Object d, Object gc, Object x, Object y) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    XDrawPoint (dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x), Get_Integer (y));
    return Void;
}

static Object Internal_Draw_Points (Object d, Object gc, Object v,
                                    Object relative,
                                    int (*func)(), Object shape) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XPoint *p;
    register int i, n;
    int rel, sh;
    Alloca_Begin;

    Check_Type (gc, T_Gc);
    Check_Type (relative, T_Boolean);
    rel = EQ(relative, True) ? CoordModePrevious : CoordModeOrigin;
    if (func == XFillPolygon)
        sh = Symbols_To_Bits (shape, 0, Polyshape_Syms);
    n = VECTOR(v)->size;
    Alloca (p, XPoint*, n * sizeof (XPoint));
    for (i = 0; i < n; i++) {
        Object point;

        point = VECTOR(v)->data[i];
        Check_Type (point, T_Pair);
        p[i].x = Get_Integer (Car (point));
        p[i].y = Get_Integer (Cdr (point));
    }
    if (func == XFillPolygon)
        XFillPolygon (dpy, dr, GCONTEXT(gc)->gc, p, n, sh, rel);
    else
        (*func)(dpy, dr, GCONTEXT(gc)->gc, p, n, rel);
    Alloca_End;
    return Void;
}

static Object P_Draw_Points (Object d, Object gc, Object v, Object relative) {
    return Internal_Draw_Points (d, gc, v, relative, XDrawPoints, Null);
}

static Object P_Draw_Line (Object d, Object gc, Object x1, Object y1,
                           Object x2, Object y2) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    XDrawLine (dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x1), Get_Integer (y1),
        Get_Integer (x2), Get_Integer (y2));
    return Void;
}

static Object P_Draw_Lines (Object d, Object gc, Object v, Object relative) {
    return Internal_Draw_Points (d, gc, v, relative, XDrawLines, Null);
}

static Object P_Draw_Segments (Object d, Object gc, Object v) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XSegment *p;
    register int i, n;
    Alloca_Begin;

    Check_Type (gc, T_Gc);
    n = VECTOR(v)->size;
    Alloca (p, XSegment*, n * sizeof (XSegment));
    for (i = 0; i < n; i++) {
        Object seg;

        seg = VECTOR(v)->data[i];
        Check_Type (seg, T_Pair);
        if (Fast_Length (seg) != 4)
            Primitive_Error ("invalid segment: ~s", seg);
        p[i].x1 = Get_Integer (Car (seg)); seg = Cdr (seg);
        p[i].y1 = Get_Integer (Car (seg)); seg = Cdr (seg);
        p[i].x2 = Get_Integer (Car (seg)); seg = Cdr (seg);
        p[i].y2 = Get_Integer (Car (seg));
    }
    XDrawSegments (dpy, dr, GCONTEXT(gc)->gc, p, n);
    Alloca_End;
    return Void;
}

static Object Internal_Draw_Rectangle (Object d, Object gc, Object x, Object y,
                                       Object w, Object h, int (*func)()) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    (*func)(dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x),
        Get_Integer (y), Get_Integer (w), Get_Integer (h));
    return Void;
}

static Object P_Draw_Rectangle (Object d, Object gc, Object x, Object y,
                                Object w, Object h) {
    return Internal_Draw_Rectangle (d, gc, x, y, w, h, XDrawRectangle);
}

static Object P_Fill_Rectangle (Object d, Object gc, Object x, Object y,
                                Object w, Object h) {
    return Internal_Draw_Rectangle (d, gc, x, y, w, h, XFillRectangle);
}

static Object Internal_Draw_Rectangles (Object d, Object gc, Object v,
                                        int (*func)()) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XRectangle *p;
    register int i, n;
    Alloca_Begin;

    Check_Type (gc, T_Gc);
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
    (*func)(dpy, dr, GCONTEXT(gc)->gc, p, n);
    Alloca_End;
    return Void;
}

static Object P_Draw_Rectangles (Object d, Object gc, Object v) {
    return Internal_Draw_Rectangles (d, gc, v, XDrawRectangles);
}

static Object P_Fill_Rectangles (Object d, Object gc, Object v) {
    return Internal_Draw_Rectangles (d, gc, v, XFillRectangles);
}

static Object Internal_Draw_Arc (Object d, Object gc, Object x, Object y,
                                 Object w, Object h, Object a1, Object a2,
                                 int (*func)()) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    (*func)(dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x), Get_Integer (y),
        Get_Integer (w), Get_Integer (h), Get_Integer (a1), Get_Integer (a2));
    return Void;
}

static Object P_Draw_Arc (Object d, Object gc, Object x, Object y, Object w,
                          Object h, Object a1, Object a2) {
    return Internal_Draw_Arc (d, gc, x, y, w, h, a1, a2, XDrawArc);
}

static Object P_Fill_Arc (Object d, Object gc, Object x, Object y, Object w,
                          Object h, Object a1, Object a2) {
    return Internal_Draw_Arc (d, gc, x, y, w, h, a1, a2, XFillArc);
}

static Object Internal_Draw_Arcs (Object d, Object gc, Object v,
                                  int (*func)()) {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XArc *p;
    register int i, n;
    Alloca_Begin;

    Check_Type (gc, T_Gc);
    n = VECTOR(v)->size;
    Alloca (p, XArc*, n * sizeof (XArc));
    for (i = 0; i < n; i++) {
        Object arc;

        arc = VECTOR(v)->data[i];
        Check_Type (arc, T_Pair);
        if (Fast_Length (arc) != 6)
            Primitive_Error ("invalid arc: ~s", arc);
        p[i].x = Get_Integer (Car (arc)); arc = Cdr (arc);
        p[i].y = Get_Integer (Car (arc)); arc = Cdr (arc);
        p[i].width = Get_Integer (Car (arc)); arc = Cdr (arc);
        p[i].height = Get_Integer (Car (arc)); arc = Cdr (arc);
        p[i].angle1 = Get_Integer (Car (arc)); arc = Cdr (arc);
        p[i].angle2 = Get_Integer (Car (arc));
    }
    (*func)(dpy, dr, GCONTEXT(gc)->gc, p, n);
    Alloca_End;
    return Void;
}

static Object P_Draw_Arcs (Object d, Object gc, Object v) {
    return Internal_Draw_Arcs (d, gc, v, XDrawArcs);
}

static Object P_Fill_Arcs (Object d, Object gc, Object v) {
    return Internal_Draw_Arcs (d, gc, v, XFillArcs);
}

static Object P_Fill_Polygon (Object d, Object gc, Object v, Object relative,
                              Object shape) {
    return Internal_Draw_Points (d, gc, v, relative, XFillPolygon, shape);
}

void elk_init_xlib_graphics () {
    Define_Primitive (P_Clear_Area,        "clear-area",       6, 6, EVAL);
    Define_Primitive (P_Copy_Area,         "copy-area",        9, 9, EVAL);
    Define_Primitive (P_Copy_Plane,        "copy-plane",      10,10, EVAL);
    Define_Primitive (P_Draw_Point,        "draw-point",       4, 4, EVAL);
    Define_Primitive (P_Draw_Points,       "draw-points",      4, 4, EVAL);
    Define_Primitive (P_Draw_Line,         "draw-line",        6, 6, EVAL);
    Define_Primitive (P_Draw_Lines,        "draw-lines",       4, 4, EVAL);
    Define_Primitive (P_Draw_Segments,     "draw-segments",    3, 3, EVAL);
    Define_Primitive (P_Draw_Rectangle,    "draw-rectangle",   6, 6, EVAL);
    Define_Primitive (P_Fill_Rectangle,    "fill-rectangle",   6, 6, EVAL);
    Define_Primitive (P_Draw_Rectangles,   "draw-rectangles",  3, 3, EVAL);
    Define_Primitive (P_Fill_Rectangles,   "fill-rectangles",  3, 3, EVAL);
    Define_Primitive (P_Draw_Arc,          "draw-arc",         8, 8, EVAL);
    Define_Primitive (P_Fill_Arc,          "fill-arc",         8, 8, EVAL);
    Define_Primitive (P_Draw_Arcs,         "draw-arcs",        3, 3, EVAL);
    Define_Primitive (P_Fill_Arcs,         "fill-arcs",        3, 3, EVAL);
    Define_Primitive (P_Fill_Polygon,      "fill-polygon",     5, 5, EVAL);
}
