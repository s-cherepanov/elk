#include "xlib.h"

extern XDrawPoints(), XDrawLines(), XDrawRectangle(), XFillRectangle();
extern XDrawRectangles(), XFillRectangles(), XDrawArc(), XFillArc();
extern XDrawArcs(), XFillArcs(), XFillPolygon();

static Object P_Clear_Area (win, x, y, w, h, e) Object win, x, y, w, h, e; {
    Check_Type (win, T_Window);
    Check_Type (e, T_Boolean);
    XClearArea (WINDOW(win)->dpy, WINDOW(win)->win, Get_Integer (x),
	Get_Integer (y), Get_Integer (w), Get_Integer (h), EQ(e, True));
    return Void;
}

static Object P_Copy_Area (src, gc, sx, sy, w, h, dst, dx, dy) Object src, gc,
	sx, sy, w, h, dst, dx, dy; {
    Display *dpy;
    Drawable ddst = Get_Drawable (dst, &dpy), dsrc = Get_Drawable (src, &dpy);

    Check_Type (gc, T_Gc);
    XCopyArea (dpy, dsrc, ddst, GCONTEXT(gc)->gc, Get_Integer (sx),
	Get_Integer (sy), Get_Integer (w), Get_Integer (h),
	Get_Integer (dx), Get_Integer (dy));
    return Void;
}

static Object P_Copy_Plane (src, gc, plane, sx, sy, w, h, dst, dx, dy)
	Object src, gc, plane, sx, sy, w, h, dst, dx, dy; {
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

static Object P_Draw_Point (d, gc, x, y) Object d, gc, x, y; {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    XDrawPoint (dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x), Get_Integer (y));
    return Void;
}

static Object Internal_Draw_Points (d, gc, v, relative, func, shape)
	Object d, gc, v, relative, shape; int (*func)(); {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XPoint *p;
    register i, n;
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

static Object P_Draw_Points (d, gc, v, relative) Object d, gc, v, relative; {
    return Internal_Draw_Points (d, gc, v, relative, XDrawPoints, Null);
}

static Object P_Draw_Line (d, gc, x1, y1, x2, y2)
	Object d, gc, x1, y1, x2, y2; {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    XDrawLine (dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x1), Get_Integer (y1),
	Get_Integer (x2), Get_Integer (y2));
    return Void;
}

static Object P_Draw_Lines (d, gc, v, relative) Object d, gc, v, relative; {
    return Internal_Draw_Points (d, gc, v, relative, XDrawLines, Null);
}

static Object P_Draw_Segments (d, gc, v) Object d, gc, v; {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XSegment *p;
    register i, n;
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

static Object Internal_Draw_Rectangle (d, gc, x, y, w, h, func)
	Object d, gc, x, y, w, h; int (*func)(); {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    (*func)(dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x),
	Get_Integer (y), Get_Integer (w), Get_Integer (h));
    return Void;
}

static Object P_Draw_Rectangle (d, gc, x, y, w, h) Object d, gc, x, y, w, h; {
    return Internal_Draw_Rectangle (d, gc, x, y, w, h, XDrawRectangle);
}

static Object P_Fill_Rectangle (d, gc, x, y, w, h) Object d, gc, x, y, w, h; {
    return Internal_Draw_Rectangle (d, gc, x, y, w, h, XFillRectangle);
}

static Object Internal_Draw_Rectangles (d, gc, v, func)
	Object d, gc, v; int (*func)(); {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XRectangle *p;
    register i, n;
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

static Object P_Draw_Rectangles (d, gc, v) Object d, gc, v; {
    return Internal_Draw_Rectangles (d, gc, v, XDrawRectangles);
}

static Object P_Fill_Rectangles (d, gc, v) Object d, gc, v; {
    return Internal_Draw_Rectangles (d, gc, v, XFillRectangles);
}

static Object Internal_Draw_Arc (d, gc, x, y, w, h, a1, a2, func)
	Object d, gc, x, y, w, h, a1, a2; int (*func)(); {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);

    Check_Type (gc, T_Gc);
    (*func)(dpy, dr, GCONTEXT(gc)->gc, Get_Integer (x), Get_Integer (y),
	Get_Integer (w), Get_Integer (h), Get_Integer (a1), Get_Integer (a2));
    return Void;
}

static Object P_Draw_Arc (d, gc, x, y, w, h, a1, a2)
	Object d, gc, x, y, w, h, a1, a2; {
    return Internal_Draw_Arc (d, gc, x, y, w, h, a1, a2, XDrawArc);
}

static Object P_Fill_Arc (d, gc, x, y, w, h, a1, a2)
	Object d, gc, x, y, w, h, a1, a2; {
    return Internal_Draw_Arc (d, gc, x, y, w, h, a1, a2, XFillArc);
}

static Object Internal_Draw_Arcs (d, gc, v, func) Object d, gc, v;
	int (*func)(); {
    Display *dpy;
    Drawable dr = Get_Drawable (d, &dpy);
    register XArc *p;
    register i, n;
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

static Object P_Draw_Arcs (d, gc, v) Object d, gc, v; {
    return Internal_Draw_Arcs (d, gc, v, XDrawArcs);
}

static Object P_Fill_Arcs (d, gc, v) Object d, gc, v; {
    return Internal_Draw_Arcs (d, gc, v, XFillArcs);
}

static Object P_Fill_Polygon (d, gc, v, relative, shape)
	Object d, gc, v, relative, shape; {
    return Internal_Draw_Points (d, gc, v, relative, XFillPolygon, shape);
}

elk_init_xlib_graphics () {
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
