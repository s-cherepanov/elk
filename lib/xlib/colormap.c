#include "xlib.h"

Generic_Predicate (Colormap)

Generic_Equal_Dpy (Colormap, COLORMAP, cm)

Generic_Print (Colormap, "#[colormap %lu]", COLORMAP(x)->cm)

Generic_Get_Display (Colormap, COLORMAP)

Object Make_Colormap (finalize, dpy, cmap) Display *dpy; Colormap cmap; {
    Object cm;

    if (cmap == None)
	return Sym_None;
    cm = Find_Object (T_Colormap, (GENERIC)dpy, Match_X_Obj, cmap);
    if (Nullp (cm)) {
	cm = Alloc_Object (sizeof (struct S_Colormap), T_Colormap, 0);
	COLORMAP(cm)->tag = Null;
	COLORMAP(cm)->cm = cmap;
	COLORMAP(cm)->dpy = dpy;
	COLORMAP(cm)->free = 0;
	Register_Object (cm, (GENERIC)dpy, finalize ? P_Free_Colormap :
	    (PFO)0, 0);
    }
    return cm;
}

Colormap Get_Colormap (c) Object c; {
    Check_Type (c, T_Colormap);
    return COLORMAP(c)->cm;
}

Object P_Free_Colormap (c) Object c; {
    Check_Type (c, T_Colormap);
    if (!COLORMAP(c)->free)
	XFreeColormap (COLORMAP(c)->dpy, COLORMAP(c)->cm);
    Deregister_Object (c);
    COLORMAP(c)->free = 1;
    return Void;
}

static Object P_Alloc_Color (cmap, color) Object cmap, color; {
    XColor c;
    Colormap cm = Get_Colormap (cmap);
    int r;
    
    c = *Get_Color (color);
    Disable_Interrupts;
    r = XAllocColor (COLORMAP(cmap)->dpy, cm, &c);
    Enable_Interrupts;
    if (!r)
	return False;
    return Make_Pixel (c.pixel);
}

static Object P_Alloc_Named_Color (cmap, name) Object cmap, name; {
    Colormap cm = Get_Colormap (cmap);
    XColor screen, exact;
    int r;
    Object ret, t, x;
    GC_Node2;

    Disable_Interrupts;
    r = XAllocNamedColor (COLORMAP(cmap)->dpy, cm, Get_Strsym (name),
	&screen, &exact);
    Enable_Interrupts;
    if (!r)
	return False;
    t = ret = P_Make_List (Make_Integer (3), Null);
    GC_Link2 (t, ret);
    x = Make_Pixel (screen.pixel);
    Car (t) = x; t = Cdr (t);
    x = Make_Color (screen.red, screen.green, screen.blue);
    Car (t) = x; t = Cdr (t);
    x = Make_Color (exact.red, exact.green, exact.blue);
    Car (t) = x;
    GC_Unlink;
    return ret;
}

elk_init_xlib_colormap () {
    Generic_Define (Colormap, "colormap", "colormap?");
    Define_Primitive (P_Colormap_Display, "colormap-display", 1, 1, EVAL);
    Define_Primitive (P_Free_Colormap,    "free-colormap",    1, 1, EVAL);
    Define_Primitive (P_Alloc_Color,      "alloc-color",      2, 2, EVAL);
    Define_Primitive (P_Alloc_Named_Color,"alloc-named-color",2, 2, EVAL);
}
