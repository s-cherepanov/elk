#include "xlib.h"

Generic_Predicate (Pixel)

Generic_Simple_Equal (Pixel, PIXEL, pix)

Generic_Print (Pixel, "#[pixel 0x%lx]", PIXEL(x)->pix)

Object Make_Pixel (val) unsigned long val; {
    Object pix;

    pix = Find_Object (T_Pixel, (GENERIC)0, Match_X_Obj, val);
    if (Nullp (pix)) {
	pix = Alloc_Object (sizeof (struct S_Pixel), T_Pixel, 0);
	PIXEL(pix)->tag = Null;
	PIXEL(pix)->pix = val;
	Register_Object (pix, (GENERIC)0, (PFO)0, 0);
    }
    return pix;
}

unsigned long Get_Pixel (p) Object p; {
    Check_Type (p, T_Pixel);
    return PIXEL(p)->pix;
}

static Object P_Pixel_Value (p) Object p; {
    return Make_Unsigned_Long (Get_Pixel (p));
}

static Object P_Black_Pixel (d) Object d; {
    Check_Type (d, T_Display);
    return Make_Pixel (BlackPixel (DISPLAY(d)->dpy,
	DefaultScreen (DISPLAY(d)->dpy)));
}

static Object P_White_Pixel (d) Object d; {
    Check_Type (d, T_Display);
    return Make_Pixel (WhitePixel (DISPLAY(d)->dpy, 
	DefaultScreen (DISPLAY(d)->dpy)));
}

elk_init_xlib_pixel () {
    Generic_Define (Pixel, "pixel", "pixel?");
    Define_Primitive (P_Pixel_Value,   "pixel-value",    1, 1, EVAL);
    Define_Primitive (P_Black_Pixel,   "black-pixel",    1, 1, EVAL);
    Define_Primitive (P_White_Pixel,   "white-pixel",    1, 1, EVAL);
}
