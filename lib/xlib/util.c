#include "xlib.h"

static Object P_Get_Default (d, program, option) Object d, program, option; {
    register char *ret;

    Check_Type (d, T_Display);
    if (ret = XGetDefault (DISPLAY(d)->dpy, Get_Strsym (program),
            Get_Strsym (option)))
        return Make_String (ret, strlen (ret));
    return False;
}

static Object P_Resource_Manager_String (d) Object d; {
    register char *ret;

    Check_Type (d, T_Display);
    ret = XResourceManagerString (DISPLAY(d)->dpy);
    return ret ? Make_String (ret, strlen (ret)) : False;
}

static Object P_Parse_Geometry (string) Object string; {
    Object ret, t;
    register mask;
    int x, y;
    unsigned w, h;

    mask = XParseGeometry (Get_Strsym (string), &x, &y, &w, &h);
    t = ret = P_Make_List (Make_Integer (6), False);
    if (mask & XNegative) Car (t) = True; t = Cdr (t);
    if (mask & YNegative) Car (t) = True; t = Cdr (t);
    if (mask & XValue) Car (t) = Make_Integer (x); t = Cdr (t);
    if (mask & YValue) Car (t) = Make_Integer (y); t = Cdr (t);
    if (mask & WidthValue) Car (t) = Make_Unsigned (w); t = Cdr (t);
    if (mask & HeightValue) Car (t) = Make_Unsigned (h);
    return ret;
}

static Object P_Parse_Color (d, cmap, spec) Object d, cmap, spec; {
    XColor ret;

    Check_Type (d, T_Display);
    if (XParseColor (DISPLAY(d)->dpy, Get_Colormap (cmap), Get_Strsym (spec),
            &ret))
        return Make_Color (ret.red, ret.green, ret.blue);
    return False;
}

elk_init_xlib_util () {
    Define_Primitive (P_Get_Default,       "get-default",         3, 3, EVAL);
    Define_Primitive (P_Resource_Manager_String,
                        "resource-manager-string",                1, 1, EVAL);
    Define_Primitive (P_Parse_Geometry,    "parse-geometry",      1, 1, EVAL);
    Define_Primitive (P_Parse_Color,       "parse-color",         3, 3, EVAL);
}
