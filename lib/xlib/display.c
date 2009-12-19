/* display.c
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

#include <string.h>

static int Display_Visit (Object *dp, int (*f)()) {
    (*f)(&DISPLAY(*dp)->after);
    return 0;
}

Generic_Predicate (Display)

Generic_Equal (Display, DISPLAY, dpy)

static int Display_Print (Object d, Object port,
                          int raw, int depth, int length) {
    Printf (port, "#[display %lu %s]",
        (unsigned long)(uintptr_t)DISPLAY(d)->dpy,
        DisplayString (DISPLAY(d)->dpy));
    return 0;
}

Object Make_Display (int finalize, Display *dpy) {
    Object d;

    d = Find_Object (T_Display, (GENERIC)dpy, Match_X_Obj);
    if (Nullp (d)) {
        d = Alloc_Object (sizeof (struct S_Display), T_Display, 0);
        DISPLAY(d)->dpy = dpy;
        DISPLAY(d)->free = 0;
        DISPLAY(d)->after = False;
        Register_Object (d, (GENERIC)dpy, finalize ? P_Close_Display :
            (PFO)0, 1);
    }
    return d;
}

static Object P_Open_Display (int argc, Object *argv) {
    register char *s;
    Display *dpy;

    if (argc == 1) {
        if ((dpy = XOpenDisplay (Get_Strsym (argv[0]))) == 0)
            Primitive_Error ("cannot open display ~s", argv[0]);
    } else if ((dpy = XOpenDisplay ((char *)0)) == 0) {
        s = XDisplayName ((char *)0);
        Primitive_Error ("cannot open display ~s",
            Make_String (s, strlen (s)));
    }
    return Make_Display (1, dpy);
}

Object P_Close_Display (Object d) {
    register struct S_Display *p;

    Check_Type (d, T_Display);
    p = DISPLAY(d);
    if (!p->free) {
        Terminate_Group ((GENERIC)p->dpy);
        XCloseDisplay (p->dpy);
    }
    Deregister_Object (d);
    p->free = 1;
    return Void;
}

static Object P_Display_Default_Root_Window (Object d) {
    Check_Type (d, T_Display);
    return Make_Window (0, DISPLAY(d)->dpy,
        DefaultRootWindow (DISPLAY(d)->dpy));
}

static Object P_Display_Default_Colormap (Object d) {
    register Display *dpy;

    Check_Type (d, T_Display);
    dpy = DISPLAY(d)->dpy;
    return Make_Colormap (0, dpy, DefaultColormap (dpy, DefaultScreen (dpy)));
}

static Object P_Display_Default_Gcontext (Object d) {
    register Display *dpy;

    Check_Type (d, T_Display);
    dpy = DISPLAY(d)->dpy;
    return Make_Gc (0, dpy, DefaultGC (dpy, DefaultScreen (dpy)));
}

static Object P_Display_Default_Depth (Object d) {
    register Display *dpy;

    Check_Type (d, T_Display);
    dpy = DISPLAY(d)->dpy;
    return Make_Integer (DefaultDepth (dpy, DefaultScreen (dpy)));
}

static Object P_Display_Default_Screen_Number (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (DefaultScreen (DISPLAY(d)->dpy));
}

int Get_Screen_Number (Display *dpy, Object scr) {
    register int s;

    if ((s = Get_Integer (scr)) < 0 || s > ScreenCount (dpy)-1)
        Primitive_Error ("invalid screen number");
    return s;
}

static Object P_Display_Cells (Object d, Object scr) {
    Check_Type (d, T_Display);
    return Make_Integer (DisplayCells (DISPLAY(d)->dpy,
        Get_Screen_Number (DISPLAY(d)->dpy, scr)));
}

static Object P_Display_Planes (Object d, Object scr) {
    Check_Type (d, T_Display);
    return Make_Integer (DisplayPlanes (DISPLAY(d)->dpy,
        Get_Screen_Number (DISPLAY(d)->dpy, scr)));
}

static Object P_Display_String (Object d) {
    register char *s;

    Check_Type (d, T_Display);
    s = DisplayString (DISPLAY(d)->dpy);
    return Make_String (s, strlen (s));
}

static Object P_Display_Vendor (Object d) {
    register char *s;
    Object ret, name;
    GC_Node;

    Check_Type (d, T_Display);
    s = ServerVendor (DISPLAY(d)->dpy);
    name = Make_String (s, strlen (s));
    GC_Link (name);
    ret = Cons (Null, Make_Integer (VendorRelease (DISPLAY(d)->dpy)));
    Car (ret) = name;
    GC_Unlink;
    return ret;
}

static Object P_Display_Protocol_Version (Object d) {
    Check_Type (d, T_Display);
    return Cons (Make_Integer (ProtocolVersion (DISPLAY(d)->dpy)),
        Make_Integer (ProtocolRevision (DISPLAY(d)->dpy)));
}

static Object P_Display_Screen_Count (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (ScreenCount (DISPLAY(d)->dpy));
}

static Object P_Display_Image_Byte_Order (Object d) {
    Check_Type (d, T_Display);
    return Bits_To_Symbols ((unsigned long)ImageByteOrder (DISPLAY(d)->dpy),
        0, Byte_Order_Syms);
}

static Object P_Display_Bitmap_Unit (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (BitmapUnit (DISPLAY(d)->dpy));
}

static Object P_Display_Bitmap_Bit_Order (Object d) {
    Check_Type (d, T_Display);
    return Bits_To_Symbols ((unsigned long)BitmapBitOrder (DISPLAY(d)->dpy),
        0, Byte_Order_Syms);
}

static Object P_Display_Bitmap_Pad (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (BitmapPad (DISPLAY(d)->dpy));
}

static Object P_Display_Width (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (DisplayWidth (DISPLAY(d)->dpy,
        DefaultScreen (DISPLAY(d)->dpy)));
}

static Object P_Display_Height (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (DisplayHeight (DISPLAY(d)->dpy,
        DefaultScreen (DISPLAY(d)->dpy)));
}

static Object P_Display_Width_Mm (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (DisplayWidthMM (DISPLAY(d)->dpy,
        DefaultScreen (DISPLAY(d)->dpy)));
}

static Object P_Display_Height_Mm (Object d) {
    Check_Type (d, T_Display);
    return Make_Integer (DisplayHeightMM (DISPLAY(d)->dpy,
        DefaultScreen (DISPLAY(d)->dpy)));
}

static Object P_Display_Motion_Buffer_Size (Object d) {
    Check_Type (d, T_Display);
    return Make_Unsigned_Long (XDisplayMotionBufferSize (DISPLAY(d)->dpy));
}

static Object P_Display_Flush_Output (Object d) {
    Check_Type (d, T_Display);
    XFlush (DISPLAY(d)->dpy);
    return Void;
}

static Object P_Display_Wait_Output (Object d, Object discard) {
    Check_Type (d, T_Display);
    Check_Type (discard, T_Boolean);
    XSync (DISPLAY(d)->dpy, EQ(discard, True));
    return Void;
}

static Object P_No_Op (Object d) {
    Check_Type (d, T_Display);
    XNoOp (DISPLAY(d)->dpy);
    return Void;
}

static Object P_List_Depths (Object d, Object scr) {
    int num;
    register int *p, i;
    Object ret;

    Check_Type (d, T_Display);
    if (!(p = XListDepths (DISPLAY(d)->dpy,
            Get_Screen_Number (DISPLAY(d)->dpy, scr), &num)))
        return False;
    ret = Make_Vector (num, Null);
    for (i = 0; i < num; i++)
        VECTOR(ret)->data[i] = Make_Integer (p[i]);
    XFree ((char *)p);
    return ret;
}

static Object P_List_Pixmap_Formats (Object d) {
    register XPixmapFormatValues *p;
    int num;
    register int i;
    Object ret;
    GC_Node;

    Check_Type (d, T_Display);
    if (!(p = XListPixmapFormats (DISPLAY(d)->dpy, &num)))
        return False;
    ret = Make_Vector (num, Null);
    GC_Link (ret);
    for (i = 0; i < num; i++) {
        Object t;

        t = P_Make_List (Make_Integer (3), Null);
        VECTOR(ret)->data[i] = t;
        Car (t) = Make_Integer (p[i].depth); t = Cdr (t);
        Car (t) = Make_Integer (p[i].bits_per_pixel); t = Cdr (t);
        Car (t) = Make_Integer (p[i].scanline_pad);
    }
    GC_Unlink;
    XFree ((char *)p);
    return ret;
}

void elk_init_xlib_display () {
    T_Display = Define_Type (0, "display", NOFUNC, sizeof (struct S_Display),
        Display_Equal, Display_Equal, Display_Print, Display_Visit);
    Define_Primitive (P_Displayp,        "display?",         1, 1, EVAL);
    Define_Primitive (P_Open_Display,    "open-display",     0, 1, VARARGS);
    Define_Primitive (P_Close_Display,   "close-display",    1, 1, EVAL);
    Define_Primitive (P_Display_Default_Root_Window,
                        "display-default-root-window",       1, 1, EVAL);
    Define_Primitive (P_Display_Default_Colormap,
                        "display-default-colormap",          1, 1, EVAL);
    Define_Primitive (P_Display_Default_Gcontext,
                        "display-default-gcontext",          1, 1, EVAL);
    Define_Primitive (P_Display_Default_Depth,
                        "display-default-depth",             1, 1, EVAL);
    Define_Primitive (P_Display_Default_Screen_Number,
                        "display-default-screen-number",     1, 1, EVAL);
    Define_Primitive (P_Display_Cells,   "display-cells",    2, 2, EVAL);
    Define_Primitive (P_Display_Planes,  "display-planes",   2, 2, EVAL);
    Define_Primitive (P_Display_String,  "display-string",   1, 1, EVAL);
    Define_Primitive (P_Display_Vendor,  "display-vendor",   1, 1, EVAL);
    Define_Primitive (P_Display_Protocol_Version,
                        "display-protocol-version",          1, 1, EVAL);
    Define_Primitive (P_Display_Screen_Count,
                        "display-screen-count",              1, 1, EVAL);
    Define_Primitive (P_Display_Image_Byte_Order,
                        "display-image-byte-order",          1, 1, EVAL);
    Define_Primitive (P_Display_Bitmap_Unit,
                        "display-bitmap-unit",               1, 1, EVAL);
    Define_Primitive (P_Display_Bitmap_Bit_Order,
                        "display-bitmap-bit-order",          1, 1, EVAL);
    Define_Primitive (P_Display_Bitmap_Pad,
                        "display-bitmap-pad",                1, 1, EVAL);
    Define_Primitive (P_Display_Width,   "display-width",    1, 1, EVAL);
    Define_Primitive (P_Display_Height,  "display-height",   1, 1, EVAL);
    Define_Primitive (P_Display_Width_Mm,"display-width-mm", 1, 1, EVAL);
    Define_Primitive (P_Display_Height_Mm,
                        "display-height-mm",                 1, 1, EVAL);
    Define_Primitive (P_Display_Motion_Buffer_Size,
                        "display-motion-buffer-size",        1, 1, EVAL);
    Define_Primitive (P_Display_Flush_Output,
                        "display-flush-output",              1, 1, EVAL);
    Define_Primitive (P_Display_Wait_Output,
                        "display-wait-output",               2, 2, EVAL);
    Define_Primitive (P_No_Op,           "no-op",            1, 1, EVAL);
    Define_Primitive (P_List_Depths,      "list-depths",     2, 2, EVAL);
    Define_Primitive (P_List_Pixmap_Formats,
                        "list-pixmap-formats",               1, 1, EVAL);
}
