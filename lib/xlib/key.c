/* key.c
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

#ifdef XLIB_RELEASE_5_OR_LATER

/* I don't know if XDisplayKeycodes() was already there in X11R4.
 */
static Object P_Display_Min_Keycode (d) Object d; {
    int mink, maxk;

    Check_Type (d, T_Display);
    XDisplayKeycodes(DISPLAY(d)->dpy, &mink, &maxk);
    return Make_Integer (mink);
}

static Object P_Display_Max_Keycode (d) Object d; {
    int mink, maxk;

    Check_Type (d, T_Display);
    XDisplayKeycodes(DISPLAY(d)->dpy, &mink, &maxk);
    return Make_Integer (maxk);
}

#else
static Object P_Display_Min_Keycode (d) Object d; {
    Check_Type (d, T_Display);
    return Make_Integer (DISPLAY(d)->dpy->min_keycode);
}

static Object P_Display_Max_Keycode (d) Object d; {
    Check_Type (d, T_Display);
    return Make_Integer (DISPLAY(d)->dpy->max_keycode);
}
#endif

#ifdef XLIB_RELEASE_5_OR_LATER

/* I'm not sure if this works correctly in X11R4:
 */
static Object P_Display_Keysyms_Per_Keycode (d) Object d; {
    KeySym *ksyms;
    int mink, maxk, ksyms_per_kode;

    Check_Type (d, T_Display);
    XDisplayKeycodes(DISPLAY(d)->dpy, &mink, &maxk);
    ksyms = XGetKeyboardMapping(DISPLAY(d)->dpy, (KeyCode)mink,
        maxk - mink + 1, &ksyms_per_kode);
    return Make_Integer (ksyms_per_kode);
}

#else
static Object P_Display_Keysyms_Per_Keycode (d) Object d; {
    Check_Type (d, T_Display);
    /* Force initialization: */
    Disable_Interrupts;
    (void)XKeycodeToKeysym (DISPLAY(d)->dpy, DISPLAY(d)->dpy->min_keycode, 0);
    Enable_Interrupts;
    return Make_Integer (DISPLAY(d)->dpy->keysyms_per_keycode);
}
#endif

static Object P_String_To_Keysym (s) Object s; {
    KeySym k;

    k = XStringToKeysym (Get_Strsym (s));
    return k == NoSymbol ? False : Make_Unsigned_Long ((unsigned long)k);
}

static Object P_Keysym_To_String (k) Object k; {
    register char *s;

    s = XKeysymToString ((KeySym)Get_Long (k));
    return s ? Make_String (s, strlen (s)) : False;
}

static Object P_Keycode_To_Keysym (d, k, index) Object d, k, index; {
    Object ret;

    Check_Type (d, T_Display);
    Disable_Interrupts;
    ret = Make_Unsigned_Long ((unsigned long)XKeycodeToKeysym (DISPLAY(d)->dpy,
        Get_Integer (k), Get_Integer (index)));
    Enable_Interrupts;
    return ret;
}

static Object P_Keysym_To_Keycode (d, k) Object d, k; {
    Object ret;

    Check_Type (d, T_Display);
    Disable_Interrupts;
    ret = Make_Unsigned (XKeysymToKeycode (DISPLAY(d)->dpy,
        (KeySym)Get_Long (k)));
    Enable_Interrupts;
    return ret;
}

static Object P_Lookup_String (d, k, mask) Object d, k, mask; {
    XKeyEvent e;
    char buf[1024];
    register len;
    KeySym keysym_return;
    XComposeStatus status_return;

    Check_Type (d, T_Display);
    e.display = DISPLAY(d)->dpy;
    e.keycode = Get_Integer (k);
    e.state = Symbols_To_Bits (mask, 1, State_Syms);
    Disable_Interrupts;
    len = XLookupString (&e, buf, 1024, &keysym_return, &status_return);
    Enable_Interrupts;
    return Make_String (buf, len);
}

static Object P_Rebind_Keysym (d, k, mods, str) Object d, k, mods, str; {
    KeySym *p;
    register i, n;
    Alloca_Begin;

    Check_Type (d, T_Display);
    Check_Type (str, T_String);
    Check_Type (mods, T_Vector);
    n = VECTOR(mods)->size;
    Alloca (p, KeySym*, n * sizeof (KeySym));
    for (i = 0; i < n; i++)
        p[i] = (KeySym)Get_Long (VECTOR(mods)->data[i]);
    XRebindKeysym (DISPLAY(d)->dpy, (KeySym)Get_Long (k), p, n,
        (unsigned char *)STRING(str)->data, STRING(str)->size);
    Alloca_End;
    return Void;
}

static Object P_Refresh_Keyboard_Mapping (w, event) Object w, event; {
    static XMappingEvent fake;

    Check_Type (w, T_Window);
    fake.type = MappingNotify;
    fake.display = WINDOW(w)->dpy;
    fake.window = WINDOW(w)->win;
    fake.request = Symbols_To_Bits (event, 0, Mapping_Syms);
    XRefreshKeyboardMapping (&fake);
    return Void;
}

elk_init_xlib_key () {
    Define_Primitive (P_Display_Min_Keycode, "display-min-keycode",
                                                              1, 1, EVAL);
    Define_Primitive (P_Display_Max_Keycode, "display-max-keycode",
                                                              1, 1, EVAL);
    Define_Primitive (P_Display_Keysyms_Per_Keycode,
                        "display-keysyms-per-keycode",        1, 1, EVAL);
    Define_Primitive (P_String_To_Keysym,  "string->keysym",  1, 1, EVAL);
    Define_Primitive (P_Keysym_To_String,  "keysym->string",  1, 1, EVAL);
    Define_Primitive (P_Keycode_To_Keysym, "keycode->keysym", 3, 3, EVAL);
    Define_Primitive (P_Keysym_To_Keycode, "keysym->keycode", 2, 2, EVAL);
    Define_Primitive (P_Lookup_String,     "lookup-string",   3, 3, EVAL);
    Define_Primitive (P_Rebind_Keysym,     "rebind-keysym",   4, 4, EVAL);
    Define_Primitive (P_Refresh_Keyboard_Mapping,
                        "refresh-keyboard-mapping",           2, 2, EVAL);
}
