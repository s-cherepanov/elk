/* grab.c
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

static Object Sym_Any;

Time Get_Time (Object time) {
    if (EQ(time, Sym_Now))
        return CurrentTime;
    return (Time)Get_Long (time);
}

static int Get_Mode (Object m) {
    Check_Type (m, T_Boolean);
    return EQ(m, True) ? GrabModeSync : GrabModeAsync;
}

static Object P_Grab_Pointer (Object win, Object ownerp, Object events,
                              Object psyncp, Object ksyncp, Object confine_to,
                              Object cursor, Object time) {
    Check_Type (win, T_Window);
    Check_Type (ownerp, T_Boolean);
    return Bits_To_Symbols ((unsigned long)XGrabPointer (WINDOW(win)->dpy,
            WINDOW(win)->win,
            EQ(ownerp, True), Symbols_To_Bits (events, 1, Event_Syms),
            Get_Mode (psyncp), Get_Mode (ksyncp),
            Get_Window (confine_to), Get_Cursor (cursor), Get_Time (time)),
        0, Grabstatus_Syms);
}

static Object P_Ungrab_Pointer (Object d, Object time) {
    Check_Type (d, T_Display);
    XUngrabPointer (DISPLAY(d)->dpy, Get_Time (time));
    return Void;
}

static Object P_Grab_Button (Object win, Object button, Object mods,
                             Object ownerp, Object events, Object psyncp,
                             Object ksyncp, Object confine_to, Object cursor) {
    Check_Type (win, T_Window);
    Check_Type (ownerp, T_Boolean);
    XGrabButton (WINDOW(win)->dpy, Symbols_To_Bits (button, 0, Button_Syms),
        Symbols_To_Bits (mods, 1, State_Syms), WINDOW(win)->win,
        EQ(ownerp, True), Symbols_To_Bits (events, 1, Event_Syms),
        Get_Mode (psyncp), Get_Mode (ksyncp),
        Get_Window (confine_to), Get_Cursor (cursor));
    return Void;
}

static Object P_Ungrab_Button (Object win, Object button, Object mods) {
    Check_Type (win, T_Window);
    XUngrabButton (WINDOW(win)->dpy, Symbols_To_Bits (button, 0, Button_Syms),
        Symbols_To_Bits (mods, 1, State_Syms), WINDOW(win)->win);
    return Void;
}

static Object P_Change_Active_Pointer_Grab (Object d, Object events,
                                            Object cursor, Object time) {
    Check_Type (d, T_Display);
    XChangeActivePointerGrab (DISPLAY(d)->dpy, Symbols_To_Bits (events, 1,
        Event_Syms), Get_Cursor (cursor), Get_Time (time));
    return Void;
}

static Object P_Grab_Keyboard (Object win, Object ownerp, Object psyncp,
                               Object ksyncp, Object time) {
    Check_Type (win, T_Window);
    Check_Type (ownerp, T_Boolean);
    return Bits_To_Symbols ((unsigned long)XGrabKeyboard (WINDOW(win)->dpy,
            WINDOW(win)->win, EQ(ownerp, True), Get_Mode (psyncp),
            Get_Mode (ksyncp), Get_Time (time)),
        0, Grabstatus_Syms);
}

static Object P_Ungrab_Keyboard (Object d, Object time) {
    Check_Type (d, T_Display);
    XUngrabKeyboard (DISPLAY(d)->dpy, Get_Time (time));
    return Void;
}

static Object P_Grab_Key (Object win, Object key, Object mods, Object ownerp,
                          Object psyncp, Object ksyncp) {
    int keycode = AnyKey;

    Check_Type (win, T_Window);
    if (!EQ(key, Sym_Any))
        keycode = Get_Integer (key);
    Check_Type (ownerp, T_Boolean);
    XGrabKey (WINDOW(win)->dpy, keycode, Symbols_To_Bits (mods, 1, State_Syms),
        WINDOW(win)->win, EQ(ownerp, True), Get_Mode (psyncp),
        Get_Mode (ksyncp));
    return Void;
}

static Object P_Ungrab_Key (Object win, Object key, Object mods) {
    int keycode = AnyKey;

    Check_Type (win, T_Window);
    if (!EQ(key, Sym_Any))
        keycode = Get_Integer (key);
    XUngrabKey (WINDOW(win)->dpy, keycode,
        Symbols_To_Bits (mods, 1, State_Syms), WINDOW(win)->win);
    return Void;
}

static Object P_Allow_Events (Object d, Object mode, Object time) {
    Check_Type (d, T_Display);
    XAllowEvents (DISPLAY(d)->dpy, Symbols_To_Bits (mode, 0,
        Allow_Events_Syms), Get_Time (time));
    return Void;
}

static Object P_Grab_Server (Object d) {
    Check_Type (d, T_Display);
    XGrabServer (DISPLAY(d)->dpy);
    return Void;
}

static Object P_Ungrab_Server (Object d) {
    Check_Type (d, T_Display);
    XUngrabServer (DISPLAY(d)->dpy);
    return Void;
}

void elk_init_xlib_grab () {
    Define_Primitive (P_Grab_Pointer,    "grab-pointer",    8, 8, EVAL);
    Define_Primitive (P_Ungrab_Pointer,  "ungrab-pointer",  2, 2, EVAL);
    Define_Primitive (P_Grab_Button,     "grab-button",     9, 9, EVAL);
    Define_Primitive (P_Ungrab_Button,   "ungrab-button",   3, 3, EVAL);
    Define_Primitive (P_Change_Active_Pointer_Grab,
                             "change-active-pointer-grab",  4, 4, EVAL);
    Define_Primitive (P_Grab_Keyboard,   "grab-keyboard",   5, 5, EVAL);
    Define_Primitive (P_Ungrab_Keyboard, "ungrab-keyboard", 2, 2, EVAL);
    Define_Primitive (P_Grab_Key,        "grab-key",        6, 6, EVAL);
    Define_Primitive (P_Ungrab_Key,      "ungrab-key",      3, 3, EVAL);
    Define_Primitive (P_Allow_Events,    "allow-events",    3, 3, EVAL);
    Define_Primitive (P_Grab_Server,     "grab-server",     1, 1, EVAL);
    Define_Primitive (P_Ungrab_Server,   "ungrab-server",   1, 1, EVAL);
    Define_Symbol (&Sym_Any, "any");
}
