#include "xlib.h"

static Object Sym_Any;

Time Get_Time (time) Object time; {
    if (EQ(time, Sym_Now))
	return CurrentTime;
    return (Time)Get_Long (time);
}

static Get_Mode (m) Object m; {
    Check_Type (m, T_Boolean);
    return EQ(m, True) ? GrabModeSync : GrabModeAsync;
}

static Object P_Grab_Pointer (win, ownerp, events, psyncp, ksyncp, confine_to,
	cursor, time) Object win, ownerp, events, psyncp, ksyncp, confine_to,
	cursor, time; {
    Check_Type (win, T_Window);
    Check_Type (ownerp, T_Boolean);
    return Bits_To_Symbols ((unsigned long)XGrabPointer (WINDOW(win)->dpy,
	    WINDOW(win)->win,
	    EQ(ownerp, True), Symbols_To_Bits (events, 1, Event_Syms),
	    Get_Mode (psyncp), Get_Mode (ksyncp),
	    Get_Window (confine_to), Get_Cursor (cursor), Get_Time (time)),
	0, Grabstatus_Syms);
}

static Object P_Ungrab_Pointer (d, time) Object d, time; {
    Check_Type (d, T_Display);
    XUngrabPointer (DISPLAY(d)->dpy, Get_Time (time));
    return Void;
}

static Object P_Grab_Button (win, button, mods, ownerp, events, psyncp, ksyncp,
	confine_to, cursor) Object win, button, mods, ownerp, events,
	psyncp, ksyncp, confine_to, cursor; {
    Check_Type (win, T_Window);
    Check_Type (ownerp, T_Boolean);
    XGrabButton (WINDOW(win)->dpy, Symbols_To_Bits (button, 0, Button_Syms),
	Symbols_To_Bits (mods, 1, State_Syms), WINDOW(win)->win,
	EQ(ownerp, True), Symbols_To_Bits (events, 1, Event_Syms),
	Get_Mode (psyncp), Get_Mode (ksyncp),
	Get_Window (confine_to), Get_Cursor (cursor));
    return Void;
}

static Object P_Ungrab_Button (win, button, mods) Object win, button, mods; {
    Check_Type (win, T_Window);
    XUngrabButton (WINDOW(win)->dpy, Symbols_To_Bits (button, 0, Button_Syms),
	Symbols_To_Bits (mods, 1, State_Syms), WINDOW(win)->win);
    return Void;
}

static Object P_Change_Active_Pointer_Grab (d, events, cursor, time)
	Object d, events, cursor, time; {
    Check_Type (d, T_Display);
    XChangeActivePointerGrab (DISPLAY(d)->dpy, Symbols_To_Bits (events, 1,
	Event_Syms), Get_Cursor (cursor), Get_Time (time));
    return Void;
}

static Object P_Grab_Keyboard (win, ownerp, psyncp, ksyncp, time) Object win,
	ownerp, psyncp, ksyncp, time; {
    Check_Type (win, T_Window);
    Check_Type (ownerp, T_Boolean);
    return Bits_To_Symbols ((unsigned long)XGrabKeyboard (WINDOW(win)->dpy,
	    WINDOW(win)->win, EQ(ownerp, True), Get_Mode (psyncp),
	    Get_Mode (ksyncp), Get_Time (time)),
	0, Grabstatus_Syms);
}

static Object P_Ungrab_Keyboard (d, time) Object d, time; {
    Check_Type (d, T_Display);
    XUngrabKeyboard (DISPLAY(d)->dpy, Get_Time (time));
    return Void;
}

static Object P_Grab_Key (win, key, mods, ownerp, psyncp, ksyncp) Object win,
	key, mods, ownerp, psyncp, ksyncp; {
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

static Object P_Ungrab_Key (win, key, mods) Object win, key, mods; {
    int keycode = AnyKey;

    Check_Type (win, T_Window);
    if (!EQ(key, Sym_Any))
	keycode = Get_Integer (key);
    XUngrabKey (WINDOW(win)->dpy, keycode,
	Symbols_To_Bits (mods, 1, State_Syms), WINDOW(win)->win);
    return Void;
}

static Object P_Allow_Events (d, mode, time) Object d, mode, time; {
    Check_Type (d, T_Display);
    XAllowEvents (DISPLAY(d)->dpy, Symbols_To_Bits (mode, 0,
	Allow_Events_Syms), Get_Time (time));
    return Void;
}

static Object P_Grab_Server (d) Object d; {
    Check_Type (d, T_Display);
    XGrabServer (DISPLAY(d)->dpy);
    return Void;
}

static Object P_Ungrab_Server (d) Object d; {
    Check_Type (d, T_Display);
    XUngrabServer (DISPLAY(d)->dpy);
    return Void;
}

elk_init_xlib_grab () {
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
