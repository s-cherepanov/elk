#include "xlib.h"

static Object Set_Attr_Slots;
static Object Conf_Slots;
static Object GC_Slots;
static Object Geometry_Slots;
static Object Win_Attr_Slots;
static Object Font_Info_Slots;
static Object Char_Info_Slots;
static Object Wm_Hints_Slots;
static Object Size_Hints_Slots;

static Object Sym_Parent_Relative, Sym_Copy_From_Parent;

XSetWindowAttributes SWA;
RECORD Set_Attr_Rec[] = {
    { (char *)&SWA.background_pixmap,     "background-pixmap",     T_BACKGROUND,
	0,                  CWBackPixmap },
    { (char *)&SWA.background_pixel,      "background-pixel",      T_PIXEL,
	0,                  CWBackPixel },
    { (char *)&SWA.border_pixmap,         "border-pixmap",         T_BORDER,
	0,                  CWBorderPixmap },
    { (char *)&SWA.border_pixel,          "border-pixel",          T_PIXEL,
	0,                  CWBorderPixel },
    { (char *)&SWA.bit_gravity,           "bit-gravity",           T_SYM,
	Bit_Grav_Syms,      CWBitGravity },
    { (char *)&SWA.win_gravity,           "gravity",               T_SYM,
	Grav_Syms,          CWWinGravity },
    { (char *)&SWA.backing_store,         "backing-store",         T_SYM,
	Backing_Store_Syms, CWBackingStore },
    { (char *)&SWA.backing_planes,        "backing-planes",        T_PIXEL,
	0,                  CWBackingPlanes },
    { (char *)&SWA.backing_pixel,         "backing-pixel",         T_PIXEL,
	0,                  CWBackingPixel },
    { (char *)&SWA.save_under,            "save-under",            T_BOOL,
	0,                  CWSaveUnder },
    { (char *)&SWA.event_mask,            "event-mask",            T_MASK,
	Event_Syms,         CWEventMask },
    { (char *)&SWA.do_not_propagate_mask, "do-not-propagate-mask", T_MASK,
	Event_Syms,         CWDontPropagate },
    { (char *)&SWA.override_redirect,     "override-redirect",     T_BOOL,
	0,                  CWOverrideRedirect },
    { (char *)&SWA.colormap,              "colormap",              T_COLORMAP,
	0,                  CWColormap },
    { (char *)&SWA.cursor,                "cursor",                T_CURSOR,
	0,                  CWCursor },
    { 0, 0, T_NONE, 0, 0 }
};
int Set_Attr_Size = sizeof Set_Attr_Rec / sizeof (RECORD);

XWindowChanges WC;
RECORD Conf_Rec[] = {
    { (char *)&WC.x,            "x",            T_INT,     0, CWX },
    { (char *)&WC.y,            "y",            T_INT,     0, CWY },
    { (char *)&WC.width,        "width",        T_INT,     0, CWWidth },
    { (char *)&WC.height,       "height",       T_INT,     0, CWHeight },
    { (char *)&WC.border_width, "border-width", T_INT,     0, CWBorderWidth },
    { (char *)&WC.sibling,      "sibling",      T_WINDOW,  0, CWSibling },
    { (char *)&WC.stack_mode,   "stack-mode",   T_SYM,     Stack_Mode_Syms,
	CWStackMode },
    { 0, 0, T_NONE, 0, 0 }
};
int Conf_Size = sizeof Conf_Rec / sizeof (RECORD);

XGCValues GCV;
RECORD GC_Rec[] = {
    { (char *)&GCV.function,           "function",       T_SYM,
	Func_Syms,        GCFunction },
    { (char *)&GCV.plane_mask,         "plane-mask",     T_PIXEL,
	0,                GCPlaneMask },
    { (char *)&GCV.foreground,         "foreground",     T_PIXEL,
	0,                GCForeground },
    { (char *)&GCV.background,         "background",     T_PIXEL,
	0,                GCBackground },
    { (char *)&GCV.line_width,         "line-width",     T_INT,
	0,                GCLineWidth },
    { (char *)&GCV.line_style,         "line-style",     T_SYM,
	Line_Style_Syms,  GCLineStyle },
    { (char *)&GCV.cap_style,          "cap-style",      T_SYM,
	Cap_Style_Syms,   GCCapStyle },
    { (char *)&GCV.join_style,         "join-style",     T_SYM,
	Join_Style_Syms,  GCJoinStyle },
    { (char *)&GCV.fill_style,         "fill-style",     T_SYM,
	Fill_Style_Syms,  GCFillStyle },
    { (char *)&GCV.fill_rule,          "fill-rule",      T_SYM,
	Fill_Rule_Syms,   GCFillRule },
    { (char *)&GCV.arc_mode,           "arc-mode",       T_SYM,
	Arc_Mode_Syms,    GCArcMode },
    { (char *)&GCV.tile,               "tile",           T_PIXMAP,
	0,                GCTile },
    { (char *)&GCV.stipple,            "stipple",        T_PIXMAP,
	0,                GCStipple },
    { (char *)&GCV.ts_x_origin,        "ts-x",           T_INT,
	0,                GCTileStipXOrigin },
    { (char *)&GCV.ts_y_origin,        "ts-y",           T_INT,
	0,                GCTileStipYOrigin },
    { (char *)&GCV.font,               "font",           T_FONT,
	0,                GCFont },
    { (char *)&GCV.subwindow_mode,     "subwindow-mode", T_SYM,
	Subwin_Mode_Syms, GCSubwindowMode },
    { (char *)&GCV.graphics_exposures, "exposures",      T_BOOL,
	0,                GCGraphicsExposures },
    { (char *)&GCV.clip_x_origin,      "clip-x",         T_INT,
	0,                GCClipXOrigin },
    { (char *)&GCV.clip_y_origin,      "clip-y",         T_INT,
	0,                GCClipYOrigin },
    { (char *)&GCV.clip_mask,          "clip-mask",      T_PIXMAP,
	0,                GCClipMask },
    { (char *)&GCV.dash_offset,        "dash-offset",    T_INT,
	0,                GCDashOffset },
    { (char *)&GCV.dashes,             "dashes",         T_CHAR,
	0,                GCDashList },
    {0, 0, T_NONE, 0, 0 }
};
int GC_Size = sizeof GC_Rec / sizeof (RECORD);

GEOMETRY GEO;
RECORD Geometry_Rec[] = {
    { (char *)&GEO.root,              "root",         T_WINDOW, 0, 0 },
    { (char *)&GEO.x,                 "x",            T_INT,    0, 0 },
    { (char *)&GEO.y,                 "y",            T_INT,    0, 0 },
    { (char *)&GEO.width,             "width",        T_INT,    0, 0 },
    { (char *)&GEO.height,            "height",       T_INT,    0, 0 },
    { (char *)&GEO.border_width,      "border-width", T_INT,    0, 0 },
    { (char *)&GEO.depth,             "depth",        T_INT,    0, 0 },
    {0, 0, T_NONE, 0, 0 }
};
int Geometry_Size = sizeof Geometry_Rec / sizeof (RECORD);

XWindowAttributes WA;
RECORD Win_Attr_Rec[] = {
    { (char *)&WA.x,                    "x",                      T_INT,
	0,                  0 },
    { (char *)&WA.y,                    "y",                      T_INT,
	0,                  0 },
    { (char *)&WA.width,                "width",                  T_INT,
	0,                  0 },
    { (char *)&WA.height,               "height",                 T_INT,
	0,                  0 },
    { (char *)&WA.border_width,         "border-width",           T_INT,
	0,                  0 },
    { (char *)&WA.depth,                "depth",                  T_INT,
	0,                  0 },
    { (char *)&WA.visual,               "visual",                 T_NONE,
	0,                  0 },
    { (char *)&WA.root,                 "root",                   T_WINDOW,
	0,                  0 },
#if defined(__cplusplus) || defined(c_plusplus)
    { (char *)&WA.c_class,              "class",                  T_SYM,
#else
    { (char *)&WA.class,                "class",                  T_SYM,
#endif
	Class_Syms,         0 },
    { (char *)&WA.bit_gravity,          "bit-gravity",            T_SYM,
	Bit_Grav_Syms,      0 },
    { (char *)&WA.win_gravity,          "gravity",                T_SYM,
	Grav_Syms,          0 },
    { (char *)&WA.backing_store,        "backing-store",          T_SYM,
	Backing_Store_Syms, 0 },
    { (char *)&WA.backing_planes,       "backing-planes",         T_PIXEL,
	0,                  0 },
    { (char *)&WA.backing_pixel,        "backing-pixel",          T_PIXEL,
	0,                  0 },
    { (char *)&WA.save_under,           "save-under",             T_BOOL,
	0,                  0 },
    { (char *)&WA.colormap ,            "colormap",               T_COLORMAP,
	0,                  0 },
    { (char *)&WA.map_installed,        "map-installed",          T_BOOL,
	0,                  0 },
    { (char *)&WA.map_state,            "map-state",              T_SYM,
	Map_State_Syms,     0 },
    { (char *)&WA.all_event_masks,      "all-event-masks",        T_MASK,
	Event_Syms,         0 },
    { (char *)&WA.your_event_mask,      "your-event-mask",        T_MASK,
	Event_Syms,         0 },
    { (char *)&WA.do_not_propagate_mask, "do-not-propagate-mask", T_MASK,
	Event_Syms,         0 },
    { (char *)&WA.override_redirect,    "override-redirect",      T_BOOL,
	0,                  0 },
    { (char *)&WA.screen,               "screen",                 T_NONE,
	0,                  0 },
    {0, 0, T_NONE, 0, 0 }
};
int Win_Attr_Size = sizeof Win_Attr_Rec / sizeof (RECORD);

XFontStruct FI;
RECORD Font_Info_Rec[] = {
    { (char *)&FI.direction,            "direction",             T_SYM,
	Direction_Syms,     0 },
    { (char *)&FI.min_char_or_byte2,    "min-byte2",             T_INT,
	0,                  0 },
    { (char *)&FI.max_char_or_byte2,    "max-byte2",             T_INT,
	0,                  0 },
    { (char *)&FI.min_byte1,            "min-byte1",             T_INT,
	0,                  0 },
    { (char *)&FI.max_byte1,            "max-byte1",             T_INT,
	0,                  0 },
    { (char *)&FI.all_chars_exist,      "all-chars-exist?",      T_BOOL,
	0,                  0 },
    { (char *)&FI.default_char,         "default-char",          T_INT,
	0,                  0 },
    { (char *)&FI.ascent,               "ascent",                T_INT,
	0,                  0 },
    { (char *)&FI.descent,              "descent",               T_INT,
	0,                  0 },
    {0, 0, T_NONE, 0, 0 }
};
int Font_Info_Size = sizeof Font_Info_Rec / sizeof (RECORD);

XCharStruct CI;
RECORD Char_Info_Rec[] = {
    { (char *)&CI.lbearing,      "lbearing",       T_SHORT, 0, 0 },
    { (char *)&CI.rbearing,      "rbearing",       T_SHORT, 0, 0 },
    { (char *)&CI.width,         "width",          T_SHORT, 0, 0 },
    { (char *)&CI.ascent,        "ascent",         T_SHORT, 0, 0 },
    { (char *)&CI.descent,       "descent",        T_SHORT, 0, 0 },
    { (char *)&CI.attributes,    "attributes",     T_SHORT, 0, 0 },
    {0, 0, T_NONE, 0, 0 }
};
int Char_Info_Size = sizeof Char_Info_Rec / sizeof (RECORD);

XWMHints WMH;
RECORD Wm_Hints_Rec[] = {
    { (char *)&WMH.input,         "input?",        T_BOOL,
	0,                  InputHint },
    { (char *)&WMH.initial_state, "initial-state", T_SYM,
	Initial_State_Syms, StateHint },
    { (char *)&WMH.icon_pixmap,   "icon-pixmap",   T_PIXMAP,
	0,                  IconPixmapHint },
    { (char *)&WMH.icon_window,   "icon-window",   T_WINDOW,
	0,                  IconWindowHint },
    { (char *)&WMH.icon_x,        "icon-x",        T_INT,
	0,                  IconPositionHint },
    { (char *)&WMH.icon_y,        "icon-y",        T_INT,
	0,                  IconPositionHint },
    { (char *)&WMH.icon_mask,     "icon-mask",     T_PIXMAP,
	0,                  IconMaskHint },
    { (char *)&WMH.window_group,  "window-group",  T_WINDOW,
	0,                  WindowGroupHint },
    {0, 0, T_NONE, 0, 0 }
};
int Wm_Hints_Size = sizeof Wm_Hints_Rec / sizeof (RECORD);

XSizeHints SZH;
RECORD Size_Hints_Rec[] = {
    { (char *)&SZH.x,              "x",               T_INT,  0, PPosition },
    { (char *)&SZH.y,              "y",               T_INT,  0, PPosition },
    { (char *)&SZH.width,          "width",           T_INT,  0, PSize },
    { (char *)&SZH.height,         "height",          T_INT,  0, PSize },
    { (char *)&SZH.x,              "x",               T_INT,  0, USPosition },
    { (char *)&SZH.y,              "y",               T_INT,  0, USPosition },
    { (char *)&SZH.width,          "width",           T_INT,  0, USSize },
    { (char *)&SZH.height,         "height",          T_INT,  0, USSize },
    { (char *)&SZH.min_width,      "min-width",       T_INT,  0, PMinSize },
    { (char *)&SZH.min_height,     "min-height",      T_INT,  0, PMinSize },
    { (char *)&SZH.max_width,      "max-width",       T_INT,  0, PMaxSize },
    { (char *)&SZH.max_height,     "max-height",      T_INT,  0, PMaxSize },
    { (char *)&SZH.width_inc,      "width-inc",       T_INT,  0, PResizeInc },
    { (char *)&SZH.height_inc,     "height-inc",      T_INT,  0, PResizeInc },
    { (char *)&SZH.min_aspect.x,   "min-aspect-x",    T_INT,  0, PAspect },
    { (char *)&SZH.min_aspect.y,   "min-aspect-y",    T_INT,  0, PAspect },
    { (char *)&SZH.max_aspect.x,   "max-aspect-x",    T_INT,  0, PAspect },
    { (char *)&SZH.max_aspect.y,   "max-aspect-y",    T_INT,  0, PAspect },
    { (char *)&SZH.base_width,     "base-width",      T_INT,  0, PBaseSize },
    { (char *)&SZH.base_height,    "base-height",     T_INT,  0, PBaseSize },
    { (char *)&SZH.win_gravity,    "gravity",         T_SYM,  Grav_Syms,
								 PWinGravity },
    {0, 0, T_NONE, 0, 0 }
};
int Size_Hints_Size = sizeof Size_Hints_Rec / sizeof (RECORD);

unsigned long Vector_To_Record (v, len, sym, rp) Object v, sym;
	register RECORD *rp; {
    register Object *p;
    unsigned long mask = 0;

    Check_Type (v, T_Vector);
    p = VECTOR(v)->data;
    if (VECTOR(v)->size != len && !EQ(p[0], sym))
	Primitive_Error ("invalid argument");
    for ( ; rp->slot; rp++) {
	++p;
	if (rp->type == T_NONE || Nullp (*p))
	    continue;
	switch (rp->type) {
	case T_INT:
	    *(int *)rp->slot = Get_Integer (*p); break;
	case T_SHORT:
	    *(short *)rp->slot = Get_Integer (*p); break;
	case T_CHAR:
	    *(char *)rp->slot = Get_Integer (*p); break;
	case T_PIXEL:
	    *(unsigned long *)rp->slot = Get_Pixel (*p); break;
	case T_BACKGROUND:
	    if (EQ(*p, Sym_None))
		*(Pixmap *)rp->slot = None;
	    else if (EQ(*p, Sym_Parent_Relative))
		*(Pixmap *)rp->slot = ParentRelative;
	    else
		*(Pixmap *)rp->slot = Get_Pixmap (*p);
	    break;
	case T_BORDER:
	    if (EQ(*p, Sym_Copy_From_Parent)) {
		*(Pixmap *)rp->slot = CopyFromParent;
		break;
	    }
	    /* fall through */
	case T_PIXMAP:
	    *(Pixmap *)rp->slot = Get_Pixmap (*p); break;
	case T_BOOL:
	    Check_Type (*p, T_Boolean);
	    *(Bool *)rp->slot = (Bool)(FIXNUM(*p));
	    break;
	case T_FONT:
	    *(Font *)rp->slot = Get_Font (*p);
	    break;
	case T_COLORMAP:
	    *(Colormap *)rp->slot = Get_Colormap (*p); break;
	case T_CURSOR:
	    *(Cursor *)rp->slot = Get_Cursor (*p);
	    break;
	case T_WINDOW:
	    break;
	case T_MASK:
	    *(long *)rp->slot = Symbols_To_Bits (*p, 1, rp->syms);
	    break;
	case T_SYM:
	    *(int *)rp->slot = (int)Symbols_To_Bits (*p, 0, rp->syms);
	    break;
	default:
	    Panic ("vector->record");
	}
	mask |= rp->mask;
    }
    return mask;
}

Object Record_To_Vector (rp, len, sym, dpy, flags) Object sym;
	register RECORD *rp; Display *dpy; unsigned long flags; {
    register i;
    Object v, x;
    GC_Node2;

    v = Null;
    GC_Link2 (sym, v);
    v = Make_Vector (len, Null);
    VECTOR(v)->data[0] = sym;
    for (i = 1; rp->slot; i++, rp++) {
	if (rp->type == T_NONE)
	    continue;
	if (rp->mask && !(flags & rp->mask))
	    continue;
	x = Null;
	switch (rp->type) {
	case T_INT:
	    x = Make_Integer (*(int *)rp->slot); break;
	case T_SHORT:
	    x = Make_Integer (*(short *)rp->slot); break;
	case T_CHAR:
	    x = Make_Integer (*(char *)rp->slot); break;
	case T_PIXEL:
	    x = Make_Pixel (*(unsigned long *)rp->slot); break;
	case T_PIXMAP:
	    if (*(unsigned long *)rp->slot == ~0L)
		x = Sym_None;
	    else
		x = Make_Pixmap_Foreign (dpy, *(Pixmap *)rp->slot);
	    break;
	case T_FONT:
	    if (*(unsigned long *)rp->slot == ~0L)
		x = Sym_None;
	    else {
		register XFontStruct *info;
		Disable_Interrupts;
		info = XQueryFont (dpy, *(Font *)rp->slot);
		Enable_Interrupts;
		x = Make_Font_Foreign (dpy, False, *(Font *)rp->slot, info);
	    }
	    break;
	case T_BOOL:
	    x = *(Bool *)rp->slot ? True : False; break;
	case T_COLORMAP:
	    x = Make_Colormap (0, dpy, *(Colormap *)rp->slot); break;
	case T_WINDOW:
	    x = Make_Window (0, dpy, *(Window *)rp->slot); break;
	case T_MASK:
	    x = Bits_To_Symbols (*(long *)rp->slot, 1, rp->syms);
	    break;
	case T_SYM:
	    x = Bits_To_Symbols ((unsigned long)*(int *)rp->slot, 0, rp->syms);
	    break;
	default:
	    Panic ("record->vector");
	}
	VECTOR(v)->data[i] = x;
    }
    GC_Unlink;
    return v;
}

SYMDESCR Func_Syms[] = {
    { "clear",         GXclear },
    { "and",           GXand },
    { "and-reverse",   GXandReverse },
    { "copy",          GXcopy },
    { "and-inverted",  GXandInverted },
    { "no-op",         GXnoop },
    { "xor",           GXxor },
    { "or",            GXor },
    { "nor",           GXnor },
    { "equiv",         GXequiv },
    { "invert",        GXinvert },
    { "or-reverse",    GXorReverse },
    { "copy-inverted", GXcopyInverted },
    { "or-inverted",   GXorInverted },
    { "nand",          GXnand },
    { "set",           GXset },
    { 0, 0 }
};

SYMDESCR Bit_Grav_Syms[] = {
    { "forget",        ForgetGravity },
    { "north-west",    NorthWestGravity },
    { "north",         NorthGravity },
    { "north-east",    NorthEastGravity },
    { "west",          WestGravity },
    { "center",        CenterGravity },
    { "east",          EastGravity },
    { "south-west",    SouthWestGravity },
    { "south",         SouthGravity },
    { "south-east",    SouthEastGravity },
    { "static",        StaticGravity },
    { 0, 0 }
};

SYMDESCR Grav_Syms[] = {
    { "unmap",         UnmapGravity },
    { "north-west",    NorthWestGravity },
    { "north",         NorthGravity },
    { "north-east",    NorthEastGravity },
    { "west",          WestGravity },
    { "center",        CenterGravity },
    { "east",          EastGravity },
    { "south-west",    SouthWestGravity },
    { "south",         SouthGravity },
    { "south-east",    SouthEastGravity },
    { "static",        StaticGravity },
    { 0, 0 }
};

SYMDESCR Backing_Store_Syms[] = {
    { "not-useful",    NotUseful },
    { "when-mapped",   WhenMapped },
    { "always",        Always },
    { 0, 0 }
};

SYMDESCR Stack_Mode_Syms[] = {
    { "above",        Above },
    { "below",        Below },
    { "top-if",       TopIf },
    { "bottom-if",    BottomIf },
    { "opposite",     Opposite },
    { 0, 0 }
};

SYMDESCR Line_Style_Syms[] = {
    { "solid",        LineSolid },
    { "dash",         LineOnOffDash },
    { "double-dash",  LineDoubleDash },
    { 0, 0 }
};

SYMDESCR Cap_Style_Syms[] = {
    { "not-last",     CapNotLast },
    { "butt",         CapButt },
    { "round",        CapRound },
    { "projecting",   CapProjecting },
    { 0, 0 }
};

SYMDESCR Join_Style_Syms[] = {
    { "miter",        JoinMiter },
    { "round",        JoinRound },
    { "bevel",        JoinBevel },
    { 0, 0 }
};

SYMDESCR Fill_Style_Syms[] = {
    { "solid",        FillSolid },
    { "tiled",        FillTiled },
    { "stippled",     FillStippled },
    { "opaque-stippled", FillOpaqueStippled },
    { 0, 0 }
};

SYMDESCR Fill_Rule_Syms[] = {
    { "even-odd",     EvenOddRule },
    { "winding",      WindingRule },
    { 0, 0 }
};

SYMDESCR Arc_Mode_Syms[] = {
    { "chord",        ArcChord },
    { "pie-slice",    ArcPieSlice },
    { 0, 0 }
};

SYMDESCR Subwin_Mode_Syms[] = {
    { "clip-by-children",    ClipByChildren },
    { "include-inferiors",   IncludeInferiors },
    { 0, 0 }
};

SYMDESCR Class_Syms[] = {
    { "input-output",    InputOutput },
    { "input-only",      InputOnly },
    { 0, 0 }
};

SYMDESCR Map_State_Syms[] = {
    { "unmapped",      IsUnmapped },
    { "unviewable",    IsUnviewable },
    { "viewable",      IsViewable },
    { 0, 0 }
};

SYMDESCR State_Syms[] = {
    { "shift",        ShiftMask },
    { "lock",         LockMask },
    { "control",      ControlMask },
    { "mod1",         Mod1Mask },
    { "mod2",         Mod2Mask },
    { "mod3",         Mod3Mask },
    { "mod4",         Mod4Mask },
    { "mod5",         Mod5Mask },
    { "button1",      Button1Mask },
    { "button2",      Button2Mask },
    { "button3",      Button3Mask },
    { "button4",      Button4Mask },
    { "button5",      Button5Mask },
    { "any-modifier", AnyModifier },
    { 0, 0 }
};

SYMDESCR Button_Syms[] = {
    { "any-button",   AnyButton },
    { "button1",      Button1 },
    { "button2",      Button2 },
    { "button3",      Button3 },
    { "button4",      Button4 },
    { "button5",      Button5 },
    { 0, 0 }
};

SYMDESCR Cross_Mode_Syms[] = {
    { "normal",       NotifyNormal },
    { "grab",         NotifyGrab },
    { "ungrab",       NotifyUngrab },
    { 0, 0 }
};

SYMDESCR Cross_Detail_Syms[] = {
    { "ancestor",          NotifyAncestor },
    { "virtual",           NotifyVirtual },
    { "inferior",          NotifyInferior },
    { "nonlinear",         NotifyNonlinear },
    { "nonlinear-virtual", NotifyNonlinearVirtual },
    { 0, 0 }
};

SYMDESCR Focus_Detail_Syms[] = {
    { "ancestor",          NotifyAncestor },
    { "virtual",           NotifyVirtual },
    { "inferior",          NotifyInferior },
    { "nonlinear",         NotifyNonlinear },
    { "nonlinear-virtual", NotifyNonlinearVirtual },
    { "pointer",           NotifyPointer },
    { "pointer-root",      NotifyPointerRoot },
    { "none",              NotifyDetailNone },
    { 0, 0 }
};

SYMDESCR Visibility_Syms[] = {
    { "unobscured",         VisibilityUnobscured },
    { "partially-obscured", VisibilityPartiallyObscured },
    { "fully-obscured",     VisibilityFullyObscured },
    { 0, 0 }
};

SYMDESCR Place_Syms[] = {
    { "top",      PlaceOnTop },
    { "bottom",   PlaceOnBottom },
    { 0, 0 }
};

SYMDESCR Prop_Syms[] = {
    { "new-value", PropertyNewValue },
    { "deleted",   PropertyDelete },
    { 0, 0 }
};

SYMDESCR Mapping_Syms[] = {
    { "modifier", MappingModifier },
    { "keyboard", MappingKeyboard },
    { "pointer",  MappingPointer },
    { 0, 0 }
};

SYMDESCR Direction_Syms[] = {
    { "left-to-right", FontLeftToRight },
    { "right-to-left", FontRightToLeft },
    { 0, 0 }
};

SYMDESCR Polyshape_Syms[] = {
    { "complex",       Complex },
    { "non-convex",    Nonconvex },
    { "convex",        Convex },
    { 0, 0 }
};

SYMDESCR Propmode_Syms[] = {
    { "replace",    PropModeReplace },
    { "prepend",    PropModePrepend },
    { "append",     PropModeAppend },
    { 0, 0 }
};

SYMDESCR Grabstatus_Syms[] = {
    { "success",         Success },
    { "not-viewable",    GrabNotViewable },
    { "already-grabbed", AlreadyGrabbed },
    { "frozen",          GrabFrozen },
    { "invalid-time",    GrabInvalidTime },
    { 0, 0 }
};

SYMDESCR Bitmapstatus_Syms[] = {
    { "success",         BitmapSuccess },
    { "open-failed",     BitmapOpenFailed },
    { "file-invalid",    BitmapFileInvalid },
    { "no-memory",       BitmapNoMemory },
    { 0, 0 }
};

SYMDESCR Circulate_Syms[] = {
    { "raise-lowest",      RaiseLowest },
    { "lower-highest",     LowerHighest },
    { 0, 0 }
};

SYMDESCR Allow_Events_Syms[] = {
    { "async-pointer",    AsyncPointer },
    { "sync-pointer",     SyncPointer },
    { "replay-pointer",   ReplayPointer },
    { "async-keyboard",   AsyncKeyboard },
    { "sync-keyboard",    SyncKeyboard },
    { "replay-keyboard",  ReplayKeyboard },
    { "async-both",       AsyncBoth },
    { "sync-both",        SyncBoth },
    { 0, 0 }
};

SYMDESCR Revert_Syms[] = {
    { "none",         RevertToNone },
    { "pointer-root", RevertToPointerRoot },
    { "parent",       RevertToParent },
    { 0, 0 }
};

SYMDESCR Shape_Syms[] = {
    { "cursor",  CursorShape },
    { "tile",    TileShape },
    { "stipple", StippleShape },
    { 0, 0 }
};

SYMDESCR Initial_State_Syms[] = {
    { "dont-care", DontCareState },
    { "normal",    NormalState },
    { "zoom",      ZoomState },
    { "iconic",    IconicState },
    { "inactive",  InactiveState },
    { 0, 0 }
};

SYMDESCR Ordering_Syms[] = {
    { "unsorted",  Unsorted },
    { "y-sorted",  YSorted },
    { "yx-sorted", YXSorted },
    { "yx-banded", YXBanded },
    { 0, 0 }
};

SYMDESCR Byte_Order_Syms[] = {
    { "lsb-first", LSBFirst },
    { "msb-first", MSBFirst },
    { 0, 0 }
};

SYMDESCR Saveset_Syms[] = {
    { "insert",    SetModeInsert },
    { "delete",    SetModeDelete },
    { 0, 0 }
};

SYMDESCR Closemode_Syms[] = {
    { "destroy-all",         DestroyAll },
    { "retain-permanent",    RetainPermanent },
    { "retain-temporary",    RetainTemporary },
    { 0, 0 }
};

SYMDESCR Event_Syms[] = {
    { "key-press",               KeyPressMask },
    { "key-release",             KeyReleaseMask },
    { "button-press",            ButtonPressMask },
    { "button-release",          ButtonReleaseMask },
    { "enter-window",            EnterWindowMask },
    { "leave-window",            LeaveWindowMask },
    { "pointer-motion",          PointerMotionMask },
    { "pointer-motion-hint",     PointerMotionHintMask },
    { "button-1-motion",         Button1MotionMask },
    { "button-2-motion",         Button2MotionMask },
    { "button-3-motion",         Button3MotionMask },
    { "button-4-motion",         Button4MotionMask },
    { "button-5-motion",         Button5MotionMask },
    { "button-motion",           ButtonMotionMask },
    { "keymap-state",            KeymapStateMask },
    { "exposure",                ExposureMask },
    { "visibility-change",       VisibilityChangeMask },
    { "structure-notify",        StructureNotifyMask },
    { "resize-redirect",         ResizeRedirectMask },
    { "substructure-notify",     SubstructureNotifyMask },
    { "substructure-redirect",   SubstructureRedirectMask },
    { "focus-change",            FocusChangeMask },
    { "property-change",         PropertyChangeMask },
    { "colormap-change",         ColormapChangeMask },
    { "owner-grab-button",       OwnerGrabButtonMask },
    { "all-events",              ~(unsigned long)0 },
    { 0, 0 }
};

SYMDESCR Error_Syms[] = {
    { "bad-request",        BadRequest },
    { "bad-value",          BadValue },
    { "bad-window",         BadWindow },
    { "bad-pixmap",         BadPixmap },
    { "bad-atom",           BadAtom },
    { "bad-cursor",         BadCursor },
    { "bad-font",           BadFont },
    { "bad-match",          BadMatch },
    { "bad-drawable",       BadDrawable },
    { "bad-access",         BadAccess },
    { "bad-alloc",          BadAlloc },
    { "bad-color",          BadColor },
    { "bad-gcontext",       BadGC },
    { "bad-id-choice",      BadIDChoice },
    { "bad-name",           BadName },
    { "bad-length",         BadLength },
    { "bad-implementation", BadImplementation },
    { 0, 0 }
};

static Init_Record (rec, size, name, var) RECORD *rec; char *name;
	Object *var; {
    Object list, tail, cell;
    register i;
    char buf[128];
    GC_Node2;

    GC_Link2 (list, tail);
    for (list = tail = Null, i = 1; i < size; tail = cell, i++, rec++) {
	cell = Intern (rec->name);
	cell = Cons (cell, Make_Integer (i));
	cell = Cons (cell, Null);
	if (Nullp (list))
	    list = cell;
	else
	    P_Set_Cdr (tail, cell);
    }
    sprintf (buf, "%s-slots", name);
    Define_Variable (var, buf, list);
    GC_Unlink;
}

elk_init_xlib_type () {
    Init_Record (Set_Attr_Rec, Set_Attr_Size, "set-window-attributes",
	&Set_Attr_Slots);
    Init_Record (Conf_Rec, Conf_Size, "window-configuration", &Conf_Slots);
    Init_Record (GC_Rec, GC_Size, "gcontext", &GC_Slots);
    Init_Record (Geometry_Rec, Geometry_Size, "geometry", &Geometry_Slots);
    Init_Record (Win_Attr_Rec, Win_Attr_Size, "get-window-attributes",
	&Win_Attr_Slots);
    Init_Record (Font_Info_Rec, Font_Info_Size, "font-info", &Font_Info_Slots);
    Init_Record (Char_Info_Rec, Char_Info_Size, "char-info", &Char_Info_Slots);
    Init_Record (Wm_Hints_Rec, Wm_Hints_Size, "wm-hints", &Wm_Hints_Slots);
    Init_Record (Size_Hints_Rec, Size_Hints_Size, "size-hints",
	&Size_Hints_Slots);
    Define_Symbol (&Sym_Parent_Relative, "parent-relative");
    Define_Symbol (&Sym_Copy_From_Parent, "copy-from-parent");
}
