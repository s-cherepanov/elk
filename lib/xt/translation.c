#include "xt.h"

XtTranslations Get_Translations (t) Object t; {
    register char *s;
    XtTranslations ret;
    Alloca_Begin;

    Get_Strsym_Stack (t, s);
    if ((ret = XtParseTranslationTable (s)) == 0)
	Primitive_Error ("bad translation table: ~s", t);
    Alloca_End;
    return ret;
}

static Object P_Augment_Translations (w, t) Object w, t; {
    Check_Widget (w);
    XtAugmentTranslations (WIDGET(w)->widget, Get_Translations (t));
    return Void;
}

static Object P_Override_Translations (w, t) Object w, t; {
    Check_Widget (w);
    XtOverrideTranslations (WIDGET(w)->widget, Get_Translations (t));
    return Void;
}

static Object P_Uninstall_Translations (w) Object w; {
    Check_Widget (w);
    XtUninstallTranslations (WIDGET(w)->widget);
    return Void;
}

/* Due to a bug in Xt these functions drop core when invoked with a
 * display not owned by Xt.
 */
static Object P_Multi_Click_Time (d) Object d; {
    Check_Type (d, T_Display);
    return Make_Integer (XtGetMultiClickTime (DISPLAY(d)->dpy));
}

static Object P_Set_Multi_Click_Time (d, t) Object d, t; {
    Check_Type (d, T_Display);
    XtSetMultiClickTime (DISPLAY(d)->dpy, Get_Integer (t));
    return Void;
}

elk_init_xt_translation () {
    Define_Primitive (P_Augment_Translations,
			"augment-translations",                  2, 2, EVAL);
    Define_Primitive (P_Override_Translations,
			"override-translations",                 2, 2, EVAL);
    Define_Primitive (P_Uninstall_Translations,
			"uninstall-translations",                1, 1, EVAL);
    Define_Primitive (P_Multi_Click_Time,   "multi-click-time",  1, 1, EVAL);
    Define_Primitive (P_Set_Multi_Click_Time,
			"set-multi-click-time!",                 2, 2, EVAL);
}
