/* translation.c
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
