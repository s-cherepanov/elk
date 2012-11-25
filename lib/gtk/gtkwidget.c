/* gtkwidget.c
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

#include "gtk.h"

int T_GtkWidget;

GtkWidget *Get_GtkWidget (Object w) {
    Check_Type (w, T_GtkWidget);
    return GTKWIDGET(w)->widget;
}

Object Make_GtkWidget (GtkWidget *w) {
    Object yield;

    yield = Alloc_Object (sizeof (struct S_GtkWidget), T_GtkWidget, 0);
    GTKWIDGET(yield)->widget = w;
    return yield;
}

static int GtkWidget_Equal (Object x, Object y) {
    return GTKWIDGET(x)->widget == GTKWIDGET(y)->widget;
}

static int GtkWidget_Print (Object x, Object port, int raw, int depth,
        int len) {
    Printf (port, "#[gtk-widget %lu]", (unsigned long)GTKWIDGET(x)->widget);
    return 0;
}

static Object GtkWidgetp (Object x) {
    return TYPE(x) == T_GtkWidget ? True : False;
}

static Object P_Gtk_Widget_Show_All (Object w) {
    gtk_widget_show_all (Get_GtkWidget(w));
    return Void;
}

void elk_init_gtk_gtkwidget () {
    T_GtkWidget = Define_Type (0, "gtk-widget", NOFUNC,
        sizeof (struct S_GtkWidget), GtkWidget_Equal, GtkWidget_Equal,
        GtkWidget_Print, NOFUNC);
    Define_Primitive (GtkWidgetp, "gtk-widget?", 1, 1, EVAL);
}
