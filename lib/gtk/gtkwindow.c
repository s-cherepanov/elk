/* gtkwindow.c
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

#include <string.h>
#include "gtk.h"

static Object P_Gtk_Window_New (Object sym) {
    Object string;
    char *str;
    GtkWidget *w;

    Check_Type (sym, T_Symbol);
    string = P_Symbol_To_String (sym);
    str = Get_String (string);
    if (!strcmp(str, "gtk-window-toplevel"))
        w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    else if (!strcmp(str, "gtk-window-popup"))
        w = gtk_window_new(GTK_WINDOW_POPUP);
    else
        Primitive_Error ("wrong symbol: %s", str);

    return Make_GtkWidget (w);
}

static Object P_Gtk_Window_Set_Title (Object w, Object title) {
    gtk_window_set_title (GTK_WINDOW(Get_GtkWidget(w)), Get_String(title));
    return Void;
}

void elk_init_gtk_gtkwindow () {
    Define_Primitive (P_Gtk_Window_New, "gtk-window-new", 1, 1, EVAL);
    Define_Primitive (P_Gtk_Window_Set_Title, "gtk-window-set-title", 2, 2, EVAL);
}
