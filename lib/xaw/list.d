;; list.d
;;
;; $Id$
;;
;; Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
;; Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
;;
;; This software was derived from Elk 1.2, which was Copyright 1987, 1988,
;; 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
;; by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
;; between TELES and Nixdorf Microprocessor Engineering, Berlin).
;;
;; Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
;; owners or individual owners of copyright in this software, grant to any
;; person or company a worldwide, royalty free, license to
;;
;;    i) copy this software,
;;   ii) prepare derivative works based on this software,
;;  iii) distribute copies of this software or derivative works,
;;   iv) perform this software, or
;;    v) display this software,
;;
;; provided that this notice is not removed and that neither Oliver Laumann
;; nor Teles nor Nixdorf are deemed to have made any representations as to
;; the suitability of this software for any purpose nor are held responsible
;; for any defects of this software.
;;
;; THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.

(define-widget-type 'list "List.h")

(prolog
"
static char **Get_List (x) Object x; {
    register i, n;
    register char *s, **l;
    Alloca_Begin;

    Check_List (x);
    n = Fast_Length (x);
    l = (char **)XtMalloc ((n+1) * sizeof (char *));
    for (i = 0; i < n; i++, x = Cdr (x)) {
	Get_Strsym_Stack (Car (x), s);
	l[i] = XtNewString (s);
    }
    l[i] = 0;
    Alloca_End;
    return l;
}")

(define-widget-class 'list 'listWidgetClass)

(define-callback 'list 'callback #t)

(c->scheme 'callback:list-callback
"   XawListReturnStruct *p = (XawListReturnStruct *)x;
    return Cons (Make_String (p->string, strlen (p->string)),
	Make_Integer (p->list_index));")

(scheme->c 'list-list
"   return (XtArgVal)Get_List (x);")

(define-primitive 'list-change! '(w x resize)
"   Check_Widget_Class (w, listWidgetClass);
    Check_Type (resize, T_Boolean);
    XawListChange (WIDGET(w)->widget, Get_List (x), 0, 0, EQ (resize, True));
    return Void;")

(define-primitive 'list-highlight '(w i)
"   Check_Widget_Class (w, listWidgetClass);
    XawListHighlight (WIDGET(w)->widget, Get_Integer (i));
    return Void;")

(define-primitive 'list-unhighlight '(w)
"   Check_Widget_Class (w, listWidgetClass);
    XawListUnhighlight (WIDGET(w)->widget);
    return Void;")

(define-primitive 'list-current '(w)
"   XawListReturnStruct *p;

    Check_Widget_Class (w, listWidgetClass);
    p = XawListShowCurrent (WIDGET(w)->widget);
    if (p->list_index == XAW_LIST_NONE)
	return False;
    return Cons (Make_String (p->string, strlen (p->string)),
	Make_Integer (p->list_index));")
