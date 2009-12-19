;; scrollbar.d
;;
;; $Id$
;;
;; Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
;; Copyright 2002, 2003 Sam Hocevar <sam@hocevar.net>, Paris
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

(define-widget-type 'scrollbar "Scrollbar.h")

(prolog

"static SYMDESCR Orientation_Syms[] = {
    { \"horizontal\",  XtorientHorizontal },
    { \"vertical\",    XtorientVertical },
    { 0, 0 }
};")

(define-widget-class 'scrollbar 'scrollbarWidgetClass)

(scheme->c 'scrollbar-orientation
"   return (XtArgVal)Symbols_To_Bits (x, 0, Orientation_Syms);")

(c->scheme 'scrollbar-orientation
"   return Bits_To_Symbols ((unsigned long)x, 0, Orientation_Syms);")

(define-callback 'scrollbar 'scrollProc #t)
(define-callback 'scrollbar 'jumpProc #t)

(c->scheme 'callback:scrollbar-scrollProc
"    return Make_Integer ((int)x);")

(c->scheme 'callback:scrollbar-jumpProc
"    return Make_Reduced_Flonum ((double)*(float *)x);")

(define-primitive 'scrollbar-set-thumb! '(w t s)
"   Check_Widget_Class (w, scrollbarWidgetClass);
    XawScrollbarSetThumb (WIDGET(w)->widget, Get_Double (t), Get_Double (s));
    return Void;")
