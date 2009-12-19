;; toggle.d
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

(define-widget-type 'toggle "Toggle.h")

(define-widget-class 'toggle 'toggleWidgetClass)

(define-callback 'toggle 'callback #f)

(scheme->c 'toggle-radioData
"   return (XtArgVal)Get_Integer (x);")

(c->scheme 'toggle-radioData
"   return Make_Integer ((int)x);")

(define-primitive 'toggle-change-radio-group! '(w1 w2)
"   Check_Widget_Class (w1, toggleWidgetClass);
    Check_Widget_Class (w2, toggleWidgetClass);
    XawToggleChangeRadioGroup (WIDGET(w1)->widget, WIDGET(w2)->widget);
    return Void;")

(define-primitive 'toggle-get-current '(w)
"   Check_Widget_Class (w, toggleWidgetClass);
    return Make_Integer ((int)XawToggleGetCurrent (WIDGET(w)->widget));")

(define-primitive 'toggle-set-current! '(w x)
"   Check_Widget_Class (w, toggleWidgetClass);
    XawToggleSetCurrent (WIDGET(w)->widget, (caddr_t)Get_Integer (x));
    return Void;")

(define-primitive 'toggle-unset-current! '(w)
"   Check_Widget_Class (w, toggleWidgetClass);
    XawToggleUnsetCurrent (WIDGET(w)->widget);
    return Void;")
