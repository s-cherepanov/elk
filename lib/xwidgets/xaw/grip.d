;; grip.d
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

(define-widget-type 'grip "Grip.h")

(define-widget-class 'grip 'gripWidgetClass)

(define-callback 'grip 'callback #t)

(c->scheme 'callback:grip-callback
"   Object args, ret, t;
    register unsigned int i;
    GripCallData p = (GripCallData)x;
    GC_Node3;

    args = ret = t = Null;
    GC_Link3 (args, ret, t);
    args = Get_Event_Args (p->event);
    ret = Cons (Copy_List (args), Null);
    Destroy_Event_Args (args);
    t = P_Make_List (Make_Integer (p->num_params), Null);
    for (i = 0, Cdr (ret) = t; i < p->num_params; i++, t = Cdr (t)) {
        Object s;

        s = Make_String (p->params[i], strlen (p->params[i]));
        Car (t) = s;
    }
    GC_Unlink;
    return ret;")
