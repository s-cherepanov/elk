;; scale.d
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

(define-widget-type 'scale "Scale.h")

(define-widget-class 'scale 'xmScaleWidgetClass)

(prolog

"static Object Get_Scale_CB (XmScaleCallbackStruct *p) {
    Object ret, s;
    extern SYMDESCR Reason_Syms[];
    GC_Node2;

    ret = s = Make_Integer (p->value);
    GC_Link2 (ret, s);
    ret = Cons (ret, Null);
#ifdef SCALE_WIDGET_WORKS   /* It doesn't. */
    s = Get_Any_CB ((XmAnyCallbackStruct *)p);
#else
    s = Intern (\"event-goes-here-when-Xm-is-fixed\");
    s = Cons (s, Null);
    s = Cons (Bits_To_Symbols ((unsigned long)p->reason, 0, Reason_Syms), s);
#endif
    ret = Cons (Cdr (s), ret);
    ret = Cons (Car (s), ret);
    GC_Unlink;
    return ret;
}")

(define-callback 'scale 'dragCallback          #t)
(define-callback 'scale 'valueChangedCallback  #t)

(define scale-callback->scheme
"   return Get_Scale_CB ((XmScaleCallbackStruct *)x);")

(c->scheme 'callback:scale-dragCallback         scale-callback->scheme)
(c->scheme 'callback:scale-valueChangedCallback scale-callback->scheme)
