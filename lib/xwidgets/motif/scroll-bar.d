;; scroll-bar.d
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

(define-widget-type 'scrollbar "ScrollBar.h")

(define-widget-class 'scroll-bar 'xmScrollBarWidgetClass)

(prolog

"static Object Get_Scrollbar_CB (XmScrollBarCallbackStruct *p) {
    Object ret, s;
    GC_Node2;

    ret = s = Cons (Make_Integer (p->pixel), Null);
    GC_Link2 (ret, s);
    ret = Cons (Make_Integer (p->value), ret);
    s = Get_Any_CB ((XmAnyCallbackStruct *)p);
    ret = Cons (Cdr (s), ret);
    ret = Cons (Car (s), ret);
    GC_Unlink;
    return ret;
}")

(define-callback 'scroll-bar 'decrementCallback     #t)
(define-callback 'scroll-bar 'incrementCallback     #t)
(define-callback 'scroll-bar 'pageDecrementCallback #t)
(define-callback 'scroll-bar 'pageIncrementCallback #t)
(define-callback 'scroll-bar 'dragCallback          #t)
(define-callback 'scroll-bar 'toTopCallback         #t)
(define-callback 'scroll-bar 'toBottomCallback      #t)
(define-callback 'scroll-bar 'valueChangedCallback  #t)

(define scrollbar-callback->scheme
"   return Get_Scrollbar_CB ((XmScrollBarCallbackStruct *)x);")

(c->scheme 'callback:scroll-bar-decrementCallback  scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-incrementCallback  scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-pageDecrementCallback
                                                   scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-pageIncrementCallback
                                                   scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-dragCallback       scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-toTopCallback      scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-toBottomCallback   scrollbar-callback->scheme)
(c->scheme 'callback:scroll-bar-valueChangedCallback
                                                   scrollbar-callback->scheme)
