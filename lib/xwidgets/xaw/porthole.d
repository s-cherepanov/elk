;; porthole.d
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

(define-widget-type 'porthole "Porthole.h")

(prolog

"static SYMDESCR Panner_Syms[] = {
    { \"slider-x\",       XawPRSliderX },
    { \"slider-y\",       XawPRSliderY },
    { \"slider-width\",   XawPRSliderWidth },
    { \"slider-height\",  XawPRSliderHeight },
    { \"canvas-width\",   XawPRCanvasWidth },
    { \"canvas-height\",  XawPRCanvasHeight },
    { 0, 0 }
};")

(define-widget-class 'porthole 'portholeWidgetClass)

(define-callback 'porthole 'reportCallback #t)

(c->scheme 'callback:porthole-reportCallback
"   Object ret;
    XawPannerReport *p = (XawPannerReport *)x;
    GC_Node;

    ret = Null;
    GC_Link (ret);
    ret = Cons (Make_Integer (p->canvas_height), ret);
    ret = Cons (Make_Integer (p->canvas_width), ret);
    ret = Cons (Make_Integer (p->slider_height), ret);
    ret = Cons (Make_Integer (p->slider_width), ret);
    ret = Cons (Make_Integer (p->slider_y), ret);
    ret = Cons (Make_Integer (p->slider_x), ret);
    ret = Cons (Bits_To_Symbols ((unsigned long)p->changed, 1, Panner_Syms),
        ret);
    GC_Unlink;
    return ret;")
