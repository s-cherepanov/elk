;;; -*-Scheme-*-

(define-widget-type 'panner "Panner.h")

(define-widget-class 'panner 'pannerWidgetClass)

(define-callback 'panner 'reportCallback #t)

(c->scheme 'callback:panner-reportCallback
"   XawPannerReport *p = (XawPannerReport *)x;

    return Cons (Make_Integer (p->slider_x), Make_Integer (p->slider_y));")
