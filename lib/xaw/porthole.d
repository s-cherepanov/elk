;;; -*-Scheme-*-

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
