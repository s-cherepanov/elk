;;; -*-Scheme-*-

(define-widget-type 'scrollbar "ScrollBar.h")

(define-widget-class 'scroll-bar 'xmScrollBarWidgetClass)

(prolog

"static Object Get_Scrollbar_CB (p) XmScrollBarCallbackStruct *p; {
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
