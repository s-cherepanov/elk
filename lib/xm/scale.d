;;; -*-Scheme-*-

(define-widget-type 'scale "Scale.h")

(define-widget-class 'scale 'xmScaleWidgetClass)

(prolog

"static Object Get_Scale_CB (p) XmScaleCallbackStruct *p; {
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
