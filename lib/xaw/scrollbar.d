;;; -*-Scheme-*-

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
