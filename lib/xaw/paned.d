;;; -*-Scheme-*-

(define-widget-type 'paned "Paned.h")

(define-widget-class 'paned 'panedWidgetClass)

(define-primitive 'paned-allow-resize '(w enable)
"   Check_Widget (w);
    Check_Type (enable, T_Boolean);
    XawPanedAllowResize (WIDGET(w)->widget, EQ (enable, True));
    return Void;")

(define-primitive 'paned-set-min-max! '(w min max)
"   Check_Widget (w);
    XawPanedSetMinMax (WIDGET(w)->widget, Get_Integer (min),
	Get_Integer (max));
    return Void;")

(define-primitive 'paned-get-min-max '(w)
"   int min, max;
    Check_Widget (w);
    XawPanedGetMinMax (WIDGET(w)->widget, &min, &max);
    return Cons (Make_Integer (min), Make_Integer (max));")

(define-primitive 'paned-set-refigure-mode! '(w enable)
"   Check_Widget_Class (w, panedWidgetClass);
    Check_Type (enable, T_Boolean);
    XawPanedSetRefigureMode (WIDGET(w)->widget, EQ (enable, True));
    return Void;")

(define-primitive 'paned-get-num-sub '(w)
"   Check_Widget_Class (w, panedWidgetClass);
    return Make_Integer (XawPanedGetNumSub (WIDGET(w)->widget));")
