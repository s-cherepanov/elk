;;; -*-Scheme-*-

(define-widget-type 'toggle "Toggle.h")

(define-widget-class 'toggle 'toggleWidgetClass)

(define-callback 'toggle 'callback #f)

(scheme->c 'toggle-radioData
"   return (XtArgVal)Get_Integer (x);")

(c->scheme 'toggle-radioData
"   return Make_Integer ((int)x);")

(define-primitive 'toggle-change-radio-group! '(w1 w2)
"   Check_Widget_Class (w1, toggleWidgetClass);
    Check_Widget_Class (w2, toggleWidgetClass);
    XawToggleChangeRadioGroup (WIDGET(w1)->widget, WIDGET(w2)->widget);
    return Void;")

(define-primitive 'toggle-get-current '(w)
"   Check_Widget_Class (w, toggleWidgetClass);
    return Make_Integer ((int)XawToggleGetCurrent (WIDGET(w)->widget));")

(define-primitive 'toggle-set-current! '(w x)
"   Check_Widget_Class (w, toggleWidgetClass);
    XawToggleSetCurrent (WIDGET(w)->widget, (caddr_t)Get_Integer (x));
    return Void;")

(define-primitive 'toggle-unset-current! '(w)
"   Check_Widget_Class (w, toggleWidgetClass);
    XawToggleUnsetCurrent (WIDGET(w)->widget);
    return Void;")
