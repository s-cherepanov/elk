;;; -*-Scheme-*-

(define-widget-type 'form "Form.h")

(define-widget-class 'form 'formWidgetClass)

(define-primitive 'form-do-layout '(w enable)
"   Check_Widget_Class (w, formWidgetClass);
    Check_Type (enable, T_Boolean);
    XawFormDoLayout (WIDGET(w)->widget, EQ (enable, True));
    return Void;")
