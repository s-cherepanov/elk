;;; -*-Scheme-*-

(define-widget-type 'simplemenu "SimpleMenu.h")

(define-widget-class 'simplemenu 'simpleMenuWidgetClass)

(define-primitive 'simplemenu-add-global-actions '(c)
"   Check_Context (c);
    XawSimpleMenuAddGlobalActions (CONTEXT(c)->context);
    return Void;")

(define-primitive 'simplemenu-get-active-entry '(w)
"   Check_Widget_Class (w, simpleMenuWidgetClass);
    return
	Make_Widget_Foreign (XawSimpleMenuGetActiveEntry (WIDGET(w)->widget));")

(define-primitive 'simplemenu-clear-active-entry '(w)
"   Check_Widget_Class (w, simpleMenuWidgetClass);
    XawSimpleMenuClearActiveEntry (WIDGET(w)->widget);
    return Void;")
