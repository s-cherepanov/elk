;;; -*-Scheme-*-

(define-widget-type 'tree "Tree.h")

(define-widget-class 'tree 'treeWidgetClass)

(define-primitive 'tree-force-layout '(w)
"   Check_Widget_Class (w, treeWidgetClass);
    XawTreeForceLayout (WIDGET(w)->widget);
    return Void;")
