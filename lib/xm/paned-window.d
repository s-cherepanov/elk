;;; -*-Scheme-*-

(define-widget-type 'panedwindow "PanedW.h")

(prolog "extern WidgetClass xmSashWidgetClass;")

(define-widget-class 'paned-window 'xmPanedWindowWidgetClass)

(define-widget-class 'sash 'xmSashWidgetClass)
