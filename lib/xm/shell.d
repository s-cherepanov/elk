;;; -*-Scheme-*-

(define-widget-type 'shell '(DialogS.h MenuShell.h))

(define-widget-class 'shell 'shellWidgetClass)
(define-widget-class 'override-shell 'overrideShellWidgetClass)
(define-widget-class 'wm-shell 'wmShellWidgetClass)
(define-widget-class 'vendor-shell 'vendorShellWidgetClass)
(define-widget-class 'transient-shell 'transientShellWidgetClass)
(define-widget-class 'toplevel-shell 'topLevelShellWidgetClass)
(define-widget-class 'application-shell 'applicationShellWidgetClass)

(define-widget-class 'dialog-shell 'xmDialogShellWidgetClass)

(define-widget-class 'menu-shell 'xmMenuShellWidgetClass
   '(width Width Dimension) '(height Height Dimension))
