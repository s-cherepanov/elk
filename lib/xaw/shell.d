;;; -*-Scheme-*-

(define-widget-type 'shell "")

(prolog                             ; Shell.h is always under X11, since it's
"#include <X11/Shell.h>")           ; actually part of Xt, not of Xaw.

(define-widget-class 'shell 'shellWidgetClass)
(define-widget-class 'override-shell 'overrideShellWidgetClass)
(define-widget-class 'wm-shell 'wmShellWidgetClass)
(define-widget-class 'transient-shell 'transientShellWidgetClass)
(define-widget-class 'toplevel-shell 'topLevelShellWidgetClass)
(define-widget-class 'application-shell 'applicationShellWidgetClass)
