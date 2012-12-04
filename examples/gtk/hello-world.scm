;;; -*-Scheme-*-

(require 'gtk.la)

(gtk-init (command-line-args))
(define window (gtk-window-new 'gtk-window-toplevel))
(gtk-window-set-title window "Hello, World!")
(define callback (lambda args (gtk-main-quit)))
(g-signal-connect window "destroy" callback)
(gtk-widget-show-all window)
(gtk-main)
