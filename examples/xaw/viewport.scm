;;; -*-Scheme-*-
;;;
;;; Viewport widget demo
;;; This only works with X11R5; there is no clock widget in X11R6-Xaw.

(require 'xwidgets)
(load-widgets shell clock viewport)

(define top (application-initialize 'viewport))

(define v (create-managed-widget (find-class 'viewport) top
  'force-bars #t 'allow-horiz #t 'allow-vert #t))
(set-values! v 'width 120 'height 120)

(define c (create-managed-widget (find-class 'clock) v))
(set-values! c 'width 200 'height 200)

(realize-widget top)
(context-main-loop (widget-context top))
