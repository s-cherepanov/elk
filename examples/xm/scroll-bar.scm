;;; -*-Scheme-*-
;;;
;;; Scroll bar demo

(require 'motif)
(load-widgets shell scroll-bar)

(define top (application-initialize 'scrollbar))
(set-values! top 'allow-shell-resize #t)

(define scr (create-managed-widget (find-class 'scroll-bar) top))
(set-values! scr 'height 500)

(define (f . r) (print r))

(add-callback scr 'decrement-callback f)
(add-callback scr 'increment-callback f)
(add-callback scr 'page-increment-callback f)
(add-callback scr 'page-decrement-callback f)
(add-callback scr 'drag-callback f)
(add-callback scr 'to-top-callback f)
(add-callback scr 'to-bottom-callback f)
(add-callback scr 'value-changed-callback f)

(realize-widget top)
(context-main-loop (widget-context top))
