;;; -*-Scheme-*-
;;;
;;; Drawing area demo

(require 'motif)
(print "loading widgets\n")
(load-widgets shell drawing-area)

(define top (application-initialize 'drawing-area))
(set-values! top 'width 300 'height 100)

(define dr (create-managed-widget (find-class 'drawing-area) top
  'expose-callback (list (lambda r (format #t "expose: ~s~%" r)))))

(set-values! dr 'resize-callback
  (list (lambda r (format #t "resize: ~s~%" r))))

(add-callback dr 'input-callback
  (lambda r (format #t "input:  ~s~%" r)))

(realize-widget top)
(context-main-loop (widget-context top))
