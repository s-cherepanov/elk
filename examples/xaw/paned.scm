;;; -*-Scheme-*-
;;;
;;; Paned widget demo

(require 'xwidgets)
(load-widgets shell label command paned)

(define top (application-initialize 'paned))

(define paned (create-managed-widget (find-class 'paned) top))

(define w1 (create-managed-widget (find-class 'label) paned))
(set-values! w1 'min 50 'max 50 'label "Fixed size")

(define w2 (create-managed-widget (find-class 'command) paned))
(set-values! w2 'show-grip #f 'label "Click to toggle show-grip"
		'preferred-pane-size 30)
(add-callback w2 'callback
  (lambda _
    (set-values! w2 'show-grip (not (car (get-values w2 'show-grip))))))

(define w3 (create-managed-widget (find-class 'label) paned))
(set-values! w3 'label "Another child" 'preferred-pane-size 60)

(define w4 (create-managed-widget (find-class 'command) paned))
(set-values! w4 'label "Click to report sub-panes" 'preferred-pane-size 60)
(add-callback w4 'callback
  (lambda _
    (format #t "Paned widget has ~s sub-panes~%" (paned-get-num-sub paned))))

(realize-widget top)
(context-main-loop (widget-context top))
