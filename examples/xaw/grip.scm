;;; -*-Scheme-*-
;;;
;;; Grip widget demo

(require 'xwidgets)
(load-widgets shell grip)

(define top (application-initialize 'grip))
(set-values! top 'width 50 'height 50)

(define g (create-managed-widget (find-class 'grip) top))

(augment-translations g
"   <Btn1Down>:      GripAction(press)
    <Btn1Motion>:    GripAction(move)
    <Btn1Up>:        GripAction(release,done)")

(add-callback g 'callback
  (lambda (w x)
    (format #t "Action: ~s    Event: ~s~%" (cdr x) (caar x))))

(realize-widget top)
(context-main-loop (widget-context top))
