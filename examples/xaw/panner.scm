;;; -*-Scheme-*-
;;;
;;; Panner widget demo

(require 'xwidgets)
(load-widgets shell panner)

(define top (application-initialize 'panner))
(set-values! top 'width 150 'height 150)

(define panner (create-managed-widget (find-class 'panner) top))

(set-values! panner 'background-stipple 'grid2 'allow-off #t
   'canvas-width 150 'canvas-height 150
   'slider-width 60 'slider-height 60)

(add-callback panner 'report-callback
  (lambda (w slider)
    (format #t "(~s,~s)~%" (car slider) (cdr slider))))

(realize-widget top)
(context-main-loop (widget-context top))
