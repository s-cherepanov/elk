;;; -*-Scheme-*-
;;;
;;; Porthole widget demo
;;; This only works with X11R5; there is no clock widget in X11R6-Xaw.

(require 'xwidgets)
(load-widgets shell clock form panner porthole)

(define top (application-initialize 'porthole))

(define form (create-managed-widget (find-class 'form) top))

(define panner (create-managed-widget (find-class 'panner) form))
(set-values! panner 'background-stipple 'grid2 'default-scale 15)

(define porthole (create-managed-widget (find-class 'porthole) form))
(set-values! porthole 'width 150 'height 150 'from-vert panner)

(define clock (create-managed-widget (find-class 'clock) porthole))
(set-values! clock 'width 300 'height 300)

(add-callback panner 'report-callback
  (lambda (w xy)
    (set-values! clock 'x (- (car xy)) 'y (- (cdr xy)))))

(add-callback porthole 'report-callback
  (lambda (w args)
    (multiple-value-bind (what x y sw sh cw ch) args
      (set-values! panner 'slider-x x 'slider-y y)
      (set-values! panner 'slider-width sw 'slider-height sh
	                  'canvas-width cw 'canvas-height ch))))

(realize-widget top)
(context-main-loop (widget-context top))
