;;; -*-Scheme-*-

(define-widget-type 'repeater "Repeater.h")

(define-widget-class 'repeater 'repeaterWidgetClass)

(define-callback 'repeater 'startCallback #f)
(define-callback 'repeater 'stopCallback  #f)
(define-callback 'repeater 'callback      #f)
