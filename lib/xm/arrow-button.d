;;; -*-Scheme-*-

(define-widget-type 'arrowbutton '("ArrowB.h" "ArrowBG.h"))

(define-widget-class 'arrow-button        'xmArrowButtonWidgetClass)
(define-widget-class 'arrow-button-gadget 'xmArrowButtonGadgetClass)

(define-callback 'arrow-button 'activateCallback #t)
(define-callback 'arrow-button 'armCallback      #t)
(define-callback 'arrow-button 'disarmCallback   #t)

(define-callback 'arrow-button-gadget 'activateCallback #t)
(define-callback 'arrow-button-gadget 'armCallback      #t)
(define-callback 'arrow-button-gadget 'disarmCallback   #t)
