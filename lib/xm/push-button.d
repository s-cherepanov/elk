;;; -*-Scheme-*-

(define-widget-type 'pushbutton '("PushB.h" "PushBG.h"))

(define-widget-class 'push-button        'xmPushButtonWidgetClass)
(define-widget-class 'push-button-gadget 'xmPushButtonGadgetClass)

(define-callback 'push-button 'activateCallback #t)
(define-callback 'push-button 'armCallback      #t)
(define-callback 'push-button 'disarmCallback   #t)

(define-callback 'push-button-gadget 'activateCallback #t)
(define-callback 'push-button-gadget 'armCallback      #t)
(define-callback 'push-button-gadget 'disarmCallback   #t)
