;;; -*-Scheme-*-

(define-widget-type 'cascadebutton '("CascadeB.h" "CascadeBG.h"))

(define-widget-class 'cascade-button        'xmCascadeButtonWidgetClass)
(define-widget-class 'cascade-button-gadget 'xmCascadeButtonGadgetClass)

(define-callback 'cascade-button 'activateCallback  #t)
(define-callback 'cascade-button 'cascadingCallback #t)

(define-callback 'cascade-button-gadget 'activateCallback  #t)
(define-callback 'cascade-button-gadget 'cascadingCallback #t)
