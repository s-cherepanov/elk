;;; -*-Scheme-*-

(define-widget-type 'drawnbutton "DrawnB.h")

(define-widget-class 'drawn-button 'xmDrawnButtonWidgetClass)

(define-callback 'drawn-button 'activateCallback #t)
(define-callback 'drawn-button 'armCallback      #t)
(define-callback 'drawn-button 'disarmCallback   #t)
(define-callback 'drawn-button 'exposeCallback   #t)
(define-callback 'drawn-button 'resizeCallback   #t)
