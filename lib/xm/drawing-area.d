;;; -*-Scheme-*-

(define-widget-type 'drawingarea "DrawingA.h")

(define-widget-class 'drawing-area 'xmDrawingAreaWidgetClass)

(define-callback 'drawing-area 'exposeCallback #t)
(define-callback 'drawing-area 'inputCallback  #t)
(define-callback 'drawing-area 'resizeCallback #t)
