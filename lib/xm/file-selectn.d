;;; -*-Scheme-*-

(define-widget-type 'fileselection "FileSB.h")

(define-widget-class 'file-selection 'xmFileSelectionBoxWidgetClass)

(define-callback 'file-selection 'applyCallback   #t)
(define-callback 'file-selection 'cancelCallback  #t)
(define-callback 'file-selection 'noMatchCallback #t)
(define-callback 'file-selection 'okCallback      #t)
(define-callback 'file-selection 'helpCallback    #t)
