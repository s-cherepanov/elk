;;; -*-Scheme-*-
;;;
;;; Scrollbar widget demo

(require 'xaw)

(define top (application-initialize 'scrollbar))

(define scroll (create-managed-widget (find-class 'scrollbar) top
                                      'thickness 35 'length 400))

(define (sp w x) (format #t "(scroll-proc ~s)~%" x))
(define (jp w x) (format #t "(jump-proc ~s)~%" x))

(add-callback scroll 'scroll-proc sp)
(set-values! scroll 'jump-proc (list jp))

(scrollbar-set-thumb! scroll 0.3 0.2)

(realize-widget top)
(context-main-loop (widget-context top))
