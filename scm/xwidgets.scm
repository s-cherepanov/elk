;;; -*-Scheme-*-
;;;
;;; The Scheme part of the Xt extension.

(define widgets (if (feature? 'motif) 'motif 'xaw))

(require 'xlib)
(require (string->symbol (format #f "~a.la" widgets)))

(provide 'xwidgets)

(define (manage-child w)
  (manage-children (list w)))

(define (unmanage-child w)
  (unmanage-children (list w)))

(define (add-callback w name fun)
  (add-callbacks w name (list fun)))

(define (create-managed-widget . args)
  (let ((w (apply create-widget args)))
    (manage-child w)
    w))

(define application-initialize #f)

(let ((con) (dpy) (app-class #f) (shell-class #f))
  (set! application-initialize
    (lambda (name . fallback-res)
      (set! con (create-context))
      (if (not (null? fallback-res))
	(apply set-context-fallback-resources! con fallback-res))
      (set! dpy (initialize-display con #f name app-class))
      (create-shell name shell-class (find-class 'application-shell) dpy))))

;; Backwards compatibility:

(define widget-window widget->window)
