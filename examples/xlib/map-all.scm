;;; -*-Scheme-*-
;;;
;;; Map all windows.

(require 'xlib)

(define (foreach-window root fun)
  (let ((l (vector->list (car (query-tree root)))))
    (for-each
      (lambda (w)
	(fun w)
	(foreach-window w fun))
      l)))

(let ((dpy (open-display)))
  (unwind-protect
    (foreach-window (display-root-window dpy) map-window)
    (close-display dpy)))
