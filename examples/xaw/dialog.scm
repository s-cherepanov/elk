;;; -*-Scheme-*-
;;;
;;; Dialog box demo

(require 'xaw)

(define top (application-initialize 'dialog))
(define dpy (widget-display top))

(define f (open-font dpy "*courier-bold-r-normal--14*"))

(define gray-bits "\10\2\10\2")
(define gray
  (create-pixmap-from-bitmap-data
    (display-root-window dpy) gray-bits 4 4
    (black-pixel dpy) (white-pixel dpy) (display-default-depth dpy)))

(define bb (create-managed-widget (find-class 'box) top))
(define quit (create-managed-widget (find-class 'command) bb 'label "Quit"))
(define p (create-managed-widget (find-class 'command) bb 'label "Press me"))
(define pshell (create-popup-shell (find-class 'transient-shell) top))
(set-values! pshell 'width 150 'height 100)

(add-callback quit 'callback (lambda _ (exit)))

(add-callback p 'callback
  (lambda _
    (let* ((width (car (get-values top 'width)))
	  (height (car (get-values top 'height)))
	  (pos (widget-translate-coordinates top (truncate (/ width 2))
						 (truncate (/ height 2)))))
      (set-values! pshell 'x (car pos) 'y (cdr pos)))
    (set-sensitive! p #f)
    (set-sensitive! quit #f)
    (popup pshell 'grab-nonexclusive)))

(define (dialog-popdown . _)
  (popdown pshell)
  (set-sensitive! p #t)
  (set-sensitive! quit #t))

(define dialog (create-managed-widget (find-class 'dialog) pshell))
(set-values! dialog 'background-pixmap gray)
(set-values! dialog 'value "/tmp/test" 'label "Filename:")
(set-values! (name->widget dialog 'value) 'font f)

(define b (create-managed-widget (find-class 'command) dialog 'label "cancel"))
(add-callback b 'callback dialog-popdown)

(define b2 (create-managed-widget (find-class 'command) dialog 'label "write"))
(add-callback b2 'callback
  (lambda (w)
    (format #t "Filename is ~s~%"
	    (car (get-values (widget-parent w) 'value)))
    (dialog-popdown)))

(realize-widget top)
(context-main-loop (widget-context top))
