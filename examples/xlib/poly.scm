;;; -*-Scheme-*-

(require 'xlib)

(define (poly)
  (let* ((dpy (open-display))
	 (black (black-pixel dpy)) (white (white-pixel dpy))
	 (width 400) (height 400)
	 (win (create-window 'parent (display-root-window dpy)
			   'width width 'height height
			   'background-pixel white 'event-mask '(exposure)))
	 (gc (create-gcontext 'window win 'function 'xor
	                    'background white 'foreground black))
	 (l '(#f #f #f))
	 (rand (lambda (x) (modulo (random) x))))
    (map-window win)
    (handle-events dpy #t #f
      (else (lambda args
	      (set! width (window-width win))
	      (set! height (window-height win)) #t)))
    (unwind-protect
     (let loop ((n 0))
       (if (= n 200)
	 (begin
	   (clear-window win)
	   (display-wait-output dpy #f)
	   (set! n 0)))
       (fill-polygon win gc
		     (list->vector
		      (map (lambda (x) (cons (rand width) (rand height))) l))
		     #f 'convex)
       (loop (1+ n)))
    (close-display dpy))))
		  
(poly)
