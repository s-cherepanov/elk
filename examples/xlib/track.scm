;;; -*-Scheme-*-

(require 'xlib)

(define (track)
  (let* ((dpy (open-display))
	 (root (display-root-window dpy))
	 (gc (create-gcontext 'window root
			    'function 'xor
			    'foreground (black-pixel dpy)
			    'subwindow-mode 'include-inferiors))
	 (where (query-pointer root))
	 (lx (car where)) (ly (cadr where)) (lw 300) (lh 300)
	 (move-outline
	  (lambda (x y)
	    (if (not (and (= x lx) (= y ly)))
		(begin
		  (draw-rectangle root gc lx ly lw lh)
		  (draw-rectangle root gc x y lw lh)
		  (set! lx x) (set! ly y))))))
    (unwind-protect
     (case (grab-pointer root #f '(pointer-motion button-press)
			 #f #f 'none 'none 'now)
       (success
	(with-server-grabbed dpy
	  (draw-rectangle root gc lx ly lw lh)
	  (display-flush-output dpy)
	  (handle-events dpy #t #f
	    (motion-notify
	     (lambda (event root win subwin time x y . rest)
	       (move-outline x y) #f))
	    (else (lambda args #t)))))
       (else
	(format #t "Not grabbed!~%")))
       (draw-rectangle root gc lx ly lw lh)
       (close-display dpy))))

(track)
