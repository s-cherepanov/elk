;;; -*-Scheme-*-

(require 'xlib)

(define (lines)
  (let*
    ((dpy (open-display))
     (black (black-pixel dpy)) (white (white-pixel dpy))
     (win (create-window 'parent (display-root-window dpy)
		       'width 400 'height 400
		       'background-pixel white
		       'event-mask '(exposure button-press
					      enter-window leave-window)))
     (gc (create-gcontext 'window win 'background white
			'foreground black))
     (draw
      (lambda (inc)
	(clear-window win)
	(with win
	   (let ((width (window-width win))
		 (height (window-height win)))
	     (do ((x 0 (+ x inc))) ((> x width))
	       (draw-line win gc x 0 (- width x) height))
	     (do ((y height (- y inc))) ((< y 0))
	       (draw-line win gc 0 y width (- height y))))))))

    (map-window win)
    (unwind-protect
     (handle-events dpy #t #f
       (button-press
	(lambda args #t))
       (expose
	(lambda args
	  (draw 2)
	  #f))
       ((enter-notify leave-notify)
	(lambda (e . args)
	  (set-window-border-pixel! win
				    (if (eq? e 'enter-notify) white black))
	  #f)))
     (close-display dpy))))

(lines)
