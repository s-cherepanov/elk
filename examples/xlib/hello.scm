;;; -*-Scheme-*-

(require 'xlib)

(define (hello-world)
  (let* ((dpy (open-display))
	 (black (black-pixel dpy)) (white (white-pixel dpy))
	 (font (open-font dpy "*-new century schoolbook-bold-r*24*"))
	 (text (translate-text "Hello world!"))
	 (width (+ 2 (text-width font text '1-byte)))
	 (height (+ 2 (max-char-ascent font) (max-char-descent font)))
	 (win (create-window 'parent (display-root-window dpy)
			   'width width 'height height
			   'background-pixel white
			   'event-mask '(exposure button-press)))
	 (gc (create-gcontext 'window win 'background white
			    'foreground black 'font font)))
    (map-window win)
    (unwind-protect
     (handle-events dpy #t #f
       (button-press
	(lambda ignore #t))
       (expose
	(lambda ignore
	  (let ((x (truncate (/ (- (window-width win) width) 2)))
		(y (truncate (/ (- (+ (window-height win)
				      (max-char-ascent font))
				   (max-char-descent font)) 2))))
	    (draw-poly-text win gc x y text '1-byte)) #f)))
     (close-display dpy))))
		  
(hello-world)
