;;; -*-Scheme-*-
;;;
;;; Demonstrate use of the WM_DELETE_WINDOW protocol.

(require 'xlib)

(let* ((dpy (open-display))
       (del-atom (intern-atom dpy 'WM_DELETE_WINDOW))
       (prot-atom (intern-atom dpy 'WM_PROTOCOLS))
       (win (create-window
	      'parent (display-root-window dpy)
	      'width 100 'height 100
	      'background-pixel (white-pixel dpy))))
  (set-wm-name! win '(fine))
  (set-wm-protocols! win (vector del-atom))
  (map-window win)
  (unwind-protect
    (handle-events dpy #t #f
      (client-message
	(lambda (event w type data)
	    (and (eq? type prot-atom) (vector? data)
		 (eq? (make-atom (vector-ref data 0)) del-atom)))))
    (close-display dpy)))
