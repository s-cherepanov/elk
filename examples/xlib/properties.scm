;;; -*-Scheme-*-
;;;
;;; Display all properties of all windows (with name, type, format,
;;; and data).

(require 'xlib)

(define (properties)

  (define (tab obj n)
    (let* ((s (format #f "~s" obj))
           (n (- n (string-length s))))
      (display s)
      (if (positive? n)
      (do ((i 0 (1+ i))) ((= i n)) (display #\space)))))

  (define (do-window w prop)
    (format #t "Window ~s:~%" w)
    (for-each
      (lambda (p)
        (tab (atom-name (window-display w) p) 20)
	(display "= ")
        (let ((p (get-property w p #f 0 20 #f)))
          (tab (atom-name (window-display w) (car p)) 18)
          (tab (cadr p) 3)
          (format #t "~s~%" (caddr p))))
    (vector->list prop))
  (newline))
	
  (define (do-children root)
    (for-each
      (lambda (w)
	(do-window w (list-properties w))
	(do-children w))
      (vector->list (car (query-tree root)))))

  (let* ((dpy (open-display))
	 (root (display-root-window dpy)))
    (unwind-protect
      (begin
        (do-window root (list-properties root))
        (do-children root))
      (close-display dpy))))

(properties)
