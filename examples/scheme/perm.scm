;;; -*-Scheme-*-

(define (perm x)
  (if (null? x)
      (list x)
      (let ((res '()))
        (for-each
          (lambda (e)
	    (set! res (append res (map (lambda (p) (cons e p))
				       (perm (del e x))))))
	  x) res)))

(define (del e l)
  (let loop ((r l))
    (if (pair? r)
	(if (eq? e (car r))
	    (loop (cdr r))
	    (cons (car r) (loop (cdr r))))
	'())))

(print (perm '(a b c d)))
