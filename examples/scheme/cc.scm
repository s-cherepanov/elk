;;; -*-Scheme-*-

(define acc)
(define bcc)
(define n 5)

(define (a)
  (if (not (= 0 (call-with-current-continuation
		 (lambda (cc)
		   (set! acc cc) 0))))
      (if (> n 0)
	  (begin
	    (set! n (- n 1))
	    (display "resume b") (newline)
	    (bcc 1))
	  #v)
      acc))

(define (b)
  (if (not (= 0 (call-with-current-continuation
		 (lambda (cc)
		   (set! bcc cc) 0))))
      (begin
        (display "resume a") (newline)
        (acc 1)))
  bcc)

(a)
(b)
(acc 1)
