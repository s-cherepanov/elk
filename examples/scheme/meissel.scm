;;; -*-Scheme-*-

(define (partial-sum i n e ee base)
  (- (quotient base (* i e))
     (quotient base (* (+ 2 i) ee))))

(define (a n base)             ; atan(1/n)
  (do ((i 1 (+ 4 i))
       (delta 1 (partial-sum i n e (* e n n) base))
       (e n (* e n n n n))
       (sum 0 (+ sum delta)))
      ((zero? delta) sum)))

(define (calc-pi base)
  (- (* 32 (a  10 base))
     (* 16 (a 515 base))
     (*  4 (a 239 base))))

(define (run)
  (format #t "How many digits of pi do you want (0 to exit): ")
  (let ((num (read)))
    (if (and (not (eof-object? num)) (positive? num))
	(let* ((extra (+ 5 (truncate (log num))))
	       (base (expt 10 (+ num extra)))
	       (pi (calc-pi base)))
	  (format #t "~a.~a~%"
		  (quotient pi base)
		  (quotient (remainder pi base) (expt 10 extra)))
	  (run)))))

(run)
