;;; -*-Scheme-*-

(define (sqrt-of a base)
  (do ((old 0) (xn (* a base base)) (an (* a base base)))
      ((equal? old xn) xn)
    (begin
      (set! old xn)
      (set! xn (quotient (+ xn (quotient an xn)) 2)))))

; pi = 9801/(sqrt(8) * sum(...))
(define (rama base)
  (define (step n)
    (quotient (* base (* (fact (* 4 n)) (+ 1103 (* 26390 n))))
	      (* (expt (fact n) 4) (expt 396 (* 4 n)))))
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum delta))
       (delta 1 (step i)))
      ((zero? delta)
       sum)))

(define (calc-pi-ramanujan base)
  (quotient (* base base base 9801) (* (sqrt-of 8 base) (rama base))))

(define (fact n)
  (let f ((i n) (a 1))
    (if (zero? i)
	a
	(f (- i 1) (* a i)))))

(define (square x) (* x x))

(define base
  (let ((d (format #t "How many decimals of pi do you want (0 to exit): "))
	(num (read)))
    (if (and (not (eof-object? num)) (positive? num))
	(let* ((extra (+ 5 (truncate (log num)))))
	  (cons (expt 10 (+ num extra)) extra))
	#f)))
	  
(define (print-pi pi base)
  (format #t "~a.~a~%"
	  (quotient pi (car base))
	  (quotient (remainder pi (car base)) (expt 10 (cdr base)))))

(if base (print-pi (calc-pi-ramanujan (car base)) base))
