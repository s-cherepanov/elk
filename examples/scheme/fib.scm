;;; -*-Scheme-*-

(define (f n)
  (if (= n 0)
      0
      (let fib ((i n) (a1 1) (a2 0))
	(if (= i 1)
	    a1
	    (fib (- i 1) (+ a1 a2) a1)))))

(print (f 20))

(define tau (/ (+ 1 (sqrt 5.0)) 2))

(define (fib n)
  (/ (+ (expt tau n) (expt tau (- 0 n))) (sqrt 5.0)))

(print (fib 20))
