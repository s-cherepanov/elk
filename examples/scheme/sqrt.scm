;;; -*-Scheme-*-

(define (sqrt x)
    (define (good-enough? guess)
	(< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
	(average guess (/ x guess)))
    (define (sqrt-iter guess)
	(if (good-enough? guess)
	    guess
	    (sqrt-iter (improve guess))))
    (sqrt-iter 1))

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (abs x) (if (negative? x) (- x) x))

(print (sqrt 2))
(print (sqrt 4))
