;;; -*-Scheme-*-
;;;
;;; from BYTE Feb. 88 page 208

(define (fixed-point f initial-value)
  (define epsilon 1.0e-10)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) epsilon))
  (define (loop value)
    (let ((next-value (f value)))
      (if (close-enough? value next-value)
	  next-value
	  (loop next-value))))
  (loop initial-value))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1))

(print (sqrt 2))
(print (sqrt 4))
