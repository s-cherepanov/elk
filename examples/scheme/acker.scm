;;; -*-Scheme-*-
;;;
;;; The Ackermann function

(define (acker x y)
  (cond
    ((zero? x)
      (+ y 1))
    ((zero? y)
      (acker (- x 1) 1))
    (else
      (acker (- x 1) (acker x (- y 1))))))

(print (acker 3 2))
