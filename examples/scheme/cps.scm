;;; -*-Scheme-*-

(define (identity value) value)

(define (gcd a b) (cps-gcd a b identity))

(define (cps-gcd a b k)
  (if (= b 0)
      (k a)
      (cps-remainder a b (lambda (v) (cps-gcd b v k)))))

(define (cps-remainder n d k)
  (if (< n d)
      (k n)
      (cps-remainder (- n d) d k)))

(print (gcd 4 6))
