;;; -*-Scheme-*-

(define (p n)
  (let f ((n n) (i 2))
    (cond
     ((> i n) '())
     ((integer? (/ n i))
      (cons i (f (/ n i) i)))
     (else
      (f n (+ i 1))))))

(print (p 12))
(print (p 3628800))
(print (p 4194304))
