;;; -*-Scheme-*-

(define (kons left right)
  (lambda (op)
    (case op
      (a left)
      (d right))))

(define (kar cell) (cell 'a))
(define (kdr cell) (cell 'd))

(let ((k (kons 1 2)))
  (print (cons (kar k) (kdr k))))
