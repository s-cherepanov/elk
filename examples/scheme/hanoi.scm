;;; -*-Scheme-*-
;;;
;;; Towers of Hanoi

(define (hanoi n)
  (if (zero? n)
      (display "Huh?\n")
      (transfer 'A 'B 'C n)))

(define (print-move from to)
  (format #t "Move disk from ~s to ~s~%" from to))

(define (transfer from to via n)
  (if (= n 1)
      (print-move from to)
      (transfer from via to (1- n))
      (print-move from to)
      (transfer via to from (1- n))))

(hanoi 3)
