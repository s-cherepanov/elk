;;; -*-Scheme-*-
;;;
;;; Quicksort (straight from Wirth, Algorithmen & Datenstrukturen, p. 117)

(provide 'sort)

(define (sort obj pred)
  (if (vector? obj)
      (sort! (vector-copy obj) pred)
      (vector->list (sort! (list->vector obj) pred))))

(define (sort! v pred)
  (define (internal-sort l r)
    (let ((i l) (j r) (x (vector-ref v (quotient (1- (+ l r)) 2))))
      (let loop ()
	(do () ((not (pred (vector-ref v i) x))) (set! i (1+ i)))
	(do () ((not (pred x (vector-ref v j)))) (set! j (1- j)))
	(if (<= i j)
	    (begin
	      (vector-set! v j (vector-set! v i (vector-ref v j)))
	      (set! i (1+ i))
	      (set! j (1- j))))
	(if (<= i j)
	    (loop)))
      (if (< l j)
	  (internal-sort l j))
      (if (< i r)
	  (internal-sort i r))))
  (let ((len (vector-length v)))
    (if (> len 1)
	(internal-sort 0 (1- len)))
    v))
