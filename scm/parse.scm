;;; -*-Scheme-*-
;;;
;;; string-tokenize -- parse a string into a list of tokens

(define (string-tokenize s)
  (let ((i 0) (j)
	(n (string-length s)))
    (let loop ((args '()))
      (while (and (< i n) (char-whitespace? (string-ref s i)))
	(set! i (1+ i)))
      (if (>= i n)
	(reverse! args)
        (set! j i)
	(while (and (< i n) (not (char-whitespace? (string-ref s i))))
	  (set! i (1+ i)))
        (loop (cons (substring s j i) args))))))
