;;; -*-Scheme-*-
;;;
;;; Demonstrate the regular expression primitives.

(require 'regexp)

;; Returns a list of substrings of string `str' that match the
;; pattern `pat'

(define (matches str pat)
  (let loop ((r (make-regexp pat '(extended))) (result '()) (from 0))
       (let ((m (regexp-exec r str from)))
	 (if (regexp-match? m)
	     (loop r (cons (substring str (+ from (regexp-match-start m 0))
			                  (+ from (regexp-match-end m 0)))
			   result)
		   (+ from (regexp-match-end m 0)))
	     (reverse result)))))

(cond
  ((feature? ':regular-expressions)
    (print (matches "Hello, world!" "[a-zA-z]+"))
    (print (matches "Hello, world!" ".")))
  (else
    (format #t "Regular expressions not supported by ~a-~a~%"
      site-machine site-os)))
