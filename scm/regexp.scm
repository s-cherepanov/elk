;;; -*-Scheme-*-
;;;
;;; The Scheme layer of the regexp extension is (almost) empty for now.
;;; It mainly exists to enable use of "(require 'regexp)".

(require 'regexp.o)

(define (describe-regexp r)
  (format #t "a regular expression.~%")
  (format #t "Its pattern is ~s,~%" (regexp-pattern r))
  (format #t "and its flags are ~s.~%" (regexp-flags r)))

(define (describe-regexp-match m)
  (format #t "a regular expression match.~%")
  (let ((n (regexp-match-number m)))
    (if (zero? n)
        (format #t "It has no substring matches.~%")
        (format #t "It has ~s substring match~a:~%" n (if (= n 1) "" "es"))
        (do ((i 0 (1+ i))) ((= i n))
	  (format #t "   ~s~%" (cons (regexp-match-start m i)
				     (regexp-match-end m i)))))))

(provide 'regexp)
