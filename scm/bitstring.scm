;;; -*-Scheme-*-
;;;
;;; The Scheme layer of the bitstring extension.

(require 'bitstring.o)

(define (bitstring-copy b)
  (let ((new (make-bitstring (bitstring-length b) #f)))
    (bitstring-move! new b)
    new))

(define (bitstring-append a b)
  (let* ((alen (bitstring-length a))
	 (blen (bitstring-length b))
	 (new (make-bitstring (+ alen blen) #f)))
    (bitstring-substring-move! a 0 alen new 0)
    (bitstring-substring-move! b 0 blen new alen)
    new))

(define (bitstring-substring b from to)
  (let ((new (make-bitstring (- to from) #f)))
    (bitstring-substring-move! b from to new 0)
    new))

(define (bitstring-not b)
  (let ((new (bitstring-copy b)))
    (bitstring-not! new b)
    new))

(define (bitstring-make-logical-function fun!)
  (lambda (a b)
    (let ((new (bitstring-copy a)))
      (fun! new b)
      new)))

(define bitstring-and    (bitstring-make-logical-function bitstring-and!))
(define bitstring-andnot (bitstring-make-logical-function bitstring-andnot!))
(define bitstring-or     (bitstring-make-logical-function bitstring-or!))
(define bitstring-xor    (bitstring-make-logical-function bitstring-xor!))

(define (signed-integer->bitstring len n)
  (if (or (>= n (expt 2 (1- len))) (< n (- (expt 2 (1- len)))))
      (error 'signed-integer->bitstring
	     "length ~s too small for signed integer ~s" len n))
  (unsigned-integer->bitstring len (if (negative? n) (+ n (expt 2 len)) n)))

(define (bitstring->signed-integer b)
  (let ((n (bitstring->unsigned-integer b))
	(len (bitstring-length b)))
    (cond ((zero? len) 0)
	  ((bitstring-ref b (1- len)) (- n (expt 2 len)))
	  (else n))))

(define (describe-bitstring b)
  (let ((len (bitstring-length b)))
    (format #t "a bitstring of length ~s bit~a.~%" len
	    (if (= len 1) "" "s"))))

(provide 'bitstring)
