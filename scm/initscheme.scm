;;; -*-Scheme-*-
;;;
;;; Initialization code for the Elk interpreter kernel.
;;;
;;; This file is loaded on startup before the toplevel (or the file
;;; supplied along with the -l option) is loaded.
;;;
;;; If a garbage collection is triggered while loading this file,
;;; it is regarded as an indication that the heap size is too small
;;; and an error message is printed.


;;; Primitives that are part of the core functionality but are not
;;; implemented in C.  This is a bad thing, because extension or
;;; application writers should be able to rely on P_Expt().

(define (expt x y)

  (define (square x) (* x x))

  (define (integer-expt b n)
    (cond ((= n 0) 1)
	  ((negative? n) (/ 1 (integer-expt b (abs n))))
          ((even? n) (square (integer-expt b (/ n 2))))
          (else (* b (integer-expt b (- n 1))))))

  (cond ((zero? x) (if (zero? y) 1 0))
	((integer? y) (integer-expt x y))
	(else (exp (* (log x) y)))))


;;; Synonyms:

(define call/cc call-with-current-continuation)


;;; Backwards compatibility.  These procedures are really obsolete;
;;; please do not use them any longer.

(define (close-port p)
  (if (input-port? p) (close-input-port p) (close-output-port p)))

(define (void? x) (eq? x (string->symbol "")))

(define (re-entrant-continuations?) #t)


;;; Useful macros (these were loaded by the standard toplevel in
;;; earlier versions of Elk).  They shouldn't really be here, but
;;; it's too late...

(define (expand form)
  (if (or (not (pair? form)) (null? form))
      form
      (let ((head (expand (car form))) (args (expand (cdr form))) (result))
	(if (and (symbol? head) (bound? head))
	    (begin
	      (set! result (macro-expand (cons head args)))
	      (if (not (equal? result form))
		  (expand result)
		  result))
	    (cons head args)))))

(define-macro (unwind-protect body . unwind-forms)
  `(dynamic-wind
    (lambda () #f)
    (lambda () ,body)
    (lambda () ,@unwind-forms)))

(define-macro (while test . body)
  `(let loop ()
     (cond (,test ,@body (loop)))))

(define-macro (when test . body)
  `(cond (,test ,@body)))

(define-macro (unless test . body)
  `(when (not ,test) ,@body))

(define-macro (multiple-value-bind vars form . body)
  `(apply (lambda ,vars ,@body) ,form))
