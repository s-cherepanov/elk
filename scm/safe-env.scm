;;; -*-Scheme-*-
;;;
;;; This macro evaluates its arguments (arbitrary expressions) in a
;;; lexical environment created as a copy of the global environment
;;; in which all the predefined primitives are bound.
;;; Contributed by Carsten Bormann <cabo@informatik.uni-bremen.de>

(define-macro (with-safe-environment . body)
  (let* ((built-in-environment
	   (car (last-pair (environment->list (the-environment)))))
	 (binding-copy
	   (map (lambda (p)
		  (list (car p) (car p)))
	        built-in-environment)) )
    `(let ,binding-copy ,@body)))
