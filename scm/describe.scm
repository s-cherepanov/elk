;;; -*-Scheme-*-
;;;
;;; describe -- print information about a Scheme object

(define (describe x)
  (fluid-let
      ((print-depth 2)
       (print-length 3))
    (format #t "~s is " (if (void? x) '\#v x)))
  (case (type x)
    (integer
     (format #t "an integer.~%"))
    (real
     (format #t "a real.~%"))
    (null
     (format #t "an empty list.~%"))
    (boolean
     (format #t "a boolean value (~s).~%" (if x 'true 'false)))
    (character
     (format #t "a character, ascii value is ~s~%" (char->integer x)))
    (symbol
     (format #t "a symbol~a." (if (void? x) " (the non-printing object)" ""))
     (let ((l (symbol-plist x)))
       (if (null? l)
	   (format #t "  It has no property list.~%")
	   (format #t "~%Its property list is: ~s.~%" l))))
    (pair
     (if (pair? (cdr x))
	 (let ((p (last-pair x)))
	   (if (null? (cdr p))
	       (format #t "a list of length ~s.~%" (length x))
	       (format #t "an improper list.~%")))
	 (format #t "a pair.~%")))
    (environment
     (format #t "an environment.~%"))
    (string
     (if (eqv? x "")
	 (format #t "an empty string.~%")
	 (format #t "a string of length ~s.~%" (string-length x))))
    (vector
     (if (eqv? x '#())
	 (format #t "an empty vector.~%")
	 (if (and (feature? 'oops) (memq (vector-ref x 0)
					 '(class instance)))
	     (if (eq? (vector-ref x 0) 'class)
		 (begin
		   (format #t "a class.~%~%")
		   (describe-class x))
		 (format #t "an instance.~%~%")
		 (describe-instance x))
	     (format #t "a vector of length ~s.~%" (vector-length x)))))
    (primitive
     (format #t "a primitive procedure.~%"))
    (compound
     (format #t "a compound procedure (type ~s).~%"
	     (car (procedure-lambda x))))
    (control-point
     (format #t "a control point (continuation).~%"))
    (promise
     (format #t "a promise.~%"))
    (port
     (format #t "a port.~%"))
    (end-of-file
     (format #t "the end-of-file object.~%"))
    (macro
      (format #t "a macro.~%"))
    (else
     (let ((descr-func (string->symbol
			(format #f "describe-~s" (type x)))))
       (if (bound? descr-func)
	   ((eval descr-func) x)
	   (format #t "an object of unknown type (~s)~%" (type x)))))))
