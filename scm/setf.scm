;;; -*-Scheme-*-
;;;
;;; An attempt on defsetf and setf

(define defsetf)
(define get-setter)

(let ((setters '()))

  (set! defsetf
	(lambda (accessor setter)
	  (set! setters (cons (cons accessor setter) setters))
	  #v))

  (set! get-setter
	(lambda (accessor)
	  (let ((a (assoc accessor setters)))
	    (if a
		(cdr a)
		(error 'get-setter "no setter for ~s" accessor))))))

(define-macro (setf var val)
  (cond
   ((symbol? var) `(set! ,var ,val))
   ((pair? var)
    (let ((setter (get-setter (eval (car var)))))
      `(,setter ,@(cdr var) ,val)))
   (else (error 'setf "symbol or form expected"))))
