;;; -*-Scheme-*-

(define (make-cell)
  (call-with-current-continuation
    (lambda (return-from-make-cell)
      (letrec ((state
		 (call-with-current-continuation
		   (lambda (return-new-state)
		     (return-from-make-cell
		       (lambda (op)
			 (case op
			   ((set)
			    (lambda (value)
			      (call-with-current-continuation
				(lambda (return-from-access)
				  (return-new-state
				    (list value return-from-access))))))
			   ((get) (car state)))))))))
	((cadr state) 'done)))))

(define c (make-cell))
(print ((c 'set) 99))
(print (c 'get))
