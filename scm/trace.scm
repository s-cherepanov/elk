;;; -*-Scheme-*-
;;;
;;; A simple trace package contributed in 1990 by WAKITA Ken
;;; (ken-w@is.s.u-tokyo.ac.jp)

(define trc:trace-list '(()))

(define (reset-trace) (set! trc:trace-list '(())))

(define-macro (trace func)
  `(let ((the-func (eval ,func))
	 (result #v))
     (if (assoc ',func trc:trace-list)
	 (error 'trace "~s already trace on." ,func))
     (if (not (compound? ,func))
	 (error 'trace "wrong argument type ~s (expected compound)"
		(type ,func)))
     (set! trc:trace-list
	   (cons '()
		 (cons (cons ',func the-func)
		       (cdr trc:trace-list))))
     (set! ,func
	   (lambda param-list
	     (format #t "# Entering ~s~%"
		     (cons ',func param-list))
	     (set! result (apply the-func param-list))
	     (format #t "# Exiting  ~s ==> ~s~%"
		     (cons ',func param-list)
		     result)
	     result))))

(define-macro (untrace func)
  `(let ((the-func (assoc ',func trc:trace-list)))
     
     (define (remove! func)
       (let ((prev trc:trace-list)
	     (here (cdr trc:trace-list)))
	 (while (and here
		     (not (eq? func (caar here))))
		(set! prev here)
		(set! here (cdr here)))
	 (if (not here)
	     (error 'remove "item ~s not found." func)
	     (set-cdr! prev (cdr here)))))
     
     (if the-func
	 (begin (remove! ',func)
		(set! ,func (cdr the-func))))))
