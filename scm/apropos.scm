;;; -*-Scheme-*-
;;;
;;; apropos -- print matching symbols

(define apropos)

(let ((found))

(define (got-one sym)
  (if (bound? sym)
      (begin
	(set! found #t)
	(print sym))))

(set! apropos (lambda (what)
  (if (symbol? what)
      (set! what (symbol->string what))
      (if (not (string? what))
	  (error 'apropos "string or symbol expected")))
  (set! found #f)
  (do ((tail (oblist) (cdr tail))) ((null? tail))
    (do ((l (car tail) (cdr l))) ((null? l))
      (if (substring? what (symbol->string (car l)))
	  (got-one (car l)))))
  (if (not found)
      (format #t "~a: nothing appropriate~%" what))
  #v)))
