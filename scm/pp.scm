;;; -*-Scheme-*-
;;;
;;; Trivial pretty-printer

(provide 'pp)

(define pp)

(let ((max-pos 55) (pos 0) (tab-stop 8))
  
  (put 'lambda  'special #t)
  (put 'macro   'special #t)
  (put 'define  'special #t)
  (put 'define-macro     'special #t)
  (put 'define-structure 'special #t)
  (put 'fluid-let        'special #t)
  (put 'let     'special #t)
  (put 'let*    'special #t)
  (put 'letrec  'special #t)
  (put 'case    'special #t)

  (put 'call-with-current-continuation 'long #t)

  (put 'quote            'abbr "'")
  (put 'quasiquote       'abbr "`")
  (put 'unquote          'abbr ",")
  (put 'unquote-splicing 'abbr ",@")

(set! pp (lambda (x)
  (set! pos 0)
  (cond ((eq? (type x) 'compound)
         (set! x (procedure-lambda x)))
	((eq? (type x) 'macro)
	 (set! x (macro-body x))))
  (fluid-let ((garbage-collect-notify? #f))
    (pp-object x))
  #v))

(define (flat-size s)
  (fluid-let ((print-length 50) (print-depth 10))
    (string-length (format #f "~a" s))))

(define (pp-object x)
  (if (or (null? x) (pair? x))
      (pp-list x)
      (if (void? x)
	  (display "#v")
          (write x))
      (set! pos (+ pos (flat-size x)))))

(define (pp-list x)
  (if (and (pair? x)
	   (symbol? (car x))
	   (string? (get (car x) 'abbr))
	   (= 2 (length x)))
      (let ((abbr (get (car x) 'abbr)))
	(display abbr)
	(set! pos (+ pos (flat-size abbr)))
	(pp-object (cadr x)))
      (if (> (flat-size x) (- max-pos pos))
	  (pp-list-vertically x)
	  (pp-list-horizontally x))))

(define (pp-list-vertically x)
  (maybe-pp-list-vertically #t x))

(define (pp-list-horizontally x)
  (maybe-pp-list-vertically #f x))

(define (maybe-pp-list-vertically vertical? list)
  (display "(")
  (set! pos (1+ pos))
  (if (null? list)
      (begin
	(display ")")
	(set! pos (1+ pos)))
      (let ((pos1 pos))
	(pp-object (car list))
	(if (and vertical?
		 (or
		  (and (pair? (car list))
		       (not (null? (cdr list))))
		  (and (symbol? (car list))
		       (get (car list) 'long))))
	    (indent-newline (1- pos1)))
	(let ((pos2 (1+ pos)) (key (car list)))
	  (let tail ((flag #f) (l (cdr list)))
	    (cond ((pair? l)
		   (if flag
		       (indent-newline
			(if (and (symbol? key) (get key 'special))
			    (1+ pos1)
			    pos2))
		       (display " ")
		       (set! pos (1+ pos)))
		   (pp-object (car l))
		   (tail vertical? (cdr l)))
		  (else
		   (cond ((not (null? l))
			  (display " . ")
			  (set! pos (+ pos 3))
			  (if flag (indent-newline pos2))
			  (pp-object l)))
		   (display ")")
		   (set! pos (1+ pos)))))))))

 (define (indent-newline x)
   (newline)
   (set! pos x)
   (let loop ((i x))
     (cond ((>= i tab-stop)
	    (display "\t")
	    (loop (- i tab-stop)))
	   ((> i 0)
	    (display " ")
	    (loop (1- i)))))))

