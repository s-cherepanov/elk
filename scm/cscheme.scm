;;; -*-Scheme-*-
;;;
;;; A few C-Scheme compatibility hacks

(provide 'cscheme)

(define-macro (syntax-table-define table name mac)
  `(define ,(eval name) ,mac))

(define mapcar map)

(define user-initial-environment (global-environment))

(define (rep-environment) (global-environment))

(define (atom? x)
  (not (pair? x)))

(define nil '())

(define *the-non-printing-object* #v)

(define (integer->string i)
  (format #f "~s" i))

(define (get* sym prop)
  (let ((ret (get sym prop)))
    (if ret ret '())))

(define-macro (access sym env)
  `(eval ',sym ,env))

(define-macro (in-package env . body)
  `(eval '(begin ,@body) ,env))

(define-macro (without-interrupts thunk)
  `(,thunk))

(define-macro (rec var exp)
  `(letrec ((,var ,exp)) ,exp))

(define (cons* first . rest)
  (let loop ((curr first) (rest rest))
    (if (null? rest)
	curr
	(cons curr (loop (car rest) (cdr rest))))))

(define sequence begin)

(define -1+ 1-)

(define (remq x y)
  (cond ((null? y) y)
	((eq? x (car y)) (remq x (cdr y)))
	(else (cons (car y) (remq x (cdr y))))))

(define (remv x y)
  (cond ((null? y) y)
	((eqv? x (car y)) (remv x (cdr y)))
	(else (cons (car y) (remv x (cdr y))))))

(define (remove x y)
  (cond ((null? y) y)
	((equal? x (car y)) (remove x (cdr y)))
	(else (cons (car y) (remove x (cdr y))))))

(define (remq! x y)
  (cond ((null? y) y)
	((eq? x (car y)) (remq! x (cdr y)))
	(else (let loop ((prev y))
		(cond ((null? (cdr prev))
		       y)
		      ((eq? (cadr prev) x)
		       (set-cdr! prev (cddr prev))
		       (loop prev))
		      (else (loop (cdr prev))))))))

(define (remv! x y)
  (cond ((null? y) y)
	((eqv? x (car y)) (remv! x (cdr y)))
	(else (let loop ((prev y))
		(cond ((null? (cdr prev))
		       y)
		      ((eqv? (cadr prev) x)
		       (set-cdr! prev (cddr prev))
		       (loop prev))
		      (else (loop (cdr prev))))))))

(define (remove! x y)
  (cond ((null? y) y)
	((equal? x (car y)) (remove! x (cdr y)))
	(else (let loop ((prev y))
		(cond ((null? (cdr prev))
		       y)
		      ((equal? (cadr prev) x)
		       (set-cdr! prev (cddr prev))
		       (loop prev))
		      (else (loop (cdr prev))))))))

(define delq remq)
(define delv remv)
(define delete remove)
(define delq! remq!)
(define delv! remv!)
(define delete! remove!)

(empty-list-is-false-for-backward-compatibility #t)

(if (feature? 'bitstring)
    (begin
      (define (bit-string-allocate k) (make-bitstring k #f))
      (define bit-string-copy bitstring-copy)
      (define bit-string? bitstring?)
      (define bit-string-length bitstring-length)
      (define bit-string-ref bitstring-ref)
      (define (bit-string-set! b i) (bitstring-set! b i #t))
      (define (bit-string-clear! b i) (bitstring-set! b i #f))
      (define bit-string-append bitstring-append)
      (define bit-substring bitstring-substring)
      (define bit-string-zero? bitstring-zero?)
      (define bit-string=? bitstring=?)
      (define bit-string-not bitstring-not)
      (define bit-string-movec! bitstring-not!)
      (define bit-string-and bitstring-and)
      (define bit-string-andc bitstring-andnot)
      (define bit-string-or bitstring-or)
      (define bit-string-xor bitstring-xor)
      (define bit-string-and! bitstring-and!)
      (define bit-string-or! bitstring-or!)
      (define bit-string-xor! bitstring-xor!)
      (define bit-string-andc! bitstring-andnot!)
      (define bit-string-fill! bitstring-fill!)
      (define bit-string-move! bitstring-move!)
      (define bit-substring-move-right! bitstring-substring-move!)
      (define unsigned-integer->bit-string unsigned-integer->bitstring)
      (define signed-integer->bit-string signed-integer->bitstring)
      (define bit-string->unsigned-integer bitstring->unsigned-integer)
      (define bit-string->signed-integer bitstring->signed-integer)))
