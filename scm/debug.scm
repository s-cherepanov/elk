;;; -*-Scheme-*-
;;;
;;; A simple debugger (improvements by Thomas M. Breuel <tmb@ai.mit.edu>).

(define (backtrace . args)
  (if (> (length args) 1)
      (error 'backtrace "too many arguments"))
  (if (not (null? args))
      (if (not (eq? (type (car args)) 'control-point))
	  (error 'backtrace "argument must be a control point")))
  (let ((trace (apply backtrace-list args)))
    (if (null? args)
	(set! trace (cdddr trace)))
    (show-backtrace trace 0 999999)))

(define (show-backtrace trace start-frame end-frame)
  (define (rjust n x)
    (let* ((y (string-append (make-string n #\space) x))
	   (l (string-length y)))
      (substring y (- l n) l)))
  (let ((maxlen 28))
    (let loop ((frames (list-tail trace start-frame)) (num start-frame))
      (if (or (null? frames) (>= num end-frame)) #v
	  (let ((frame (car frames)))
	    (let* ((func
		    (format #f "~s" (vector-ref frame 0)))
		   (indent 
		    (- maxlen (+ 5 (string-length func)))))
	      (display (rjust 4 (number->string num)))
	      (display " ")
	      (display func)
	      (if (negative? indent)
		  (begin
		    (newline)
		    (set! indent maxlen)))
	      (do ((i indent (1- i)))
		  ((> 0 i))
		(display " ")))
	    (fluid-let
		((print-depth 2)
		 (print-length 3))
	      (display (vector-ref frame 1)))
	    (newline))
	  (loop (cdr frames) (1+ num))))))

(define (show-environment env)
  (fluid-let
      ((print-length 2)
       (print-depth 2))
    (do ((f (environment->list env) (cdr f)))
	((null? f))
      (do ((b (car f) (cdr b)))
	  ((null? b))
	(format #t "~s\t~s~%" (caar b) (cdar b)))
      (print '-------)))
  #v)

(define inspect)

(let ((frame)
      (trace)
      (help-text
       '("q     -- quit inspector"
	 "f     -- print current frame"
	 "u     -- go up one frame"
	 "d     -- go down one frame"
	 "^     -- go to top frame"
	 "$     -- go to bottom frame"
	 "g <n> -- goto to n-th frame"
	 "e     -- eval expressions in environment"
	 "p     -- pretty-print procedure"
	 "v     -- show environment"
	 "<n>   -- pretty-print n-th argument"
	 "b     -- show backtrace starting at current frame"
	 "t     -- show top of bracktrace starting at current frame"
	 "z     -- show and move top of backtrace starting at current frame"
         "o     -- obarray information")))
  
  (define (inspect-command-loop)
    (let ((input) (done #f))
      (display "inspect> ")
      (set! input (read))
      (case input
	(q
	 (set! done #t))
	(? 
	 (for-each
	  (lambda (msg)
	    (display msg)
	    (newline))
	  help-text))
	(f
	 (print-frame))
	(^
	 (set! frame 0)
	 (print-frame))
	($
	 (set! frame (1- (length trace)))
	 (print-frame))
	(u
	 (if (zero? frame)
	     (format #t "Already on top frame.~%")
	     (set! frame (1- frame))
	   (print-frame)))
	(d
	 (if (= frame (1- (length trace)))
	     (format #t "Already on bottom frame.~%")
	     (set! frame (1+ frame))
	   (print-frame)))
	(g
	 (set! input (read))
	 (if (integer? input)
	     (set! frame
		   (cond ((negative? input) 0)
			 ((>= input (length trace)) (1- (length trace)))
			 (else input)))
	     (format #t "Frame number must be an integer.~%")))
	(v
	 (show-environment (vector-ref (list-ref trace frame) 2)))
	(e
	 (format #t "Type ^D to return to Inspector.~%")
	 (let loop ()
	   (display "eval> ")
	   (set! input (read))
	   (if (not (eof-object? input))
	       (begin
		 (write (eval input
			      (vector-ref (list-ref trace frame) 2)))
		 (newline)
		 (loop))))
	 (newline))
	(p
	 (pp (vector-ref (list-ref trace frame) 0))
	 (newline))
        (z
         (show-backtrace trace frame (+ frame 10))
         (set! frame (+ frame 9))
         (if (>= frame (length trace)) (set! frame (1- (length trace)))))
        (t
         (show-backtrace trace frame (+ frame 10)))
        (b
         (show-backtrace trace frame 999999))
	(o
	 (let ((l (map length (oblist))))
	   (let ((n 0))
	     (for-each (lambda (x) (set! n (+ x n))) l)
	     (format #t "~s symbols " n)
	     (format #t "(maximum bucket: ~s).~%" (apply max l)))))
	(else
	 (cond
	  ((integer? input)
	   (let ((args (vector-ref (list-ref trace frame) 1)))
	     (if (or (< input 1) (> input (length args)))
		 (format #t "No such argument.~%")
		 (pp (list-ref args (1- input)))
	       (newline))))
	  ((eof-object? input)
	   (set! done #t))
	  (else
	   (format #t "Invalid command.  Type ? for help.~%")))))
      (if (not done)
	  (inspect-command-loop))))

  (define (print-frame)
    (format #t "~%Frame ~s of ~s:~%~%" frame (1- (length trace)))
    (let* ((f (list-ref trace frame)) (args (vector-ref f 1)))
      (format #t "Procedure:    ~s~%" (vector-ref f 0))
      (format #t "Environment:  ~s~%" (vector-ref f 2))
      (if (null? args)
	  (format #t "No arguments.~%")
	  (fluid-let
	      ((print-depth 2)
	       (print-length 3))
	    (do ((i 1 (1+ i)) (args args (cdr args))) ((null? args))
	      (format #t "Argument ~s:   ~s~%" i (car args))))))
    (newline))

  (define (find-frame proc)
    (let loop ((l trace) (i 0))
         (cond ((null? l) -1)
               ((eq? (vector-ref (car l) 0) proc) i)
               (else (loop (cdr l) (1+ i))))))
  
  (set! inspect
	(lambda ()
	  (set! trace (backtrace-list))
	  (set! trace (cddr trace))
	  (do ((t trace (cdr t)) (f 1 (1+ f))) ((null? t))
	    (if (not (null? (vector-ref (car t) 1)))
		(let ((last (last-pair (vector-ref (car t) 1))))
		  (if (not (null? (cdr last)))
		      (begin
			(format #t
 "[inspector: fixing improper arglist in frame ~s]~%" f)
			(set-cdr! last (cons (cdr last) '())))))))
	  (set! frame (find-frame error-handler))
	  (if (negative? frame)
	      (set! frame 0))
	  (format #t "Inspector (type ? for help):~%")
	  (let loop ()
	    (if (call-with-current-continuation
		 (lambda (control-point)
		   (push-frame control-point)
		   (inspect-command-loop)
		   #f))
		(begin
		  (pop-frame)
		  (loop))))
	  (newline)
	  (pop-frame)
	  (let ((next-frame (car rep-frames)))
	    (next-frame #t)))))
