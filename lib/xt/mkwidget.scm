;;; -*-Scheme-*-

(define type-name #f)

(define classes '())
(define callbacks '())
(define primitives '())
(define converters '())

(define f)

(define (check-string proc x name)
  (if (not (memq (type x) '(symbol string)))
      (error proc (format #f "~s must be string or symbol" name))))

(define (define-widget-type name include)
    (if type-name
	(error 'define-widget-type "must be called once"))
    (check-string 'define-widget-type name 'name)
    (if (pair? include)
	(for-each
	  (lambda (i) (check-string 'define-widget-type i 'include)) include)
        (check-string 'define-widget-type include 'include))
    (set! type-name name)
    (format f "#include \"../xt/xt.h\"~%")
    (case widget-set
      (xm
	(format f "#include <Xm/Xm.h>~%")))
    (if (and (not (eqv? include "")) (not (null? include)))
	(begin
	  (define dir)
	  (case widget-set
	    (xm
	     (set! dir "Xm"))
	    (xaw
	     (set! dir "X11/Xaw")))
	  (if (pair? include)
	      (for-each
		(lambda (i)
	          (if (char=? (string-ref (format #f "~a" i) 0) #\<)
		      (format f "#include ~a~%" i)
		      (format f "#include <~a/~a>~%" dir i)))
		include)
	      (if (char=? (string-ref (format #f "~a" include) 0) #\<)
		  (format f "#include ~a~%" include)
		  (format f "#include <~a/~a>~%" dir include)))))
    (newline f))

(define (prolog code)
  (if (not type-name)
      (error 'prolog "must define a widget-type first"))
  (check-string 'prolog code 'code)
  (display code f)
  (format f "~%~%"))

(define (define-callback class name has-arg?)
  (check-string 'define-callback class 'class)
  (check-string 'define-callback name 'name)
  (if (not (boolean? has-arg?))
      (error 'define-callback "has-arg? must be boolean"))
  (set! callbacks (cons (list class name has-arg?) callbacks)))

(define (c->scheme name body)
  (check-string 'c->scheme name 'name)
  (define c-name (scheme-to-c-name name))
  (string-set! c-name 0 #\S)
  (format f "static Object ~a (x) XtArgVal x; {~%" c-name)
  (display body f)
  (format f "~%}~%~%")
  (define s
    (format #f "    Define_Converter_To_Scheme (\"~a\", ~a);~%"
	    name c-name))
  (set! converters (cons s converters)))

(define (scheme->c name body)
  (check-string 'scheme->c name 'name)
  (define c-name (scheme-to-c-name name))
  (string-set! c-name 0 #\C)
  (format f "static XtArgVal ~a (x) Object x; {~%" c-name)
  (display body f)
  (format f "~%}~%~%")
  (define s
    (format #f "    Define_Converter_To_C (\"~a\", ~a);~%"
	    name c-name))
  (set! converters (cons s converters)))

(define (define-primitive scheme-name args body)
  (check-string 'define-primitive scheme-name 'scheme-name)
  (if (not (pair? args))
      (error 'define-primitive "args must be a list"))
  (define c-name (scheme-to-c-name scheme-name))
  (format f "static Object ~a (" c-name)
  (do ((a args a)) ((null? a))
    (display (car a) f)
    (set! a (cdr a))
    (if (not (null? a)) (display ", " f)))
  (display ") " f)
  (if (not (null? args))
      (begin
	(display "Object " f)
	(do ((a args a)) ((null? a))
	  (display (car a) f)
	  (set! a (cdr a))
	  (if (not (null? a)) (display ", " f)))
	(display "; {" f)))
  (newline f)
  (display body f)
  (format f "~%}~%~%")
  (define s
    (format #f "    Define_Primitive (~a, \"~a\", ~a, ~a, EVAL);~%"
	    c-name scheme-name (length args) (length args)))
  (set! primitives (cons s primitives)))

;;; [missing conversion from -> to "to"]
(define (scheme-to-c-name s)
  (if (symbol? s)
      (set! s (symbol->string s)))
  (define len (string-length s))
  (if (char=? (string-ref s (1- len)) #\?)
      (string-set! s (1- len) #\p))
  (if (char=? (string-ref s (1- len)) #\!)
      (set! len (1- len)))
  (let loop ((ret "P") (i 0))
    (if (>= i len)
	ret
	(define next
	  (do ((j i (1+ j)))
	      ((or (= j len) (memq (string-ref s j) '(#\- #\:))) j)))
	(loop (format #f "~a_~a~a" ret (char-upcase (string-ref s i))
		      (substring s (1+ i) next)) (1+ next)))))

(define (define-widget-class name class . sub-resources)
  (check-string 'define-widget-class name 'name)
  (check-string 'define-widget-class class 'class)
  (set! classes (cons (list name class sub-resources) classes)))

(define (filename-to-widget-name fn)
  (let loop ((w widget-aliases))
    (cond
      ((null? w)
	fn)
      ((eq? (cdar w) fn)
	(caar w))
      (else
	(loop (cdr w))))))

(define (feature-name fn)
  (let ((i (substring? ".d" fn)))
    (if (not i)
	(error 'mkwidget "bad filename suffix in ~a (expected .d)" fn))
    (filename-to-widget-name (string->symbol (substring fn 0 i)))))

(define widget-aliases)
(load 'ALIASES)

(define args (command-line-args))
(if (not (= (length args) 3))
    (error 'mkwidget "expected three arguments"))
(define widget-set (string->symbol (caddr args)))
(set! f (open-output-file (cadr args)))
(load (car args))
(if (not type-name)
    (error 'mkwidget "no widget type defined"))
(format f "elk_init_~a_~a () {~%" widget-set type-name)
(if (not (null? classes))
    (format f "    XtResourceList r = 0;~%"))
(do ((c classes (cdr c))) ((null? c))
  (define cl (car c))
  (define res (caddr cl))
  (if (not (null? res))
      (begin
	(format f
	  "    r = (XtResourceList)XtMalloc (~a * sizeof (XtResource));~%"
	  (length res))
	(do ((r res (cdr r)) (num 0 (1+ num))) ((null? r))
	  (define x (car r))
	  (if (not (= (length x) 3))
	      (error 'mkwidget "bad sub-resource declaration"))
	  (for-each
	   (lambda (r)
	     (if (not (memq (type r) '(symbol string)))
		 (error 'mkwidget "bad type in sub-resource declaration")))
	   x)
	  (format f "    r[~a].resource_name = \"~a\";~%" num (car x))
	  (format f "    r[~a].resource_class = \"~a\";~%" num (cadr x))
	  (format f "    r[~a].resource_type = \"~a\";~%" num (caddr x)))))
  (format f "    Define_Class (\"~a\", ~a, r, ~a);~%" (car cl) (cadr cl)
	  (length res)))
(do ((c callbacks (cdr c))) ((null? c))
  (define cb (car c))
  (format f "    Define_Callback (\"~a\", \"~a\", ~a);~%" (car cb) (cadr cb)
	  (if (caddr cb) 1 0)))
(for-each (lambda (x) (display x f)) primitives)
(for-each (lambda (x) (display x f)) converters)
(format f "    P_Provide(Intern(\"~a:~a.o\"));~%" widget-set
        (feature-name (car args)))
(format f "}~%")
