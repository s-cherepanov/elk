;;; -*-Scheme-*-
;;;
;;; From Kent Dybvig's book on Chez Scheme

(define unify)

(letrec
    ((occurs?
      (lambda (u v)
	(and (pair? v)
	     (define (f l)
		(and (not (null? l))
		     (or (eq? u (car l))
			 (occurs? u (car l))
			 (f (cdr l)))))
	     (f (cdr v)))))
     (sigma
      (lambda (u v s)
	(lambda (x)
	  (define (f x)
	    (if (symbol? x)
		(if (eq? x u) v x)
		(cons (car x) (map f (cdr x)))))
	  (f (s x)))))
     (try-subst
      (lambda (u v s ks kf)
	(let ((u (s u)))
	  (if (not (symbol? u))
	      (uni u v s ks kf)
	      (let ((v (s v)))
		(cond
		 ((eq? u v) (ks s))
		 ((occurs? u v) (kf "loop"))
		 (else (ks (sigma u v s)))))))))
     (uni
      (lambda (u v s ks kf)
	(cond
	 ((symbol? u) (try-subst u v s ks kf))
	 ((symbol? v) (try-subst v u s ks kf))
	 ((and (eq? (car u) (car v))
	       (= (length u) (length v)))
	  (define (f u v s)
	    (if (null? u)
		(ks s)
		(uni (car u)
		     (car v)
		     s
		     (lambda (s) (f (cdr u) (cdr v) s))
		     kf)))
	  (f (cdr u) (cdr v) s))
	  (else (kf "clash"))))))
     (set! unify
	   (lambda (u v)
	     (uni u
		  v
		  (lambda (x) x)
		  (lambda (s) (s u))
		  (lambda (msg) msg)))))
		
(print (unify 'x 'y))
(print (unify '(f x y) '(g x y)))
(print (unify '(f x (h)) '(f (h) y)))
(print (unify '(f (g x) y) '(f y x)))
(print (unify '(f (g x) y) '(f y (g x))))
