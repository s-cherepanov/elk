;;; -*-Scheme-*-

(define cont #f)
(define done #f)

(define (pr msg)
  (display msg) (newline))

(define (doit)
  (dynamic-wind
    (lambda () (pr "  1:in"))
    (lambda ()
      (set! done (call-with-current-continuation
		   (lambda (c) (set! cont c) (pr "    catch") #f))))
    (lambda () (pr "  1:out")))
  (if (not done)
      (dynamic-wind
        (lambda () (pr "  2:in"))
        (lambda () (pr "    throw") (cont #t))
        (lambda () (pr "  2:out")))))

(dynamic-wind
  (lambda () (pr "0:in"))
  doit
  (lambda () (pr "0:out")))
