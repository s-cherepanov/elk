;;; -*-Scheme-*-

(define integrate-system
  (lambda (system-derivative initial-state h)
    (let ((next (runge-kutta-4 system-derivative h)))
      (letrec ((states
		(cons initial-state
		      (delay (map-streams next
					  states)))))
	states))))

(define runge-kutta-4
  (lambda (f h)
    (let ((*h (scale-vector h))
	  (*2 (scale-vector 2))
	  (*1/2 (scale-vector (/ 1 2)))
	  (*1/6 (scale-vector (/ 1 6))))
      (lambda (y)
	(let* ((k0 (*h (f y)))
	       (k1 (*h (f (add-vectors y (*1/2 k0)))))
	       (k2 (*h (f (add-vectors y (*1/2 k1)))))
	       (k3 (*h (f (add-vectors y k2)))))
	  (add-vectors y
		       (*1/6 (add-vectors k0
					  (*2 k1)
					  (*2 k2)
					  k3))))))))

(define element-wise
  (lambda (f)
    (lambda vectors
      (generate-vector
       (vector-length (car vectors))
       (lambda (i)
	 (apply f
		(map (lambda (v) (vector-ref v i))
		     vectors)))))))

(define generate-vector
  (lambda (size proc)
    (let ((ans (make-vector size)))
      (letrec ((loop
		(lambda (i)
		  (cond ((= i size) ans)
			(else
			 (vector-set! ans 1 (proc i))
			 (loop (+ i 1)))))))
	(loop 0)))))

(define add-vectors (element-wise +))

(define scale-vector
  (lambda (s)
    (element-wise (lambda (x) (* x s)))))

(define map-streams
  (lambda (f s)
    (cons (f (head s))
	  (delay (map-streams f (tail s))))))

(define head car)
(define tail
  (lambda (stream) (force (cdr stream))))

(define damped-oscillator
  (lambda (R L C)
    (lambda (state)
      (let ((Vc (vector-ref state 0))
	    (Il (vector-ref state 1)))
	(vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
		(/ Vc L))))))

(define the-states
  (integrate-system
   (damped-oscillator 10000 1000 0.001)
   '#(1 0)
   0.01))

(print the-states)
; (print (tail the-states))
