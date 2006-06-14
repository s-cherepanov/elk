
;; this test sometimes crashes the GC with the well-known
;; Panic: Visit: object not in prev space at 0x40210b2c ('pair') 8199 8201 (dumping core).

(display "testing garbage collector integrity (1000 loops)\n")
;(set! garbage-collect-notify? #t)
(define c 0)
(define cb
  (lambda ignore
    (let ((s '()))
      (set! c (+ 1 c))
        (call/cc
          (lambda (return)
            (do ((i 0 (+ i 1)))
                ((= i 100))
                (let ((a (+ i 1)))
                  (set! s (append s (list i))))
                (if (= i 60) (return #t))))))))
(do ((i 0 (+ i 1))) ((= i 1000)) (cb))
(display "test passed.\n")

;; This test used to crash the GC, too.

(display "testing deep calls (2000 calls)\n")
(define crash
  (lambda (x)
    (begin
      (if (> x 0)
        (crash (- x 1)))
      (collect))))
(crash 2000)
(display "test passed.\n")

