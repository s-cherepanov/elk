;;; -*-Scheme-*-

(let ((k (call-with-current-continuation (lambda (c) c))))
  (display 1)
  (call-with-current-continuation (lambda (c) (k c)))
  (display 2)
  (call-with-current-continuation (lambda (c) (k c)))
  (display 3)
  (newline))
