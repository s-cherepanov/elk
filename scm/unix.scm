;;; -*-Scheme-*-
;;;
;;; The Scheme layer of the UNIX extension.

(require 'record)
(require 'recordutil)
(require 'unix.o)

(define-record-type stat (type mode ino dev nlink uid gid size
			  atime mtime ctime))
(define-record-accessors stat-record)

(define (unix-stat fn)
  (let* ((ret (make-stat-record))
	 (err (unix-stat-vector-fill! fn (record-values ret))))
    (if (unix-error? err) err ret)))

(if (feature? 'unix:symlinks)
    (define (unix-lstat fn)
      (let* ((ret (make-stat-record))
	     (err (unix-lstat-vector-fill! fn (record-values ret))))
	(if (unix-error? err) err ret))))


(define-record-type time (seconds minutes hours day-of-month month year
                          weekday day-of-year dst))
(define-record-accessors time-record)
(define-record-modifiers time-record)

(define (unix-decode-localtime t)
  (let ((ret (make-time-record)))
    (unix-decode-time-vector-fill! t (record-values ret) #f)
    ret))

(define (unix-decode-utc t)
  (let ((ret (make-time-record)))
    (unix-decode-time-vector-fill! t (record-values ret) #t)
    ret))

(define (unix-time->string t)
  (cond
    ((integer? t)
     (unix-time->string-internal t))
    ((time-record? t)
     (unix-time->string-internal (record-values t)))
    (else
     (error 'unix-time->string "argument must be integer or time-record"))))


(define-record-type nanotime (nanoseconds minuteswest dst))
(define-record-accessors nanotime-record)

(define (unix-internal-make-nanotime v i)
  (if (vector-ref v i)
      (vector-set! v i (+ (* (car (vector-ref v i)) 1000000000)
			  (cdr (vector-ref v i))))))

(define (unix-nanotime)
  (let* ((ret (make-nanotime-record))
	 (v (record-values ret)))
    (unix-nanotime-vector-fill! v)
    (vector-set! v 0 (+ (* (car (vector-ref v 0)) 1000000000)
			(cdr (vector-ref v 0))))
    ret))


(define-record-type system (hostname sysname osname))
(define-record-accessors system-record)

(define (unix-system-info)
  (let ((ret (make-system-record)))
    (unix-system-info-vector-fill! (record-values ret))
    ret))


(define-record-type passwd (name password uid gid gecos homedir shell))
(define-record-accessors passwd-record)

(define (unix-get-passwd . arg)
  (let* ((ret (make-passwd-record))
	 (err (apply unix-get-passwd-vector-fill! (record-values ret) arg)))
    (if (unix-error? err) err ret)))


(define-record-type group (name password gid members))
(define-record-accessors group-record)

(define (unix-get-group . arg)
  (let* ((ret (make-group-record))
	 (err (apply unix-get-group-vector-fill! (record-values ret) arg)))
    (if (unix-error? err) err ret)))


(define-record-type resources (user-time system-time))
(define-record-accessors resources-record)

(define (unix-process-resources)
  (let* ((self (make-resources-record))
	 (children (make-resources-record))
	 (v1 (record-values self))
	 (v2 (record-values children))
	 (ticks/s (unix-process-resources-vector-fill! v1 v2))
	 (convert (lambda (ticks) (round (/ (* ticks 1000000000) ticks/s)))))
    (vector-set! v1 0 (convert (vector-ref v1 0)))
    (vector-set! v1 1 (convert (vector-ref v1 1)))
    (vector-set! v2 0 (convert (vector-ref v2 0)))
    (vector-set! v2 1 (convert (vector-ref v2 1)))
    (cons self children)))


(if (feature? 'unix:file-locking)
  (begin
    (define-record-type lock (exclusive? whence start length))
    (define-record-accessors lock-record)
    (define-record-modifiers lock-record)

    (define (unix-set-lock fd lock wait?)
      (if (not (lock-record? lock))
          (error 'unix-set-lock "argument not a lock-record"))
      (unix-internal-lock-operation fd (record-values lock) wait? #\s 0))

    (define (unix-remove-lock fd lock)
      (if (not (lock-record? lock))
	  (error 'unix-remove-lock "argument not a lock-record"))
      (unix-internal-lock-operation fd (record-values lock) #f #\r 0))

    (define (unix-query-lock fd lock)
      (if (not (lock-record? lock))
	  (error 'unix-remove-lock "argument not a lock-record"))
      (let* ((ret (make-lock-record))
	     (pid (unix-internal-lock-operation fd (record-values lock)
						#f #\q (record-values ret))))
	(if pid
	    (cons pid ret)
	    #f)))))


(define-record-type wait (pid status code core-dump? resources))
(define-record-accessors wait-record)

(define (unix-wait . options)
  (let* ((ret (make-wait-record))
	 (resources ((record-constructor resources-record) #f #f))
	 (v (record-values ret))
	 (rv (record-values resources))
	 (err (apply unix-wait-vector-fill! v rv options)))
    (unix-internal-make-nanotime rv 0)
    (unix-internal-make-nanotime rv 1)
    (vector-set! v 4 resources)
    (if (unix-error? err) err ret)))

(if (feature? 'unix:wait-process)
    (define (unix-wait-process pid . options)
      (let* ((ret (make-wait-record))
	     (resources ((record-constructor resources-record) #f #f))
	     (v (record-values ret))
	     (rv (record-values resources))
	     (err (apply unix-wait-process-vector-fill! v rv pid options)))
        (unix-internal-make-nanotime rv 0)
        (unix-internal-make-nanotime rv 1)
        (vector-set! v 4 resources)
        (if (unix-error? err) err ret))))
      

(define (unix-perror str)
  (format #t "~a: ~E" str))

(define-macro (unix-errval expr)
  `(fluid-let ((unix-call-standard-error-handler? #f))
     ,expr))

;; also need the opposite of unix-errval (i.e. make sure error is handled)

(provide 'unix)
