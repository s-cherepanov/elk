;;; -*-Scheme-*-
;;;
;;; The `strucuture' extension is obsolete and should not be used in
;;; applications any longer; it has been replaced by the more powerful
;;; `record' extension.
;;;
;;; The Scheme part of the structures implementation
;;;
;;; (define-structure name slot slot ...)
;;;
;;; slot  =  slot-name  or  (slot-name initial-value)

(require 'struct.o)

(define-macro (define-structure name . slot-descr)
  (internal-define-structure name slot-descr #t))

(define-macro (define-simple-structure name . slot-descr)
  (internal-define-structure name slot-descr #f))

(define (internal-define-structure name slot-descr full?)
  (if (not (symbol? name))
      (error 'define-structure "structure name must be a symbol"))
  (if (null? slot-descr)
      (error 'define-structure "structure has no slots"))
  (let* ((s (symbol->string name))
         (constructor
          (string->symbol (string-append "make-" s)))
         (predicator
          (string->symbol (string-append s "?")))
         (copier
          (string->symbol (string-append "copy-" s)))
         (slots '()) (arg-slots '()))
    (for-each
     (lambda (slot)
       (cond ((symbol? slot)
              (set! slots (cons slot slots))
              (set! arg-slots (cons slot arg-slots)))
             ((pair? slot)
              (if (or (not (pair? (cdr slot)))
                      (not (null? (cddr slot))))
                  (error 'define-structure "invalid slot specification")
                  (if (not (symbol? (car slot)))
                      (error 'define-structure "slot name must be a symbol"))
                  (set! slots (cons (car slot) slots))))
             (else
              (error 'define-structure "slot must be symbol or list"))))
     slot-descr)
    (set! slots (reverse slots))
    `(begin
       (make-constructor ,constructor ,name ,slots
                         ,(reverse arg-slots) ,slot-descr)
       (make-predicator ,predicator ',name)
       (make-copier ,copier)
       ,@(let ((offset -1))
           (map
            (lambda (slot)
              (let ((f
                     (string->symbol (format #f "~s-~s" name slot))))
                (set! offset (1+ offset))
                `(make-accessor ,f ',name ,offset)))
            slots))
       ,@(if full? (let ((offset -1))
           (map
            (lambda (slot)
              (let ((f
                     (string->symbol (format #f "set-~s-~s!" name slot))))
                (set! offset (1+ offset))
                `(make-mutator ,f ',name ,offset)))
            slots)))
    ',name)))

(define-macro (make-constructor constructor name slots arg-slots descr)
  `(define (,constructor ,@arg-slots)
     (let ((,name (make-structure ',name ',slots)))
       ,@(let ((offset -1))
           (map
            (lambda (slot)
              (set! offset (1+ offset))
              `(structure-set! ,name ',name ,offset
                               ,(if (symbol? slot)
                                    slot
                                    (cadr slot))))
            descr))
       ,name)))
       
(define-macro (make-predicator predicator name)
  `(define (,predicator x)
     (and (structure? x) (eq? (structure-name x) ,name))))

(define-macro (make-copier copier)
  `(define (,copier x)
     (copy-structure x)))

(define-macro (make-accessor accessor name offset)
  `(define (,accessor x)
     (structure-ref x ,name ,offset)))

(define-macro (make-mutator mutator name offset)
  `(define (,mutator x val)
     (structure-set! x ,name ,offset val)))

(define (copy-structure s)
  (let* ((slots (structure-slots s))
         (name (structure-name s))
         (new (make-structure name slots))
         (size (length slots)))
    (do ((offset 0 (1+ offset))) ((= offset size) new)
      (structure-set! new name offset (structure-ref s name offset)))))

(define (describe-structure s)
  (format #t "a structure of type ~s.~%" (structure-name s))
  (if (null? (structure-slots s))
      (format #t "It has no slots.~%")
      (format #t "Its slots are:")
      (for-each (lambda (s v) (format #t " (~s ~s)" s v))
		(structure-slots s) (structure-values s))
      (format #t ".~%")))

(provide 'struct)
