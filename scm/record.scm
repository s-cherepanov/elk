;;; -*-Scheme-*-
;;;
;;; The Scheme layer of the record extension.

(require 'record.o)

(define (record-field-index name fields)
  (let loop ((fields fields) (index 0))
       (cond ((null? fields)
	      (error 'record-field-index "invalid field name"))
	     ((eq? name (car fields))
	      index)
	     (else
	      (loop (cdr fields) (1+ index))))))

(define (record-constructor rtd . fields)

  (define (check-fields f)
    (if (not (null? f))
        (if (or (not (symbol? (car f))) (memq (car f) (cdr f)))
            (error 'record-constructor "invalid field name")
            (check-fields (cdr f)))))

  (let* ((rtd-fields (record-type-field-names rtd))
	 (indexes '())
	 (size (length rtd-fields)))
    (if (null? fields)
	(set! fields rtd-fields)
	(if (not (null? (cdr fields)))
	    (error 'record-constructor "too many arguments"))
	(set! fields (car fields))
	check-fields fields)
    (set! indexes
	  (map (lambda (x) (record-field-index x rtd-fields)) fields))
    (lambda args
      (if (not (= (length args) (length fields)))
	  (error 'record-constructor "invalid number of fields"))
      (let ((vec (make-vector size '())))
        (for-each
	  (lambda (index value)
	    (vector-set! vec index value))
	  indexes args)
	(make-record rtd vec)))))

(define (record-predicate rtd)
  (if (not (record-type? rtd))
      (error 'record-predicate "argument not a record-type"))
  (lambda (obj)
    (and (record? obj) (eq? (record-type-descriptor obj) rtd))))

(define (record-accessor rtd field-name)
  (let ((index (record-field-index field-name (record-type-field-names rtd))))
    (lambda (obj)
      (if (and (record? obj) (eq? (record-type-descriptor obj) rtd))
          (vector-ref (record-values obj) index)
	  (error 'record-accessor "argument not of correct record type")))))

(define (record-modifier rtd field-name)
  (let ((index (record-field-index field-name (record-type-field-names rtd))))
    (lambda (obj val)
      (if (and (record? obj) (eq? (record-type-descriptor obj) rtd))
          (vector-set! (record-values obj) index val)
	  (error 'record-modifier "argument not of correct record type")))))

(define (describe-record-type rtd)
  (format #t "a record type.~%")
  (if (null? (record-type-field-names rtd))
      (format #t "It has no fields.~%")
      (format #t "Its fields are: ~s.~%" (record-type-field-names rtd))))

(define (describe-record rec)
  (format #t "a record.~%")
  (let ((fields (record-type-field-names (record-type-descriptor rec))))
    (if (null? fields)
	(format #t "It has no fields.~%")
	(format #t "Its fields are:")
	(for-each (lambda (f v) (format #t " (~s ~s)" f v))
		  fields (vector->list (record-values rec)))
	(format #t ".~%"))))

(provide 'record)
