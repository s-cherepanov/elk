;;; -*-Scheme-*-
;;;
;;; Utility macros for use with the record extension.

(define-macro (define-record-type name fields)
  (let* ((rtd (eval `(make-record-type ',name ',fields)))
	 (namestr (symbol->string name)))
    `(begin
       (define
         ,(string->symbol (string-append namestr "-record")) ,rtd)
       (define
	 ,(string->symbol (string-append "make-" namestr "-record"))
	 ,(record-constructor rtd '()))
       (define
	 ,(string->symbol (string-append namestr "-record?"))
	 ,(record-predicate rtd)) #v)))

(define-macro (define-record-accessors rtd)
  (let* ((r (eval rtd)))
    `(begin
       ,@(map (lambda (field)
		`(define (
		     ,(string->symbol (string-append (record-type-name r) "-"
						     (symbol->string field)))
		     record)
		   (,(record-accessor r field) record)))
	      (record-type-field-names r)) #v)))

(define-macro (define-record-modifiers rtd)
  (let* ((r (eval rtd)))
    `(begin
       ,@(map (lambda (field)
		`(define (
		     ,(string->symbol (string-append
					"set-" (record-type-name r) "-"
					(symbol->string field) "!"))
		     record value)
		   (,(record-modifier r field) record value)))
	      (record-type-field-names r)) #v)))

(provide 'recordutil)
