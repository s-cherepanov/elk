;;; -*-Scheme-*-
;;;
;;; Auxiliary definitions for button boxes

(define (create-radio-box type parent . args)
  (let ((box (create-managed-widget (find-class 'row-column) parent)))
    (set-values! box 'packing "pack_column" 'orientation "horizontal"
		 'is-homogeneous #t 'entry-class (find-class type)
		 'radio-behavior #t 'radio-always-one #t)
    (if args (apply set-values! box args))
    box))

(define (radio-box-add-button! box . args)
  (let* ((type (car (get-values box 'entry-class)))
         (button (create-managed-widget type box)))
    (if args (apply set-values! button args))
    button))
