;;; -*-Scheme-*-
;;;
;;; Message box dialog demo

(require 'motif)
(load 'radio-stuff.scm)

(define top (application-initialize 'message-box))

(define rc (create-managed-widget (find-class 'row-column) top))

(define box (create-radio-box 'push-button rc))

(define buttons
  (map (lambda (label)
	 (radio-box-add-button! box 'label-string label
				'alignment "alignment_center"))
  '(error information message question warning working)))

(for-each
  (lambda (button)
    (add-callback button 'activate-callback
		  (lambda _
		    (post-dialog (car (get-values button 'label-string))))))
  buttons)

(define box2 (create-radio-box 'toggle-button rc 'radio-behavior #f))

(define ok (radio-box-add-button! box2 'label-string 'OK-button 'set #t))
(define cancel (radio-box-add-button! box2 'label-string 'Cancel-button
				      'set #t))
(define help (radio-box-add-button! box2 'label-string 'Help-button 'set #t))

(define (post-dialog type)
  (let* ((shell (create-popup-shell (find-class 'dialog-shell) rc))
         (box (create-widget
		(find-class 'message-box) shell
		'dialog-type (string->symbol (string-append "dialog-" type)))))
	(unless (car (get-values ok 'set))
		(unmanage-child (name->widget box 'OK)))
	(unless (car (get-values cancel 'set))
		(unmanage-child (name->widget box 'Cancel)))
	(unless (car (get-values help 'set))
		(unmanage-child (name->widget box 'Help)))
	(manage-child box)))

(realize-widget top)
(context-main-loop (widget-context top))
