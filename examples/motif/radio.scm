;;; -*-Scheme-*-
;;;
;;; Radio box and button demo

(require 'motif)
(load 'radio-stuff.scm)

(define top (application-initialize 'radio))

(define rc (create-managed-widget (find-class 'row-column) top))


;;; Create a button box containing arrow buttons; add callbacks
;;; to each button:

(define box1 (create-radio-box 'arrow-button rc))

(define buttons1
  (map (lambda (dir)
	 (radio-box-add-button! box1 'width 50 'height 30
				'arrow-direction dir))
       '(arrow_up arrow_down arrow_left arrow_right)))

(for-each
  (lambda (w)
    (for-each
      (lambda (cb)
        (add-callback w cb
	  (lambda (w r)
	    (print (list w (car r))))))
    '(activate-callback arm-callback disarm-callback)))
  buttons1)

;;; Create a button box containing push buttons; define an
;;; entry callback:

(define box2 (create-radio-box 'push-button rc))

(add-callback box2 'entry-callback
	     (lambda (w args)
	       (print (car (get-values (caddr args) 'label-string)))))

(define buttons2
  (map (lambda (label)
	 (radio-box-add-button! box2 'label-string label
				'alignment "alignment_center"))
       '(Play Stop Record Rewind Forward)))


;;; Create a button box containing toggle buttons; add a callback
;;; to each button:

(define box3 (create-radio-box 'toggle-button rc))

(define buttons3
  (map (lambda (label)
	 (radio-box-add-button! box3 'label-string label))
       '(KMQX WMQY WHFX KWIT)))

(for-each
  (lambda (w)
    (add-callback w 'value-changed-callback
		  (lambda r 
		    (let ((station (car (get-values w 'label-string)))
			  (set? (car (get-values w 'set))))
		      (print (list station set?))
		      (if (and (string=? station "KWIT") set?) (exit))))))
  buttons3)


(realize-widget top)
(context-main-loop (widget-context top))
