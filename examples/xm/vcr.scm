;;; -*-Scheme-*-
;;;
;;; VCR simulation


;; Initialization

(require 'motif)
(load-widgets shell bulletin-board row-column label push-button)

(define top (application-initialize 'vcr))
(define con (widget-context top))


;; The layout of the VCR's controls

(define vcr (create-managed-widget (find-class 'row-column) top))

(define panel (create-managed-widget (find-class 'bulletin-board) vcr))

(define tape (create-managed-widget (find-class 'push-button) panel))
(set-values! tape 'x 10 'y 10 'width 150 'border-width 2 'label-string 'empty
                  'recompute-size #f
		  'activate-callback (list (lambda _ (engine 'load))))

(define counter (create-managed-widget (find-class 'push-button) panel))
(set-values! counter 'x 170 'y 10 'width 50 'label-string "0"
		     'alignment "alignment_end" 'recompute-size #f)

(define function (create-managed-widget (find-class 'push-button) panel))
(set-values! function 'x 230 'y 10 'width 70 'label-string "stop"
                      'recompute-size #f)

(define buttons (create-managed-widget (find-class 'row-column) vcr))
(set-values! buttons 'orientation 'horizontal)

(define-macro (define-button label activate arm disarm)
  `(let ((b (create-managed-widget (find-class 'push-button) buttons)))
     (set-values! b 'label-string ,label)
     (add-callback b 'activate-callback (lambda _ ,activate))
     (add-callback b 'arm-callback      (lambda _ ,arm))
     (add-callback b 'disarm-callback   (lambda _ ,disarm))))

(define-button 'eject (begin (engine 'stop) (engine 'empty)) #f #f)
(define-button 'play  (engine 'play) #f #f)
(define-button 'stop  (engine 'stop) #f #f)
(define-button 'forw  (engine 'forw) (engine 'cue #t) (engine 'cue #f))
(define-button 'rew   (engine 'rew) (engine 'review #t) (engine 'review #f))
(define-button 'pause (engine 'pause) #f #f)


;; The `logic' of the VCR

(define engine
  (let ((timer #f) (interval) (loaded #f) (cnt 0) (state 'stop))

  (define (advance x)
    (set! cnt (modulo (+ cnt x) 10000000))
    (set-values! counter 'label-string (format #f "~s" cnt)))

  (define (timeout x)
    (advance x)
    (set! timer (context-add-timeout con interval (lambda _ (timeout x)))))

  (define (set-timer when x)
    (stop-timer)
    (set! interval when)
    (set! timer (context-add-timeout con when (lambda _ (timeout x)))))

  (define (stop-timer)
    (if timer (remove-timeout timer))
    (set! timer #f))

  (define (cue/review on? x)
    (if on?
	(if (not (eq? state 'play))          ; do nothing if not playing
	    state
	    (set-timer 100 x)                ; else
	    'cue/review)
	(if (not (eq? state 'cue/review))    ; do nothing if not in cue/review
	    state                            ;   mode
	    (set-timer 1000 100)             ; else switch back to play mode
	    'play)))

  (lambda (op . args)
    (call-with-current-continuation
      (lambda (return)
        (case op
          (load
	    (set-values! tape 'label-string 'loaded)
	    (set! loaded #t))
          (empty
	    (set-values! tape 'label-string 'empty)
	    (set! loaded #f))
          (else
	    (if (not loaded)
		(return #f))
            (case op
	      (stop
		(stop-timer))
	      (cue    (set! op (cue/review (car args) 100)))
	      (review (set! op (cue/review (car args) -100)))
	      (pause
		(cond ((eq? state 'pause)
		       (set-timer 1000 100)
		       (set! op 'play))
		      ((eq? state 'play)
		       (stop-timer))
		      (else
		       (return #f))))
              (forw
		 (cond ((eq? state 'pause)
			(advance 4)
			(set! op 'pause))              ; stay in pause mode
		       ((not (eq? state 'cue/review))
		        (set-timer 1000 10000))
		       (else (set! op state))))        ; stay in the old mode
              (rew
		 (cond ((eq? state 'pause)
			(advance -4)
			(set! op 'pause))
		       ((not (eq? state 'cue/review))
		        (set-timer 1000 -10000))
		       (else (set! op state))))
              (play
	         (set-timer 1000 100)))
	    (set! state op)
            (set-values! function 'label-string op))))))))

(realize-widget top)
(context-main-loop con)
