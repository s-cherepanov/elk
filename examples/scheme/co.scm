;;; -*-Scheme-*-

(require 'cscheme)

(define (displayLine . someArgs)
  (for-each
   (lambda (aTerm) (display aTerm) (display " "))
   someArgs)
  (newline))

(define (Monitor)

  (define stopAtMonitorLevel #f)
  (define clock 0)
  (define stopTime 0)
  (define processIndicators '())

  (define (setInitialProcessState! aContinuation)
    (set! processIndicators
	  (cons (list 0 aContinuation) processIndicators))
    (stopAtMonitorLevel #f))

  (define (startSimulation! aDuration)
    (set! stopTime aDuration)
    (if (not (null? processIndicators))
	(let ((firstIndicatorOnList (car processIndicators)))
	  (set! processIndicators
		(remove firstIndicatorOnList processIndicators))
	  (resumeSimulation! firstIndicatorOnList))
	(displayLine "*** no active process recorded!")))
  
  (define (resumeSimulation! aProcessState)
    (set! processIndicators
	  (cons aProcessState processIndicators))
    (let ((nextProcessState aProcessState))
      (for-each (lambda (aStatePair)
		  (if (< (car aStatePair) (car nextProcessState))
		      (set! nextProcessState aStatePair)))
		processIndicators)
      (let ((time (car nextProcessState))
	    (continuation (cadr nextProcessState)))
	(set! processIndicators
	      (remove nextProcessState processIndicators))
	(if (<= time stopTime)
	    (begin (set! clock time)
		   (continuation #f))
	    (begin (displayLine "*** simulation stops at:" clock)
		   (stopAtMonitorLevel #f))))))

  (define (dispatch aMessage . someArguments)
    (cond ((eq? aMessage 'initialize)
	   (setInitialProcessState! (car someArguments)))
	  ((eq? aMessage 'startSimulation)
	   (startSimulation! (car someArguments)))
	  ((eq? aMessage 'proceed)
	   (resumeSimulation! (car someArguments)))
	  ((eq? aMessage 'time)
	   clock)
	  ((eq? aMessage 'processIndicators)
	   processIndicators)
	  (else
	   "Sorry, I don't know how to do this!")))

  (call-with-current-continuation
   (lambda (anArg)
     (set! stopAtMonitorLevel anArg)))
  dispatch)
	    
		      
    
    
(define (Tourist aName aMonitor)
  (call-with-current-continuation
   (lambda (anArg)
     (aMonitor 'initialize anArg)))
  (displayLine aName "starts at" (aMonitor 'time))
  (while #t
   (displayLine aName "walks on at" (aMonitor 'time))
   (call-with-current-continuation
    (lambda (anArg)
      (aMonitor 'proceed
		(list (+ (aMonitor 'time) 1) anArg))))
    (displayLine aName "arrives at new attraction at" (aMonitor 'time))
    (call-with-current-continuation
     (lambda (anArg)
       (aMonitor 'proceed
		 (list (+ (aMonitor 'time) 2)
		       anArg))))))


(define Gallery (Monitor))

(Tourist 'Jane  Gallery)
(Tourist 'Bruce Gallery)

(Gallery 'startSimulation 5)
