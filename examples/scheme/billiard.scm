;;; BILLIARD.SCM: This file contains code for a very simple billiard ball
;;;               simulator.  The simulation takes place in two dimensions.
;;;               The balls are really disks in that their height is not taken
;;;               into account.  All interactions are assumed to be
;;;               frictionless so spin in irrelevant and not accounted for.
;;;               (See section on limitations.)
;;;
;;; NOTES: A simulation is initiated by creating a number of balls and bumpers
;;;        and and specifying a duration for the simulation.  For each ball,
;;;        its mass, radius, initial position, and initial velocity must be
;;;        specified.  For each bumper, the location of its two ends must be
;;;        specified.  (Bumpers are assumed to have zero width.)
;;;
;;;        A sample run might be started as follows:
;;;        (simulate
;;;         (list (make-ball 2 1 9 5 -1 -1)
;;;               (make-ball 4 2 2 5 1 -1))
;;;         (list (make-bumper 0 0 0 10)
;;;               (make-bumper 0 0 10 0)
;;;               (make-bumper 0 10 10 10)
;;;               (make-bumper 10 0 10 10))
;;;         30)
;;;
;;;        It would create one billiard ball of mass 2 and radius 1 at position
;;;        (9, 5) with initial velocity (-1, -1) and a second ball of mass 4
;;;        and radius 2 at position (2, 5) with initial velocity (1, -1).  The
;;;        table would be a 10X10 square.  (See diagram below)
;;;
;;;        +---------------------------+
;;;        |                           |
;;;        |                           |
;;;        |    XXXX                   |
;;;        |  XXXXXXXX             XX  |
;;;        |XXXXXX4XXXXX         XXX2XX|
;;;        |  XXXXXXXX            /XX  |
;;;        |    XXXX \                 |
;;;        |                           |
;;;        |                           |
;;;        +---------------------------+
;;;
;;; LIMITATIONS:  This simulator does not handle 3 body problems correctly.  If
;;;               3 objects interact at one time, only the interactions of 2 of
;;;               the bodies will be accounted for.  This can lead to strange
;;;               effects like balls tunneling through walls and other balls.
;;;               It is also possible to get balls bouncing inside of each
;;;               other in this way. 
;;;           


;;MAKE-QUEUE-RECORD returns a queue record with the given next, previous, and
;;value values
;;NEXT = The next record pointer
;;PREV = The previous record pointer
;;REST = A list of values for any optional fields (this can be used for
;;       creating structure inheritance)
(define-macro (make-queue-record next prev . rest)
  `(vector ,next ,prev ,@rest))
	  
;;QUEUE-RECORD-NEXT returns the next field of the given queue record
;;QUEUE-RECORD = The queue record whose next field is to be returned
(define-macro (queue-record-next queue-record)
  `(vector-ref ,queue-record 0))

;;SET-QUEUE-RECORD-NEXT! sets the next field of the given queue record
;;QUEUE-RECORD = The queue record whose next field is to be set
;;VALUE = The value to which the next field is to be set
(define-macro (set-queue-record-next! queue-record value)
  `(vector-set! ,queue-record 0 ,value))

;;QUEUE-RECORD-PREV returns the prev field of the given queue record
;;QUEUE-RECORD = The queue record whose prev field is to be returned
(define-macro (queue-record-prev queue-record)
  `(vector-ref ,queue-record 1))

;;SET-QUEUE-RECORD-PREV! sets the prev field of the given queue record
;;QUEUE-RECORD = The queue record whose prev field is to be set
;;VALUE = The value to which the prev field is to be set
(define-macro (set-queue-record-prev! queue-record value)
  `(vector-set! ,queue-record 1 ,value))

;;QUEUE-RECORD-LEN returns the length of a queue record which has no optional
;;fields 
(define-macro (queue-record-len) 2)

;;QUEUE-HEAD returns a dummy record at the end of the queue with the record
;;with the smallest key.
;;QUEUE = the queue whose head record is to be returned
(define-macro (queue-head queue)
  `(vector-ref ,queue 0))

;;QUEUE-TAIL returns a dummy record at the end of the queue with the record
;;with the largest key.
;;QUEUE = the queue whose tail record is to be returned
(define-macro (queue-tail queue)
  `(vector-ref ,queue 1))

;;QUEUE-<? returns the less-than comparitor to be used in sorting
;;records into the queue
;;QUEUE = The queue whose comparitor is to be returned
(define-macro (queue-<? queue)
  `(vector-ref ,queue 2))


;;MAKE-SORTED-QUEUE returns a queue object.  A queue header is a vector which
;;contains a head pointer, a tail pointer, and a less-than comparitor. 
;;QUEUE-<? = A predicate for sorting queue items
(define (make-sorted-queue queue-<?)
  (let ((queue
	 (vector
	  (make-queue-record		;The queue head record has no initial
	   '()				;next, previous, or value values
	   '())
	  (make-queue-record		;The queue tail record has no intial
	   '()				;next, previous, or value values
	   '())
	  queue-<?)))
    (set-queue-record-next!
     (queue-head queue)
     (queue-tail queue))
    (set-queue-record-prev!
     (queue-tail queue)
     (queue-head queue))
    queue))

;;MAKE-EVENT-QUEUE-RECORD returns an event queue record with the given next,
;;previous, object, and collision-time values
;;NEXT = The next record pointer
;;PREV = The previous record pointer
;;OBJECT = The simulation object associated with this record
;;COLLISION-TIME = The collision time for this object
(define-macro (make-event-queue-record next prev object collision-time)
  `(make-queue-record ,next ,prev ,object ,collision-time))

;;EVENT-QUEUE-RECORD-OBJECT returns the object associated with the given record
;;QUEUE-RECORD = The queue record whose object field is to be returned
(define-macro (event-queue-record-object queue-record)
  `(vector-ref ,queue-record ,(queue-record-len)))

;;EVENT-QUEUE-COLLISION-TIME returns the collision time associated with the
;;given queue record
;;QUEUE-RECORD = The queue record whose collision time field is to be returned
(define-macro (event-queue-record-collision-time queue-record)
  `(vector-ref ,queue-record ,(1+ (queue-record-len))))

;;SET-EVENT-QUEUE-COLLISION-TIME! sets the collision time associated with the
;;given queue record
;;QUEUE-RECORD = The queue record whose collision time field is to be returned
;;VALUE = The value to which it is to be set
(define-macro (set-event-queue-record-collision-time! queue-record value)
  `(vector-set! ,queue-record ,(1+ (queue-record-len)) ,value))


;;QUEUE-INSERT inserts the given record in the given queue based on its value
;;QUEUE = The queue into which the record is to be inserted
;;QUEUE-RECORD = The record to be inserted in the queue
(define (queue-insert queue queue-record)
  (define (actual-insert insert-record next-record)
    (if (or				;If the insert position has been found
	 (eq? next-record		;or the end on the queue has been 
	      (queue-tail queue))	;reached
	 ((queue-<? queue)		
	  insert-record
	  next-record))
	(sequence			;Link the insert record into the queue
	  (set-queue-record-next!	;just prior to next-record
	   (queue-record-prev
	    next-record)
	   insert-record)
	  (set-queue-record-prev!
	   insert-record
	   (queue-record-prev
	    next-record))
	  (set-queue-record-next!
	   insert-record
	   next-record)
	  (set-queue-record-prev!
	   next-record
	   insert-record))
	(actual-insert			;Else, continue searching for the 
	 insert-record			;insert position
	 (queue-record-next
	  next-record))))
  (actual-insert			;Search for the correct position to 
   queue-record				;perform the insert starting at the
   (queue-record-next			;queue head and perform the insert 
    (queue-head queue))))		;once this position has been found
     
;;QUEUE-REMOVE removes the given queue record from its queue
;;QUEUE-RECORD = The record to be removed from the queue
(define (queue-remove queue-record)
  (set-queue-record-next!
   (queue-record-prev
    queue-record)
   (queue-record-next
    queue-record))
  (set-queue-record-prev!
   (queue-record-next
    queue-record)
   (queue-record-prev
    queue-record)))

;;QUEUE-SMALLEST returns the queue record with the smallest key on the given
;;queue 
;;QUEUE = The queue from which the smallest record is to be extracted
(define (queue-smallest queue)
  (queue-record-next
   (queue-head queue)))


;;CLEAR-QUEUE! clears the given queue by destructively removing all the records
;;QUEUE = The queue to be cleared
(define (clear-queue queue)
  (set-queue-record-next!
   (queue-head queue)
   (queue-tail queue))
  (set-queue-record-prev!
   (queue-tail queue)
   (queue-head queue)))

;;EMPTY-QUEUE? returns true if the given queue is empty
;;QUEUE = The queue to be tested for emptiness
(define (empty-queue? queue)
  (eq? (queue-record-next
	(queue-head queue))
       (queue-tail queue)))


;;MAKE-SIMULATION-OBJECT returns a simulation object containing the given
;;fields 
;;COLLISION-PROCEDURE = A function for processing information about a potential
;;                      collision between this object and some ball
;;REST = A list of values for any optional fields (this can be used for
;;       creating structure inheritance)
(define-macro (make-simulation-object collision-procedure . rest)
  `(vector ,collision-procedure ,@rest))

;;SIMULATION-OBJECT-COLLLISION-PROCEDURE returns the collision procedure for
;;the given simulation object
;;OBJECT = The object whose collision procedure is to be returned
(define-macro (simulation-object-collision-procedure object)
  `(vector-ref ,object 0))

;;SIMULATION-OBJECT-LEN returns the length of a simulation object which has no
;;optional fields
(define-macro (simulation-object-len) 1)


;;ACTUAL-MAKE-BALL returns a ball object
;;BALL-NUMBER = An index into the ball vector for this ball
;;MASS = The ball's mass
;;RADIUS = The ball's radius
;;PX = The x-coordinate of the ball's initial position
;;PY = The y-coordinate of the ball's initial position
;;VX = The x-coordinate of the ball's initial velocity
;;VY = The y-coordinate of the ball's initial velocity
(define-macro (actual-make-ball ball-number mass radius px py vx vy)
  `(make-simulation-object
    ball-collision-procedure		;The collision procedure for a ball
    ,ball-number
    ,mass
    ,radius
    (make-sorted-queue			;The event queue
     collision-time-<?)
    0					;Time of last collision
    ,px					;Position of last collision
    ,py					; "
    ,vx					;Velocity following last colliosion
    ,vy					; "
    '()					;No vector of queue records for ball's
					;with smaller numbers  
    '()					;No vector of queue records for bumpers
    '()					;No list of balls with larger numbers
    '()))				;No global event queue record, yet
  
(define (make-ball mass radius px py vx vy)
  (actual-make-ball '() mass radius px py vx vy))

;;BALL-NUMBER returns the index of the given ball
;;BALL = The ball whose index is to be returned
(define-macro (ball-number ball)
  `(vector-ref ,ball ,(simulation-object-len)))

;;SET-BALL-NUMBER! set the index of the given ball to the given value
;;BALL = The ball whose index is to be set
;;VALUE = The value to which it is to be set
(define-macro (set-ball-number! ball value)
  `(vector-set! ,ball ,(simulation-object-len) ,value))

;;BALL-MASS returns the mass of the given ball
;;BALL = The ball whose mass is to be returned
(define-macro (ball-mass ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 1)))

;;BALL-RADIUS returns the radius of the given ball
;;BALL = The ball whose radius is to be returned
(define-macro (ball-radius ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 2)))

;;BALL-EVENT-QUEUE returns the sort queue of collision events for the given
;;ball
;;BALL = The ball whose event is to be returned
(define-macro (ball-event-queue ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 3)))

;;BALL-COLLISION-TIME returns the time of the last collision for the given ball
;;BALL = The ball whose collision time is to be returned
(define-macro (ball-collision-time ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 4)))


;;SET-BALL-COLLISION-TIME! sets the time of the last collision for the given
;;ball 
;;BALL = The ball whose collision time is to be set
;;VALUE = The value to which the ball's collision time is to be set
(define-macro (set-ball-collision-time! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 4) ,value))

;;BALL-COLLISION-X-POSITION returns the x-coordinate of the position  of the
;;last collision for the given ball 
;;BALL = The ball whose collision position is to be returned
(define-macro (ball-collision-x-position ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 5)))

;;SET-BALL-COLLISION-X-POSITION! sets the x-coordinate of the position of the
;;last collision for the given ball 
;;BALL = The ball whose collision position is to be set
;;VALUE = The value to which the ball's collision position is to be set
(define-macro (set-ball-collision-x-position! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 5) ,value))

;;BALL-COLLISION-Y-POSITION returns the y-coordinate of the position  of the
;;last collision for the given ball 
;;BALL = The ball whose collision position is to be returned
(define-macro (ball-collision-y-position ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 6)))

;;SET-BALL-COLLISION-Y-POSITION! sets the y-coordinate of the position of the
;;last collision for the given ball 
;;BALL = The ball whose collision position is to be set
;;VALUE = The value to which the ball's collision position is to be set
(define-macro (set-ball-collision-y-position! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 6) ,value))

;;BALL-X-VELOCITY returns the x-coordinate of the velocity of the given ball
;;following its last collision
;;BALL = The ball whose velocity is to be returned
(define-macro (ball-x-velocity ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 7)))

;;SET-BALL-X-VELOCITY! sets the x-coordinate of the velocity of the given ball
;;BALL = The ball whose velocity is to be set
;;VALUE = The value to which the ball's velocity is to be set
(define-macro (set-ball-x-velocity! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 7) ,value))

;;BALL-Y-VELOCITY returns the y-coordinate of the velocity  of the given ball
;;following its last collision
;;BALL = The ball whose velocity is to be returned
(define-macro (ball-y-velocity ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 8)))

;;SET-BALL-Y-VELOCITY! sets the y-coordinate of the velocity of the given ball
;;BALL = The ball whose velocity is to be set
;;VALUE = The value to which the ball's velocity is to be set
(define-macro (set-ball-y-velocity! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 8) ,value))


;;BALL-BALL-VECTOR returns the vector of queue records for balls with smaller
;;ball numbers
;;BALL = The ball whose ball vector is to be returned
(define-macro (ball-ball-vector ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 9)))

;;SET-BALL-BALL-VECTOR! sets the vector of queue records for balls with smaller
;;ball numbers
;;BALL = The ball whose ball vector is to be set
;;VALUE = The vector to which the field is to be set
(define-macro (set-ball-ball-vector! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 9) ,value))

;;BALL-BUMPER-VECTOR returns the vector of queue records for bumpers
;;BALL = The ball whose bumper vector is to be returned
(define-macro (ball-bumper-vector ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 10)))

;;SET-BALL-BUMPER-VECTOR! sets the vector of queue records for bumpers
;;BALL = The ball whose bumper vector is to be set
;;VALUE = The vector to which the field is to be set
(define-macro (set-ball-bumper-vector! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 10) ,value))

;;BALL-BALL-LIST returns a list of balls with larger ball numbers than the
;;given ball
;;BALL = The ball whose ball list is to be returned
(define-macro (ball-ball-list ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 11)))

;;SET-BALL-BALL-LIST! sets the list of balls with larger ball numbers than the
;;given ball
;;BALL = The ball whose ball list is to be set
;;VALUE = The value to which the ball list is to be set
(define-macro (set-ball-ball-list! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 11) ,value))

;;BALL-GLOBAL-EVENT-QUEUE-RECORD returns the global event queue record for the
;;given ball
;;BALL = The ball whose global event queue record is to be returned
(define-macro (ball-global-event-queue-record ball)
  `(vector-ref ,ball ,(+ (simulation-object-len) 12)))

;;SET-BALL-GLOBAL-EVENT-QUEUE-RECORD! set the global event queue record for the
;;given ball to the given value
;;BALL = The ball whose global event queue record is to be set
;;VALUE = The value to which the global event queue record field is to be set
(define-macro (set-ball-global-event-queue-record! ball value)
  `(vector-set! ,ball ,(+ (simulation-object-len) 12) ,value))



;;ACTUAL-MAKE-BUMPER returns a bumper object
;;BUMPER-NUMBER = An index into the bumper vector for this bumper
;;X1 = The x-coordiante of one end of the bumper
;;Y1 = The y-coordiante of one end of the bumper
;;X2 = The x-coordiante of the other end of the bumper
;;Y2 = The y-coordiante of the other end of the bumper
(define-macro (actual-make-bumper bumper-number x1 y1 x2 y2)
  `(make-simulation-object
    bumper-collision-procedure		;The collision procedure for a bumper
    ,bumper-number
    ,x1					;The bumper endpoints
    ,y1
    ,x2
    ,y2))

(define (make-bumper x1 y1 x2 y2)
  (actual-make-bumper '() x1 y1 x2 y2))

;;BUMPER-NUMBER returns the index of the given bumper
;;BUMPER = The bumper whose index is to be returned
(define-macro (bumper-number bumper)
  `(vector-ref ,bumper ,(simulation-object-len)))

;;SET-BUMPER-NUMBER! set the index of the given bumper to the given value
;;BUMPER = The bumper whose index is to be set
;;VALUE = The value to which it is to be set
(define-macro (set-bumper-number! bumper value)
  `(vector-set! ,bumper ,(simulation-object-len) ,value))

;;BUMPER-X1 returns the x-coordinate of one end of the given bumber
;;BUMPER = the bumper whose x-coordinate is to be returned
(define-macro (bumper-x1 bumper)
  `(vector-ref ,bumper ,(1+ (simulation-object-len))))

;;SET-BUMPER-X1! sets the x-coordinate of one end of the given bumber
;;BUMPER = the bumper whose x-coordinate is to be set
;;VALUE = The value to which the bumpers x-coordinate is to be set
(define-macro (set-bumper-x1! bumper value)
  `(vector-set! ,bumper ,(1+ (simulation-object-len)) ,value))

;;BUMPER-Y1 returns the y-coordinate of one end of the given bumber
;;BUMPER = the bumper whose y-coordinate is to be returned
(define-macro (bumper-y1 bumper)
  `(vector-ref ,bumper ,(+ (simulation-object-len) 2)))

;;SET-BUMPER-Y1! sets the y-coordinate of one end of the given bumber
;;BUMPER = the bumper whose y-coordinate is to be set
;;VALUE = The value to which the bumpers y-coordinate is to be set
(define-macro (set-bumper-y1! bumper value)
  `(vector-set! ,bumper ,(+ (simulation-object-len) 2) ,value))

;;BUMPER-X2 returns the x-coordinate of the other end of the given bumber
;;BUMPER = the bumper whose x-coordinate is to be returned
(define-macro (bumper-x2 bumper)
  `(vector-ref ,bumper ,(+ (simulation-object-len) 3)))

;;SET-BUMPER-X2! sets the x-coordinate of the other end of the given bumber
;;BUMPER = the bumper whose x-coordinate is to be set
;;VALUE = The value to which the bumpers x-coordinate is to be set
(define-macro (set-bumper-x2! bumper value)
  `(vector-set! ,bumper ,(+ (simulation-object-len) 3) ,value))


;;BUMPER-Y2 returns the y-coordinate of the other end of the given bumber
;;BUMPER = the bumper whose y-coordinate is to be returned
(define-macro (bumper-y2 bumper)
  `(vector-ref ,bumper ,(+ (simulation-object-len) 4)))

;;SET-BUMPER-Y2! sets the y-coordinate of the other end of the given bumber
;;BUMPER = the bumper whose y-coordinate is to be set
;;VALUE = The value to which the bumpers y-coordinate is to be set
(define-macro (set-bumper-y2! bumper value)
  `(vector-set! ,bumper ,(+ (simulation-object-len) 4) ,value))

;;COLLISION-TIME-<? is a predicate which returns true if the first event queueu
;;record represents a collision that will take place at an earlier time than
;;the one for the second event queue record
;;EVENT-QUEUE-RECORD1 = The first event queue record
;;EVENT-QUEUE-RECORD2 = The second event queue record
(define (collision-time-<? event-queue-record1 event-queue-record2)
  (time-<?
   (event-queue-record-collision-time
    event-queue-record1)
   (event-queue-record-collision-time
    event-queue-record2)))

;;TIME-<? is a predicate which returns true if the first time is smaller than
;;the second.  '() represents a time infinitly large.
(define (time-<? time1 time2)
  (if (null? time1)
      #f
      (if (null? time2)
	  #t
	  (< time1 time2))))

;;SQUARE returns the square of its argument
(define (square x)
  (* x x))


;;BALL-BALL-COLLISION-TIME returns the time at which the two given balls would
;;collide if neither interacted with any other objects, '() if never.  This
;;calculation is performed by setting the distance between the balls to the sum
;;of their radi and solving for the contact time.
;;BALL1 = The first ball
;;BALL2 = The second ball
(define (ball-ball-collision-time ball1 ball2)
  (let ((delta-x-velocity		;Cache the difference in the ball's
	 ( - (ball-x-velocity ball2)	;velocities,
	     (ball-x-velocity ball1)))
	(delta-y-velocity
	 ( - (ball-y-velocity ball2)	
	     (ball-y-velocity ball1)))
	(radius-sum			;the sum of their radi,
	 (+ (ball-radius ball1)
	    (ball-radius ball2)))
	(alpha-x			;and common subexpressions in the time
	 (-				;equation
	  (- (ball-collision-x-position
	      ball2)
	     (ball-collision-x-position
	      ball1))
	  (-
	   (* (ball-x-velocity ball2)	
	      (ball-collision-time
	       ball2))
	   (* (ball-x-velocity ball1)	
	      (ball-collision-time
	       ball1)))))
	(alpha-y
	 (-
	  (- (ball-collision-y-position
	      ball2)
	     (ball-collision-y-position
	      ball1))
	  (-
	   (* (ball-y-velocity ball2)	
	      (ball-collision-time
	       ball2))
	   (* (ball-y-velocity ball1)	
	      (ball-collision-time
	       ball1))))))
    (let* ((delta-velocity-magnitude-squared
	    (+ (square
		delta-x-velocity)
	       (square		
		delta-y-velocity)))
	   (discriminant
	    (- (* (square radius-sum)
		  delta-velocity-magnitude-squared)
	       (square
		(- (* delta-y-velocity
		      alpha-x)
		   (* delta-x-velocity
		      alpha-y))))))


      (if (or (negative? discriminant)	;If the balls don't colloide:
	      (zero?
	       delta-velocity-magnitude-squared))
	  '()				;Return infinity
	  (let ((time			;Else, calculate the collision time
		 (/
		  (- 0
		     (+ (sqrt discriminant)
			(+
			 (* delta-x-velocity
			    alpha-x)
			 (* delta-y-velocity
			    alpha-y))))
		  (+ (square
		      delta-x-velocity)
		     (square
		      delta-y-velocity)))))
	    (if (and			;If the balls collide in the future:
		 (time-<?
		  (ball-collision-time
		   ball1)
		  time)
		 (time-<?
		  (ball-collision-time
		   ball2)
		  time))
		time			;Return the collision time
		'()))))))		;Else, return that they never collide

;;BALL-BUMPER-COLLISION-TIME returns the time at which the given ball would
;;collide with the given bumper if the ball didn't interacted with any other
;;objects, '() if never.  This is done by first calculating the time at which
;;the ball would collide with a bumper of infinite length and then checking if
;;the collision position represents a portion of the actual bumper.
;;BALL = The ball
;;BUMPER = The bumper
(define (ball-bumper-collision-time ball bumper)
  (let ((delta-x-bumper			;Collision time with the bumper of 
	 (- (bumper-x2 bumper)		;infinite extent is calculated by 
	    (bumper-x1 bumper)))	;setting the distance between the ball
	(delta-y-bumper			;and the bumper to be the radius of the
	 (- (bumper-y2 bumper)		;ball and solving for the time.  The
	    (bumper-y1 bumper))))	;distance is calculated by |aXb|/|a|,
    (let ((bumper-length-squared	;where 'a' is the vector from one end
	   (+ (square delta-x-bumper)	;of the bumper to the other and 'b' is
	      (square delta-y-bumper)))	;the vector from the first end of the 
	  (denominator			;bumper to the center of the ball
	   (- (* (ball-y-velocity ball)
		 delta-x-bumper)
	      (* (ball-x-velocity ball)
		 delta-y-bumper))))
      (if (zero? denominator)		;If the ball's motion is parallel to
					;the bumper:
	  '()				;Return infinity
	  (let ((delta-t		;Calculate the collision time
		 (-
		  (/
		   (+
		    (*
		     (-	(ball-collision-x-position
			 ball)
			(bumper-x1 bumper))
		     delta-y-bumper)
		    (*
		     (- (ball-collision-y-position
			 ball)
			(bumper-y1 bumper))
		     delta-x-bumper))
		   denominator)
		  (/
		   (* (ball-radius
		       ball)
		      (sqrt
		       bumper-length-squared))
		   (abs denominator)))))
	    (if (not (positive?		;If the ball is moving away from the
		      delta-t))		;bumper:
		'()			;Return infinity


		(let ((ball-x-contact	;Whether the ball contacts the actual
		       (+ (ball-collision-x-position ;bumper of limited extent
			   ball)	;will be determined by comparing |b.a|
			  (* (ball-x-velocity ;with |a|^2
			      ball)
			     delta-t)))
		      (ball-y-contact
		       (+ (ball-collision-y-position
			   ball)
			  (* (ball-y-velocity
			      ball)
			     delta-t))))
		  (let ((delta-x-ball
			 (- ball-x-contact
			    (bumper-x1
			     bumper)))
			(delta-y-ball
			 (- ball-y-contact
			    (bumper-y1
			     bumper))))
		    (let ((dot-product
			   (+
			    (* delta-x-ball
			       delta-x-bumper)
			    (* delta-y-ball
			       delta-y-bumper))))
		      (if (or		;If the ball misses the bumper on 
			   (negative?	;either end:
			    dot-product)
			   (> dot-product
			      bumper-length-squared))
			  '()		;Return infinity
			  (+ delta-t	;Else, return the contact time
			     (ball-collision-time
			      ball))))))))))))
			       

;;BALL-COLLISION-PROCEDURE calculates the new velocities of the given balls
;;based on their collision at the given time.  Also, tells all other balls
;;about the new trajectories of these balls so they can update their event
;;queues 
;;BALL1 = The first ball
;;BALL2 = The second ball
;;COLLISION-TIME = The collision time
;;GLOBAL-EVENT-QUEUE = The global queue of earliest events for each ball
(define (ball-collision-procedure ball1 ball2 collision-time
				  global-event-queue)
  (queue-remove				;Remove the earliest event associated
   (ball-global-event-queue-record	;with each ball from the global event 
    ball1))				;queue
  (queue-remove
   (ball-global-event-queue-record
    ball2))
  (let ((ball1-collision-x-position	;Calculate the positions of both balls
	 (+ (ball-collision-x-position	;when they collide
	     ball1)
	    (* (ball-x-velocity
		ball1)
	       (- collision-time
		  (ball-collision-time
		   ball1)))))
	(ball1-collision-y-position
	 (+ (ball-collision-y-position
	     ball1)
	    (* (ball-y-velocity
		ball1)
	       (- collision-time
		  (ball-collision-time
		   ball1)))))
	(ball2-collision-x-position
	 (+ (ball-collision-x-position
	     ball2)
	    (* (ball-x-velocity
		ball2)
	       (- collision-time
		  (ball-collision-time
		   ball2)))))
	(ball2-collision-y-position
	 (+ (ball-collision-y-position
	     ball2)
	    (* (ball-y-velocity
		ball2)
	       (- collision-time
		  (ball-collision-time
		   ball2))))))
    (let ((delta-x			;Calculate the displacements of the
	   (- ball2-collision-x-position ;centers of the two balls
	      ball1-collision-x-position))
	  (delta-y
	   (- ball2-collision-y-position
	      ball1-collision-y-position)))


      (let* ((denominator		;Calculate the angle of the line 
	      (sqrt (+ (square		;joining the centers at the collision 
			delta-x)	;time with the x-axis  (this line is
		       (square		;the normal to the balls at the
			delta-y))))	;collision point)
	     (cos-theta			
	      (/ delta-x denominator))
	     (sin-theta
	      (/ delta-y denominator)))
	  (let ((ball1-old-normal-velocity ;Convert the velocities of the balls
		 (+ (* (ball-x-velocity	;into the coordinate system defined by 
			ball1)		;the normal and tangential lines at 
		       cos-theta)	;the collision point
		    (* (ball-y-velocity
			ball1)
		       sin-theta)))
		(ball1-tang-velocity
		 (- (* (ball-y-velocity
			ball1)
		       cos-theta)
		    (* (ball-x-velocity
			ball1)
		       sin-theta)))
		(ball2-old-normal-velocity
		 (+ (* (ball-x-velocity
			ball2)
		       cos-theta)
		    (* (ball-y-velocity
			ball2)
		       sin-theta)))
		(ball2-tang-velocity
		 (- (* (ball-y-velocity
			ball2)
		       cos-theta)
		    (* (ball-x-velocity
			ball2)
		       sin-theta)))
		(mass1 (ball-mass
			ball1))
		(mass2 (ball-mass
			ball2)))
	    (let ((ball1-new-normal-velocity ;Calculate the new velocities
		   (/			;following the collision (the 
		    (+			;tangential velocities are unchanged
		     (*			;because the balls are assumed to be
		      (* 2		;frictionless)
			 mass2)
		      ball2-old-normal-velocity)
		     (*
		      (- mass1 mass2)
		      ball1-old-normal-velocity))
		    (+ mass1 mass2)))


		  (ball2-new-normal-velocity
		   (/
		    (+
		     (*
		      (* 2
			 mass1)
		      ball1-old-normal-velocity)
		     (*
		      (- mass2 mass1)
		      ball2-old-normal-velocity))
		    (+ mass1 mass2))))
	      (set-ball-x-velocity!	;Store data about the collision in the
	       ball1			;structure for each ball after 
	       (- (* ball1-new-normal-velocity ;converting the information back
		     cos-theta)		;to the x,y frame
		  (* ball1-tang-velocity
		     sin-theta)))
	      (set-ball-y-velocity!
	       ball1
	       (+ (* ball1-new-normal-velocity
		     sin-theta)
		  (* ball1-tang-velocity
		     cos-theta)))
	      (set-ball-x-velocity!
	       ball2
	       (- (* ball2-new-normal-velocity
		     cos-theta)
		  (* ball2-tang-velocity
		     sin-theta)))
	      (set-ball-y-velocity!
	       ball2
	       (+ (* ball2-new-normal-velocity
		     sin-theta)
		  (* ball2-tang-velocity
		     cos-theta)))
	      (set-ball-collision-time!
	       ball1
	       collision-time)
	      (set-ball-collision-time!
	       ball2
	       collision-time)
	      (set-ball-collision-x-position!
	       ball1
	       ball1-collision-x-position)
	      (set-ball-collision-y-position!
	       ball1
	       ball1-collision-y-position)
	      (set-ball-collision-x-position!
	       ball2
	       ball2-collision-x-position)
	      (set-ball-collision-y-position!
	       ball2
	       ball2-collision-y-position))))))


  (newline)
  (display "Ball ")
  (display (ball-number ball1))
  (display " collides with ball ")
  (display (ball-number ball2))
  (display " at time ")
  (display (ball-collision-time ball1))
  (newline)
  (display "   Ball ")
  (display (ball-number ball1))
  (display " has a new velocity of ")
  (display (ball-x-velocity ball1))
  (display ",")
  (display (ball-y-velocity ball1))
  (display " starting at ")
  (display (ball-collision-x-position ball1))
  (display ",")
  (display (ball-collision-y-position ball1))
  (newline)
  (display "   Ball ")
  (display (ball-number ball2))
  (display " has a new velocity of ")
  (display (ball-x-velocity ball2))
  (display ",")
  (display (ball-y-velocity ball2))
  (display " starting at ")
  (display (ball-collision-x-position ball2))
  (display ",")
  (display (ball-collision-y-position ball2))

  (recalculate-collisions ball1 global-event-queue)
  (recalculate-collisions ball2 global-event-queue))


;;BUMPER-COLLISION-PROCEDURE calculates the new velocity of the given ball
;;following its collision with the given bumper at the given time.  Also, tells
;;other balls about the new trajectory of the given ball so they can update
;;their event queues.
;;BALL = The ball
;;BUMPER = The bumper
;;COLLISION-TIME = The collision time
;;GLOBAL-EVENT-QUEUE = The global queue of earliest events for each ball
(define (bumper-collision-procedure ball bumper collision-time
				    global-event-queue)
  (queue-remove				;Remove the earliest event associated
   (ball-global-event-queue-record	;with the ball from the global event 
    ball))				;queue
  (let ((delta-x-bumper			;Compute the bumper's delta-x
	 (- (bumper-x2 bumper)
	    (bumper-x1 bumper)))
	(delta-y-bumper			;delta-y
	 (- (bumper-y2 bumper)
	    (bumper-y1 bumper))))
    (let ((bumper-length		;length
	   (sqrt
	    (+ (square
		delta-x-bumper)
	       (square
		delta-y-bumper)))))
      (let ((cos-theta			;and cosine and sine of its angle with
	     (/ delta-x-bumper		;respect to the positive x-axis
		bumper-length))
	    (sin-theta
	     (/ delta-y-bumper
		bumper-length))
	    (x-velocity			;Cache the ball's velocity in the x,y
	     (ball-x-velocity ball))	;frame
	    (y-velocity
	     (ball-y-velocity ball)))
	(let ((tang-velocity		;Calculate the ball's velocity in the
	       (+ (* x-velocity		;bumper frame
		     cos-theta)
		  (* y-velocity
		     sin-theta)))
	      (normal-velocity
	       (- (* y-velocity
		     cos-theta)
		  (* x-velocity
		     sin-theta))))


	  (set-ball-collision-x-position! ;Store the collision position
	   ball
	   (+ (ball-collision-x-position
	       ball)
	      (* (- collision-time
		    (ball-collision-time
		     ball))
		 (ball-x-velocity
		  ball))))
	  (set-ball-collision-y-position!
	   ball
	   (+ (ball-collision-y-position
	       ball)
	      (* (- collision-time
		    (ball-collision-time
		     ball))
		 (ball-y-velocity
		  ball))))
	  (set-ball-x-velocity!		;Calculate the new velocity in the 
	   ball				;x,y frame based on the fact that 
	   (+ (* tang-velocity		;tangential velocity is unchanged and
		 cos-theta)		;the normal velocity is inverted when
	      (* normal-velocity	;the ball collides with the bumper
		 sin-theta)))
	  (set-ball-y-velocity!
	   ball
	   (- (* tang-velocity
		 sin-theta)
	      (* normal-velocity
		 cos-theta)))
	  (set-ball-collision-time!
	   ball
	   collision-time)))))
  (newline)
  (display "Ball ")
  (display (ball-number ball))
  (display " collides with bumper ")
  (display (bumper-number bumper))
  (display " at time ")
  (display (ball-collision-time ball))
  (newline)
  (display "   Ball ")
  (display (ball-number ball))
  (display " has a new velocity of ")
  (display (ball-x-velocity ball))
  (display ",")
  (display (ball-y-velocity ball))
  (display " starting at ")
  (display (ball-collision-x-position ball))
  (display ",")
  (display (ball-collision-y-position ball))

  (recalculate-collisions ball global-event-queue))


;;RECALCULATE-COLLISIONS removes all old collisions for the given ball from
;;all other balls' event queues and calcultes new collisions for these balls
;;and places them on the event queues.  Also, updates the global event queue if
;;the recalculation of the collision effects the earliest collision for any
;;other balls.
;;BALL = The ball whose collisions are being recalculated
;;GLOBAL-EVENT-QUEUE = The global queue of earliest events for each ball
(define (recalculate-collisions ball global-event-queue)
  (clear-queue (ball-event-queue	;Clear the queue of events for this 
		ball))			;ball as they have all changed
  (let ((event-queue			;Calculate all ball collision events
	 (ball-event-queue ball)))	;with balls of lower number
    (let ((ball-vector
	   (ball-ball-vector ball)))
      (do ((i (-1+ (ball-number ball))
	      (-1+ i)))
	  ((negative? i))
	(let ((ball2-queue-record
	       (vector-ref
		ball-vector
		i)))
	  (set-event-queue-record-collision-time!
	   ball2-queue-record
	   (ball-ball-collision-time
	    ball
	    (event-queue-record-object
	     ball2-queue-record)))
	  (queue-insert
	   event-queue
	   ball2-queue-record))))
    (let ((bumper-vector		;Calculate all bumper collision events
	   (ball-bumper-vector ball)))
      (do ((i (-1+ (vector-length
		    bumper-vector))
	      (-1+ i)))
	  ((negative? i))
	(let ((bumper-queue-record
	       (vector-ref
		bumper-vector
		i)))
	  (set-event-queue-record-collision-time!
	   bumper-queue-record
	   (ball-bumper-collision-time
	    ball
	    (event-queue-record-object
	     bumper-queue-record)))
	  (queue-insert
	   event-queue
	   bumper-queue-record))))


    (let ((global-queue-record		;Get the global event queue record 
	   (ball-global-event-queue-record ;for this ball
	    ball)))
      (set-event-queue-record-collision-time! ;Set the new earliest event time
       global-queue-record		;for this ball
       (if (empty-queue? event-queue)
	   '()
	   (event-queue-record-collision-time
	    (queue-smallest event-queue))))
      (queue-insert			;Enqueue on the global event queue
       global-event-queue		;the earliest event between this ball
       global-queue-record)))		;and any ball of lower number or any
					;bumper
  (for-each				;For each ball on the ball list:
   (lambda (ball2)
     (let ((ball2-event-queue
	    (ball-event-queue ball2)))
       (let ((alter-global-event-queue?	;Set flag to update global event queue 
	      (and			;if the earliest event for ball2 was
	       (not (empty-queue?	;with the deflected ball
		     ball2-event-queue))
	       (eq? ball
		    (event-queue-record-object
		     (queue-smallest
		      ball2-event-queue)))))
	     (ball-event-queue-record	;Get the queue record for the deflected
	      (vector-ref		;ball for this ball
	       (ball-ball-vector
		ball2)
	       (ball-number ball))))
	 (queue-remove			;Remove the queue record for the 
	  ball-event-queue-record)	;deflected ball
	 (set-event-queue-record-collision-time! ;Recalculate the collision 
	  ball-event-queue-record	;time for this ball and the deflected
	  (ball-ball-collision-time	;ball
	   ball
	   ball2))
	 (queue-insert			;Enqueue the new collision event
	  ball2-event-queue
	  ball-event-queue-record)
	 (if (or alter-global-event-queue? ;If the earliest collision event for
		 (eq? ball		;this ball has changed:
		      (event-queue-record-object
		       (queue-smallest
			ball2-event-queue))))
	     (let ((queue-record	;Remove the old event from the global
		    (ball-global-event-queue-record ;event queue and replace it
		     ball2)))		;with the new event
	       (set-event-queue-record-collision-time! 
		queue-record
		(event-queue-record-collision-time
		 (queue-smallest
		  ball2-event-queue)))
	       (queue-remove
		queue-record)
	       (queue-insert
		global-event-queue
		queue-record))))))
   (ball-ball-list ball)))
	   

;;SIMULATE performs the billiard ball simulation for the given ball list and
;;bumper list until the specified time.  
;;BALL-LIST = A list of balls
;;BUMPER-LIST = A list of bumpers
;;END-TIME = The time at which the simulation is to terminate
(define (simulate ball-list bumper-list end-time)
  (let ((num-of-balls			;Cache the number of balls and bumpers
	 (length ball-list))
	(num-of-bumpers
	 (length bumper-list))
	(global-event-queue		;Build the global event queue
	 (make-sorted-queue
	  collision-time-<?)))
    (let ((complete-ball-vector		;Build a vector for the balls
	   (make-vector
	    num-of-balls)))
      (let loop ((ball-num 0)		;For each ball:
		 (ball-list ball-list))
	(if (not (null? ball-list))
	    (let ((ball (car ball-list)))
	      (set-ball-number!		;Store the ball's number
	       ball
	       ball-num)
	      (vector-set!		;Place it in the ball vector
	       complete-ball-vector
	       ball-num
	       ball)
	      (set-ball-ball-list!	;Save the list of balls with ball
	       ball			;numbers greater than the current ball
	       (cdr ball-list))
	      (display-ball-state
	       ball)
	      (loop
	       (1+ ball-num)
	       (cdr ball-list)))))
      (let loop ((bumper-num 0)		;For each bumper:
		 (bumper-list
		  bumper-list))
	(if (not (null? bumper-list))
	    (sequence
	      (set-bumper-number!	;Store the bumper's number
	       (car bumper-list)
	       bumper-num)
	      (display-bumper-state
	       (car bumper-list))
	      (loop
	       (1+ bumper-num)
	       (cdr bumper-list)))))

      (do ((ball-num 0 (1+ ball-num)))	;For each ball:
	  ((= ball-num num-of-balls))
	(let* ((ball (vector-ref	;Cache a reference to the ball
		      complete-ball-vector
		      ball-num))
	       (ball-vector		;Build a vector for the queue records 
		(make-vector		;of balls with smaller numbers than 
		 ball-num))		;this ball
	       (bumper-vector		;Build a vector for the queue records
		(make-vector		;of bumpers
		 num-of-bumpers))
	       (event-queue		;Build an event queue for this ball
		(ball-event-queue
		 ball)))
	  (set-ball-ball-vector!	;Install the vector of ball queue 
	   ball				;records
	   ball-vector)
	  (do ((i 0 (1+ i)))		;For each ball of smaller number than 
		  ((= i ball-num))	;the current ball:
		(let* ((ball2		;Cache the ball
			(vector-ref
			 complete-ball-vector
			 i))
		       (queue-record	;Create a queue record for this ball
			(make-event-queue-record ;based on the collision time 
			 '()		;of the two balls
			 '()
			 ball2
			 (ball-ball-collision-time
			  ball
			  ball2))))
		  (vector-set!		;Install the queue record in the ball
		   ball-vector		;vector for this ball
		   i
		   queue-record)
		  (queue-insert		;Insert the queue record into the event
		   event-queue		;queue for this ball
		   queue-record)))

	  (set-ball-bumper-vector!	;Install the vector of bumper queue
	   ball				;records
	   bumper-vector)
	  (let loop ((bumper-num 0)
		     (bumper-list
		      bumper-list))
	    (if (not (null? bumper-list))
		(let* ((bumper		;Cache the bumper
			(car
			 bumper-list))
		       (queue-record	;Create a queue record for this bumper
			(make-event-queue-record ;based on the collision time 
			 '()		;of the current ball and this bumper
			 '()
			 bumper
			 (ball-bumper-collision-time
			  ball
			  bumper))))
		  (vector-set!		;Install the queue record in the bumper
		   bumper-vector	;vector for this ball
		   bumper-num
		   queue-record)
		  (queue-insert		;Insert the queue record into the event
		   event-queue		;queue for this ball
		   queue-record)
		  (loop
		   (1+ bumper-num)
		   (cdr bumper-list)))))
	  (let ((queue-record		;Build a global event queue record for
		 (make-event-queue-record ;the earliest event on this ball's 
		  '()			;event queue
		  '()
		  ball
		  (if (empty-queue?
		       event-queue)
		      '()
		      (event-queue-record-collision-time
		       (queue-smallest
			event-queue))))))
	    (set-ball-global-event-queue-record! ;Store this queue record in 
	     ball			;the frame for this ball
	     queue-record)
	    (queue-insert		;Insert this queue record in the global
	     global-event-queue		;event queue
	     queue-record)))))
    (actually-simulate			;Now that all of the data structures
     global-event-queue			;have been built, actually start the 
     end-time)))			;simulation
	      

;;DISPLAY-BALL-STATE displays the ball number, mass, radius, position, and
;;velocity of the given ball
;;BALL = The ball whose state is to be displayed
(define (display-ball-state ball)
  (newline)
  (display "Ball ")
  (display (ball-number ball))
  (display " has mass ")
  (display (ball-mass ball))
  (display " and radius ")
  (display (ball-radius ball))
  (newline)
  (display "   Its position at time ")
  (display (ball-collision-time ball))
  (display " was ")
  (display (ball-collision-x-position ball))
  (display ",")
  (display (ball-collision-y-position ball))
  (display " and its velocity is ")
  (display (ball-x-velocity ball))
  (display ",")
  (display (ball-y-velocity ball)))

;;DISPLAY-BUMPER-STATE displays the bumper number and position of the given
;;bumper 
;;BUMPER = The bumper whose state is to be displayed
(define (display-bumper-state bumper)
  (newline)
  (display "Bumper ")
  (display (bumper-number bumper))
  (display " extends from ")
  (display (bumper-x1 bumper))
  (display ",")
  (display (bumper-y1 bumper))
  (display " to ")
  (display (bumper-x2 bumper))
  (display ",")
  (display (bumper-y2 bumper)))


;;ACTUALLY-SIMULATE performs the actual billiard ball simulation
;;GLOBAL-EVENT-QUEUE = The global queue of earliest events for each ball.
;;                     Contains a single event for each ball which is the
;;                     earliest collision it would have with a ball of a
;;                     smaller number or a bumper, if no other collisions took
;;                     place first.
;;END-TIME = The time at which the simulation should be terminated
(define (actually-simulate global-event-queue end-time)
  (letrec ((loop			
	    (lambda ()
	      (let* ((record		;Get the globally earliest event and
		      (queue-smallest	;its time
		       global-event-queue))
		     (collision-time
		      (event-queue-record-collision-time
		       record)))
		(if (not		;If this event happens before the
		     (time-<?		;simulation termination time:
		      end-time
		      collision-time))
		    (let* ((ball	;Get the ball involved in the event,
			    (event-queue-record-object
			     record))
			   (ball-queue	;the queue of events for that ball,
			    (ball-event-queue
			     ball))
			   (other-object ;and the first object with which the 
			    (event-queue-record-object ;ball interacts
			     (queue-smallest
			      ball-queue))))
		      ((simulation-object-collision-procedure ;Process this
			other-object)	;globally earliest collision
		       ball
		       other-object
		       collision-time
		       global-event-queue)
		      (loop)))))))	;Process the next interaction
    (loop)))


(require 'cscheme)
(set! autoload-notify? #f)

        (simulate
         (list (make-ball 2 1 9 5 -1 -1)
               (make-ball 4 2 2 5 1 -1))
         (list (make-bumper 0 0 0 10)
               (make-bumper 0 0 10 0)
               (make-bumper 0 10 10 10)
               (make-bumper 10 0 10 10))
         100)

(newline)
