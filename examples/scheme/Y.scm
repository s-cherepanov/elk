; Date: 15 Nov 88 23:03:24 GMT
; From: uoregon!markv@beaver.cs.washington.edu  (Mark VandeWettering)
; Organization: University of Oregon, Computer Science, Eugene OR
; Subject: The Paradoxical Combinator -- Y (LONG)
; 
; Alternatively entitled:
; 	"Y?  Why Not?" :-)
; 
; The discussion that has been going on in regards to the Y combinator as
; the basic operation in implementing recursive functions are interesting.
; The practical tests that people have made have shown that the Y
; combinator is orders of magnitude slower for implementing recursion than
; directly compiling it.
; 
; This is true for Scheme.  I hold that for an interesting set of
; languages, (lazy languages) that this result will not necessarily hold.
; 
; The problem with Y isn't its complexity, it is the fact that it is an
; inherently lazy operation.  Any implementation in Scheme is clouded by
; the fact that Scheme is an applicative order evaluator, while Y prefers
; to be evaluated in normal order.
; 
; 	
 (define Y
   (lambda (g)	
     ((lambda (h) (g (lambda (x) ((h h) x))))
      (lambda (h) (g (lambda (x) ((h h) x)))))))
; 
 (define fact
   (lambda (f)
     (lambda (n)
       (if (= n 1)
 	  1
 	(* n (f (- n 1)))))))
; 
; 
; Evaluating (Y fact) 2 results in the following operations in
; Scheme:
; 
; The argument is (trivially) evaluated, and returns two.
; (Y fact) must be evaluated.  What is it?  Y and fact each evaluate
; to closures.  When applied, Y binds g to fact, and executes the
; body.
; 
; The body is an application of a closure to another closure.  The
; operator binds h to the operand, and executes its body which....
; 
; Evaluates (g (lambda (x) ((h h) x))).  The operand is a closure,
; which gets built and then returns.  g evaluates to fact.  We
; substitute the closure (lambda (x) ((h h) x)) in for the function
; f in the definition of fact, giving...
; 
; (lambda (n)
;   (if (= n 1) 
;       1
;     (* n ((lambda (x) ((h h) x)) (- n 1)))))
; 
; Which we return as the value of (Y fact).  When we apply this to 2, we get
; 
; (* 2 ((lambda (x) ((h h) x)) 1))
; 
; We then have to evaluate
; ((lambda (x) ((h h) x)) 1)
; 
; or 
; ((h h) 1)
; 
; But remembering that h was (lambda (h) (g (lambda (x) ((h h) x)))), 
; we have 
; 
; (((lambda (h) (g (lambda (x) ((h h) x))))
;   (lambda (h) (g (lambda (x) ((h h) x)))))
;  1) ....
; 
; So, we rebind h to be the right stuff, and evaluate the body, which is
; 
; ((g (lambda (x) ((h h) x))) 1)
; 
; Which by the definition of g (still == fact) is just 1.
; 
; (* 2 1) = 2.
; 
; ########################################################################
; 
; Summary:  If you didn't follow this, performing this evaluation
; was cumbersome at best.  As far as compiler or interpreter is
; concerned, the high cost of evaluating this function is related
; to two different aspects:
; 
; It is necessary to create "suspended" values.  These suspended
; values are represented as closures, which are in general heap
; allocated and expensive.
; 
; For every level of recursion, new closures are created (h gets
; rebound above).  While this could probably be optimized out by a
; smart compiler, it does seem like the representation of suspended
; evaluation by lambdas is inefficient.
; 
; 	   
; ########################################################################
; 
; You can try to figure out how all this works.  It is complicated, I
; believe I understand it.  The point in the derivation above is that in
; Scheme, to understand how the implementation of Y works, you have to
; fall back on the evaluation mechanism of Scheme.  Suspended values must
; be represented as closures.  It is the creation of these closures that
; cause the Scheme implementation to be slow.
; 
; If one wishes to abandon Scheme (or at least applicative order
; evaluators of Scheme) one can typically do much better.  My thesis work
; is in graph reduction, and trying to understand better the issues having
; to do with implementation.
; 
; In graph reduction, all data items (evaluated and unevaluated) have the
; same representation: as graphs in the heap.  We choose to evaluate using
; an outermost, leftmost strategy.  This allows the natural definition of
; (Y h) = (h (Y h)) to be used.  An application node of the form:
; 
; 			    @
; 			   / \
; 			  /   \
; 			 Y     h
; 
; can be constructed in the obvious way:
;                             @
; 			   / \
; 			  /   \
; 			 h     @
; 			      /	\
; 			     /   \
; 			    Y     h
; 
; costing one heap allocation per level of recursion, which is
; certainly cheaper than the multiple allocations of scheme
; closures above.  More efficiently, we might choose to implement
; it using a "knot tying" version:
; 
; 
; 			      /\
;                              / 	\
; 			    @	 |
; 			   / \ 	/
; 			  /   \/
; 			 h
; 
; Which also works quite well.  Y has been eliminated, and will
; cause no more reductions.  
; 
; The basic idea is somehow that recursion in functional languages
; is analogous to cycles in the graph in a graph reduction engine.
; Therefore, the Y combinator is a specific "textual" indicator of
; the graph.
; 
; The G-machine (excellently described in Peyton Jones' book "The
; Implementation of Functional Programming Languages") also
; described the Y combinator as being efficient.  He chose letrecs
; as being a primitive in the extended lambda calculus.  His
; methodology behind compiling these recursive definitions was
; basically to compile fixed code which directly built these cyclic
; structures, rather than having them built at runtime.
; 
; I think (and my thesis work is evolving into this kind of
; argument) that Y is overlooked for trivial reasons.  Partial
; evaluation and smarter code generation could make an SK based
; compiler generate code which is equal in quality to that produced
; by supercombinator based compilation.
; 
; 
; This is too long already, ciao for now.
; 
; Mark VandeWettering

(print ((Y fact) 10))
