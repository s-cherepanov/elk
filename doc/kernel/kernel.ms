.so ../util/tmac.scheme
.Ul
.ds R "R\*(^4RS
.TL
Elk \*- The Extension Language Kit
.sp .5
Scheme Reference
.AU
Oliver Laumann
.
.Ch "Introduction"
.
.PP
This reference manual lists the primitive procedures, special forms,
and other facilities implemented by the Scheme interpreter included
in Elk.
This \f2kernel\fP functionality can be augmented by applications
using Elk as their extension language implementation or by
reusable Elk extensions (such as the UNIX or X11 extensions included
in the distribution).
The predefined Elk extensions and the C/C++ programmer's interface
to Elk are described in separate documents.
.PP
Only the procedures and special forms that are not defined by the
official Scheme language specification ``\*R'' (William Clinger and
Jonathan Rees (editors), \f2Revised\*(^4 Report on the Algorithmic
Language Scheme\fP, 1991) are described in detail.
The language features that are part of the official language are only
mentioned without a description or examples.
.
.Ch "Lambda Expressions, Procedures"
.
.Sy lambda formals body
.LP
See \*R.
.
.Pr procedure-lambda procedure
.LP
Returns a copy of the \f2lambda\fP expression which has been
evaluated to create the given procedure.
.br
Example:
.Ss
(define (square x) (* x x))
(procedure-lambda square)        ==>  (lambda (x) (* x x))
.Se
.
.Pr procedure? obj
.LP
See \*R.
.
.Pr primitive? obj
.LP
Returns #t if \f2obj\fP is a primitive procedure, #f otherwise.
.
.Pr compound? obj
.LP
Returns #t if \f2obj\fP is a compound procedure (a procedure that
has been created by evaluating a lambda expression), #f otherwise.
.
.Ch "Local Bindings"
.
.[[
.Sy let bindings body
.Sy let* bindings body
.Sy letrec bindings body
.]]
.LP
See \*R.
.
.Ch "Fluid Binding"
.
.Sy fluid-let bindings body
.LP
\f2bindings\fP is of the form ((\f2variable\*1\fP \f2init1\fP) ...).
The \f2init\fPs are temporarily assigned to the \f2variable\fPs
and the \f2body\fP is executed.
The variables must be bound in an enclosing scope.
When the body is exited normally or by invoking a control point,
the old values of the variables are restored.
In the latter case, when the control returns back to the body
of the fluid-let by invocation of a control point created within
the body, the bindings are changed again to the values they had
when the body exited.
.br
Examples:
.Ss
((lambda (x)
  (+ x (fluid-let ((x 3)) x))) 1)     ==>  4
.Se
.Ss
(fluid-let ((print-length 2))
  (write '(a b c d)))                 ==>  '(a b ...)
.Se
.Ss
(define (errset thunk)
  (call-with-current-continuation
    (lambda (catch)
      (fluid-let
          ((error-handler
            (lambda msg (catch #f))))
        (list (thunk))))))
.sp
(errset (lambda () (+ 1 2)))          ==>  (3)
(errset (lambda () (/ 1 0)))          ==>  #f
.Se
.
.Ch "Definitions"
.
.[[
.Sy define variable expression
.Sy define (variable formals) body
.Sy define (variable . formal) body
.]]
.LP
See \*R.
.br
Returns a symbol, the identifier that has been bound.
Definitions may appear anywhere within a local body (e.\|g.\& a lambda
body or a \f2let\fP).
If the \f2expression\fP is omitted, \f2void\fP (the non-printing
object) is used.
.br
Examples:
.Ss
(define nil #f)
.Se
.Ss
(define ((f x) y) (cons x y))
(define (g x) ((f x) 5))
(g 'a)                           ==>  (a . 5)
.Se
.
.Ch "Assignment"
.
.Sy set! variable expression
.LP
See \*R.
.br
Returns the previous value of \f2variable\fP.
.br
Examples:
.Ss
(define-macro (swap x y)
  `(set! ,x (set! ,y ,x)))
.Se
.
.Ch "Procedure Application"
.
.Sy operator operand\*1 ...
.LP
See \*R.
\f2operator\fP can be a macro (see below).
.
.Pr apply arg\*1 ... args
.LP
See \*R.
.
.Ch "Quotation, Quasiquotation"
.
.[[
.Sy quote datum
.br
.ie \n(.U \f3'\fP\f2datum\fP
.el .tl ,\f3'\fP\f2datum\fP,,\f3syntax\fP,
.br
.ie \n(.U \f2constant\fP
.el .tl ,\f2constant\fP,,\f3syntax\fP
.]]
.Id constant
.LP
See \*R.
.
.[[
.Sy quasiquote expression
.Sy unquote expression
.Sy unquote-splicing expression
.]]
.LP
See \*R.
.
.Ch "Sequencing"
.
.Sy begin expression\*1 expression\*2 ...
.LP
See \*R.
.
.Sy begin1 expression\*1 expression\*2 ...
.LP
Identical to \f2begin\fP, except that the result of the first
\f2expression\fP is returned.
.
.Ch "Conditionals"
.
.[[
.Sy if test consequent alternate
.Sy if test consequent
.]]
.LP
See \*R.
.br
In the first form, \f2alternate\fP can be a sequence of expressions
(implicit \f2begin\fP).
.
.Sy case key clause\*1 clause\*2 ...
.LP
See \*R.
.br
Each \f2clause\fP not beginning with \f2else\fP can be of the form
.DS
((\f2datum\*1\fP ...) \f2expression\*1\fP \f2expression\*2\fP ...)
.DE
or
.DS
(\f2datum\fP \f2expression\*1\fP \f2expression\*2\fP ...)
.DE
In the latter case, the \f2key\fP is matched against the \f2datum\fP.
.
.Sy cond clause\*1 clause\*2 ...
.LP
See \*R.
.
.[[
.Sy and test\*1 ...
.Sy or test\*1 ...
.]]
.LP
See \*R.
.
.Ch "Booleans"
.
.Pr not obj
.LP
See \*R.
.
.Pr boolean? obj
.LP
See \*R.
.
.Ch "Iteration"
.
.Sy let variable bindings body
.LP
``Named \f2let\fP''.
See \*R.
.
.[[
.Pr map procedure list\*1 list\*2 ...
.Pr for-each procedure list\*1 list\*2 ...
.]]
.LP
See \*R.
\f2for-each\fP returns the empty list.
.
.Sy do initializations test body
.LP
See \*R.
.
.Ch "Continuations"
.
.Pr call-with-current-continuation procedure
.LP
See \*R.
.
.Pr control-point? obj
.LP
Returns #t if \f2obj\fP is a control point (a continuation),
#f otherwise.
.
.Pr dynamic-wind thunk thunk thunk
.LP
\f2dynamic-wind\fP is a generalization of the
.Ix unwind-protect
\f2unwind-protect\fP facility provided by many Lisp systems.
.br
All three arguments are procedures of no arguments.
In the normal case, all three thunks are applied in order.
The first thunk is also applied when the body (the second thunk)
is entered by the application of a control point created within
the body (by means of
.Ix call-with-current-continuation
\f2call-with-current-continuation\fP).
Similarly, the third thunk is also applied whenever the body is
exited by invocation of a control point created outside the body.
.br
Examples:
.Ss
(define-macro (unwind-protect body . unwind-forms)
  `(dynamic-wind
    (lambda () #f)
    (lambda () ,body)
    (lambda () ,@unwind-forms)))
.Se
.Ss
(let ((f (open-input-file "foo")))
  (dynamic-wind
    (lambda () #f)
    (lambda () \f2do something with\fP f)
    (lambda () (close-input-port f))))
.Se
.
.Ch "Delayed Evaluation"
.
.[[
.Sy delay expression
.Pr force promise
.]]
.LP
See \*R.
.
.Pr promise? obj
.LP
Returns #t if \f2obj\fP is a promise, an object returned by the
application of \f2delay\fP.
Otherwise #f is returned.
.
.Ch "Equivalence Predicates"
.
.[[
.Pr eq? obj\*1 obj\*2
.Pr eqv? obj\*1 obj\*2
.Pr equal? obj\*1 obj\*2
.]]
.LP
See \*R.
.
.Ch "Pairs and Lists"
.
.Pr cons obj\*1 obj\*2
.LP
See \*R.
.
.[[
.Pr car pair
.Pr cdr pair
.]]
.LP
See \*R.
.
.Pr cxr pair pattern
.LP
\f2pattern\fP is either a symbol or a string consisting of a combination
of the characters `a' and `d'.
It encodes a sequence of \f2car\fP and \f2cdr\fP operations;
each `a' denotes the application of \f2car\fP, and each `d' denotes
the application of \f2cdr\fP.
For example, \f2(cxr p "ada")\fP is equivalent to \f2(cadar p)\fP.
.
.Pr caar pair
.br
   ...
.br
.Pr cddddr pair
.LP
See \*R.
.
.[[
.Pr set-car! pair obj
.Pr set-cdr! pair obj
.]]
.LP
See \*R.
.br
Both procedures return \f2obj\fP.
.
.Pr make-list k obj
.LP
Returns a list of length \f2k\fP initialized with \f2obj\fP.
.br
Examples:
.Ss
(make-list 0 'a)                 ==>  ()
(make-list 2 (make-list 2 1))    ==>  ((1 1) (1 1))
.Se
.
.Pr list obj ...
.LP
See \*R.
.
.Pr length list
.LP
See \*R.
.
.Pr list-ref list k
.LP
See \*R.
.
.Pr list-tail list k
.LP
See \*R.
.
.Pr last-pair list
.LP
See \*R.
.
.Pr append list ...
.LP
See \*R.
.
.Pr append! list ...
.LP
Like \f2append\fP, except that the original
arguments are modified (destructive \f2append\fP).
The cdr of each argument is changed to point to the next argument.
.br
Examples:
.Ss
(define x '(a b))
(append x '(c d))                ==>  (a b c d)
x                                ==>  (a b)
(append! x '(c d))               ==>  (a b c d)
x                                ==>  (a b c d)
.Se
.
.Pr reverse list
.LP
See \*R.
.
.Pr reverse! list
.LP
Destructive \f2reverse\fP.
.
.[[
.Pr memq obj list
.Pr memv obj list
.Pr member obj list
.]]
.LP
See \*R.
.
.[[
.Pr assq obj alist
.Pr assv obj alist
.Pr assoc obj alist
.]]
.LP
See \*R.
.
.[[
.Pr null? obj
.Pr pair? obj
.]]
.LP
See \*R.
.
.Pr list? obj
.LP
See \*R.
.
.Ch "Numbers"
.
.[[
.Pr = z\*1 z\*2 ...
.Pr < z\*1 z\*2 ...
.Pr > z\*1 z\*2 ...
.Pr <= z\*1 z\*2 ...
.Pr >= z\*1 z\*2 ...
.]]
.LP
See \*R.
.
.[[
.Pr 1+ z
.Pr -1+ z
.]]
.LP
Returns \f2z\fP plus 1 or \f2z\fP minus 1, respectively.
.
.Pr 1- z
.LP
A synonym for \f2-1+\fP (for backwards compatibility).
.
.[[
.Pr + z\*1 ...
.Pr * z\*1 ...
.]]
.LP
See \*R.
.
.[[
.Pr - z\*1 z\*2 ...
.Pr / z\*1 z\*2 ...
.]]
.LP
See \*R.
.
.[[
.Pr zero? z
.Pr positive? z
.Pr negative? z
.Pr odd? z
.Pr even? z
.Pr exact? z
.Pr inexact? z
.]]
.LP
See \*R.
.
.Pr abs z
.LP
See \*R.
.
.[[
.Pr quotient n\*1 n\*2
.Pr remainder n\*1 n\*2
.Pr modulo n\*1 n\*2
.]]
.LP
See \*R.
.
.[[
.Pr gcd n\*1 ...
.Pr lcm n\*1 ...
.]]
.LP
See \*R.
.
.[[
.Pr floor x
.Pr ceiling x
.Pr truncate x
.Pr round x
.]]
.LP
See \*R.
.
.Pr sqrt z
.LP
See \*R.
.
.Pr expt z\*1 z\*2
.LP
See \*R.
.
.[[
.Pr exp z
.Pr log z
.Pr sin z
.Pr cos z
.Pr tan z
.Pr asin z
.Pr acos z
.Pr atan z
.Pr atan y x
.]]
.LP
See \*R.
.
.[[
.Pr min x\*1 x\*2 ...
.Pr max x\*1 x\*2 ...
.]]
.LP
See \*R.
.
.Pr random
.LP
Returns an integer pseudo-random number in the range from 0 to
.ie \n(.U 2^31-1.
.el 2\v'-.3m'\s-131\s0\v'.3m'-1.
.
.Pr srandom n
.LP
Sets the random number generator to the starting point \f2n\fP.
\f2srandom\fP returns \f2n\fP.
.
.[[
.Pr number? obj
.Pr complex? obj
.Pr real? obj
.Pr rational? obj
.Pr integer? obj
.]]
.LP
See \*R.
.
.[[
.Pr exact\(mi>inexact z
.Pr inexact\(mi>exact z
.]]
.LP
See \*R.
.
.[[
.Pr number\(mi>string number
.Pr number\(mi>string number radix
.]]
.LP
See \*R.
.
.[[
.Pr string\(mi>number string
.Pr string\(mi>number string radix
.]]
.LP
See \*R.
.
.Ch "Characters"
.
.[[
.Pr char\(mi>integer char
.Pr integer\(mi>char n
.]]
.LP
See \*R.
.
.[[
.Pr char-upper-case? char
.Pr char-lower-case? char
.]]
.LP
See \*R.
.
.[[
.Pr char-alphabetic? char
.Pr char-numeric? char
.Pr char-whitespace? char
.]]
.LP
See \*R.
.
.[[
.Pr char-upcase char
.Pr char-downcase char
.]]
.LP
See \*R.
.
.[[
.Pr char=? char\*1 char\*2
.Pr char<? char\*1 char\*2
.Pr char>? char\*1 char\*2
.Pr char<=? char\*1 char\*2
.Pr char>=? char\*1 char\*2
.]]
.LP
See \*R.
.
.[[
.Pr char-ci=? char\*1 char\*2
.Pr char-ci<? char\*1 char\*2
.Pr char-ci>? char\*1 char\*2
.Pr char-ci<=? char\*1 char\*2
.Pr char-ci>=? char\*1 char\*2
.]]
.LP
See \*R.
.
.Pr char? obj
.LP
See \*R.
.
.Ch "Strings"
.
.Pr string char ...
.LP
Returns a string containing the specified characters.
.br
Examples:
.Ss
(string)                         ==>  ""
(string #\ea #\espace #\eb)         ==>  "a b"
.Se
.
.Pr string? obj
.LP
See \*R.
.
.Pr make-string k char
.LP
See \*R.
.
.Pr string-length string
.LP
See \*R.
.
.Pr string-ref string k
.LP
See \*R.
.
.Pr string-set! string k char
.LP
See \*R.
.br
Returns the previous value of element \f2k\fP of the given string.
.
.Pr substring string start end
.LP
See \*R.
.
.Pr string-copy string
.LP
See \*R.
.
.Pr string-append string ...
.LP
See \*R.
.
.[[
.Pr list\(mi>string chars
.Pr string\(mi>list string
.]]
.LP
See \*R.
.
.Pr string-fill! string char
.LP
See \*R.
.br
Returns \f2string\fP.
.
.Pr substring-fill! string start end char
.LP
Stores \f2char\fP in every element of \f2string\fP from \f2start\fP
(inclusive) to \f2end\fP (exclusive).
Returns \f2string\fP.
.
.[[
.Pr string=? string\*1 string\*2
.Pr string<? string\*1 string\*2
.Pr string>? string\*1 string\*2
.Pr string<=? string\*1 string\*2
.Pr string>=? string\*1 string\*2
.]]
.LP
See \*R.
.
.[[
.Pr string-ci=? string\*1 string\*2
.Pr string-ci<? string\*1 string\*2
.Pr string-ci>? string\*1 string\*2
.Pr string-ci<=? string\*1 string\*2
.Pr string-ci>=? string\*1 string\*2
.]]
.LP
See \*R.
.
.[[
.Pr substring? string\*1 string\*2
.Pr substring-ci? string\*1 string\*2
.]]
.LP
If \f2string\*1\fP is a substring of \f2string\*2\fP, these
procedures return the starting position of the first occurrence of the
substring within \f2string\*2\fP.
Otherwise #f is returned.
\f2substring-ci?\fP is the case insensitive version of \f2substring?\fP.
.br
Examples:
.Ss
(define s "Hello world")
(substring? "foo" x)             ==>  #f
(substring? "hello" x)           ==>  #f
(substring-ci? "hello" x)        ==>  0
(substring? "!" x)               ==>  11
.Se
.
.Ch "Vectors"
.
.Pr vector? obj
.LP
See \*R.
.
.[[
.Pr make-vector k
.Pr make-vector k fill
.]]
.LP
See \*R.
.
.Pr vector obj ...
.LP
See \*R.
.
.Pr vector-length vector
.LP
See \*R.
.
.Pr vector-ref vector k
.LP
See \*R.
.
.Pr vector-set! vector k obj
.LP
See \*R.
.br
Returns the previous value of element \f2k\fP of the vector.
.
.[[
.Pr vector\(mi>list vector
.Pr list\(mi>vector list
.]]
.LP
See \*R.
.
.Pr vector-fill! vector fill
.LP
See \*R.
.br
Returns \f2vector\fP.
.
.Pr vector-copy vector
.LP
Returns a copy of \f2vector\fP.
.
.Ch "Symbols"
.
.[[
.Pr string\(mi>symbol string
.Pr symbol\(mi>string symbol
.]]
.LP
See \*R.
.
.[[
.Pr put symbol key value
.Pr put symbol key
.]]
.LP
Associates \f2value\fP with \f2key\fP in the
.Ix "property list"
property list of the given symbol.
\f2key\fP must be a symbol.
Returns \f2key\fP.
.br
If \f2value\fP is omitted, the property is removed from the symbol's
property list.
.
.Pr get symbol key
.LP
Returns the value associated with \f2key\fP in the
.Ix "property list"
property list of \f2symbol\fP.
\f2key\fP must be a symbol.
If no value is associated with \f2key\fP in the symbol's property
list, #f is returned.
.br
Examples:
.Ss
(put 'norway 'capital "Oslo")
(put 'norway 'continent "Europe")
(get 'norway 'capital)           ==>  "Oslo"
.Se
.
.Pr symbol-plist symbol
.LP
Returns a copy of the
.Ix "property list"
property list of \f2symbol\fP as an \f2alist\fP.
.br
Examples:
.Ss
(put 'norway 'capital "Oslo")
(put 'norway 'continent "Europe")
(symbol-plist 'norway)
   ==>  ((capital . "Oslo") (continent . "Europe"))
(symbol-plist 'foo)              ==>  ()
.Se
.
.Pr symbol? obj
.LP
See \*R.
.
.Pr oblist
.LP
Returns a list of lists containing all currently interned symbols.
Each sublist represents a bucket of the interpreters internal
hash array.
.br
Examples:
.Ss
(define (apropos what)
  (let ((ret ()))
    (do ((tail (oblist) (cdr tail))) ((null? tail))
      (do ((l (car tail) (cdr l))) ((null? l))
        (if (substring? what (symbol->string (car l)))
            (set! ret (cons (car l) ret)))))
    ret))
.Se
.Ss
(apropos "let")       ==>  (let* let letrec fluid-let)
(apropos "make")      ==>  (make-list make-vector make-string)
(apropos "foo")       ==>  ()
.Se
.
.Ch "Environments"
.
.Pr the-environment
.LP
Returns the current environment.
.
.Pr global-environment
.LP
Returns the global environment (the ``root'' environment in which
all predefined procedures are bound).
.
.Pr environment\(mi>list environment
.LP
Returns a list representing the specified environment.
The list is a list of \f2frames\fP, each frame is a list of bindings
(an \f2alist\fP).
The car of the list represents the most recently established environment.
The list returned by \f2environment\(mi>list\fP can contain cycles.
.br
Examples:
.Ss
(let ((x 1) (y 2))
  (car (environment->list
    (the-environment))))            ==>  ((y . 2) (x . 1))
.Se
.Ss
((lambda (foo)
   (caar (environment->list
     (the-environment)))) "abc")    ==>  (foo . "abc")
.Se
.Ss
(eq?
  (car (last-pair (environment->list
    (the-environment))))
  (car (environment->list
    (global-environment))))         ==>  #t
.Se
.
.[[
.Pr procedure-environment procedure
.Pr promise-environment promise
.Pr control-point-environment control-point
.]]
.LP
Returns the environment in which the the body of the \f2procedure\fP
is evaluated, the environment in which a value for the \f2promise\fP
is computed when \f2force\fP is applied to it, or the environment in
which the \f2control-point\fP has been created, respectively.
.
.Pr environment? obj
.LP
Returns #t if \f2obj\fP is an environment, #f otherwise.
.
.Ch "Ports and Files"
.LP
Generally, a
.Ix "file name"
file name can either be a string or a symbol.
If a symbol is given, it is converted into a string by applying
.Ix symbol\(mi>string
\f2symbol\(mi>string\fP.
A
.Ix tilde
tilde at the beginning of a file name is expanded according
to the rules employed by the C-Shell (see \f2csh\fP(1)).
.LP
Elk adds a third type of ports, \f2input-output\fP (bidirectional) ports.
Both \f2input-port?\fP and \f2output-port?\fP return #t when applied
to an input-output port, and both input primitives and output
primitives may be applied to input-output ports.
An input-output port (in fact, \f2any\fP port) may be closed with any of
the primitives \f2close-input-port\fP and \f2close-output-port\fP.
.LP
The only way to create an input-output-port is by means of the procedure
.Ix open-input-output-file
\f2open-input-output-file\fP.
Extensions may provide additional means to create bidirectional ports.
.
.[[
.Pr call-with-input-file file procedure
.Pr call-with-output-file file procedure
.]]
.LP
See \*R.
.
.[[
.Pr input-port? obj
.Pr output-port? obj
.]]
.LP
See \*R.
.
.[[
.Pr current-input-port
.Pr current-output-port
.]]
.LP
See \*R.
.
.[[
.Pr with-input-from-file file thunk
.Pr with-output-to-file file thunk
.]]
.LP
See \*R.
.br
\f2file\fP can be a string as well as a symbol.
.
.[[
.Pr open-input-file file
.Pr open-output-file file
.Pr open-input-output-file file
.]]
.LP
See \*R.
.br
\f2file\fP can be a string as well as a symbol.
\f2open-input-output-file\fP opens the file for reading and writing
and returns an input-output port; the file must exist and is not
truncated.
.
.[[
.Pr close-input-port port
.Pr close-output-port port
.]]
.LP
See \*R.
.br
Calls to \f2close-input-port\fP and \f2close-output-port\fP are ignored
when applied to string ports or to ports connected with the standard
input or standard output of the process.
.
.[[
.Pr clear-output-port
.Pr clear-output-port output-port
.]]
.LP
If the argument is omitted, it defaults to the current output port.
.br
In case of ``buffered'' output, this procedure is used to discard
all characters that have been
output to the port but have not yet been sent to the file associated
with the port.
.
.[[
.Pr flush-output-port
.Pr flush-output-port output-port
.]]
.LP
If the argument is omitted, it defaults to the current output port.
.br
In case of ``buffered'' output, this procedure is used to force
all characters that have been output to the port to be printed
immediately.
This may be necessary to force output that is not terminated with a newline 
to appear on the terminal.
An output port is flushed automatically when it is closed.
.
.[[
.Pr clear-input-port
.Pr clear-input-port input-port
.]]
.LP
If the argument is omitted, it defaults to the current input port.
.br
In case of ``buffered'' input,
this procedure discards all characters that have already been read
from the file associated with the port but have not been processed
using \f2read\fP or similar procedures.
.
.Pr port-file-name port
.LP
Returns the name of the file associated with \f2port\fP if it is
a file port, #f otherwise.
.
.Pr port-line-number
.LP
Returns the current line number of a file input port or string input
port, i.\|e.\& the number of newline characters that have been read from
this port plus one.
``Unreading'' a newline character decrements the line number, but it
never drops below one.
The result of applying \f2port-line-number\fP to an output port is
undefined.
.
.Pr tilde-expand file
.LP
If \f2file\fP starts with a tilde, performs tilde expansion as
described above and returns the result of the expansion
(a string); returns \f2file\fP otherwise.
\f2file\fP is a string or a symbol.
.
.Pr file-exists? file
.LP
Returns #t if \f2file\fP is accessible, #f otherwise.
\f2file\fP is a string or a symbol; tilde expansion is not performed.
.
.Ch "Input"
.
.[[
.Pr read
.Pr read input-port
.]]
.LP
See \*R.
.
.[[
.Pr read-char
.Pr read-char input-port
.]]
.LP
See \*R.
.
.[[
.Pr read-string
.Pr read-string input-port
.]]
.LP
If the argument is omitted, it defaults to the current input port.
.br
Returns the rest of the current input line as a string (not
including the terminating newline).
.
.[[
.Pr unread-char char
.Pr unread-char char input-port
.]]
.LP
If the second argument is omitted, it defaults to the current input port.
.br
Pushes \f2char\fP back on the stream of input characters.
It is \f2not\fP an error for \f2char\fP not to be the last character
read from the port.
It is undefined whether more than one character can be pushed back without
an intermittent read operation, and whether a character can be pushed
back before something has been read from the port.
The procedure returns \f2char\fP.
.
.[[
.Pr peek-char
.Pr peek-char input-port
.]]
.LP
See \*R.
.LP
\f2peek-char\fP uses \f2unread-char\fP to push back the character.
.
.Pr eof-object? obj
.LP
See \*R.
.
.Pr char-ready? input-port
.LP
See \*R.
.LP
\f2char-ready\fP cannot be implemented correctly based on C FILE pointers.
In the current version, \f2char-ready\fP can return #f although
a call to \f2read-char\fP would not block.
.
.Ch "Output"
.
.[[
.Va print-length
.Va print-depth
.]]
.LP
These variables are defined in the global environment.
They control the maximum length and maximum depth, respectively, of
a list or vector that is printed.
If one of the variables is not bound to an integer, or if its value
exceeds a certain, large maximum value (which is at least 2^20),
a default value is taken.
The default value for \f2print-length\fP is 1000, and the default
value for \f2print-depth\fP is 20.
Negative values of \f2print-length\fP and \f2print-depth\fP are
treated as ``unlimited'', i.\|e.\& output is not truncated.
.
.[[
.Pr write obj
.Pr write obj output-port
.]]
.LP
See \*R.
.
.[[
.Pr display obj
.Pr display obj output-port
.]]
.LP
See \*R.
.
.[[
.Pr write-char char
.Pr write-char char output-port
.]]
.LP
See \*R.
.
.[[
.Pr newline
.Pr newline output-port
.]]
.LP
See \*R.
.
.[[
.Pr print obj
.Pr print obj output-port
.]]
.LP
If the second argument is omitted, it defaults to the current output port.
.br
Prints \f2obj\fP using \f2write\fP and then prints a newline.
\f2print\fP returns \f2void\fP.
.
.Pr format destination format-string obj ...
.LP
Prints the third and the following arguments according to the
specifications in the string \f2format-string\fP.
Characters from the format string are copied to the output.
When a tilde is encountered in the format string, the tilde and
the immediately following character are replaced in the output
as follows:
.IP "~s"
is replaced by the printed representation of the next \f2obj\fP
in the sense of \f2write\fP.
.IP "~a"
is replaced by the printed representation of the next \f2obj\fP
in the sense of \f2display\fP.
.IP "~~"
is replaced by a single tilde.
.IP "~%"
is replaced by a newline.
.LP
An error is signaled if fewer \f2obj\fPs are provided than
required by the given format string.
If the format string ends in a tilde, the tilde is ignored.
.LP
If \f2destination\fP is #t, the output is sent to the current
output port; if #f is given, the output is returned as a string;
otherwise, \f2destination\fP must be an output or input-output port.
.br
Examples:
.Ss
(format #f "Hello world!")       ==>  "Hello world"
(format #f "~s world!" "Hello")  ==>  "\e"Hello\e" world"
(format #f "~a world!" "Hello")  ==>  "Hello world"
(format #f "Hello~a")            ==>  "Hello!"
.Se
.Ss
(define (flat-size s)
  (fluid-let ((print-length 1000) (print-depth 100))
    (string-length (format #f "~a" s))))
.Se
.Ss
(flat-size 1.5)                  ==>  3
(flat-size '(a b c))             ==>  7
.Se
.
.Ch "String Ports"
.LP
.Ix "string ports"
String ports are similar to file ports, except that characters are
appended to a string instead of being sent to a file, or taken
from a string instead of being read from a file.
It is not necessary to close string ports.
When an string input port has reached the end of the input string,
successive read operations return end-of-file.
.
.Pr open-input-string string
.LP
Returns a new string input port initialized with \f2string\fP.
.br
Examples:
.Ss
(define p (open-input-string "Hello world!"))
(read-char p)                    ==>  #\eH
(read p)                         ==>  ello
(read p)                         ==>  world!
(read p)                         ==>  \f2end of file\fP
.Se
.Ss
(define p (open-input-string "(cons 'a 'b)"))
(eval (read p))                  ==>  (a . b)
.Se
.
.Pr open-output-string
.LP
Returns a new string output port.
.
.Pr get-output-string string-output-port
.LP
Returns the string currently associated with the specified string
output port.
As a side-effect, the string is reset to zero length.
.br
Examples:
.Ss
(define p (open-output-string))
(display '(a b c) p)
(get-output-string p)            ==>  "(a b c)"
(get-output-string p)            ==>  ""
.Se
.Ss
(define (flat-size s)
  (let ((p (open-output-string)))
    (display s p)
    (string-length (get-output-string p))))
.Se
.
.Ch "Loading"
.
.[[
.Pr load file
.Pr load file environment
.]]
.LP
Loads a source file or one or more object files.
If the file contains source code, the expressions in the file are
read and evaluated.
If a file contains
.Ix "object code"
object code, the contents of the file is linked
together with the running interpreter and with additional libraries
that are specified by the variable
.Ix load-libraries
\f2load-libraries\fP (see below).
Names of
.Ix "object files"
object files must have the
.Ix suffix
suffix ``.o''.
\f2load\fP returns \f2void\fP.
.LP
\f2file\fP must be either a string or a symbol or a list of strings
or symbols.
If it is a list, all elements of the list must be the names of object files.
In this case, all object files are linked by a single run of the
.Ix linker
linker.
.br
If an optional \f2environment\fP is specified, the contents of the file
is evaluated in this environment instead of the current environment.
.LP
Loading of object files is not supported on some platforms.
On the platforms where it is supported, the feature
.Ix feature
.Ix elk:load-object
\f2elk:load-object\fP is provided by the interpreter on startup (see
``Features'' below).
.br
Example:
.Ss
(fluid-let ((load-noisily? #t))
  (load 'test.scm))
.Se
.
.Va load-path
.LP
This variable is defined in the global environment.
It is bound to a list of directories in which files to be loaded are
searched for.
Each element of the list (a string or a symbol) is used in turn as
a prefix for the file name passed to \f2load\fP until opening succeeds.
Elements of \f2load-path\fP that are not of type string or symbol are ignored.
.LP
If the value of \f2load-path\fP is not a list of at least one valid
component, or if the name of the file to be loaded starts with ``/''
or with ``~'', it is opened directly.
.LP
The initial value of \f2load-path\fP is a list of the three elements
``.'' (i.\|e.\& the current directory), ``$scheme_dir'', and ``$lib_dir'';
$scheme_dir and $lib_dir are the directories into which
the runtime Scheme files and object files are installed (typically
``/usr/elk/runtime/scm'' and ``/usr/elk/runtime/obj''; defined in
the installation's
.Ix "site file"
site file).
.
.Va load-noisily?
.LP
This variable is defined in the global environment.
When a file is loaded and the value of \f2load-noisily?\fP is true,
the result of the evaluation of each expression is printed.
The initial value of \f2load-noisily?\fP is #f.
.
.Va load-libraries
.LP
This variable is defined in the global environment.
If \f2load-libraries\fP is bound to a string, its value specifies
additional load libraries to be linked together with an
.Ix "object file"
object file that is loaded into the interpreter (see \f2load\fP above).
Its initial value is ``\-lc''.
.
.Pr autoload symbol file
.LP
Binds \f2symbol\fP in the current environment (as with \f2define\fP).
When \f2symbol\fP is evaluated the first time, \f2file\fP is loaded.
The definitions loaded from the file must provide a definition
for \f2symbol\fP different from \f2autoload\fP, otherwise an error
is signaled.
.LP
\f2file\fP must be either a string or a symbol or a list of strings
or symbols, in which case all elements of the list must be the names of
.Ix "object file"
object files (see \f2load\fP above).
.
.Va autoload-notify?
.LP
This variable is defined in the global environment.
If the value of \f2autoload-notify?\fP is true, a message is printed
whenever evaluation of a symbol triggers autoloading of a file.
\f2autoload-notify?\fP is bound to #t initially.
.
.Ch "Macros"
.
.Sy macro formals body
.LP
This special form creates a macro.
The syntax is identical to the syntax of \f2lambda\fP expressions.
When a macro is called, the actual arguments are bound to
the formal arguments of the \f2macro\fP expression \f2in the current
environment\fP (they are \f2not\fP evaluated), then the \f2body\fP is evaluated.
The result of this evaluation is considered the \f2macro expansion\fP
and is evaluated in place of the macro call.
.
.[[
.Sy define-macro (variable formals) body
.Sy define-macro (variable . formal) body
.]]
.LP
Like \f2define\fP, except that \f2macro\fP is used instead of \f2lambda\fP.
.br
Examples:
.Ss
(define-macro (++ x) `(set! ,x (1+ ,x)))
(define foo 5)
foo           ==>  5
(++ foo)
foo           ==>  6
.Se
.Ss
(define-macro (while test . body)
  `(let loop ()
     (cond (,test ,@body (loop)))))
.Se
.
.Pr macro? obj
.LP
Returns #t if \f2obj\fP is a macro, #f otherwise.
.
.Pr macro-body macro
.LP
Returns a copy of the \f2macro\fP expression which has been evaluated to
created the given macro (similar to
.Ix procedure-lambda
\f2procedure-lambda\fP).
.br
Examples:
.Ss
(define-macro (++ x) `(set! ,x (1+ ,x)))
.sp
(macro-body ++)
  ==>  (macro (x) (quasiquote (set! (unquote x) (1+ (unquote x)))))
.Se
.
.Pr macro-expand list
.LP
If the expression \f2list\fP is a macro call, the macro call
is expanded.
.br
Examples:
.Ss
(define-macro (++ x) `(set! ,x (1+ ,x)))
.sp
(macro-expand '(++ foo))         ==>  (set! foo (1+ foo))
.Se
.sp
The following function can be used to expand \f2all\fP macro calls
in an expression, i.\|e.\& not only at the outermost level:
.Ss
(define (expand form)
  (if (or (not (pair? form)) (null? form))
      form
      (let ((head (expand (car form)))
            (args (expand (cdr form)))
            (result))
        (if (and (symbol? head) (bound? head))
            (begin
              (set! result (macro-expand (cons head args)))
              (if (not (equal? result form))
                  (expand result)
                  result))
              (cons head args)))))
.Se
.
.Ch "Error and Exception Handling"
.
.Va error-handler
.LP
This variable is defined in the global environment.
When an error occurs or when the procedure \f2error\fP is invoked
and the variable \f2error-handler\fP is bound to a compound procedure
(the \f2error handler\fP), the interpreter invokes this procedure.
The error handler is called with an object (either the first argument
that has been passed to \f2error\fP or a symbol identifying the
primitive procedure that has caused the error), and an error
message consisting of a format string
and a list of objects suitable to be passed to
.Ix format
\f2format\fP.
.LP
Typically, a user-defined error handler prints the error message and then
calls a control point that has been created outside the error handler.
If the error handler terminates normally or if \f2error-handler\fP
is not bound to a procedure, the error message is printed in a
default way, and then a
.Ix reset
\f2reset\fP is performed.
.
.Va interrupt-handler
.LP
This variable is defined in the global environment.
When an interrupt occurs (typically as a result of typing the
interrupt character on the keyboard), and the variable
\f2interrupt-handler\fP is bound to a procedure (the \f2interrupt
handler\fP), this procedure is called with no arguments.
If \f2interrupt-handler\fP is not bound to a procedure or if
the procedure terminates normally, a message is printed, and a
.Ix reset
\f2reset\fP is performed.
.br
Examples:
.Ss
(set! interrupt-handler
  (lambda ()
    (newline)
    (backtrace)
    (reset)))
.Se
.
.[[
.Pr disable-interrupts
.Pr enable-interrupts
.]]
.LP
\f2disable-interrupts\fP causes
.Ix signals
signals to be blocked from delivery to
the interpreter; \f2enable-interrupts\fP enables delivery of signals.
These functions control delivery of keyboard-generated interrupt signals
(see \f2interrupt-handler\fP above) as well as additional signals used by
extensions (such as the alarm signal).
The interpreter automatically blocks delivery of signals during critical 
operations, such as garbage collection.
Signals are enabled on startup after initialization has completed.
.LP
A call to \f2enable-interrupts\fP immediately delivers signals that have
been generated while signals were disabled, but blocked signals are not
queued.
On platforms that support neither POSIX-style nor BSD-style reliable
signals, \f2disable-interrupts\fP causes signals to be ignored (as
opposed to blocking them until the next call to \f2enable-interrupts\fP).
.LP
Calls to \f2disable-interrupts\fP and \f2enable-interrupts\fP can be
nested.
The functions maintain a count indicating the number of calls
to \f2enable-interrupts\fP that it takes to return from a nested
\f2disable-interrupts\fP invocation to the topmost level (i.\|e.\& to
actually enable delivery of signals again).
Both functions return this nesting level as an integer.
.LP
Example: the following loop ensures that delivery of signals is enabled,
regardless of the current nesting depth of \f2disable-interrupts\fP calls:
.Ss
(let loop ((intr-level (enable-interrupts)))
     (if (positive? intr-level)
         (loop (enable-interrupts))))

.Se
.LP
.Ix dynamic-wind
\f2dynamic-wind\fP can be used to write a macro
.Ix with-interrupts-disabled
\f2with-interrupts-disabled\fP to protect a
.Ix "critical section"
critical section of code from being interrupted by a signal:
.Ss
(define-macro (with-interrupts-disabled . body)
  `(dynamic-wind
     (lambda () (disable-interrupts))
     (lambda () ,@body)
     (lambda () (enable-interrupts))))
.Se
.
.Pr error obj string obj ...
.LP
Signals an error.
The arguments of \f2error\fP are passed to the
.Ix error-handler
\f2error-handler\fP.
.br
Examples:
.Ss
(define (foo sym)
  (if (not (symbol? sym))
      (error 'foo "argument not a symbol: ~s" sym))
  ...
.Se
.
.[[
.Va top-level-control-point
.Pr reset
.]]
.LP
\f2reset\fP performs a reset by calling the control point to which the
variable \f2top-level-control-point\fP is bound in the global environment.
The control point is called with the argument #t.
If \f2top-level-control-point\fP is not bound to a control point,
or does not exist at all,
an error message is printed and the interpreter is terminated.
.br
Examples:
.Ss
(if (call-with-current-continuation
      (lambda (x)
        (fluid-let ((top-level-control-point x))
\0\0\0\0\0\0\0\0\0\0\f2do\0something\fP
          #f)))
    (print "Got a reset!"))
.Se
.
.[[
.Pr exit
.Pr exit n
.]]
.LP
Terminates the interpreter.
The optional argument \f2n\fP indicates the
.Ix "exit code"
exit code; it defaults to zero.
.
.Ch "Garbage Collection"
.LP
The interpreter supports two
.Ix "garbage collector"
garbage collectors: the
.Ix "garbage collector, stop-and-copy"
stop-and-copy garbage collector that was part of older versions of Elk, and a
.Ix "garbage collector, generational"
.Ix "garbage collector, incremental"
generational, incremental garbage collector.
.LP
If generational garbage collection has been selected, Scheme objects
surviving two garbage collections will not be touched again until
there is only a certain amount of memory left on the heap, triggering
a full garbage collection.
Particularly in applications with large amounts of Scheme code or
constant data, partial garbage collections run much faster than full
garbage collections.
In contrast to the stop-and-copy garbage collector, the generational
garbage collector is not limited to a pre-allocated amount of
heap; it will expand the heap in steps of 1 MB if the free space left
after a full garbage collection falls below a certain amount.
.LP
Another feature of the generational garbage collector (available on
some platforms only) is the ability to do incremental garbage
collection.
Starting a garbage collection does not interrupt the application until
the garbage collector is done.
Instead, the collector returns control to the application almost
immediately.
To synchronize between the garbage collection and the running
application, the code makes use of the \f2mprotect\fP system call.
.
.Pr garbage-collect-status strategy mode
.LP
\f2garbage-collect-status\fP is used to select a garbage collector
and an optional, garbage collector specific mode of operation, and
to query the currently enabled garbage collector and mode.
.LP
\f2strategy\fP is a symbol identifying a garbage collector.
Permitted values are \f2stop-and-copy\fP and \f2generational\fP
(future version of Elk may support additional garbage collectors).
The optional \f2mode\fP argument may be specified if the \f2strategy\fP
argument is equal to \f2generational\fP.
Currently, only the symbol \f2incremental\fP may be used for the
\f2mode\fP argument to enable incremental garbage collection.
.LP
The current version of the interpreter does not support dynamic
switching between the stop-and-copy and the generational, incremental
garbage collector at runtime.
Instead, a garbage collector has to be selected at compile time
(by setting the \f2generational_gc\fP variable in the installation's
.Ix "site file"
site file to either \f2yes\fP or \f2no\fP).
Thus, \f2garbage-collect-status\fP can currently only be used to query
the garbage collector and, if the generational, incremental garbage
collector has been selected, to enable and disable incremental
garbage collection (this restriction may be removed in future versions).
.LP
\f2garbage-collect-status\fP returns a list of symbols indicating
the currently enabled garbage collector and mode.
This list resembles the arguments to \f2garbage-collect-status\fP,
i.\|e.\& the first element of the list one of the symbols
\f2stop-and-copy\fP and \f2generational\fP, and an optional, second
symbol (\f2incremental\fP) may be present if the first symbol is
equal to \f2generational\fP.
.LP
If \f2garbage-collect-status\fP is invoked with no arguments, or if
the desired garbage collector or mode of operation cannot be enabled
(either because selection of a strategy at runtime is not supported,
of because the mode of operation cannot be supported), the primitive
just returns the currently active strategy and mode.
.
.Pr collect
.LP
Causes a garbage collection.
Even if incremental garbage collection has been enabled, \f2collect\fP
always performs a full garbage collection run.
.
.Pr collect-incremental
.LP
This primitive is only present if the generational
garbage collector has been selected.
An error is signaled if \f2collect-incremental\fP is
invoked and incremental garbage collection has not been enabled,
i.\|e.\& if a call to \f2garbage-collect-status\fP would return
the list \f2(generational)\fP.
.LP
\f2collect-incremental\fP starts an incremental garbage
collection and then returns immediately.
If an incremental garbage collection is already in progress,
\f2collect-incremental\fP triggers one incremental
garbage collection step, i.\|e.\& scans a few more pages of memory,
and then returns immediately.
The primitive returns true if the incremental garbage collection
has been finished, false otherwise.
.LP
If incremental garbage collection is disabled by a call to
\f2(garbage-collect-status 'generational)\fP while an incremental
garbage collection run is in progress, the next call to
\f2collect-incremental\fP finishes the incremental garbage collection run 
and returns #t; further calls to \f2collect-incremental\fP will
signal an error.
.
.Va garbage-collect-notify?
.LP
This variable is defined in the global environment.
If the value of \f2garbage-collect-notify?\fP is true,
a message indicating the amount of free memory on the heap and
the size of the heap are displayed whenever a stop-and-copy garbage
collection is performed.
If the generational, incremental garbage collector has been enabled,
the amount of reclaimed memory is displayed on each garbage
collection run, and a message is displayed each time the heap
is expanded by the garbage collector.
\f2garbage-collect-notify?\fP is bound to #t initially.
.
.Ch "Features"
.
.Pr feature? symbol
.LP
Returns #t if \f2symbol\fP is a feature, i.\|e.\& \f2provide\fP has
been called to indicate that the feature \f2symbol\fP is present;
#f otherwise.
.
.Pr provide symbol
.LP
Indicates that the feature \f2symbol\fP is present.
Returns \f2void\fP.
.
.[[
.Pr require symbol
.Pr require symbol file
.Pr require symbol file environment
.]]
.LP
If the feature \f2symbol\fP is not present (i.\|e.
(feature? \f2symbol\fP) evaluates to #f), \f2file\fP is loaded.
A message is displayed prior to loading the file if the value of the
global variable \f2autoload-notify?\fP is true.
If the feature is still not present after the file has been loaded,
an error is signaled.
.LP
If the \f2file\fP argument is omitted, it defaults to \f2symbol\fP;
if \f2symbol\fP does not end in a
.Ix suffix
suffix (i.\|e.\& does not contain a dot character), the suffix \f2.scm\fP
is appended to obtain a file name.
.LP
If an \f2environment\fP argument is supplied, the file is loaded
into given environment.
If the \f2environment\fP argument is omitted, it defaults to the
current environment.
.LP
\f2file\fP must be either a string or a symbol or a list of strings
or symbols, in which case all elements of the list must be the names
of object files (see \f2load\fP above).
.
.Pr features
.LP
Returns the currently provided features a list of symbols.
.
.Ch "Miscellaneous"
.
.Pr dump file
.LP
Writes a snapshot of the running interpreter to \f2file\fP and
returns #f.
When \f2file\fP is executed, execution of the interpreter resumes such
that the call to \f2dump\fP returns #t
(i.e., \f2dump\fP actually returns twice).
\f2dump\fP closes all ports except the current input and current
output port.
.LP
This primitive is not supported on platforms that are not capable
of creating an executable file from the memory image of the
running process.
If \f2dump\fP is available, the
.Ix feature
.Ix elk:dump
feature \f2elk:dump\fP is provided by the interpreter on startup
(see ``Features'' above).
.
.[[
.Pr eval list
.Pr eval list environment
.]]
.LP
Evaluates the expression \f2list\fP in the specified environment.
If \f2environment\fP is omitted, the expression is evaluated
in the current environment.
.br
Examples:
.Ss
(let ((car 1))
  (eval 'car (global-environment)))     ==>  \f2primitive\fP \f1car\fP
.Se
.Ss
(define x 1)
(define env
  (let ((x 2)) (the-environment)))
(eval 'x)       ==>  1
(eval 'x env)   ==>  2
.Se
.
.Pr bound? symbol
.LP
Returns #t if \f2symbol\fP is bound in the current environment,
#f otherwise.
.
.Pr type obj
.LP
Returns a symbol indicating the type of \f2obj\fP.
.br
Examples:
.Ss
(type 13782343423544)              ==>  integer
(type 1.5e8)                       ==>  real
(type (lambda (x y) (cons x y)))   ==>  compound
(type #\ea)                         ==>  character
(type '(a b c))                    ==>  pair
(type '())                         ==>  null
(type (read
  (open-input-string "")))         ==>  end-of-file
.Se
.
.Pr void? obj
.LP
Returns true if \f2obj\fP is the non-printing object, false otherwise.
.
.Pr command-line-args
.LP
Returns the command line arguments of the interpreter's invocation,
a list of strings.
.
.Ch "\*R Language Features not Implemented by Elk"
.IP \(bu
Rational and complex numbers are not supported.
.IP \(bu
Radix prefixes (#b, #o, #d, and #x) for real numbers are currently
not implemented.
.IP \(bu
The exponent markers \f2s\fP, \f2f\fP, \f2d\fP, and \f2l\fP are not
implemented; the character \f2#\fP is not permitted in place of digits
in numerical constants.
.IP \(bu
\f2char-ready\fP
.Ix char-ready
is not implemented correctly (see above).
.IP \(bu
\f2transcript-on\fP and \f2transcript-off\fP are not implemented.
.LP
