.if \n(.g .do char \[a:] \[:a]
.\" \(a: is a lower case a with diaeresis (a umlaut)
.\" \(-> is an arrow pointing to the right
.\" \(mu is a multiplication sign (cross)
.\" \*- is an em dash
.
.\" .fp 1 PR
.\" .fp 2 PI
.\" .fp 3 PB
.nr VS 20
.fp 5 C
.fp 6 CO
.ie \n(.U .ds ^4 ^4
.el .ds ^4 \u\s-1\|4\s0\d
.		\" Second level section
.de P
.NH 2
..
.              \" Scheme code start
.de Ss
.KS
.ta 8.5c
.nr sF \\n(.f
.ft 5
.ps -2
.\".vs -2
.vs 13
.nf
.in 1c
.if !\n(.U .sp .3c
..
.              \" Scheme code end
.de Se
.in
.fi
.vs
.ps
.ft \\n(sF
.KE
.if !\n(.U .sp .5
..
.              \" Newline in Scheme code
.de Sl
.sp .52
..
.nr lS 0 1
.              \" Listing start
.de Ls
.br
.KF
.sp .5
.LP
\l'\\n(.lu_'
..
.              \" Listing caption:  .Lc caption
.de Lc
.sp .2
.ce 999
\f3\s-1Listing \\n+(lS:\fP  \c
\\$1\s0
.if !\\$2 \s-1\&\\$2\s0
.ce 0
..
.              \" Listing end:  .Le reference
.de Le
.tm s/@L(\\$1)/\\n(lS/g
.LP
\l'\\n(.lu_'
.sp
.KE
..
.              \" Notes start (at end of listing)
.de Ns
.sp
.ps -2
.vs -2
.in 1c
.ll -1c
..
.              \" Notes end
.de Ne
.ll
.in
.vs
.ps
.sp -.3
..
.ds E Elk
.TL
\*E: The Extension Language Kit
.AU
Oliver Laumann* and Carsten Bormann\v'-.2m'\(dg\v'.2m'
.AI
* Technische Universit\(a:t Berlin, Germany
.br
\(dg Universit\(a:t Bremen, Germany
.AB
In the past, users of an application generally were at the mercy of
its authors when it came to adapting it to their individual needs and
tastes.
Fitting an application with an \f2extension language\fP (or \f2embedded
language\fP) enables users to customize and enhance it without having to
modify its source code.
Recently, variants of Lisp have become increasingly
popular for this purpose, to the point where the abundance of different
dialects has grown into a problem.
Of the two standardized dialects of Lisp, only \f2Scheme\fP is suitably
modest, yet sufficiently general, to serve as an extension language.
.PP
\f2\*E\fP, the \f2Extension Language Kit\fP, is a Scheme implementation
that is intended to be used as a general, reusable extension language
subsystem for integration into existing and future applications.
Applications can define their own Scheme data types and primitives,
providing for a tightly-knit integration of the C/C++ parts of the application
with Scheme code.
Library interfaces, for example to the UNIX operating system
and to various X Window System libraries, show the effectiveness of
this approach.
Several features of \*E such as dynamic loading of object files and
freezing of fully customized applications into executables
(implemented for those UNIX environments where it was feasible)
increase its usability as the backbone of a complex application.
\*E has been used in this way for seven years within a
locally-developed ODA-based multimedia document editor; it has been
used in numerous other projects after it could be made freely
available five years ago.
.AE
.NH
Introduction
.PP
The designers and implementors of a large or complex application can
rarely anticipate all requirements future users will have on the
application.
Typically, users wish to be able to customize the user interfaces
of applications according to their personal tastes or requirements,
or they want to extend the functionality of an application
(either by combining existing functions into new ones or by adding
entirely new capabilities).
This is especially true for applications used routinely, such as text
editors, and for applications with a high degree
of user interaction or with complex graphical user interfaces.
.PP
Certainly any application can be customized by modifying its source
code and recompiling it.
But this approach is often not feasible, as the source code of the
application or the tools needed to recompile it may not be available.
Even if it were feasible, it would be a time-consuming process;
it would be hard to keep up with new releases of the application;
and the coexistence of multiple, similar versions of the same
application would become a general maintenance headache.
.PP
The alternative to this approach is not to ``hard-wire'' the entire
functionality and all external aspects of an application in the source
code at all, but to provide means to customize the application's
behavior later by its users.
.P
Early Customization and Extension Languages
.PP
Many applications support at least simple methods for customization,
such as command line options or configuration files.
More powerful tools for customization are \f2macro languages\fP,
\f2command languages\fP, or \f2scripting languages\fP that are
typically found in text editors and word processors.
Prominent examples of such customization and extension languages
are the macro language of the now legendary TECO editor and,
in UNIX, the macro language of the \f2troff\fP text formatter
[Ossanna 1979] and the configuration language of the \f2sendmail\fP
program.
.PP
Although many of these classic extension languages are quite
powerful (some of them are full-fledged programming languages),
they have a reputation of being ``cryptic'' and hard to understand
and use by untrained users.
The prevailing opinion seems to be that only experts can actually
benefit from these types of extension languages (for example,
people who have mastered the \f2sendmail\fP configuration language
in all details are commonly appointed the status of a ``guru'').
In fact, it can be observed that only very few users of the \f2troff\fP
text formatter (whose macro language is reputed to be particularly
cryptic) are using macro packages written by themselves; many users
give up after some time and fall back on vendor-supplied macro
packages or packages written by a ``troff guru.''
.PP
Experience also indicates that simplified or specialized extension
languages often have more features added and grow until they resemble
a full programming language.
Such ``organically grown'' extension languages are likely to be
contorted designs as they will consist of several levels of extensions
glued on to their initial, more limited design.
.P
High-Level Extension Languages
.PP
Recently application designers have begun to abandon
specialized and cryptic macro-style extension languages in
favor of extension languages that resemble usual high-level
programming languages, mainly languages with Algol/Pascal-style
or Lisp-style syntax and semantics.
Prominent examples of such high-level extension languages are
TPU developed by DEC, the \f2Ness\fP language of the Andrew
Toolkit [Hansen 1990], AutoDesk's CAD extension language (a dialect
of Lisp), and \f2Emacs-Lisp\fP, the extension
language of Richard Stallman's popular GNU Emacs editor
[Stallman 1981, Lewis et al. 1990].
.PP
Emacs was the first wide-spread application to employ an
already existing and widely used high-level programming
language as its extension and customization language.
Emacs-Lisp is a dynamically scoped dialect of Lisp with additional
operations for text-editing.
The approach taken by Emacs has been tremendously successful;
users of Emacs have contributed a wealth of extensions written
in Emacs-Lisp.
.PP
Note that Emacs-Lisp is not a \f2scripting language\fP.
It is tightly interwoven with the application for which it provides
extensibility.
It also is somewhat inaccessible to the casual user,
who is unlikely to have previous experience with Lisp-like languages.
This can be contrasted with languages such as Tcl [Ousterhout 1990] and
REXX [Cowlishaw 1985], whose underlying models are no less complex,
but which are
similar enough to well-known languages such as BASIC to present less of
an obstacle to casual users.
On the other hand, non-trivial extensions benefit from the structuring
functionality inherent in general purpose programming languages such as
Lisp.
.P
\*E as a General, Reusable Extension Language
.PP
Using Lisp or Lisp-style languages as extension languages
seems to enjoy growing popularity; several applications besides
Emacs now use dialects of Lisp as their extension language.
This development has one disadvantage: the number of
incompatible (but similar) extension languages is continually growing.
Users have to learn a new language for each new application,
and application writers keep implementing new extension language
interpreters instead of reusing existing ones.
.PP
These problems can be solved by a general, reusable extension language
implementation that application writers can include into their
applications, an \f2extension language kit\fP.
The main objective of the \f2\*E\fP project was to develop such an
extension language kit and to make it freely available to encourage
use by application writers.
.NH
Overview of the Extension Language Kit
.P
The Evolution of \*E
.PP
We were prompted to develop \*E when a search for a suitable extension
language implementation for ISOTEXT [Bormann et al. 1988, Bormann
1991] was fruitless.
ISOTEXT,
a document processing system with a graphical user interface,
is almost entirely written in C++; its user interface is
based on the X window system [Scheifler et al. 1986, Scheifler et al. 1992]
and the OSF/Motif widget set.
Customizability and extensibility through a full extension language
were basic requirements on the design of ISOTEXT.
.PP
As we consider language design to be the domain of a ``selected few''
and did not want to act as amateurs in this field, we decided to use
an existing programming language as the basis for the extension
language of ISOTEXT.
This decision was also influenced by our desire to develop a
general, reusable extension language implementation that is
not hard-wired into one specific application.
For a number of reasons an interpreted language seemed preferable:
extensions can be added to (or modified in)
a running application without re-linking it;
bugs in extensions can be caught in the interpreter and 
do not crash the application;
interpreted languages usually offer better debugging facilities;
and implementing an interpreter generally is easier
than implementing a compiler.
.PP
From the beginning we favored Lisp or a dialect of Lisp
as the basis for a general extension language.
Most dialects of the Lisp family are ``small'', easy to implement,
general-purpose languages with simple syntax and powerful semantics,
and the suitability of Lisp as an extension language had already been
demonstrated by several applications, among them GNU Emacs.
Early in the project we considered to use Emacs-Lisp,
but it appeared infeasible to isolate the Lisp interpreter
from the rest of Emacs.
In addition, at the time we investigated Emacs-Lisp it was lacking
several desirable language features, such as support for floating point
and arbitrary precision numbers (\f2bignums\fP).
We also considered using MIT Scheme [MIT 1984], but due to the enormous
size of its implementation it would have dominated the size of
the application.
.P
Scheme as an Extension Language
.PP
As other implementations of Lisp or Lisp-like languages available
did not meet our requirements, we
finally decided to write an interpreter for the Lisp dialect \f2Scheme\fP
[Clinger et al. 1991, Dybvig 1987, Springer et al. 1989,
Abelson et al. 1985].
This Scheme interpreter is the main component of the \*E package.
Scheme is a simplified, ``cleaned-up'' dialect of Lisp with
first-class procedures and static scoping rules.
The Scheme language is based on only a few language features and
semantic concepts; it consists of a small core of syntactic
forms, a set of extended forms derived from them, and a number
of standard procedures (\f2primitive\fP procedures) that operate
on a comprehensive set of types of objects (among them numbers, lists, vectors,
symbols, characters, and strings).
In 1990 Scheme became an IEEE standard [IEEE\|Std\|1178-1990]
(the standard document, although only 50 pages long,
includes the formal semantics of the language).
.PP
The standardization effort has increased the acceptance of Scheme;
for instance, the Extension Language Working Group
of the CAD Framework Initiative has recently selected Scheme as the
extension language for future CAD applications [CFI 1991a, CFI 1991b].
Among the established programming languages we consider Scheme the
ideal candidate for a general extension language \*-
it is standardized; its semantics are
well-defined; it has a simple syntax and is easy to implement; and it
is sufficiently small to not dwarf the application it makes extensible.
.P
Extending the Extension Language
.PP
The implementation of an extension language must itself be
extensible.
Extension language code that manipulates objects or state of the
application requires adding application-specific primitive procedures
to the base extension language.
To allow \*E programs to be expressive in the context of a given
application, application writers are encouraged (and expected) to
extend standard Scheme by a rich set of application-specific data types
and Scheme primitives to operate on objects of these types.
In fact, easy extensibility of the language has been the primary
design consideration in the development of \*E (as opposed to
performance or number of language features).
Adding new types and primitives to \*E is an inexpensive operation;
it is not uncommon for an application to define hundreds
of application-specific Scheme primitives.
.\"
.\" implementation must fit well to `host language' (schreibt cabo...)
.PP
All primitive procedures of \*E are implemented as C or C++ functions.
This is true for both built-in primitives (such as \f2car\fP and \f2cdr\fP)
and primitives defined by extensions.
From the Scheme programmers' point of view, primitives and types from the
base set of the language are indistinguishable from application-specific
primitives and types.
Extensions ``register'' new primitives with the interpreter
by supplying the name of the primitive along with a pointer
to the function implementing the primitive and
information about the arguments and calling style.
New types are defined in a similar way.
Registration of new primitives and types usually takes place on startup
of the interpreter or when a compiled extension is loaded
into the running interpreter.
.PP
Another way to use the extension mechanisms of \*E is to provide
interfaces to libraries, such as the C library or the libraries
of the X window system (e.\|g.\& \f2Xlib\fP).
\*E has no facility to directly import ``foreign'' functions
(although one such facility has been contributed as an extension to \*E).
Therefore, a small amount of code acting as ``glue'' between \*E and
the library has to be written to make the contents of a library
available to Scheme programmers.
The main purpose of this interface code is to check the arguments
supplied to the library functions, to convert Scheme objects
into C types, and to convert the results of library functions back
into Scheme objects.
Such \f2library extensions\fP often act as an additional layer
between the application
to be extended and the libraries used by the application; they allow
the application writers to abstract from the details of the
libraries.
Although it is useful to distinguish between \f2library\fP extensions 
and extensions interfacing to \f2applications\fP, there is no
technical difference \*- in both cases a collection of types
and functions is made available to the Scheme world.
.PP
Since many of today's applications need to interact with the X Window
System, library extensions are included with \*E that interface to the
X11 ``Xlib'' (similar in its functionality to ``CLX'' [CLX 1991], but
implemented on top of Xlib), to the X11 toolkit intrinsics (``Xt''),
and to the Athena and OSF/Motif widget sets.
.PP
In addition, the \*E UNIX extension provides Scheme access to most
UNIX system calls and operating system interface C library functions\**.
.FS
The UNIX extension defines procedures for low-level,
file-descriptor-based I/O; creation of pipes; file/record locking;
file and directory system calls; process creation and control; signal
handling; error handling; and obtaining information about date, time,
users, limits, process resources, etc.
.FE
The extension supports a wide range of different UNIX platforms
without restricting its functionality to the lowest common denominator
or the POSIX 1003.1 functions.
To facilitate writing portable Scheme programs, the extension attempts
to hide differences between the types of supported UNIX flavors.
.\"(Two examples are appended: one forks off a process
.\"and communicates with it through pipes; the other one measure the maximum
.\"capacity of a pipe using non-blocking I/O.)
.NH
Using \*E in Applications
.\" .P
.\" Bringing Everything Together
.PP
In contrast to other extension language implementations
(e.\|g.\& Tcl),
\*E does not provide its functionality in the form of a library that is
statically linked into an application to be extended.
Instead, the object modules comprising the application
and all required library extensions are dynamically linked with and
loaded into the running Scheme interpreter.
To accomplish this, the \f2load\fP primitive of \*E has been
extended to load not only files containing Scheme code,
but also object files \*- compiled extensions written
in C or C++.
Dynamic loading enables applications to load less frequently
used modules into the running program only on demand; such an
application is initially smaller than the equivalent statically
linked application (where all modules must be combined into
one large executable file).
.PP
Dynamic loading of object files is often used together with the
\f2dump\fP primitive that creates an executable file from
the running interpreter, similar to \f2unexec\fP of GNU Emacs or
\f2dump\%lisp\fP in some Lisp systems.
The \f2dump\fP primitive of \*E differs from existing, similar
mechanisms in that the newly created executable, when called, starts at
the point where \f2dump\fP was called in the original invocation (as
opposed to the program's \f2main\fP entry point).
Here the return value of \f2dump\fP is ``true'', while in the original
invocation it returns ``false'' \*- not unlike the UNIX \f2fork\fP system
call.
.P
Dynamic Loading and Dump in Cooperation
.PP
To generate a new instance of an application one would typically
invoke the Scheme interpreter, load all object modules and all Scheme
code required initially, perform all initializations that can survive a
\f2dump\fP, and finally dump an image of the running interpreter
containing all the loaded code into a new executable on disk.
The use of \f2dump\fP avoids time-consuming activities such as
loading of object files and other initializations on each startup.
The dumped executable, when started, resumes after the call to
\f2dump\fP; at this point one would perform the remaining,
environment-dependent initializations and finally invoke the
application's ``main program'' (e.\|g.\& enter the X toolkit's event
processing main loop).
Listing @L(dump) shows a (slightly simplified) Scheme program that
generates and starts a new instance of an application.
.Ls
.Ss
;;; Load initially required object files and Scheme files of
;;; application and dump image into executable file.
;;; Dumped file enters application's main loop on startup.
.Sl
(load 'main.o)     ; initial object modules
(load 'edit.o)
(load 'x11.o)      ; (a library extension)
\&...
(load 'ui.scm)     ; initial Scheme files
(load 'custom.scm)
(load 'x11.scm)
\&...
(initialize-application)
.Sl
(if (dump 'a.out)
    (begin                   ; dumped a.out starts execution here
      (initialize-depending-on-environment)
      (main-loop-of-application)
      (exit)))
.Sl
;; Original invocation gets here when dump is finished.  We're done.
.Se
.Lc "Scheme code to generate and start an application"
.Ns
\f2Note:\fP Filenames can be given as symbols (besides the usual string
literals).
A more meaningful name than a.out would probably be chosen in practice.
.Ne
.Le dump
.PP
On systems that do not support dynamic linking and loading of
object files (such as older versions of UNIX System V)
or where \f2dump\fP cannot be implemented,
the interpreter kernel and the application and library extensions
are linked statically and combined into one executable.
.PP
In any event, in an application using \*E, the control initially
rests in the Scheme interpreter.
The interpreter acts as the ``main program'' of the application; it is the
interpreter's \f2main()\fP function which is invoked on startup of
the program.
Therefore the first code to execute in an application is Scheme code;
this Scheme code provides the shell functionality of the application
(hence it is called \f2shell code\fP).
The shell code may perform a few simple tasks, for instance, load a
user-provided initialization file containing customization code for
the application and then enter the application's main loop,
or it may be as complex as in ISOTEXT, where the entire X-based
user interface is written in Scheme.
.P
Making Oneself Known to the Extension Language
.PP
The application, as it is linked with the extension language
interpreter, has full access to all external functions and variables of
the interpreter kernel.
The interpreter, on the other hand, does not have any knowledge of the
contents of dynamically linked and loaded object modules; all it
sees of an object file being loaded is the file's symbol table.
To obtain ``hooks'' into a newly loaded extension, the interpreter
searches the symbol table of each object file being loaded for
functions whose names start with the prefix ``elk_init_'' 
(\f2extension initialization functions\fP) and invokes these functions
as they are encountered.
Likewise, to support extensions written in C++, any C++ static
constructors found in the symbol table are called.
When linked statically with its extensions, the interpreter must scan
its own symbol table on startup to find and invoke the initialization
functions.
(Similar support is available for calling extension finalization functions
and C++ static destructors on termination.)
.PP
Besides initializing private data of the modules being loaded,
these initialization functions register with the interpreter
the Scheme primitives and Scheme data types implemented by the extensions.
To enable extensions to register new primitive procedures and types,
the interpreter kernel exports two functions: \f2Define_Primitive()\fP
to register a new Scheme primitive and \f2Define_Type()\fP to
register a new Scheme data type.
Both functions take pointers to C functions as arguments that implement
the new primitive or the basic access functions of the type (such as
the print function and the equality predicates).
.PP
A simple example for a library extension is presented in Appendix A.
.NH
Notes on the Implementation
.PP
Designing \*E, not as another Scheme implementation, but as an
extension language kit, provided a design space different from
that traditionally available for Lisp implementations.
The necessary deviations from the treaded paths of UNIX programming
uncovered limitations in portability, aggravated by badly tested
corners of standard UNIX facilities.
This section discusses the more interesting examples of such issues.
.P
Implementing Continuations
.PP
Finding a way to efficiently implement Scheme's \f2continuations\fP
called for considerable efforts during the design phase of \*E.
Continuations are a powerful language feature; they support the
definition of arbitrary control structures such as non-local
loop and procedure exits, \f2break\fP and \f2return\fP as in C,
exception handling facilities, explicit backtracking, co-routines,
or multitasking based on \f2engines\fP [Dybvig 1987].
.PP
The primitive procedure
.Ss
\s+1(call-with-current-continuation \f2receiver\fP)\s0
.Se
packages up the current execution state of the program into
an object (the \f2continuation\fP or \f2escape procedure\fP)
and passes this object as an argument to \f2receiver\fP (which is
a procedure of one argument).
Continuations are first-class objects in Scheme; they are
represented as procedures of one argument (not to be confused
with the \f2receiver\fP procedure).
Each time a continuation procedure is called with a value, 
it causes this value to be returned as the result of the
\f2call-with-current-continuation\fP expression which created this
continuation.
If the procedure \f2receiver\fP terminates normally (i.\|e.\& does
not invoke the continuation given to it), the value
returned by \f2call-with-current-continuation\fP is the return
value of \f2receiver\fP.
.PP
As long as the use of a continuation is confined to the runtime
of the \f2receiver\fP procedure, \f2call-with-current-continuation\fP
is similar in its functionality to \f2catch/throw\fP in most
Lisp dialects or \f2setjmp/longjmp\fP in C.
However, continuations, like all procedures in Scheme, have indefinite
extent (unlimited lifetime); they can be stored in variables and
called an arbitrary number of times, even after the \f2receiver\fP and
the enclosing \f2call-with-current-continuation\fP have already
terminated.
Listing @L(call-cc) shows a program fragment where continuations
are used to get back an arbitrary number of times into the middle
of an expression whose computation has already been completed.
While not particularly useful, this example demonstrates that
continuations can be used to build control structures that
cannot be implemented by means of less general language features like
catch/throw or setjmp/longjmp.
.Ls
.Ss
(define my-function
  (lambda (n m)
    (+ n (mark m)))                    ; return n+m
.Sl
(define get-back "uninitialized")
.Sl
(define mark                           ; identity function, but also
  (lambda (value)                      ; assign current continuation
    (call-with-current-continuation    ; to a global variable
      (lambda (continuation)
        (set! get-back continuation)   ; (assign it)
        value))))
.Sl
.Sl
(my-function 10 20)                    ; invoke my-function     \f2prints 30\fP
(get-back 5)                           ; resume with new value  \f2prints 15\fP
(get-back 0)                           ; ...once more           \f2prints 10\fP
.Se
.Lc "Using continuations with unlimited extent"
.Le call-cc
.PP
The different approaches applicable to implementing
continuations are intimately tied to the strategies used for
interpreting the language itself.
Scheme interpreters generally employ a lexical analyzer and parser
\*- the \f2reader\fP \*- to read and parse the Scheme source code and
produce an intermediate representation of the program.
During this phase, symbols are collected in a global hash table
(in Lisp jargon, the symbols are \f2interned\fP), and a tree
structure representing the program's S-expressions is built up
on the heap of the interpreter.
The majority of interpreters compile this intermediate representation
into an abstract machine language (such as \f2byte code\fP).
The evaluator is then implemented as an abstract machine which interprets
the low-level language; this machine \*- usually a simple stack
machine \*- may even be implemented in hardware.
.PP
In an abstract machine implementation, the straightforward approach to
implement \f2call-with-current-continuation\fP is to package up the
contents of the abstract machine's registers (program counter, stack
pointer, etc.) and runtime stack.
Since continuations have indefinite
extent, it would not suffice to just capture its registers (as the C
library function \f2setjmp\fP does for the real machine).
To be able to continue the evaluation of procedures that have
already returned and whose frames are therefore no longer on the stack,
a continuation must also embody the contents of the abstract
machine's stack at the time it is created.
When a continuation is applied, the machine resumes the ``frozen''
computation by restoring the saved registers and stack contents
of the abstract machine.
.PP
Just saving the abstract machine's state would not work in \*E, because
at the time a continuation is created, arbitrary library functions may
be active in addition to Scheme primitives.
For instance, consider the \*E interface to the
``Xt'' toolkit intrinsics of the X window system.
Here, a typical scenario is that some Scheme
procedure invokes the primitive that enters the toolkit's event
dispatching main loop (\f2XtAppMainLoop()\fP). 
When an event arrives (for example, a mouse button press event), 
the toolkit's main loop invokes a callback function, which in turn
calls a user-supplied Scheme procedure to be executed when a
mouse button is pressed.
This Scheme procedure might in turn invoke yet another function
from the ``Xt'' library, and so on.
A similar example would be a \f2qsort\fP or \f2ftw\fP extension to \*E,
where the user-supplied function called by the \f2qsort()\fP or
\f2ftw()\fP C library function would invoke a procedure written
in Scheme.
.PP
The interpreter's thread of execution at any time obviously involves
both Scheme primitives and library functions (such as
\f2XtAppMainLoop()\fP and \f2qsort()\fP in the examples above) in an
arbitrary combination.
Therefore, a continuation must embody not only the execution
state of the active Scheme procedures, but also that of the
currently active library functions (such as local variables
used by the library functions).
In the approach used by \*E, a continuation is created by capturing
the machine's registers \*- like \f2setjmp\fP in C does \*- and the C
runtime stack.
When a continuation is applied later, the registers and the saved
stack contents are copied back.
Actually, we did not follow the usual ``abstract machine''
technique in \*E at all; instead, the Scheme evaluator directly
interprets the intermediate representation produced by the reader.
In a sense, it is the ``real'' machine (the hardware on which \*E
is executed) that plays the role the abstract machine plays in
implementations with byte-code compilation.
.PP
Although the abstract machine technique usually yields faster
execution of Scheme code, the performance of \*E resembles
that of existing interpreters employing this technique,
and the implementation of \*E is simpler than that of comparable
interpreters using byte-code compilation.
While the technique to implement continuations in \*E is not strictly
portable \*- it is based on certain assumptions on the machine's stack
layout and the C compiler and runtime environment \*-
.\"implementations of the small machine-dependent part now exist for
it works on most major machine architectures (with two
exceptions, which are supported using \f2asm\fP statements).
.P
The Implementation of ``dump''
.PP
Continuations provide a natural basis for implementing the
execution-state preserving semantics of the \f2dump\fP primitive.
When called, \f2dump\fP invokes \f2call-with-current-continuation\fP.
The real work is done in the \f2receiver\fP procedure; 
it stores the newly created continuation into a global variable,
sets a global \f2was-dumped\fP flag to indicate that a dump has taken place,
creates an executable file from the image of the running process,
and finally returns ``false''.
The return value of the \f2dump\fP primitive is the return value
of this call to \f2call-with-current-continuation\fP, i.\|e.\&
``false'' if a dump has just been performed.
.PP
When the interpreter \*- either the original program or a dumped
executable \*- is started, it examines the \f2was-dumped\fP flag
as its very first action.
If the flag is set, the running interpreter was started from a 
dumped executable.
In this case the interpreter immediately invokes, with an argument of
``true'', the continuation that was saved away by a call to \f2dump\fP;
this causes that call to \f2dump\fP to finish and return ``true'' to
its caller.
If, on the other hand, the \f2was-dumped\fP flag is not set (i.\|e.\&
the running process was not started from a dumped image), the
interpreter initializes and starts up as usual.
.PP
Before writing an image of the running process to disk, \f2dump\fP
has to close all open Scheme file ports, as open file descriptors would
not survive a \f2dump\fP \*- they would no longer be valid in the
dumped executable.
Generally, this is true for all objects pointing to information
maintained by the UNIX kernel, such as the current directory, the
current signal dispositions, resource limits, or interval timers.
Users and implementors of \*E extensions must be aware of this
particular restriction.
For instance, users of the X11 extensions have to make sure that,
if \f2dump\fP is to be used, connections to X-displays are only
established in the dumped invocation.
.PP
To be able to create an executable from the running process, \f2dump\fP
has to open and read the a.out file from which the running process was
started (actually, if the system linker has been called to dynamically
load object files, the output of the most recent invocation of the
linker is used instead of the original a.out).
The symbol table of the new executable is copied from the a.out file of
the running program; in addition, the a.out header has to be read to
obtain the length of the text segment and the start of the data segment
of the running process.
To do so, \f2dump\fP has to determine the filename of the a.out file from
which the process was started based on the information in \f2argv[0]\fP
and in the PATH environment variable.
This approach is obviously based on several prerequisites: \f2dump\fP
must be able to access its a.out file (\f2argv[0]\fP must carry
meaningful information; the file must be readable) and the running
program's a.out file must not have been stripped.
It would have been advantageous for the implementation of \f2dump\fP
if the entire a.out file were automatically mapped into memory
on startup, like it is done, for instance, in NeXT-OS/Mach.
.PP
\f2dump\fP combines the data segment and the ``bss'' segment of the
running process into the data segment of the new executable.
If \*E had a separate heap for storing constant objects (future
versions may have one),
\f2dump\fP could place this read-only part of the memory into the new
executable's text segment to make it sharable.
When the interpreter's heap is written to disk, \f2dump\fP seeks
over the unused portions of the heap, so that fake blocks (holes) can be
used for these parts of the file.
This results in a considerable conservation of disk space in
the final executable, as at least half of the interpreter's
heap is unused at any time due to the garbage collection
algorithm of \*E.
.PP
Since the a.out formats used in the numerous
versions of UNIX differ vastly, \*E has to include separate
implementations of \f2dump\fP for the currently supported
a.out formats.
Version 2.2 of \*E handles the BSD-style a.out format used
in BSD and ``derived'' UNIX versions (such as SunOS 4.1),
the COFF a.out format (used in older releases of UNIX System V
and in A/UX), Convex SOFF,
Extended COFF of MIPS-based computers (DEC, SGI), and
the ELF a.out format of System V Release 4 and related UNIX
versions (Solaris 2.x, OSF/1).
.P
Dynamic Loading of Object Files
.PP
When loading an object file during runtime, addresses
within this object file must be relocated to their new location
in the program's address space.
To allow extensions to directly reference objects of the interpreter
kernel, such as the heap and the built-in primitives, unresolved
references into the \f2base program\fP must be resolved during
dynamic loading.
Finally, the object file needs to be able to export its entry points
(such as \*E's extension initialization functions) to the base program.
.PP
More than one object file may have to be loaded into one invocation
of \*E.
To manage non-trivial, hierarchically structured sets of extensions,
where a number of high-level extensions require one or more lower-level
extensions to be loaded, it is essential that object files loaded later
can make use of the symbols defined by previously loaded object files.
As this style of dynamic loading allows building complex systems from
small components incrementally, we will use the term \f2incremental
loading\fP.
.PP
With the advent of 4.0\|BSD in 1980 [Joy 1980],
support for incremental
loading was added to the system linker and has since been supported by
most major UNIX variants:
when the \-A option and the name of the base executable are supplied to the
linker, linking is performed in a way that the object file produced by
the linker can be read into the already running executable.
The symbol table of the resulting object file is a combination of the
symbols defined by the base program and the newly defined symbols added
by the linking process, from the object file or from libraries used in
linking.
Only this newly linked code and data is entered into the
resulting object file.
The incremental style of dynamic loading is achieved by saving
the resulting output file each time the linker is invoked and using
this file as the base program for the next incremental loading step,
such that both old and new symbols can be referenced.
.PP
Incremental loading is generally supported by the linkers of UNIX
versions that use the BSD-style a.out format and by those of several
UNIX systems based on more modern a.out formats (e.\|g.\& Ultrix).
It is not supported by any existing release of UNIX System V.
Some newer UNIX versions that have shared libraries and dynamic linking
(such as System V Release 4 or SunOS) offer a library interface to
the dynamic linker.
In some systems this kind of interface is intended to replace the
incremental loading functionality of the system linker.
These dynamic linker interfaces usually come in the form of a library that
exports functions such as \f2dlopen()\fP to map a shared object module or
shared library into the address space of the caller (the base program)
and \f2dlsym()\fP to obtain the address of a function or data item in
the newly attached object module.
.PP
In some implementations, object files attached through \f2dlopen()\fP may
directly reference symbols in the base program; in other implementations
they may not.
In any case, object files cannot directly reference symbols defined
by objects that have been placed into the program by previous calls
to \f2dlopen()\fP (only, if at all, indirectly by calling \f2dlsym()\fP).
Thus, these dynamic linker interfaces are clearly inferior to
incremental loading, as they lack the important capability to
load a set of object files \f2incrementally\fP.
Many vendors who have replaced ``/bin/ld \-A'' by a \f2dlopen\fP-style library
in their UNIX systems, or who intend to do so, do not seem to be
aware of the fact that this change will break applications that
rely on incremental loading.
.PP
For \*E, the consequence of being restricted to dynamic linker
interfaces of that kind is that, except for the simplest applications,
one must pre-link all possible combinations of extensions that are
not completely independent of each other.
In general, given a set of \f2n\fP extensions each of which can be
based on one out of \f2m\fP other extensions, this means having to prepare
and keep around \f2n\|\(mu\|m\fP pre-linked object files; not to
mention the contortions one has to go through when the hierarchy of
extensions has a depth greater than two (not an unlikely scenario in
practice).
If the number of extensions and relations between them is larger than
trivial, or if the extensions are large or require large libraries,
keeping around all pre-linked combinations of object modules will cause
a maintenance problem and may waste a considerable amount of disk space.
.PP
Another, although minor, problem with these dynamic linker interfaces
is that they usually offer only a simple-minded function (such as
\f2dlsym()\fP) to look up the address of a specific symbol of a newly
accessed object module (typically some kind of module initialization
function); but they do not provide a way to scan all newly defined
symbols.
This functionality is insufficient to implement extension
initialization in \*E, where a dynamically loadable extension often is
composed from a number of small modules, each defining its own
initialization function.
Requiring a single, common initialization function name for the entire
object file implies that (often configuration-dependent) ``glue code''
must be added to call all the individual initialization functions,
including the C++ static constructors.
.PP
Version 2.2 of \*E supports dynamic loading in environments with
``ld\|\|\-A'' (such as BSD, SunOS 4, Ultrix, and certain versions of
SGI Irix and HP-UX), in HP-UX 9 (based on \f2shl_load\fP), and in
MACH/NeXT-OS (\f2rld_load\fP).
By generating shared objects on the fly, System V Release 4 and
SunOS 5 (Solaris 2) are also supported, although in a limited and
not yet satisfactory way.
.P
Non-Standard Language Features
.PP
As the current version of the Scheme standard (deliberately) does not
specify several important language issues, such as error handling or
syntactic extensions, we have added a number of non-standard language
features to the Scheme interpreter of \*E to fill some of the holes.
.PP
A proposal for a macro extension has only recently been
added as an addendum to the \f2Revised\*(^4 Report on the
Algorithmic Language Scheme\fP [Clinger et al. 1991] and is still being
discussed controversially within the Scheme community.
To avoid having to wait for a final version of a macro system to
evolve and be included in the Scheme standard, we implemented a
simple-minded macro mechanism in \*E that resembles the macro
facilities offered by various existing Scheme and Lisp systems.
.PP
One area where the Scheme standard does not specify any language
features yet is error and exception handling; the standard merely
states which error situations a conforming implementation is
required to detect and report.
Since it is essential for a non-trivial application to be able to
gracefully handle error situations (such as failures in interactions
with the operating system) and other exceptional conditions, we have
added a simple error and exception handling facility to \*E.
.PP
When an error is detected by the interpreter, a user-supplied
error handling procedure is invoked with arguments identifying the
type and source of the error.
The standard interactive environment (top-level) of \*E provides a
default error handler that prints an error message and then resumes the
main read-eval-print loop by means of a \f2reset\fP primitive.
Most primitives of \*E and the extensions use this error handling
facility to signal an error, as opposed to indicating failure by
a distinctive return value (which would be prone to being ignored).
To by-pass the standard error handler and ``catch'' failure of a
particular primitive, programs may enclose the call to the primitive by
\f2call-with-current-continuation\fP and dynamically bind the error
handler to the continuation (as shown in listing @L(errset)).
.Ls
.Ss
(define (open-input-file-or-not name)
  (call-with-current-continuation
    (lambda (return)                 ; \f6return\fP becomes escape procedure
      (fluid-let ((error-handler     ; rebind \f6error-handler\fP
                    (lambda args (return #f))))
        (open-input-file name)))))
.Se
.Lc "A version of open-input-file that returns the newly opened port \
on success, #f on error"
.Le errset
.PP
\*E provides a similar facility to handle an \f2interrupt\fP exception:
a user-supplied interrupt handler is invoked when a SIGINT signal is sent
to the interpreter (usually by typing the interrupt character on the
keyboard).
Support for other exceptions, such as timer interrupts, may be provided
in future versions.
.PP
Another non-standard primitive that facilitates handling of errors is
\f2dynamic-wind\fP, a generalization of the \f2unwind-protect\fP form
offered by many Lisp dialects.
\f2dynamic-wind\fP is used to implement the \f2fluid-let\fP special
form (to create \f2fluid\fP or dynamic variable bindings).
Both \f2dynamic-wind\fP and \f2fluid-let\fP are also provided by
several other Scheme dialects [MIT 1984, Dybvig 1987].
.PP
The current version of the Scheme standard does not provide any
language features that would make it possible to implement a useful
Scheme debugger (apart from a debugger based on source code
instrumentation).
To compensate for this shortcoming, we have added a few primitives that
aid the implementation of a simple interactive debugger, among them an
\f2eval\fP primitive (although, in theory, \f2eval\fP could be
implemented by writing an expression into a temporary file and then
loading this file).
In addition, \*E, like a few other Scheme dialects, provides lexical
environments as first class (but immutable) objects.
Other non-standard primitives that aid writing debuggers are
\f2procedure-lambda\fP to obtain the lambda expression that evaluated
to a given procedure, and a primitive that returns the list of
currently active procedures together with their actual arguments and
the lexical environments in which the procedure calls took place
(a \f2backtrace\fP).
.\"
.\" provide, require; autoloading
.P
Garbage Collection
.PP
The garbage collector of \*E is based on the \f2stop-and-copy\fP
algorithm (see e.\|g. [Abelson et al. 1985]).
The heap area is divided into two \f2semispaces\fP, only one of which
is active during normal operation.
In a garbage collection, all objects that are still reachable are moved
into the unused semispace; the previously used semispace then remains
unused until the next garbage collection.
An incremental, generational garbage collector for \*E, inspired by
Yip's garbage collector [Yip 1991], has recently been implemented
as an alternative to the stop-and-copy garbage collector\**.
.FS
With a generational garbage collector, objects surviving garbage
collections will not be touched again until there is only a certain
amount of memory left on the heap, triggering a full garbage
collection.
Particularly in applications with large amounts of Scheme code or
other constant data, partial GCs run much faster than full GCs.
With incremental garbage collection, starting a garbage collection does
not suspend the application until the GC is done;
instead, the collector returns control to the application almost
immediately (after having marked pages of interest unreadable with the
\f2mprotect\fP system call) and regains control with a SIGSEGV signal.
.FE
.PP
Extensions to \*E can register \f2before-GC\fP and \f2after-GC\fP
functions with the interpreter; these functions are invoked by the
garbage collector immediately before and after each garbage collection
run.
Within \f2after-GC\fP functions, extensions can determine whether
a particular Scheme object has become garbage, i.\|e. no references
to the object exist any longer.
In this case, an extension may perform some kind of clean-up action;
for example, if the now unreferenced object contains a handle to an open
file, close this file.
.PP
The \*E distribution contains a library based on this mechanism that
enables extensions to register a \f2termination function\fP for
objects of a particular type.
The termination function associated with an object is then invoked
by the garbage collector automatically when this object has been
detected to be unused.
The Xlib extension of \*E uses this library to perform suitable
finalization operations on objects created by the extensions, for
example, close windows, unload fonts, and free colormap objects that
have become unreferenced.
This mechanism is slightly complicated by the fact that objects may
have to be terminated in a predefined order; for instance, when an
X11 display becomes garbage, all objects associated with this
display must be terminated before the display itself is finally closed.
.P
Library Extensions
.PP
The problems we encountered when designing
and implementing \*E's interfaces to the C libraries of X11
are likely to apply to a wide range of similar APIs.
The X11 libraries, especially Xlib, are quite complex; the core Xlib
alone exports more than 600 functions and macros, with
numerous different mechanisms for passing arguments and
for manipulating objects, some of which could be considered rather
verbose and error-prone.
This complexity is, at least partly, caused by the semantic
restrictiveness of the C programming language.
Thus, when designing the Scheme language interface, we had the
opportunity to eliminate some of the ``warts.''
.PP
If integration of a library with an extension language (or interactive
language in general) is not anticipated at the time the programmer's
interface of the library is designed, writing a properly functioning
extension language interface to this library can become rather
challenging or even impossible.
This problem is exemplified by the ``Xt'' toolkit intrinsics library
of X11, in particular by earlier versions of this library.
The following example illustrates a typical difficulty caused by
the ``static'' nature of the programmer's interface to ``Xt'':
.PP
Each class of graphical objects (\f2widgets\fP in ``Xt'' terminology)
exports a list of attributes (\f2resources\fP) that are associated with
objects of this class.
A function is provided by ``Xt'' to obtain the list of resources of a
widget class together with the name and C type (integer, string,
pixmap, color, etc.) of each resource.
On this basis, operations like setting the value of a widget's resource
from within Scheme can be implemented in a straightforward way.
The ``Xt'' extension just has to check if the user-supplied Scheme
value can be converted into a C object of the resource's type, perform
this conversion, and call the Xt-function to set the resource, or
complain to the user if the value is not suitable for this resource.
However, in early versions of Xt, some classes of widgets had a subset of
resources (the \f2constraint resources\fP) whose names and types
could not be obtained by an ``Xt'' application.
While this omission was usually not perceived as a problem for C
programmers (who would know each widget's resources \f2a priori\fP from
reading the documentation), it had a dramatic effect on \*E's ``Xt''
extension, as now the knowledge about these resources had to be
hard-wired into the extension.
As a result, the extension's source code had to be modified for each
new widget set to be made usable from within Scheme code.
.PP
This particular problem has been remedied in recent releases of X11,
though several similar problems remain; even in the UNIX C library.
While design flaws of library interfaces often go unnoticed or are
considered minor when writing C or C++ programs (e.\|g.\& the fact
that implementations of the \f2qsort()\fP functions are
non-reentrant), they become crucial when these libraries are made
accessible to an extension language.
As the importance of extension languages is growing, it is essential
that future library interfaces are designed with the particular
requirements of extensions languages in mind.
.PP
.\" automatic generation of interfaces / foreign functions
.NH
Practical Experiences with \*E
.P
\*E and ISOTEXT
.PP
In developing the document processing system ISOTEXT, \*E
proved to be a major asset [Bormann 1991].
Scheme was used as the implementation language for all user interface
aspects of ISOTEXT.
Apart from providing extensibility to users of ISOTEXT, using \*E as
the base for ISOTEXT made it possible to write the shell code in a high
level language with all its amenities, e.\|g.\& automatic storage
reclamation.
As no recompilation and relinking is necessary, it is a quick operation
to apply and test changes to the user interface.
.PP
\*E provides for a strong ``firewall'' in the ISOTEXT system:
bugs in the Scheme code give rise to errors at the Scheme level, which can
easily be debugged using the (primitive, but functional) built-in
debugger of \*E, while conditions such as core dumps always are the
result of bugs in the ISOTEXT kernel implementation.
.PP
All this assistance for the development of ISOTEXT could be
obtained without sacrificing the performance of the ISOTEXT kernel
system, which is still written in efficient C++.
.PP
\*E also allowed us to isolate the ISOTEXT kernel from the choice of an
X toolkit:
the ISOTEXT kernel is unaware of the toolkit being used (``Xt'' with
OSF/Motif).
The Scheme code builds a user interface using the Motif library
interface and provides X windows to the ISOTEXT kernel.
Input is processed by the Scheme code which calls editor primitives
provided by the ISOTEXT kernel and schedules redisplay operations.
Replacing Xt and OSF/Motif by e.\|g.\& \f2Xview\fP would require no
changes in the ISOTEXT kernel.
.\".PP
.\"As extensions and the \*E kernel are effectively linked together, the
.\"current interface between the two allows extensions to call every
.\"global function in the \*E kernel.
.\"This makes it difficult to rewrite in Scheme primitives that originally
.\"were written in C.
.PP
The work on ISOTEXT clearly identified one single main problem in
writing non-trivial extensions: as any request for new heap space can
trigger a garbage collection, extensions must register local or
temporary Scheme objects with the garbage collector to protect them
from being discarded during a GC run caused by any nested procedure
call.
While this scheme has the advantage that maximum utilization of the
available heap space is guaranteed, it imposes a strict discipline
on the extension programmer.
Failure to properly protect temporary Scheme objects usually results
in delayed crashes of the application that are hard to trace back to the
actual source of the problem.
For instance, when developing the X11 extensions to \*E, most of the time
spent for debugging was due to GC-related bugs.
.\" Similarly, the following bug in the interpreter kernel went unnoticed for years:
.\" .Ss
.\" \s+1newframe = Add_Binding(newframe, Car(b), Eval(val));\s0
.\" .Se
.\" Depending on the C compiler used, \f2newframe\fP is pushed on the argument 
.\" stack before \f2Eval\fP, which may trigger a GC, is called.
.\" The GC generally moves the object to which \f2newframe\fP points,
.\" updating the variable \f2newframe\fP, but not the copy
.\" on the argument stack, which is now a dangling reference.
.\" When the GNU C compiler uncovered this problem, the line was changed to
.\" the proper:
.\" .Ss
.\" \s+1temp = Eval(val);\s0
.\" \s+1newframe = Add_Binding(newframe, Car(b), temp);\s0
.\" .Se
.\"
.\"(cabo: recruiting problems)
.P
\*E and TELES.VISION
.PP
Another example for using Elk and its X interface as the basis for a
user interface subsystem is the TELES.VISION desktop video
conferencing system [TELES 1991].
First, a somewhat generalized User Interface Management System was
built in about 1500 lines of Scheme, which was then instantiated to
build a number of revisions of the TELES.VISION user interface.
The user interface communicates with the rest of the conferencing
system via a remote procedure call C library, using Scheme
continuations as a basis for a simple form of multithreading.
According to the TELES.VISION implementors [Bastian 1993], Elk was a
``perfect fit'' for this application, with the single exception that
its initial garbage collector placed too heavy a burden on the memory
starved initial environment (where 8 MB of memory had to be shared
between an operating system, various realtime device drivers, drivers
for video codec hardware, and an MS-Windows emulation subsystem).
This has since been remedied by adding memory.
Using Elk also helped when TELES.VISION was ported to OS/2 \*- in
particular, its continuations ported easily.
Also, Elk was used in the TELES.VISION project to build a rapid
prototype of the central conference management subsystem (again using
continuations to provide multithreading) within less than two weeks.
.P
Other Projects
.PP
While \*E has been used in the ISOTEXT project since 1987, legal
issues prevented making it publicly available until the fall of 1989.
Since, \*E has gained acceptance, in fact sufficient momentum to
encourage others to contribute software.
Elk has been used successfully as an extension language for a
hypertext database, a distributed version
management system, various CAD programs, testing and simulation
systems for digital circuits as well as environmental models.
It also has found use simply as a Scheme programming environment, in
particular for its X and Motif interface.
.PP
The X extensions have proven useful in particular for writers of
applications with graphical user interfaces based on X; \*E enables
them to write their user interfaces or parts thereof in Scheme to
achieve a high degree of customizability.
.PP
\*E also has found use as a free-standing Scheme implementation.
In combination with the X extensions it is well-suited for teaching X
to beginners, as a tool for interactively exploring X, and as a
platform for rapid prototyping of X-based applications.
.PP
Outside of the UNIX world, we are aware of user-done ports to DOS (both
16 bit and 32 bit using DJGPP), OS/2, and MacOS.
.PP
Users cited the following features as significant for their choice of
Elk: dynamic object code loading, dumping of ready-to-run executables,
Elk's performance, its legally unencumbered availability, and finally
its simplicity and adaptability (and, as users say, its consistent,
clean and well-structured code).
.PP
Users are not happy with various artificial limitations still in the
system (such as the static heap size which with the stop-and-copy
garbage collector needs to be fixed at invocation time), with Elk's
performance, and with the fact that Elk ``likes to be in control''
(i.e.\&, supplies the main program).
In addition, prospective users tend to ponder acceptance problems with
their fellow workers and customers (who might not be well versed in
Lisp/Scheme) before committing to Elk.
Finally, for many extension language applications, Elk is ``too big'',
and users have asked for versions without the more expensive Elk
features such as arbitrary size number support or continuations.
On the other hand, users have asked for additional features such as an
inter-process communication interface, or a better debugger.
Also, a port to MS-Windows has been actively sought.
.NH
Conclusions
.PP
Since the \*E project began, both the research community and
significant industry projects have generated increasing numbers of
``embeddable language'' implementations.
While many such languages inherit the syntactic flavor of BASIC, those
projects that focus on the ability to build non-trivial extensions
recently seem to almost exclusively turn to the Scheme language.
.PP
Scheme has proven to be an effective language for extension language
purposes.
In the beginning of the ISOTEXT project, there were concerns that an
implementation of the full Scheme language would be both too large and
too slow.
These reservations proved to be unfounded:
the binary code size of \*E is still significantly below that of a
medium size application such as \f2vi\fP.
While the performance of \*E may be uninspiring (no compiler is
available), this turned out not to be a critical issue, as any
bottlenecks can easily be replaced by a primitive recoded in C or C++.
.PP
There also were concerns that Scheme was going to be hard to learn for
UNIX users familiar with, say, the Bourne Shell and C.
This seems to be more of a problem with initial acceptance than with
a steep learning curve:
after having overcome the initial barrier (which generally had
to do mainly with perceiving the syntax as queer), users reported the
same rapid increase in productivity they already knew from shell
programming.
It certainly has not been necessary to recruit Lisp programmers to be
able to extend applications with \*E.
.PP
.ds Tx "T\v'.2m'E\v'-.2m'X
Finally, \*E was an exercise in writing portable software without
being restricted to what is considered portable today.
Apart from the well-known problem that true portability between
current relevant platforms cannot be attained by just picking one of
the proclaimed ``standards'', and the unwieldy situation that there
are too many standards for (auto-)configuration of software, a
significant part of the effort in generating \*E was consumed by
devising support for each new platform for dynamic loading, generation
of executables from running programs, and switching between threads of
control (continuations).
Note that many non-trivial applications of today (apart from
Lisp programming environments, GNU emacs and \*(Tx come to mind) need one
or more of these features; also note that most relevant current
platforms can be made to support these features quite well \*- just in
wildly different ways.
.NH
Availability
.PP
\*E is available in legally unencumbered status.
The current version as of June 1994 is 2.2.
The most recent version of \*E is available via anonymous FTP from
ftp.x.org (/contrib) and ftp.fu-berlin.de
(/pub/unix/languages/scheme).
.NH
Acknowledgments
.PP
An early version of \*E was written while one of us was employed at
TELES GmbH, Berlin.
We are grateful to Prof.\& Dr.\& Sigram Schindler of TELES and TU
Berlin for providing the work environment for ISOTEXT and \*E and for
the permission to publish this software.
.PP
The present version is a result of our research work at Technische
Universit\(a:t Berlin, with the benefit of the work of many
contributors.
In particular, we wish to thank Marco Scheibe who wrote the
generational, incremental garbage collector.
.NH
References
.IP "[Abelson et al. 1985]
Harold Abelson and Gerald J. Sussman with Julie Sussman,
\f2Structure and Interpretation of Computer Programs\fP,
MIT Press, Cambridge, Mass., 1985.
.\"
.IP "[Bastian 1993]"
Personal communication with Jan Bastian, TELES.
.\"
.IP "[Bormann et al. 1988]
Ute Bormann, Carsten Bormann, C. Bathe,
SDE \*- A WYSIWYG Editing and Formatting System for ODA and
SGML Documents,
ESPRIT '88, \f2Proceedings of the 5th Annual ESPRIT Conference,
Brussels\fP, November 14-17, 1988.
.\"
.IP "[Bormann 1991]"
Carsten Bormann,
Open Document Processing and the ISOTEXT System,
Doctoral Dissertation, TU-Berlin, 1991.
.\"
.IP "[CFI 1991a]"
CAD Framework Initiative, CFI Extension Language Sub-Committee,
\f2CFI Extension Language Selection Document\fP,
CFI Document Number 87, CAD Framework Initiative Inc., Austin, Texas, 1991.
.\"
.IP "[CFI 1991b]"
CAD Framework Initiative, Extension Language Working Group:
Architecture Technical Sub-Committee,
\f2Extension Language: Core Language Selection\fP,
Draft Proposal Version 0.7, CFI Document Number ARCH-91-G-1,
CAD Framework Initiative Inc., Austin, Texas, 1991.
.\"
.IP "[Clinger et al. 1991]"
William Clinger and Jonathan Rees (Editors),
\f2Revised\*(^4 Report on the Algorithmic Language Scheme\fP,
November 2, 1991.
Available as ftp://cs.indiana.edu/pub/scheme-repository/doc/r4rs.ps.Z.
.\"
.IP "[CLX 1991]"
CLX \*- Common LISP X Interface, 1991.
(Part of the X11 Release 5 distribution available from the
MIT software distribution center.)
.IP "[Cowlishaw 1985]"
M. F. Cowlishaw,
\f2The REXX Language \*- A Practical Approach to Programming\fP
Prentice Hall, Englewood Cliffs, NJ, 1985.
.\"
.IP "[Dybvig 1987]"
R. Kent Dybvig,
\f2The Scheme Programming Language\fP,
Prentice Hall, Englewood Cliffs, NJ, 1987.
.\"
.IP "[Hansen 1990]"
Wilfred J. Hansen,
Enhancing documents with embedded programs: How Ness extends
insets in the Andrew ToolKit,
\f2Proceedings of IEEE Computer Society 1990 International
Conference on Computer Languages\fP,
March 12-15, 1990, New Orleans.
.\"
.IP "[IEEE\|Std\|1178-1990]"
\f2IEEE Standard for the Scheme Programming Language\fP,
New York, May 28, 1991 (approved December 10, 1990).
.\"
.IP "[Joy 1980]"
Bill Joy,
Changes in the VAX system in the Fourth Berkeley Distribution,
Computer Systems Research Group,
University of California, Berkeley,
November 1980.
.\"
.IP "[Lewis et al. 1990]"
Bil Lewis, Dan LaLiberte, the GNU Manual Group,
GNU Emacs Lisp Reference Manual,
Edition 1.03, Free Software Foundation, Cambridge, Mass.,
December 1990.
.\"
.IP "[MIT 1984]"
MIT Scheme Manual, Seventh Edition,
Department of Electrical Engineering and Computer Science,
Massachusetts Institute of Technology,
Cambridge, Mass., September 1984.
.\"
.IP "[Ossanna 1979]"
J. F. Ossanna,
Nroff/Troff User's Manual,
UNIX Programmer's Manual, Seventh Edition, vol. 2,
Bell Telephone Laboratories, Murray Hill, NJ, January 1979.
.\"
.IP "[Ousterhout 1990]"
John K. Ousterhout,
Tcl: An Embeddable Command Language,
\f2Proceedings of the USENIX 1990 Winter Conference\fP,
January 1990, pp. 133-146.
.\"
.IP "[Scheifler et al. 1986]"
Robert W. Scheifler and Jim Gettys,
The X Window System,
\f2ACM Transactions on Graphics\fP, vol. 5, no. 2, pp. 79-109, 1986.
.\"
.IP "[Scheifler et al. 1992]"
Robert Scheifler and James Gettys,
\f2X Window System\fP,
Third Edition,
Digital Press,
1992.
.\"
.IP "[Springer et al. 1989]"
George Springer and Daniel O. Friedman,
\f2Scheme and the Art of Programming\fP,
MIT Press, Cambridge, Mass., 1989.
.\"
.IP "[Stallman 1981]"
Richard M. Stallman,
EMACS \*- The Extensible, Customizable, Self-documenting Display
Editor Production System,
\f2SIGPLAN Notices\fP, vol. 16, no. 6, pp. 147-156, Association for
Computing Machinery, New York, 1981.
.\"
.IP "[TELES 1991]
Das TELES.VISION System \*- Philosophie und Technologie,
TELES GmbH, Berlin, 1991 (in German).
.\"
.IP "[Yip 1991]"
G. May Yip,
Incremental, Generational Mostly-Copying Garbage Collection in
Uncooperative Environments,
WRL Research Report 91/8,
DEC Western Research Laboratory,
Palo Alto, California, 1991.
.ie \n(.U \{\
.NH S A
Appendix:  Extending \*E \*- An Example
.\}
.el \{\
.bp
.SH
.nr H1 1
.af H1 A
Appendix A:  Extending \*E \*- An Example
.\}
.P
The ``ndbm'' Library Extension
.PP
The extensibility mechanisms of \*E can be demonstrated best by
examining a simple library extension.
Consider the \f2ndbm\fP library that is available on most versions
of UNIX.
This library implements functions to maintain a simple database
file of key/contents pairs.
.PP
As shown in Listing @L(ndbm), both the keys and the data to be stored
are described by the type \f2datum\fP; it consists of the data
(a string of bytes) and the length of the data.
\f2dbm_open()\fP opens a database file and returns a handle
to that file to be used in subsequent operations on that
database (a pointer to an opaque data type, similar to the \f2fopen\fP
and \f2readdir\fP interfaces); it returns a null pointer if the
file could not be opened.
A database is closed by a call to \f2dbm_close()\fP.
The data stored under a given key is accessed by the function
\f2dbm_fetch()\fP; it returns an object of type \f2datum\fP
(with a null \f2dptr\fP if the key could not be found).
\f2dbm_store\fP is used to insert an entry into a database and
to modify an existing entry; it returns zero on success and a
non-zero value on error.
.Ls
.Ss
#include <ndbm.h>
.Sl
.Sl
typedef struct {
    char *dptr;
    int dsize;
} datum;
.Sl
.Sl
DBM *dbm_open(char *file, int flags, int mode);
.Sl
void dbm_close(DBM *db);
.Sl
datum dbm_fetch(DBM *db, datum key);
.Sl
int dbm_store(DBM *db, datum key, datum data, int flags);
.Se
.Lc "The UNIX \f2ndbm\fP library"
.Ns
\f2Note:\fP For simplicity, several functions have been omitted.
The \f2flags\fP and \f2mode\fP arguments of \f2dbm_open\fP are that
of the \f2open\fP system call.
The \f2flags\fP argument of \f2dbm_store\fP can be DBM_INSERT to
insert a new entry into the database or DBM_REPLACE to change
an existing entry.
.Ne
.Le ndbm
.PP
The straightforward way to write an \f2ndbm\fP extension to \*E is to
provide a new Scheme data type \f2dbm-file\fP together with the
obligatory type predicate \f2dbm-file?\fP and the Scheme primitive
procedures \f2dbm-open\fP, \f2dbm-close\fP, \f2dbm-fetch\fP and
\f2dbm-store\fP that operate on objects of type \f2dbm-file\fP.
.PP
\f2dbm-open\fP receives the filename (a string or a symbol);
the second argument is one of the symbols \f2reader\fP (open
the file read-only), \f2writer\fP (read and write access), and
\f2create\fP (read and write access, create new file if it does
not exist).
The optional filemode argument is an integer.
\f2dbm-open\fP returns an object of type \f2dbm-file\fP or #f
(false) if the file could not be opened.
\f2dbm-close\fP closes the database file associated with its
argument of type \f2dbm-file\fP.
As this function is called for its side-effect only,
and for lack of a better result, it returns a non-printing object.
.PP
\f2dbm-fetch\fP expects a \f2dbm-file\fP and a string argument
(the key to be searched) and returns a string (the data stored
under the key) or #f if the key does not exist.
Note that in \*E strings may contain arbitrary 8-bit characters,
including the null byte.
\f2dbm-store\fP is called with a \f2dbm-file\fP, two strings
(key and data) and one of the symbols \f2insert\fP and \f2replace\fP.
Its integer return value is the return value of \f2dbm_store()\fP.
.PP
These procedures and the new \f2dbm-file\fP type can be used
by application programmers to manipulate database files
in those parts of their applications that are written in Scheme.
Listing @L(ndbm-example) shows a small example.
.Ls
.Ss
(define expand-mail-alias
  (lambda (alias)
    (let ((d (dbm-open "/etc/aliases" 'reader)))
      (if (not d)
          (error 'expand-mail-alias "cannot open database"))
      (unwind-protect
        (dbm-fetch d alias)
        (dbm-close d)))))
.Sl
(define address-of-staff (expand-mail-alias "staff"))
.Se
.Lc "Using the ndbm extension"
.Ns
\f2Note:\fP The \f2unwind-protect\fP and the \f2error\fP form
are not present in standard Scheme.
.Ne
.Le ndbm-example
.P
The Anatomy of a Scheme Type
.PP
Listing @L(ndbm-skeleton) shows the part of the extension that deals with
the new data type \f2dbm-file\fP and the extension initialization
function.
The variable \f2T_Dbm\fP will hold the unique identifier of the
newly defined type.
The structure \f2S_Dbm\fP defines the C representation of the type;
one such C structure is declared for each composite Scheme type.
Its main component is the handle of the database file that is
contained in each object of type \f2dbm-file\fP.
.Ls
.Ss
#include <scheme.h>
#include <ndbm.h>
.Sl
int T_Dbm;
.Sl
struct S_Dbm {
    DBM *dbm;
    char alive;   /* 0 or 1 */
};
.Sl
#define DBMF(obj) ((struct S_Dbm *)POINTER(obj))
.Sl
int Dbm_Equal(a, b) Object a, b; {
    return DBMF(a)->alive && DBMF(b)->alive && DBMF(a)->dbm == DBMF(b)->dbm;
}
.Sl
void Dbm_Print(d, port) Object d, port; {
    Printf(port, "#[dbm-file %lu]", DBMF(d)->dbm);
}
.Sl
Object P_Is_Dbm(x) Object x; {
    return TYPE(x) == T_Dbm ? True : False;
}
.Sl
void elk_init_dbm() {
    Define_Primitive(P_Is_Dbm,    "dbm-file?", 1, 1, EVAL);
    Define_Primitive(P_Dbm_Open,  "dbm-open",  2, 3, VARARGS);
    Define_Primitive(P_Dbm_Close, "dbm-close", 1, 1, EVAL);
    Define_Primitive(P_Dbm_Store, "dbm-store", 4, 4, EVAL);
    Define_Primitive(P_Dbm_Fetch, "dbm-fetch", 2, 2, EVAL);
.Sl
    T_Dbm = Define_Type("dbm-file", sizeof(struct S_Dbm),
        Dbm_Equal, Dbm_Equal, Dbm_Print, NOFUNC);
}
.Se
.Lc "Skeleton of the ndbm extension"
.Ns
\f2Note:\fP For simplicity some details have been omitted in this
listing, and the calling interface of some functions has been
simplified; the program would not compile in this form.
A working \f2gdbm\fP (GNU dbm) extension is included in the \*E
distribution.
.Ne
.Le ndbm-skeleton
.PP
Scheme objects can usually live longer than their underlying
C objects.
In case of the \f2dbm-file\fP type, a Scheme object of that type
can obviously still be referenced after its database handle has been
closed by a call to \f2dbm-close\fP.
As \*E extensions must not crash the application, we must prevent
such stale objects from being used in further calls to
\f2dbm-fetch\fP, \f2dbm-store\fP, and \f2dbm-close\fP.
One way to achieve this is to record in each Scheme object whether
the underlying C object is still alive or has been terminated.
The boolean component \f2alive\fP in the \f2dbm-file\fP type
serves this purpose.
It is initialized with true and is set to false in \f2dbm-close\fP.
Further operations on objects with \f2alive\fP being false are
rejected.
.PP
The interpreter stores all Scheme objects in variables of type
\f2Object\fP.
An \f2Object\fP is typically a 32-bit value; it is composed of
a \f2tag\fP part and a \f2pointer\fP part.
The \f2tag\fP part indicates the type of the object, and the remaining
bits hold the actual memory address of the object (they point into
the interpreter's heap).
The macros \f2TYPE\fP and \f2POINTER\fP are provided to extract
the fields of an \f2Object\fP.
Each type definition must define a macro to extract the object's
memory address from an \f2Object\fP (by means of \f2POINTER\fP) 
and then cast it into a pointer to the underlying C structure
(see \f2#define DBMF\fP in Listing @L(ndbm-skeleton)).
.PP
\f2Dbm_Equal()\fP implements both the \f2eqv?\fP and the \f2equal?\fP
predicates for \f2dbm-file\fP objects; it returns true if both objects
being compared are alive and contain identical \f2DBM\fP handles.
.PP
\f2Dbm_Print()\fP is called by the interpreter each time an object
of type \f2dbm-file\fP is to be printed; it is invoked with the
object and the Scheme port to which the output is to be sent.
.PP
\f2P_Is_Dbm()\fP implements the primitive procedure \f2dbm-file?\fP
(the type predicate).
As with all primitives, it receives arguments of type \f2Object\fP and
returns an \f2Object\fP, and it has a name beginning with ``P_''.
.PP
The definition of the initialization function \f2elk_init_dbm()\fP
is straightforward; it invokes \f2Define_Primitive()\fP once
for each primitive procedure and finally \f2Define_Type()\fP to
make the new type known to the interpreter.
.PP
The arguments that can be supplied to \f2Define_Primitive()\fP are a
pointer to the function implementing the primitive procedure, the
Scheme name of the primitive, the minimum and maximum number of
arguments, and a symbol indicating the \f2calling discipline\fP of the
primitive.
For most of the functions in this example, the calling discipline is
\f2EVAL\fP, indicating a normal procedure with a fixed number of
arguments, such as \f2car\fP.
Elk also supports procedures with variable argument list, such as
\f2list\fP (\f2VARARGS\fP); and \f2NOEVAL\fP for \f2special forms\fP
(variable number of unevaluated arguments).
.PP
\f2Define_Type()\fP is invoked with the Scheme name of the type, the
size of the type's representation in C or C++ (given as a constant or
as a function), two functions implementing the \f2eqv?\fP and
\f2equal?\fP predicates for objects of this type, a function that is
called by the interpreter to print an object of the new type (the
type's \f2print function\fP), and a function providing information
about the type to the garbage collector.
The return value of \f2Define_Type()\fP is a ``handle'' to the newly
defined type (a small, unique integer); its main uses are to
check the type of arguments supplied to primitive procedures and to
instantiate objects of this type.
.P
Primitive Procedures \*- The Details
.PP
Listing @L(dbm-open) gives the definitions of the primitives \f2dbm-open\fP
and \f2dbm-close\fP.
.Ls
.Ss
static SYMDESCR Flag_Syms[] = {
    { "reader", O_RDONLY },
    { "writer", O_RDWR },
    { "create", O_RDWR|O_CREAT },
    { 0, 0 }
};
.Sl
Object P_Dbm_Open(argc, argv) int argc; Object *argv; {
    char *p;
    DBM *dp;
    Object d;
.Sl
    Make_C_String(argv[0], p);
    dp = dbm_open(p, Symbols_To_Bits(argv[1], 0, Flag_Syms),
                  argc == 3 ? Get_Integer(argv[2]) : 0666);
    if (dp == 0)
        return False;
    d = Alloc_Object(sizeof(struct S_Dbm), T_Dbm, 0);
    DBMF(d)->dbm = dp;
    DBMF(d)->alive = 1;
    return d;
}
.Sl
void Check_Dbm(d) Object d; {
    Check_Type(d, T_Dbm);
    if (!DBMF(d)->alive)
        Primitive_Error("invalid dbm-file: ~s", d);
}
.Sl
Object P_Dbm_Close(d) Object d; {
    Check_Dbm(d);
    DBMF(d)->alive = 0;
    dbm_close(DBMF(d)->dbm);
    return Void;
}
.Se
.Lc "ndbm extension \*- implementation of \f2dbm-open\fP and \f2dbm-close\fP
.Le dbm-open
.PP
\f2dbm-open\fP, as it has an optional argument, is a function with
\f2VARARGS\fP calling discipline (not to be confused with the C
language feature of the same name), as indicated by the last argument
to the \f2Define_Primitive\fP call.
Primitives of this type receive an array of \f2Objects\fP and a count.
.PP
The initial call to the macro \f2Make_C_String\fP checks if the
first argument to \f2dbm-open\fP is a string (or a symbol) and
converts it to a C string.
To obtain the second argument to \f2dbm_open()\fP, the symbol
passed to the Scheme primitive (\f2reader\fP, \f2writer\fP, etc.)
has to be mapped to a corresponding flags combination (\f2O_RDONLY\fP,
\f2O_RDWR\fP, etc.).
This is accomplished by the \*E function \f2Symbols_To_Bits()\fP; it
is invoked with a Scheme symbol, a flag indicating whether a single
symbol or a list of symbols (a mask) is to be converted, and a
table of pairs of symbol names and C integers.
The third argument to \f2dbm_open\fP is the filemode;
\f2Get_Integer()\fP converts a Scheme number to a C integer.
\f2dbm-open\fP finally allocates a new Scheme object of type \f2T_Dbm\fP
on the heap, initializes the components of the object, and returns it.
.PP
The auxiliary function \f2Check_Dbm()\fP is used by the remaining
primitives to check whether a given object is of type \f2dbm-file\fP
and if so, whether it is stale.
In this case an error is signaled; \f2Primitive_Error()\fP enters
the error handler of \*E.
.PP
\f2P_Dbm_Close()\fP just marks the object as stale by setting
\f2alive\fP to false and closes the database file.
.PP
Listing @L(dbm-store) shows the implementation of \f2dbm-store\fP and
\f2dbm-fetch\fP.
\f2Make_Integer()\fP is the counterpart to \f2Get_Integer()\fP;
it converts a C integer into a Scheme number.
Likewise, \f2Make_String()\fP converts a C string into a
Scheme string.
.bp
.Ls
.Ss
static SYMDESCR Store_Syms[] = {
    { "insert",  DBM_INSERT },
    { "replace", DBM_REPLACE },
    { 0, 0 }
};
.Sl
Object P_Dbm_Store(d, key, content, flag) Object d, key, content, flag; {
    datum k, c;
    int result;
.Sl
    Check_Dbm(d);
    Check_Type(key, T_String);
    Check_Type(content, T_String);
    k.dptr = STRING(key)->data;       k.dsize = STRING(key)->size;
    c.dptr = STRING(content)->data;   c.dsize = STRING(content)->size;
    result = dbm_store(DBMF(d)->dbm, k, c,
                       Symbols_To_Bits(flag, 0, Store_Syms));
    return Make_Integer(result);
}
.Sl
Object P_Dbm_Fetch(d, key) Object d, key; {
    datum k, c;
.Sl
    Check_Dbm(d);
    Check_Type(key, T_String);
    k.dptr = STRING(key)->data;       k.dsize = STRING(key)->size;
    c = dbm_fetch(DBMF(d)->dbm, k);
    return c.dptr ? Make_String(c.dptr, c.dsize) : False;
}
.Se
.Lc "ndbm extension \*- implementation of \f2dbm-store\fP and \f2dbm-fetch\fP
.Le dbm-store
.\" ----------
.\" Erwaehnen: Little Languages
.\" ----------
.\" Read the CFI papers again
