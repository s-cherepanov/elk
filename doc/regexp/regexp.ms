.so ../util/tmac.scheme
.Ul
.TL
Reference Manual for the
.sp .5
Elk Regular Expression Extension
.AU
Oliver Laumann
.
.Ch "Introduction"
.
.PP
The regular expression extension defines Scheme language bindings
for the
.Ix POSIX
POSIX regular expression functions that are provided by most
modern
.Ix UNIX
UNIX
versions (\f2regcomp()\fP and \f2regexec()\fP).
You may want to refer to your UNIX system's
.Ix regcomp
\f2regcomp(3)\fP manual for details.
The Scheme interface to the regular expression functions makes
the entire functionality of the usual C language interface
available to the Scheme programmer.
To load the regular expression extension, evaluate the expression
.Ss
(require 'regexp)
.Se
.PP
This causes the files
.Ix regexp.scm
\f2regexp.scm\fP and
.Ix regexp.o
\f2regexp.o\fP to be loaded (\f2regexp.o\fP must be statically
linked with the interpreter on platforms that do not support dynamic
loading of object files).
.PP
Loading the extension provides the
.Ix feature
features \f2regexp\fP and \f2regexp.o\fP.
On systems that do not support the regular expression library
functions, loading the extension succeeds, but no further primitives
or features are defined.
Otherwise, the additional feature
.Ix :regular-expressions
\f2:regular-expressions\fP is provided, so that the expression
.Ss
(feature? ':regular-expressions)
.Se
can be used in Scheme programs to check whether regular
expressions are available on the local platform.
.
.Ch "Creating Regular Expressions"
.
.[[
.Pr make-regexp pattern
.Pr make-regexp pattern flags
.]]
.LP
\f2make-regexp\fP returns an object of the new Scheme type \f2regexp\fP
representing the regular expression specified by the string
argument \f2pattern\fP.
An error is signaled if the underlying call to the C library function
.Ix regcomp
\f2regcomp(3)\fP fails.
The optional
.Ix flags
\f2flags\fP argument is a list of zero or more of the
symbols \f2extended, ignore-case, no-subexpr,\fP and \f2newline\fP;
these correspond to the C constants \s-1\f2REG_EXTENDED, REG_ICASE,
REG_NOSUB,\fP\s0 and \s-1\f2REG_NEWLINE\fP\s0.
.PP
.Ix equality
Two objects of the type \f2regexp\fP are equal in the sense of
\f2equal?\fP if their flags are identical and if their patterns
are equal in the sense of \f2string=?\fP.
Two regular expressions are \f2eq?\fP if their flags are identical
and if they share the same pattern string.
.
.Pr regexp? obj
.LP
This
.Ix "type predicate"
type predicate returns #t if \f2obj\fP is a regular expression, #f otherwise.
.
.[[
.Pr regexp-pattern regexp
.Pr regexp-flags regexp
.]]
.LP
These primitives return the pattern (or
.Ix flags
flags, respectively) specified
in the call to
.Ix make-regexp
\f2make-regexp\fP that has created the regular expression object.
.
.Ch "Matching Regular Expressions"
.
.[[
.Pr regexp-exec regexp string offset
.Pr regexp-exec regexp string offset flags
.]]
.LP
This primitive applies the specified regular expression to the
given string starting at the given offset.
\f2offset\fP is an integer larger than or equal to zero and less than
or equal to the length of \f2string\fP.
If the match succeeds, \f2regexp-exec\fP returns an object of the
new Scheme type
.Ix regexp-match
\f2regexp-match\fP, otherwise #f.
The optional
.Ix flags
\f2flags\fP argument is a list of zero or more of the symbols
\f2not-bol\fP and \f2not-eol\fP which correspond to the constants
\s-1\f2REG_NOTBOL\fP\s0 and \s-1\f2NOT_EOL\fP\s0 in the C language
interface.
.
.Pr regexp-match? obj
.LP
This
.Ix "type predicate"
type predicate returns #t if \f2obj\fP is a regular expression match
(that is, the return value of a successful call to \f2regexp-match\fP),
#f otherwise.
.
.Pr regexp-match-number match
.LP
This primitive returns the number of substrings that matched parenthetic
.Ix subexpression
subexpressions in the original pattern when the given match was created,
plus one (the first substring corresponds to the entire regular
expression rather than a subexpression; see
.Ix regexec
\f2regexec(3)\fP for details).
A value of zero is returned if the match has been created by applying
a regular expression with the
.Ix no-subexpr
\f2no-subexpr\fP flag set.
.
.[[
.Pr regexp-match-start match number
.Pr regexp-match-end match number
.]]
.LP
These primitives return the start offset (or end offset, respectively)
of the substring denoted by the integer \f2number\fP.
A \f2number\fP argument of zero refers to the substring corresponding to
the entire pattern.
The offsets returned by these primitives can be directly used as
arguments to the
.Ix "substring primitive"
\f2\%substring\fP primitive of Elk.
.
.KS
.Ch "Example"
.
.PP
The following program demonstrates a simple Scheme procedure
\f2matches\fP that returns a list of substrings of a given
string that match a given pattern.
An error message is displayed if regular expressions are
not supported by the local platform.
.Ss
.in
(require 'regexp)
.sp .4
(define (matches str pat)
  (let loop ((r (make-regexp pat '(extended))) (result '()) (from 0))
       (let ((m (regexp-exec r str from)))
         (if (regexp-match? m)
             (loop r (cons (substring str (+ from (regexp-match-start m 0))
                                          (+ from (regexp-match-end m 0)))
                           result)
                   (+ from (regexp-match-end m 0)))
             (reverse result)))))
.Se
.KE
