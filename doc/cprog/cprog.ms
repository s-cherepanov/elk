.\" $Revision: 1.25 $
.\"
.ds Vs 3.0
.\"
.so ../util/tmac.scheme
.\"
.\"           Courier bold; used for system output in transcripts.
.ie \n(.U .fp 6 B
.el .fp 6 CB
.\"
.\"           Code start.
.de Cs
.nr sF \\n(.f
.ft 5
.ps -1
.vs -1
.ie \n(.U .RS
.el .in 1c
.nf
.if !\n(.U .sp .3c
..
.\"           Code end.
.de Ce
.fi
.ie \n(.U .RE
.el .in
.vs
.ps
.ft \\n(sF
..
.\"           Newline in code.
.de Cl
.sp .6
..
.\"           Same as .Cl
.de El
.Cl
..
.\"           Example start/end.  As floating keeps (used for figures
.\"           in this document) and regular keeps cannot be mixed, the
.\"           functionality must be simulated here.  This sucks...
.de Es
.Cs
.if !\n(.U .di EE
..
.de Ee
.Ce
.if !\n(.U \{\
.di
.if \\n(dn-\\n(.t .sp 1000
.nf
.EE
.fi
.sp .5
.\}
..
.\"           .K1 header-text
.\"           Major heading with TOC entry.
.de K1
.NH
\\$1
.XS
\\*(SN \\$1
.XE
..
.\"           .K2 header-text
.\"           Level-2 heading with TOC entry.
.de K2
.NH 2
\\$1
.XS \\n(PN 2n
\\*(SN \\$1
.XE
..
.\"           .K3 header-text
.\"           Level-3 heading with TOC entry.
.de K3
.NH 3
\\$1
.XS \\n(PN 4n
\\*(SN \\$1
.XE
..
.\"           .AP appendix-text
.\"            Appendix with TOC entry.
.de AP
.ie \\n(.U .NH
.el .SH
\\$1
.XS
\\$1
.XE
..
.\"           .Rf name value
.\"           Reference anchor.  Each occurrence of `name' anywhere in
.\"           the document will be replaced by `value'.
.de Rf
.if !\n(.U .tm s/@(\\$1)/\\$2/g
..
.\"
.\"           Counter for Figures (auto-pre-increment).
.nr fS 0 1
.\"
.\"           Figure start.
.de Fs
.br
.ie \\n(.$ .KS
.el .KF
.sp 1.2
\u\l'\\n(.lu_'\d
.nr sF \\n(.f
.ft 5
.ps -1
.vs -1
.nf
..
.\"           .Fc caption-text
.\"           Figure caption.  Used at end of Figure, before .Fe.
.de Fc
.sp .2
.fi
.ps
.vs
.ft \\n(sF
.ce 999
\s-1\f3Figure \\n+(fS:\fP  \c
\\$1\s0
.if \\n(.$=2 \s-1\&\\$2\s0
.ce 0
..
.\"           .Fe name
.\"           Figure end.  Defines a reference anchor `name' with the
.\"           number of the Figure as value.
.de Fe
.Rf \\$1 \\n(fS
.LP
\l'\\n(.lu_'
.sp
.KE
..
.\"           Relative indent start.
.de Rs
.if !\\n(.U .RS
..
.\"           Relative indent end.
.de Re
.if !\\n(.U .RE
..
.\"
.TL
Building Extensible Applications with Elk \*-
.sp .3
C/C++ Programmer's Manual
.AU
Oliver Laumann
.AB
Elk (\f2Extension Language Kit\fP) is a Scheme implementation designed
as an embeddable, reusable extension language subsystem for
integration into existing and future applications written in C or C++.
The programmer's interface to Elk provides for a close interworking of
the C/C++ parts of Elk-based, \f2hybrid\fP applications with extensible
Scheme code.
This manual describes the facilities of the C/C++ programmer's
interface that can be used by authors of extensible applications and
Scheme extensions.
Topics range from the architecture of Elk-based applications
and the definition of application-specific Scheme types and primitives
to more advanced subjects such as weak data structures and interacting with
the garbage collector.
Many examples throughout the text illustrate the facilities and
techniques discussed in this manual.
.AE
.\" ---------------------------------------------------------------------------
.K1 "Additional Documentation"
.PP
The official specification of the Scheme programming language is
the @[.``R\*(^4RS''] (William Clinger and Jonathan Rees (editors),
\f2Revised\*(^4 Report on the Algorithmic Language Scheme\fP,
1991).
A slightly modified version of an earlier revision of this report
was adopted as an IEEE an ANSI standard in 1990 (IEEE\|Std\|1178-1990,
\f2IEEE Standard for the Scheme Programming Language\fP, 1991).
.PP
The dialect of Scheme implemented by Elk (a superset of the
official language) is described in the \f2Reference Manual for the
Elk Extension Language Interpreter\fP that is included in the
Elk distribution as troff source and preformatted PostScript files.
Reference manuals for the various predefined Elk extensions
(such as the UNIX and X11 extensions) are also part of the distribution;
see the file ``doc/README'' for an overview of the available
documentation.
.PP
This manual supersedes the document \f2Interfacing Scheme to the
``Real World''\fP that was included in earlier versions of Elk.
.PP
An article about Elk has appeared in USENIX Computing Systems
in 1994 (Oliver Laumann and Carsten Bormann, Elk: The Extension Language Kit,
\f2USENIX Computing Systems\fP, vol.\& 7, no.\& 4, pp.\& 419\-449).
.PP
A recent example of an application that uses Elk as its extension
language implementation is freely available in source and binary
form as \f2http://www.informatik.uni-bremen.de/~net/unroff\fP.
@[.\f2unroff\fP] is a programmable, extensible troff translator with
Scheme-based back-ends for the Hypertext Markup Language.
The source code shown in Appendix B has been directly taken from the
\f2unroff\fP source; authors of Elk-based applications are
encourage to reuse this and other parts of the \f2unroff\fP
source for their own projects.
.\" ---------------------------------------------------------------------------
.K1 "Introduction"
.PP
This manual can be roughly divided into two parts.
The first part (chapters\ @(ch-arch) to\ @(ch-static)) describes the
architectural aspects of Elk-based applications and Elk extensions.
Facilities and tools for building extensible applications with Elk are
introduced here.
Readers who are already familiar with the concepts explained in
this part of the document may want to skip it and begin
reading at chapter\ @(ch-notes) or later.
The second part (covering chapters\ @(ch-notes) to\ @(ch-advanced))
specifies the C functions and types available to application
programmers and describes techniques for building data structures that can
be interfaced to Scheme in an efficient way.
Appendix C briefly summarizes all the functions, macros, types, and
variables exported by the Elk kernel to the C/C++ programmer.
.PP
Here is a short overview of the remaining chapters of this manual.
Chapter\ @(ch-arch) discusses the architecture of extensible
applications based on Elk and their relation to Elk extensions.
Chapter\ @(ch-linking) provides an overview of the two basic
methods for integrating an application (or extensions) with Elk:
dynamic loading and static linking.
Chapter\ @(ch-dynl) describes use of dynamic loading in more detail;
topics include automatic extension initialization and C++ static
constructors embedded in dynamically loaded modules.
Chapter\ @(ch-static) describes several forms of linking user-supplied
code with Elk statically and how these affect the structure
of an application's \f2main()\fP function.
.PP
The remaining chapters are a complete specification of the
functions and types of the C/C++ programmer's interface to Elk.
Chapter\ @(ch-notes) provides introductory notes and advice for
programmers of C/C++ code interfacing to Elk (use of include
files, predefined preprocessor symbols, etc.).
Chapter\ @(ch-anatomy) describes the anatomy of Scheme objects
from the C/C++ programmer's point of view.
Chapter\ @(ch-defprim) explains how applications and extensions can
define new Scheme primitives.
Chapter\ @(ch-types) presents the standard, built-in Scheme types
implemented by Elk (numbers, pairs, vectors, etc.) and functions
for creating and accessing Scheme objects of these types from
within C/C++ code.
The facilities for defining new, first-class Scheme data types
are described in chapter\ @(ch-deftype).
Finally, chapter\ @(ch-advanced) deals with a number of more
advanced topics, such as functions for interacting with the
garbage collector, automatic finalization of inaccessible objects,
definition of user-supplied reader functions, error handling, etc.
.PP
A note on the naming conventions followed by the C identifiers
used throughout this document:
the names of all functions, macros, types, and variables exported by
Elk have their components separated by underscores and capitalized
(as in \f2Register_Object()\fP, for example).
In contrast, the names defined by examples shown in this manual only
use lower case letters, so that they can be distinguished easily from
predefined functions exported by Elk.
.\" ---------------------------------------------------------------------------
.K1 "The Architecture of Extensible Applications"
@[.=application architecture]@[.=extensible application]
.Rf ch-arch \*(SN
.PP
Extensible applications built with Elk are @[.=hybrid application]
\f2hybrid\fP in that they consist of code written in a mixture of
languages\*-code written in the application's
@[.\f2implementation language\fP] (C or C++) and code written in the
@[.\f2extension language\fP] (Scheme).
An application of this kind is usually composed of two layers,
a low-level C/C++ layer that provides the basic,
performance-critical functionality of the application, and on top of
that a higher-level layer which is written in Scheme and interpreted
at runtime.
.PP
The Scheme-language portion of an Elk-based application may range from
just a few dozen lines of Scheme code (if a simple form of
customization is sufficient) to fifty percent of the application or
more (if a high degree of extensibility is required).
As Scheme code is interpreted at runtime by an interpreter embedded
in the application, users can customize and modify the application's
Scheme layer or add and test their own Scheme procedures;
recompilation, access to the C/C++ source, or knowledge of the
implementation language are not required.
Therefore, an application can achieve highest extensibility by
restricting its low-level part to just a small core of time-critical
C/C++ code.
.PP
To enable extensions to ``work on'' an application's internal data
structures and state, the application core exports a set of new,
application-specific Scheme data types
and primitives operating on them to the Scheme layer.
These types and primitives can be thought of as a ``wrapper''
around some of the C/C++ types and functions used by the application's core.
For example, the core of an Elk-based newsreader program would export
first-class Scheme types representing \f2newsgroups\fP,
\f2subscriptions\fP, and \f2news articles\fP; these types would
encapsulate the corresponding low-level C ``structs'' or C++ classes.
In addition, it would export a number of Scheme primitives to
operate on these types\*-to create members of them (e.\|g.\& by
reading a news article from disk), to present them to the user through
the application's user-interface, etc.
Each of these primitives would recur on one or more corresponding C or
C++ functions implementing the functionality in an efficient way.
.PP
Another job of the low-level C/C++ layer of an application is to hide
platform-specific or system-specific details by providing suitable
abstractions, so that the Scheme part can be kept portable and simple.
For example, in case of the newsreader program, extension writers
should not have to care about whether the news articles are stored in a
local file system or retrieved from a network server, or about the
idiosyncrasies of the system's networking facilities.
Most of these system-specific details can be better dealt with in a
language oriented towards systems programming, such as C, than in
Scheme.
.PP
To decide whether to make a function part of the low-level
part of an application or to write it in the extension language,
you may ask yourself the following questions:
.IP \(bu
\f2Is the function performance-critical?\&\fP
.RS
.LP
If the answer to this question is \f2yes\fP,
put the function into the C/C++ core.
For example, in case of the newsreader application, a primitive to search
all articles in a given newsgroup for a pattern is certainly
performance-critical and would therefore be written in the
implementation language, while a function to ask the user to
select an item from a list of newsgroups is not time-critical
and could be written Scheme.
.RE
.IP \(bu
\f2Does the function have to deal with platform-specific details?\&\fP
.RS
.LP
For example, a function that needs to allocate and open a UNIX
pseudo-tty or to establish a network connection needs to care
about numerous system-specific details and different kinds of
operating system facilities and will therefore be written in
C/C++ rather than in Scheme.
.RE
.IP \(bu
\f2In which language can the function be expressed more ``naturally''?\&\fP
.RS
.LP
A function that parses and tokenizes a string can be expressed more
naturally (that is, in a significantly more concise and efficient
way) in a language such as C than in Scheme.
On the other hand, functions to construct trees of news articles, to
traverse them, and to apply a function to each node are obvious
candidates for writing them in a Lisp-like language (Scheme).
.RE
.IP \(bu
\f2Are customizability and extensibility important?\&\fP
.RS
.LP
If it is likely that the application's users will want to customize
or augment a function or even replace it with their own versions,
write it in the extension language.
If, for some reason, this is impossible or not practicable, at least
provide suitable @[.``hooks''] that enable users to influence the
function's operation from within Scheme code.
.RE
.\" ---------------------------------------------------------------------------
.K2 "Scheme Extensions"
@[.=Scheme extensions]
.PP
In addition to the Scheme interpreter component, Elk consists of
a number of \f2Scheme extensions\fP.
These extensions are not specific to any kind application and are
therefore reusable.
They provide the ``glue'' between Scheme and a number of
external libraries, in particular the X11 libraries and the UNIX C
library (exceptions are the @[.record extension] and the
@[.bitstring extension] which provide a functionality of their own).
The purpose of these extensions
is to make the functionality of the external libraries
(for example, the UNIX system calls) available to Scheme as Scheme data
types and primitives operating on them.
.PP
While the Scheme extensions are useful for writing freestanding Scheme
programs (e.\|g.\& for @[.rapid prototyping] of X11-based Scheme programs),
their main job is to help building applications that
need to interface to external libraries on the extension language
level.
The @[.X11 extension]s, for instance, are intended to be used
by applications with a graphical user interface based on the
X window system.
By linking the X11 extensions (in addition to the Scheme interpreter)
with an Elk-based application,
the application's user interface can be written entirely
in Scheme and will therefore be inherently customizable and extensible.
As the Scheme extensions are reusable and can be shared between
applications, extension language code can be written in a portable
manner.
.\" ---------------------------------------------------------------------------
.K2 "Applications versus Extensions"
.PP
As far as the C/C++ programmer's interface to Elk (that is, the subject
of this manual) is concerned, there is not really a technical
difference between Scheme \f2extensions\fP on the one hand (such as the
X11 extensions), and Elk-based, extensible \f2applications\fP on the
other hand.
Both are composed of an efficient, low-level C/C++ core and,
above that, a higher-level layer written in Scheme.
In both cases, the C/C++ layer exports a set of Scheme types and
primitives to the Scheme layer (that is, to the Scheme
\f2programmer\fP) and thus needs to interact with the Scheme interpreter.
Because of this analogy, the rest of the manual will mostly drop
the distinction between applications and extensions and concentrate
on the interface between C/C++ and Elk.
.PP
The only noteworthy difference between applications and extensions
is that the former tend to have their own @[.\f2main()\fP]
function that gains control on startup, while Scheme extensions do not
have a \f2main()\fP entry point\*-they are usually loaded into the
interpreter (or application) during runtime.
This distinction will become important in the next chapter, when
the different ways of joining Elk and C/C++ code will be discussed.
.\" ---------------------------------------------------------------------------
.K1 "Linking Applications and Extensions with Elk"
.Rf ch-linking \*(SN
.PP
There are two different mechanisms for integrating compiled C/C++ code
(extensions or an application) with Elk:
@[.\f2static linking\fP] and @[.\f2dynamic loading\fP].
The object files that make up an Elk-based application are usually
linked statically with the Scheme interpreter in the normal
way to produce an executable program.
Compiled extensions, on the other hand, are usually dynamically
loaded into the running Scheme interpreter as they are needed.
These conventions reflect the normal case;
Scheme extensions may as well be linked statically with the interpreter
.IP \(bu
to produce a ``specialized'' instance of the interpreter (for example,
when developing X11-based Scheme code, an extended version of the
interpreter may be produced by linking it statically with the
X11 extensions);
.IP \(bu
if a particular extension is required by an application from the
beginning (an application with an X-based user-interface would
be linked with the X11 extensions statically, as loading on-demand would
not be useful in this case);
.IP \(bu
on the (few) platforms where dynamic loading is not supported or
where dynamic loading has a large performance overhead.
.PP
Likewise, dynamic loading is not only useful for on-demand loading
of reusable Scheme extensions; \f2applications\fP can benefit
from this facility as well.
To reduce the size of the final executable, parts of an
application may loaded dynamically rather than linked statically if
they are used infrequently or if only a few of them are used at a time.
Dynamic loading enables the author of an extensible application to
decompose it into an arbitrary number of individual parts as an
alternative to combining them statically into a large, monolithic
executable.
An extensible newsreader program, for example, may include a separate
spelling check module that is dynamically loaded the first time it
is needed (i.\|e.\& when a newly written news article is to be
spell-checked).
.PP
The capability to dynamically load compiled C/C++ code into a running
application enables users to write @[.\f2hybrid extension]s\fP which
consist of a low-level C/C++ part and a high-level part written in
Scheme.
As a result, extensions can execute much faster (extensions to the
Emacs editor, for example, must be entirely written in Emacs-Lisp and
can therefore become slow if sufficiently complex); and
extensions can deal more easily with low-level, platform-specific
details.
.\" ---------------------------------------------------------------------------
.K1 "Dynamic Loading"
.Rf ch-dynl \*(SN
@[.=dynamic loading]
.PP
Object files (compiled C/C++ code) are loaded by means of the standard
@[.\f2load\fP primitive] of Scheme, just like ordinary Scheme files.
All you need to do is to compile your C or C++ source file,
apply the @[.\f2makedl\fP script] that comes with the Elk distribution
to the resulting object file, and load it into the interpreter or
application.
\f2makedl\fP prepares object files for dynamic loading (which is
a no-op on most platforms) and combines several object files into
one to speed up loading; arguments are the output file and one
or more input files or additional libraries (input and output file
may be identical):
.Es
\f6%\fP cc \-c \-I/usr/elk/include file.c
\f6%\fP /usr/elk/lib/makedl file.o file.o
\f6%\fP scheme
\f6>\fP (load 'file.o)
\f6>\fP
.Ee
(This examples assumes that Elk has been installed under ``/usr/elk''
on your site.
Additional arguments may be required for the call to \f2cc\fP.)
.PP
Elk does not attempt to discriminate object code and Scheme code
based on the files' contents; the names of object files are
required to end in ``.o'', the standard suffix for object modules
in UNIX.
Scheme files, on the other hand, end in ``.scm'' by convention.
This convention is not enforced by Elk\*-everything that is not
an object file is considered to be a Scheme file.
A list of object files may be passed to the \f2load\fP primitive
which may save time on platforms where a call to the system linker
is involved.
.PP
Loading object files directly as shown above is uncommon.
Instead, the Scheme part of a @[.hybrid extension] usually loads its
corresponding object file (and all the other files that are required)
automatically, so that one can write, for example,
.Es
(require 'unix)
.Ee
to load the @[.UNIX extension].
This expression causes the file \f2unix.scm\fP to be loaded, which
then loads the object file \f2unix.o\fP\*-the UNIX extension's low-level
part\*-automatically on startup.
Additional \f2load-libraries\fP (as explained in the next section)
may be set by the Scheme file immediately before loading the
extension's object file.
.PP
When an object file is loaded, @[.unresolved reference]s are resolved
against the symbols exported by the running interpreter or by the
combination of an application and the interpreter (the \f2base
program\fP).
This is an essential feature, as dynamically loaded extensions
must be able to reference the elementary Scheme primitives
defined by the interpreter core
and all the other functions that are available to the
extension/application programmer.
In addition, references are resolved against the symbols exported
by all previously loaded object files.
The term @[.\f2incremental loading\fP] is used for this style of dynamic
loading, as it allows building complex applications from small
components incrementally.
.\" ---------------------------------------------------------------------------
.K2 "Load Libraries"
.PP
Dynamically loadable object files usually have unresolved references
into one or more libraries, most likely at least into the standard
@[.C library].
Therefore, when loading an object file, references are resolved not
only against the base program and previously loaded object files,
but also against a number of user-supplied @[.\f2load libraries\fP].
The @[.X11 extension]s of Elk, for instance, need to be linked
against the respective libraries of the @[.X window system], such as
\f2libX11\fP and \f2libXt\fP.
These load libraries can be assigned to the Scheme variable
\f2load-libraries\fP which is bound in the top-level environment
of Elk.
Typically, \f2load-libraries\fP is dynamically assigned a set of
library names by means of @[.\f2fluid-let\fP] immediately before calling
\f2load\fP.
For example, the @[.Xlib extension] (\f2xlib.scm\fP) contains
code such as
.Es
(fluid-let
  ((load-libraries
     (string-append "\-L/usr/X11/lib \-lX11 " load-libraries)))
  (load 'xlib.o))
.Ee
to load the accompanying object file (\f2xlib.o\fP), linking it against the
system's X library in addition to whatever libraries were already in
use at that point.
The default value of \f2load-libraries\fP is ``\-lc'' (i.\|e.\& the
C library), as extensions are likely to use functions from this
library in addition to those C library functions that have already
been linked into the base program or have been pulled in by
previously loaded object files.
By using \f2string-append\fP in the example above, the specified
libraries are added to the default value of \f2load-libraries\fP rather
than overwriting it.
The exact syntax of the load libraries is platform specific.
For instance, ``\-L/usr/X11/lib'' as used above is
recognized by the system linker of most UNIX variants as an option
indicating in which directory the libraries reside on the system,
but different options or additional libraries are required on certain
platforms (as specified by the platform's ``config/site'' file
in the Elk distribution).
.\" ---------------------------------------------------------------------------
.K2 "Extension Initializers and Finalizers"
.PP
When loading an object file, Elk scans the file's symbol table
for the names of @[.extension initialization function]s or
@[.\f2extension initializer\fP]s.
These extension initializers are the initial entry points to
the newly loaded extension; their names must have the prefix
@[.``elk_init_''] (earlier the prefix ``init_'' was used; it was changed
in Elk \*(Vs to avoid name conflicts).
Each extension initializer found in the object file is invoked
to pass control to the extension.
The job of the extension initializers is to register the Scheme
types and primitives defined by the extension with the interpreter
and to perform any dynamic initializations.
.PP
As each extension may have an arbitrary number of initialization
functions rather than one single function with a fixed name, extension
writers can divide their extensions into a number of independent
modules, each of which provides its own initialization function.
The compiled modules can then be combined into one dynamically loadable
object file without having to lump all initializations into a central
initialization function.
.PP
In the same manner, extension can define an arbitrary number of
@[.\f2extension finalization function]s\fP which are called on termination
of the Scheme interpreter or application.
The names of finalization functions begin with @[.``elk_finit_''].
Extension finalization functions are typically used for clean-up
operations such as removing temporary files.
.PP
The extension initializers (as well as the finalizers) are called
in an unspecified order.
.\" ---------------------------------------------------------------------------
.K2 "C++ Static Constructors and Destructors"
.PP
In addition to calling extension initialization functions, the
\f2load\fP primitives invokes all @[.C++ static constructor]s that are
present in the dynamically loaded object file in case it contains
compiled C++ code.
Likewise, @[.C++ static destructor]s are called automatically on
termination.
The constructors and destructors are called in an unspecified order,
but all constructors (destructors) are called before calling any
extension initializers (finalizers).
Elk recognizes the function name prefixes of static constructor and
destructor functions used by all major UNIX @[.C++ compiler]s; new prefixes
can be added if required.
.\" ---------------------------------------------------------------------------
.K1 "Static Linking"
@[.=static linking]
.Rf ch-static \*(SN
.PP
Linking user-supplied code with Elk statically can be used as an
alternative to dynamic loading on platforms that do not support it,
for applications with their own @[.\f2main()\fP],
and to avoid the overhead of loading frequently used Elk extensions.
Dynamic loading and static linking may be used in combination\*-
additional object files can be loaded in a running executable
formed by linking the Scheme interpreter with extensions or with
an application (or parts thereof).
.PP
When making the Scheme interpreter component of Elk, these executables
and object files get installed (relative to your \f2install_dir\fP
which usually is ``/usr/elk'' or ``/usr/local/elk''):
.Rs
.IP \f2bin/scheme\fP
The freestanding, plain Scheme interpreter.
.IP \f2lib/standalone.o\fP
@[.=standalone.o]
The Scheme interpreter as a relocatable object file which can be
linked with user-supplied object files to form an executable.
This object file contains a \f2main()\fP function; thus the
Scheme interpreter starts up in the normal way when the executable
is invoked.
.IP \f2lib/module.o\fP
@[.=module.o]
Like \f2standalone.o\fP, except that the object file does not
export its own \f2main()\fP function.
Therefore, the object files linked with it have to supply a \f2main()\fP.
.Re
.PP
The object file \f2standalone.o\fP is typically linked with a number
of Elk extensions (e.\|g.\& the X11 extensions), while \f2module.o\fP
is used by Elk-based applications which contribute their own
\f2main()\fP and need to be ``in control'' on startup.
.\" ---------------------------------------------------------------------------
.K2 "Linking the Scheme Interpreter with Extensions"
.PP
A shell script @[.\f2linkscheme\fP] (installed as ``lib/linkscheme'')
simplifies combining the Scheme interpreter with a number
of\*-user-supplied or predefined\*-extensions statically.
This script is called with the name of the output file (the resulting
executable) and any number of object files and libraries.
It basically links the object files and libraries with
``standalone.o'' and supplies any additional libraries that may
be required by the interpreter.
In general, this can be done just as well by calling the linker or
compiler directly, but \f2linkscheme\fP also takes care of
additional processing that needs to be performed on at least one
platform (currently AIX).
.PP
To create an instance of Elk including the Xlib, Xt, and Xaw
extensions, \f2linkscheme\fP would be used as follows (again
assuming you have installed the software under ``/usr/elk''):
.Es
\f6%\fP cd /usr/elk
\f6%\fP lib/linkscheme x11scheme runtime/obj/xt.o runtime/obj/xaw/*.o \e
     \-lXaw \-lXmu \-lXt \-lSM \-lICE \-lX11 \-lXext
.Ee
.PP
The exact form of the libraries depends on your platform and X11
version; for example, additional options may be required if X11
is not installed in a standard location at your site.
\f2xlib.o\fP is the @[.Xlib extension], \f2xt.o\fP is the X toolkit
intrinsics (Xt) extension, and the subdirectory \f2xaw\fP holds
the object files for all the @[.Athena widgets].
The executable \f2x11scheme\fP can now be used to run arbitrary
X11 applications using the Athena widgets without requiring
any runtime loading of object files belonging to the
@[.X11 extension]s:
.Es
\f6%\fP x11scheme
\f6>\fP (load '../examples/xaw/dialog.scm)
[Autoloading xwidgets.scm]
[Autoloading xt.scm]
[Autoloading siteinfo.scm]
\&...
.Ee
.PP
In the same way, \f2linkscheme\fP can be used to link the
Scheme interpreter with any new, user-supplied extensions,
with parts of an Elk-based application, or with any combination
thereof.
.\" ---------------------------------------------------------------------------
.K3 "Automatic Extension Initialization"
.Rf ch-autoinit \*(SN
.PP
When linking Elk with extensions, it is \f2not\fP necessary to add
calls to the @[.extension initializer]s to the Scheme interpreter's
\f2main()\fP function and recompile the interpreter;
all extensions are initialized automatically on startup.
To accomplish this kind of automatic initialization, Elk scans
its own symbol table on startup, invoking any @[.``elk_init_'']
functions and @[.C++ static constructor]s, in the
same way the symbol table of object files is scanned when
they are dynamically loaded.
@[.Extension finalizer]s and @[.C++ static destructor]s are saved
for calling on exit.
Automatic extension initialization only works if
.Rs
.IP \(bu
the executable file has a symbol table (i.\|e.\& you must not
strip it)
.IP \(bu
the executable file can be opened for reading
.IP \(bu
the interpreter can locate its executable file by scanning the
shell's directory search path.
.Re
.PP
The performance overhead caused by the initial scanning of the
symbol is small; the program's symbol table can be read or mapped
into memory efficiently (it it has not been automatically mapped
into the address space by the operating system in the first place).
.\" ---------------------------------------------------------------------------
.K2 "Linking the Scheme Interpreter with an Application"
.PP
Elk-based applications that have their own \f2main()\fP are linked with
the Scheme interpreter installed as \f2module.o\fP which, unlike
\f2standalone.o\fP, does not export a \f2main()\fP function.
No special \f2linkscheme\fP script is required to link with \f2module.o\fP;
application writers usually will add ``/usr/elk/lib/module.o''
(or whatever the correct path is) to the list of object files
in their Makefile.
To simplify linking with Elk, a trivial script @[.\f2ldflags\fP]
(which lives in ``lib'' along with \f2linkscheme\fP) is supplied that
just echoes any additional libraries required by the Scheme
interpreter.
Application developers may use \f2ldflags\fP in their Makefiles.
.PP
As \f2module.o\fP does not have a \f2main()\fP entry point,
an application using it must initialize the interpreter from
within its own \f2main()\fP.
This is done by calling .@[.\f2Elk_Init()\fP]:
.Es
void Elk_Init(int argc, char **argv, int init_flag, char *filename);
.Ee
.PP
\f2Elk_Init()\fP is only defined by \f2module.o\fP and is essentially
a ``wrapper'' around the Scheme interpreter's \f2main()\fP.
\f2argc\fP and \f2argv\fP are the arguments to be passed to
the Scheme interpreter's \f2main()\fP.
These may or may not be the calling program's original arguments;
however, @[.\f2argv[0\]\fP] must be that from the calling program
in any case (because its address is used by Elk to determine
the program's stack base).
If \f2init_flag\fP is nonzero, the interpreter scans its symbol table
to invoke @[.extension initializer]s as described in @(ch-autoinit).
@[.C++ static constructor]s, however, are never invoked by
\f2module.o\fP (regarless of \f2init_flag\fP), because they are already
taken care of by the runtime startup in this case.
If \f2filename\fP is nonzero, it is the name of Scheme file to
be loaded by \f2Elk_Init()\fP.
.\" ---------------------------------------------------------------------------
.K3 "An Example ``main()'' Function"
.PP
Figure @(main) shows a realistic (yet somewhat simplified) example
\f2main()\fP function of an application using Elk.
.Fs
char *directory;
.El
int main(int ac, char **av) {
	char **eav;
	int eac = 1, c;
.El
	Set_App_Name(av[0]);
	eav = safe_malloc((ac+2+1) * sizeof(char *));    /* ac + -p xxx + 0 */
	eav[0] = av[0];
	while ((c = getopt(ac, av, "gh:o")) != EOF) switch (c) {
		case 'o':
			\f2process option...\fP
		case 'g':
			eav[eac++] = "-g"; break;
		case 'h':
			eav[eac++] = "-h"; eav[eac++] = optarg; break;
		case '?':
			usage(); return 1;
	}
	if ((directory = getenv("APP_DIR")) == 0)
		directory = DEFAULT_DIR;
	eav[eac++] = "-p";
	eav[eac] = safe_malloc(strlen(directory) + 11);
	sprintf(eav[eac++], ".:%s/elk/scm", directory);
	eav[eac] = 0;
	Elk_Init(eac, eav, 0, 0);
.El
	\f2initialize application's modules...\fP
.El
	boot_code();
.El
	\f2application's main loop (if written in C)\fP
	...
.Fc "Example \f2main()\fP of an Elk-based application (simplified)"
.Fe main
.PP
The code shown in the example must construct a new argument
vector to be passed to \f2Elk_Init()\fP, because the application
has command line options of its own (just \f2\-o\fP in the example).
Two Elk-options (\f2\-g\fP and \f2\-h\fP) are handed to
\f2Elk_Init()\fP if present, so that a mixture of Elk-specific and
application-specific options can be given (see the manual page for
the Scheme interpreter for the meaning of Elk's options).
(\f2safe_malloc()\fP is assumed to be a wrapper around \f2malloc()\fP
with proper error-checking.)
@[.\f2Set_App_Name()\fP] is provided by Elk and is called with a name
to be displayed in front of fatal error messages by the interpreter.
.PP
When all the options have been parsed, an additional option
\f2\-p\fP is synthesized to provide a minimal initial @[.\f2load-path\fP]
for Elk.
This load-path consists of the current directory and a subdirectory
of the directory under which the application expects its files
that are needed during runtime.
An environment variable can be used to set this directory.
Defining a load-path like this has the benefit that a minimal,
self-contained Elk runtime environment (e.\|g.\& a toplevel
and the debugger) can be shipped with binary distributions of the
application so that users are not required to have Elk installed at
their sites.
.PP
When Elk has been initialized by calling \f2Elk_Init()\fP,
the application may initialize all its other modules and finally
load an initial Scheme file that ``boots'' the Scheme part of the
application (which may involve loading further Scheme files).
This initial Scheme file may be quite simple and just define a few
functions used later, or it main contain the application's entire
``driving logic'' or interactive user-interface.
This is accomplished by a function \f2boot_code()\fP which may
as simple as this:
.Es
void boot_code(void) {
	char *fn = safe_malloc(strlen(directory) + 30);
.El
	sprintf(fn, "%s/scm/app.scm", directory);
	Set_Error_Tag("initial load");
	Load_File(fn);
	free(fn);
}
.Ee
.PP
@[.\f2Load_File()\fP] is defined by Elk and loads a Scheme file
whose name is supplied as a C string.
@[.\f2Set_Error_Tag()\fP] may be used by extensions and applications to
define the symbol that is passed as the first argument to the
standard @[.error handler] when a Scheme error is signaled
(see section @(ch-error)).
.\" ---------------------------------------------------------------------------
.K2 "Who is in Control?"
.Rf ch-control \*(SN
.PP
When an application's object files are loaded into the interpreter
dynamically or are linked with the interpreter using @[.\f2linkscheme\fP],
control initially rests in the interpreter.
In contrast, when the application is linked using @[.\f2module.o\fP]
and @[.\f2Elk_Init()\fP] as shown in the previous section, it defines
its own \f2main()\fP function, and hence the application is
``in control'' on startup.
.PP
From a technical point of view, it does not really make a difference
whether control rests in the interpreter or in the application
initially.
In the first case, the main ``driving logic'' (or ``main loop'') of
the application can simply be wrapped in a Scheme primitive which
is then called by the Scheme toplevel on startup to pass control
back to the application, if this is desired.
In any case, control usually changes frequently between the Scheme
interpreter and the actual application anyway\*-the Scheme interpreter
invokes callback functions or Scheme primitives provided by the
application, which may in turn invoke Scheme procedures or load
Scheme files, and so on.
.PP
The @[.Tcl]-like style of use, where control rests in the C-part of the
application most of the time, and where this C code ``calls out'' to
the interpreter occasionally by passing it an extension language
expression or a small script, is not typical for Elk.
It is supported, though; Elk provides a simple extension
to pass a Scheme expression to the interpreter as a C string and
receive the result in the same form, similar to what \f2Tcl_Eval()\fP
does in Tcl (see section @(ch-funcall)).
In a typical Elk-based application the extension language serves
as the ``backbone'' of the application:
the application's driving logic or main loop is written entirely in
Scheme, and this Scheme code calls out to the application's C layer,
using the data types, primitives, and other callbacks exported to the
extension language by the application.
With the help of the @[.X11 extension]s, the entire (graphical) user
interface of an application can be written in Scheme easily;
control can then passed to the application's C/C++ layer whenever
an Xt callback is triggered.
In this case, the application's ``main loop'' consists of a call
to the Scheme primitive corresponding to the X toolkit function
\f2XtAppMainLoop()\fP (the main event dispatch loop).
.\" ---------------------------------------------------------------------------
.K1 "Notes for Writing C/C++ Code Using Elk"
.Rf ch-notes \*(SN
.PP
This chapter describes general conventions and usage notes for
Elk-based C/C++ code and introduces a few useful facilities that
are not directly related to Scheme.
.\" ---------------------------------------------------------------------------
.K2 "Elk Include Files"
.PP
Every C or C++ file using functions, macros, or variables defined
by Elk must @[.=include files]include the file @[.\f2scheme.h\fP]:
.Es
#include <scheme.h>      \f1or:\fP      #include "scheme.h"
.Ee
.PP
This include file resides in a subdirectory \f2include\fP of
the directory where Elk has been installed on your system.
You must insert a suitable \-I option into your Makefiles to add
this directory to the C compiler's search path.
``scheme.h'' includes several other Elk-specific include files
from the same directory and, in addition, the standard C include
files @[.\f2<stdio.h>\fP] and @[.\f2\%<signal.h>\fP].
.\" ---------------------------------------------------------------------------
.K2 "Standard C and Function Prototypes"
.PP
All the examples shown in this manual are written in @[.ANSI/ISO C].
This assumes that the Elk include files have been installed with
@[.function prototypes] enabled.
Whether or not function prototypes are enabled is controlled by
a definition in the platform- and compiler-specific ``config/system''
file that has been selected for configuring Elk.
However, if the include files have function prototypes disabled,
prototypes are enable automatically if you are compiling your
code with a @[.C compiler] that defines the symbol @[.``_\^_STDC_\^_]''
as non-zero, or with a @[.C++ compiler] that defines @[.``_\^_cplusplus'']\**.
.FS
Although the public include files provided by Elk can be used
by C++ code, Elk itself cannot be compiled with a C++ compiler.
The interpreter has been written in C to maximize portability.
.FE
.PP
Elk include files that have been installed with function prototypes
disabled can also be ``upgraded'' by defining the symbol
@[.``WANT_PROTOTYPES''] before including ``scheme.h''.
Similarly, include files installed without function prototypes
can be used with a non-ANSI C compiler by defining the symbol
@[.``NO_PROTOTYPES''] before including ``scheme.h''.
.\" ---------------------------------------------------------------------------
.K2 "External Symbols Defined by Elk"
.PP
As extensions or applications are linked with Elk (regarless of whether
dynamic loading or static linking is used), they can in general
reference all external symbols exported by Elk.
Of these, only the symbols described in this manual may be used safely.
Use of other (private) symbols results in non-portable code, as
the symbols may change their meaning or may even be removed from future
releases of Elk.
The same restriction applies to the macros and types defined by
the include files of Elk.
.PP
In addition to the symbols defined by the Scheme interpreter kernel,
those exported by other @[.Scheme extensions] that are present in the same
executable (or have been loaded earlier) can be referenced from within
C/C++ code.
These extensions are not subject of this manual; you should refer
to the relevant documentation and the public include files that
are part of the extensions.
.PP
If Elk is linked with an application that has its own \f2main()\fP
function, none of the functions exported by Elk must be used before
the initial call to @[.\f2Elk_Init()\fP] (except \f2Set_App_Name()\fP).
.\" ---------------------------------------------------------------------------
.K2 "Calling Scheme Primitives"
.Rf ch-prims \*(SN
.PP
A large subset of the symbols exported by the Scheme interpreter is
the set of functions implementing the @[.Scheme primitives].
These may be used safely by extensions and applications.
There exists one C function for each Scheme primitive.
Its name is that of the corresponding primitive with the following
conversions applied:
.Rs
.IP \(bu
dashes are replaced by underscores, and the initial letters of the
resulting word components are capitalized;
.IP \(bu
the prefix ``P_'' is prepended;
.IP \(bu
``\(mi>'' is replaced by ``_To_'' (as in \f2vector\(mi>list\fP);
.IP \(bu
a trailing exclamation mark is deleted, except for \f2append!\fP and
\f2reverse!\fP, where ``_Set'' is appended;
.IP \(bu
a trailing question mark is replaced by the letter `p' (except for
\f2eq?, eqv?, equal?\&\fP and the string and character comparison
primitives, where it is deleted);
.Re
.LP
The names of a few functions are derived differently as shown
by this table:
.RS
.TS
box, tab(~);
c c
c l.
Scheme Primitive~C Function
_
<~P_Generic_Less()
>~P_Generic_Greater()
\&=~P_Generic_Equal()
<=~P_Generic_Eq_Less()
>=~P_Generic_Eq_Greater()
1+~P_Inc()
1\(mi and \(mi1+~P_Dec()
+~P_Generic_Plus()
\(mi~P_Generic_Minus()
*~P_Generic_Multiply()
/~P_Generic_Divide()
let*~P_Letseq()
.TE
.RE
.PP
According to these rules, the primitive \f2exact\(mi>inexact\fP can
be used from within C as \f2P_Exact_To_Inexact()\fP,
the predicate \f2integer?\&\fP is available as \f2P_Integerp()\fP, etc.
Authors of reusable Scheme extensions are encouraged to follow
these (or similar) naming conventions in their code.
.PP
All the functions implementing Scheme primitives (as well as
special forms, which are treated as primitives in Elk) receive
Scheme objects or arrays thereof as their arguments and return
Scheme objects as their values.
The underlying C type will be described in the next chapter.
For the semantics of the non-standard Scheme primitives defined
by Elk refer to the Reference Manual for the interpreter.
.\" ---------------------------------------------------------------------------
.K2 "Portable alloca()"
.Rf ch-alloca \*(SN
.PP
Elk provides a portable variant of @[.\f2alloca()\fP] as a set of macros
that can be used by extensions and applications.
\f2alloca()\fP, which is supported by most modern UNIX systems
and C compilers, allocates memory in the caller's stack frame;
the memory is automatically released when the function returns.
Elk simulates this functionality on the (rare) platforms where
\f2alloca()\fP is not available.
.PP
To allocate memory, the macro @[.\f2Alloca()\fP] is called with
a variable to which the newly allocated memory is assigned,
the type of that variable, and the number of bytes that are
requested.
The macro @[.\f2Alloca_End\fP] must be called (without an
argument list) before returning from a function or block that uses
@[.\f2Alloca()\fP]; this macro is empty on those platforms
that support the ordinary \f2alloca()\fP.
Finally, a call to the macro @[.\f2Alloca_Begin\fP] must be placed
in the function's declarations.
\f2Alloca()\fP usually is more efficient than \f2malloc()\fP and
\f2free()\fP, and the memory need not be freed when the function
is left prematurely because of an interrupt or by calling
a @[.continuation].
.LP
As an example, here is the skeleton of a function that is called
with a filename prefix and a suffix, concatenates them (separated
by a period), and opens the resulting file:
.Es
int some_function(char *prefix, char *suffix) {
    char *name;
    int len, fd;
    Alloca_Begin;
.El
    len = strlen(prefix) + 1 + strlen(suffix) + 1;
    Alloca(name, char*, len);
    sprintf(name, "%s.%s", prefix, suffix);
    fd = open(name, ...);
    ...
    Alloca_End;
}
.Ee
.\" ---------------------------------------------------------------------------
.K2 "Other Useful Macros and Functions"
.PP
The preprocessor symbols @[.ELK_MAJOR] and @[.ELK_MINOR] expand to
the major and minor version number of the current release of Elk.
They did not exist in versions older than Elk \*(Vs.
.PP
@[.\f2index()\fP], @[.\f2bcopy()\fP], @[.\f2bcmp()\fP], and
@[.\f2bzero()\fP] are defined as suitable macros on systems that do not
have them in their C library; they may be used by source files that
include ``scheme.h'', regardless of the actual platform.
.LP
Code linked with Elk may use the two functions
.Es
@[.=Safe_Malloc()]@[.=Safe_Realloc()]
char *Safe_Malloc(unsigned size);
char *Safe_Realloc(char *old_pointer, unsigned size);
.Ee
as alternatives to \f2malloc()\fP and \f2realloc()\fP.
If the request for memory cannot be satisfied, the standard Elk error
handler is called with a suitable error message.
.\" ---------------------------------------------------------------------------
.K1 "The Anatomy of Scheme Objects"
.Rf ch-anatomy \*(SN
.PP
All Scheme objects, regarless of their Scheme type, are represented
as instances of the type @[.\f2Object\fP] in C.
\f2Object\fP is implemented as a small C \f2struct\fP in newer Elk
releases and was an integral type earlier.
However, code using Elk should not assume a specific representation,
as it may change again in future revisions.
An \f2Object\fP consists of three components:
.Rs
.IP \(bu
the type of the corresponding Scheme object as a small integer
(the @[.``type field''] or @[.``tag field'']),
.IP \(bu
the contents of the object, either directly (for small objects) or
as a pointer into the Scheme @[.heap] (the @[.``pointer field'']),
.IP \(bu
a @[.``const bit''] which, if set, indicates that the object is read-only
and cannot be modified by destructive Scheme primitives.
.Re
.PP
Elk defines a few macros to retrieve and modify the fields
of an \f2Object\fP independent of its representation:
.Es
@[.=TYPE()]@[.=POINTER()]@[.=ISCONST()]@[.=SETCONST()]@[.=SET()]
TYPE(obj)          ISCONST(obj)       SET(obj,t,ptr)
POINTER(obj)       SETCONST(obj)
.Ee
.PP
\f2TYPE()\fP returns the contents of the type field of an \f2Object\fP;
\f2POINTER()\fP returns the contents of the pointer field as an
\f2unsigned long\fP (different macros are provided for types which
have their values stored directly in the \f2Object\fP rather than
in the heap);
\f2ISCONST()\fP returns the value of the const bit;
and \f2SETCONST()\fP sets the const bit to 1 (it cannot be cleared
once it has been set).
\f2ISCONST()\fP and \f2SETCONST()\fP may only be applied to \f2Objects\fP
that have their value stored on the heap (such as vectors, strings, etc.);
all other types of Scheme objects are \f2ipso facto\fP read-only.
Another macro, \f2SET()\fP, can be used to set both the type and pointer
field of a new object.
.PP
Two objects can be compared by means of the macro
@[.=EQ()]
\f2EQ()\fP, which is also used as the basis for the Scheme
predicate @[.\f2eq?\fP]:
.Es
EQ(obj1,obj2)
.Ee
\f2EQ()\fP expands to a non-zero value if the type fields and the
pointer fields of the two objects are identical, else zero
(regardless of whether the pointer field really holds a pointer
or the object's actual value).
As \f2EQ()\fP may evaluate its arguments twice, it should not be
invoked with function calls or complex expressions.
.\" ---------------------------------------------------------------------------
.K2 "Type-specific Macros"
.PP
For each predefined Scheme type, there exists a preprocessor symbol
that expands to the integer value of that type (the contents of the
type field of members of the type).
The name of each such symbol is the name of the type with the
prefix ``T_'':
.Es
T_Boolean     T_Pair     T_Vector     \f1etc...\fP
.Ee
These symbols are typically used as case labels in switch-statements to
discriminate the possible types of a given object, or in if-statements
to check whether a Scheme object is of a given type:
.Es
if (TYPE(obj) == T_Vector)
	...
.Ee
In addition, each type defines a macro to extract the contents of
an object of that type and to convert it to the correct C type.
For example, the macro
.Es
@[.=CHAR()]
CHAR(obj)
.Ee
is used to fetch the character value (a C \f2int\fP) from members of
the Scheme type \f2character\fP, that is, from objects whose type field
contains the value \f2T_Character\fP.
Similarly, the macro
.Es
@[.=VECTOR()]
VECTOR(obj)
.Ee
gets the heap pointer conveyed in objects of the Scheme
type @[.\f2vector\fP].
For objects such as vectors, pairs, and procedures, the heap address is
coerced to a pointer to a C \f2struct\fP defining the layout of the
object.
There exists one structure type declaration for each such Scheme type;
their names are that of the type with ``S_'' prepended.
For example, \f2VECTOR()\fP returns a pointer to a structure with
the components \f2size\fP (the number of elements in the vector)
and \f2data\fP (the elements as an array of \f2Objects\fP).
These can be used from within C code like this:
.Es
int i, num = VECTOR(obj)->size;
.El
for (i = 0; i < num; i++)
	VECTOR(obj)->data[i] = ...;
.Ee
Similarly, the structure underlying the Scheme type @[.\f2pair\fP] is
defined as:
.Es
struct S_Pair { Object car, cdr; };
.Ee
and the macro \f2PAIR()\fP returns a (heap) pointer to a member of
the structure \f2S_Pair\fP.
Macros such as \f2VECTOR()\fP and \f2PAIR()\fP just convert the contents
of the pointer field to a pointer of the correct type:
.Es
#define VECTOR(obj)   ((struct S_Vector *)POINTER(obj))
#define PAIR(obj)     ((struct S_Pair   *)POINTER(obj))
.Ee
.PP
Authors of Scheme extensions and Elk-based applications are
encouraged to follow these conventions in their code and,
for each new type \f2xyz\fP, store the new type value
(which is allocated by the interpreter when the type is registered)
in a variable \f2T_Xyz\fP, and define a structure or class
\f2S_Xyz\fP, and a macro \f2XYZ()\fP that makes a pointer
to this structure from a member of the type.
Capitalization may vary according to personal preference.
.\" ---------------------------------------------------------------------------
.K1 "Defining New Scheme Primitives"
@[.=Scheme primitives]
.Rf ch-defprim \*(SN
.PP
In Elk, there exists a one-to-one relationship between Scheme
primitives and C functions:
each Scheme primitive\*-whether predefined or user-defined\*-is
implemented by a corresponding C function.
This includes @[.special forms], which are treated as a special kind
of primitives in Elk.
Extensions and applications use the function @[.\f2Define_Primitive()\fP]
to register a new Scheme primitive with the interpreter, supplying
its name and the C function that implements it.
In case of dynamically loadable extensions or application modules,
the calls to \f2Define_Primitive()\fP are placed in the
@[.extension initialization function]s that are called automatically
as the object file is loaded.
\f2Define_Primitive()\fP is declared as
.Es
void Define_Primitive((Object (*func)()), const char *name,
                      int minargs, int maxargs,
                      enum discipline disc);
.Ee
The arguments are:
.Rs
.IP \f2func\fP
a pointer to the C function implementing the new primitive;
.IP \f2name\fP
the name of the primitive as a null-terminated C string;
.IP \f2minargs\fP
the minimum number of arguments accepted by the primitive;
.IP \f2maxargs\fP
the maximum number of arguments (identical to \f2minargs\fP in most cases);
.IP \f2disc\fP
the @[.\f2calling discipline\fP] (usually \f2EVAL\fP).
.Re
.PP
\f2Define_Primitive()\fP creates a Scheme variable of the specified
name in the current (i.\|e.\& the caller's) lexical environment
and binds it to the newly created procedure.
Each C function that implements a primitive has a return type
of \f2Object\fP and, for a calling discipline of \f2EVAL\fP, zero
or more arguments of type \f2Object\fP which are bound to
the evaluated arguments passed to the Scheme primitive when
it is called.
The calling discipline must be one of the following:
.Rs
.IP \f2EVAL\fP\0\0
@[.=EVAL]
The primitive expects a fixed number of arguments; \f2minargs\fP
and \f2maxargs\fP must be identical\**.
.FS
Because of a limitation in the C language, primitives of type \f2EVAL\fP
can only have a fixed maximum number of arguments (currently 10).
If more arguments are required, \f2VARARGS\fP must be used instead.
.FE
.IP \f2VARARGS\fP
@[.=VARARGS]
The primitive has a variable number of arguments, and the
underlying C function is called with an argument count and
an array of arguments.
Defining primitives with a variable number of arguments will
explained in more detail in section @(ch-varargs).
.IP \f2NOEVAL\fP
@[.=NOEVAL]
The arguments are passed as a Scheme list of unevaluated objects\*-a
single argument of the type \f2Object\fP.
Primitives using this discipline will then use \f2Eval()\fP
as described in section @(ch-funcall) to evaluate some or all
of the arguments.
\f2NOEVAL\fP is only rarely used (with the exception of the built-in
@[.special forms] of Elk); extensions and applications mostly use macros as a
more convenient way to defined new syntactical forms.
.Re
.LP
Figure @(defprim) shows a simple example for defining a new
Scheme primitive.
.Fs
#include "scheme.h"
.El
Object p_vector_reverse(Object vec) {
	Object tmp, *s, *t;
.El
	Check_Type(vec, T_Vector);
	for (s = VECTOR(vec)->data, t = s+VECTOR(vec)->size; --t > s; s++)
		tmp = *s, *s = *t, *t = tmp;
	return vec;
}
.El
void elk_init_vector(void) {
	Define_Primitive(p_vector_reverse, "vector-reverse!", 1, 1, EVAL);
}
.Fc "Defining a new Scheme Primitive"
.Fe defprim
.PP
The primitive @[.\f2vector-reverse!\fP] defined by the example extension
reverses the elements of a Scheme @[.vector] in place and returns
its argument (note the final exclamation mark indicating the
destructive operation).
@[.\f2Check_Type()\fP] is a simple macro that compares the type field
of the first argument (an \f2Object\fP) with the second argument
and signals and error if they do not match.
This macro is used primarily for type-checking the arguments to
Scheme primitives.
A call to the macro @[.\f2Check_Mutable()\fP] with the vector
as an argument
could have been inserted before the loop to check whether the vector
is read-only and to automatically raise an error if this is the case.
The example code forms a complete extension including an
@[.extension initialization function] and could be linked with
the interpreter, or loaded dynamically into the interpreter as
follows:
.Es
\f6%\fP cc \-c \-I/usr/elk/include vec.c; makedl vec.o vec.o
\f6%\fP scheme
\f6>\fP (load 'vec.o)
\f6>\fP (define v '#(hello word))
\f6v
>\fP (vector-reverse! v)
\f6#(world hello)
>\fP v
\f6#(world hello)
>\fP
.Ee
.\" ---------------------------------------------------------------------------
.K2 "Making Objects Known to the Garbage Collector"
.Rf ch-gc \*(SN
@[.=garbage collector]
.PP
Consider the non-destructive version of the primitive
@[.\f2vector-reverse\fP] shown in Figure @(vecrev1), which returns a new
vector instead of altering the contents of the original vector.
.Fs
Object p_vector_reverse(Object vec) {
	Object ret;
	int i, j;
.El
	Check_Type(vec, T_Vector);
	ret = Make_Vector(VECTOR(vec)->size, False);
	for (i = 0, j = VECTOR(vec)->size; --j >= 0; i++)
		VECTOR(ret)->data[i] = VECTOR(vec)->data[j];
	return ret;
}
.Fc "Non-destructive Scheme primitive \f2vector-reverse\fP"
.Fe vecrev1
.PP
The code in Figure @(vecrev1) is identical to that shown in Figure
@(defprim), except that a new vector is allocated, filled with
the contents of the original vector in reverse order, and returned
as the result of the primitive.
@[.\f2Make_Vector()\fP] is declared by Elk:
.Es
Object Make_Vector(int size, Object fill);
.Ee
\f2size\fP is the length of the vector, and all elements are initialized
to the Scheme object \f2fill\fP.
In the example, the predefined global variable @[.\f2False\fP] is
used as the \f2fill\fP object; it holds the boolean Scheme constant #f
(any \f2Object\fP could have been used here).
.PP
Although the C function may look right, there is a problem when
it comes to garbage collection.
To understand the problem and its solution, it may be helpful to have a
brief look at how the garbage collector\**
.FS
Elk actually employs two garbage collectors, one based on the
traditional stop-and-copy strategy, and a generational, incremental
garbage collector which is less disruptive but not supported
on all platforms.
.FE
works (the following description presents a simplified view; the real
algorithm is more complex).
In Elk, a @[.garbage collection] is triggered automatically whenever
a request for heap space cannot be satisfied because
the @[.heap] is full, or explicitly by calling the primitive
@[.\f2collect\fP] from within Scheme code.
The garbage collector traces all ``live'' objects starting with
a known @[.\f2root set\fP] of pointers to reachable objects
(basically the interpreter's global lexical environment and its
symbol table).
Following these pointers, all accessible Scheme objects are located
and copied to a new heap space in memory (``forwarded''), thereby
compacting the heap.
Whenever an object is relocated in memory during garbage collection,
the contents of the @[.pointer field] of the corresponding C \f2Object\fP
is updated to point to the new location.
After that, any constituent objects (e.\|g.\& the elements of a
vector) are forwarded in the same way.
.PP
As live objects are relocated in memory, \f2all\fP pointers to an
object need to be updated properly when that object is forwarded
during garbage collection.
If a pointer to a live object were not in the root set (that is,
not reachable by the garbage collector), the object would either
become garbage erroneously during the next garbage collection, or,
if it had been reached through some other pointer, the original
pointer would now point to an invalid location.\**
.FS
The problem of managing an ``exact root set'' can be avoided by
a technique called \f2conservative\fP garbage collection.
A conservative garbage collector treats the data segment, stack,
and registers of the running program as \f2ambiguous roots\fP.
If the set of ambiguous roots is a superset of the \f2actual\fP roots,
then a pointer that looks like a heap pointer can safely be considered
as pointing to an accessible object that cannot be reclaimed.
At the time Elk was designed, conservative GC was still in its
infancy and sufficient experience did not exist.
For this reason, and because of the implied risks on certain
machine architectures, the inherent portability problems, and
the inability to precisely determine the actual memory utilization,
a traditional GC strategy was chosen for Elk.
.FE
This is exactly what happens in the example shown in Figure @(vecrev1).
.PP
The call to \f2Make_Vector()\fP in the example triggers a garbage
collection if the heap is too full to satisfy the request for heap
space.
As the \f2Object\fP pointer stored in the argument \f2vec\fP
is invisible to the garbage collector, its pointer field cannot
be updated when the vector to which it points is forwarded during
the garbage collection started inside \f2Make_Vector()\fP.
As a result, all further references to \f2VECTOR(vec)\fP will
return an invalid address and may cause the program to crash
(immediately or, worse, at a later point).
The solution is simple: the primitive just needs to add \f2vec\fP
to the set of initial pointers used by the garbage collector.
This is done by inserting the line
.Es
GC_Link(vec);
.Ee
at the beginning of the function before the call to \f2Make_Vector()\fP.
@[.\f2GC_Link()\fP] is a macro.
Another macro, @[.\f2GC_Unlink\fP], must be called later (e.\|g.\& at
the end of the function) without an argument list to remove the object
from the root set again.
In addition, a call to @[.\f2GC_Node\fP] (again without an argument
list) must be placed in the declarations at the beginning of
the enclosing function or block.
Figure @(vecrev2) shows the revised, correct code.
.Fs
Object p_vector_reverse(Object vec) {
	Object ret;
	int i, j;
	GC_Node;
.El
	GC_Link(vec);
	Check_Type(vec, T_Vector);
	ret = Make_Vector(VECTOR(vec)->size, False);
	for (i = 0, j = VECTOR(vec)->size; --j >= 0; i++)
		VECTOR(ret)->data[i] = VECTOR(vec)->data[j];
	GC_Unlink;
	return ret;
}
.Fc "Non-destructive Scheme primitive \f2vector-reverse\fP, corrected version"
.Fe vecrev2
.PP
Appendix A lists the C functions which can trigger a garbage collection.
Any @[.local variable] or argument of type \f2Object\fP must be protected
in the manner shown above if one of these functions is called during
its lifetime.
This may sound more burdensome than it really is, because most of
the ``dangerous'' functions are rarely or never used from within
C/C++ extensions or applications in practice.
Most primitives that require calls to \f2GC_Link()\fP use some function
that creates a new Scheme object, such as \f2Make_Vector()\fP in
the example above.
.PP
To simplify GC protection of more than a single argument or variable,
additional macros @[.\f2GC_Link2()\fP], @[.\f2GC_Link3()\fP], and
so on up to \f2GC_Link7()\fP are provided.
Each of these can be called with as many arguments of type \f2Object\fP
as is indicated by the digit (separate macros are required, because
macros with a variable number of arguments cannot be defined in C).
A corresponding macro @[.\f2GC_Node2\fP], @[.\f2GC_Node3\fP], and so on,
must be placed in the declarations.
Different \f2GC_Link*()\fP calls cannot be mixed.
All @[.local variable]s passed to one of the macros must have been
initialized.
GC protection is not required for ``pointer-less'' objects such as
booleans and small integers, and for the arguments of primitives
with a variable number of arguments (as described in section @(ch-varargs)).
Section @(ch-gcglobal) will describe how global (external)
\f2Object\fP variables can be added to the root set.
.PP
Here is how the implementation of the primitive @[.\f2cons\fP] uses
\f2GC_Link2()\fP to protect its arguments (the @[.car] and the @[.cdr] of
the new pair):
.Es
Object P_Cons(Object car, Object cdr) {
	Object new_pair;
	GC_Node2;
.El
	GC_Link2(car, cdr);
	new_pair = \f2allocate heap space and initialize object\fP;
	GC_Unlink;
	return new_pair;
}
.Ee
.PP
There are a few pitfalls to be aware of when using ``dangerous''
functions from within your C/C++ code.
For example, consider this code fragment which fills a Scheme
vector with the program's environment strings that are available
through the null-terminated string array \f2environ[]\fP:
.Es
Object vec = \f2new vector of the right size\fP;
int i;
GC_Node;
.El
GC_Link(vec);
for (i = 0; environ[i] != 0; i++)
	VECTOR(vec)->data[i] = Make_String(environ[i], strlen(environ[i]));
.Ee
(\f2Make_String()\fP creates and initializes a new Scheme string.)
The body of the for-loop contains a subtle bug: depending on the
compiler used, the left hand side of the assignment (the expression
involving \f2vec\fP) may be evaluated before @[.\f2Make_String()\fP]
is invoked.
As a result, a copy of the contents of \f2vec\fP might be, for instance,
stored in a register before a garbage collection is triggered while
evaluating the right hand side of the assignment.
The garbage collector would then move the vector object in memory,
updating the\*-properly GC-protected\*-variable \f2vec\fP, but not the
temporary copy in the register, which is now a dangling reference.
To avoid this, the loop must be modified along these lines:
.Es
for (i = 0; environ[i]; i++) {
	Object temp = Make_String(environ[i], strlen(environ[i]));
	VECTOR(vec)->data[i] = temp;
}
.Ee
A related pitfall to watch out for is exemplified by this code
fragment:
.Es
Object obj;
\&...
GC_Link(obj);
\&...
some_function(obj, P_Cons(car, cdr));
.Ee
Here, the call to @[.\f2P_Cons()\fP]\*-just like \f2Make_String()\fP
above\*-can trigger a garbage collection.
Depending on the C compiler, the properly GC-protected object
pointer \f2obj\fP may be pushed on the argument stack before \f2P_Cons()\fP
is invoked, as the order in which function arguments\*-just like the
operands of the assignment operator\*-are evaluated is undefined in the
C language.
In this case, if a garbage collection takes place and the heap object
to which \f2obj\fP points is moved, \f2obj\fP will be updated
properly, but the copy on the stack will not.
Again, the problem can be avoided easily by assigning the result of the
nested function call to a temporary \f2Object\fP variable and
use this variable in the enclosing function call:
.Es
temp = P_Cons(car, cdr);
some_function(obj, temp);
.Ee
.\" ---------------------------------------------------------------------------
.K2 "Primitives with Variable-Length Argument Lists"
.Rf ch-varargs \*(SN
.PP
Primitives with a variable number of arguments are registered with
the interpreter by calling @[.\f2Define_Primitive()\fP] with
the @[.calling discipline] @[.\f2VARARGS\fP] and with different
values for \f2minargs\fP and \f2maxargs\fP.
The special symbol @[.\f2MANY\fP] can be given as the maximum number
of arguments to indicate that there is no upper limit on the
primitive's number of actual arguments.
The C/C++ function implementing a primitive with a variable number
of arguments is called with two arguments: an integer count
that specifies the number of actual arguments, and the
Scheme arguments as an array of \f2Objects\fP (that is, a pointer
to \f2Object\fP).
The objects passed as the argument vector of \f2VARARGS\fP primitives
are already registered with the garbage collector; calls to
\f2GC_Link()\fP are not required.
As an example for a primitive with an arbitrary number of arguments,
here is the definition of a simplified variant of @[.\f2append!\fP]
(which does not handle empty lists):
.Es
Object p_append_set (int argc, Object *argv); {
	int i;
.El
	for (i = 0; i < argc-1; i++)
		(void)P_Set_Cdr (P_Last_Pair (argv[i]), argv[i+1]);
	return *argv;
}
.Ee
The corresponding call to \f2Define_Primitive()\fP would read:
.Es
Define_Primitive(p_append_set, "append!", 0, MANY, VARARGS);
.Ee
.PP
Besides implementing primitives with an indefinite maximum number
of arguments, the \f2VARARGS\fP discipline is frequently used for
primitives with an optional argument.
For example, a primitive encapsulating the UNIX \f2open()\fP system
call, which has two fixed arguments (filename, flags) and an optional
third argument (the mode for newly created files, i.\|e.\& calls with
the flag \f2O_CREAT\fP), could be defined as follows:
.Es
Object p_unix_open(int argc, Object *argv) {
	char *name = get_file_name(argv[0]);
	int flags = get_flags(argv[1]);
	mode_t mode;
.El
	if (flags & O_CREAT) {
		if (argc < 3)
			\f2error--too few arguments\fP
		mode = get_mode(argv[2]);
		...
.Ee
The call to \f2Define_Primitive()\fP could then be written as:
.Es
Define_Primitive(p_unix_open, "unix-open", 2, 3, VARARGS);
.Ee
.\" ---------------------------------------------------------------------------
.K1 "Predefined Scheme Types"
.Rf ch-types \*(SN
.PP
This chapter introduces the Scheme types predefined by Elk.
It begins with the ``pointer-less'' types such as boolean, whose
values are stored directly in the pointer field of an \f2Object\fP;
followed by the types whose members are C \f2structs\fP that
reside on the Scheme heap.
.\" ---------------------------------------------------------------------------
.K2 "Booleans (T_Boolean)"
@[.=T_Boolean]
.PP
\f2Objects\fP of type \f2T_Boolean\fP can hold the values #t and #f.
Two \f2Objects\fP initialized to #t and #f, respectively, are
available as the external C variables \f2True\fP and \f2False\fP.
The macro
.Es
@[.=Truep()]
Truep(obj)
.Ee
can be used to check whether an arbitrary Scheme object is regarded
as true.
Use of \f2Truep()\fP is not necessarily equivalent to
.Es
!EQ(obj,False)
.Ee
because the empty list may count as false in addition to #f if
backwards compatibility to older Scheme language versions has
been enabled.
\f2Truep()\fP may evaluate its argument twice and should therefore
not be invoked with a function call or a complex expression.
.LP
The two functions
.Es
@[.=Eqv()]@[.=Equal()]
int Eqv(Object, Object);
int Equal(Object, Object);
.Ee
are identical to the primitives \f2P_Eqv()\fP and \f2P_Equal()\fP,
except that they return a C integer rather than a Scheme boolean and
therefore can be used more conveniently in C/C++.
.\" ---------------------------------------------------------------------------
.K2 "Characters (T_Character)"
@[.=T_Character]
.PP
The character value stored in an \f2Object\fP of type \f2T_Character\fP
can be obtained by the macro
.Es
@[.=CHAR()]
CHAR(char_obj)
.Ee
as a non-negative \f2int\fP.
A new character object is created by calling the function
.Es
@[.=Make_Char()]
Object Make_Char(int c);
.Ee
The predefined external C variable @[.\f2Newline\fP] holds the
newline character as a Scheme \f2Object\fP.
.\" ---------------------------------------------------------------------------
.K2 "Empty List (T_Null)"
@[.=T_Null]
.PP
The type \f2T_Null\fP has exactly one member\*-the empty list;
hence all \f2Objects\fP of this type are identical.
The empty list is available as the external C variable @[.\f2Null\fP].
This variable is often used to initialize \f2Objects\fP that will
be assigned their real values later, for example, as the fill
element for newly created vectors or to initialize \f2Objects\fP
in order to \f2GC_Link()\fP them.
A macro \f2Nullp()\fP is provided as a shorthand for checking if an
\f2Object\fP is the empty list:
.Es
@[.=Nullp()]
#define Nullp(obj)  (TYPE(obj) == T_Null)
.Ee
This macro is used frequently in the termination condition of
for-loops that scan a Scheme list:
.Es
Object tail;
\&...
for (tail = some_list; !Nullp(tail); tail = Cdr(tail))
	process_element(Car(tail));
.Ee
(\f2Car()\fP and \f2Cdr()\fP essentially are shorthands for
\f2P_Car()\fP and \f2P_Cdr()\fP and will be revisited in
the section on pairs).
.\" ---------------------------------------------------------------------------
.K2 "End of File (T_End_Of_File)"
@[.=T_End_Of_File]
.PP
The type \f2T_End_Of_File\fP has one member\*-the
@[.end-of-file object]\*-and is only rarely used from within
user-supplied C/C++ code.
The external C variable @[.\f2Eof\fP] is initialized to the
end-of-file object.
.\" ---------------------------------------------------------------------------
.K2 "Integers (T_Fixnum and T_Bignum)"
@[.=T_Fixnum]@[.=T_Bignum]
.PP
Integers come in two flavors: @[.\f2fixnums\fP] and @[.\f2bignums\fP].
The former have their value stored directly in the pointer field and
are wide enough to hold most C \f2ints\fP.
Bignums can hold integers of arbitrary size and are stored in the heap.
Two macros are provided to test whether a given signed (or unsigned,
respectively) integer fits into a fixnum:
.Es
@[.=FIXNUM_FITS()]@[.=UFIXNUM_FITS()]
FIXNUM_FITS(integer)
UFIXNUM_FITS(unsigned_integer)
.Ee
The former always returns 1 in Elk \*(Vs, but the range of integer
values that can be represented as a fixnum may be restricted in
future revisions.
It is guaranteed, however, that at least two bits less than the
machine's word size will be available for fixnums in future
versions of Elk.
.LP
The value stored in a fixnum can be obtained as a C \f2int\fP by
calling the macro
.Es
@[.=FIXNUM()]
FIXNUM(fixnum_obj)
.Ee
A macro
.Es
@[.=Check_Integer()]
Check_Integer(obj)
.Ee
can be used as a shorthand for checking whether an \f2Object\fP is
a fixnum or a bignum and raising an error otherwise.
.LP
The following functions are provided to convert C integers to
Scheme integers:
.Es
@[.=Make_Integer()]@[.=Make_Unsigned()]
@[.=Make_Long()]@[.=Make_Unsigned_Long()]
Object Make_Integer(int);
Object Make_Unsigned(unsigned);
Object Make_Long(long);
Object Make_Unsigned_Long(unsigned long);
.Ee
\f2Make_Integer()\fP returns a fixnum object if \f2FIXNUM_FITS()\fP
returns true for the argument, otherwise a bignum.
Likewise, \f2Make_Long()\fP usually returns a fixnum but may have to resort
to bignums on architectures where a C \f2long\fP is wider than an \f2int\fP.
\f2Make_Unsigned()\fP returns a bignum if the specified integer
is larger than the largest positive \f2int\fP that fits into a fixnum
(\f2UFIXNUM_FITS()\fP returns zero in this case).
Another set of functions convert a Scheme number to a C integer:
.Es
@[.=Get_Integer()]@[.=Get_Exact_Integer()]
int Get_Integer(Object);
int Get_Exact_Integer(Object);
.El
@[.=Get_Unsigned()]@[.=Get_Exact_Unsigned()]
unsigned Get_Unsigned(Object);
unsigned Get_Exact_Unsigned(Object);
.El
@[.=Get_Long()]@[.=Get_Exact_Long()]
long Get_Long(Object);
long Get_Exact_Long(Object);
.El
@[.=Get_Unsigned_Long()]@[.=Get_Exact_Unsigned_Long()]
unsigned long Get_Unsigned_Long(Object);
unsigned long Get_Exact_Unsigned_Long(Object);
.Ee
These functions signal an error if one of the following
conditions is true:
.Rs
.IP \(bu
the argument is neither a fixnum, nor a bignum, nor a flonum (real
number) with a fractional part of zero (more about @[.flonums] in the
next section);
.IP \(bu
the function is one of the ``unsigned'' variants and the argument is
a negative number;
.IP \(bu
the argument is a bignum too large for the respective return type;
.IP \(bu
the function is one of the ``exact'' variants and the argument
is neither a fixnum nor a bignum;
.IP \(bu
the argument is a flonum that cannot be coerced to the respective
return type.
.Re
.LP
As all of the above functions include suitable type-checks, primitives
receiving integer arguments can be written in a simple and
straightforward way.
For example, a primitive encapsulating the UNIX \f2dup\fP system
call (which returns an integer file descriptor pointing to the
same file as the original one) can be written as:
.Es
Object p_unix_dup(Object fd) {
    return Make_Integer(dup(Get_Exact_Unsigned(fd)));
.Ee
Note that if \f2Get_Unsigned()\fP (or \f2Get_Integer()\fP) had been
used here in place of the ``exact'' conversion function, it would be
possible to write expressions such as:
.Es
(define fd (unix-dup (truncate 1.2)))
.Ee
.\" ---------------------------------------------------------------------------
.K2 "Floating Point Numbers (T_Flonum)"
@[.=T_Flonum]
.PP
@[.=real numbers]
Real and @[.inexact number]s are represented as \f2Objects\fP of type
\f2T_Flonum\fP.
Each such object holds a pointer to a structure on the heap with
a component \f2val\fP of type \f2double\fP, so that the expression
.Es
@[.=FLONUM()]
FLONUM(flonum_obj)->val
.Ee
can be used to obtain the \f2double\fP value.
To convert a Scheme number to a \f2double\fP regardless of its
type, the more general function
.Es
@[.=Get_Double()]
double Get_Double(Object);
.Ee
can be used.
It raises an error if the argument is not a fixnum, bignum, or flonum,
or if it is a bignum too large to fit into a \f2double\fP.
.LP
The functions
.Es
@[.=Make_Flonum()]@[.=Make_Reduced_Flonum()]
Object Make_Flonum(double);
Object Make_Reduced_Flonum(double);
.Ee
convert a C \f2double\fP to a flonum; the latter returns a fixnum
if the \f2double\fP is small enough to fit into a fixnum and
has a fractional part of zero.
The macro
.Es
@[.=Check_Number()]
Check_Number(obj)
.Ee
checks whether the given \f2Object\fP is a number (that is, a fixnum,
bignum, or flonum in the current revision of Elk) and raises an
error otherwise.
.\" ---------------------------------------------------------------------------
.K2 "Pairs (T_Pair)"
@[.=T_Pair]
.PP
Pairs have two components of type \f2Object\fP, the @[.car] and the @[.cdr],
that can be accessed as:
.Es
@[.=PAIR()]
PAIR(pair_obj)->car
PAIR(pair_obj)->cdr
.Ee
Two macros @[.\f2Car()\fP] and @[.\f2Cdr()\fP] are provided as shorthands
for these expressions, and another macro @[.\f2Cons()\fP] can be
used in place of @[.\f2P_Cons()\fP] to create a new pair.
The macro
.Es
@[.=Check_List()]
Check_List(obj)
.Ee
checks whether the specified \f2Object\fP is either a pair or
the empty list and signals an error otherwise.
The predefined function
.Es
@[.=Fast_Length()]
int Fast_Length(Object list);
.Ee
can be used to compute the length of the given Scheme list.
This function is more efficient than the primitive \f2P_Length()\fP,
because it neither checks the type of the argument nor whether
the given list is proper, and the result need not be converted
to a Scheme number.
The function
.Es
@[.=Copy_List()]
Object Copy_List(Object list);
.Ee
returns a copy of the specified list (including all its sublists).
.PP
As explained in section @(ch-gc), care must be taken when mixing
calls to these macros, because \f2Cons()\fP may trigger a garbage
collection:
an expression such as
.Es
Car(x) = Cons(y, z);
.Ee
is wrong, even if \f2x\fP is properly ``GC_Linked'', and should be
replaced by
.Es
tmp = Cons(x, y);
Car(x) = tmp;
.Ee
or a similar sequence.
.\" ---------------------------------------------------------------------------
.K2 "Symbols (T_Symbol)"
@[.=T_Symbol]
.PP
\f2Objects\fP of type \f2T_Symbol\fP have one public component\*-the
symbol's name as a Scheme string (that is, an \f2Object\fP of type
\f2T_String\fP):
.Es
@[.=SYMBOL]
SYMBOL(symbol_obj)->name
.Ee
A new symbol can be created by calling one of the functions
.Es
@[.=Intern()]@[.=CI_Intern()]
Object Intern(const char *);
Object CI_Intern(const char *);
.Ee
with the new symbol's name as the argument.
\f2CI_Intern()\fP is the case-insensitive variant of \f2Intern()\fP;
it maps all upper case characters to lower case.
\f2EQ()\fP yields true for all \f2Objects\fP returned by calls
to \f2Intern()\fP with strings with the same contents (or calls
to \f2CI_Intern()\fP with strings that are identical after
case conversion).
This is the main property that distinguishes symbols from strings
in Scheme.
.PP
A symbol that is used by more than one function can be stored in
a global variable to save calls to \f2Intern()\fP.
This can be done using the convenience function
.Es
@[.=Define_Symbol()]
void Define_Symbol(Object *var, const char *name);
.Ee
\f2Define_Symbol()\fP is called with the address of a variable
where the newly-interned symbol is stored and the name of
the symbol to be handed to \f2Intern()\fP.
The function adds the new symbol to the garbage collector's
@[.root set] to make it reachable (as described in section @(ch-gcglobal).
Example:
.Es
static Object sym_else;
\&...
void elk_init_example(void) {
	Define_Symbol(&sym_else, "else");
	...
}
.Ee
.\" ---------------------------------------------------------------------------
.K3 "The Non-Printing Symbol"
.PP
By convention, Scheme primitives that do not have a useful return value
(for example the output primitives) return the @[.``non-printing symbol'']
in Elk.
The name of this symbol consists of the empty string;
it does not produce any output when it is printed, for example,
by the toplevel read-eval-print loop.
In Scheme code, the non-printing symbol can be generated by using
the reader syntax ``#v'' or by calling \f2string\(mi>symbol\fP with
the empty string.
On the C language level, the non-printing symbol is available as
the external variable @[.\f2Void\fP], so that primitives lacking
a useful return value can use
.Es
return Void;
.Ee
.\" ---------------------------------------------------------------------------
.K2 "Strings (T_String)"
@[.=T_String]
.PP
\f2Objects\fP of type string have two components\*-the length and the
contents of the string as a pointer to \f2char\fP:
.Es
STRING(string_obj)->size
STRING(string_obj)->data
.Ee
The \f2data\fP component is not null-terminated, as a string
itself may contain a null-byte as a valid character in Elk.
A Scheme string is created by calling the function
.Es
@[.=Make_String()]
Object Make_String(const char *init, int size);
.Ee
\f2size\fP is the length of the newly-created string.
\f2init\fP is either the null-pointer or a pointer to \f2size\fP
characters that are copied into the new Scheme string.
For example, the sequence
.Es
Object str;
\&...
str = Make_String(0, 100);
bzero(STRING(str)->data, 100);
.Ee
generates a string holding 100 null-bytes.
.PP
Most primitives that receive a Scheme string as one of their arguments
pass the string's contents to a C function (for example a C library function)
that expects an ordinary, null-terminated C string.
For this purpose Elk provides a function
.Es
@[.=Get_String()]
char *Get_String(Object);
.Ee
that returns the contents of the Scheme string argument as a
null-terminated C string.
An error is raised if the argument is not a string.
\f2Get_String()\fP has to create a copy of the contents of the Scheme
string in order to append the null-character.
To avoid requiring the caller to provide and release space for the
copy, \f2Get_String()\fP operates on and returns @[.NUMSTRBUFS]
internal, cyclically reused buffers (the value of NUMSTRBUFS is 3
in Elk \*(Vs).
Consequently, no more than NUMSTRBUFS results of \f2Get_String()\fP
can be used simultaneously (which is rarely a problem in practice).
As an example, a Scheme primitive that calls the C library
function \f2getenv()\fP and returns #f on error can be written as
.Es
Object p_getenv(Object name) {
	char *ret = getenv(Get_String(name));
	return ret ? Make_String(ret, strlen(ret)) : False;
}
.Ee
.PP
If more strings are to be used simultaneously, the macro
@[.\f2Get_String_Stack()\fP] can be used instead.
It is called with the Scheme object and the name of a
variable of type ``char*'' to which the C string will be assigned.
\f2Get_String_Stack()\fP allocates space by means of @[.\f2Alloca()\fP]
(as explained in section @(ch-alloca)); hence a call to
@[.\f2Alloca_Begin\fP] must be placed in the declarations of the
enclosing function or block, and @[.\f2Alloca_End\fP] must be
called before returning from it.
.PP
An additional function @[.\f2Get_Strsym()\fP] and an additional
macro @[.\f2Get_Strsym_Stack()\fP] are provided by Elk; these
are identical to \f2Get_String()\fP and \f2Get_String_Stack()\fP,
respectively, except that the Scheme object may also be a symbol.
In this case, the symbol's name is taken as the string to
be converted.
.PP
As an example for the use of \f2Get_String_Stack()\fP, here is
a simple Scheme primitive \f2exec\fP that is called with the
name of a program and one more more arguments and passes them
to the \f2execv()\fP system call:
.Es
Object p_exec(int argc, Object *argv) {
	char **argp; int i;
	Alloca_Begin;
.El
	Alloca(argp, char**, argc*sizeof(char *));
	for (i = 1; i < argc; i++)
		Get_String_Stack(argv[i], argp[i-1]);
	argp[i-1] = 0;
	execv(Get_String(*argv), argp);   /* must not return */
	\f2error...\fP
}
.El
elk_init_example() {
	Define_Primitive(p_exec, "exec", 2, MANY, VARARGS);
}
.Ee
The primitive can be used as follows:
.Es
(exec "/bin/ls" "ls" "-l")
.Ee
\f2Get_String()\fP could not be used in this primitive, because
the number of string arguments may exceed the number of static
buffers maintained by \f2Get_String()\fP.
.\" ---------------------------------------------------------------------------
.K2 "Vectors (T_Vector)"
@[.=T_Vector]
.PP
The layout of \f2Objects\fP of type vector is identical to that
of strings, except that the \f2data\fP component is an array
of \f2Objects\fP.
A function @[.\f2Make_Vector()\fP] creates a new vector as has been
explained in section @(ch-gc) above.
.\" ---------------------------------------------------------------------------
.K2 "Ports (T_Port)"
@[.=T_Port]
.PP
The components of \f2Objects\fP of type \f2T_Port\fP are not
normally accessed directly from within C/C++ code, except for
.Es
PORT(port_obj)->closefun
.Ee
which is a pointer to a function receiving an argument of
type ``FILE*'' (for example, a pointer to \f2fclose()\fP),
provided that the port is a file port.
It is called automatically whenever the port is closed,
either because \f2close-input-port\fP or \f2close-output-port\fP
is applied to it or because the garbage collector has determined
that the port is no longer reachable.
.LP
A new file port is created by calling
.Es
@[.=Make_Port()]
Object Make_Port(int flags, FILE *f, Object name);
.Ee
with a first argument of either zero (output port),
\f2P_INPUT\fP (input port) or \f2P_BIDIR\fP (bidirectional port),
the file pointer, and the name of the file as a Scheme string.
The macros
.Es
@[.=Check_Input_Port()]@[.=Check_Output_Port()]
Check_Input_Port(obj)
Check_Output_Port(obj)
.Ee
check whether the specified port is open and is capable of
input (or output, respectively); an error is raised otherwise.
.PP
To arrange for a newly-created port to be closed automatically when it
becomes garbage, it must be passed to the function
\f2Register_Object()\fP as follows:
.Es
@[.=Register_Object()]@[.=Terminate_File()]
Register_Object(the_port, 0, Terminate_File, 0);
.Ee
\f2Register_Object()\fP will be described in section @(ch-term).
The current input and output port as well as ports pointing to the
program's initial standard input and output are available as four
external variables of type \f2Object\fP:
.Es
@[.=Curr_Input_Port]@[.=Curr_Output_Port]
@[.=Standard_Input_Port]@[.=Standard_Output_Port]
Curr_Input_Port      Standard_Input_Port
Curr_Output_Port     Standard_Output_Port
.Ee
The function
.Es
@[.=Reset_IO()]
void Reset_IO(int destructive_flag);
.Ee
clears any input queued at the current input port, then flushes
the current output port (if \f2destructive_flag\fP is zero)
or discards characters queued at the output port (if
\f2destructive_flag\fP is non-zero), and finally resets the
current input and current output port to their initial values
(the program's standard input and standard output).
This function is typically used in error situations to reset
the current ports to a defined state.
.PP
In addition to the standard Scheme primitives for output, extensions
and applications can use a function
.Es
@[.=Printf()]
void Printf(Object port, char *fmt, ...);
.Ee
to send output to a Scheme port using C \f2printf\fP.
The first argument to \f2Printf()\fP is the Scheme port to which
the output will be sent (it must be an output port); the remaining
arguments are that of the C library function \f2printf()\fP.
.LP
To output a Scheme object, the following function can be used
in addition to the usual primitives:
.Es
@[.=Print_Object()]
void Print_Object(Object obj, Object port, int raw_flag,
		  int print_depth, int print_length);
.Ee
The arguments to \f2Print_Object()\fP are identical to the arguments
of the ``print function'' that must be supplied for each user-defined
Scheme type (as described in section @(ch-deftype):
the \f2Object\fP to be printed, the output port, a flag indicating
that the object should be printed in human-readable form (\f2display\fP
sets the flag, \f2write\fP does not), and the ``print depth'' and
``print length'' for that operation.
For debugging purposes, the macro
.Es
@[.=Print()]
Print(obj);
.Ee
may be used to output an \f2Object\fP to the current output port.
.LP
A function
.Es
@[.=Load_Source_Port()]
void Load_Source_Port(Object port);
.Ee
can be used to load Scheme expressions from a file that has already
been opened as a Scheme port.
.\" ---------------------------------------------------------------------------
.K2 "Miscellaneous Types"
.PP
Other built-in Scheme types are lexical environments, primitive procedures,
compound procedures, macros, continuations (also called ``control points''
at a few places in Elk), and promises.
These types are not normally created or manipulated from within C or
C++ code.
If you are writing a specialized extension that depends on the
C representation of these types, refer to the declarations in the
public include file ``object.h'' (which is included automatically via
``scheme.h'').
.PP
Lexical environments are identical to pairs except that the type
is @[.\f2T_Environment\fP] rather than \f2T_Pair\fP.
The current environment and the initial (gobal) environment
are available as the external C variables
@[.\f2The_Environment\fP] and @[.\f2Global_Environment\fP].
The predefined type constants for primitives, compound procedures (the
results of evaluating lambda expressions), and macros are
@[.\f2T_Primitive\fP], @[.\f2T_Compound\fP], and @[.\f2T_Macro\fP],
respectively.
The function
.Es
@[.=Check_Procedure()]
void Check_Procedure(Object);
.Ee
checks whether the specified object is either a compound procedure
or a primitive procedure with a calling discipline different from
\f2NOEVAL\fP and raises an error otherwise.
The type constant for continuations is @[.\f2T_Control\fP].
``Promise'' is the type of object returned by the special form
\f2delay\fP; the corresponding type constant is named @[.\f2T_Promise\fP].
.\" ---------------------------------------------------------------------------
.K1 "Defining New Scheme Types"
.Rf ch-deftype \*(SN
.PP
A new, disjoint Scheme type is registered with Elk by calling the
function @[.\f2Define_Type()\fP], similar to \f2Define_Primitive()\fP
for new primitives.
Making a new type known to Elk involves passing it information about
the underlying C/C++ representation of the type and a number of C or
C++ functions that are ``called back'' by the interpreter in
various situations to pass control to the code that implements
the type.
The prototype of \f2Define_Type()\fP is:
.Es
int Define_Type(int zero, const char *name,
	int (*size)(Object), int const_size,
	int (*eqv)(Object, Object),
	int (*equal)(Object, Object),
	int (*print)(Object, Object, int, int, int),
	int (*visit)(Object*, int (*)(Object*)));
.Ee
The arguments to \f2Define_Primitive()\fP are in detail:
.Rs
.IP \f2zero\fP 1
The first argument must be zero (in early versions of Elk it could be
used to request a fixed, predefined type number for the new type);
.IP \f2name\fP 1
The name of the new type.
.IP "\f2size, const_size\fP" 1
The size of the corresponding C type (usually a \f2struct\fP) in bytes,
given as one of two, mutually-exclusive arguments:
\f2size\fP, a pointer to a function called by the interpreter to determine
the size of an object (for types whose individual members are of different
sizes, such as the \f2vector\fP type);
and \f2const_size\fP, the size as a constant (for all other types).
A null-pointer is given for \f2const_size\fP if \f2size\fP is to
be used instead.
.IP "\f2eqv, equal\fP" 1
Pointers to (callback) functions that are invoked by the
interpreter whenever the Scheme predicate \f2equal?\&\fP, or \f2eqv?\&\fP
respectively, is applied to members of the newly defined type.
As an application-defined type is opaque from the interpreter's
point of view, the equality predicates have to be supplied by
the application or extension.
Each of these (boolean) functions is passed two objects of the new type
as arguments when called back.
.IP \f2print\fP 1
A pointer to a function that is used by the interpreter to print
a member of this type.
When calling the print function, the interpreter passes as arguments
the Scheme object to be printed, a Scheme \f2port\fP to which the output is
to be sent, a flag indicating whether output is to be rendered in
human-readable form (\f2display\fP Scheme primitive) or machine-readable,
read-write-invariance preserving form (\f2write\fP), and finally the
current remainders of the maximum \f2print depth\fP and \f2print length\fP.
The return value of this function is not used (the type is \f2int\fP
for historical reasons).
.IP \f2visit\fP 1
A pointer to a @[.``visit'' function] called by the @[.garbage collector]
when tracing the set of all currently accessible objects.
This function is only required if other Scheme objects
are reachable from objects of the newly defined type (a null
pointer can be given otherwise).
It is invoked with two arguments:
a pointer to the object being visited by the garbage collector, and a
pointer to another function to be called once with the address of
each object accessible through the original object.
For example, the implementation of pairs would supply a visit function
that invokes its second argument twice\*-once with the address of
the car of the original object, and once with the address of the cdr.
.Re
.PP
The return value of \f2Define_Type()\fP is a small, unique integer
identifying the type; it is usually stored in a ``T_*'' (or ``t_*'')
variable following the convention used for the built-in types.
.PP
In the current version of Elk, \f2Define_Type()\fP cannot be used
to define new ``pointer-less'' types resembling built-in types
such as \f2fixnum\fP or \f2boolean\fP.
.PP
The first component of the C structure implementing a user-defined
Scheme type must be an \f2Object\fP; its space is used by
the @[.garbage collector] to store a special tag indicating
that the object has been forwarded.
If you are defining a type that has several components one of
which is an \f2Object\fP, just move the \f2Object\fP to the
front of the \f2struct\fP declaration.
Otherwise insert an additional \f2Object\fP component.
.PP
The Scheme primitive that instantiates a new type can request
heap space for the new object by calling the function
@[.\f2Alloc_Object()\fP]:
.Es
Object Alloc_Object(int size, int type, int const_flag);
.Ee
The arguments to \f2Alloc_Object()\fP are the size of
the object in bytes (usually obtained by applying \f2sizeof\fP
to the underlying \f2struct\fP), the type of which the new
object is a member (i.\|e.\& the return value of \f2Define_Type()\fP),
and a flag indicating whether the newly created object is to
be made read-only.
The return value is a fully initialized \f2Object\fP.
.\" ---------------------------------------------------------------------------
.K2 "Example for a User-Defined Scheme Type"
.PP
Figure @(ndbm1) shows the skeleton of an extension that provides a
simple Scheme interface to the UNIX \f2ndbm\fP library; it can be
loaded dynamically into the Scheme interpreter, or into an Elk-based
application that needs access to a simple database from within the
extension language.
Please refer to your system's documentation if you are not familiar with
\f2ndbm\fP.
The extension defines a new, first-class Scheme type \f2dbm-file\fP
corresponding to the \f2DBM\fP type defined by the C library.
Again, note the naming convention to use lower-case for
new identifiers (in contrast to the predefined ones).
.Fs
#include <scheme.h>
#include <ndbm.h>
.El
int t_dbm;
.El
struct s_dbm {
	Object unused;
	DBM *dbm;
	char alive;   /* 0: has been closed, else 1 */
};
.El
#define DBMF(obj) ((struct s_dbm *)POINTER(obj))
.El
int dbm_equal(Object a, Object b) {
	return DBMF(a)->alive && DBMF(b)->alive && DBMF(a)->dbm == DBMF(b)->dbm;
}
.El
int dbm_print(Object d, Object port, int raw, int length, int depth) {
	Printf(port, "#[dbm-file %lu]", DBMF(d)->dbm);
	return 0;
}
.El
Object p_is_dbm(Object d) {
	return TYPE(d) == t_dbm ? True : False;
}
.El
void elk_init_dbm(void) {
	t_dbm = Define_Type(0, "dbm-file", 0, sizeof(struct s_dbm),
		dbm_equal, dbm_equal, dbm_print, 0);
.El
	Define_Primitive(p_is_dbm,    "dbm-file?", 1, 1, EVAL);
	Define_Primitive(p_dbm_open,  "dbm-open",  2, 3, VARARGS);
	Define_Primitive(p_dbm_close, "dbm-close", 1, 1, EVAL);
}
.Fc "Skeleton of a UNIX ndbm extension"
.Fe ndbm1
.PP
The code shown in Figure @(ndbm1) declares a variable \f2t_dbm\fP
to hold the return value of \f2Define_Primitive()\fP, and the
C structure \f2s_dbm\fP that represents the new type.
The structure is composed of the required initial \f2Object\fP,
the \f2DBM\fP pointer returned by the C library function \f2dbm_open()\fP,
and a flag indicating whether the database pointed to by this
object has already been closed (in this case the flag is cleared).
As a \f2dbm-file\fP Scheme object can still be passed to primitives
after the \f2DBM\fP handle has been closed by a call to \f2dbm_close()\fP,
the \f2alive\fP flag had to be added to avoid further use of a ``stale''
object:
the ``dbm'' primitives include an initial check for the flag and raise
an error if it is zero.
.PP
The macro \f2DBMF\fP is used to cast the pointer field of an
\f2Object\fP of type \f2t_dbm\fP to a pointer to the correct structure
type.
\f2dbm_equal()\fP implements both the \f2eqv?\&\fP and the
\f2equal?\&\fP predicates; it returns true if the \f2Objects\fP
compared point to an open database and contain identical \f2DBM\fP
pointers.
The print function just prints the numeric value of the \f2DBM\fP
pointer; this could be improved by printing the name of the database
file instead, which must then be included in each Scheme object.
The primitive \f2p_is_dbm()\fP provides the usual @[.type predicate].
Finally, an @[.extension initialization function] is supplied to
enable @[.dynamic loading] of the compiled code; it registers the new
type and three primitives operating on it.
Note that a @[.visit function] (the final argument to \f2Define_Type()\fP)
is not required here, as the new type does not include any components
of type \f2Object\fP that the garbage collector must know of\*-the
required initial \f2Object\fP is not used here and therefore can
be neglected.
The type constructor primitive \f2dbm-open\fP and the primitive
\f2dbm-close\fP are shown in Figure @(ndbm2).
.PP
.Fs
Object p_dbm_open(int argc, Object *argv) {
	DBM *dp;
	int flags = O_RDWR|O_CREAT;
	Object d, sym = argv[1];
.El
	Check_Type(sym, T_Symbol);
	if (EQ(sym, Intern("reader")))
		flags = O_RDONLY;
	else if (EQ(sym, Intern("writer")))
		flags = O_RDWR;
	else if (!EQ(sym, Intern("create")))
		Primitive_Error("invalid argument: ~s", sym);
	if ((dp = dbm_open(Get_String(argv[0]), flags,
			argc == 3 ? Get_Integer(argv[2]) : 0666)) == 0)
		return False;
	d = Alloc_Object(sizeof(struct s_dbm), t_dbm, 0);
	DBMF(d)->dbm = dp;
	DBMF(d)->alive = 1;
	return d;
}
.El
Object p_dbm_close(Object d) {
	Check_Type(d, t_dbm);
	if (!DBMF(d)->alive)
		Primitive_Error("invalid dbm-file: ~s", d);
	DBMF(d)->alive = 0;
	dbm_close(DBMF(d)->dbm);
	return Void;
}
.Fc "Implementation of \f2dbm-open\fP and \f2dbm-close\fP"
.Fe ndbm2
.PP
The primitive \f2dbm-open\fP shown in Figure @(ndbm2) is called with
the name of the database file, a symbol indicating the type of access
(\f2reader\fP for read-only access, \f2writer\fP for read/write access,
and \f2create\fP for creating a new file with read/write access), and
an optional third argument specifying the file permissions for a
newly-created database file.
A default of 0666 is used for the file permissions if the primitive
is invoked with just two arguments.
Section @(ch-symbits) will introduce a set of functions that avoid clumsy
if-cascades such as the one at the beginning of \f2p_dbm_open()\fP.
@[.\f2Primitive_Error()\fP] is called with a @[.``format string''] and
zero or more arguments and signals a Scheme error (see section @(ch-error)).
\f2dbm-open\fP returns #f if the database file could not be opened,
so that the caller can deal with the error.
.PP
Note that \f2dbm-close\fP first checks the \f2alive\fP bit to
raise an error if the database pointer is no longer valid
because of an earlier call to \f2dbm-close\fP.
This check needs to be performed by all primitives working on
\f2dbm-file\fP objects; it may be useful to wrap it in a separate
function\*-together with the initial type-check.
Ideally, database objects should be closed automatically during
@[.garbage collection] when they become inaccessible; section @(ch-term)
will introduce functions to accomplish this.
.PP
At least two primitives \f2dbm-store\fP and \f2dbm-fetch\fP need
to be added to the database extension to make it really useful;
these are not shown here (their implementation is fairly simple and
straightforward).
Using these primitives, the extension discussed in this section can
be used to write Scheme code such as this procedure (which looks up an
electronic mailbox name in the mail alias database maintained on
most UNIX systems):
.Es
(define expand-mail-alias
  (lambda (alias)
    (let ((d (dbm-open "/etc/aliases" 'reader)))
      (if (not d)
          (error 'expand-mail-alias "cannot open database"))
      (unwind-protect
        (dbm-fetch d alias)
        (dbm-close d)))))
.El
(define address-of-staff (expand-mail-alias "staff"))
.Ee
.\" ---------------------------------------------------------------------------
.K1 "Advanced Topics"
.Rf ch-advanced \*(SN
.\" ---------------------------------------------------------------------------
.K2 "Converting between Symbols, Integers, and Bitmasks"
.Rf ch-symbits \*(SN
.PP
Symbols are frequently used as the arguments to Scheme primitives which
call an underlying C or C++ function with some kind of @[.bitmask] or with a
predefined enumeration constant or preprocessor symbol.
For example, the primitive \f2dbm-open\fP shown in Figure @(ndbm2)
above uses symbols to represent the symbolic constants passed to
\f2dbm_open()\fP.
Similarly, a Scheme primitive corresponding to the UNIX system call
\f2open()\fP could receive a list of symbols represending the
logical OR of the usual \f2open()\fP flags, so that one can
write Scheme code such as:
.Es
(let ((tty-fd (unix-open "/dev/ttya"    '(read write exclusive)))
      (tmp-fd (unix-open "/tmp/somefile '(write create))))
	...
.Ee
.PP
To facilitate conversion of symbols to C integers or enumeration
constants and vice versa, these two functions are provided:
.Es
@[.=Symbols_To_Bits()]@[.=Bits_To_Symbols()]
unsigned long Symbols_To_Bits(Object syms, int mask_flag,
    SYMDESCR *table);
Object Bits_To_Symbols(unsigned long bits, int mask_flag,
    SYMDESCR *table);
.Ee
The type @[.\f2SYMDESCR\fP] is defined as:
.Es
typedef struct {
	char *name;
	unsigned long val;
} SYMDESCR;
.Ee
.PP
\f2Symbols_To_Bits()\fP converts a symbol or a list of symbols to
an integer; \f2Bits_To_Symbols()\fP is the reverse operation and is
usually applied to the return value of a C/C++ function to
convert it to a Scheme representation.
Both functions receive as the third argument a table specifying the
correspondence between symbols and C constants; each table entry is a
pair consisting of the \f2name\fP of a symbol as a C string and an
integer \f2val\fP (typically an enumeration constant or a \f2#define\fP
constant).
Each \f2SYMDESCR\fP array is terminated by an entry with a zero
\f2name\fP component:
.Es
SYMDESCR lseek_syms[] = {
	{ "set",      SEEK_SET },
	{ "current",  SEEK_CUR },
	{ "end",      SEEK_END },
	{ 0, 0 }
};
.Ee
.PP
The second argument to the conversion functions controls whether a
single symbol is converted to an integer or vice versa (\f2mask_flag\fP
is zero), or whether a list of symbols is converted to the logical OR
of a set of matching values or vice versa (\f2mask_flag\fP is
non-zero).
\f2Symbols_To_Bits()\fP signals an error if the symbol does not
match any of the names in the given table or, if \f2mask_flag\fP
is non-zero, if any of the list elements does not match.
The empty list is converted to zero.
If \f2Bits_To_Symbols()\fP is called with a non-zero \f2mask_flag\fP,
it matches the \f2val\fP components against the \f2bits\fP argument
using logical AND.
Regardless of \f2mask_flag\fP, \f2Bits_To_Symbols\fP returns the empty
list if no match occurs.
Figure @(ndbm3) shows an improved version of \f2p_dbm_open()\fP
using \f2Symbols_To_Bits()\fP in place of nested if-statements.
.Fs
static SYMDESCR flag_syms[] = {
	{ "reader", O_RDONLY },
	{ "writer", O_RDWR },
	{ "create", O_RDWR|O_CREAT },
	{ 0, 0 }
};
.El
Object p_dbm_open(int argc, Object *argv) {
	DBM *dp;
	Object d;
.El
	dp = dbm_open(Get_String(argv[0]),
	    Symbols_To_Bits(argv[1], 0, flag_syms),
	    argc == 3 ? Get_Integer(argv[2]) : 0666);
	if (dp == 0)
		return False;
	d = Alloc_Object(sizeof(struct s_dbm), t_dbm, 0);
	DBMF(d)->dbm = dp;
	DBMF(d)->alive = 1;
	return d;
}
.Fc "Improved version of \f2dbm-open\fP using \f2Symbols_To_Bits()\fP"
.Fe ndbm3
.PP
A Scheme primitive calling the UNIX system call \f2access()\fP
could use \f2Symbols_To_Bits()\fP with a non-zero \f2mask_flag\fP
to construct a bitmask:
.Es
Object p_access(Object fn, Object mode) {
	access(Get_String(fn), (int)Symbols_To_Bits(mode, 1, access_syms));
	...
.Ee
where \f2access_syms\fP is defined as:
.Es
static SYMDESCR access_syms[] = {
	{ "read",       R_OK },
	{ "write",      W_OK },
	{ "execute",    X_OK },
	{ 0, 0 }
};
.Ee
Note that in this example the empty list can be passed as the \f2mode\fP
argument to test for existence of the file, because in this case
\f2Symbols_To_Bits()\fP returns zero (the value of \f2F_OK\fP).
.\" ---------------------------------------------------------------------------
.K2 "Calling Scheme Procedures, Evaluating Scheme Code"
.Rf ch-funcall \*(SN
.PP
A Scheme procedure can be called from within C or C++ code using
the function
.Es
@[.=Funcall()]
Object Funcall(Object fun, Object argl, int eval_flag);
.Ee
The first argument is the Scheme procedure\*-either a primitive
procedure (\f2T_Primitive\fP) or a compound procedure (\f2T_Compound\fP).
The second argument is the list of arguments to be passed to
the procedure, as a Scheme list.
The third argument, if non-zero, specifies that the arguments need to be
evaluated before calling the Scheme procedure.
This is usually not the case (except in some special forms).
The return value of \f2Funcall()\fP is the result of the Scheme
procedure.
.PP
\f2Funcall()\fP is frequently used from within C callback functions
that can be registered for certain events, such as the user-supplied
X11 error handlers, X11 event handlers, timeout handlers, the C++
\f2new\fP handler, etc.
Here, use of \f2Funcall()\fP allows to register a user-defined Scheme
procedure for this event from within a Scheme program.
As an example, Figure @(funcall) shows the generic signal handler
that is associated with various UNIX signals by the UNIX extension.
.Fs
void scheme_signal_handler(int sig) {
	Object fun, args;
.El
	Set_Error_Tag("signal-handler");
	Reset_IO(1);
	args = Bits_To_Symbols((unsigned long)sig, 0, signal_syms);
	args = Cons(args, Null);
	fun = VECTOR(handlers)->data[sig];
	if (TYPE(fun) != T_Compound)
		Fatal_Error("no handler for signal %d", sig);
	(void)Funcall(fun, args, 0);
	Printf(Curr_Output_Port, "\en\e7Signal!\en");
	(void)P_Reset();
	/*NOTREACHED*/
}
.Fc "Using \f2Funcall()\fP to call a Scheme procedure"
.Fe funcall
.PP
The signal handler shown in Figure @(funcall) uses the signal
number supplied by the system to index a vector of user-defined
Scheme procedures (that is, \f2Objects\fP of type \f2T_Compound\fP).
@[.\f2Reset_IO()\fP] is used here to ensure that the current input
and output port are in defined state when the Scheme signal
handler starts executing.
The argument list is constructed by calling @[.\f2Cons()\fP];
it consists of a single element\*-the signal number as a Scheme
symbol.
\f2signal_syms\fP is an array of @[.\f2SYMDESCR\fP] records that
maps the UNIX signal names (\f2sighup\fP, \f2sigint\fP, etc.)
to corresponding Scheme symbols of the same names.
The Scheme procedure called from the signal handler is not supposed
to return (it usually invokes a continuation); therefore the result
of \f2Funcall()\fP is ignored.
In case the Scheme handler (and thus the call to \f2Funcall()\fP)
does return, a message is printed and the primitive \f2reset\fP
is called to return to the application's toplevel or standard
Scheme toplevel.
.PP
An S-expression can be evaluated by calling the function
.Es
@[.=Eval()]
Object Eval(Object expr);
.Ee
which is identical to the primitive \f2eval\fP (\f2P_Eval()\fP in C),
except that no optional environment can be supplied.
\f2Eval()\fP is very rarely used by extensions or applications,
mainly by implementations of new special forms.
Both \f2Eval()\fP and \f2Funcall()\fP can trigger a
@[.garbage collection]; all @[.local variable]s holding Scheme \f2Objects\fP
with heap pointers must be properly registered with the
garbage collector to survive calls to these functions.
.PP
Occasionally an S-expression needs to be evaluated that exists as a C
string, for example, when a Scheme expression has been entered through
a ``text widget'' in a graphical user interface.
Here, evaluation requires calling the Scheme reader to parse the
expression; therefore a straightforward solution is to create a
@[.string port] holding the string and then just ``load'' the
contents of the port:
.Es
void eval_string(char *expr) {
	Object port; GC_Node;
.El
	port = P_Open_Input_String(Make_String(expr, strlen(expr)));
	GC_Link(port);
	Load_Source_Port(port);
	GC_Unlink;
	(void)P_Close_Input_Port(port);
}
.Ee
If a more sophisticated function is required, the \f2eval-string\fP
extension included in the Elk distribution can be used
(``lib/misc/elk-eval.c'').
This extension provides a function
.Es
@[.=Elk_Eval()]
char *Elk_Eval(char *expr);
.Ee
that converts the result of evaluating the stringized expression
back to a C string and returns it as a result.
A null pointer is returned if an error occurs during evaluation.
.PP
Applications should not use this function as the primary interface
to the extension language.
In contrast to languages such as @[.Tcl], the semantic concepts and
data structures of Scheme are not centered around strings, and strings
are not a practicable representation for S-expressions.
Instead, applications should pass control to the extension
language by calling Scheme procedures (using @[.\f2Funcall()\fP])
or by loading files containing Scheme code.
The extension language then calls back into the application's C/C++
layer by invoking application-supplied Scheme primitives and other
forms of callbacks as explained in section @(ch-control).
.\" ---------------------------------------------------------------------------
.K2 "GC-Protecting Global Objects"
.Rf ch-gcglobal \*(SN
.PP
Section @(ch-gc) explained when\*-and how\*-to register with
the @[.garbage collector] function-local \f2Object\fP variables
holding heap pointers.
Similarly, @[.global variable]s must usually be added to the set of
reachable objects as well if they are to survive garbage collections
(a useful exception to this rule will be introduced in section @(ch-term)).
In contrast to local variables, global variables are only made
known to the garbage collector once\*-after initialization\*-as
their lifetime is that of the entire program.
To add a global variable to the garbage collector's root set, the
macro
.Es
@[.=Global_GC_Link()]
Global_GC_Link(obj)
.Ee
must be called with the properly initialized variable of type
\f2Object\fP.
The macro takes the address of the specified object.
If that is a problem, an equivalent functional interface can be used:
.Es
@[.=Func_Global_GC_Link()]
void Func_Global_GC_Link(Object *obj_ptr);
.Ee
This function must be supplied the address of the global variable to
be registered with the garbage collector.
.PP
When writing extensions that maintain global \f2Object\fP variables,
\f2Global_GC_Link()\fP (or \f2Func_Global_GC_Link()\fP) is usually
called from within the @[.extension initialization function] right
after each variable is assigned a value.
For instance, the global Scheme vector \f2handlers\fP that was
used in Figure @(funcall) to associate procedures with UNIX signals
is initialized and GC-protected as follows:
.Es
void elk_init_unix_signal(void) {
	handlers = Make_Vector(NSIG, False);
	Global_GC_Link(handlers);
	...
}
.Ee
\f2NSIG\fP is the number of UNIX signal types as defined by the system
include file.
The signal handling Scheme procedures that are inserted into the
vector later need not be registered with the garbage collector, because
they are now reachable through another object which itself is reachable.
.\" ---------------------------------------------------------------------------
.K3 "Dynamic C Data Structures"
.PP
Dynamic data structures, such as the nodes of a linked list containing
Scheme \f2Objects\fP, cannot be easily registered with the garbage
collector.
The simplest solution is to build these data structures in Scheme
rather than in C or C++ in the first place.
For example, a linked list of Scheme objects can be built from
Scheme pairs much more naturally and more straightforward than
from C structures or the like, in particular if the list will
be traversed and manipulated using Scheme primitives anyway.
Besides, data structures programmed in Scheme benefit from automatic
memory management, whereas use of \f2malloc()\fP and \f2free()\fP
in C frequently is a source of memory leaks and related errors.
.PP
If for some reason a dynamic data structure must be built in C or
C++ rather than in Scheme, reachability problems can be avoided
by inserting all \f2Objects\fP into a global, GC-protected vector
(such as \f2handlers\fP in Figure @(funcall)) and then use the
corresponding vector indexes rather than the actual \f2Objects\fP.
This sounds more difficult than it really is; Appendix B shows
the complete source code of a small module to register \f2Objects\fP
in a Scheme vector.
The module exports three functions:
\f2register_object()\fP inserts an \f2Object\fP into the vector
and returns the index as an \f2int\fP;
\f2deregister_object()\fP removes an \f2Object\fP with a given
index from the vector;
and \f2get_object()\fP returns the \f2Object\fP stored under a
given index.
\f2register_object()\fP dynamically grows the vector to avoid
artificial limits.
.PP
A dynamic data structure (e.\|g.\& linked list) implementation using
this module would call \f2register_object()\fP when inserting a new
\f2Object\fP into the list and then use the integer return value in
place of the \f2Object\fP itself.
Similarly, it would call \f2deregister_object()\fP whenever a node
is removed from the list.
\f2get_object()\fP would be used to retrieve the \f2Object\fP associated
with a given list element.
Note that with these functions the same \f2Object\fP can be
registered multiple times (each time under a new index) without
having to maintain reference counts:
the garbage collector does not care how often a particular
\f2Object\fP is traversed during garbage collection, as long
as it will be reached at least once.
.\" ---------------------------------------------------------------------------
.K2 "Weak Pointers and Object Termination"
.Rf ch-term \*(SN
.PP
A data structure implementation may deliberately use \f2Objects\fP
that are not added to the global set of reachable pointers
(as described in the previous section) and are thus invisible to
the @[.garbage collector].
In this case, it becomes possible to determine whether or not
garbage collection has found any \f2other\fP pointers to the same
Scheme objects.
This property can be exploited in several ways by extensions or
applications using Elk.
.PP
Pointers that are not included in the garbage collector's
reachability search are called @[.``weak pointers''].
The memory occupied by a Scheme object that is only referenced by
weak pointers will be reclaimed.
The term \f2weak\fP expresses the notion that the pointer is
not strong enough to prevent the object it points to from
being garbage collected.
Code using weak pointers can scan the pointers immediately after
each garbage collection and check whether the target object
has been visited by the just-finished garbage collection.
If this is the case, normal (strong) pointers to the object must exist
(which can therefore be considered ``live''), and the weak pointer is
updated manually to point to the object's new location.
On the other hand, if the object has not been visited,
no more (normal) references to it exist and the memory occupied by it
has been reclaimed.
.PP
Weak pointers are useful in implementing certain types of data
structures where the sole existence of a (weak) pointer to an object
from within this data structure should not keep the object alive
(\f2weak sets\fP, \f2populations\fP, certain kinds of hash tables, etc.).
Objects that are not reachable through @[.strong pointers] are then
removed from the @[.weak data structure] after garbage collection.
In this case, it is frequently useful to invoke a
@[.``termination function''] for each such object, e.\|g.\& for objects
that contain resources of which only a finite amount is available, such
as UNIX file descriptors (or FILE structures), X displays
and windows, etc.
The termination function for Scheme ports closes the file pointer
encapsulated in a port object if it is still open;
likewise, the termination function for X windows closes the window and
thereby removes it from the display, and so on.
Thus, should an object holding some kind of resource go
inaccessible before it was terminated ``properly'' by calling
the respective Scheme primitive (\f2close-input-port\fP,
\f2close-output-port\fP, \f2destroy-window\fP, etc.), then
resource will be reclaimed after the next garbage collection run.
.\" ---------------------------------------------------------------------------
.K3 "Using Weak Pointers"
.PP
Code using @[.weak pointers] must scan the pointers immediately after
each @[.garbage collection], but \f2before\fP the interpreter resumes
normal operation, because the memory referenced by the weak pointers
can be reused the next time heap space is requested.
This can be accomplished by registering a so-called
@[.``after-GC function].
Elk's garbage collector invokes all after-GC functions (without
arguments) upon completion.
To register an after-GC functions, the function
.Es
@[.=Register_After_GC()]
void Register_After_GC((void (*func)(void)));
.Ee
is used, typically in an @[.extension initializer].
Similarly, extensions and applications can register
@[.=before-GC function]``before-GC functions'' using
.Es
@[.=Register_Before_GC()]
void Register_Before_GC((void (*func)(void)));
.Ee
These functions are called immediately before each garbage collection
and may be used, for instance, to change the application's cursor
to an hourglass symbol.
After-GC and before-GC functions must not trigger another garbage
collection.
.PP
An after-GC function scanning a set of weak pointers makes use
of the three macros @[.\f2IS_ALIVE()\fP], @[.\f2WAS_FORWARDED()\fP],
and @[.\f2UPDATE_OBJ()\fP].
For example, an after-GC function scanning a table of
elements holding \f2Objects\fP with weak pointers could be
written as shown in Figure @(aftergc).
.Fs
void scan_weak_table(void) {
	int i;
.El
	for (i = 0; i < table_size; i++) {
		Object obj = table[i].obj;
		if (IS_ALIVE(obj)) {            /* object is still reachable */
			if (WAS_FORWARDED(obj))
				UPDATE_OBJ(obj);
		} else {
			terminate_object(obj);  /* object is dead; finalize... */
			table[i] = 0;           /* and remove it from the table */
		}
	}
}
.Fc "After-GC function that scans a table containing weak pointers"
.Fe aftergc
.PP
The function \f2scan_weak_table()\fP shown in Figure @(aftergc) can then
be registered as an after-GC function by invoking
.Es
Register_After_GC(scan_weak_table);
.Ee
.PP
The then-part of the if-statement in \f2scan_weak_table()\fP is entered
if the just-completed garbage collection has encountered any pointers
to the Scheme object pointed to by \f2obj\fP; in this case the
pointer conveyed in \f2obj\fP is updated manually using \f2UPDATE_OBJ()\fP
(when using the generational garbage collector included in Elk,
reachability of an object does not necessarily imply that it was
forwarded, hence the additional call to \f2WAS_FORWARDED()\fP).
If \f2IS_ALIVE()\fP returns false, no more strong pointers to the
object exist and it can be terminated and removed from the weak
data structure.
\f2terminate_object()\fP typically would release any external
resources contained in the Scheme object, but it must neither
create any new objects nor attempt to ``revive'' the
dead object in any way (e.\|g.\& create a new strong pointer
to it by inserting it into another, live object).
.\" ---------------------------------------------------------------------------
.K3 "Functions for Automatic Object Termination"
.PP
As automatic termination of Scheme objects using user-supplied
@[.termination function]s is the most frequent use of @[.weak pointers],
Elk offers a set of convenience functions for this purpose.
Extensions and applications can insert \f2Objects\fP into a
@[.weak list] maintained by Elk and remove them from the list
using the two functions
.Es
@[.=Register_Object()]@[.=Deregister_Object()]
void Register_Object(Object obj, char *group,
                     (Object (*term)(Object)), int leader_flag);
void Deregister_Object(Object obj);
.Ee
.PP
\f2term\fP is the termination function that is called automatically
with \f2obj\fP when the object becomes unreachable (its result
is not used);
\f2group\fP is an opaque ``cookie'' associated with \f2obj\fP
and can be used to explicitly terminate all objects with the
same value for \f2group\fP;
a non-zero \f2leader_flag\fP indicates that \f2obj\fP is the
``leader'' of the specified \f2group\fP.
Elk automatically registers an @[.after-GC function] to scan
the weak list maintained by these two functions and to call
the \f2term\fP function for all objects that could be proven
unreachable by the garbage collector, similar to the function
shown in Figure @(aftergc).
.PP
Object termination takes place in two phases:
first all objects registered with a zero \f2leader_flag\fP
are terminated, after that the termination functions of
the leaders are invoked.
This group and leader notion is used, for example, by the
@[.Xlib extension] to associate windows (and other resources) with
an X display:
the ID of the display to which a window belongs is used as
the window's group, and the display is marked as the group leader.
Thus, if a display becomes unreachable or is closed by the program, all
its windows are closed before the display is finally destroyed\**.
.FS
This interface has evolved in a slightly \f2ad hoc\fP way;
the two-stage relationship expressed by groups and group leaders
may not be sufficient for more complex hierarchies than those
used in X.
.FE
.LP
Two additional functions are provided for explicitly calling
the termination functions:
.Es
@[.=Terminate_Type()]@[.=Terminate_Group()]
void Terminate_Type(int type);
void Terminate_Group(char *group);
.Ee
\f2Terminate_Type()\fP invokes the termination function (if any) for
all objects of a given type and deletes them from the weak list.
For example, to close all ports currently held open by Elk (and
thus apply \f2fclose()\fP to the FILE pointers embedded in them),
one would call
.Es
@[.=T_Port]
Terminate_Type(T_Port)
.Ee
\f2Terminate_Group()\fP calls the termination functions of
all non-leader objects belonging to the specified \f2group\fP.
.LP
Finally, another function, @[.\f2Find_Object()\fP],
locates an object in the weak list:
.Es
Object Find_Object(int type, char *group,
		   (int (*match_func)(Object, ...)), ...);
.Ee
Arguments are a Scheme type, a group, and a match function called
once for each object in the weak list that has the specified type
and group.
The match function is passed the \f2Object\fP and the remaining arguments
to \f2Find_Object()\fP, if any.
If the match function returns true for an object, this object becomes
the return value of \f2Find_Object()\fP; otherwise it returns \f2Null\fP.
.PP
Complicated as it may seem, \f2Find_Object()\fP is quite useful\*-extensions
can check whether a Scheme object with certain properties
has already been registered with the weak list earlier and, if this is the
case, return \f2this\fP object instead of creating a new one.
This is critical for Scheme objects encapsulating some kind of
external resource, such as file descriptors or X windows.
Consider, for example, a Scheme primitive that obtains the topmost
window on a given X display and returns it as a Scheme \f2window\fP
object.
If the primitive just were to instantiate a Scheme object
encapsulating the corresponding X window ID for each call, it would
become possible for two or more distinct Scheme \f2window\fP objects to
reference the same real X window.
This is not acceptable, because two Scheme objects pointing to the same X
object should certainly be equal in the sense of \f2eq?\&\fP,
not to mention the problems that would ensue if one of the Scheme
\f2window\fP objects were closed (thereby destroying the underlying
X window) and the second one were still be operated on afterwards.
Example uses of \f2Find_Object()\fP can be found in the @[.Xlib extension]
and in the @[.Xt extension] that are included in the Elk distribution.
.\" ---------------------------------------------------------------------------
.K2 "Errors"
.Rf ch-error \*(SN
.PP
User-supplied code can signal an error by calling
@[.\f2Primitive_Error()\fP] with a @[.format string] and as many additional
arguments (\f2Objects\fP) as there are @[.format specifier]s in the format
string:
.Es
void Primitive_Error(char *fmt, ...);
.Ee
\f2Primitive_Error()\fP calls the default or user-defined
@[.error handler] as described in the Elk Reference Manual, passing it an
@[.``error tag''] identifying the source of the error, the format
string, and the remaining arguments.
A special format specifier ``~E'' can be used to interpolate the standard
error message text corresponding to the UNIX error number @[.\f2errno\fP];
this is useful for primitives that invoke UNIX system calls or certain
C library functions (if ``~e'' is used, the first character of
the text is converted to lower case).
If this format specifier is used, the current \f2errno\fP must be
assigned to a variable @[.\f2Saved_Errno\fP] prior to calling
\f2Primitive_Error()\fP to prevent it from being overwritten
by the next system call or C library function.
\f2Primitive_Error()\fP does not return.
.PP
Applications that need to supply their own error handler by
redefining \f2error-handler\fP usually do so in Scheme,
typically at the beginning of the initial Scheme file loaded
in \f2main()\fP.
.PP
If \f2Primitive_Error()\fP is called from within a C function
that implements a Scheme primitive, an error tag is supplied
by Elk (the name of the primitive).
Applications may set the error tag explicitly at the
beginning of sections of C/C++ code that reside outside of
primitives, for example, before loading an initial Scheme
file in the application's \f2main()\fP.
Two functions are provided to set and query the current error tag:
.Es
@[.=Set_Error_Tag()]@[.=Get_Error_Tag()]
void Set_Error_Tag(const char *tag);
char *Get_Error_Tag(void);
.Ee
The following three functions can be used by primitives to signal
errors with standardized messages in certain situations:
.Es
@[.=Range_Error()]@[.=Wrong_Type()]@[.=Wrong_Type_Combination()]
void Range_Error(Object offending_obj);
void Wrong_Type(Object offending_obj, int expected_type);
void Wrong_Type_Combination(Object offending_obj, char *expected_type);
.Ee
\f2Range_Error()\fP can be used when an argument to a primitive
is out of range (typically some kind of index).
\f2Wrong_Type()\fP signals a failed type-check for the given
\f2Object\fP; the second argument is the expected type of the
\f2Object\fP.
This function is used, for example, by @[.\f2Check_Type()\fP].
\f2Wrong_Type_Combination()\fP is similar to \f2Wrong_Type()\fP;
the expected type is specified as a string.
This is useful if an \f2Object\fP can be a member of one out
of two or more types, e.\|g.\& a string or a symbol.
.LP
Fatal errors can be signaled using the functions
.Es
@[.=Fatal_Error()]@[.=Panic()]
void Fatal_Error(char *fmt, ...);
void Panic(char *msg);
.Ee
\f2Fatal_Error()\fP passes its arguments to \f2printf()\fP and
then terminates the program.
\f2Panic()\fP is used in situations that ``cannot happen''
(failed consistency checks or failed assertions);
it prints the specified message and terminates the program
with a core dump.
.\" ---------------------------------------------------------------------------
.K2 "Exceptions"
.PP
As explained in the Elk Reference Manual, a user-supplied Scheme
procedure is called each time an @[.\f2exception\fP] is raised.
Currently, the set of UNIX @[.signals] that are caught by the interpreter
or an extension (at least \f2interrupt\fP and \f2alarm\fP) are used
as exceptions.
As signals occur asynchronously, extensions and applications must be
able to protect non-reentrant or otherwise critical code sections
from the delivery of signals.
In particular, calls to external library functions are frequently
not reentrant\** and need to be protected from being disrupted.
.FS
Fortunately, with the advent of multithreading, vendors are now
beginning to provide reentrant versions of their system libraries.
.FE
.PP
Extensions may call the macros @[.\f2Disable_Interrupts\fP] and
@[.\f2Enable_Interrupts\fP] (without arguments) to enclose code fragments
that must be protected from exceptions.
Calls to these macros can be nested, and they are also available
as Scheme primitives on the Scheme-language level.
As all modern UNIX versions provide a facility to temporarily block
the delivery of signals, a signal that occurs after a call to
\f2Disable_Interrupts\fP will be delayed until the outermost matching
\f2Enable_Interrupts\fP is executed.
Two additional macros, @[.\f2Force_Disable_Interrupts\fP] and
@[.\f2Force_Enable_Interrupts\fP] can be used to enable
and disable signal delivery regarless of the current nesting level.
Extensions that use additional signals (such as the \f2alarm\fP signal)
must register these with the interpreter core to make sure they are
included in the \f2mask\fP of signals that is maintained by
\f2Disable_Interrupts\fP and \f2Enable_Interrupts\fP (the interface
for registering signals is still being revised; refer to the source
code of the UNIX extension for an example).
.PP
The ability to protect code from exceptions is particularly useful
for primitives that temporarily open a file or allocate some other
kind of resource that must subsequently be released again.
If the relevant code fragment were not enclosed by calls to
\f2Disable_Interrupts\fP and \f2Enable_Interrupts\fP, an exception
handler could abandon execution of the code section by calling
a continuation, thus causing the file to remain open forever.
While situations like this can be handled by \f2dynamic-wind\fP
on the Scheme level, some form of
\f2try/catch\fP facility is not available on the C-language level,
and using the C function implementing the \f2dynamic-wind\fP primitive
would be cumbersome.
.LP
The function
.Es
@[.=Signal_Exit()]
void Signal_Exit(int signal_number);
.Ee
may be used as the handler for signals that must terminate the
application; it ensures that the temporary files maintained by Elk are
removed and calls the @[.extension finalization functions] in
the normal way.
.\" ---------------------------------------------------------------------------
.K2 "Defining Scheme Variables"
.PP
User-supplied C/C++ code can define global Scheme variables that are
maintained as corresponding \f2Object\fP C variables.
The Scheme interpreter itself defines several such variables,
for example, the variable @[.\f2load-path\fP] (see section @(ch-dynl))
which can be modified and read both from Scheme and from C.
The function @[.\f2Define_Variable()\fP] is used
to define a Scheme variable and bind an initial value to it:
.Es
void Define_Variable(Object *var, const char *name, Object init);
.Ee
\f2var\fP is the address of the C variable corresponding to
the newly-created Scheme variable, \f2name\fP is the
name of the Scheme variable, and \f2init\fP is its initial value.
\f2Define_Variable()\fP calls @[.\f2Intern()\fP] to create the
variable name included in the new binding and
@[.\f2Func_Global_GC_Link()\fP] to properly register the C variable
with the garbage collector.
.LP
The C side of a Scheme variable cannot be accessed directly;
the functions
.Es
@[.=Var_Set()]@[.=Var_Get()]
Var_Set(Object variable, Object value);
Var_Get(Object variable)
Var_Is_True(Object variable)
.Ee
must be used instead to assign a value to the variable and
to read its current value; the first argument to each function
is the \f2Object\fP whose address was passed to \f2Define_Variable()\fP.
\f2Var_Is_True()\fP is convenient for boolean variables and tests
whether the contents of the variable is true in the sense of \f2Truep()\fP.
As an example, Figure @(defvar) shows how the @[.Xt extension]
defines a Scheme variable that is associated with the user-defined
``warning handler'' called by the Xt library to output warning messages.
.Fs
Object V_Xt_Warning_Handler;
.El
void Xt_Warning(char *msg) {
	Object args, fun;
.El
	args = Cons(Make_String(msg, strlen(msg)), Null);
	fun = Var_Get(V_Xt_Warning_Handler);
	if (TYPE(fun) == T_Compound)
		(void)Funcall(fun, args, 0);
	else
		Printf(Curr_Output_Port, "%s\en", msg);
}
.El
void elk_init_xt_error(void) {
	Define_Variable(&V_Xt_Warning_Handler, "xt-warning-handler", Null);
	XtSetWarningHandler(Xt_Warning);
}
.Fc "The Xt extension defines a Scheme variable holding a ``warning handler''"
.Fe defvar
.PP
In the example in Figure @(defvar), the function \f2Xt_Warning()\fP
is registered as the Xt ``warning handler'' by passing it to
\f2XtSetWarningHandler()\fP.
It is invoked by Xt with a warning message.
The message is converted to a Scheme string, and, if the Scheme
variable \f2xt-warning-handler\fP has been assigned a procedure,
this procedure is called with the string using @[.\f2Funcall()\fP].
Otherwise the string is just sent to the current output port.
The call to \f2Define_Variable()\fP in the extension initialization
function associates the Scheme variable \f2xt-warning-handler\fP
with the C variable \f2V_Xt_Warning_Handler\fP (as a convention,
Elk uses the prefix ``V_'' for variables of this kind).
.\" ---------------------------------------------------------------------------
.K2 "Defining Readers"
.PP
In addition or as an alternative to the constructor primitive
for a new Scheme type, applications and extensions may define a
@[.\f2reader\fP function] for each new type.
The @[.bitstring extension], for example, defines a reader to allow
input of bitstring literals using the \f2#*10110001\fP syntax.
Each user-defined read syntax is introduced by the `#' symbol
followed by one more character, identifying the type of the object.
To define a reader, the following function is called (typically
from within an @[.extension initialization function]):
.Es
@[.=Define_Reader()]
void Define_Reader(int c,
    (Object (*func)(Object port, int c, int const_flag)));
.Ee
.PP
The arguments to \f2Define_Reader()\fP are the as yet unused
character identifying the type (e.\|g.\& `*' for bitstrings)
and a pointer to a \f2reader function\fP that is invoked by the
Scheme parser whenever the newly defined syntax is encountered.
This reader function is passed a Scheme input port from which it reads
the next token, the character following the `#' symbol (to facilitate
using the same reader for different types), and a flag indicating
whether the newly-created object is expected to be made read-only
(this is true when expressions are loaded from a file).
The reader function must return a new object of the given type.
.PP
You may want to refer to the bitstring extension included in the Elk
distribution for an example definition of a reader function
(``lib/misc/bitstring.c''), and for the macros that can be used by
reader functions to efficiently read characters from a port.
.\" ---------------------------------------------------------------------------
.K2 "Fork Handlers"
.PP
Extensions may need to be notified when a copy of the running
interpreter (or application) is created by means of the \f2fork()\fP
UNIX system call.
For example, consider an extension that stores information in a
temporary file and removes this file on termination of the program.
If another extension created a copy of the running interpreter by
calling \f2fork()\fP, the child process would remove the temporary
file on exit\*-the file would not be available to the original
instance of the interpreter (i.\|e.\& the parent process) any longer.
To prevent premature removal of the file, the extension that owns
it can define a @[.\f2fork handler\fP] by calling @[.\f2Register_Onfork()\fP]
with a pointer to a C function:
.Es
void Register_Onfork((void (*func)(void)));
.Ee
The function could create an additional link to the file, so that
a child process would just remove this link on exit, leaving the
original link intact.
.PP
Extensions that use \f2fork()\fP without executing a new program
in the child process (e.\|g.\& the @[.UNIX extension] which
defines a \f2unix-fork\fP primitive) are required to call the function
@[.\f2Call_Onfork()\fP] in the newly created child process to invoke all
currently defined fork handlers:
.Es
void Call_Onfork(void);
.Ee
.\" ---------------------------------------------------------------------------
.AP "Appendix A: Functions that can Trigger a Garbage Collection"
.PP
This appendix lists the functions exported by Elk that may trigger a
@[.garbage collection].
Within C/C++ code, local Scheme objects must be protected as shown in
section @(ch-gc) when one of these functions is called during the
objects' lifetime.
.PP
The C functions corresponding to the following Scheme primitives can
cause a garbage collection:
.Es
append                  load                    read-string
apply                   macro-body              require
autoload                macro-expand            reverse
backtrace-list          make-list               string
call-with-input-file    make-string             string->list
call-with-output-file   make-vector             string->number
call/cc                 map                     string->symbol
command-line-args       oblist                  string-append
cons                    open-input-file         string-copy
dump                    open-input-output-file  substring
dynamic-wind            open-input-string       symbol-plist
eval                    open-output-file        tilde-expand
for-each                open-output-string      type
force                   port-line-number        vector
get-output-string       procedure-lambda        vector->list
list                    provide                 vector-copy
list->string            put                     with-input-from-file
list->vector            read                    with-output-to-file
.El
.ft 2
all special forms
all mathematical primitives except predicates
all output primitives if output is sent to a string port
.ft
.Ee
.PP
In practice, most of these functions, in particular the special forms,
are rarely or never used in extensions or Elk-based applications.
In addition to these primitives, the following C functions can
trigger a garbage collection:
.Es
Alloc_Object()          Make_Reduced_Flonum()   Make_String()
Make_Port()             Make_Flonum()           Make_Const_String()
Load_Source_Port()      Define_Primitive()      Intern()
Load_File()             Printf()                CI_Intern()
Copy_List()             Print_Object()          Define_Variable()
Const_Cons()            General_Print_Object()  Define_Symbol()
Make_Integer()          Format()                Bits_To_Symbols()
Make_Unsigned()         Eval()                  Make_Vector()
Make_Long()             Funcall()               Make_Const_Vector()
Make_Unsigned_Long()
.Ee
.LP
Note: \f2Make_Integer()\fP, \f2Make_Unsigned()\fP,
\f2Make_Long()\fP, and \f2Make_Unsigned_Long()\fP can only trigger a
garbage collection if \f2FIXNUM_FITS()\fP (or \f2UFIXNUM_FITS()\fP,
respectively) returns zero for the given argument.
.\" ---------------------------------------------------------------------------
.AP "Appendix B: Convenience Functions for GC-Safe Data Structures"
.PP
Figure @(gcroot) shows the source code for a set of functions to
insert Scheme objects into a vector that has been registered with the
garbage collector, to delete objects from the vector,
and to retrieve the object stored under a given vector index.
These functions help building dynamic data structures (such as
linked lists or hash tables) containing Scheme objects.
There is nothing application-specific in the code; if you find it
useful, you can directly include it in your Elk extension or
Elk-based application without any changes.
See section @(ch-gcglobal) for a detailed description.
.Fs nofloat
static int max_objects = 32;     /* initial size */
static int num_objects;
static Object objects;
static int inx;
.El
int register_object(Object x) {
	Object v;
	int n;
	GC_Node;
.El
	if (num_objects == max_objects) {
		max_objects *= 2;
		GC_Link(x);
		v = Make_Vector(max_objects, Null);
		GC_Unlink;
		memcpy(VECTOR(v)->data, VECTOR(objects)->data,
			num_objects * sizeof(Object));
		objects = v;
		inx = num_objects;
	}
	for (n = 0; !Nullp(VECTOR(objects)->data[inx]);
			inx++, inx %= max_objects) {
		n++;
		assert(n < max_objects);
	}
	VECTOR(objects)->data[inx] = x;
	num_objects++;
	return inx;
}
.El
void deregister_object(int i) {
	VECTOR(objects)->data[i] = Null;
	--num_objects;
	assert(num_objects >= 0);
}
.El
Object get_object(int i) {
	return VECTOR(objects)->data[i];
}
.El
void elk_init_gcroot(void) {
	objects = Make_Vector(max_objects, Null);
	Global_GC_Link(objects);
}
.Fc "Functions to map Scheme objects to indexes into a GC-safe vector"
.Fe gcroot
.\" ---------------------------------------------------------------------------
.AP "Appendix C: Summary of Functions, Macros, Types, and Variables"
.PP
This appendix provides a quick overview of the functions and other
definitions exported by the Elk kernel.
The list is divided in groups of definitions with related
functionality; the entries are presented in roughly the same order
in which they are introduced in the above chapters.
Full function prototypes are given for functions; in some
prototypes, arguments are given names for clarification.
The initial keywords \f3function\fP, \f3macro\fP, \f3typedef\fP,
and \f3variable\fP indicate the type of each entry (function,
preprocessor symbol with or without arguments, type definition,
and external variable defined by Elk, respectively).
The functions corresponding to Scheme primitives (as described
in section @(ch-prims)) have been omitted from the list.
.SH
Accessing the Scheme Object Representation
.LP
.Cs
\f3typedef\fP Object
.Cl
\f3macro\fP TYPE(obj)
\f3macro\fP POINTER(obj)
\f3macro\fP ISCONST(obj)
\f3macro\fP SETCONST(obj)
\f3macro\fP SET(obj, type, ptr)
\f3macro\fP EQ(obj1, obj2)
.Ce
.SH
Defining Scheme Primitives
.LP
.Cs
\f3function\fP void Define_Primitive((Object (*func)()), const char *name,
               int minargs, int maxargs, enum discipline disc);
.Ce
.SH
Making Objects Known to the Garbage Collector
.LP
.Cs
\f3macro\fP GC_Node, GC_Node2, ...
\f3macro\fP GC_Link(obj), GC_Link2(obj1, obj2), ...
\f3macro\fP GC_Unlink
\f3macro\fP Global_GC_Link(obj)
\f3function\fP void Func_Global_GC_Link(obj_ptr);
.Ce
.SH
Booleans
.LP
.Cs
\f3macro\fP T_Boolean
\f3macro\fP Truep(obj)
.Cl
\f3variable\fP Object True
\f3variable\fP Object False
.Cl
\f3function\fP int Eqv(Object, Object);
\f3function\fP int Equal(Object, Object);
.Ce
.SH
Characters
.LP
.Cs
\f3macro\fP T_Character
\f3macro\fP CHAR(char_obj)
\f3function\fP Object Make_Char(int);
\f3variable\fP Object Newline
.Ce
.SH
Pairs and Lists
.LP
.Cs
\f3macro\fP T_Null
\f3macro\fP Nullp(obj)
\f3variable\fP Null
.Cl
\f3macro\fP T_Pair
\f3macro\fP PAIR(pair_obj)
\f3macro\fP Car(obj)
\f3macro\fP Cdr(obj)
\f3macro\fP Cons(obj1, obj2)
.Cl
\f3macro\fP Check_List(obj)
\f3function\fP int Fast_Length(Object);
\f3function\fP Object Copy_List(Object);
.Ce
.SH
Integers (Fixnums and Bignums)
.LP
.Cs
\f3macro\fP T_Fixnum
\f3macro\fP T_Bignum
\f3macro\fP FIXNUM_FITS(integer)
\f3macro\fP UFIXNUM_FITS(unsigned_integer)
\f3macro\fP FIXNUM(fixnum_obj)
\f3macro\fP BIGNUM(bignum_obj)
.Cl
\f3macro\fP Check_Integer(obj)
\f3macro\fP Check_Number(obj)
.Cl
\f3function\fP Object Make_Integer(int);
\f3function\fP Object Make_Unsigned(unsigned);
\f3function\fP Object Make_Long(long);
\f3function\fP Object Make_Unsigned_Long(unsigned long);
.Cl
\f3function\fP int Get_Integer(Object);
\f3function\fP unsigned Get_Unsigned(Object);
\f3function\fP long Get_Long(Object);
\f3function\fP unsigned long Get_Unsigned_Long(Object);
.Cl
\f3function\fP int Get_Exact_Integer(Object);
\f3function\fP unsigned Get_Exact_Unsigned(Object);
\f3function\fP long Get_Exact_Long(Object);
\f3function\fP unsigned long Get_Exact_Unsigned_Long(Object);
.Ce
.SH
Floating Point Numbers (Reals)
.LP
.Cs
\f3macro\fP T_Flonum
\f3macro\fP FLONUM(flonum_obj)
\f3function\fP Object Make_Flonum(double);
\f3function\fP Object Make_Reduced_Flonum(double);
\f3function\fP double Get_Double(Object);
.Ce
.SH
Symbols
.LP
.Cs
\f3macro\fP T_Symbol
\f3macro\fP SYMBOL(symbol_obj)
\f3function\fP Object Intern(const char *);
\f3function\fP Object CI_Intern(const char *);
\f3function\fP void Define_Symbol(Object *var, const char *name);
\f3variable\fP Object Void
.Cl
\f3typedef\fP SYMDESCR
\f3function\fP unsigned long Symbols_To_Bits(Object syms, int mask_flag,
               SYMDESCR *table);
\f3function\fP Object Bits_To_Symbols(unsigned long bits, int mask_flag,
               SYMDESCR *table);
.Ce
.SH
Strings
.LP
.Cs
\f3macro\fP T_String
\f3macro\fP STRING(string_obj)
\f3function\fP Object Make_String(const char *init, int size);
\f3function\fP char *Get_String(Object);
\f3function\fP char *Get_Strsym(Object);
\f3macro\fP Get_String_Stack(obj, char_ptr)
\f3macro\fP Get_Strsym_Stack(obj, char_ptr)
.Ce
.SH
Vectors
.LP
.Cs
\f3macro\fP T_Vector
\f3macro\fP VECTOR(vector_obj)
\f3function\fP Object Make_Vector(int size, Object fill);
.Ce
.SH
Ports
.LP
.Cs
\f3macro\fP T_Port
\f3macro\fP PORT(port_obj)
\f3function\fP Object Make_Port(int flags, FILE *f, Object name);
\f3function\fP Object Terminate_File(Object port);
\f3macro\fP Check_Input_Port(obj)
\f3macro\fP Check_Output_Port(obj)
\f3variable\fP Object Curr_Input_Port, Curr_Output_Port
\f3variable\fP Object Standard_Input_Port, Standard_Output_Port
\f3function\fP void Reset_IO(int destructive_flag);
\f3function\fP void Printf(Object port, char *fmt, ...);
\f3function\fP void Print_Object(Object obj, Object port, int raw_flag,
               int print_depth, int print_length);
\f3macro\fP Print(obj)
\f3function\fP void Load_Source_Port(Object port);
\f3function\fP void Load_File(char *filename);
.Ce
.SH
Miscellaneous Types
.LP
.Cs
\f3macro\fP T_End_Of_File
\f3variable\fP Object Eof
.Cl
\f3macro\fP T_Environment
\f3variable\fP Object The_Environment, Global_Environment
.Cl
\f3macro\fP T_Primitive
\f3macro\fP T_Compound
\f3function\fP void Check_Procedure(Object);
.Cl
\f3macro\fP T_Control_Point
\f3macro\fP T_Promise
\f3macro\fP T_Macro
.Ce
.SH
Defining Scheme Types and Allocating Objects
.LP
.Cs
\f3function\fP int Define_Type(int zero, const char *name,
               int (*size)(Object), int const_size,
               int (*eqv)(Object, Object),
               int (*equal)(Object, Object),
               int (*print)(Object, Object, int, int, int),
               int (*visit)(Object*, int (*)(Object*)));
\f3function\fP Object Alloc_Object(int size, int type, int const_flag);
.Ce
.SH
Calling Scheme Procedures and Evaluating Scheme Code
.LP
.Cs
\f3function\fP Object Funcall(Object fun, Object argl, int eval_flag);
\f3function\fP Object Eval(Object expr);
\f3function\fP char *String_Eval(char *expr);
.Ce
.SH
Weak Pointers and Object Termination
.LP
.Cs
\f3function\fP void Register_Before_GC((void (*func)(void)));
\f3function\fP void Register_After_GC((void (*func)(void)));
.Cl
\f3macro\fP IS_ALIVE(obj)
\f3macro\fP WAS_FORWARDED(obj)
\f3macro\fP UPDATE_OBJ(obj)
.Cl
\f3function\fP void Register_Object(Object obj, char *group,
               (Object (*term)(Object)), int leader_flag);
\f3function\fP void Deregister_Object(Object obj);
\f3function\fP void Terminate_Type(int type);
\f3function\fP void Terminate_Group(char *group);
\f3function\fP Object Find_Object(int type, char *group,
               (int (*match_func)(Object, ...)), ...);
.Ce
.SH
Signaling Errors
.LP
.Cs
\f3function\fP void Primitive_Error(char *fmt, ...);
\f3function\fP void Set_Error_Tag(const char *tag);
\f3function\fP char *Get_Error_Tag(void);
\f3function\fP void Set_App_Name(char *name);
\f3function\fP void Range_Error(Object offending_obj);
\f3function\fP void Wrong_Type(Object offending_obj, int expected_type);
\f3function\fP void Wrong_Type_Combination(Object offending_obj,
	       char *expected_type);
\f3function\fP void Fatal_Error(char *fmt, ...);
\f3function\fP void Panic(char *msg);
\f3variable\fP int Saved_Errno
.Ce
.SH
Exceptions (Signals)
.LP
.Cs
\f3macro\fP Disable_Interrupts, Enable_Interrupts
\f3macro\fP Force_Disable_Interrupts, Force_Enable_Interrupts
\f3function\fP void Signal_Exit(int signal_number);
.Ce
.SH
Defining and Using Scheme Variables
.LP
.Cs
\f3function\fP void Define_Variable(Object *var, const char *name, Object init);
\f3function\fP void Var_Set(Object var, Object val);
\f3function\fP Object Var_Get(Object var);
\f3function\fP int Var_Is_True(Object var);
.Ce
.SH
Defining Reader Functions
.LP
.Cs
\f3function\fP void Define_Reader(int c,
               (Object (*func)(Object port, int c, int const_flag)));
.Ce
.SH
Fork Handlers
.LP
.Cs
\f3function\fP void Register_Onfork((void (*func)(void)));
\f3function\fP void Call_Onfork(void);
.Ce
.SH
Allocating Memory
.LP
.Cs
\f3function\fP char *Safe_Malloc(unsigned size);
\f3function\fP char *Safe_Realloc(char *old_pointer, unsigned size);
.Cl
\f3macro\fP Alloca_Begin, Alloca_End
\f3macro\fP Alloca(char_ptr, type, size)
.Ce
.SH
Initializing Elk from an Application's main()
.LP
.Cs
\f3function\fP void Elk_Init(int argc, char **argv, int init_flag,
	       char *filename);
.Ce
.SH
Miscellaneous Macros
.LP
.Cs
\f3macro\fP ELK_MAJOR, ELK_MINOR
\f3macro\fP NO_PROTOTYPES, WANT_PROTOTYPES
.Ce
.\" ---------------------------------------------------------------------------
.\" XXX: dynamic loading + dump
.\" ---------------------------------------------------------------------------
.if !\n(.U .so ../util/tmac.index
.if !\n(.U .so side.inx
.Tc
