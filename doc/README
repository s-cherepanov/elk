This directory tree contains the documentation for Elk as troff source
and PostScript hardcopy files.

You need a troff and the ms macro package to build the documentation
from the troff source (see the Makefile in each subdirectory).  The
files ending in .ps are pre-generated PostScript files; you can send
them directly to a PostScript printer or browse them with a PostScript
previewer.  The PostScript file have been generated with GNU groff.

As a courtesy to our US audience, the PostScript files are in US
letter format.  If you don't like this (and your groff default is A4),
deactivate the ".pl 11i" directives in util/tmac.scheme,
usenix/usenix.ms, and man/elk.1 (i.e. insert a comment token before it)
and rebuild the document(s).

All the documents have been prepared for translation to HTML using
the Elk-based `unroff' package (see ../README for availability
information).


kernel/    The Scheme Reference for Elk.  It describes the Scheme language
           dialect implemented by the Scheme interpreter included in
	   Elk (a superset of the official language).

usenix/    A paper about Elk that has appeared in USENIX Computing
	   Systems (vol. 7, no. 4, pp. 419-449, 1994).

man/       This directory holds a brief online manual page for the Scheme
	   interpreter component.  You may want to install in `/usr/share/man'
	   on your system.  The manual page essentially describes the
	   command line options of the interpreter.

xlib/      The reference manual for the Xlib extension to Elk.

xt/        The reference manual for the Xt (X11 Toolkit Intrinsics)
	   extension to Elk.

unix/      The reference manual for the UNIX extension.

record/    The reference manual for the record extension.

bitstring/ The reference manual for the bit string extension.

regexp/    The reference manual for the regular expression extension.

oops/      A manual for the simple Scheme-based object oriented programming
           tool included in Elk (oops.scm).

cprog/     The C/C++ Programmer's Manual for Elk.  This comprehensive
	   manual describes the facilities of the C/C++ interface of
	   Elk. Topics range from the general architecture of Elk-based
	   applications and defining application-specific Scheme types
	   and primitives, to more advanced subjects such as interacting
	   with the garbage collector.  The audience are authors of
	   Elk-based applications and extension writers.

	   This manual is a replacement for the document that lived in
	   a subdirectory `ex' in earlier version of Elk.

util/      A collection of troff macro files and other utilities needed
	   for typesetting the documentation in the above directories.

	   There is a small C program `mkindex.c' that is required to
	   build the C/C++ Programmer's Manual (cprog/cprog.ms); you
	   will have to compile it by calling "make" if you must typeset
	   the manual.  See the comment at the beginning of util/mkindex.c
	   for a brief explanation of what it does.
