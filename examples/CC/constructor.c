/*----------------------------------------------------------------------

This simple C++ program demonstrates that static constructors (destructors)
are invoked by Elk when loading a compiled C++ file (when exiting).

o  Compile the program with CC -c -I/usr/elk/include constructor.c, where
   /usr/elk is the toplevel directory of your Elk installation.  Under
   Solaris 2.x (and SysVR4) you also have to specify -pic (-fpic for g++).

o  Invoke Elk and set the load-libraries to point to the C++ and the
   C library, e.g. type something like:

	(set! load-libraries "-L/usr/lang/SC2.0.1 -lC -lc")
   or
	(set! load-libraries "-R/usr/lang/lib -L/usr/lang/lib -lC -lc")
   or just
	(set! load-libraries "-lC -lc")
   
   depending on the platform and the place where the C++ library has
   been installed on your system.  If you are using g++, you may have
   to mention both the g++ library and the gcc library.

o  Now "(load 'constructor.o)", observe the "invoking constructor" message,
   and evaluate "(test)", which should return 3.  Terminate the interpreter
   and observe the "invoking destructor" message.


o  If you get a message from the linker complaining about `Text relocation
   remains against symbol _GLOBAL_.D.P_Test__Fv', you have probably run
   into a known bug in g++ on ELF-based systems (such as Solaris 2.x).

   In this case you have to link your C++ extensions with Elk statically
   or use a different C++ compiler.


o  If static constructors don't get called when loading compiled C++ files,
   your C++ compiler is probably using a naming convention for static
   constructors and destructors that is not anticipated by the current
   version of Elk.
   
   In this case, you may want to find out what kind of names are used
   (by applying "nm" to an object file) and add the name prefixes to
   the Init_Prefixes and Finit_Prefixes lists in src/stab.c in the Elk
   source tree.  Then recompile Elk.  Send me mail.
----------------------------------------------------------------------*/


#include "scheme.h"

#include <iostream.h>

class C {
public:
    int i;
    C() {
       cerr << "[invoking constructor]" << endl;
       i = 3;
    }
    ~C() { cerr << "[invoking destructor]" << endl; }
};

C c;

Object P_Test() {
    return Make_Integer(c.i);
}

void elk_init_constructor() {
    Define_Primitive((Object (*)(...))P_Test, "test", 0, 0, EVAL);
}

void elk_finit_constructor() {
    cerr << "Goodbye." << endl;
}
