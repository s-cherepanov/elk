/*----------------------------------------------------------------------

This simple C++ program demonstrates that static constructors (destructors)
are invoked by Elk when loading a compiled C++ file (when exiting).

o  Compile the shared object, for instance:

   CC -pic -shared -I/usr/elk/include constructor.cpp -o constructor.so -lelk

   or:

   g++ -fPIC -shared -I/usr/include/elk constructor.ppc -o constructor.so -lelk

o  Now "(load 'constructor.so)", observe the "invoking constructor" message,
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

#include <iostream>

class C {
public:
    int i;
    C() {
       std::cerr << "[invoking constructor]" << std::endl;
       i = 3;
    }
    ~C() { std::cerr << "[invoking destructor]" << std::endl; }
};

C c;

Object P_Test() {
    return Make_Integer(c.i);
}

extern "C" void elk_init_constructor() {
    Define_Primitive((Object (*)(...))P_Test, "test", 0, 0, EVAL);
}

extern "C" void elk_finit_constructor() {
    std::cerr << "Goodbye." << std::endl;
}
