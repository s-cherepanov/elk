/*-----------------------------------------------------------------------------

This trivial Elk extension demonstrates encapsulation of a C++ class in
a first-class Scheme type, and encapsulation of member functions in
Scheme primitives.

See constructor.cpp in this directory for compilation instructions.

Here is a transcript showing a test run under Linux using the
GNU g++ compiler:

    % g++ -shared -fPIC -I/usr/include/elk class.cpp -o class.so -lelk
    %
    % scheme
    > (load 'class.so)

    > (define x (make-foo))
    x
    > (read-val x)
    1234
    > (write-val! x 11)

    > (read-val x)
    11
    > (exit)
    %

-----------------------------------------------------------------------------*/

class foo {
    int val;
public:
    int read_val(void);
    void write_val(int);
    foo() { val = 1234; };
};

int foo::read_val(void) {
    return val;
}

void foo::write_val(int newval) {
    val = newval;
}

/* ---------------------------------- */

#include "scheme.h"

struct S_Foo {
    Object tag; class foo foo;
};

int T_Foo;

#define FOO(x)  ((struct S_Foo *)POINTER(x))

Object P_Make_Foo(void) {
    Object f = Alloc_Object(sizeof (struct S_Foo), T_Foo, 0);
    FOO(f)->foo.write_val(1234); /* FOO(f)->foo.foo() is not allowed?! */
    return f;
}

Object P_Read_Val(Object x) {
    Check_Type(x, T_Foo);
    return Make_Integer(FOO(x)->foo.read_val());
}

Object P_Write_Val(Object x, Object y) {
    Check_Type(x, T_Foo);
    FOO(x)->foo.write_val(Get_Integer(y));
    return Void;
}

int Foo_Print(Object h, Object port, int raw, int depth, int length) {
    Printf(port, "#[foo %d]", FOO(h)->foo.read_val());
    return 0;
}

int Foo_Equal(Object x, Object y) {
    return FOO(x)->foo.read_val() == FOO(y)->foo.read_val();
}

extern "C" void elk_init_foo() {
    T_Foo = Define_Type(0, "foo", NOFUNC, sizeof(struct S_Foo),
        Foo_Equal, Foo_Equal, Foo_Print, NOFUNC);
    Define_Primitive((Object(*)(...))P_Make_Foo,  "make-foo",     0, 0, EVAL);
    Define_Primitive((Object(*)(...))P_Read_Val,  "read-val",     1, 1, EVAL);
    Define_Primitive((Object(*)(...))P_Write_Val, "write-val!",   2, 2, EVAL);
}

