.so ../util/tmac.scheme
.Ul
.TL
The \s-1OOPS\s0 Package for Elk Scheme
.AU
Oliver Laumann
.
.Ch "Introduction"
.
.PP
The \s-1OOPS\s0 package provides a minimal set of tools that enables
a Scheme programmer to program in an object oriented style.
The functionality of \s-1OOPS\s0 is similar to that of packages like
\s-1CLOS\s0 and \s-1SCOOPS\s0, although the current version does
not support multiple inheritance.
The rest of this memo serves as a reference guide to the
\s-1OOPS\s0 package; the reader is assumed to be familiar with
the terminology of object oriented programming.
.
.Ch "Using \s-1OOPS\s0"
.LP
Programs that make use of the \s-1OOPS\s0 package should include
the line
.Ss
(require 'oops)
.Se
.Ix oops
Since this involves autoloading of an object file, it may be desirable
to dump Scheme after the \s-1OOPS\s0 package has been loaded.
.
.Ch "Defining Classes"
.PP
New classes are defined by means of the
.S define-class
.Id define-class
macro.
The syntax of
.S define-class
is
.Ss
(define-class \f2class-name\fP . \f2options\fP)
.Se
where \f2class-name\fP is a symbol.
\f2options\fP can be of the form
.Ss
(super-class \f2class-name\fP)
.Se
.Id super-class
where \f2class-name\fP is the name of the super-class (a symbol),
or
.Ss
(class-vars . \f2var-specs\fP)
.Se
.Id class-vars
or
.Ss
(instance-vars . \f2var-specs\fP)
.Se
.Id instance-vars
to specify the class variables
.Ix "class variables"
and instance variables
.Ix "instance variables"
of the newly defined class.
Each \f2var-spec\fP is either a symbol (the name of the variable)
or of the form
.Ss
(\f2symbol\fP \f2initializer\fP).
.Se
Variables for which no initializer has been specified are initialized
to the empty list.
The initializers
.Ix initializers
for class variables are evaluated immediately;
initializers for instance variables are evaluated each time an
instance of the newly defined class is created.
Evaluation of initializers is performed in a way that the
initializer of a variable can reference all variables appearing
at the left of the variable being initialized; for instance
.Ss
(define-class foo (class-vars (a 10) (b (* a 2))))
.Se
would initialize the class variable
.S b
to 20.
.PP
A class inherits all class variables, instance variables, and
methods of its super-class.
When a class and its super-class each have an instance variable
with the same name, the corresponding \f2var-specs\fP must either
both have no initializer or initializers with the same value,
otherwise an ``initializer mismatch'' error is signaled by
.S define-class .
.PP
Each instance of a class has an instance variable named
.S self .
.Id self
The value of
.S self
is the instance with respect to which
.S self
is evaluated.
.S self
can be used by methods as the argument to
.S send
.Ix send
(see below) to invoke another method within the current instance.
.PP
.S define-class
does not have a meaningful return value,
instead it has a side-effect on the environment in which it
is invoked.
.
.Ch "Creating Instances of a Class"
.PP
The macro
.S make-instance
.Id make-instance
is used to create an instance of
a class; it returns the instance as its value.
The syntax is
.Ss
(make-instance \f2class\fP . \f2args\fP)
.Se
where \f2class\fP is the class of which an instance is to
be created.
Each \f2arg\fP of the form
.Ss
(\f2symbol\fP\ \f2initializer\fP)
.Se
where \f2symbol\fP is the name of an instance variable of the class,
is used to initialize the specified instance variable in the
newly created instance.
In this case the \f2initializer\fP supersedes any initializer
specified in the call to
.S define-class .
Thus it is possible to have instance variables with a \f2default
initializer\fP that can be overridden for individual instances.
The initializers are evaluated in the current environment.
.PP
.S make-instance
initializes the newly created instance by
invoking the
.S initialize-instance
.Id initialize-instance
method for the class
and all super-classes in super-class to sub-class order.
That is, the
.S initialize-instance
method of the class specified in the call to
.S make-instance
is called after all other
.S initialize-instance
methods.
The arguments passed to the
.S initialize-instance
method of a class are those arguments of the call to
.S make-instance
that do not represent an initialization form for an instance variable.
These arguments are evaluated in the current environment.
It is not required for a class to have an
.S initialize-instance
method.
.PP
Consider the following example:
.Ss
(require 'oops)
.sp .5
(define-class c (instance-vars a))
(define-class d (instance-vars (b 10)) (super-class c))
.sp .5
(define-method c (initialize-instance . args)
  (print (cons 'c args)))
.sp .5
(define-method d (initialize-instance . args)
  (print (cons 'd args)))
.sp .5
.Se
In this example evaluation of
.Ss
(define x 99)
(define i (make-instance d (a 20) 'foo (b x) x))
.Se
would print
.Ss
(c foo 99)
(d foo 99)
.Se
.PP
Note that first the
.S initialize-instance
method of
.S c
is invoked and then that of the class
.S d .
The instance variables
.S a
and
.S b
would be initialized to 20 and 99, respectively.
.
.Ch "Defining Methods"
.PP
A new method can be defined by means of the
.S define-method
.Id define-method
macro.
The syntax is
.Ss
(define-method \f2class\fP \f2lambda-list\fP . \f2body\fP)
.Se
where \f2class\fP is the class to which the method is to be
added, \f2lambda-list\fP is a list specifying the method's
name and formal arguments (having the same syntax as the argument
of
.S define ).
.PP
.S define-method
simply creates a class-variable with the name
of the method, creates a lambda closure using the \f2lambda-list\fP
and the \f2body\fP forms, and binds the resulting procedure to
the newly-created variable.
When a message with the name of the method is sent to an instance
of the class, the method is invoked, and the \f2body\fP is evaluated
in the scope of the instance (so that it can access all instance
and class variables).
.
.Ch "Sending Messages"
.PP
A message can be sent to an instance by means of the function
.S send .
.Id send
The syntax of
.S send
is
.Ss
(send \f2instance\fP \f2message\fP . \f2args\fP)
.Se
where \f2instance\fP is the instance to which the message is
to be sent, \f2message\fP is the name of the method to be
invoked (a symbol), and \f2args\fP are the arguments to be
passed to the method.
Example:
.Ss
(define-class c (instance-vars a) (class-vars (b 10)))
.sp .5
(define-method c (foo x)
  (cons (set! a x) b))     ; set! returns previous value
.sp .5
(define i (make-instance c (a 99)))
.sp
(send i 'foo 1)          \f1returns  (99 . 10)\fP
(send i 'foo 2)          \f1returns  (1 . 10)\fP
.Se
.PP
When a message is sent to an instance for which no method has
been defined, a ``message not understood'' error is signaled.
.PP
The function
.S send-if-handles
.Id send-if-handles
is identical to
.S send ,
except that it returns a list of one element, the return value
of the method, or
.S #f
when the message is not understood by the instance.
.
.Ch "Evaluating Expressions within an Instance"
.PP
The macro
.S with-instance
.Id with-instance
can be used to evaluate expressions within the scope of an instance.
The syntax is
.Ss
(with-instance \f2instance\fP . \f2body\fP).
.Se
The \f2body\fP forms are evaluated in the same environment in
which a method of \f2instance\fP would be evaluated,
i.\|e. they can access all and class and instance variables
(including
.S self ).
.S with-instance
returns the value of the last \f2body\fP form.
Example:
.Ss
(define-class c (class-vars (x 5)) (instance-vars y))
.sp .5
(define i (make-instance c (y 1)))
.sp .5
(define x 10)
(with-instance i (cons x y))          \f1returns  (5 . 1)\fP
.Se
.
.Ch "Setting Instance and Class Variables"
.PP
Generally class and instance variables are manipulated by methods
or, if applicable, from within a
.S with-instance
form.
In addition, values can be assigned to class and instance variables
without involving a message send by means of the
.S instance-set!
.Id instance-set!
macro.
The syntax of
.S instance-set!
is
.Ss
(instance-set! \f2instance\fP \f2variable\fP \f2value\fP)
.Se
where \f2variable\fP is a symbol, the name of the class or
instance variable.
.S instance-set!
returns the previous value of the variable (like
.S set! ).
.PP
Class variables can be modified without involving an instance
of the class by means of the macro
.S class-set! :
.Id class-set!
.Ss
(class-set! \f2class\fP \f2variable\fP \f2value\fP).
.Se
\f2variable\fP must be the name of a class variable of \f2class\fP.
Note that one difference between
.Ss
(instance-set! i 'var x)
.Se
and
.Ss
(with-instance i (set! var x))
.Se
is that in the former case
.S x
is evaluated in the current environment while in the latter case
.S x
is evaluated within the scope of the instance (here
.S x
might be a class or instance variable).
.
.Ch "Obtaining Information about Classes and Instances"
.PP
The function
.S class-name
.Id class-name
returns the name of a class (a symbol) or, when applied to an instance,
the name of the class of which it is an instance.
.PP
The predicate
.S method-known?
.Id method-known?
can be used to check whether a method of a given name is known to a class.
The syntax is
.Ss
(method-known? \f2method\fP \f2class\fP)
.Se
where \f2method\fP is a symbol.
.PP
The type predicates
.S class?
.Id class?
and
.S instance?
.Id instance?
can be used to check whether an object is a class or an instance,
respectively.
.PP
The functions
.Ss
(check-class \f2symbol\fP \f2object\fP)
.Se
.Id check-class
and
.Ss
(check-instance \f2symbol\fP \f2object\fP)
.Se
.Id check-instance
check whether \f2object\fP is a class (i.\|e. satisfies the predicate
.S class? )
or an instance, respectively, and, if not, signal an error;
in this case \f2symbol\fP is used as the first argument to
.S error .
.PP
The functions
.S describe-class
.Id describe-class
and
.S describe-instance
.Id describe-instance
print the components (name, class/instance variables, etc.) of
a class or instance, respectively.
The function
.S describe
.Id describe
has been extended in way that when
.S "(feature? 'oops)"
is true,
.S describe-class
or
.S describe-instance
are called when
.S describe
is applied to an object that satisfies
.S class?
or
.S instance? ,
respectively.
