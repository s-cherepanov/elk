.so ../util/tmac.scheme
.Ul
.TL
Reference Manual for the
.sp .5
Elk Record Extension
.AU
Oliver Laumann
.
.Ch "Introduction"
.
.PP
The record extension to Elk allows Scheme applications to define
.Ix "record data type"
record data types (similar to, although less powerful than,
Common Lisp
.Ix structures
\f2structures\fP).
.PP
A record type can be instantiated to obtain a new
.Ix record
record (a member of the given record type).
Each record is a collection of named
.Ix fields
fields that can hold arbitrary Scheme objects.
Records are first-class Scheme objects; they are members of the
\f2record\fP data type that is disjoint from all other Scheme types.
Record types are first-class objects as well; each record type is a
member of the \f2record-type\fP Scheme data type.
.PP
The record extension provides facilities to define new record types,
create
.Ix instances
instances of existing record types, define
.Ix accessor
accessor and
.Ix modifier
modifier functions to read and write the fields of records of a
given type, and
.Ix "type predicate"
type predicates for the \f2record\fP and \f2record-type\fP data types.
.PP
In addition, the extension provides
.Ix macros
macros that simplify the definition of
.Ix constructor
constructor, accessor and modifier functions for a newly defined record type.
.
.Ch "Using the Record Extension"
.
.PP
The record extension is loaded by evaluating
.Ss
(require 'record)
.Se
in the interactive toplevel or in a Scheme program.
.PP
This causes the files
.Ix record.scm
\f2record.scm\fP and
.Ix record.o
\f2record.o\fP to be loaded into the interpreter (\f2record.o\fP has to
be linked with the interpreter on platforms that do not support dynamic
loading of object files).
.PP
Loading the record extension causes the
.Ix feature
features \f2record\fP and \f2record.o\fP to be provided.
.
.Ch "Record Types"
.
.Pr make-record-type type-name fields
.LP
\f2make-record-type\fP creates a new
.Ix "record type"
record type.
\f2type-name\fP (a string or a symbol) is the name of the record type;
it is used in the printed representation of the record type and of
records belonging to the new type.
If \f2type-name\fP is a symbol, it is converted into a string first.
.Ix fields
\f2fields\fP is a list of symbols naming the fields of a record of
the new type.
An error is signaled of the list contains duplicate names.
.LP
\f2make-record-type\fP returns the new record type (an object of the
Scheme type \f2record-type\fP).
.LP
Example:
.Ss
(define time-record
  (make-record-type 'time '(hours minutes seconds)))
.Se
.
.Pr record-type? obj
.LP
This
.Ix "type predicate"
type predicate returns #t if \f2obj\fP is a \f2record-type\fP
object (i.\|e.\& the return value of a call to \f2make-record-type\fP),
#f otherwise.
.
.Pr record-type-name rt
.LP
This procedure returns the
.Ix "type name"
type name (a string) of the record type \f2rt\fP,
i.\|e.\& the \f2type-name\fP argument that was supplied to the call
to \f2make-record-type\fP that created the record type \f2rt\fP.
.
.Pr record-type-field-names rt
.LP
\f2record-type-field-names\fP returns the list of
.Ix "field names"
field names associated with the record type \f2rt\fP (a list of
symbols), i.\|e.\& the \f2fields\fP argument that was given in the call to
.Ix make-record-type
\f2make-record-type\fP that created \f2rt\fP.
.
.[[
.Pr record-constructor rt fields
.Pr record-constructor rt
.]]
.LP
\f2record-constructor\fP returns a procedure for creating
.Ix instances
instances of the record type \f2rt\fP.
.LP
The returned procedure accepts as many arguments as there are symbols
in the list \f2fields\fP; these arguments are used as the initial
values of those
.Ix fields
fields in the newly created record instance.
The values of any fields for which no
.Ix "initial value"
initial value is specified (i.\|e.
that are not present in \f2fields\fP) are undefined.
If the \f2fields\fP argument is omitted, the field names that were given
as an argument in the call to
.Ix make-record-type
\f2make-record-type\fP that created the record type \f2rt\fP are used instead.
.LP
Example:
.Ss
(define make-time
   (record-constructor time-record))
(define noon (make-time 12 0 0))
.sp .5
(define make-uninitialized-time
   (record-constructor time-record '()))
.Se
.
.Pr record-predicate rt
.LP
\f2record-predicate\fP returns a procedure for testing membership
in the record type \f2rt\fP.
The returned procedure accepts one argument and returns #t if the
argument is a member of the record type \f2rt\fP (i.\|e.\& if it
has been created by invoking a constructor returned by calling
.Ix record constructor
\f2record-constructor\fP with \f2rt\fP as an argument), #f otherwise.
.
.Pr record-accessor rt field
.LP
\f2record-accessor\fP returns a procedure for reading the value of the
.Ix field
field named by \f2field\fP of a member of the record type \f2rt\fP.
The returned procedure accepts one argument, which must be a record
of the record type \f2rt\fP; it returns the current value of the
specified field in that record.
\f2field\fP must be a member of the list of field names that was supplied
to the call to
.Ix make-record-type
\f2make-record-type\fP that created \f2rt\fP.
.LP
Example:
.Ss
(define time-hours
  (record-accessor time-record 'hours))
.sp .5
(define noon ((record-constructor time-record) 12 0 0))
(time-hours noon)  \f2\(-> 12\fP
.Se
.
.Pr record-modifier rt field
.LP
\f2record-modifier\fP returns a procedure for writing the value of the
.Ix field
field named by \f2field\fP of a member of the record type \f2rt\fP.
The returned procedure accepts two arguments: a record
of the record type \f2rt\fP and an arbitrary object; it stores the given
object into the specified field in that record and returns the
previous value of the field.
\f2field\fP must be a member of the list of field names that was supplied
to the call to
.Ix make-record-type
\f2make-record-type\fP that created \f2rt\fP.
.LP
Example
.Ss
(define set-time-hours!
  (record-modifier time-record 'hours))
.Se
.
.Pr describe-record-type rt
.LP
This procedure prints the names of the
.Ix fields
fields associated with the record type \f2rt\fP; it is automatically
invoked by the standard
.Ix describe
\f2describe\fP procedure of Elk if \f2describe\fP is invoked with a
record type.
.
.Ch "Records"
.
.Pr record? obj
.LP
This
.Ix "type predicate"
type predicate returns #t if \f2obj\fP is an object of type \f2record\fP
(i.\|e.\& the return value of a call to a record
.Ix constructor
constructor of any record type), #f otherwise.
.
.Pr record-type-descriptor record
.LP
This procedure returns the
.Ix "record type"
record type representing the type of the given record.
The returned record type object is equal (in the sense of \f2eq?\fP)
to the record type argument that was passed to
.Ix record-constructor
\f2record-constructor\fP in the call that created the constructor
procedure that created \f2record\fP.
.LP
Example: evaluating the expression
.Ss
((record-predicate (record-type-descriptor r)) r)
.Se
always yields #t for any given record \f2r\fP.
.
.Pr record-values record
.LP
\f2record-values\fP returns the current contents of the fields of
\f2record\fP as a
.Ix vector
vector.
The \f2n\fPth element of the vector corresponds to the field with the
name given as the \f2n\fPth element of the \f2fields\fP argument in
the call to
.Ix make-record-type
\f2make-record-type\fP that created the type to which \f2record\fP belongs.
.LP
The returned vector is not a copy of the actual fields; i.\|e.\& modifying
the contents of the vector directly writes the corresponding fields
of the record.
.
.Pr describe-record record
.LP
This procedure prints the names and current values of the
.Ix fields
fields of the given record; it is automatically invoked by the standard
.Ix describe
\f2describe\fP procedure of Elk if \f2describe\fP is invoked with a record.
.
.Ch "Convenience Macros"
.PP
The 
.Ix macros
macros described in this section are loaded by evaluating
.Ss
(require 'recordutil)
.Se
after having loaded the record extension.
This causes the file
.Ix recordutil.scm
\f2recordutil.scm\fP to be loaded and defines the
.Ix feature
.Ix recordutil
feature \f2recordutil\fP.
.
.Sy define-record-type name fields
.LP
This macro defines a variable \f2<name>-record\fP, invokes the procedure
.Ix make-record-type
\f2make-record-type\fP with the given \f2name\fP and
\f2fields\fP, and assigns the result to this variable.
In addition, \f2define-record-type\fP defines a
.Ix "type predicate"
type predicate for the new record type as \f2<name>\-record?\fP and a
.Ix constructor
constructor function as \f2make\-<name>\-record\fP.
The constructor function accepts no arguments and returns an
uninitialized record of the newly defined record type.
.LP
Example:
.Ss
(require 'record)
(require 'recordutil)
.sp .5
(define-record-type
  time (hours minutes seconds))
.sp .3
(record-type? time-record)  \f2\(-> #t\fP
.sp .3
(define t (make-time-record))
.sp .3
(time-record? t)  \f2\(-> #t\fP
.Se
.
.[[
.Sy define-record-accessors rt
.Sy define-record-modifiers rt
.]]
.LP
The macro \f2define-record-accessors\fP (\f2define-record-modifiers\fP)
defines
.Ix accessor
.Ix modifier
accessor (modifier) functions for the fields of the record type \f2rt\fP.
For each field named \f2field\fP, \f2define-record-accessors\fP
(\f2define-record-modifiers\fP) defines a function \f2<name>\-<field>\fP
(\f2set\-<name>\-<field>!\fP), where \f2name\fP is the type name of
the given record type.
Each of the functions is the result of a call to
.Ix record-accessor
.Ix record-modifier
\f2record-accessor\fP (\f2record-modifier\fP) as described above, with
the arguments \f2rt\fP and the name of the field.
.LP
Example:
.Ss
(define-record-type time (hours minutes seconds))
(define-record-modifiers time-record)
.sp .3
(define noon (make-time-record))
(set-time-hours! noon 12)
(set-time-minutes! noon 0)
(set-time-seconds! noon 0)
.sp .5
(define-record-accessors time-record)
.sp .3
(time-hours noon)  \f2\(-> 12\fP
.Se
