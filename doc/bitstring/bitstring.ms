.so ../util/tmac.scheme
.Ul
.TL
Reference Manual for the
.sp .5
Elk Bit String Extension
.AU
Oliver Laumann
.
.Ch "Introduction"
.
.PP
The bit string extension to Elk defines a new data type \f2bitstring\fP
(a sequence of zero or more bits) and functions to create and
manipulate bit strings.
The bits in a bit string are numbered beginning from zero up to the
number of bits minus one; bit number 0 is the
.Ix "least significant bit"
.Ix LSB
least significant bit (LSB), and the highest numbered bit is the
.Ix "most significant bit"
.Ix MSB
most significant bit (MSB).
.PP
The
.Ix "print representation"
print representation of bit strings is introduced by the sequence
`#*'; the bits are printed starting with the most significant bit.
Likewise, in the reader the sequence `#*' introduces a bit string
constant.
.LP
Example:
.Ss
#*0100110111
.sp .5
#*             \f2(empty bit string)\fP
.Se
.
.Ch "Using the Bit String Extension"
.
.PP
To load the bit string extension, evaluate the expression
.Ss
(require 'bitstring)
.Se
.PP
This causes the files
.Ix bitstring.scm
\f2bitstring.scm\fP and
.Ix bitstring.o
\f2bitstring.o\fP to be loaded (\f2bitstring.o\fP must be statically
linked with the interpreter on platforms that do not support dynamic
loading of object files).
.PP
Loading the bit string extension causes the
.Ix feature
features \f2bitstring\fP and \f2bitstring.o\fP to be provided.
.
.Ch "Creating Bit Strings"
.
.Pr make-bitstring length init
.LP
\f2make-bitstring\fP returns a new bit string of the given length.
If init is #t, all bits are initialized to 1; if init is #f, all
bits are initialized to 0.
.
.Pr bitstring-copy bitstring
.LP
This procedure returns a copy of the specified bit string.
.
.Pr bitstring-append bitstring\*1 bitstring\*2
.LP
\f2bitstring-append\fP returns a new bit string holding the
.Ix concatenation
concatenation of the specified bit string arguments.
.
.Ch "Bit String Predicates"
.
.Pr bitstring? obj
.LP
This
.Ix "type predicate"
type predicate returns #t if \f2obj\fP is a bit string, #f otherwise.
.
.Pr bitstring=? bitstring\*1 bitstring\*2
.LP
This procedure returns #t if the bit string arguments are of the same
length and contain the same bits, #f otherwise.
.
.Pr bitstring-zero? bitstring
.LP
\f2bitstring-zero?\fP returns #t if the specified bit string
contains only 0 bits, #f otherwise.
.
.Ch "Integer Conversions"
.
.[[
.Pr unsigned-integer\(mi>bitstring length i
.Pr signed-integer\(mi>bitstring length i
.]]
.LP
Both procedures convert the exact integer argument \f2i\fP into a bit
string of \f2length\fP bits and return the bit string.
\f2length\fP must be large enough to hold the bit string
representation of \f2i\fP.
The integer argument to \f2unsigned-integer->bitstring\fP must be
non-negative.
\f2signed-integer->bitstring\fP uses
.Ix "two's complement"
two's complement representation for negative integers.
.
.[[
.Pr bitstring\(mi>unsigned-integer bitstring
.Pr bitstring\(mi>signed-integer bitstring
.]]
.LP
Both procedures convert the given bit string into an integer.
\f2bitstring->signed-integer\fP interprets the bit string as the
.Ix "two's complement"
two's complement representation of a signed integer.
.
.Ch "Selecting Components of Bit Strings"
.
.Pr bitstring-length bitstring
.LP
This procedure returns the number of bits in the specified bit string.
.
.Pr bitstring-ref bitstring index
.LP
\f2bitstring-ref\fP returns #t if the \f2index\fP-th bit in the
given bit string is 1, #f otherwise.
.
.Pr bitstring-substring bitstring from to
.LP
This procedure returns a new bit string initialized with the bits
of \f2bitstring\fP starting at the index \f2from\fP (inclusive)
and ending at the index \f2to\fP (exclusive).
.
.Ch "Modifying Bit Strings"
.
.Pr bitstring-fill! bitstring init
.LP
This procedure sets all bits in the specified bit string to 1 if
\f2init\fP is #t, or to 0 if \f2init\fP is #f.
It returns the non-printing object.
.
.Pr bitstring-set! bitstring index init
.LP
\f2bitstring-set!\fP sets the \f2index\fP-th bit in the specified
bit string to 1 if \f2init\fP is #t, or to 0 if \f2init\fP is #f.
It returns the non-printing object.
.
.Pr bitstring-move! dst-bitstring src-bitstring
.LP
\f2bitstring-move!\fP destructively copies the contents of the
bit string \f2src-bitstring\fP into \f2dst-bitstring\fP.
Both bit strings must have the same length.
It returns the non-printing object.
.
.Pr bitstring-substring-move! src-bitstring from\*1 to\*1 dst-bitstring from\*2
.LP
This procedure destructively copies the bits from \f2src-bitstring\fP
starting at index \f2from\*1\fP (inclusive) and ending at index \f2to\*1\fP
(exclusive) into \f2dst-bitstring\fP starting at index \f2from\*2\fP
(inclusive).
.Ix overlapping
Overlapping is handled correctly.
The procedure returns the non-printing object.
.
.Ch "Bitwise Logical Operations"
.
.Pr bitstring-not bitstring
.LP
This procedure returns a new bit string initialized to the
bitwise logical negation of the given bit string.
.
.Pr bitstring-not! dst-bitstring src-bitstring
.LP
This procedure destructively overwrites the contents of \f2dst-bitstring\fP
with the bitwise logical negation of the bits in \f2src-bitstring\fP.
Both bit strings must have the same length.
\f2bitstring-not!\fP returns the non-printing object.
.
.[[
.Pr bitstring-and bitstring\*1 bitstring\*2
.Pr bitstring-andnot bitstring\*1 bitstring\*2
.Pr bitstring-or bitstring\*1 bitstring\*2
.Pr bitstring-xor bitstring\*1 bitstring\*2
.]]
.LP
These procedures return a new bit string initialized to the bitwise logical
\f2and\fP (logical \f2and\fP with the negation, logical \f2or\fP,
logical exclusive \f2or\fP, respectively) of the two bit string arguments.
The two bit strings must have the same length.
.
.[[
.Pr bitstring-and! dst-bitstring src-bitstring
.Pr bitstring-or! dst-bitstring src-bitstring
.Pr bitstring-andnot! dst-bitstring src-bitstring
.Pr bitstring-xor! dst-bitstring src-bitstring
.]]
.LP
These procedures are the destructive versions of the four bitwise
logical procedures described above.
They perform the corresponding logical operation on the two bit string
arguments and overwrite the contents of \f2dst-bitstring\fP with the
result.
Both bit strings must have the same length.
These procedures return the non-printing object.
