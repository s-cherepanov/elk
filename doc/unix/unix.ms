.so ../util/tmac.scheme
.Ul
.TL
Reference Manual for the
.sp .5
Elk UNIX Extension
.AU
Oliver Laumann
.
.Ch "Introduction"
.
.PP
This reference manual describes the primitive procedures and record types
defined by the UNIX extension to Elk.
.PP
The UNIX extension provides easy access to most widely available
UNIX system calls and C library functions from within Scheme programs.
The extension supports a wide range of different UNIX platforms without
restricting its functionality to the lowest common denominator or the
.Ix POSIX
POSIX 1003.1 functions.
To simplify writing portable Scheme programs, the extension
attempts to hide differences between the types of supported UNIX flavors.
For example, programmers do not have to deal with the idiosyncrasies
of the \f2wait()\fP, \f2waitpid()\fP, \f2wait3()\fP, and \f2wait4()\fP
system calls or the \f2mktemp()\fP, \f2tmpnam()\fP, and \f2tempnam()\fP
functions.
.PP
The UNIX extension defines procedures for low-level,
file-descriptor-based I/O; creation of pipes; file/record locking; file
and directory system calls; process creation and control; signal
handling; error handling; and obtaining information about date, time,
users, limits, process resources, etc.
Terminal control is not yet supported by the current version.
.PP
The reference manual assumes that you are familiar with the most common
UNIX system calls and C library functions; this document does not
attempt to duplicate the contents of the standard UNIX documentation.
Detailed descriptions are provided for functions that differ from
the standard UNIX semantics.
.
.Ch "Using the UNIX Extension"
.
.PP
The UNIX extension is loaded by evaluating
.Ss
(require 'unix)
.Se
in the interactive toplevel or in a Scheme program.
.PP
This causes the files \f2unix.scm\fP and \f2unix.o\fP to be loaded
into the interpreter (\f2unix.o\fP has to be linked with the
interpreter on platforms that do not support dynamic loading of
object files).
In addition, the
.Ix "record extension"
\f2record\fP extension is automatically loaded if it is not yet present.
The record extension is documented in a separate reference manual.
.PP
Loading the UNIX extension causes the
.Ix features
features \f2unix\fP and \f2unix.o\fP to be provided.
Optionally, one or more of the following features (described below) may
be provided by the extension to indicate that certain UNIX features
are available:
.Ss
unix:wait-options
unix:wait-process
unix:record-locks
unix:file-locking
unix:symlinks
unix:reliable-signals
.Se
.Ix unix:wait-options
.Ix unix:wait-process
.Ix unix:record-locks
.Ix unix:file-locking
.Ix unix:symlinks
.Ix unix:reliable-signals
.
.Ch "Record Types"
.
.PP
Several procedures return their results as Scheme
.Ix records
records.
All record types defined by the UNIX extension are stored in variables
with names of the form \f2<something>-record\fP (such as
\f2system-record\fP or \f2passwd-record\fP).
In addition, a type predicate, a record constructor, and accessor functions
.Ix "type predicate"
.Ix "record constructor"
.Ix "accessor functions"
for all record fields are defined for each record type.
For example, a \f2system\fP record type with the fields \f2hostname\fP,
\f2sysname\fP, and \f2osname\fP is defined, resulting in variable
\f2system-record\fP holding the record type descriptor, and the functions
.Ss
(system-record? obj)
(make-system-record)
.sp .5
(system-hostname system-record)
(system-sysname system-record)
(system-osname system-record)
.Se
Use \f2define-record-modifiers\fP if you need the
.Ix "modifier functions"
modifier functions for any of the records as well (see the record
extension reference manual for details).
.LP
The following
.Ix "record types"
record types are defined by the UNIX extension:
.sp .5
.KS
.TS
allbox, tab(~);
c c
lf5 lf5.
Record Type~Fields
_
stat~type mode ino dev nlink uid gid size atime mtime ctime
time~T{
seconds minutes hours day-of-month month year weekday
.br
day-of-year dst
T}
nanotime~nanoseconds minuteswest dst
system~hostname sysname osname
passwd~name password uid gid gecos homedir shell
group~name password gid members
resources~user-time system-time (...)
lock~exclusive? whence start length
wait~pid status code core-dump? resources
.TE
.KE
.Ix stat-record
.Ix time-record
.Ix nanotime-record
.Ix system-record
.Ix passwd-record
.Ix group-record
.Ix resources-record
.Ix lock-record
.Ix wait-record
.
.Ch "Error Handling"
.
.PP
The default error behavior of the primitive procedures defined by
the UNIX extension is to invoke the standard Scheme
.Ix "error handler"
error handler if a UNIX system call or library function fails.
As an alternative, if a specific error action is to be performed
by the application, a primitive procedure can be invoked under
control of the
.Ix unix-errval
\f2unix-errval\fP form.
In this case, a unique
.Ix "error object"
\f2error object\fP is returned if a UNIX function signals an error.
The standard UNIX system error message and the UNIX error number
are made available to the application in any case.
Details of the error handling facilities are described in the
section ``Error Functions'' below.
.
.Ch "Conventions"
.
.PP
In the following sections, the names of procedure arguments can
dictate that the arguments are of specific types.
If an argument name is also the name of a Scheme data
type, the argument must be an object of that type.
For example, a procedure with an argument named \f2string\fP must
be invoked with a string.
File descriptor arguments (named \f2fdescr\fP, or \f2fdescr\*1\fP,
\f2fdescr\*2\fP, etc.) and arguments named \f2length\fP are always
non-negative integers;
filename arguments (\f2filename\fP) are strings or symbols;
and arguments with the suffix ``?'' are booleans.
.
.Ch "Low-Level I/O, File Descriptors"
.
.Pr unix-close fdescr
.LP
.Ix close
The UNIX \f2close()\fP system call.
\f2unix-close\fP returns the non-printing object.
.
.[[
.Pr unix-dup fdescr
.Pr unix-dup fdescr\*1 fdescr\*2
.]]
.LP
.Ix dup
.Ix dup2
\f2unix-dup\fP invokes the \f2dup()\fP (first form) or \f2dup2()\fP
(second form) system call.
The result is a new file descriptor (an integer).
.
.[[
.Pr unix-open filename flags
.Pr unix-open filename flags mode
.]]
.LP
.Ix open
The UNIX \f2open()\fP system call.
\f2flags\fP is a list of one or more symbols specifying the bitmask
argument of the \f2open()\fP system call.
.LP
At least the flag symbols \f2read\fP, \f2write\fP, \f2append\fP,
\f2create\fP, \f2truncate\fP, and \f2exclusive\fP are supported;
additional symbols (such as \f2ndelay\fP) may be permitted on
certain platforms.
The procedure \f2unix-list-open-modes\fP can be used to obtain the list
of flag symbols that are supported (see below).
If \f2create\fP is present in the \f2flags\fP argument, the \f2mode\fP
argument (an integer) must be supplied.
At least one of the symbols \f2read\fP or \f2write\fP must be present
in \f2flags\fP.
.LP
\f2unix-open\fP returns a new file descriptor (an integer).
.LP
Example:
.Ss
(let ((f1 (unix-open "/etc/passwd" '(read))
      (f2 (unix-open "temp" '(read write create truncate) #o666))))
  ...)
.Se
.
.Pr unix-list-open-modes
.LP
This procedure returns the list of \f2flag\fP symbols for the
\f2unix-open\fP procedure that are supported on the local platform.
.
.Pr unix-lseek fdescr offset whence
.LP
.Ix lseek
The UNIX \f2lseek()\fP system call.
\f2offset\fP is an integer; \f2whence\fP is one of
the symbols \f2set\fP, \f2current\fP, and \f2end\fP.
\f2unix-lseek\fP returns the new file position as an integer.
.
.Pr unix-pipe
.LP
.Ix pipe
The \f2pipe()\fP system call.
\f2unix-pipe\fP returns two file descriptors as a pair of integers.
.
.[[
.Pr unix-read-string-fill! fdescr string
.Pr unix-read-string-fill! fdescr string length
.]]
.LP
.Ix read
The \f2read()\fP system call.
\f2unix-read-string-fill\fP invokes \f2read()\fP
with the Scheme string as input buffer and the length of the string
argument (first form) or the length supplied as a third argument
(second form).
If \f2length\fP is specified, it must be an integer between 0 and
the length of \f2string\fP.
.LP
\f2unix-read-string-fill!\fP destructively overwrites the contents of the
\f2string\fP argument.
It returns the number of characters actually read (0 on EOF).
.
.[[
.Pr unix-write fdescr string
.Pr unix-write fdescr string length
.]]
.LP
.Ix write
The \f2write()\fP system call.
For a description of the arguments see \f2unix-read-string-fill!\fP above.
\f2unix-write\fP returns the number of characters actually written.
.
.[[
.Pr unix-close-on-exec fdescr
.Pr unix-close-on-exec fdescr on?
.]]
.LP
\f2unix-close-on-exec\fP returns the value of the
.Ix close-on-exec
\f2close-on-exec\fP flag for the given file descriptor as a boolean.
If invoked with a second argument, the procedure sets the
\f2close-on-exec\fP flag to the specified value and returns the
previous value.
.
.[[
.Pr unix-filedescriptor-flags fdescr
.Pr unix-filedescriptor-flags fdescr flags
.]]
.LP
\f2unix-file-descriptor-flags\fP obtains the flags currently active
for the given file descriptor (by means of the
.Ix fcntl
\f2fcntl()\fP system call) and returns them as a list of symbols.
If invoked with a second arguments (a list of symbols), the procedure
sets the flags to that argument and returns the previous value.
.LP
At least the flag symbol \f2append\fP is supported;
additional symbols (such as \f2ndelay\fP or \f2sync\fP) may be permitted on
certain platforms.
The procedure \f2unix-list-filedescriptor-flags\fP can be used to obtain
the list of file descriptor flags that are supported (see below).
.LP
Example:
.Ss
;;; Enable non-blocking I/O for file descriptor (assumes POSIX)
.sp .4
(define (set-non-blocking fd)
  (let ((flags (unix-filedescriptor-flags fd)))
    (unix-filedescriptor-flags fd (cons 'nonblock flags))))
.Se
.
.Pr unix-list-filedescriptor-flags
.LP
This procedure returns the list of file descriptor \f2flag\fP symbols
that can be returned and set by \f2unix-filedescriptor-flags\fP
on the local platform.
.
.Pr unix-num-filedescriptors
.LP
\f2unix-num-filedescriptors\fP returns the maximum number of file
descriptors per process in the local system.
Depending on the UNIX flavor, the procedure invokes
.Ix getdtablesize
.Ix sysconf
\f2getdtablesize()\fP or \f2sysconf()\fP or uses a static (compile-time)
limit.
.
.Pr unix-isatty? fdescr
.LP
Returns #t if the specified file descriptor points to a terminal
device, #f otherwise (the UNIX
.Ix isatty
\f2isatty()\fP library function).
.
.Pr unix-ttyname fdescr
.LP
.Ix ttyname
The UNIX \f2ttyname()\fP function.
Returns the name of a terminal device as a string, or #f if the
file descriptor is not associated with a terminal.
.
.Pr unix-port-filedescriptor port
.LP
This procedure returns the file descriptor associated with the
.Ix "file pointer"
file pointer conveyed in the specified Scheme port.
An error is signaled if the port has already been closed or if it
is a string port.
\f2unix-port-filedescriptor\fP invokes the UNIX
.Ix fileno
\f2fileno()\fP library function.
.LP
Manipulating a file descriptor obtained by \f2unix-port-filedescriptor\fP
can cause unexpected interactions with the standard Scheme I/O functions
and with the stdio buffering mechanism.
In particular, it is not a good idea to close the file descriptor
associated with the Scheme system's current input port or current
output port.
.LP
Example:
.Ss
(let ((stdout-fileno
        (unix-port-filedescriptor (current-output-port))))
  (if (unix-isatty? stdout-fileno)
      (begin
        (display (unix-ttyname stdout-fileno))
        (newline))))
.Se
.
.Pr unix-filedescriptor\(mi>port fdescr type
.LP
Creates a Scheme port with a file pointer containing the specified
file descriptor.
\f2unix-filedescriptor\(mi>port\fP is based on the
.Ix fdopen
\f2fdopen()\fP stdio function.
\f2type\fP is a string and is used as the second argument for
\f2fdopen()\fP.
.LP
The type of the newly created Scheme port is determined by the
\f2type\fP argument.
If \f2type\fP begins with the character #\\r, an input port is created;
#\\w and #\\a indicate an output port.
If the second character of \f2type\fP is #\\+ (\f2update\fP), an
input-output (bidirectional) port is created.
.LP
No filename is associated with a Scheme port created by a call
to \f2unix-filedescriptor\(mi>port\fP.
Instead, the string \f2unix-filedescriptor[%d]\fP (where \f2%d\fP is
replaced by the numerical value of the file descriptor) will be
returned by calls to \f2port-file-name\fP and displayed when printing
the port.
.LP
Note that the file descriptor is closed by the
.Ix "garbage collector"
garbage collector when the Scheme port becomes inaccessible.
.
.Ch "Files and Directories"
.
.Pr unix-stat file
.LP
.Ix stat
.Ix fstat
The UNIX \f2stat()\fP/\f2fstat()\fP system call.
\f2file\fP is either a filename or a file descriptor.
.LP
\f2unix-stat\fP returns a
.Ix stat-record
\f2stat-record\fP with the following fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
_
type~symbol~file type
mode~integer~file access mode
ino~integer~inode number
dev~integer~device number
nlink~integer~number of links to file
uid~integer~file owner's user-ID
gid~integer~file owner's group-ID
size~integer~file size
atime~integer~last access time
mtime~integer~last modified time
ctime~integer~last inode change time
.TE
.KE
.LP
The file type is one of the symbols \f2directory\fP,
\f2character-special\fP, \f2block-special\fP, \f2regular\fP,
\f2symlink\fP, \f2socket\fP, \f2fifo\fP, or \f2unknown\fP.
.
.Pr unix-access? filename mode
.LP
\f2unix-access?\fP is based on the
.Ix access
\f2access()\fP system call.
\f2mode\fP is a list of zero or more of the symbols \f2read\fP,
\f2write\fP, and \f2execute\fP.
The empty list can be used to test for existence of the file.
The procedure returns #t if the specified access is granted, #f otherwise.
.
.Pr unix-chdir filename
.LP
.Ix chdir
The UNIX \f2chdir()\fP system call.
\f2unix-chdir\fP returns the non-printing object.
.
.Pr unix-chmod filename mode
.LP
.Ix chmod
The UNIX \f2chmod()\fP system call.
\f2mode\fP is an integer.
\f2unix-chmod\fP returns the non-printing object.
.
.Pr unix-chown filename uid gid
.LP
.Ix chown
The UNIX \f2chown()\fP system call.
\f2uid\fP and \f2gid\fP are integers.
\f2unix-chown\fP returns the non-printing object.
.
.Pr unix-unlink filename
.LP
.Ix unlink
The UNIX \f2unlink()\fP system call.
\f2unix-unlink\fP returns the non-printing object.
.
.Pr unix-link filename\*1 filename\*2
.LP
.Ix link
The UNIX \f2link()\fP system call.
\f2unix-link\fP returns the non-printing object.
.
.Pr unix-rename filename\*1 filename\*2
.LP
.Ix rename
The UNIX \f2rename()\fP system call.
\f2unix-rename\fP returns the non-printing object.
.LP
On platforms where the \f2rename()\fP function is not available,
the operation is performed by the equivalent sequence of
.Ix link
.Ix unlink
\f2link()\fP and \f2unlink()\fP calls with interrupts disabled
(certain restrictions apply in this case, e.\|g.\& directories cannot
be renamed).
.
.Pr unix-mkdir filename mode
.LP
.Ix mkdir
The UNIX \f2mkdir()\fP system call.
\f2mode\fP is an integer.
\f2unix-mkdir\fP returns the non-printing object.
.
.Pr unix-rmdir filename
.LP
.Ix rmdir
The UNIX \f2rmdir()\fP system call.
\f2unix-rmdir\fP returns the non-printing object.
.
.[[
.Pr unix-utime filename
.Pr unix-utime filename atime mtime
.]]
.LP
.Ix utime
The UNIX \f2utime()\fP function.
\f2unix-utime\fP sets the last access and last modification time of
the given file to the current time (first form) or to the specified
times (second form).
\f2atime\fP and \f2mtime\fP are integers.
\f2unix-utime\fP returns the non-printing object.
.
.Pr unix-read-directory filename
.LP
This procedure returns the contents of the specified directory
as a list of filenames (strings).
\f2filename\fP must be the name of a directory.
\f2unix-read-directory\fP is based on the
.Ix opendir
.Ix readdir
.Ix closedir
\f2opendir()\fP, \f2readdir()\fP, and \f2closedir()\fP functions.
.LP
Example:
.Ss
;;; Return directory contents as list of (filename . filetype) pairs
.sp .4
(define (get-files-and-types directory)
  (map
    (lambda (file)
      (cons file (stat-type (unix-stat file))))
    (unix-read-directory directory)))
.sp .6
(pp (get-files-and-types "."))
.Se
.
.KS
.[[
.Pr unix-tempname
.Pr unix-tempname directory
.Pr unix-tempname directory prefix
.]]
.KE
.LP
\f2unix-tempname\fP returns a pathname that can be used as the name of a
.Ix "temporary file"
temporary file (typically in
.Ix /tmp
.Ix /usr/tmp
/tmp or /usr/tmp).
The newly created pathname is not the name of an existing file.
.LP
\f2directory\fP (a string or symbol) can be used to specify the
directory component of the pathname;
\f2prefix\fP (string or symbol), if present, may be used as a prefix
for the filename component of the pathname.
However, both arguments may be ignored by \f2unix-tempname\fP.
.LP
\f2unix-tempname\fP is based on one of the UNIX functions
.Ix tempnam
.Ix mktemp
.Ix tmpnam
\f2tempnam()\fP, \f2mktemp()\fP, and \f2tmpnam()\fP (in that order);
if none of these functions is available, an algorithm similar to
the one employed by UNIX \f2mktemp()\fP is used.
.
.Ch "Symbolic Links"
.
.PP
The following procedures are only defined on platforms that support
.Ix "symbolic links"
symbolic links.
In this case, the feature
.Ix unix:symlinks
\f2unix:symlinks\fP is provided when the UNIX extension is loaded.
.
.Pr unix-lstat filename
.LP
.Ix lstat
The UNIX \f2lstat()\fP system call.
\f2unix-lstat\fP returns a
.Ix stat-record
\f2stat-record\fP (see \f2unix-stat\fP above).
.
.Pr unix-readlink filename
.LP
.Ix readlink
The UNIX \f2readlink()\fP system call.
\f2unix-readlink\fP returns the contents of specified symbolic
link as a string.
.
.Pr unix-symlink filename\*1 filename\*2
.LP
.Ix symlink
The UNIX \f2symlink()\fP system call.
\f2unix-symlink\fP returns the non-printing object.
.
.Ch "File and Record Locking"
.
.PP
The procedures described in this section are only defined if some form
of file-based
.Ix locking
locking is available on the local system (either locking of entire files, or
.Ix "record locking"
record locking).
In this case, the feature
.Ix unix:file-locking
\f2unix:file-locking\fP is provided at the time the UNIX extension is loaded.
If the local system supports locking of individual file segments,
the feature
.Ix unix:record-locks
\f2unix:record-locks\fP is provided as well, and the locking
primitives are based on the
.Ix fcntl
\f2fcntl()\fP system call
(otherwise the
.Ix flock
\f2flock()\fP system call is used).
.
.Pr unix-set-lock fdescr lock wait?
.LP
The \f2lock\fP argument is a
.Ix lock-record
\f2lock-record\fP with these fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
_
exclusive?~boolean~T{
.nf
exclusive lock (write lock) if #t,
shared lock (read lock) otherwise
T}
whence~symbol~T{
.nf
\f2set\fP, \f2current\fP, or \f2end\fP:
interpretation of \f2start\fP (see \f2unix-lseek\fP)
T}
start~integer~relative offset in bytes
length~integer~T{
.nf
length in bytes
(0 means lock to EOF)
T}
.TE
.KE
.LP
.Ix "record locks"
If record locks are supported, the fields \f2whence\fP, \f2start\fP,
and \f2length\fP specify a segment in the file referred to
by \f2fdescr\fP that is to be locked or unlocked.
If only entire files can be locked, the contents of these fields are 
ignored by the lock procedures.
.LP
An arbitrary number of
.Ix "shared lock"
shared locks for a file or file segment
may be active at a given time, but more than one
.Ix "exclusive lock"
exclusive lock,
or both shared and exclusive locks, cannot be set at the same time.
\f2fdescr\fP must be opened for reading to be able to set a
shared lock; it must be opened with write access for an exclusive lock.
A shared lock may be upgraded to an exclusive lock, and vice versa.
Mandatory locking may or may not be supported by the local system.
.LP
If the \f2wait?\fP argument is #t and the specified lock cannot be
applied, \f2unix-set-lock\fP blocks until the lock becomes available.
.LP
\f2unix-set-lock\fP returns #t if the specified lock could be applied,
#f otherwise.
.
.Pr unix-remove-lock fdescr lock
.LP
This procedure removes the specified file lock or record lock from
the file pointed to by \f2fdescr\fP.
\f2lock\fP is a \f2lock-record\fP; see \f2unix-set-lock\fP above for
a description.
\f2unix-remove-lock\fP returns the non-printing object.
.
.Pr unix-query-lock fdescr lock
.LP
If record locks are not supported, this procedure always returns #f.
If record locks are supported, \f2unix-query-lock\fP returns
information about the first lock that would cause a call to
\f2unix-set-lock\fP with \f2lock\fP to fail or block, or #f if no
such lock exists (i.\|e.\& if claiming the specified lock would succeed).
Information about the lock is returned as a pair; the car is an
integer (the process-ID of the the process that owns the lock),
the cdr is a \f2lock-record\fP.
The process-ID may be meaningless in a network environment.
.
.Ch "Obtaining Password and Group File Entries"
.
.PP
The procedures defined in this section are used to obtain entries
from the system's
.Ix "passwd database"
.Ix "group database"
\f2passwd\fP and \f2group\fP databases.
.
.[[
.Pr unix-get-passwd
.Pr unix-get-passwd user
.]]
.LP
If invoked without an argument, this procedure returns the next
entry from the \f2passwd\fP database.
Successive calls to \f2unix-get-passwd\fP return entries in a random
order.
The \f2user\fP argument, if present, is either the login name of a user
(a string or symbol) or a numeric user-ID (an integer).
In this case, the \f2passwd\fP entry for this user is returned.
.LP
\f2unix-get-passwd\fP returns a
.Ix passwd-record
\f2passwd-record\fP with the following fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
_
name~string~login name
password~string~login password
uid~integer~numeric user-ID
gid~integer~numeric primary group-ID
gecos~string~contents of GECOS field
homedir~string~home directory of user
shell~string~login shell of user
.TE
.KE
.LP
\f2unix-get-passwd\fP is based on the UNIX
.Ix getpwent
.Ix getpwuid
.Ix getpwnam
\f2getpwent()\fP, \f2getpwuid()\fP, and \f2getpwnam()\fP functions.
.
.[[
.Pr unix-get-group
.Pr unix-get-group group
.]]
.LP
\f2unix-get-group\fP is identical to
.Ix unix-get-passwd
\f2unix-get-passwd\fP (see above), except that the system's \f2group\fP
database is used instead of the \f2passwd\fP database.
.LP
The result value is a
.Ix group-record
\f2group-record\fP with these fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
name~string~group's name
password~string~group's password
gid~integer~numeric group-ID
members~list of symbols~group members
.TE
.KE
.LP
\f2unix-get-group\fP is based on the UNIX
.Ix getgrent
.Ix getgrnam
.Ix getgrgid
\f2getgrent()\fP, \f2getgrgid()\fP, and \f2getgrnam()\fP functions.
.
.[[
.Pr unix-rewind-passwd
.Pr unix-rewind-group
.]]
.LP
These procedures rewind the \f2passwd\fP and \f2group\fP files
by calling the
.Ix setpwent
.Ix setgrent
\f2setpwent()\fP and \f2setgrent()\fP UNIX functions.
.
.[[
.Pr unix-end-passwd
.Pr unix-end-group
.]]
.LP
\f2unix-end-passwd\fP and \f2unix-end-group\fP close the \f2passwd\fP
and \f2group\fP files by calling the UNIX functions
.Ix endpwent
.Ix endgrent
\f2endpwent()\fP and \f2endgrent()\fP.
.
.Ch "Process Creation and Control"
.
.Pr unix-system string
.LP
\f2unix-system\fP starts
.Ix /bin/sh
``/bin/sh'' as a child process with \f2string\fP as input and waits until the
.Ix shell
shell terminates.
All file descriptors except standard input, standard output, and standard
error output are closed in the child process.
\f2unix-system\fP returns the
.Ix "exit code"
exit code of the shell as an integer or, if the shell was interrupted
by a signal, the
.Ix "termination status"
termination status as a list of one integer element.
If the shell could not be executed, exit code 127 is returned.
.
.[[
.Pr unix-open-input-pipe string
.Pr unix-open-output-pipe string
.]]
.LP
.Ix popen
The UNIX \f2popen()\fP function.
Both procedures create a
.Ix pipe
pipe between the caller and a
.Ix shell
shell executing the command \f2string\fP; they return a Scheme port
containing the file pointer associated with the pipe.
Closing the Scheme port, or running the garbage collector after the
port has become unused, causes the pipe to be closed by a call to the
.Ix pclose
\f2pclose()\fP function.
.LP
\f2unix-open-input-pipe\fP returns an input port that can be used
to read from the standard output of the specified command;
\f2unix-open-output-pipe\fP returns an output port that accepts
input to be sent to the standard input of the command.
.
.Pr unix-fork
.LP
.Ix fork
The UNIX \f2fork()\fP system call.
\f2unix-fork\fP returns the process-ID of the newly created process
as an integer in the parent process, and the integer 0 in the child process.
.LP
The child process, as its first action, invokes the
.Ix "onfork handlers"
\f2onfork handlers\fP that may have been registered by other Elk extensions
that are currently active (one purpose of \f2onfork\fP handlers is to
make new links to
.Ix "temporary files"
temporary files in the newly created child process).
.
.KS
.[[
.Pr unix-exec filename arguments
.Pr unix-exec filename arguments environment
.sp .5
.Pr unix-exec-path filename arguments
.Pr unix-exec-path filename arguments environment
.]]
.KE
.LP
These procedures are based on the UNIX
.Ix execv
\f2execv()\fP family of system calls and library functions.
The first argument is the name of the file to be executed.
\f2arguments\fP is a list of strings to be passed to the program
as arguments.
The \f2environment\fP argument, if present, is a list of
.Ix environment
environment variable definitions to be used as the new program's
environment.
Each element of the list is pair of strings; the car of an element is
the name of an environment variable, the cdr is the variable's value
(the
.Ix unix-environ
\f2unix-environ\fP primitive can be used to obtain the current
environment of the running program).
.LP
\f2unix-exec-path\fP searches the specified filename in a list
of directories obtained from the calling program's
.Ix PATH
PATH environment variable.
The variant of \f2unix-exec-path\fP that accepts an \f2environment\fP
argument is not available on the currently supported platforms (the
reason is that there is no \f2execvpe()\fP variant of the
\f2execvp()\fP function, although \f2execve()\fP/\f2execle()\fP
variants of \f2execv()\fP and \f2execl()\fP usually exist in UNIX).
.LP
\f2unix-exec\fP and \f2unix-exec-path\fP remove the temporary files used
by the dynamic loading module of the interpreter kernel and invoke
the finalization functions that may have been registered by extensions.
As a result, attempting to load an object file after a call to
\f2unix-exec\fP or \f2unix-exec-path\fP has returned (i.\|e.\& failed)
may not work correctly.
The finalization functions are only invoked once.
.
.KS
.[[
.Pr unix-wait
.Pr unix-wait options
.sp .5
.Pr unix-wait-process pid
.Pr unix-wait-process pid options
.]]
.KE
.LP
\f2unix-wait\fP and \f2unix-wait-process\fP are based on the UNIX
.Ix wait
\f2wait()\fP family of system calls and library functions.
Both procedures return a
.Ix wait-record
\f2wait-record\fP with the following fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
pid~integer~process-ID of the terminated child process
status~symbol~reason for process termination
code~integer~exit code or termination status (signal)
core-dump?~boolean~#t if a core-dump was produced
resources~resources-record~resources of terminated process
.TE
.KE
.LP
See \f2unix-process-resources\fP below for a description of the
\f2resources-record\fP type.
.LP
The \f2wait-record\fP result holds the process-ID and termination
status of one of the terminated (or stopped) children of the calling
process.
The value of the \f2status\fP is one of the symbols \f2stopped\fP
(if the child process has been stopped), \f2signaled\fP (child
process is terminated due to a signal), or \f2exited\fP (child
process has invoked \f2exit()\fP).
\f2code\fP holds the exit code (if \f2status\fP is \f2exited\fP),
or a signal number (if \f2status\fP is either \f2stopped\fP or
\f2signaled\fP).
The \f2resources\fP field holds the user and system time consumed
by the child process and its children in nanoseconds (additional
resources may be supplied in future versions).
The fields of the \f2resources\fP record are #f on platforms that
do not support the
.Ix wait3
.Ix wait4
\f2wait3()\fP or \f2wait4()\fP system call.
.LP
\f2unix-wait-process\fP allows to collect the termination status
of an individual process or a group of processes specified by the
integer \f2pid\fP argument.
This procedure is only defined on platforms where the
.Ix waitpid
.Ix wait4
\f2waitpid()\fP or \f2wait4()\fP system call is available.
In this case, the feature
.Ix unix:wait-process
\f2unix:wait-process\fP is provided when the UNIX extension is loaded.
.LP
If no child process is available (or, in case of
\f2unix-wait-process\fP, no process as specified by the \f2pid\fP
argument), the \f2pid\fP field in the result is set to -1, and the
\f2status\fP field is set to the symbol \f2none\fP.
.LP
The \f2options\fP argument, if present, is a list of one or more
of the symbols \f2nohang\fP and \f2untraced\fP.
Options are only supported if the feature
.Ix unix:wait-options
\f2unix:wait-options\fP is provided.
.
.Pr unix-process-resources
.LP
This procedure is based on the UNIX
.Ix times
\f2times()\fP library function.
\f2unix-process-resources\fP returns the
.Ix "resource usage"
resource usage of the calling process and its terminated children as
a pair of
.Ix resources-record
\f2resources-records\fP.
Each \f2resources-record\fP has the following fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
user-time~integer~user time in nanoseconds
system-time~integer~system time in nanoseconds
.TE
.KE
.LP
Addition fields may be supplied in future versions.
.
.Pr unix-environ
.LP
\f2unix-environ\fP returns the program's
.Ix environment
environment as a list of pairs.
The car of each element is the name of an environment variable
(a string), the cdr is the value of that variable (a string).
.
.Pr unix-getenv string
.LP
This procedure returns the value of the environment variable with
the name \f2string\fP as a string, or #f if the specified variable
is not defined.
.
.Pr unix-working-directory
.LP
\f2unix-working-directory\fP
returns the calling program's current
.Ix "working directory"
working directory as a string.
The procedure is based on the
.Ix getcwd
.Ix getwd
\f2getcwd()\fP or \f2getwd()\fP function if any of these is available
and invokes the
.Ix pwd
``pwd'' command otherwise.
.
.Pr unix-getlogin
.LP
\f2unix-getlogin\fP returns the
.Ix "login name"
login name as a string (obtained by the UNIX
.Ix getlogin
\f2getlogin()\fP library function).
.
.[[
.Pr unix-getuids
.Pr unix-getgids
.]]
.LP
\f2unix-getuids\fP (\f2unix-getgids\fP) returns the calling program's
.Ix "real user-ID"
.Ix "effective user-ID"
.Ix "real group-ID"
.Ix "effective group-ID"
real and effective user-IDs (group-IDs) as a pair of integers.
.
.Pr unix-getpids
.LP
\f2unix-getpids\fP returns the
.Ix process-ID
process-ID of the calling process and the
.Ix "parent process-ID"
parent process-ID as a pair of integers.
.
.Pr unix-getgroups
.LP
\f2unix-getgroups\fP returns the current
.Ix "supplementary group-IDs"
supplementary group-IDs of the process as a list of integers.
.LP
Example:
.Ss
;;; Get list of names of supplementary group-IDs
.sp .4
(define (get-group-names)
  (map
    (lambda (gid)
      (group-name (unix-get-group gid)))
    (unix-getgroups)))
.Se
.
.Pr unix-umask mask
.LP
.Ix umask
The UNIX \f2umask()\fP system call.
\f2mask\fP is an integer.
The procedure returns the previous value of the umask.
.
.Pr unix-nice incr
.LP
.Ix nice
The UNIX \f2nice()\fP function.
\f2incr\fP is an integer.
\f2unix-nice\fP returns the new nice value (or zero on some platforms).
.
.Pr unix-sleep seconds
.LP
.Ix sleep
The UNIX \f2sleep()\fP function.
\f2seconds\fP is a positive integer.
The procedure returns the non-printing object.
.
.Ch "Obtaining System Information"
.
.Pr unix-system-info
.LP
This procedure returns a
.Ix system-record
\f2system-record\fP with these fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Contents
hostname~string~the system's hostname
sysname~string~type of hardware platform
osname~string~operating system type and version
.TE
.KE
.LP
The hostname is determined by a call to the UNIX
.Ix gethostname
.Ix uname
\f2gethostname()\fP or \f2uname()\fP function; the system name and OS
name are obtained from the configuration file that has been used to
configure and install Elk.
.
.Pr unix-file-limit limit file
.LP
\f2unix-file-limit\fP can be used to query various system
.Ix limits
limits and options associated with files.
\f2limit\fP is a symbol identifying the type of limit;
\f2file\fP is a filename or file descriptor.
.LP
At least the following limits and options can be queried:
.sp .5
.KS
.TS
allbox, tab(~);
c c
lf5 l.
Limit/Option~Meaning
_
max-links~maximum number of links to a file or directory
max-name~maximum length of a filename
max-path~maximum length of a pathname
pipe-buf~pipe buffer size
no-trunc~T{
.nf
filename exceeding maximum length causes error
instead of being silently truncated
T}
.TE
.KE
.LP
Additional limits may be present on some platforms.
The list of limits actually supported by this procedure can
be obtained by a call to
.Ix unix-list-file-limits
\f2unix-list-file-limits\fP (see below).
.LP
If present, the
.Ix POSIX
.Ix pathconf
.Ix fpathconf
POSIX \f2pathconf()\fP/\f2fpathconf()\fP function is
used to query a limit; in this case the specified filename or file
descriptor is supplied as an argument to \f2pathconf()\fP or
\f2fpathconf()\fP.
If \f2pathconf()\fP is not available, or if calling it is not
appropriate for the type of limit, a static (compile-time) value
is returned.
.LP
The result type of \f2unix-file-limit\fP depends on the type of the
specified limit (boolean in case of \f2no-trunc\fP, integer otherwise).
.
.Pr unix-list-file-limits
.LP
This procedure returns the list of limit symbols that can be supplied
as arguments to
.Ix unix-file-limit
\f2unix-file-limit\fP (see above).
.
.Pr unix-job-control?
.LP
This predicate returns #t if UNIX job control is available on the local
system, #f otherwise.
In a
.Ix POSIX
POSIX environment, this procedure may call
.Ix sysconf
\f2sysconf()\fP.
.
.Ch "Date and Time"
.
.Pr unix-time
.LP
.Ix time
The UNIX \f2time()\fP function.
\f2unix-time\fP returns the number of seconds elapsed since
midnight\ UTC,\ January\ 1,\ 1970 (\f2The Epoch\fP) as an integer.
.
.Pr unix-nanotime
.LP
This procedure returns the number of nanoseconds elapsed since
The Epoch as an integer.
\f2unix-nanotime\fP invokes one of the UNIX functions
.Ix gettimeofday
.Ix ftime
.Ix time
\f2gettimeofday()\fP, \f2ftime()\fP, \f2time()\fP (in that order,
depending on which of these function is available), thus
providing up to microsecond resolution.
.
.[[
.Pr unix-decode-localtime time
.Pr unix-decode-utc time
.]]
.LP
Both procedures convert the specified time (a number of seconds as
returned by \f2unix-time\fP) into a
.Ix time-record
\f2time-record\fP; \f2unix-decode-localtime\fP corrects for the
local time zone and DST adjustment (based on the UNIX
.Ix localtime
.Ix gmtime
\f2localtime()\fP and \f2gmtime()\fP functions).
.LP
A \f2time-record\fP has the following fields:
.sp .5
.KS
.TS
allbox, tab(~);
c c c
lf5 l l.
Field~Type~Range
_
seconds~integer~0..61
minutes~integer~0..59
hours~integer~0..23
day-of-month~integer~1..31
month~integer~0..11
year~integer~(year - 1900)
weekday~integer~0..6
day-of-year~integer~0..365
dst~integer~1 if DST in effect
.TE
.KE
.LP
Example:
.Ss
;;; Return date as a string of the form "Nov 3, 1993"
.sp .4
(define (date-string)
  (let* ((months "JanFebMarAprMayJunJulAugSepOctNovDec")
         (time (unix-decode-localtime (unix-time)))
         (month-inx (* 3 (time-month time))))
.sp .4
    (format #f "~a ~a, ~a"
            (substring months month-inx (+ 3 month-inx))
            (time-day-of-month time) (+ 1900 (time-year time)))))
.Se
.
.Pr unix-time->string time
.LP
This procedure converts the specified time into a string;
it is based on the
.Ix ctime
.Ix asctime
\f2ctime()\fP and \f2asctime()\fP UNIX functions.
\f2time\fP is either an integer (number of seconds) or a
\f2time-record\fP.
.
.Ch "Signals"
.
.PP
The procedures described in this section (except \f2unix-kill\fP,
\f2unix-list-signals\fP, and \f2unix-pause\fP)
are only defined if the local system supports
.Ix "reliable signals"
reliable signals (either
.Ix BSD
BSD-style or
.Ix POSIX
POSIX signals).
In this case, the feature
.Ix unix:reliable-signals
\f2unix:reliable-signals\fP is provided when the UNIX extension
is loaded.
.
.Pr unix-kill pid signal
.LP
.Ix kill
The UNIX \f2kill()\fP system call.
\f2pid\fP is an integer; \f2sig\fP is either an integer (a signal
number) or a symbol (a
.Ix "signal name"
signal name).
At least the following signal names are supported:
.sp .5
.KS
.TS
box, tab(~);
c s s
lf5 lf5 lf5.
Signal names
_
sigalrm~sigbus~sigfpe
sighup~sigill~sigint
sigkill~sigpipe~sigquit
sigsegv~sigterm~
.TE
.KE
.LP
The list of signal names actually supported by the local system
can be obtained by calling
.Ix unix-list-signals
\f2unix-list-signals\fP (see below).
.LP
\f2unix-kill\fP returns the non-printing object.
.
.Pr unix-list-signals
.LP
This procedure returns a list of
.Ix "signal name"
signal names (symbols) that are supported by the system.
.
.Pr alarm seconds
.LP
.Ix alarm
The UNIX \f2alarm()\fP function.
\f2seconds\fP is a positive integer.
\f2unix-alarm\fP returns the number of seconds remaining from the
previously set alarm.
.
.Pr unix-pause
.LP
.Ix pause
The UNIX \f2pause()\fP function.
This procedure does not return.
.
.[[
.Pr unix-signal sig action
.Pr unix-signal sig
.]]
.LP
\f2unix-signal\fP defines or queries the action to be performed when a
.Ix signal
signal is delivered to the program.
If an \f2action\fP argument is specified, this action is associated
with the signal \f2sig\fP, and the previous action for this
signal is returned.
If no action is given (second form), \f2unix-signal\fP just returns
the action currently associated with \f2sig\fP.
.LP
\f2sig\fP is the name of a signal (see \f2unix-kill\fP for a
description).
The action associated with \f2sigbus\fP, \f2sigfpe\fP, \f2sigill\fP,
\f2sigint\fP, \f2sigkill\fP, \f2sigsegv\fP, and \f2sigabrt\fP (if
supported) cannot be altered; either because UNIX does not permit
this (\f2sigkill\fP), or because the signal can be generated as
the result of an internal fatal error (\f2sigbus\fP etc.), or
because it is used by the interpreter internally (\f2sigsegv\fP is
used by the incremental garbage collector).
The action associated with the
.Ix "interrupt signal"
\f2interrupt\fP signal can be controlled by redefining the standard Elk
.Ix interrupt-handler
\f2interrupt-handler\fP (see the Elk reference manual for details).
.LP
\f2action\fP can be one of the following:
.RS
.IP "the symbol \f2ignore\fP
the specified signal is ignored
.IP "the symbol \f2default\fP
the default action for this signal is established
.IP "the symbol \f2exit\fP
cleanup and exit: if the signal is delivered, the interpreter's
.Ix "temporary files"
temporary files are removed, the
.Ix "finalization functions"
finalization functions and static C++ destructors of dynamically loaded
extensions are invoked, and \f2exit()\fP is called with an exit code of 1
.IP "a compound procedure"
the procedure
.Ix "signal handler"
(signal handler) is invoked on delivery of the specified signal.
.IP
.RE
.LP
The procedure specified as a signal handler must accept one or more
arguments.
When the signal is delivered, the procedure is invoked with the signal
name (a symbol) as an argument.
Signal handlers must not return (i.\|e.\& they must either exit or
call a continuation).
If a signal handler returns, a message is displayed and the
.Ix reset
\f2reset\fP primitive is called.
.LP
The signal specified as an argument to \f2unix-signal\fP is added to
(removed from) the
.Ix "signal mask"
signal mask maintained by the interpreter, i.\|e.\& calls to the
.Ix disable-interrupts
\f2disable-interrupts\fP primitive block the signal from delivery.
.LP
\f2unix-signal\fP returns the previous (current) action for the
specified signal (a procedure or \f2ignore\fP, \f2default\fP, or
\f2exit\fP) or the symbol \f2handler\fP to indicate that the
signal is handled internally by the interpreter.
.
.Ch "Miscellaneous Functions"
.
.Pr unix-getpass string
.LP
\f2unix-getpass\fP displays \f2string\fP on standard output, reads a
.Ix password
password, and returns the password as a string.
The procedure invokes the UNIX
.Ix getpass
\f2getpass()\fP function.
.
.Ch "Error Functions"
.
.Sy unix-errval expression
.LP
Normally, a Scheme error is signaled by the UNIX extension whenever a
UNIX system call or library function invoked by any of the above
primitives fails.
The macro \f2unix-errval\fP allows an application to handle an error
condition in a specific way without the need to redefine the standard
.Ix "error handler"
error handler of Elk.
.LP
\f2unix-errval\fP evaluates the specified expression and returns
the result of the evaluation.
If, during evaluation of the expression, an error is signaled due
to failure of a UNIX function, the corresponding primitive 
procedure returns a unique
.Ix "error object"
\f2error object\fP instead of performing normal error handling.
.LP
For example, evaluating the expression
.Ss
(unix-close 1000)    ; close a bad file descriptor
.Se
would invoke the standard Scheme error handler in the normal way,
whereas evaluating
.Ss
(unix-errval (unix-close 1000))
.Se
would return an error object to allow the application to handle
the error locally.
Note that evaluation of the enclosing expression is not interrupted
when an error is signaled, i.\|e.\& the expression
.Ss
(unix-errval (begin (unix-close 1000) 5))
.Se
would return the integer 5.
.
.Pr unix-error? obj
.LP
This procedure returns #t if \f2obj\fP is the \f2error object\fP,
#f otherwise.
\f2unix-error?\fP is typically used to check whether a primitive
invoked under control of \f2unix-errval\fP has signaled an error.
.
.Pr unix-errno
.LP
.Ix errno
Returns the UNIX \f2errno\fP set by the last system call that has
failed.
.Ix "error codes"
Error codes are represented as symbols corresponding to the names of the
standard UNIX error numbers with letters converted to lower case, i.\|e.
\f2enomem\fP, \f2ebadf\fP, etc.
The exact set of error codes that can be returned is platform-dependent.
.LP
The value returned by \f2unix-errno\fP is not reset when a UNIX system
call executes successfully.
However, value of \f2unix-errno\fP is also affected by functions
from the Elk kernel (such as \f2open-input-file\fP) and possibly
other extensions that make use of system calls.
.
.Pr unix-perror string
.LP
\f2unix-perror\fP writes \f2string\fP followed by a colon and a short
message describing the last UNIX error encountered to the current
output port.
\f2unix-perror\fP makes use of the ``~E'' format specifier of the
.Ix format
format primitive.
.
.bp
.Ch "Examples"
.LP
.Ix examples
This program implements a simple program interface to the UNIX
.Ix dc
\f2dc\fP desktop calculator command.
The procedure
.Ix calc-open
\f2calc-open\fP starts the \f2dc\fP command and establishes two
.Ix pipe
pipes to/from the child process; the procedure
.Ix calc
\f2calc\fP sends its argument (a \f2dc\fP expression as a string)
as input to \f2dc\fP;
.Ix calc-close
\f2calc-close\fP closes the pipes and waits for the subprocess to
terminate.
.Ss
(require 'unix)
.sp .4
(define calc-from-dc)   ; input port: standard output of dc command
(define calc-to-dc)     ; output port: standard input of dc command
(define calc-dc-pid)    ; process-ID of child process running dc
.sp .4
(define calc-dc-command "/bin/dc")
.sp .4
(define (calc-open)
  (let* ((from (unix-pipe))
         (to (unix-pipe))
         (redirect-fd (lambda (a b)
                        (unix-dup a b) (unix-close a))))
    (set! calc-dc-pid (unix-fork))
    (if (zero? calc-dc-pid)
        (begin
          (unix-close (car from))
          (unix-close (cdr to))
          (redirect-fd (car to) 0)
          (redirect-fd (cdr from) 1)
          (unix-exec calc-dc-command '("dc")))
        (begin
          (unix-close (cdr from))
          (unix-close (car to))
          (set! calc-to-dc   (unix-filedescriptor->port (cdr to)   "w"))
          (set! calc-from-dc (unix-filedescriptor->port (car from) "r"))))))
.sp .4
(define (calc expr)
  (format calc-to-dc "~a~%" expr)
  (flush-output-port calc-to-dc)
  (read-string calc-from-dc))
.sp .4
(define (calc-close)
  (close-output-port calc-to-dc)
  (close-input-port calc-from-dc)
  (unix-wait-process calc-dc-pid))

;;; Test -- print sqrt(2):
.sp .4
(calc-open)
(display (calc "10k 2v p")) (newline)
(calc-close)
.Se
.bp
.LP
The following procedure copies a file; the arguments are the source and
target file names.
The second argument may name a directory, in this case the file is
copied into the directory.
The target file must not yet exist.
.Ix copy-file
\f2copy-file\fP preserves the access mode of the source file.
.Ss
(require 'unix)
.sp .4
(define copy-buffer-size 8192)
.sp .4
(define (copy-file from to)
  (let ((from-stat (unix-stat from))
        (to-stat (unix-errval (unix-stat to))))
.sp .3
    (if (eq? (stat-type from-stat) 'directory)       ; complain if "from"
        (error 'copy-file "~s is a directory" from)) ;   is a directory
.sp .3
    (if (and (not (unix-error? to-stat))             ; destination exists
             (eq? (stat-type to-stat) 'directory))   ;   and is a directory?
        (set! to (format #f "~a/~a" to from)))
.sp .3
    (let* ((to-fd (unix-open to '(write create exclusive)
                             (stat-mode from-stat)))
           (from-fd (unix-open from '(read)))
           (buf (make-string copy-buffer-size)))

      (let loop ((num-chars (unix-read-string-fill! from-fd buf)))
           (if (positive? num-chars)
               (begin
                 (unix-write to-fd buf num-chars)
                 (loop (unix-read-string-fill! from-fd buf)))))
.sp .3
      (unix-close from-fd)
      (unix-close to-fd))))
.Se
.bp
.LP
\f2lock-vi\fP starts the
.Ix vi
.Ix editor
\f2vi\fP editor with the specified file name.
It provides exclusive access to the file during the editing session by
applying a write lock to the file and removing it when the editor finishes.
A message is displayed periodically if the lock is held by somebody else.
.Ss
(require 'unix)
.sp .4
(define (lock-vi file)
  (let* ((fd (unix-open file '(read write)))
         (lock ((record-constructor lock-record) #t 'set 0 0)))
.sp .4
    (let loop ()
         (if (not (unix-set-lock fd lock #f))
             (begin
               (format #t "Someone else is editing ~s...~%" file)
               (unix-sleep 10)
               (loop))))
.sp .4
    (unix-system (format #f "vi ~a" file))
    (unix-remove-lock fd lock)))
.Se
.sp
.LP
\f2pipe-size\fP attempts to determine the capacity of a
.Ix pipe
pipe.
It creates a pipe, places the write end of the pipe into
.Ix "non-blocking I/O"
non-blocking I/O mode and writes into the pipe until it is full,
counting the characters successfully written.
.Ss
(require 'unix)
.sp .4
(define (pipe-size)
  (let* ((pipe (unix-pipe))
         (flags (unix-filedescriptor-flags (cdr pipe)))
         (len 32)                    ; assumes capacity is multiple of len
         (noise (make-string len)))
.sp .4
    ;; enable non-blocking I/O for write side of pipe:
    (unix-filedescriptor-flags (cdr pipe) (cons 'ndelay flags))
.sp .4
    (unwind-protect
      (let loop ((size 0))
           (if (unix-error? (unix-errval (unix-write (cdr pipe) noise)))
               (if (memq (unix-errno) '(eagain ewouldblock))
                   size
                   (error 'pipe-size "~E"))
               (loop (+ size 32))))
      (unix-close (car pipe))
      (unix-close (cdr pipe)))))
.Se
