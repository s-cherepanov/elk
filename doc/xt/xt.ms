.so ../util/tmac.scheme
.Ul
.TL
Elk/Xt Reference Manual
.AU
Oliver Laumann
.
.Ch "Introduction"
.PP
This manual describes the functions, special forms, and
variables defined by the Xt (X Toolkit Intrinsics) extension included
in the Elk distribution.
Most of the functions are directly equivalent to a function of the
X toolkit C library, so that the description need not be repeated.
In such cases, only the name of the corresponding Xt function is
mentioned.
Thus, you should have the \f2X Toolkit Intrinsics \- C Language Interface\fP
manual within reach when using this reference manual.
.PP
The functions listed in this document are loaded when the expression
.DS
.ft 5
(require 'xwidgets)
.ft
.DE
.Ix xwidgets
is evaluated or, when the OSF/Motif software has been installed on
your system and you want to use Motif widgets from within Scheme,
when
.DS
.ft 5
(require 'motif)
.ft
.DE
.Ix motif
is evaluated in the interpreter's top level or in a Scheme program.
If you only want to use the toolkit functionality (and no widgets),
evaluate
.DS
.ft 5
(require 'xt).
.ft
.DE
.Ix xt
Note that all of the above forms cause the Elk/Xlib functions to be
loaded as well.
.LP
Individual widgets are loaded by evaluating
.DS
.ft 5
(load-widgets . \f2widget-names\fP)
.ft
.DE
.LP
.Id load-widgets
Each \f2widget-name\fP is a symbol (not quoted, since \f2load-widgets\fP
is a macro).
.PP
The widgets are loaded from subdirectories of ``$install_dir/runtime/obj''
(where $install_dir is the directory where you have installed Elk on
your system).
.PP
In the following, the types of arguments of the listed procedures are
not specified when they are obvious from the context or from the name.
For instance, an argument named \f2widget\fP is always of type \f2widget\fP,
an argument named \f2context\fP is an object of type \f2context\fP
(application context), etc.
Arguments the names of which end in ``?'' are always of type \f2boolean\fP.
.if !\n(.U \{\
.PP
In the following, each description of a procedure, special form, or
variable lists the kind of object in boldface.
Here, \f3procedure\fP denotes either a primitive procedure or a
compound procedure, \f3syntax\fP denotes a special form or a macro,
and \f3variable\fP denotes a global variable that has some initial
value and can be re-assigned a new value by the user (by means
of \f2set!\fP or \f2fluid-let\fP).
.\}
.
.Ch "Widget Classes"
.
.Pr class? x
.LP
Returns #t iff \f2x\fP is an object of type \f2class\fP (widget class).
.
.Pr find-class name-of-class
.LP
Returns the widget class of the specified name (an object of
type \f2class\fP).
\f2name-of-class\fP is a string or a symbol.
.
.Pr class-resources widget-class
.LP
See \f2XtGetResourceList\fP.
Returns the resource list of the specified widget class.
Each element of the list is a list of three symbols \-
the resource name, the resource class, and the resource type.
.
.Pr class-constraint-resources widget-class
.LP
See \f2XtGetConstraintRespourceList\fP.
Returns the list of constraint resources that are defined for the
specified widget class.
Each element of the list is a list of three symbols \-
the resource name, the resource class, and the resource type.
.
.Pr class-sub-resources widget-class
.LP
Returns the list of sub-resources (if there are any) of the
specified widget class.
See \f2class-resources\fP above.
.
.Pr class-exists? name-of-class
.LP
Returns #t iff a widget class of the given name exists
(i.\|e.\& has been loaded by means of \f2load-widgets\fP).
\f2name-of-class\fP is a string or a symbol.
.
.Ch "Widget Functions"
.
.Pr widget? x
.LP
Returns #t iff \f2x\fP is an object of type \f2widget\fP.
.
.Pr destroy-widget widget
.LP
See \f2XtDestroyWidget\fP.
.
.Pr create-shell application-name application-class parent display . args
.LP
See \f2XtAppCreateShell\fP.
\f2application-name\fP and \f2application-class\fP are strings or symbols
or #f (NULL is used in the latter case).
\f2parent\fP is a widget.
The number of \f2args\fP must be even, the 1st, 3rd, etc.\& argument
is the name of a resource to be set (a string or a symbol), the
2nd, 4th, etc.\& argument is the corresponding value.
.
.[[
.Pr create-widget widget-class parent . args
.Pr create-widget widget-name widget-class parent . args
.]]
.LP
See \f2XtCreateWidget\fP.
\f2widget-name\fP is a string or a symbol; \f2parent\fP is a widget.
If no \f2widget-name\fP is given, the name of the widget class is used.
The number of \f2args\fP must be even, the 1st, 3rd, etc.\& argument
is the name of a resource to be set (a string or a symbol), the
2nd, 4th, etc.\& argument is the corresponding value.
.
.Pr create-managed-widget . args
.LP
Applies \f2create-widget\fP to the arguments and then calls \f2manage-child\fP
with the newly created widget.
.
.Pr realize-widget widget
.LP
See \f2XtRealizeWidget\fP.
.
.Pr unrealize-widget widget
.LP
See \f2XtUnrealizeWidget\fP.
.
.Pr widget-realized? widget
.LP
See \f2XtIsRealized\fP.
.
.Pr widget-display widget
.LP
See \f2XtDisplay\fP.
Returns an object of type \f2display\fP.
.
.Pr widget-parent widget
.LP
See \f2XtParent\fP.
.
.Pr widget-name widget
.LP
See \f2XtName\fP.
Returns the name of a widget as a string.
.
.[[
.Pr widget\(mi>window widget
.Pr widget-window widget
.]]
.LP
See \f2XtWindow\fP.
Returns an object of type \f2window\fP.
.
.Pr widget-composite? widget
.LP
See \f2XtIsComposite\fP.
.
.Pr manage-children . widgets
.LP
See \f2XtManageChildren\fP.
.
.Pr manage-child widget
.LP
Calls \f2manage-children\fP with the specified widget.
.
.Pr unmanage-children . widgets
.LP
See \f2XtUnmanageChildren\fP.
.
.Pr unmanage-child widget
.LP
Calls \f2unmanage-children\fP with the specified widget.
.
.Pr widget-managed? widget
.LP
See \f2XtIsManaged\fP.
.
.Pr widget-class widget
.LP
See \f2XtClass\fP.
Returns an object of type \f2class\fP.
.
.Pr widget-superclass widget
.LP
See \f2XtSuperclass\fP.
Returns an object of type \f2class\fP or the symbol \f5none\fP
when the widget's class does not have a super-class.
.
.Pr widget-subclass? widget class
.LP
See \f2XtIsSubclass\fP.
.
.Pr set-mapped-when-managed! widget managed?
.LP
See \f2XtSetMappedWhenManaged\fP.
.
.Pr map-widget widget
.LP
See \f2XtMapWidget\fP.
.
.Pr unmap-widget widget
.LP
See \f2XtUnmapWidget\fP.
.
.Pr set-values! widget . args
.LP
See \f2XtSetValues\fP.
The number of \f2args\fP must be even, the 1st, 3rd, etc.\& argument
is the name of a resource to be set (a string or a symbol), the
2nd, 4th, etc.\& argument is the corresponding value.
.
.Pr get-values widget . args
.LP
See \f2XtGetValues\fP.
Each \f2arg\fP is the name of a resource (a string or a symbol).
Returns a list of the values of the specified resources.
.
.Pr widget-context widget
.LP
See \f2XtWidgetToApplicationContext\fP.
Returns an object of type \f2context\fP.
.
.Pr set-sensitive! widget sensitive?
.LP
See \f2XtSetSensitive\fP.
.
.Pr widget-sensitive? widget
.LP
See \f2XtIsSensitive\fP.
.
.Pr window\(mi>widget window
.LP
See \f2XtWindowToWidget\fP.
.
.Pr name\(mi>widget root-widget name
.LP
See \f2XtNameToWidget\fP.
\f2name\fP is a string or a symbol.
.
.Pr widget-translate-coordinates widget x y
.LP
See \f2XtTranslateCoords\fP.
Returns the root-relative x and y coordinates as a pair of integers.
.
.Ch "Callback Functions"
.
.Pr add-callbacks widget callback-name . callback-functions
.LP
See \f2XtAddCallbacks\fP.
Adds the functions to a callback list of the specified widget.
\f2callback-name\fP is a string or a symbol.
Each callback function will be called with at least one argument \-
the widget to which the function has been attached.
.
.Pr add-callback widget callback-name callback-function
.LP
Calls \f2add-callbacks\fP with the specified function.
.
.\" .Pr call-callbacks widget callback-name object
.\" Calls each callback procedure that is registered in the callback
.\" list named \f2callback-name\fP of the given \f2widget\fP with
.\" \f2object\fP as an argument.
.\" .
.Ch "Popup Shells"
.
.[[
.Pr create-popup-shell widget-class parent-widget . args
.Pr create-popup-shell widget-name widget-class parent-widget . args
.]]
.LP
See \f2XtCreatePopupShell\fP.
\f2widget-name\fP is a string or a symbol.
If no widget name is given, the name of the shell class is used.
The number of \f2args\fP must be even, the 1st, 3rd, etc.\& argument
is the name of a resource to be set (a string or a symbol), the
2nd, 4th, etc.\& argument is the corresponding value.
.
.Pr popup shell-widget grab-kind
.LP
See \f2XtPopup\fP.
\f2grab-kind\fP is a symbol (\f5grab-once\fP, \f5grab-nonexclusive\fP,
or \f5grab-exclusive\fP).
.
.Pr popdown shell-widget
.LP
See \f2XtPopdown\fP.
.
.Ch "Application Contexts"
.
.Pr context? x
.LP
Returns #t iff \f2x\fP is an object of type \f2context\fP
(application context).
.
.Pr create-context
.LP
See \f2XtCreateApplicationContext\fP.
.
.Pr destroy-context context
.LP
See \f2XtDestroyApplicationContext\fP.
.
.Pr initialize-display context display application-name application-class
.LP
See \f2XtDisplayInitialize\fP, \f2XtOpenDisplay\fP.
If \f2display\fP is an object of type \f2display\fP,
\f2XtDisplayInitialize\fP is called.
If \f2display\fP is a display name (a string or a symbol) or #f,
\f2XtOpenDisplay\fP is called (with a NULL display in the latter case),
and the newly opened display is returned.
\f2application-name\fP and \f2application-class\fP are strings or symbols
or #f (NULL and the empty string are used in the latter case).
.
.Pr application-initialize name . fallback-resources
.LP
A convenience function that creates an application context by a call
to \f2create-context\fP, sets the fallback resources (if any), initializes
the display by a call to \f2initialize-display\fP with the specified
\f2name\fP and a class of #f, and creates and returns an application
shell with the name \f2name\fP and class #f.
.LP
Calling \f2application-initialize\fP more than once may cause the
application contexts and displays that were created by earlier
calls to be closed during a future garbage collection.
.
.Pr display\(mi>context display
.LP
See \f2XtDisplayToApplicationContext\fP.
.
.Pr set-context-fallback-resources! context . resources
.LP
See \f2XtAppSetFallbackResources\fP.
Each \f2resource\fP is a string.
.
.Pr context-main-loop context
.LP
See \f2XtAppMainLoop\fP.
.
.Pr context-pending context
.LP
See \f2XtAppPending\fP.
Returns a list of symbols (\f5x-event\fP, \f5timer\fP, \f5alternate-input\fP).
.
.Pr context-process-event context . mask
.LP
See \f2XtProcessEvent\fP.
The optional argument \f2mask\fP is a list of symbols (see
\f2context-pending\fP above).
\f2XtIMAll\fP is used if the \f2mask\fP argument is omitted
.
.Pr context-add-work-proc context procedure
.LP
See \f2XtAppAddWorkProc\fP.
Returns an \f2identifier\fP that can be used as an argument to
\f2remove-work-proc\fP.
\f2procedure\fP is a procedure with no arguments.
.
.Pr remove-work-proc identifier
.LP
See \f2XtRemoveWorkProc\fP.
\f2identifier\fP must be the return value of a previous call to
\f2context-add-work-proc\fP.
Each such \f2identifier\fP can be used as an argument for
\f2remove-work-proc\fP only once.
.
.Pr context-add-timeout context timer-value procedure
.LP
See \f2XtAppAddTimeOut\fP.
\f2timer-value\fP is an integer.
Returns an \f2identifier\fP that can be used as an argument to
\f2remove-timeout\fP.
The time-out procedure will be called with one argument, the identifier
returned by the call to \f2context-add-timeout\fP (i.\|e.\& the object
that uniquely identifies the timer).
.
.Pr remove-timeout identifier
.LP
See \f2XtRemoveTimeOut\fP.
\f2identifier\fP must be the return value of a previous call to
\f2context-add-timeout\fP.
Each such \f2identifier\fP can be used as an argument for
\f2remove-timeout\fP only once.
.
.Pr context-add-input context source procedure . mask
.LP
See \f2XtAppAddInput\fP.
\f2source\fP is a file port.
Returns an \f2identifier\fP that can be used as an argument to
\f2context-remove-input\fP.
The \f2procedure\fP will be called with two arguments \- \f2source\fP
and the identifier returned by the call to \f2context-add-input\fP.
.LP
The optional \f2mask\fP argument is a list of one or more of the
symbols \f2read\fP, \f2write\fP, and \f2exception\fP.
It specifies the condition on which the procedure will be called.
If no \f2mask\fP argument is given, \f2read\fP is used if \f2source\fP
is an input-port, \f2write\fP if it is an output-port, and
both \f2read\fP and \f2write\fP if it is an input-output-port.
.
.Pr remove-input identifier
.LP
See \f2XtRemoveInput\fP.
\f2identifier\fP must be the return value of a previous call to
\f2context-add-input\fP.
Each such \f2identifier\fP can be used as an argument for
\f2remove-input\fP only once.
.
.Pr identifier? x
.LP
Returns #t iff \f2x\fP is an \f2identifier\fP (an object returned by
functions like \f2context-add-timeout\fP).
.
.Ch "Translations Management Functions"
.
.Pr context-add-action context name procedure
.LP
See \f2XtAppAddActions\fP.
\f2name\fP is the name of the action (a string or a symbol).
The action procedure will be invoked with three arguments:
a widget, a list of event-specific arguments (see \f2handle-events\fP)
and a list of strings (the action arguments).
.
.Pr augment-translations widget translation-table
.LP
See \f2XtAugmentTranslations\fP.
\f2translation-table\fP is a string; \f2XtParseTranslationTable\fP is
applied to it.
.
.Pr override-translations widget translation-table
.LP
See \f2XtOverrideTranslations\fP.
\f2translation-table\fP is a string; \f2XtParseTranslationTable\fP is
applied to it.
.
.Pr uninstall-translations widget
.LP
See \f2XtUninstallTranslations\fP.
.
.Pr install-accelerators dst-widget src-widget
.LP
See \f2XtInstallAccelerators\fP.
.
.Pr install-all-accelerators dst-widget src-widget
.LP
See \f2XtInstallAllAccelerators\fP.
.
.Pr multi-click-time display
.LP
See \f2XtGetMultiClickTime\fP.
Returns an integer.
.
.Pr set-multi-click-time! display time
.LP
See \f2XtSetMultiClickTime\fP.
\f2time\fP is an integer.
.
.Ch "Error Handling"
.
.Va xt-warning-handler
.LP
See \f2XtSetWarningHandler\fP.
When a warning message is to be printed by the Xt intrinsics and the
global variable \f2xt-warning-handler\fP is bound to a compound
procedure, this procedure is invoked with the error message (a string)
as an argument.
When this variable is not bound to a compound procedure, the message
is sent to the current output port.
The initial value of this variable is the empty list.
.LP
This interface is bogus and will be replaced by a more useful mechanism
in future versions of the software.
.
.Ch "Miscellaneous Functions"
.
.Pr xt-release-4-or-later?
.LP
Returns always #t.
.
.Pr xt-release-5-or-later?
.LP
Returns #t iff the Xt extension is linked together with the X11
Release 5 toolkit intrinsics or later versions of the intrinsics.
.
.Pr xt-release-6-or-later?
.LP
Returns #t iff the Xt extension is linked together with the X11
Release 6 toolkit intrinsics or later versions of the intrinsics.
.
.Ch "Interaction with the Garbage Collector"
.
.PP
.Ix "garbage collector"
The Scheme garbage collector destroys objects of type \f2context\fP
or \f2widget\fP that are not longer accessible from within the Scheme
program.
This is done by invoking the function \f2destroy-context\fP or
\f2destroy-widget\fP, respectively, with the unreferenced object as
an argument.
.PP
The garbage collector only destroys objects that have been created
from with the Scheme program (by functions like \f2create-context\fP
or \f2create-widget\fP).
Objects that have been obtained from Xt through functions like
\f2widget-context\fP (and are owned by the Xt internals),
are ignored by the garbage collector.
.PP
Programmers must make sure that an object is accessible during the object's
entire lifetime, otherwise future runs of the garbage collector can
result in undesired termination of the object.
One must be especially careful when results of functions that create
new objects (such as \f2create-context\fP) are ignored or assigned
to local variables as in
.Ss
(define (initialize)
  (let* ((con (create-context))
          (dpy (initialize-display con #f 'Test #f)))
    (create-shell 'Test #f (find-class 'application-shell) dpy))))
.Se
.PP
In this example, after termination of the function, the garbage
collector will destroy the application context created by the
call to \f2create-context\fP as well as the display,
as they are no longer directly accessible from within the program.
Bugs like this are often hard to find, since (in the above example)
the shell widget returned by \f2initialize\fP can still be used,
although its application context and the display associated with
the application context have already been destroyed.
.PP
The problem can be solved by re-writing the above function like this:
.Ss
(define initialize #f)
.sp .5
(let ((con) (dpy))
  (set! initialize
    (lambda ()
      (set! con (create-context))
      (set! dpy (initialize-display con #f 'Test #f))
      (create-shell 'Test #f (find-class 'application-shell) dpy))))
.Se
.PP
An alternative solution is to place the application context and
display into a global variable, so that they can be terminated
explicitly by the program when desired.
