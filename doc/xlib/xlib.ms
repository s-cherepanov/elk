.so ../util/tmac.scheme
.Ul
.TL
Elk/Xlib Reference Manual
.AU
Oliver Laumann
.
.Ch "Introduction"
.PP
This manual lists the functions, special forms, and variables
defined by the Xlib extension included in the Elk distribution.
Most of the functions are directly equivalent to a function of the
Xlib C library, so that the description need not be repeated.
In such cases, only the name of the corresponding Xlib function is
mentioned.
Thus, you should have the \f2Xlib \- C Language X Interface\fP
manual within reach when using this reference manual.
.PP
The functions listed in this document can be loaded by evaluating
the expression
.DS
.ft 5
(require 'xlib).
.ft
.DE
.Ix xlib
in the interpreter's top level or in a Scheme program.
.PP
The types of arguments of the procedures listed below are not described
when they are obvious from the context or from the name.
For instance, an argument named \f2window\fP is always of type \f2window\fP,
an argument named \f2atom\fP is an object of type \f2atom\fP, etc.
Arguments the names of which end in ``?'' are always of type \f2boolean\fP.
.PP
If a function returns several items of the same type (for instance,
a list of windows), the return value is a vector of objects of this type.
If a function returns a collection of items of different types or
of different semantics, the return value is a list of objects
(or a pair).
In this case, \f2multiple-value-bind\fP
.Ix multiple-value-bind
can be used to bind variables to the return values.
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
.Ch "Display Functions"
.
.Pr display? x
.LP
Returns #t iff \f2x\fP is an object of type \f2display\fP.
.
.Pr open-display . name-of-display
.LP
See \f2XOpenDisplay\fP.
\f2name-of-display\fP is a string or a symbol.
If no name is specified, a NULL name will be passed to \f2XOpenDisplay\fP.
.
.Pr close-display display
.LP
See \f2XCloseDisplay\fP.
Finalizes all objects associated with the display, then closes
the display.
.
.[[
.Pr display-default-root-window display
.Pr display-root-window display
.]]
.LP
See \f2XDefaultRootWindow\fP.
.
.[[
.Pr display-default-colormap display
.Pr display-colormap display
.]]
.LP
See \f2XDefaultColormap\fP.
Returns the default colormap of the display's default screen.
.
.Pr display-default-gcontext display
.LP
See \f2XDefaultGC\fP.
Returns the default graphics context of the display's default screen.
.
.Pr display-default-depth display
.LP
See \f2XDefaultDepth\fP.
Returns the default depth of the display's default screen.
.
.Pr display-default-screen-number display
.LP
See \f2XDefaultScreen\fP.
Returns an integer.
.
.Pr display-cells display screen-number
.LP
See \f2XDisplayCells\fP.
Returns an integer.
.
.Pr display-planes display screen-number
.LP
See \f2XDisplayPlanes\fP.
Returns an integer.
.
.Pr display-string display
.LP
See \f2XDisplayString\fP.
Returns a string.
.
.Pr display-vendor display
.LP
See \f2XServerVendor\fP, \f2XVendorRelease\fP.
Returns a pair; the car is a string (the vendor identification),
and the cdr is an integer (the vendor release number).
.
.Pr display-protocol-version display
.LP
See \f2XProtocolVersion\fP, \f2XProtocolRevision\fP.
Returns a pair of integers (the X protocol's major and minor version numbers).
.
.Pr display-screen-count display
.LP
See \f2XScreenCount\fP.
Returns an integer.
.
.Pr display-image-byte-order display
.LP
See \f2XImageByteOrder\fP.
Returns a symbol (\f5lsb-first\fP or \f5msb-first\fP).
.
.Pr display-bitmap-unit display
.LP
See \f2XBitmapUnit\fP.
Returns an integer.
.
.Pr display-bitmap-bit-order display
.LP
See \f2XBitmapBitOrder\fP.
Returns a symbol (\f5lsb-first\fP or \f5msb-first\fP).
.
.Pr display-bitmap-pad display
.LP
See \f2XBitmapPad\fP.
Returns an integer.
.
.[[
.Pr display-width display
.Pr display-height display
.]]
.LP
See \f2XDisplayWidth\fP, \f2XDisplayHeight\fP.
Returns the width/height of the display's default screen.
.
.[[
.Pr display-width-mm display
.Pr display-height-mm display
.]]
.LP
See \f2XDisplayWidthMM\fP, \f2XDisplayHeightMM\fP.
Returns the width/height of the display's default screen in millimeters.
.
.Pr display-motion-buffer-size display
.LP
See \f2XDisplayMotionBufferSize\fP.
Returns an integer.
.
.Pr display-flush-output display
.LP
See \f2XFlush\fP.
.
.Pr display-wait-output display discard-events?
.LP
See \f2XSync\fP.
.
.Pr no-op display
.LP
See \f2XNoOp\fP.
.
.Pr list-depths display screen-number
.LP
See \f2XListDepths\fP.
Returns a vector of integers.
.
.Pr list-pixmap-formats display
.LP
See \f2XListPixmapFormats\fP.
Returns a vector of lists of three integers (depth, bits per pixel,
and scanline pad).
.
.Pr set-after-function! display procedure
.LP
See \f2XSetAfterFunction\fP.
Returns the old after function.
If \f2procedure\fP is #f, the current after function is disassociated
from the display.
.
.Pr after-function display
.LP
Returns the after function currently associated with the given
display (#f if there is none).
.
.Pr synchronize display
.LP
Sets the display's after function to \f2display-wait-output\fP.
.
.Ch "Window Functions"
.
.Pr window? x
.LP
Returns #t iff \f2x\fP is an object of type \f2window\fP.
.
.Pr drawable? x
.LP
Returns #t iff \f2x\fP is a ``drawable'' (window or pixmap).
.
.Pr window-display window
.LP
Returns the display associated with the window.
.
.Pr create-window . args
.LP
See \f2XCreateWindow\fP.
This function is used to create a new window.
.LP
The number of arguments must be even.
The 1st, 3rd, etc. argument is the name (a symbol) of an attribute
to be set when the window is created, the 2nd, 4th, etc.\& argument
is the corresponding value.
The attributes can be specified in any order.
.LP
Attributes are \f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP,
\f2border\fP (each of which has an integer value), \f2parent\fP
(the parent window), and all attributes that can be set by means
of the \f5set-window-\fP\f2attribute\fP\f5!\fP functions below
except \f2sibling\fP and \f2stack-mode\fP.
The attributes \f2parent\fP, \f2width\fP, and \f2height\fP are
mandatory.
The default for \f2x\fP and \f2y\fP is 0, the default for
\f2border\fP is 2.
.
.[[
.Pr set-window-x! window value
.Pr set-window-y! window value
.Pr set-window-width! window value
.Pr set-window-height! window value
.Pr set-window-border-width! window value
.Pr set-window-sibling! window value
.Pr set-window-stack-mode! window value
.Pr set-window-background-pixmap! window value
.Pr set-window-background-pixel! window value
.Pr set-window-border-pixmap! window value
.Pr set-window-border-pixel! window value
.Pr set-window-bit-gravity! window value
.Pr set-window-gravity! window value
.Pr set-window-backing-store! window value
.Pr set-window-backing-planes! window value
.Pr set-window-backing-pixel! window value
.Pr set-window-save-under! window value
.Pr set-window-event-mask! window value
.Pr set-window-do-not-propagate-mask! window value
.Pr set-window-override-redirect! window value
.Pr set-window-colormap! window value
.Pr set-window-cursor! window value
.]]
.LP
See \f2XConfigureWindow\fP, \f2XChangeWindowAttributes\fP.
Set the sibling window, stacking mode, background pixmap, background
pixel, border pixel, cursor, and other attributes (see
the \f5window-\fP functions below) of the specified window.
.LP
The stacking mode is a symbol (\f5above\fP, \f5below\fP, \f5top-if\fP,
\f5bottom-if\fP, \f5opposite\fP).
The \f2value\fP argument to \f2set-window-sibling!\fP must be a window,
\f2set-window-background-pixmap!\fP expects a pixmap,
\f2set-window-background-pixel!\fP and \f2set-window-border-pixel!\fP
expect a pixel, and \f2set-window-cursor!\fP expects a cursor argument.
For the types of the \f2value\fP argument of the other functions
see the return values of the \f2window-\fP functions below.
.
.[[
.Pr window-x window
.Pr window-y window
.Pr window-width window
.Pr window-height window
.Pr window-border-width window
.Pr window-depth window
.Pr window-visual window
.Pr window-root window
.Pr window-class window
.Pr window-bit-gravity window
.Pr window-gravity window
.Pr window-backing-store window
.Pr window-backing-planes window
.Pr window-backing-pixel window
.Pr window-save-under window
.Pr window-colormap window
.Pr window-map-installed window
.Pr window-map-state window
.Pr window-all-event-masks window
.Pr window-your-event-mask window
.Pr window-do-not-propagate-mask window
.Pr window-override-redirect window
.Pr window-screen window
.]]
.LP
See \f2XGetWindowAttributes\fP.
Returns the x and y coordinates, width, height, border width,
depth, visual, root window, class, bit gravity, window gravity,
backing store availability, backing planes, backing pixel,
save under availability, colormap, colormap installation information,
map state, global event mask, local event mask, ``do-not-propagate'' mask,
override redirect attribute, and screen of the specified window.
.LP
\f2window-visual\fP and \f2window-screen\fP always return the empty
list in the current release of the software.
\f2window-root\fP returns a window.
\f2window-class\fP returns a symbol (\f5input-output\fP, \f5input-only\fP).
\f2window-bit-gravity\fP returns a symbol (\f5forget\fP, \f5north-west\fP, 
\f5north\fP, \f5north-east\fP, \f5west\fP, \f5center\fP, \f5east\fP, 
\f5south-west\fP, \f5south\fP, \f5south-east\fP, \f5static\fP).
\f2window-gravity\fP returns a symbol (same as \f2window-bit-gravity\fP
with \f5unmap\fP instead of \f5forget\fP).
\f2window-backing-store\fP returns a symbol (\f5not-useful\fP,
\f5when-mapped\fP, \f5always\fP).
\f2window-backing-planes\fP and \f2window-backing-pixel\fP return
a pixel.
\f2window-save-under\fP, \f2window-map-installed\fP and
\f2window-override-redirect\fP return #t or #f.
\f2window-colormap\fP returns a colormap.
\f2window-map-state\fP returns a symbol (\f5unmapped\fP,
\f5unviewable\fP, \f5viewable\fP).
\f2window-all-event-masks\fP, \f2window-your-event-mask\fP, and
\f2window-do-not-propagate-mask\fP return a list of symbols
(event mask names such as \f5enter-window\fP, \f5pointer-motion\fP, etc.).
All other functions return an integer.
.
.[[
.Pr drawable-root drawable
.Pr drawable-x drawable
.Pr drawable-y drawable
.Pr drawable-width drawable
.Pr drawable-height drawable
.Pr drawable-border-width drawable
.Pr drawable-depth drawable
.]]
.LP
See \f2XGetGeometry\fP.
Returns the root window, x and y coordinates, width, height,
border width, and depth of the specified drawable.
\f2drawable-root\fP returns a window, all other functions return
an integer.
.
.Pr map-window window
.LP
See \f2XMapWindow\fP.
.
.Pr unmap-window window
.LP
See \f2XUnmapWindow\fP.
.
.Pr destroy-window window
.LP
See \f2XDestroyWindow\fP.
.
.Pr destroy-subwindows window
.LP
See \f2XDestroySubwindows\fP.
.
.Pr map-subwindows window
.LP
See \f2XMapSubwindows\fP.
.
.Pr unmap-subwindows window
.LP
See \f2XUnmapSubwindows\fP.
.
.Pr circulate-subwindows window direction
.LP
See \f2XCirculateSubwindows\fP.
\f2direction\fP is a symbol (\f5raise-lowest\fP or \f5lower-highest\fP).
.
.Pr clear-window window
.LP
Performs a \f2clear-area\fP on the entire window.
.
.Pr raise-window window
.LP
See \f2XRaiseWindow\fP.
.
.Pr lower-window window
.LP
See \f2XLowerWindow\fP.
.
.Pr restack-windows list-of-windows
.LP
See \f2XRestackWindows\fP.
.
.Pr query-tree window
.LP
See\f2 XQueryTree\fP.
Returns a list of three elements: root window, parent window, and
children (a vector of windows).
. 
.Pr translate-coordinates src-window x y dst-window
.LP
See \f2XTranslateCoordinates\fP.
Returns a list of three elements: destination x and y, and child window.
.
.Pr query-pointer window
.LP
See \f2XQueryPointer\fP.
Returns a list of eight elements: x and y, a boolean indicating whether
the pointer is on the same screen as the specified window, the root
window, the root window's x and y coordinates, the child window,
and a list of modifier names (see \f2grab-button\fP
.Ix grab-button
below).
.
.Ch "Window Property and Selection Functions"
.
.Pr atom? x
.LP
Returns #t iff \f2x\fP is an object of type \f2atom\fP.
.
.Pr make-atom value
.LP
Returns an atom with the given \f2value\fP.
\f2value\fP is an integer.
.
.Pr intern-atom display name
.LP
See \f2XInternAtom\fP.
\f2name\fP is a string or a symbol.
The atom is created if it does not yet exist.
.
.Pr find-atom display name
.LP
See \f2XInternAtom\fP.
\f2name\fP is a string or a symbol.
If the atom does not exist, the symbol \f5none\fP is returned.
.
.Pr atom-name display atom
.LP
See \f2XGetAtomName\fP.
Returns a string.
.
.Pr list-properties window
.LP
See \f2XListProperties\fP.
Returns a vector of atoms.
.
.Pr get-property window property request-type offset length delete?
.LP
See \f2XGetWindowProperty\fP.
\f2property\fP is an object of type \f2atom\fP.
\f2request-type\fP is an atom or #f in which case \f2AnyPropertyType\fP
will be used.
\f2offset\fP and \f2length\fP are integers.
An error is signaled if \f2XGetWindowProperty\fP fails.
.LP
\f2get-property\fP returns a list of four items: the ``actual type''
(an atom), the format (an integer), the data (if any, the empty list
otherwise), and the number of bytes left (an integer).
.LP
The data returned is either a string (if the format indicates
8-bit data) or a vector of integers.
.
.Pr change-property window property type format mode data
.LP
See \f2XChangeProperty\fP.
\f2property\fP and \f2type\fP are atoms.
\f2format\fP is an integer (8, 16, or 32).
If \f2format\fP is 8 \f2data\fP must be a string, otherwise a vector of
integers of the appropriate size.
An error is signaled if the
value of \f2format\fP is invalid or if \f2data\fP holds an integer
that exceeds the size indicated by \f2format\fP.
\f2mode\fP is a symbol (\f5replace\fP, \f5prepend\fP, or \f5append\fP).
.
.Pr delete-property window property
.LP
See \f2XDeleteProperty\fP.
.
.Pr rotate-properties window vector-of-atoms delta
.LP
See \f2XRotateWindowProperties\fP.
\f2delta\fP is the amount to rotate (an integer).
.
.Pr set-selection-owner! display selection owner time
.LP
See \f2XSetSelectionOwner\fP.
\f2selection\fP is an atom; \f2owner\fP is a window; \f2time\fP is an
integer or the symbol \f5now\fP (for \f2CurrentTime\fP).
.
.Pr selection-owner display selection
.LP
See \f2XGetSelectionOwner\fP.
.
.Pr convert-selection selection target property requestor-window time
.LP
See \f2XConvertSelection\fP.
\f2selection\fP and \f2target\fP are atoms;
\f2property\fP is an atom or the symbol \f5none\fP.
.
.Ch "Colormap Functions"
.
.Pr color? x
.LP
Returns #t iff \f2x\fP is an object of type \f2color\fP.
.
.Pr make-color r g b
.LP
Returns an object of type \f2color\fP with the specified RGB components.
\f2r\fP, \f2g\fP, and \f2b\fP are reals in the range 0.0 to 1.0.
.
.Pr color-rgb-values color
.LP
Returns a list of three elements, the RGB components of the
given color (see \f2make-color\fP
.Ix make-color
above).
.
.Pr query-color colormap pixel
.LP
See \f2XQueryColor\fP.
.
.Pr query-colors colormap pixels
.LP
See \f2XQueryColors\fP.
\f2pixels\fP is a vector of pixels.
Returns a vector of colors of the same size as \f2pixels\fP.
.
.Pr lookup-color colormap color-name
.LP
See \f2XLookupColor\fP.
\f2color-name\fP is a string or a symbol.
Returns a pair of colors.
.
.Pr alloc-color colormap color
.LP
See \f2XAllocColor\fP.
Returns a pixel (or #f in case of an error).
.
.Pr alloc-named-color colormap color-name
.LP
See \f2AllocNamedColor\fP.
\f2color-name\fP is a string or a symbol.
Returns a list of three elements: a pixel, and two colors (the closest
color and the exact color); or #f in case of an error.
.
.Pr colormap? x
.LP
Returns #t iff \f2x\fP is an object of type \f2colormap\fP.
.
.Pr colormap-display colormap
.LP
Returns the display associated with the given colormap.
.
.Pr free-colormap colormap
.LP
See \f2XFreeColormap\fP.
.
.Ch "Pixel Functions"
.
.Pr pixel? x
.LP
Returns #t iff \f2x\fP is an object of type \f2pixel\fP.
.
.Pr pixel-value pixel
.LP
Returns the value of the pixel as an unsigned integer.
.
.[[
.Pr black-pixel display
.Pr white-pixel display
.]]
.LP
See \f2XBlackPixel\fP, \f2XWhitePixel\fP.
Returns the black/white pixel of the display's default screen.
.
.Ch "Pixmap Functions"
.
.Pr pixmap? x
.LP
Returns #t iff \f2x\fP is an object of type \f2pixmap\fP.
.
.Pr pixmap-display pixmap
.LP
Returns the display associated with the pixmap.
.
.Pr free-pixmap pixmap
.LP
See \f2XFreePixmap\fP.
.
.Pr create-pixmap drawable width height depth
.LP
See \f2XCreatePixmap\fP.
.
.Pr create-bitmap-from-data window data width height
.LP
See \f2XCreateBitmapFromData\fP.
\f2data\fP is a string.
\f5(* width height)\fP must not exceed the number of bits in \f2string\fP.
.
.Pr create-pixmap-from-bitmap-data win data width height foregrnd backgrnd depth
.LP
See \f2XCreatePixmapFromBitmapData\fP.
\f2data\fP is a string.
\f5(* width height)\fP must not exceed the number of bits in \f2string\fP.
.
.Pr read-bitmap-file drawable filename
.LP
See \f2XReadBitmapFile\fP.
\f2filename\fP is a string or a symbol.
If \f2XReadBitmapFile\fP signals an error, \f2read-bitmap-file\fP
returns a symbol (\f5open-failed\fP, \f5file-invalid\fP, or \f5no-memory\fP).
If it succeeds, \f2read-bitmap-file\fP returns a list of five elements:
the bitmap (an object of type \f2pixmap\fP), the width and height of the
bitmap, and the x and y coordinates of the hotspot.
.
.Pr write-bitmap-file filename pixmap width height x-hot y-hot
.LP
See \f2XWriteBitmapFile\fP.
\f2filename\fP is a string or a symbol.
\f2x-hot\fP and \f2y-hot\fP are optional
(\(mi1 is used if they are omitted), but either both or none of them
must be given.
\f2write-bitmap-file\fP returns a symbol (\f5success\fP, \f5open-failed\fP,
\f5file-invalid\fP, or \f5no-memory\fP).
.
.Ch "Graphics Context Functions"
.
.Pr gcontext? x
.LP
Returns #t iff \f2x\fP is an object of type \f2gcontext\fP.
.
.Pr gcontext-display gcontext
.LP
Returns the display associated with the given GC.
.
.Pr create-gcontext . args
.LP
See \f2XCreateGC\fP.
This function is used to create a new GC.
.LP
The number of arguments must be even.
The 1st, 3rd, etc. argument is the name (a symbol) of an attribute
to be set when the graphics context is created, the 2nd, 4th, etc.\& argument
is the corresponding value.
The attributes can be specified in any order.
.LP
Attributes are \f2window\fP (a drawable; mandatory) and all the attributes
that can be set by the \f5set-gcontext-\fP\f2attribute\fP\f5!\fP functions
below.
.
.Pr copy-gcontext gcontext drawable
.LP
See \f2XCopyGC\fP.
Returns a copy of \f2gcontext\fP (associated with the specified drawable).
.
.Pr free-gcontext gcontext
.LP
See \f2XFreeGC\fP.
.
.Pr query-best-size display width height shape
.LP
See \f2XQueryBestSize\fP.
\f2shape\fP is a symbol (\f5cursor\fP, \f5tile\fP, or \f5stipple\fP).
Returns a pair of integers (result width and result height).
.
.[[
.Pr query-best-cursor display width height
.Pr query-best-tile display width height
.Pr query-best-stipple display width height
.]]
.LP
See \f2XQueryBestSize\fP.
Invokes \f2query-best-size\fP with the given arguments and a shape
of \f5cursor\fP, \f5tile\fP, or \f5stipple\fP, respectively.
.
.[[
.Pr gcontext-function gcontext
.Pr gcontext-plane-mask gcontext
.Pr gcontext-foreground gcontext
.Pr gcontext-background gcontext
.Pr gcontext-line-width gcontext
.Pr gcontext-line-style gcontext
.Pr gcontext-cap-style gcontext
.Pr gcontext-join-style gcontext
.Pr gcontext-fill-style gcontext
.Pr gcontext-fill-rule gcontext
.Pr gcontext-arc-mode gcontext
.Pr gcontext-tile gcontext
.Pr gcontext-stipple gcontext
.Pr gcontext-ts-x gcontext
.Pr gcontext-ts-y gcontext
.Pr gcontext-subwindow-mode gcontext
.Pr gcontext-exposures gcontext
.Pr gcontext-clip-x gcontext
.Pr gcontext-clip-y gcontext
.Pr gcontext-dash-offset gcontext
.]]
.LP
See \f2XGetGCValues\fP.
Returns the 
logical operation, plane mask, foreground and background pixel
value, line width and style, cap and join style, fill style and rule,
arc mode, tiling and stippling pixmap, tiling x- and y-origin,
subwindow mode, clipping x- and y-origin, and dashed line
information of the specified graphics context.
.LP
\f2gcontext-function\fP returns a symbol
(\f5clear\fP, \f5and\fP, \f5and-reverse\fP, \f5copy\fP, \f5and-inverted\fP,
\f5no-op\fP, \f5xor\fP, \f5or\fP, \f5nor\fP, \f5equiv\fP, \f5invert\fP,
\f5or-reverse\fP, \f5copy-inverted\fP, \f5nand\fP, or \f5set\fP).
\f2gcontext-plane-mask\fP, \f2gcontext-foreground\fP,
and \f2gcontext-background\fP return a pixel.
\f2gcontext-tile\fP and \f2gcontext-stipple\fP return a pixmap.
The line style is a symbol (\f5solid\fP, \f5dash\fP, \f5double-dash\fP);
the cap style is a symbol (\f5not-last\fP, \f5butt\fP, \f5round\fP,
\f5projecting\fP); the join style is a symbol (\f5miter\fP, \f5round\fP,
\f5bevel\fP); the fill style is a symbol (\f5solid\fP, \f5tiled\fP,
\f5stippled\fP, \f5opaque-stippled\fP); the fill rule is a symbol
(\f5even-odd\fP, \f5winding\fP); the arc mode is a symbol (\f5chord\fP,
\f5pie-slice\fP); the subwindow-mode is a symbol
(\f5clip-by-children\fP, \f5include-inferiors\fP).
\f2gcontext-exposures\fP returns a boolean.
All other functions return an integer.
.
.[[
.Pr set-gcontext-function! gcontext value
.Pr set-gcontext-plane-mask! gcontext value
.Pr set-gcontext-foreground! gcontext value
.Pr set-gcontext-background! gcontext value
.Pr set-gcontext-line-width! gcontext value
.Pr set-gcontext-line-style! gcontext value
.Pr set-gcontext-cap-style! gcontext value
.Pr set-gcontext-join-style! gcontext value
.Pr set-gcontext-fill-style! gcontext value
.Pr set-gcontext-fill-rule! gcontext value
.Pr set-gcontext-arc-mode! gcontext value
.Pr set-gcontext-tile! gcontext value
.Pr set-gcontext-stipple! gcontext value
.Pr set-gcontext-ts-x! gcontext value
.Pr set-gcontext-ts-y! gcontext value
.Pr set-gcontext-font! gcontext value
.Pr set-gcontext-subwindow-mode! gcontext value
.Pr set-gcontext-exposures! gcontext value
.Pr set-gcontext-clip-x! gcontext value
.Pr set-gcontext-clip-y! gcontext value
.Pr set-gcontext-clip-mask! gcontext value
.Pr set-gcontext-dash-offset! gcontext value
.]]
.LP
See \f2XChangeGC\fP.
Sets the logical operation, plane mask, foreground and background pixel
value, line width and style, cap and join style, fill style and rule,
arc mode, tiling and stippling pixmap, tiling x- and y-origin, font,
subwindow mode, clipping x- and y-origin, clipping bitmap, and dashed line
information for the specified graphics context.
.LP
The \f2value\fP argument to \f2set-gcontext-font!\fP is a font,
and the \f2value\fP argument to \f2set-gcontext-clip-mask!\fP
is a pixmap.
For the types of the \f2value\fP argument of the other functions
see the return values of the \f2gcontext-\fP functions above.
.
.Pr set-gcontext-clip-rectangles! gcontext x y rectangles ordering
.LP
See \f2XSetClipRectangles\fP.
\f2x\fP and \f2y\fP are integers (the coordinates of the clip-mask origin).
\f2rectangles\fP is a vector of lists of four integers (x, y, width,
and height of each rectangle).
\f2ordering\fP is a symbol (\f5unsorted\fP, \f5y-sorted\fP, \f5yx-sorted\fP,
or \f5yx-banded\fP).
.
.Pr set-gcontext-dashlist! gcontext dash-offset dash-list
.LP
See \f2XSetDashes\fP.
\f2dash-offset\fP is an integer.
\f2dash-list\fP is a vector of integers between 0 and 255.
.
.Ch "Graphics Functions"
.
.Pr clear-area window x y width height exposures?
.LP
See \f2XClearArea\fP.
.
.Pr copy-area src-drawable gcontext src-x src-y width height dst-drawable "dst-x dst-y"
.LP
See \f2XCopyArea\fP.
.
.Pr copy-plane src-drawable gcontext plane src-x src-y width height "dst-drawable dst-x dst-y"
.LP
See \f2XCopyPlane\fP.
\f2plane\fP is an integer.
An error is signaled unless exactly one bit is set in \f2plane\fP.
.
.Pr draw-point drawable gcontext x y
.LP
See \f2XDrawPoint\fP.
.
.Pr draw-points drawable gcontext vector-of-points relative?
.LP
See \f2XDrawPoints\fP.
\f2vector-of-points\fP is a vector of pairs consisting of two integers
(the x and y coordinates).
If \f2relative?\fP is #t, \f2CoordModePrevious\fP
is passed to \f2XDrawPoints\fP, otherwise \f2CoordModeOrigin\fP is used.
.
.Pr draw-line drawable gcontext x1 y1 x2 y2
.LP
See \f2XDrawLine\fP.
.
.Pr draw-lines drawable gcontext vector-of-points relative?
.LP
See \f2XDrawLines\fP.
See \f2draw-points\fP
.Ix draw-points
above.
.
.Pr draw-segments drawable gcontext vector-of-points
.LP
See \f2XDrawSegments\fP.
\f2vector-of-points\fP is a vector of lists of four integers
(x1, y1, x2, and y2).
.
.Pr draw-rectangle drawable gcontext x y width height
.LP
See \f2XDrawRectangle\fP.
.
.Pr fill-rectangle drawable gcontext x y width height
.LP
See \f2XFillRectangle\fP.
.
.Pr draw-rectangles drawable gcontext vector-of-rectangles
.LP
See \f2XDrawRectangles\fP.
\f2vector-of-rectangles\fP is a vector of lists of four integers
(x, y, width, and height of each rectangle).
.
.Pr fill-rectangles drawable gcontext vector-of-rectangles
.LP
See \f2XFillRectangles\fP.
See \f2draw-rectangles\fP
.Ix draw-rectangles
above.
.
.Pr draw-arc drawable gcontext x y width height angle1 angle2
.LP
See \f2XDrawArc\fP.
.
.Pr fill-arc drawable gcontext x y width height angle1 angle2
.LP
See \f2XFillArc\fP.
.
.Pr draw-arcs drawable gcontext vector-of-data
.LP
See \f2XDrawArcs\fP.
\f2vector-of-data\fP is a vector of lists of six integers
(x, y, width, height, angle1, and angle2).
.
.Pr fill-arcs drawable gcontext vector-of-data
.LP
See \f2XFillArcs\fP.
See \f2draw-arcs\fP
.Ix draw-arcs
above.
.
.Pr fill-polygon drawable gcontext vector-of-points relative? shape
.LP
See \f2XFillPolygon\fP.
See \f2draw-points\fP
.Ix draw-points
above.
\f2shape\fP is a symbol (\f5complex\fP, \f5non-convex\fP, or \f5convex\fP).
.
.Ch "Font Functions"
.
.Pr font? x
.LP
Returns #t iff \f2x\fP is an object of type \f2font\fP.
.
.Pr font-display
.LP
Returns the display associated with the given font.
.
.Pr open-font display font-name
.LP
See \f2XLoadQueryFont\fP.
\f2font-name\fP is a string or a symbol.
.
.Pr close-font font
.LP
See \f2XUnloadFont\fP.
.
.Pr font-name font
.LP
Returns the name of the specified font (a string) or #f if the name could
not be determined (e.\|g.\& when the font has been obtained by a call
to \f2gcontext-font\fP).
.
.Pr gcontext-font gcontext
.LP
Calls \f2XQueryFont\fP with the GC obtained by \f2XGContextFromGC\fP.
Only a limited number of functions can be applied to a font
returned by \f2gcontext-font\fP, since it has neither a name nor
a font-ID.
.
.Pr list-font-names display pattern
.LP
See \f2XListFonts\fP.
\f2pattern\fP is a string or a symbol.
Returns a vector of font names (strings).
.
.Pr list-fonts display pattern
.LP
See \f2XListFontsWithInfo\fP.
\f2pattern\fP is a string or a symbol.
Returns a vector of fonts.
These fonts are ``pseudo fonts'' which do not have a font-ID.
A pseudo font is loaded automatically and turned into a ``real''
font the first time it is passed to a function that makes use
of the font-ID.
.
.[[
.Pr font-direction font
.Pr font-min-byte2 font
.Pr font-max-byte2 font
.Pr font-min-byte1 font
.Pr font-max-byte1 font
.Pr font-all-chars-exist? font
.Pr font-default-char font
.Pr font-ascent font
.Pr font-descent font
.]]
.LP
These functions return the font direction as a symbol (\f5left-to-right\fP
or \f5right-to-left\fP), the first and last character (as an integer),
the first and last row (integer), an indication whether all characters
have non-zero size (boolean), the default character (integer), and the
ascent and descent (integer) of the specified font.
.
.[[
.Pr char-rbearing font index
.Pr char-lbearing font index
.Pr char-width font index
.Pr char-ascent font index
.Pr char-descent font index
.]]
.LP
These functions return the metrics of
the character specified by the integer \f2index\fP of the given font.
Each function returns an integer.
\f2font\fP can be a 1-byte as well as a 2-byte font.
.
.[[
.Pr max-char-lbearing font
.Pr max-char-rbearing font
.Pr max-char-width font
.Pr max-char-ascent font
.Pr max-char-descent font
.]]
.LP
These functions return the maximum metrics over all characters
in the specified font.
Each function returns an integer.
.
.[[
.Pr min-char-lbearing font
.Pr min-char-rbearing font
.Pr min-char-width font
.Pr min-char-ascent font
.Pr min-char-descent font
.]]
.LP
These functions return the minimum metrics over all characters
in the specified font.
Each function returns an integer.
.
.Pr font-properties font
.LP
Returns a vector of font properties; each element of the vector
is a pair consisting of the property name (an atom) and an
unsigned integer (the value of the property).
.
.Pr font-property font property-name
.LP
Returns the value of the specified property associated with the
specified font.
\f2property-name\fP is a string or a symbol.
.
.Pr font-path display
.LP
See \f2XGetFontPath\fP.
Returns the current font path as a vector of strings.
.
.Pr set-font-path! display path
.LP
See \f2XSetFontPath\fP.
\f2path\fP is a list; each element is a string or a symbol.
.
.Ch "Text Metrics and Text Drawing Functions"
.
.Pr text-width font text format
.LP
See \f2XTextWidth\fP, \f2XTextWidth16\fP.
\f2format\fP indicates whether 8-bit or 16-bit text is used; it is either
the symbol \f51-byte\fP or the symbol \f52-byte\fP.
\f2text\fP is a vector of integers; the integers must not exceed the
size indicated by the format.
.
.[[
.Pr extents-lbearing font text format
.Pr extents-rbearing font text format
.Pr extents-width font text format
.Pr extents-ascent font text format
.Pr extents-descent font text format
.]]
.LP
See \f2XTextExtents\fP, \f2XTextExtents16\fP.
These functions are used to compute the overall metrics of an 8-bit
or 16-bit character string.
Each function returns an integer.
For the format of \f2text\fP and \f2format\fP see \f2text-width\fP
.Ix text-width
above.
.
.Pr draw-image-text drawable gcontext x y text format
.LP
See \f2XDrawImageString\fP, \f2XDrawImageString16\fP.
See \f2text-width\fP
.Ix text-width
above.
.
.Pr draw-poly-text drawable gcontext x y text format
.LP
See \f2XDrawText\fP, \f2XDrawText16\fP.
See \f2text-width\fP
.Ix text-width
above.
\f2text\fP is a vector of integers with intermixed objects of type \f2font\fP.
.
.Pr translate-text string
.LP
Converts the string into a representation suitable as an argument
to \f2text-width\fP, \f2draw-image-text\fP, or \f2draw-poly-text\fP
(a vector of integers obtained by applying \f2char\(mi>integer\fP
to the characters of the string argument).
.
.Ch "Cursor Functions"
.
.Pr cursor? x
.LP
Returns #t iff \f2x\fP is an object of type \f2cursor\fP.
.
.Pr cursor-display cursor
.LP
Returns the display associated with the given cursor.
.
.Pr free-cursor
.LP
See \f2XFreeCursor\fP.
.
.Pr create-cursor src mask x y foreground background
.LP
See \f2XCreatePixmapCursor\fP.
\f2src\fP and \f2mask\fP are pixmaps.
\f2mask\fP can be the symbol \f5none\fP.
.
.Pr create-glyph-cursor src src-char mask mask-char foreground background
.LP
See \f2XCreateGlyphCursor\fP.
\f2src\fP and \f2mask\fP are fonts.
\f2mask\fP can be the symbol \f5none\fP.
The display is obtained from \f2src\fP.
\f2src-char\fP and \f2mask-char\fP are integers.
.
.Pr create-font-cursor display src-char
.LP
See \f2XCreateGlyphCursor\fP.
Calls \f2create-glyph-cursor\fP with the font named ``cursor'', the
specified \f2src-char\fP, a \f2mask-char\fP of \f5(1+ src-char)\fP,
black foreground, and white background.
.
.Pr recolor-cursor cursor foreground background
.LP
See \f2XRecolorCursor\fP
.
.Pr define-cursor window cursor
.LP
Synonym for \f5(set-window-cursor! window cursor)\fP.
.
.Pr undefine-cursor window
.LP
Synonym for \f5(set-window-cursor! window 'none)\fP.
.
.Ch "Grab Functions"
.
.Pr grab-pointer window owner? events ptr-sync? kbd-sync? confine-to cursor time
.LP
See \f2XGrabPointer\fP.
\f2window\fP and \f2confine-to\fP are windows.
\f2events\fP is a list of symbols (event mask names, such as \f5enter-window\fP,
\f5pointer-motion\fP, etc.).
\f2ptr-sync?\fP and \f2kbd-sync?\fP determine whether synchronous
or asynchronous grab mode is to be used.
\f2time\fP is an integer or the symbol \f5now\fP (for \f2CurrentTime\fP).
\f2grab-pointer\fP returns a symbol (\f5success\fP, \f5not-viewable\fP,
\f5already-grabbed\fP, \f5frozen\fP, or \f5invalid-time\fP).
.
.Pr ungrab-pointer display time
.LP
See \f2XUngrabPointer\fP.
.
.Pr grab-button win button mod owner? events ptr-sync? kbd-sync? "confine-to cursor"
.LP
See \f2XGrabButton\fP.
\f2button\fP is a symbol (\f5button1\fP ... \f5button5\fP, or \f5any-button\fP).
\f5mod\fP (modifiers) is a list of symbols (\f5shift\fP, \f5lock\fP,
\f5control\fP, \f5mod1\fP ... \f5mod5\fP, \f5button1\fP ... \f5button5\fP,
or \f5any-modifier\fP).
For the other arguments see \f2grab-pointer\fP
.Ix grab-pointer
above.
.
.Pr ungrab-button window button modifiers
.LP
See \f2XUngrabButton\fP.
See \f2grab-button\fP
.Ix grab-button
above.
.
.Pr change-active-pointer-grab display events cursor time
.LP
See \f2XChangeActivePointerGrab\fP.
\f2events\fP is a list of symbols (event mask names, such as \f5enter-window\fP,
\f5pointer-motion\fP, etc.).
.
.Pr grab-keyboard window owner? pointer-sync? keyboard-sync? time
.LP
See \f2XGrabKeyboard\fP.
For a description of the arguments and the return value see \f2grab-pointer\fP
.Ix grab-pointer
above.
.
.Pr ungrab-keyboard display time
.LP
See \f2XUngrabKeyboard\fP.
.
.Pr grab-key window key modifiers owner? pointer-sync? keyboard-sync?
.LP
See \f2XGrabKey\fP.
\f2key\fP is a keycode (an integer) or the symbol \f5any\fP.
For the other arguments see \f2grab-pointer\fP
.Ix grab-pointer
above.
.
.Pr ungrab-key window key modifiers
.LP
See \f2XUngrabKey\fP.
See \f2grab-key\fP
.Ix grab-key
above.
.
.Pr allow-events display mode time
.LP
See \f2XAllowEvents\fP.
\f2mode\fP is a symbol (\f5async-pointer\fP, \f5sync-pointer\fP,
\f5replay-pointer\fP, \f5async-keyboard\fP, \f5sync-keyboard\fP,
\f5replay-keyboard\fP, \f5async-both\fP, or \f5sync-both\fP).
.
.Pr grab-server display
.LP
See \f2XGrabServer\fP.
.
.Pr ungrab-server display
.LP
See \f2XUngrabServer\fP.
.
.Sy with-server-grabbed display . body-forms
.LP
This macro performs a \f2grab-server\fP on the specified display,
evaluates the \f2body-forms\fP in order, and then ungrabs the server.
The macro body is guarded by a \f2dynamic-wind\fP to ensure that the
\f2ungrab-server\fP is performed when a body-form calls a continuation
created outside the macro, and that it is grabbed again when
the body is re-entered at a later point in time.
\f2with-server-grabbed\fP returns the value of the last body-form.
.
.Ch "Window Manager Functions"
.
.Pr reparent-window window parent-window x y
.LP
See \f2XReparentWindow\fP.
.
.Pr install-colormap colormap
.LP
See \f2XInstallColormap\fP.
.
.Pr uninstall-colormap colormap
.LP
See \f2XUninstallColormap\fP.
.
.Pr list-installed-colormaps window
.LP
See \f2XListInstalledColormaps\fP.
Returns a vector of colormaps.
.
.Pr set-input-focus display window revert-to time
.LP
See \f2XSetInputFocus\fP.
\f2window\fP can be the symbol \f5pointer-root\fP.
\f2revert-to\fP is a symbol (\f5none\fP, \f5pointer-root\fP, or \f5parent\fP).
\f2time\fP is an integer or the symbol \f5now\fP.
.
.Pr input-focus display
.LP
See \f2XGetInputFocus\fP.
Returns a pair the car of which is a window, and the cdr is a symbol
(\f5none\fP, \f5pointer-root\fP, or \f5parent\fP).
.
.Pr general-warp-pointer display dst-win dst-x dst-y src-win src-x src-y "src-width src-height"
.LP
See \f2XWarpPointer\fP.
.
.Pr warp-pointer dst-window dst-x dst-y
.LP
See \f2XWarpPointer\fP.
Invokes \f2general-warp-pointer\fP with the display associated with the
\f2dst-window\fP, the \f2dst-window\fP, \f2dst-x\fP, \f2dst-y\fP,
a \f2src-window\fP of \f5none\fP, and zero source coordinates and dimensions.
.
.Pr warp-pointer-relative display x-offset y-offset
.LP
See \f2XWarpPointer\fP.
Invokes \f2general-warp-pointer\fP with the specified \f2display\fP,
a \f2dst-window\fP of \f5none\fP, \f2x-offset\fP, \f2y-offset\fP,
a \f2src-window\fP of \f5none\fP, and zero source coordinates and dimensions.
.
.Pr bell display . percent
.LP
See \f2XBell\fP.
\f2percent\fP is an integer between -100 and 100.
If \f2percent\fP is omitted, 0 is used.
.
.Pr set-access-control display enable?
.LP
See \f2XSetAccessControl\fP.
.
.Pr change-save-set window mode
.LP
See \f2XChangeSaveSet\fP.
\f2mode\fP is a symbol (\f5insert\fP or \f5delete\fP).
.
.Pr set-close-down-mode display mode
.LP
See \f2XSetCloseDownMode\fP.
\f2mode\fP is a symbol (\f5destroy-all\fP, \f5retain-permanent\fP,
or \f5retain-temporary\fP).
.
.Pr get-pointer-mapping display
.LP
See \f2XGetPointerMapping\fP.
Returns a vector of 256 integers.
.
.Pr set-pointer-mapping display mapping
.LP
See \f2XSetPointerMapping\fP.
\f2mapping\fP is a vector of integers.
Returns #t if \f2XSetPointerMapping\fP succeeds, #f otherwise.
.
.Ch "Event Handling Functions"
.
.Pr event-listen display wait?
.LP
See \f2XPending\fP, \f2XPeekEvent\fP.
Returns the size of the display's event queue.
If \f2wait?\fP is true and the event queue is empty, \f2event-listen\fP
flushes the output buffer and blocks until an event is received from
the server.
.
.Pr get-motion-events window from-time to-time
.LP
See \f2XGetMotionEvents\fP.
\f2from-time\fP and \f2to-time\fP are integers or the symbol \f5now\fP.
\f2get-motion-events\fP returns a vector of lists of three elements:
a time stamp (an integer or the symbol \f5now\fP), and the x and y 
coordinates (integers).
.
.Sy handle-events display discard? peek? . clauses
.LP
See \f2XNextEvent\fP, \f2XPeekEvent\fP, \f2XIfEvent\fP, \f2XPeekIfEvent\fP.
\f2handle-events\fP is a special form.
Each \f2clause\fP is of the form \f2(guard function)\fP; \f2guard\fP
is either an event name (a symbol, e.\|g.\& \f5key-press\fP or \f5exposure\fP),
a list of event names, or the symbol \f5else\fP.
\f2handle-events\fP gets the next event from the specified display.
Then the event type is matched against each event name in each guard
in order.
When a match occurs, the corresponding function is invoked with
the name of the event being dispatched (a symbol) and other, event
specific arguments (see below).
When no clause matches and an \f5else\fP clause is present, the function
from this clause is invoked.
\f2handle-events\fP loops until a function returns a value not
equal to #f in which case handle-events returns this value.
.LP
If \f2discard?\fP is true, unprocessed events (i.\|e.\& events for which
no matching clause has been found) are removed from the event queue,
otherwise they are left in place.
If \f2peek?\fP is true, processed events are not removed from
the event queue.
.LP
The following list gives all event specific arguments for each
event type.
The first argument is always the event type (a symbol).
.LP
In the following list, arguments with names of the form
\f2something-window\fP (or simply \f2window\fP) are always of type
\f2window\fP;
arguments with names of the form \f2something-atom\fP (or simply \f2atom\fP)
are always of type \f2atom\fP.
\f2time\fP is an integer or the symbol \f5now\fP.
\f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP, \f2border-width\fP,
\f2x-root\fP, \f2y-root\fP, \f2count\fP, \f2major-code\fP, \f2minor-code\fP,
and \f2keycode\fP are integers.
\f2state\fP is a list of symbols (\f5shift\fP, \f5lock\fP, \f5control\fP,
\f5mod1\fP ... \f5mod5\fP, \f5button1\fP ... \f5button5\fP).
\f2button\fP is one of the symbols \f5button1\fP ... \f5button5\fP,
\f2button-mask\fP is a list of one or more of these symbols.
\f2cross-mode\fP is a symbol (\f5normal\fP, \f5grab\fP, \f5ungrab\fP).
\f2place\fP is a symbol (\f5top\fP or \f5bottom\fP).
.
.IP "\f3key-press, key-release:\fP"
.Ix "Event types" key-press
.Ix "Event types" key-release
\f2window\fP, \f2root-window\fP, \f2sub-window\fP, \f2time\fP,
\f2x\fP, \f2y\fP, \f2x-root\fP, \f2y-root\fP, \f2state\fP, \f2keycode\fP,
\f2same-screen?\fP.
.
.IP "\f3button-press, button-release:\fP"
.Ix "Event types" button-press
.Ix "Event types" button-release
\f2window\fP, \f2root-window\fP, \f2sub-window\fP, \f2time\fP,
\f2x\fP, \f2y\fP, \f2x-root\fP, \f2y-root\fP, \f2state\fP, \f2button\fP,
\f2same-screen?\fP.
.
.IP "\f3motion-notify:\fP"
.Ix "Event types" motion-notify
\f2window\fP, \f2root-window\fP, \f2sub-window\fP, \f2time\fP,
\f2x\fP, \f2y\fP, \f2x-root\fP, \f2y-root\fP, \f2state\fP, \f2is-hint?\fP,
\f2same-screen?\fP.
.
.IP "\f3enter-notify, leave-notify:\fP"
.Ix "Event types" enter-notify
.Ix "Event types" leave-notify
\f2window\fP, \f2root-window\fP, \f2sub-window\fP, \f2time\fP,
\f2x\fP, \f2y\fP, \f2x-root\fP, \f2y-root\fP, \f2cross-mode\fP,
\f2cross-detail\fP (one of the symbols \f5ancestor\fP, \f5virtual\fP,
\f5inferior\fP, \f5nonlinear\fP, \f5nonlinear-virtual\fP),
\f2same-screen?\fP, \f2focus?\fP, \f2button-mask\fP.
.
.IP "\f3focus-in, focus-out:\fP"
.Ix "Event types" focus-in
.Ix "Event types" focus-out
\f2window\fP, \f2cross-mode\fP, \f2focus-detail\fP (one of the symbols
\f5ancestor\fP, \f5virtual\fP, \f5inferior\fP, \f5nonlinear\fP,
\f5nonlinear-virtual\fP, \f5pointer\fP, \f5pointer-root\fP, \f5none\fP).
.
.IP "\f3keymap-notify:\fP"
.Ix "Event types" keymap-notify
\f2window\fP, \f2keymap\fP (a string of length 32).
.
.IP "\f3expose:\fP"
.Ix "Event types" expose
\f2window\fP, \f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP, \f2count\fP.
.
.IP "\f3graphics-expose:\fP"
.Ix "Event types" graphics-expose
\f2window\fP, \f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP, \f2count\fP,
\f2major-code\fP, \f2minor-code\fP.
.
.IP "\f3no-expose:\fP"
.Ix "Event types" no-expose
\f2window\fP, \f2major-code\fP, \f2minor-code\fP.
.
.IP "\f3visibility-notify:\fP"
.Ix "Event types" visibility-notify
\f2window\fP, \f2visibility-state\fP (one of the symbols \f5unobscured\fP,
\f5partially-obscured\fP, \f5fully-obscured\fP).
.
.IP "\f3create-notify:\fP"
.Ix "Event types" create-notify
\f2parent-window\fP, \f2window\fP, \f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP,
\f2border-width\fP, \f2override-redirect?\fP.
.
.IP "\f3destroy-notify:\fP"
.Ix "Event types" destroy-notify
\f2event-window\fP, \f2window\fP.
.
.IP "\f3unmap-notify:\fP"
.Ix "Event types" unmap-notify
\f2event-window\fP, \f2window\fP, \f2from-configure\fP.
.
.IP "\f3map-notify:\fP"
.Ix "Event types" map-notify
\f2event-window\fP, \f2window\fP, \f2override-redirect\fP.
.
.IP "\f3map-request:\fP"
.Ix "Event types" map-request
\f2parent-window\fP, \f2window\fP.
.
.IP "\f3reparent-notify:\fP"
.Ix "Event types" reparent-notify
\f2event-window\fP, \f2parent-window\fP, \f2window\fP, \f2x\fP, \f2y\fP,
\f2override-redirect\fP.
.
.IP "\f3configure-notify:\fP"
.Ix "Event types" configure-notify
\f2event-window\fP, \f2window\fP, \f2x\fP, \f2y\fP, \f2width\fP,
\f2height\fP, \f2border-width\fP, \f2above-window\fP,
\f2override-redirect?\fP.
.
.IP "\f3configure-request:\fP"
.Ix "Event types" configure-request
\f2parent-window\fP, \f2window\fP, \f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP,
\f2border-width\fP, \f2above-window\fP, \f2stack-mode\fP (see
\f2set-window-stack-mode!\fP above), \f2value-mask\fP (an integer).
.
.IP "\f3gravity-notify:\fP"
.Ix "Event types" gravity-notify
\f2event-window\fP, \f2window\fP, \f2x\fP, \f2y\fP.
.
.IP "\f3resize-request:\fP"
.Ix "Event types" resize-request
\f2window\fP, \f2width\fP, \f2height\fP.
.
.IP "\f3circulate-notify:\fP"
.Ix "Event types" circulate-notify
\f2event-window\fP, \f2window\fP, \f2place\fP.
.
.IP "\f3circulate-request:\fP"
.Ix "Event types" circulate-request
\f2parent-window\fP, \f2window\fP, \f2place\fP.
.
.IP "\f3property-notify:\fP"
.Ix "Event types" property-notify
\f2window\fP, \f2atom\fP, \f2time\fP, \f2property-state\fP (one of the
symbols \f5new-value\fP, \f5deleted\fP).
.
.IP "\f3selection-clear:\fP"
.Ix "Event types" selection-clear
\f2window\fP, \f2selection-atom\fP, \f2time\fP.
.
.IP "\f3selection-request:\fP"
.Ix "Event types" selection-request
\f2owner-window\fP, \f2requestor-window\fP, \f2selection-atom\fP,
\f2target-atom\fP, \f2property-atom\fP, \f2time\fP.
.
.IP "\f3selection-notify:\fP"
.Ix "Event types" selection-notify
\f2requestor-window\fP, \f2selection-atom\fP, \f2target-atom\fP,
\f2property-atom\fP, \f2time\fP.
.
.IP "\f3colormap-notify:\fP"
.Ix "Event types" colormap-notify
\f2window\fP, \f2colormap\fP, \f2new?\fP, \f2colormap-installed?\fP.
.
.IP "\f3client-message:\fP"
.Ix "Event types" client-message
\f2window\fP, \f2message type\fP (an atom), \f2message data\fP
(a string of length 20, or a vector of 10 or 5 integer numbers,
or, if the format field of the event is wrong, the format as a
number).
.
.IP "\f3mapping-notify:\fP"
.Ix "Event types" mapping-notify
\f2window\fP, \f2request\fP (one of the symbols \f5modifier\fP,
\f5keyboard\fP, \f5pointer\fP), \f2keycode\fP, \f2count\fP.
.
.Ch "Inter-Client Communication Functions"
.
.Pr iconify-window window screen-number
.LP
See \f2XIconifyWindow\fP.
.
.Pr withdraw-window window screen-number
.LP
See \f2XWithdrawWindow\fP.
.
.Pr reconfigure-wm-window . args
.LP
See \f2XReconfigureWMWindow\fP.
.LP
For the format of the arguments see \f2create-window\fP
.Ix create-window
above.
Mandatory attributes are \f2window\fP and \f2screen-number\fP
(an integer).
Optional attributes are \f2x\fP, \f2y\fP, \f2width\fP, \f2height\fP
\f2border-width\fP (integers), \f2sibling\fP (a window), and
\f2stack-mode\fP (a symbol; one of \f5above\fP, \f5below\fP, \f5top-if\fP,
\f5bottom-if\fP, \f5opposite\fP).
.
.Pr get-text-property window atom
.LP
See \f2XGetTextProperty\fP.
Returns a text property as a list of strings or #f if the specified property
does not exist.
.
.Pr set-text-property! window value atom
.LP
See \f2XSetTextProperty\fP.
\f2value\fP is a list holding the items of the text property
(strings or symbols).
.
.Pr wm-protocols window
.LP
See \f2XGetWMProtocols\fP.
Returns a vector of atoms.
.
.Pr set-wm-protocols! window protocols
.LP
See \f2XSetWMProtocols\fP.
\f2protocols\fP is a vector of atoms.
.
.Pr wm-name window
.LP
See \f2XGetTextProperty\fP.
Returns the WM_NAME property as a list of strings or #f if it does not exist.
.
.Pr set-wm-name! window name
.LP
See \f2XSetTextProperty\fP.
\f2name\fP is a list of strings or symbols.
.
.Pr wm-icon-name window
.LP
See \f2XGetTextProperty\fP.
Returns the WM_ICON_NAME property as a list of strings
or #f if it does not exist.
.
.Pr set-wm-icon-name! window name
.LP
See \f2XSetTextProperty\fP.
\f2name\fP is a list of strings or symbols.
.
.Pr wm-client-machine window
.LP
See \f2XGetTextProperty\fP, \f2XGetWMClientMachine\fP.
Returns the WM_CLIENT_MACHINE property as a list of strings
or #f if it does not exist.
.
.Pr set-wm-client-machine! window value
.LP
See \f2XSetTextProperty\fP, \f2XSetWMClientMachine\fP.
\f2value\fP is a list of strings or symbols.
.
.Pr wm-class window
.LP
See \f2XGetClassHint\fP.
Returns a pair (name and class) each component of which is either
a string or #f.
.
.Pr set-wm-class! window name class
.LP
See\f2 XSetClassHint\fP.
\f2name\fP and \f2class\fP are strings or symbols.
.
.Pr wm-command window
.LP
See \f2XGetCommand\fP (in X11 Release 4 or newer releases).
Returns the value of the WM_COMMAND property of the given window
as a list of strings.
.
.Pr set-wm-command! window command
.LP
See \f2XSetCommand\fP.
\f2command\fP is a list; each element is either a string or a symbol.
.
.Pr transient-for window
.LP
See \f2XGetTransientForHint\fP.
Returns a window.
.
.Pr set-transient-for! window property-window
.LP
See \f2XSetTransientForHint\fP.
.
.Pr wm-normal-hints window
.LP
See \f2XGetWMSizeHints\fP.
Returns a list of hints.
Each element is set to the empty list if the corresponding hint
has not been set for the specified window.
.LP
The elements of the list correspond to the following hints
(in this order): \f2x\fP, \f2y\fP, \f2width\fP, and \f2height\fP
(program specified); \f2x\fP, \f2y\fP, \f2width\fP and \f2height\fP
(user specified); \f2min-width\fP and \f2min-height\fP; \f2max-width\fP
and \f2max-height\fP; \f2width-inc\fP and \f2height-inc\fP;
\f2min-aspect-x\fP, \f2min-aspect-y\fP, \f2max-aspect-x\fP and
\f2max-aspect-y\fP; \f2base-width\fP and \f2base-height\fP;
and \f2gravity\fP.
All elements are integers except for the value of \f2gravity\fP
which is a symbol (see the \f2window-gravity\fP
.Ix window-gravity
procedure above).
.
.Pr set-wm-normal-hints! . args
.LP
See \f2XSetWMSizeHints\fP.
For the format of the arguments see \f2create-window\fP
.Ix create-window
above.
Attributes are \f2window\fP (mandatory) and the names of the hints
listed under \f2wm-normal-hints\fP
.Ix wm-normal-hints
above.
.
.Pr wm-hints window
.LP
See \f2XGetWMHints\fP.
Returns a list of hints.
Each element is set to the empty list if the corresponding hint
has not been set for the specified window.
.LP
The elements of the list correspond to the following hints
(in this order): \f2input?\fP, \f2initial-state\fP, \f2icon-pixmap\fP,
\f2icon-window\fP, \f2icon-x\fP, \f2icon-y\fP, \f2icon-mask\fP,
and \f2window-group\fP.
The value of \f2input?\fP is a boolean.
\f2initial-state\fP is a symbol (\f5dont-care\fP, \f5normal\fP, \f5zoom\fP,
\f5iconic\fP, \f5inactive\fP).
The values of \f2icon-pixmap\fP and \f2icon-mask\fP are pixmaps.
\f2icon-window\fP and \f2window-group\fP are windows.
\f2icon-x\fP and \f2icon-y\fP are integers.
.
.Pr set-wm-hints! . args
.LP
See \f2XSetWMHints\fP.
For the format of the arguments see \f2create-window\fP
.Ix create-window
above.
Attributes are \f2window\fP (mandatory) and the names of the hints
listed under \f2wm-hints\fP
.Ix wm-hints
above.
.
.Pr icon-sizes window
.LP
See \f2XGetIconSizes\fP.
Returns a vector of lists of six integers (\f2min-width\fP, \f2min-height\fP,
\f2max-width\fP, \f2max-height\fP, \f2width-inc\fP, and \f2height-inc\fP).
.
.Pr set-icon-sizes! window icon-sizes
.LP
See \f2XSetIconSizes\fP.
\f2icon-sizes\fP is a vector of lists of six integers (see \f2icon-sizes\fP
.Ix icon-sizes
above).
.
.Ch "Keyboard Utility Functions"
.
.[[
.Pr display-min-keycode display
.Pr display-max-keycode display
.]]
.LP
Returns the minimum/maximum keycode (an integer) for the given display.
.
.Pr display-keysyms-per-keycode display
Returns the number of keysyms per keycode for the given display.
.
.Pr string\(mi>keysym string
.LP
See \f2XStringToKeysym\fP.
\f2string\fP is a string or a symbol.
Returns an integer if \f2XStringToKeysym\fP succeeds, #f otherwise.
.
.Pr keysym\(mi>string keysym
.LP
See \f2XKeysymToString\fP.
\f2keysym\fP is an integer.
Returns #f if \f2XKeysymToString\fP fails.
.
.Pr keycode\(mi>keysym display keycode index
.LP
See \f2XKeycodeToKeysym\fP.
\f2keycode\fP and \f2index\fP are integers.
.
.Pr keysym\(mi>keycode display keysym
.LP
See \f2XKeysymToKeycode\fP.
\f2keysym\fP is an integer.
.
.Pr lookup-string display keycode mask
.LP
See \f2XLookupString\fP.
\f2keycode\fP is an integer.
\f2mask\fP is a list of symbols (\f5shift\fP, \f5lock\fP, \f5control\fP,
\f5mod1\fP ... \f5mod5\fP, \f5button1\fP ... \f5button5\fP,
or \f5any-modifier\fP).
.
.Pr rebind-keysym display keysym modifiers string
.LP
See \f2XRebindKeysym\fP.
\f2keysym\fP is an integer.
\f2modifiers\fP is a vector of integers.
.
.Pr refresh-keyboard-mapping window type
.LP
See \f2XRefreshKeyboardMapping\fP.
\f2type\fP is a symbol (\f5modifier\fP, \f5keyboard\fP, or \f5pointer\fP).
Invokes \f2XRefreshKeyboardMapping\fP with a faked event structure holding
the specified window and request type.
.
.Ch "Other Utility Functions"
.
.Pr xlib-release-4-or-later?
.LP
Returns always #t.
.
.Pr xlib-release-5-or-later?
.LP
Returns #t iff the Xlib extension is linked together with the X11
Release 5 Xlib or later versions of the Xlib.
.
.Pr xlib-release-6-or-later?
.LP
Returns #t iff the Xlib extension is linked together with the X11
Release 6 Xlib or later versions of the Xlib.
.
.Pr get-default display program option
.LP
See \f2XGetDefault\fP.
\f2program\fP and \f2option\fP are strings or symbols.
Returns a string of #f if the option does not exist for the
specified program.
.
.Pr resource-manager-string display
.LP
See \f2XResourceManagerString\fP.
Returns a string or #f if the RESOURCE_MANAGER property does not
exist on the root window.
.
.Pr parse-geometry string
.LP
See \f2XParseGeometry\fP.
Returns a list of six elements: two booleans indicating whether x or
or y are negative and four integers (x, y, width, and height).
Each of the elements can be #f to indicate that the respective
value was not found in the string.
.
.Pr parse-color colormap string
.LP
See \f2XParseColor\fP.
Returns an object of type \f2color\fP or #f if \f2XParseColor\fP fails.
.
.Pr store-buffer display bytes buffer
.LP
See \f2XStoreBuffer\fP.
\f2bytes\fP is a string; \f2buffer\fP is an integer between 0 and 7.
.
.Pr store-bytes display bytes
.LP
See \f2XStoreBytes\fP.
\f2bytes\fP is a string.
.
.Pr fetch-buffer display buffer
.LP
See \f2XFetchBuffer\fP.
\f2buffer\fP is an integer between 0 and 7.
Returns a string.
.
.Pr fetch-bytes display
.LP
See \f2XFetchBytes\fP.
Returns a string.
.
.Pr rotate-buffers display delta
.LP
See \f2XRotateBuffers\fP.
\f2delta\fP is an integer (the amount to rotate the buffers).
.
.Sy with object . body-forms
.LP
\f2with\fP is a macro.
\f2object\fP must be a drawable, a graphics context, or a font.
The \f2body-forms\fP are evaluated in order; \f2with\fP returns the value
of the last body-form.
.LP
Within the scope of the \f2with\fP, the first call to an accessor
function accessing \f2object\fP (such as \f5window-\fP\f2attribute\fP
or \f5font-\fP\f2attribute\fP) causes the result of the corresponding
Xlib function to be retained in a cache; subsequent calls just return
the value from the cache.
Likewise, calls to Xlib functions for mutator functions modifying
\f2object\fP (such as \f5set-window-\fP\f2attribute\fP\f5!\fP)
are delayed until exit of the \f2with\fP body or until an accessor
function is called and the cached data for this accessor function
has been invalidated by the call to a mutator function.
.
.Ch "Server Extension Functions"
.
.Pr list-extensions display
.LP
See \f2XListExtensions\fP.
Returns a vector of strings.
.
.Pr query-extension display name
.LP
See \f2XQueryExtension\fP.
\f2name\fP is a string or a symbol.
Returns a list of three elements: the major opcode (an integer) or #f
if the extension has no major opcode, the base event type code (an
integer) of #f if the extension has no additional event types, and
the base error code (an integer) of #f if the extension has no
additional error codes.
\f2query-extension\fP returns #f if the specified extension is not present.
.
.Ch "Error Handling"
.
.Va x-error-handler
.LP
See \f2XSetErrorHandler\fP.
If an error event is received and the global variable \f2x-error-handler\fP
is bound to a compound procedure, this procedure is invoked with the
following arguments: a display, the serial number of the failed request
(an integer), the error code (either an integer or one of the symbols
\f5bad-request\fP, \f5bad-value\fP, \f5bad-window\fP, \f5bad-pixmap\fP, 
\f5bad-atom\fP, \f5bad-cursor\fP, \f5bad-font\fP, \f5bad-match\fP, 
\f5bad-drawable\fP, \f5bad-access\fP, \f5bad-alloc\fP, \f5bad-color\fP, 
\f5bad-gcontext\fP, \f5bad-id-choice\fP, \f5bad-name\fP, \f5bad-length\fP, 
or \f5bad-implementation\fP), the major and minor op-code of the
failed request (integers), and a resource-ID (an integer).
.LP
If an error event is received and this variable is not bound to a
compound procedure, the Xlib default error handler is invoked.
The initial value of this variable is the empty list.
.
.Va x-fatal-error-handler
.LP
See \f2XSetIOErrorHandler\fP.
If a fatal I/O error occurs and the global variable
\f2x-fatal-error-handler\fP is bound to a compound procedure, this
procedure is invoked with a display as argument.
The procedure must invoke \f2exit\fP.
If a fatal error occurs and this variable is not bound to a
compound procedure, or if the procedure returns, the Xlib default
fatal error handler is invoked and the interpreter terminates with
an exit code of 1.
The initial value of this variable is the empty list.
.
.Ch "Interaction with the Garbage Collector"
.
.PP
.Ix "garbage collector"
The Scheme garbage collector destroys objects of type \f2colormap\fP,
\f2cursor\fP, \f2display\fP, \f2font\fP, \f2gcontext\fP, \f2pixmap\fP,
or \f2window\fP that are not longer accessible from within the Scheme
program.
This is done by invoking the function \f2free-colormap\fP, \f2free-cursor\fP,
\f2close-display\fP, \f2close-font\fP, \f2free-gcontext\fP,
\f2free-pixmap\fP, or \f2destroy-window\fP, respectively, with the
object to be destroyed as an argument.
.PP
The garbage collector only destroys objects that have been created
from with the Scheme program (by functions like \f2create-pixmap\fP
or \f2open-display\fP).
Objects that have been obtained from the Xlib through functions like
\f2display-default-colormap\fP (and are owned by the Xlib internals),
are ignored by the garbage collector.
.PP
Programmers must make sure that an object is accessible during the object's
entire lifetime, otherwise future runs of the garbage collector can
result in undesired termination of the object.
One must be especially careful when results of functions that create
new objects (such as \f2create-window\fP) are ignored or assigned
to local variables as in
.Ss
(define dpy (open-display))
(define root (display-root-window dpy))
.sp .5
(do ((x 0 (+ x 10)) (y 0 (+ y 10))) ((= x 50))
  (let ((win
          (create-window 'parent root 'x x 'y y 'width 20 'height 20)))
    (manage-window win)))
.Se
.PP
In this example, after termination of the do-loop, the garbage
collector will destroy the newly created windows, as they are not
accessible from within the program.
If this is not desired, the windows could be put into a variable (for
instance, be \f2consed\fP into a list) that is defined outside of the
body of the loop.
