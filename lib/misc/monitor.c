/* monitor.c: A trivial function to enable and disable execution profiling.
 *
 * Evaluate "(monitor #t)" to enable profiling; "(monitor #f)" to
 * disable profiling and create a mon.out (this is done automatically
 * on exit by means of an extension finalization function).
 *
 * $Id$
 *
 * Copyright 1990, 1991, 1992, 1993, 1994, 1995, Oliver Laumann, Berlin
 * Copyright 2002, 2003 Sam Hocevar <sam@zoy.org>, Paris
 *
 * This software was derived from Elk 1.2, which was Copyright 1987, 1988,
 * 1989, Nixdorf Computer AG and TELES GmbH, Berlin (Elk 1.2 has been written
 * by Oliver Laumann for TELES Telematic Services, Berlin, in a joint project
 * between TELES and Nixdorf Microprocessor Engineering, Berlin).
 *
 * Oliver Laumann, TELES GmbH, Nixdorf Computer AG and Sam Hocevar, as co-
 * owners or individual owners of copyright in this software, grant to any
 * person or company a worldwide, royalty free, license to
 *
 *    i) copy this software,
 *   ii) prepare derivative works based on this software,
 *  iii) distribute copies of this software or derivative works,
 *   iv) perform this software, or
 *    v) display this software,
 *
 * provided that this notice is not removed and that neither Oliver Laumann
 * nor Teles nor Nixdorf are deemed to have made any representations as to
 * the suitability of this software for any purpose nor are held responsible
 * for any defects of this software.
 *
 * THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

/* This extension may not work on some platforms.
 *
 * On DECstations running Ultrix, you may have to evaluate
 *    (set! load-libraries "/usr/lib/cmplrs/cc/libprof1_G0.a -lc_G0")
 * before loading monitor.o.
 *
 * On older versions of BSD and SunOS you might have to
 *    ar x /lib/libc.a mon.o
 * and replace in the symbol table of mon.o the symbols mcount and
 * _moncontrol by something else (e.g. Mcount and _Mcontrol); then
 *    ld -r mon.o monitor.o; mv a.out monitor.o; rm mon.o
 * Or you might have to
 *     cp /lib/mcrt0.o mon.o,
 * then, in mon.o, replace start by Start and _environ by _Environ
 * and call the linker as shown above.  You can do the symbol table
 * hacking by editing mon.o with emacs.
 */


#include "scheme.h"

#include <sys/types.h>

#define MONSTART 2

static int monitoring;

static Object P_Monitor (Object on) {
    char *brk;
    extern char *sbrk();

    Check_Type (on, T_Boolean);
    Disable_Interrupts;
    if (Truep (on)) {
	if (!monitoring) {
	    brk = sbrk (0);
	    monstartup ((int (*)())MONSTART, (int (*)())brk);
	    monitoring = 1;
	}
    } else {
	monitor (0);
	monitoring = 0;
    }
    Enable_Interrupts;
    return Void;
}

void elk_init_lib_monitor () {
    Define_Primitive (P_Monitor, "monitor", 1, 1, EVAL);
}

void elk_finit_lib_monitor () {
    if (monitoring) {
	monitoring = 0;
	printf ("[writing mon.out]\n");
	monitor (0);
    }
}
