/* A trivial function to enable and disable execution profiling.
 *
 * Evaluate "(monitor #t)" to enable profiling; "(monitor #f)" to
 * disable profiling and create a mon.out (this is done automatically
 * on exit by means of an extension finalization function).
 *
 *
 * This extension may not work on some platforms.
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

static monitoring;

static Object P_Monitor (on) Object on; {
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

elk_init_lib_monitor () {
    Define_Primitive (P_Monitor, "monitor", 1, 1, EVAL);
}

elk_finit_lib_monitor () {
    if (monitoring) {
	monitoring = 0;
	printf ("[writing mon.out]\n");
	monitor (0);
    }
}
