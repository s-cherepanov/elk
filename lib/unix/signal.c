/* signal.c
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

#include "unix.h"

static Object Sym_Exit, Sym_Default, Sym_Ignore;

static SYMDESCR Signal_Syms[] = {
    { "sigalrm", SIGALRM },
#ifdef SIGBUS
    { "sigbus", SIGBUS },
#endif
    { "sigfpe", SIGFPE },
    { "sighup", SIGHUP },
    { "sigill", SIGILL },
    { "sigint", SIGINT },
    { "sigkill", SIGKILL },
    { "sigpipe", SIGPIPE },
    { "sigquit", SIGQUIT },
    { "sigsegv", SIGSEGV },
    { "sigterm", SIGTERM },
#ifdef SIGABRT
    { "sigabrt", SIGABRT },
#endif
#ifdef SIGAIO
    { "sigaio", SIGAIO },
#endif
#ifdef SIGARRAYSIZE
    { "sigarraysize", SIGARRAYSIZE },
#endif
#ifdef SIGCHLD
    { "sigchld", SIGCHLD },
#endif
#ifdef SIGCLD
    { "sigcld", SIGCLD },
#endif
#ifdef SIGCONT
    { "sigcont", SIGCONT },
#endif
#ifdef SIGDANGER
    { "sigdanger", SIGDANGER },
#endif
#ifdef SIGDIL
    { "sigdil", SIGDIL },
#endif
#ifdef SIGEMT
    { "sigemt", SIGEMT },
#endif
#ifdef SIGGRANT
    { "siggrant", SIGGRANT },
#endif
#ifdef SIGINFO
    { "siginfo", SIGINFO },
#endif
#ifdef SIGIO
    { "sigio", SIGIO },
#endif
#ifdef SIGIOINT
    { "sigioint", SIGIOINT },
#endif
#ifdef SIGIOT
    { "sigiot", SIGIOT },
#endif
#ifdef SIGLOST
    { "siglost", SIGLOST },
#endif
#ifdef SIGLWP
    { "siglwp", SIGLWP },
#endif
#ifdef SIGMIGRATE
    { "sigmigrate", SIGMIGRATE },
#endif
#ifdef SIGMSG
    { "sigmsg", SIGMSG },
#endif
#ifdef SIGPOLL
    { "sigpoll", SIGPOLL },
#endif
#ifdef SIGPRE
    { "sigpre", SIGPRE },
#endif
#ifdef SIGPROF
    { "sigprof", SIGPROF },
#endif
#ifdef SIGPTY
    { "sigpty", SIGPTY },
#endif
#ifdef SIGPWR
    { "sigpwr", SIGPWR },
#endif
#ifdef SIGRESERVE
    { "sigreserve", SIGRESERVE },
#endif
#ifdef SIGRETRACT
    { "sigretract", SIGRETRACT },
#endif
#ifdef SIGSAK
    { "sigsak", SIGSAK },
#endif
#ifdef SIGSOUND
    { "sigsound", SIGSOUND },
#endif
#ifdef SIGSTOP
    { "sigstop", SIGSTOP },
#endif
#ifdef SIGSYS
    { "sigsys", SIGSYS },
#endif
#ifdef SIGTRAP
    { "sigtrap", SIGTRAP },
#endif
#ifdef SIGTSTP
    { "sigtstp", SIGTSTP },
#endif
#ifdef SIGTTIN
    { "sigttin", SIGTTIN },
#endif
#ifdef SIGTTOU
    { "sigttou", SIGTTOU },
#endif
#ifdef SIGURG
    { "sigurg", SIGURG },
#endif
#ifdef SIGUSR1
    { "sigusr1", SIGUSR1 },
#endif
#ifdef SIGUSR2
    { "sigusr2", SIGUSR2 },
#endif
#ifdef SIGVIRT
    { "sigvirt", SIGVIRT },
#endif
#ifdef SIGVTALRM
    { "sigvtalrm", SIGVTALRM },
#endif
#ifdef SIGWAITING
    { "sigwaiting", SIGWAITING },
#endif
#ifdef SIGWINCH
    { "sigwinch", SIGWINCH },
#endif
#ifdef SIGWINDOW
    { "sigwindow", SIGWINDOW },
#endif
#ifdef SIGXCPU
    { "sigxcpu", SIGXCPU },
#endif
#ifdef SIGXFSZ
    { "sigxfsz", SIGXFSZ },
#endif
    { 0, 0 }
};

static Object P_Kill(Object pid, Object sig) {
    int t, s;

    if ((t = TYPE(sig)) == T_Fixnum || t == T_Bignum)
        s = Get_Integer(sig);
    else if (t == T_Symbol)
        s = Symbols_To_Bits(sig, 0, Signal_Syms);
    else
        Wrong_Type_Combination(sig, "symbol or integer");
    if (kill(Get_Integer(pid), s) == -1)
        Raise_System_Error("~E");
    return Void;
}

static Object P_List_Signals() {
    return Syms_To_List(Signal_Syms);
}

static Object P_Pause() {
    pause();
    Fatal_Error("pause() returned unexpectedly");
}

#if defined(HAVE_SIGPROCMASK) || defined(HAVE_SIGBLOCK)

static Object Handlers;

static Object P_Alarm(Object s) {
    return Make_Unsigned(alarm(Get_Unsigned(s)));
}

/*ARGSUSED*/
void General_Handler(int sig) {
    Object fun, args;

#if defined(HAVE_SIGPROCMASK)
    (void)signal(sig, General_Handler);
#endif
    Set_Error_Tag("signal-handler");
    Reset_IO(1);
    args = Bits_To_Symbols((unsigned long)sig, 0, Signal_Syms);
    args = Cons(args, Null);
    fun = VECTOR(Handlers)->data[sig];
    if (TYPE(fun) != T_Compound)
        Fatal_Error("no handler for signal %d", sig);
    (void)Funcall(fun, args, 0);
    Format(Curr_Output_Port, "~%\7Signal!~%", 15, 0, (Object *)0);
    (void)P_Reset();
    /*NOTREACHED*/
}

static Object Action_To_Sym(void (*act)()) {
    char *sym;

    if (act == Signal_Exit)
        sym = "exit";
    else if (act == SIG_IGN)
        sym = "ignore";
    else if (act == SIG_DFL || act == (void (*)())-1)
        sym = "default";
    else
        sym = "handler";
    return Intern(sym);
}

void Add_To_Mask(int sig) {
#ifdef HAVE_SIGPROCMASK
    sigaddset(&Sigset_Block, sig);
#else
    Sigmask_Block |= sigmask(sig);
#endif
    if (Intr_Level > 0)  /* make sure new signal gets blocked */
        Force_Disable_Interrupts;
}

void Remove_From_Mask(int sig) {
#ifdef HAVE_SIGPROCMASK
    sigdelset(&Sigset_Block, sig);
#else
    Sigmask_Block &= ~sigmask(sig);
#endif
}

static Object P_Signal(int argc, Object *argv) {
    int sig;
    Object handler, old;
    void (*disp)();

    sig = Symbols_To_Bits(argv[0], 0, Signal_Syms);
    if (sig >= NSIG)
        Fatal_Error("signal %d >= NSIG", sig);
    if (argc == 1) {
        handler = VECTOR(Handlers)->data[sig];
        if (Truep(handler))
            return handler;
        if ((disp = signal(sig, SIG_DFL)) != SIG_DFL)
            (void)signal(sig, disp);
        return Action_To_Sym(disp);
    }
    switch (sig) {
#ifdef SIGBUS
    case SIGBUS:
#endif
    case SIGFPE:
    case SIGILL:
    case SIGINT:
    case SIGKILL:
    case SIGSEGV:
#ifdef SIGABRT
    case SIGABRT:
#endif
        Primitive_Error("changing signal ~s not permitted", argv[0]);
    }
    handler = argv[1];
    if (EQ(handler, Sym_Exit)) {
        disp = Signal_Exit;
    } else if (EQ(handler, Sym_Default)) {
        disp = SIG_DFL;
    } else if (EQ(handler, Sym_Ignore)) {
        disp = SIG_IGN;
    } else if (TYPE(handler) == T_Compound) {
        if (COMPOUND(handler)->min_args > 1 ||
                COMPOUND(handler)->max_args == 0)
            Primitive_Error("handler expects wrong number of args");
        disp = General_Handler;
    } else
        Primitive_Error("invalid handler: ~s", handler);
    old = VECTOR(Handlers)->data[sig];
    VECTOR(Handlers)->data[sig] = (disp == General_Handler) ? handler : False;
    if (disp == General_Handler)
        Add_To_Mask(sig);
    else
        Remove_From_Mask(sig);
    if ((disp = signal(sig, disp)) == (void (*)())-1)
        Raise_System_Error("~E");
    return Truep(old) ? old : Action_To_Sym(disp);
}
#endif /* defined(HAVE_SIGPROCMASK) || defined(HAVE_SIGBLOCK) */

void elk_init_unix_signal() {
    Define_Symbol(&Sym_Exit, "exit");
    Define_Symbol(&Sym_Default, "default");
    Define_Symbol(&Sym_Ignore, "ignore");
    Def_Prim(P_Kill,                "unix-kill",                2, 2, EVAL);
    Def_Prim(P_List_Signals,        "unix-list-signals",        0, 0, EVAL);
    Def_Prim(P_Pause,               "unix-pause",               0, 0, EVAL);
#if defined(HAVE_SIGPROCMASK) || defined(HAVE_SIGBLOCK)
    Def_Prim(P_Alarm,               "unix-alarm",               1, 1, EVAL);
    Handlers = Make_Vector(NSIG, False);
    Global_GC_Link(Handlers);
    Def_Prim(P_Signal,              "unix-signal",              1, 2, VARARGS);
    P_Provide(Intern("unix:reliable-signals"));
#endif
}
