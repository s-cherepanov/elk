extern int Intr_Was_Ignored;
extern unsigned long Intr_Level;

#ifdef POSIX_SIGNALS
    extern sigset_t Sigset_Old, Sigset_Block;
#else
#ifdef BSD_SIGNALS
    extern int Sigmask_Old, Sigmask_Block;
#else
    C_LINKAGE_BEGIN
    extern void Intr_Handler P_((int));
    C_LINKAGE_END
#endif
#endif

#ifdef BSD_SIGNALS
#  ifndef sigmask
#    define sigmask(n)  (1 << ((n)-1))
#  endif
#endif

#define Disable_Interrupts {\
    if (Intr_Level++ == 0) Force_Disable_Interrupts;\
}

#define Enable_Interrupts {\
    if (Intr_Level > 0 && --Intr_Level == 0) Force_Enable_Interrupts;\
}

#ifdef BSD_SIGNALS
#define Force_Disable_Interrupts  (void)sigblock (Sigmask_Block)
#define Force_Enable_Interrupts   (void)sigsetmask (Sigmask_Old)
#else
#ifdef POSIX_SIGNALS
#define Force_Disable_Interrupts  (void)sigprocmask (SIG_BLOCK, &Sigset_Block,\
				      (sigset_t *)0)
#define Force_Enable_Interrupts   (void)sigprocmask (SIG_SETMASK, &Sigset_Old,\
				      (sigset_t *)0)
#else
#define Force_Disable_Interrupts  {\
    if (!Intr_Was_Ignored) (void)signal (SIGINT, SIG_IGN);\
}
#define Force_Enable_Interrupts   {\
    if (!Intr_Was_Ignored) (void)signal (SIGINT, Intr_Handler);\
}
#endif
#endif
