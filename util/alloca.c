/* Check if the system's alloca() function actually extends the stack.
 * If it doesn't, it's not usable for Elk.
 *
 * The second value printed should be about 100 larger (or smaller,
 * depending on the stack growing direction) than the first value.
 *
 * On some systems you may have to enable the #include and delete the line
 * declaring alloca().
 */

/* #include <alloca.h> */

extern char *alloca();

char *stkbase;

prstk(s) char *s; {
    char foo;

    printf("stack %s calling alloca(100): %lu\n", s, (long)(stkbase - &foo));
}
    
main(ac, av) char **av; {
    char *foo;

    stkbase = (char *)&foo;
    prstk("before");
    foo = alloca(100);
    prstk(" after");
    return 0;
}
