/* mkindex
 *
 * Copy named files or standard input (if no arguments are given) to
 * standard output, replacing @[something] by troff macro calls.
 *
 * These replacements are performed:
 *
 * @[.something]   -->  \n.Ix "something"\nsomething
 * @[!something]   -->  \n.Id "something"\nsomething
 *
 * @[.some|thing]  -->  \n.Ix "thing, some"\nsome thing
 * @[!some|thing]  -->  \n.Id "thing, some"\nsome thing
 *
 * @[.=something]  -->  \n.Ix "something"\n
 * @[!=something]  -->  \n.Id "something"\n
 *
 *
 *  1)  initial \n is omitted at the beginning of an output line
 *
 *  2)  initial \n is prefixed by \c if @[ follows "(" in input
 *
 *  3)  omit final \n if @[...] is at end of input line
 *
 *  4)  within @[...], \] is replaced by ]
 *
 *  5)  in the macro argument ("something"), all sequences of the form
 *      `` or '' or \fX or \% are removed
 */

#include <stdio.h>

char *index();

char buf[10000];
long line;
char *fn;

main(ac, av) char **av; {
    FILE *fp;

    if (ac < 2) {
        fn = "stdin";
        doit(stdin);
    } else {
        while (--ac > 0) {
            fn = *++av;
            if ((fp = fopen(fn, "r")) == 0) {
                perror(fn); exit(1);
            }
            doit(fp);
            fclose(fp);
        }
    }
    return 0;
}

doit(fp) FILE *fp; {
    char *p, *q, *start, *macro;
    char inx[1000], arg[1000];
    int n, need_nl = 0;

    line = 1;
    while (fgets(buf, 10000, fp) != NULL) {
        if (p = index(buf, '\n'))
            *p = 0;
        p = buf;
        while (*p) {
            if (*p == '@' && p[1] == '[') {
                start = p;
                p += 2;
                switch (*p) {
                case '.':
                    macro = "Ix"; break;
                case '!':
                    macro = "Id"; break;
                case 0:
                    error("index truncated");
                default:
                    error("invalid index type");
                }
                p++;
                q = inx;
                while (*p != ']') {
                    if (*p == 0)
                        error("missing ]");
                    if (*p == '\\' && p[1] == ']')
                        p++;
                    *q++ = *p++;
                }
                if (q == inx)
                    error("empty index");
                *q = 0;
                eatfont(inx, arg);
                if (start > buf && start[-1] == '(')
                        printf("\\c");
                if (need_nl)
                    putchar('\n');
                printf(".%s ", macro);
                p++;
                if (arg[0] == '=') {
                    printf("\"%s\"", arg+1);
                    if (*p) {
                        putchar('\n');
                        need_nl = 0;
                        if (*p == ' ')
                            p++;
                    }
                } else if (q = index(arg, '|')) {
                    *q = 0; q++;
                    printf("\"%s, %s\"\n%s %s", q, arg, arg, q);
                    need_nl = 1;
                } else {
                    printf("\"%s\"\n%s", arg, inx);
                    need_nl = 1;
                }
            } else {
                putchar(*p);
                need_nl = 1;
                p++;
            }
        }
        putchar('\n');
        need_nl = 0;
        line++;
    }
}

eatfont(from, to) char *from, *to; {
    while (*from) {
        if (*from == '\\' && from[1] == 'f' && from[2]) {
            from += 3;
        } else if (*from == '\'' && from[1] == '\'') {
            from += 2;
        } else if (*from == '`' && from[1] == '`') {
            from += 2;
        } else if (*from == '\\' && from[1] == '%') {
            from += 2;
        } else *to++ = *from++;
    }
    *to = 0;
}

error(s) char *s; {
    fprintf(stderr, "Error in %s line %d, %s:\n%s\n", fn, line, s, buf);
    exit(1);
}
