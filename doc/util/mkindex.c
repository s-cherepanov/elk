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

main(int ac, char **av) {
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
	    /* Traite tous les fichiers un par un */
            doit(fp);
            fclose(fp);
        }
    }
    return 0;
}

doit(FILE *fp) {
    char *p, *q, *start, *macro;
    char inx[1000], arg[1000];
    int n, need_nl = 0;

    line = 1;
    /* Proceed line by line */
    while (fgets(buf, 10000, fp) != NULL) {
        if (p = index(buf, '\n'))
            *p = 0;
        p = buf;
	/* Proceed char by char */
        while (*p) {
	    /* On cherche @[ , sinon on affiche le nouveau caractère */
            if (*p != '@' || p[1] != '[') {
                putchar(*p);
                need_nl = 1;
                p++;
                start = p;
            } else {
                p += 2;
                switch (*p) {
		/* @[. -> Ix */
                case '.':
                    macro = "Ix"; break;
		/* @[! -> Ix */
                case '!':
                    macro = "Id"; break;
                case 0:
                    error("index truncated");
                default:
                    error("invalid index type");
                }
                p++;
                q = inx;
		/* On cherche ] */
                while (*p != ']') {
                    if (*p == 0)
                        error("missing ]");
		    /* Si ] est escapé, on l'imprime */
                    if (*p == '\\' && p[1] == ']')
                        p++;
                    *q++ = *p++;
                }
		/* inx devient ce qu'il y avait entre [ ] */
                if (q == inx)
                    error("empty index");
                *q = 0;
		/* Vire \f. '' `` \% de l'index et on le met dans arg */
                eatfont(inx, arg);
		/* Si le précédent caractère est un "(", on affiche "\\c" */
                if (start > buf && start[-1] == '(')
                        printf("\\c");
		/* Si need_nl == 1, on saute une ligne */
                if (need_nl)
                    putchar('\n');
		/* on affiche .Ix ou .Id */
                printf(".%s ", macro);
		/* On bouffe le caractère suivant de l'input */
                p++;
		/* Si arg commence par '=', on l'affiche entre "" */
                if (arg[0] == '=') {
                    printf("\"%s\"", arg+1);
		    /* S'il reste du data, on saute une ligne et si c'est
			une espace, on la saute sans l'afficher ; par
			ailleurs au prochain coup ça sera pas la peine de
			sauter une ligne */
                    if (*p) {
                        putchar('\n');
                        need_nl = 0;
                        if (*p == ' ')
                            p++;
                    }
		/* Si arg est du genre "foo|bar", on affiche
		    "bar, foo"\nfoo bar */
                } else if (q = index(arg, '|')) {
                    *q = 0; q++;
                    printf("\"%s, %s\"\n%s %s", q, arg, arg, q);
                    need_nl = 1;
                } else {
		/* Sinon on affiche "arg"\ninx */
                    printf("\"%s\"\n%s", arg, inx);
                    need_nl = 1;
                }
            }
        }
	/* Si on a fini de parser, on saute une ligne */
        putchar('\n');
        need_nl = 0;
        line++;
    }
}

eatfont(char *from, char *to) {
    while (*from) {
	/* "\f." -> "" */
        if (*from == '\\' && from[1] == 'f' && from[2]) {
            from += 3;
	/* "''" -> "" */
        } else if (*from == '\'' && from[1] == '\'') {
            from += 2;
	/* "``" -> "" */
        } else if (*from == '`' && from[1] == '`') {
            from += 2;
	/* "\%" -> "" */
        } else if (*from == '\\' && from[1] == '%') {
            from += 2;
        } else *to++ = *from++;
    }
    *to = 0;
}

error(char *s) {
    fprintf(stderr, "Error in %s line %d, %s:\n%s\n", fn, line, s, buf);
    exit(1);
}
