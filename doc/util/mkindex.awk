#! /usr/bin/awk -f

# mkindex.awk: Copy named files or standard input (if no arguments are
# given) to standard output, replacing @[something] by troff macro calls.
# $Id$
#
# These replacements are performed:
#
# @[.something]   -->  \n.Ix "something"\nsomething
# @[!something]   -->  \n.Id "something"\nsomething
#
# @[.some|thing]  -->  \n.Ix "thing, some"\nsome thing
# @[!some|thing]  -->  \n.Id "thing, some"\nsome thing
#
# @[.=something]  -->  \n.Ix "something"\n
# @[!=something]  -->  \n.Id "something"\n
#
#
#  1)  initial \n is omitted at the beginning of an output line
#
#  2)  initial \n is prefixed by \c if @[ follows "(" in input
#
#  3)  omit final \n if @[...] is at end of input line
#
#  4)  within @[...], \] is replaced by ]
#
#  5)  in the macro argument ("something"), all sequences of the form
#      `` or '' or \fX or \% are removed

{
    need_nl = 0;
    do_line($0);
    printf "\n";
}

function do_line(line) {
    nxt = index(line, "@[")

    if(nxt == 0) {
        printf "%s", line;
        return;
    }

    if(nxt > 1) {
        if(substr(line, nxt - 1, 1) == "(") {
            need_c = 1;
        }
        need_nl = 1;
        printf "%s", substr(line, 1, nxt - 1);
        do_line(substr(line, nxt));
        return;
    }

    tmp = substr(line, 1, 3);
    if(tmp == "@[.") {
        macro = "Ix";
    } else
    if(tmp == "@[.") {
        macro = "Id";
    } else {
        printf "error: invalid index %s\n", tmp > "/dev/stderr";
        exit 1;
    }

    end = match(line, "[^\\\\]]");

    if(end == 0) {
        printf "error: unfinished @[\n" > "/dev/stderr";
        exit 1;
    }

    inx = substr(line, 4, end - 3);
    gsub("\\\\]", "]", inx);
    arg = inx;
    gsub("(\\\\f.|''|``|\\\\%)", "", arg);

    if(arg == "") {
        printf "error: empty index\n" > "/dev/stderr";
        exit 1;
    }

    if(need_c) { printf "\\c"; }
    if(need_nl) { printf "\n"; }

    printf ".%s ", macro;

    line = substr(line, end + 2);

    if(sub("^=", "", arg)) {
        printf "\"%s\"", arg;
        if(line != "") {
            printf "\n";
            need_nl = 0;
            sub("^ ", "", line);
        }
    } else if (arg ~ /[|]/) {
        q = arg; sub("[^|]*[|]", "", q); sub("[|].*", "", arg);
        printf "\"%s, %s\"\n%s %s", q, arg, arg, q;
        need_nl = 1;
    } else {
        printf "\"%s\"\n%s", arg, inx;
        need_nl = 1;
    }

    do_line(line);
}

