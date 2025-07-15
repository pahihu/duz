BEGIN \
{
    readek = 1;
    ops = "+-*/?";
}

/###/ \
{
    readek = 0;
    next;
}

function wtoi(s ,n)
{
    n = index("~JKLMNOPQR", substr(s, 10, 1));
    if (0 == n) {
        return s + 0;
    }
    return -1 * (10 * (substr(s, 1, 9) + 0) + n - 1);
}

{
    if (readek) {
        lines[NR] = $0;
        next;
    }
    inp = lines[$1 + 0];
    out = $0;

    print inp;
    print out;

    u = substr(inp, 10+1, 10);
    v = substr(inp, 20+1, 10);
    for (i = 0; i < 5; i++) {
        w = substr(inp, 30+1 + i * 10, 10);
        result = substr(out, 30+1 + i * 10, 10);
        if (w != result) {
            n = wtoi(result);
            sign = "+";
            if (n < 0) {
                sign = "-"; n = -n;
            }
            printf "-m 0 %s %s %s / %s%o\n", u, v, substr(ops, i+1, 1), sign, n;
        }
    }
    print "";
}
