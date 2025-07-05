#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#define B   64
#define Q   (B / 2)

double m2d(int e, int *fracs)
{
    double d;
    int i;
    int ee;

    d = 0.0;
    for (i = 3; i >= 0; i--) {
        d += fracs[i];
        d /= (double)B;
    }
    ee = e - Q;
    if (ee < 0) {
        for (i = 0; i < -ee; i++) {
            d /= B;
        }
    } else {
        for (i = 0; i < ee; i++) {
            d *= B;
        }
    }
    return d;
}

void approximate(double f, int *fracs)
{
    int j;
    double fpart, ipart;
    int inc;

    for (j = 0; j < 5; j++) {
        fpart = modf(f, &ipart);
        fracs[j] = (int)ipart;
        f = B * (f - ipart);    
    }
    /* round */
    inc = 0;
    if (fracs[4] > Q) {
        inc = 1;
    } else if (Q == fracs[4]) {
        if (0 == ((fracs[3] + Q) & 1)) {
            inc = 1;
        }
    }
    for (j = 3; inc && (j >= 0); j--) {
        fracs[j] += inc;
        if (fracs[j] >= B) {
            fracs[j] %= B;
            inc = 1;
        } else {
            inc = 0;
        }
    }
}

int d2m(double d, int *fracs)
{
    int e, eq;
    int sign;

    sign = d < 0.0;
    d = fabs(d);

    e = 0;
    while (d < 1.0) {
        d *= B; e--;
    }
    while (d >= B) {
        d /= B; e++;
    }
    approximate(d, fracs); e++;
    eq = e + Q;
    return eq;
}

void prinfracs(int e, int *fracs)
{
    int j;

    fprintf(stderr, "(%2d,", e);
    for (j = 0; j < 4; j++) {
        fprintf(stderr, " %02d", fracs[j]);
    }
    fprintf(stderr, " [%02d]) ", fracs[4]);
}

void cvtFloat2Mix(int i, double e, int eb, double b)
{
	int eq, fracs[5];
    double f, d;

    eb++;

    f = e / b;
    fprintf(stderr, "10^%3d ~ %18.15lf * B^%3d - ", i, f, eb);
    approximate(f, fracs);
    prinfracs(eb + Q, fracs);
    d = m2d(eb + Q, fracs);
    fprintf(stderr, "%.3le - ", fabs(e - d));

    eq = d2m(f, fracs);
    prinfracs(eq, fracs);
    fprintf(stderr, "\n");
}

void tenExp(void)
{
    double e, b;
    int ee, eb;

    fprintf(stderr, "=== 10^E ====================================================| E R R |\n");
    ee = 0; e = 1.0; b = 1.0; eb = 0;
    while(1) {
        if (e > B*b) {
            b *= B; eb++;
            if (eb >= Q-1)
                break;
        }
        cvtFloat2Mix(ee, e, eb, b);
        e *= 10.0; ee++;
    }
    fprintf(stderr, "\n");
    ee = -1; e = 0.1; b = 1.0/64.0; eb = -1;
    while (1) {
        if (e < b) {
            b /= B; eb--;
            if (eb < -(Q+1))
                break;
        }
        cvtFloat2Mix(ee, e, eb, b);
        e /= 10.0; ee--;
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "\n");
}

void cvtMixToFloat(int eb, double b)
{
    char buf[64];
    char *ptr;
    double mpart;
    int epart;
    int fracs[5];
    double d;

    sprintf(buf, "%.15e", b);
    ptr = strchr(buf, 'e');
    *ptr = 0;
    sscanf(buf, "%lf", &mpart);
    sscanf(ptr+1, "%d", &epart);
    fprintf(stderr, "B^%3d %.15e %.15lf %3d - ", eb, b, mpart, epart);
    approximate(mpart, fracs);
    prinfracs(1 + Q, fracs);
    d = m2d(1 + Q, fracs);
    fprintf(stderr, " %.3e", fabs(mpart - d));
    fprintf(stderr, "\n");
}

void bExp(void)
{
    int i;
    double b;
    int eb;

    fprintf(stderr, "=== B^E ====================================================================| E R R |\n");;

    eb = 0; b = 1.0;
    for (i = 0; i < Q; i++) {
        cvtMixToFloat(eb, b);
        b *= B;
        eb++;
    }
    fprintf(stderr, "\n");
    eb = -1; b = 1.0/B;
    for (i = 1; i <= Q; i++) {
        cvtMixToFloat(eb, b);
        b /= B;
        eb--;
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "\n");
}


int main(int argc, char *argv[])
{
    int fracs[5], eq;
    double d = 7.62942363516372e-06;

    tenExp();
    bExp();
    eq = d2m(d, fracs);
    prinfracs(eq, fracs);
    return 0;
}

/* vim:set ts=4 sw=4 et: */
