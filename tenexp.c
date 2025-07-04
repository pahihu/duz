#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define B   64
#define Q   (B / 2)

void approximate(int i, double e, int eb, double b)
{
	int fracs[5];
	int inc, j;
    double f, ipart, fpart;

    f = e / b;
    fprintf(stderr, "10^%3d ~ %18.15lf * B^%3d - (%02d,", i, f, eb, eb + Q);
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
    for (j = 0; j < 4; j++) {
        fprintf(stderr, " %02d", fracs[j]);
    }
    fprintf(stderr, " [%02d]", fracs[4]);
    fprintf(stderr, ")\n");	
}

int main(int argc, char*argv[])
{
    int i;
    double e, b;
    int eb;

    e = 10; b = 1.0; eb = 0;
    for (i = 1; 1; i++) {
        if (e > B*b) {
            b *= B; eb++;
            if (eb >= Q)
                break;
        }
        approximate(i, e, eb, b);
        e *= 10.0;
    }
    e = 0.1; b = 1.0/64.0; eb = -1;
    for (i = -1;  1; i--) {
        if (e < b) {
            b /= B; eb--;
            if (eb < -Q)
                break;
        }
        approximate(i, e, eb, b);
        e /= 10.0;
    }
    return 0;
}

/* vim:set ts=4 sw=4 et: */
