#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define B   64
#define Q   (B / 2)

int main(int argc, char*argv[])
{
    int i, j;
    double f, e, b;
    double ipart, fpart;
    int eb;
    int fracs[5], inc;

    e = 10; b = 1.0; eb = 0;
    for (i = 1; 1; i++) {
        if (e > B*b) {
            b *= B; eb++;
            if (eb >= Q)
                break;
        }
        f = e / b;
        fprintf(stderr, "10^%3d ~ %18.15lf * B^%3d - (%02d,", i, f, eb, eb + Q);
        for (j = 0; j < 5; j++) {
            fpart = modf(f, &ipart);
            fracs[j] = (int)ipart;
            f = B * (f - ipart);    
        }
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
        for (j = 0; j < 5; j++) {
            fprintf(stderr, " %02d", fracs[j]);
        }
        fprintf(stderr, ")\n");
        e *= 10.0;
    }
    e = 0.1; b = 1.0/64.0; eb = -1;
    for (i = -1;  1; i--) {
        if (e < b/B) {
            b /= B; eb--;
            if (eb < -Q)
                break;
        }
        f = e / b;
        fprintf(stderr, "10^%3d ~ %18.15lf * B^%3d - (%02d,", i, f, eb, eb + Q);
        for (j = 0; j < 5; j++) {
            fpart = modf(f, &ipart);
            fprintf(stderr, " %02d", (int)ipart);
            f = B * (f - ipart);    
        }
        fprintf(stderr, ")\n");
        e /= 10.0;
    }
    return 0;
}

/* vim:set ts=4 sw=4 et: */
