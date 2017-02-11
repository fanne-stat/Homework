#include <stdio.h>
#include <math.h>

#define P0 0.01 /*lower limit of the probability (p) */
#define P1 0.5 /* upper limit of the probability (p) */
#define PLEN 10 /* number of columns */
#define N 5 /* number of experiments (n) */

int factorial(int n)
{
    if (n <= 1)
        return 1;
    else
        return (n * factorial(n - 1));
}

double binopmf(int n, int x, double p)
{
    return (factorial(n) / (factorial(n - x) * factorial(x))) * pow(p, x) * pow(1 - p, n - x);
}

int main()
{
    printf("x\\p\t");
    double step = (P1 - P0) / (double) (PLEN - 1);
    for (int i = 1; i <= PLEN - 1; i++)
    {
        printf("%.4f\t", P0 + (i - 1)*step);
    }
    printf("%.4f\n", P0 + (PLEN - 1)*step);

    for (int x = 0; x <= N; x++)
    {
        printf("%d\t", x);
        for (int i = 1; i <= PLEN - 1; i++)
        {
            printf("%.4f\t", binopmf(N, x, P0 + (i - 1)*step));
        }
        printf("%.4f\n", binopmf(N, x, P0 + (PLEN - 1)*step));
    }
    return 0;
}