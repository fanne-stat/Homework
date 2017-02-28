#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define N 16 /* number of observations */
#define P 2 /* number of predictors */

void dgesvd_(char *JOBU, char *JOBVT, int *m, int *n, double *A, int *LDA, double *S, double *U, int *LDU, double *VT, int *LDVT, double *WORK, int *LWORK, int *INFO);

int main()
{
    char jobu = 'S';
    char jobvt = 'A';
    int m = N;
    int n = P;
    double A[m * n];
    double s[n];
    double u[m * n];
    double vt[n * n];
    double *work;
    int lwork = -1;
    double lworkopt;
    int i, j, info;

    /* longley dataset from R */
    double X[N][P] =
    {
        {83, 107.608},
        {88.5, 108.632},
        {88.2, 109.773},
        {89.5, 110.929},
        {96.2, 112.075},
        {98.1, 113.27},
        {99, 115.094},
        {100, 116.219},
        {101.2, 117.388},
        {104.6, 118.734},
        {108.4, 120.445},
        {110.8, 121.95},
        {112.6, 123.366},
        {114.2, 125.368},
        {115.7, 127.852},
        {116.9, 130.081}
    };

    double Xbar[P];
    for (j = 0; j < P; j++)
    {
        Xbar[j] = 0;
        for (i = 0; i < N; i++)
        {
            Xbar[j] = Xbar[j] + X[i][j];
        }
        Xbar[j] = Xbar[j] / (double) N ;
    }

    for (i = 0; i < m; i++)
    {
        for (j = 0; j < n; j++)
        {
            A[j * m + i] = X[i][j] - Xbar[j];
        }
    }

    dgesvd_(&jobu, &jobvt, &m, &n, A, &m, s, u, &m, vt, &n, &lworkopt, &lwork, &info);

    if (info != 0)
    {
        printf("The dgesvd error %d\n", info);
    }
    else
    {
        lwork = (int) lworkopt;
        work = (double *) malloc(lwork * sizeof(double));
        assert(work != NULL);

        dgesvd_(&jobu, &jobvt, &m, &n, A, &m, s, u, &m, vt, &n, work, &lwork, &info);

        if (info != 0)
        {
            printf("The dgesvd error %d\n", info);
        }
        else
        {
            printf("The principal component scores:\n");
            for (i = 0; i < m; i++)
            {
                for (j = 0; j < n; j++)
                {
                    printf("%.6f\t", u[j * m + i] * s[j]);
                }
                printf("\n");
            }
        }
    }
    return 0;
}
