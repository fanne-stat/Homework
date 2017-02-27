#include <stdio.h>

#define N 16 /* number of observations */
#define P 2 /* number of predictors */



void dgels_(char *TRANS, int *M, int *N, int *NRHS, double *A, int *LDA, double *B, int *LDB, double *WORK, int *LWORK, int *INFO);

int main()
{
    /* longley dataset from R: Employed (Y) GNP.deflator and Population (X) */
    double Y[N] = {60.323, 61.122, 60.171, 61.187, 63.221, 63.639, 64.989,
                   63.761, 66.019, 67.857, 68.169, 66.513, 68.655, 69.564,
                   69.331, 70.551
                  };
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


    char trans = 'N';
    int m = N;
    int n = P;
    int nrhs = 1;
    int lwork = 2 * N * P;
    double work[lwork];
    int info;
    int i, j;

    double A[m * n];
    double B[m];

    for (i = 0; i < m; i++)
    {
        for (j = 0; j < n; j++)
        {
            A[j * m + i] = X[i][j];
        }
    }

    for (i = 0; i < m; i++)
    {
        B[i] = Y[i];
    }

    dgels_(&trans, &m, &n, &nrhs, A, &m, B, &m, work, &lwork, &info);

    if (info != 0)
    {
        printf("dgels error %d\n", info);
    }
    else
    {
        printf("The regression coefficirnts: ");
        for (i = 0; i < n; i++)
        {
            printf("%d\t", B[i]);
        }
        printf("\n");
    }


}

