#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void dgesv_(int *n, int *nrhs, double *a, int *lda, int *ipiv, double *b, int *ldb, int *info);

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        printf("This program performs a linear regression and returns the regression coefficients.\n");
        printf("Arguments: data intercept\n");
        printf("           data: data file\n");
        printf("           intercept: 1 = intercept, 0 = no intercept\n");
        return 1;
    }

    FILE *f;
    int N = 0, P = 0;
    int i, j, k;
    char cursor;

    f = fopen(argv[1], "r");
    while (((cursor = fgetc(f)) != EOF) && (cursor != '\n'))
    {
        if (cursor == ' ') P++;
    }
    rewind(f);
    while ((cursor = fgetc(f)) != EOF)
    {
        if (cursor == '\n') N++;
    }
    rewind(f);

    printf("Sample size and number of predictors are %d and %d respectively.\n", N, P);

    double data[N * (P + 1)];

    for (i = 0; i < N * (P + 1); i++)
    {
        fscanf(f, "%lf", &data[i]);
    }

    fclose(f);
  
    double Y[N];
    for (i = 0; i < N; i++)
    {
        Y[i] = data[i * (P + 1)];
    }

    if (atoi(argv[2]) == 1)
    {
        double X[N][(P + 1)];
        int n1 = P + 1, n2 = 1, ipiv[P + 1], info;
        double XtX[n1 * n1];
        double XtY[n1];
        for (i = 0; i < N; i++)
        {
            X[i][0] = 1;
            for (j = 1; j < n1; j++)
            {
                X[i][j] = data[i * (P + 1) + j];
            }
        }

        for (i = 0; i < n1; i++)
        {
            for (j = 0; j < n1; j++)
            {
                XtX[i * n1 + j] = 0;
                for (k = 0; k < N; k++)
                    XtX[i * n1 + j] += X[k][i] * X[k][j];
            }
        }

        for (i = 0; i < n1; i++)
        {
            XtY[i] = 0;
            for (j = 0; j < N; j++)
            {
                XtY[i] += X[j][i] * Y[j];
            }
        }

        /* XtX is symmetric, no transpose needed before passing to Fortran subrountine */
        dgesv_(&n1, &n2, XtX, &n1, ipiv, XtY, &n1, &info);
        if (info != 0)  printf("failure with error %d\n", info);

        /* print beta */
        printf("The regression coefficients: ");
        for (i = 0; i < n1; i++)
        {
            printf("%f ", XtY[i]);
        }
        printf("\n");




    }
    else if (atoi(argv[2]) == 0)
    {
        double X[N][P];
        int n1 = P, n2 = 1, ipiv[P], info;
        double XtX[n1 * n1];
        double XtY[n1];
        for (i = 0; i < N; i++)
        {
            for (j = 0; j < n1; j++)
            {
                X[i][j] = data[i * (P + 1)+ j+1];
            }
        }

        for (i = 0; i < n1; i++)
        {
            for (j = 0; j < n1; j++)
            {
                XtX[i * n1 + j] = 0;
                for (k = 0; k < N; k++)
                    XtX[i * n1 + j] += X[k][i] * X[k][j];
            }
        }

        for (i = 0; i < n1; i++)
        {
            XtY[i] = 0;
            for (j = 0; j < N; j++)
            {
                XtY[i] += X[j][i] * Y[j];
            }
        }

        /* XtX is symmetric, no transpose needed before passing to Fortran subrountine */
        dgesv_(&n1, &n2, XtX, &n1, ipiv, XtY, &n1, &info);
        if (info != 0)  printf("failure with error %d\n", info);

        /* print beta */
        printf("The regression coefficients: ");
        for (i = 0; i < n1; i++)
        {
            printf("%f ", XtY[i]);
        }
        printf("\n");
    }


    return 0;

}
