#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP Gibbs(SEXP Ra, SEXP Rb, SEXP Rx){
	int a = asReal(Ra), b = asReal(Rb), n = length(Rx), i, j;
	int *x = INTEGER(Rx);
	int r[n];

	SEXP lambdas, ps;
	PROTECT(lambdas = allocVector(REALSXP, 4000));
	PROTECT(ps = allocVector(REALSXP, 4000));
	double *lambda_chain = REAL(lambdas);
	double *p_chain = REAL(ps);



	// Initiate lambda and p

	double lambda = 1, p = 0.5;

	double Sx = 0, Sr;

	for (i = 0; i < n; i++)
		Sx = Sx + x[i];

	GetRNGstate();
	// burn-in
	for (i = 0; i < 1000; i++){
		Sr = 0;
		for (j = 0; j < n; j++){
			if (x[j] == 0){
				r[j] = rbinom(1, p * exp(-lambda))/(p*exp(-lambda) + (1 - p));
			}
			else 
				r[j] = 1;
			Sr = Sr +r[j];
		}


		lambda = rgamma(a + Sx, 1/(b + Sr));

		p = rbeta(1 + Sr, n + 1 - Sr);
	}


	// take sample from p's and lambda's for every step
	for (i = 0; i < 4000; i++){
		Sr = 0;
		for (j = 0; j < n; j++){
			if (x[j] == 0){
				r[j] = rbinom(1, p * exp(-lambda))/(p*exp(-lambda) + (1 - p));
			}
			else 
				r[j] = 1;
			Sr = Sr +r[j];
		}

		lambda = rgamma(a + Sx, 1/(b + Sr));

		lambda_chain[i] = lambda;

		p = rbeta(1 + Sr, n + 1 - Sr);

		p_chain[i] = p;
	}

	PutRNGstate();

	SEXP samples;
	PROTECT(samples = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(samples, 0, lambdas);
	SET_VECTOR_ELT(samples, 1, ps);

	UNPROTECT(3);
	return samples;
}