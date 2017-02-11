#include <stdio.h>
#include <time.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>

int main()
{
	double u, x;

	set_seed(time(NULL), 580580); /* set seed */

	u = unif_rand();
	x = pow(10.0, u);
	printf("%f\n", x);

	return 0;
}