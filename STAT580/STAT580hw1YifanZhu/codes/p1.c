#include <stdio.h>
#define lower 0
#define upper 200
#define rows 12

int main()
{
	double f, c, step;
	int i;

	f = lower;
	step = (upper - lower) / (double) (rows - 1);
	printf("F\tC\n");

	for (i = 1; i <= rows; i++)
	{
		c = 5.0 * (f - 32.0) / 9.0;
		printf("%.1f\t%.1f\n", f,c);
		f = f + step ;
	}
	return 0;
}