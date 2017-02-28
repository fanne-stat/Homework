#include <stdio.h>

#define N 10

int main(){
	double x[N] = {3.1, -1.2, 5.3, 1, 4.4, 21, 3, 7, -1.2, 3.2};
	int i, j;
	double temp;
	for (i = 1; i < N; i++){
		j = i;
		while (j > 0 && x[j-1] > x[j]){
			temp = x[j-1];
			x[j-1] = x[j];
			x[j] = temp;
			j--;
		}
	}
	printf("Sorted data:\n");
	for (i=0; i < N; i++){
		printf("%f ", x[i]);
	}

	printf("\n Median:\n");
	if (N%2 == 0){
		printf("%f\n", (x[N/2 -1] + x[N/2])/2.0);
	} else {
		printf("%f\n", x[N/2]);
	}
	return 0;
}