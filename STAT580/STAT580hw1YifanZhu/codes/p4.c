#include <stdio.h>

int main()
{
	char ch1, ch2;
	int count = 0;

	ch1 = getchar();
	if (ch1 != EOF)
	{
		ch2 = ch1;
		ch2 = getchar();
		if (ch1 != ' ' && ch1 != '\t' && ch1 != '\n')
		count++;
	} else {
		printf("\nNumber of words: %d\n", count);
	}

	

	while (ch2 != EOF)
	{
		if ((ch1 == ' ' || ch1 == '\t' || ch1 == '\n') && (ch2 != ' ' && ch2 != '\t' && ch2 != '\n'))
			count++;
		ch1 = ch2;
		ch2 = getchar();
	}
	printf("\nNumber of words: %d\n", count);
	return 0;
}