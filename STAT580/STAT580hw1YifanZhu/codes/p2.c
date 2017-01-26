#include <stdio.h>

int main()
{
	char ch;

	ch = getchar();
	while (ch != EOF)
	{
		if (ch >= 48 && ch <= 57)
		{
			putchar(ch);
		}
		ch = getchar();
	}
	return 0;
}