#include <stdio.h>

int main()
{
	char ch;

	ch = getchar();
	while (ch != EOF)
	{
		if (ch >= 'a' && ch <= 'x')
		{
			putchar(ch + 2);
		} else {
			putchar(ch);
		}
		ch = getchar();
	}
	return 0;
}