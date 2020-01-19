#include <stdio.h>
#include <stdlib.h>

#include "sys.h"

int main()
{
	_pledge();
	getchar();
	exit(0);
}
