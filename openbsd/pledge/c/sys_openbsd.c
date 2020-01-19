#include "sys.h"

#include <unistd.h>
#include <err.h>

void
_pledge()
{
	if (pledge("stdio", NULL) != 0) {
		err(1, "pledge");
	}
}
