#include	"mnemosyne.h"

static	char	rcsid[] = "$Header: /home/disk/atmos/CVSROOT/ive/mnemosyne/mtest.c,v 1.1.1.1 1993/04/07 19:41:30 harry Exp $";

/*
test harness/demo of mnemosyne library. deliberately leaks memory on the
floor, double frees, frees unallocated memory, etc.

	Marcus J. Ranum, 1990. (mjr@decuac.dec.com)
*/


main()
{
	char	*d = "foobar";
	char	*xx;
	int	x;

	xx = malloc(922);
	xx = malloc(123);

	/* huh ? */
/*	xx = malloc(-9);*/

	/* waste some memory */
	for(x = 1; x < 8; x++)
		xx = malloc(x);

	/* free something we don't own */
	free(d);

	/* double free something */
	free(xx);
	free(xx);

	/* not necessary - this triggers a better printout of statistics */
	mnem_writestats();
}
