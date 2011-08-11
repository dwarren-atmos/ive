/*
 * $Id: save_state.c,v 1.3 1997/04/15 17:24:41 harry Exp $
 *
 * This Fortran callable routine saves the state of the specified plot
 * in the "saved_state" variable for later use by restore_state in
 * the looping process.
 *
 * call save_state(plot, num_overlay)
 *
 * where "plot" is the plot to save, "num_overlay" is the number of
 * overlays in the plot.
 *
 * $Log: save_state.c,v $
 * Revision 1.3  1997/04/15 17:24:41  harry
 * Remove time_tag - use multiple files instead.
 * Various fixes to vector/scalar looping.
 *
 * Revision 1.2  1996/10/09 18:54:45  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.1  1993/06/18  17:19:53  harry
 * Make long loops work by adding save_state.c and changing loop.f and
 * restore_state.c.
 *
 *
 */

static char ident[] = "$Id: save_state.c,v 1.3 1997/04/15 17:24:41 harry Exp $";

#include <string.h>
#include <strings.h>
#include <malloc.h>
#include <stdio.h>
#include <state.h>

char *saved_state[NVCMND][MAX_OVERLAY];

save_state_(plot, num_overlay)
int *plot;
int *num_overlay;
{
    int i, j;

    for (j=0; j < *num_overlay; ++j) {
	for (i=0; i<NVCMND; i++) {
	    if (plot_commands[i][j][*plot]) {
		if (saved_state[i][j])
		    saved_state[i][j]
			= (char *)realloc(saved_state[i][j],
				         strlen(plot_commands[i][j][*plot])+1);
		else
		    saved_state[i][j]
			= (char *)malloc(strlen(plot_commands[i][j][*plot])+1);
		strcpy(saved_state[i][j],plot_commands[i][j][*plot]);
	    }
	    else {
		if (saved_state[i][j]) {
		    free(saved_state[i][j]);
		    saved_state[i][j] = NULL;
		}
	    }
	}
    }
}
