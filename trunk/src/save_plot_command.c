/*
 * $Id: save_plot_command.c,v 1.3 1997/04/15 17:24:41 harry Exp $
 * This Fortran callable routine saves the current commands for a plot.
 *
 * call save_plot_command(overlay, plot)
 *
 *  where "overlay" is the overlay number, "plot" is the plot number.
 *
 * $Log: save_plot_command.c,v $
 * Revision 1.3  1997/04/15 17:24:41  harry
 * Remove time_tag - use multiple files instead.
 * Various fixes to vector/scalar looping.
 *
 * Revision 1.2  1996/10/09 18:54:44  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:17  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.4  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.3  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.2  1993/01/05  22:34:42  harry
 * Add Id and Log info.
 *
 */

static char ident[] = "$Id: save_plot_command.c,v 1.3 1997/04/15 17:24:41 harry Exp $";

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <stdio.h>
#include "state.h"

char *plot_commands[NVCMND][MAX_OVERLAY][MAX_LIST];

save_plot_command_(overlay, plotnum)

int *overlay, *plotnum;

{
    int over = *overlay % MAX_OVERLAY;
    int plot = *plotnum % MAX_LIST;
    int i;

    for (i=0; i<NVCMND; i++) {
	if (cur_commands[i]) {
	    if (plot_commands[i][over][plot])
		plot_commands[i][over][plot]
		    = (char *) realloc(plot_commands[i][over][plot],
				       strlen(cur_commands[i])+1);
	    else
		plot_commands[i][over][plot]
		    = (char *) malloc(strlen(cur_commands[i])+1);
	    strcpy(plot_commands[i][over][plot], cur_commands[i]);
	}
	else {
	    if (plot_commands[i][over][plot]) {
		free(plot_commands[i][over][plot]);
		plot_commands[i][over][plot] = NULL;
	    }
	}
    }
}

