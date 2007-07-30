/*
 * $Id: write_log.c,v 1.2 2001/08/15 22:25:49 davidive Exp $
 * This routine writes a log file.
 *
 * $Log: write_log.c,v $
 * Revision 1.2  2001/08/15 22:25:49  davidive
 * Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
 * Added code to set up which axis to use and to convert the labels in 1d plots.
 *
 * Revision 1.1.1.1  1993/04/07 19:40:45  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.4  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.3  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.2  1992/12/23  21:15:57  harry
 * Indicate level of comment logging.
 *
 * Revision 1.1  1992/12/23  19:07:50  harry
 * Initial revision
 *
 */

static char ident[] = "$Id: write_log.c,v 1.2 2001/08/15 22:25:49 davidive Exp $";

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <log.h>

static int comment = 0;

void
comment_log_(flag)

int *flag;

{
    if (*flag == 1) ++comment;
    else if (comment && *flag == -1) --comment;
    else if (*flag == 0) comment = 0;
}

void
write_log_(string, length)

char *string;
int length;

{
    if (log_fd != -1) {
	if (comment) write(log_fd, "# # # # # # # # # # # ", comment+comment);
	write(log_fd, string, length);
	write(log_fd, "\n", 1);
    }
}
