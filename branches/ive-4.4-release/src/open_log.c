/*
 * $Id: open_log.c,v 1.3 1998/09/04 20:52:00 harry Exp $
 * This routine opens a log file.
 *
 * $Log: open_log.c,v $
 * Revision 1.3  1998/09/04 20:52:00  harry
 * Add colorbar button to annotation widget.  Rewrite file widgets to
 * remember previous entries.
 *
 * Revision 1.2  1995/08/22  21:46:31  warren
 * Build .F files with correct pointer size on alpha's, don't do strncasecmp
 * without an n.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:11  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.4  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.3  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.2  1992/12/31  17:50:23  harry
 * Allow for two versions of the log file.
 *
 * Revision 1.1  1992/12/23  19:06:47  harry
 * Initial revision
 *
 */

static char ident[] = "$Id: open_log.c,v 1.3 1998/09/04 20:52:00 harry Exp $";

static int not_window=0; /*flag passed to driver; means do update windows*/ 

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <Xm/XmAll.h>
#include <unistd.h>
#include <fcntl.h>
#include <ive_macros.h>
#include <file_widgets.h>
#include <log.h>

int log_fd = -1;

int
open_log_(iflag)

int *iflag;

{
    char buf[1024], buf2[1024], **argv, **get_args();
    int new;
    int argc;

    argv = get_args(&argc);
    if (argc != 2) {
	sprintf(buf, "Command %s requires 1 value", argv[0]);
	(void) make_help_widget_(buf);
	return_args(1);
    }
    if (!strcasecmp(argv[1], "off")||
	!strcasecmp(argv[1], "no")) {
	new = -1;
	printf("bad new\n");
    }
    else {
	strcpy(buf, argv[1]);
	expand_path_(buf, 1024);
	/*
 * If the log file exists, rename it to the same name with a "~".
 */
	if (access(buf, F_OK) == 0) {
	    strcpy(buf2, buf);
	    strcat(buf2, "~");
	    unlink(buf2);
	    link(buf, buf2);
	    unlink(buf);
	}
	if ((new = open(buf, O_WRONLY|O_CREAT|O_TRUNC, 0644)) < 0) {
	    sprintf(buf, "Command %s cannot access file %s",
		    argv[0], argv[1]);
	    (void) make_help_widget_(buf);
	    return_args(1);
	}
    }
    if (log_fd != -1) close(log_fd);
    log_fd = new;
    if (*iflag == 0)
	set_file_box(log_widget.List, argv[1], &log_widget.fsb);
    return_args(0);
}
