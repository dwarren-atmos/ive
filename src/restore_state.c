/*
 * $Id: restore_state.c,v 1.13 2001/01/06 00:00:27 harry Exp $
 * This Fortran callable routine restores the commands from the saved plot.
 *
 * call restore_state(overlay)
 *
 * where "overlay" is the overlay number.
 *
 * $Log: restore_state.c,v $
 * Revision 1.13  2001/01/06 00:00:27  harry
 * The TYPE command should be done first in creating a plot.
 *
 * Revision 1.12  1998/12/29 16:40:23  harry
 * Properly loop plots with "draw_line" command.
 *
 * Revision 1.11  1997/06/21 00:15:02  harry
 * Fixes to looping for switches between contour values and contour intervals.
 *
 * Revision 1.10  1997/04/15 17:24:41  harry
 * Remove time_tag - use multiple files instead.
 * Various fixes to vector/scalar looping.
 *
 * Revision 1.9  1996/10/16 14:42:25  warren
 * don't resize on loop
 *
 * Revision 1.8  1996/10/09 18:54:42  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.7  1995/05/26  15:21:17  harry
 * Add SAVE_WINDOW command and widget.
 *
 * Revision 1.6  1993/08/12  18:54:43  harry
 * If FIELD changes, do not change CONTOUR_INTERVAL or CONTOUR_VALUES.
 *
 * Revision 1.5  1993/07/22  19:00:03  warren
 * ignore lockcolortabel command in loop.
 *
 * Revision 1.4  1993/06/28  21:26:28  harry
 * Add "STEP" command and debug argument to "INPUT" command to allow
 * single steps of input files.
 *
 * Revision 1.3  1993/06/18  17:19:51  harry
 * Make long loops work by adding save_state.c and changing loop.f and
 * restore_state.c.
 *
 * Revision 1.2  1993/06/11  20:02:08  harry
 * Change loop to use NEW_PLOT & OVERLAY_PLOT and check # frames.
 * Add NEW_PLOT and OVERLAY_PLOT to list of commands to ignore in
 * restore_state.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:16  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.8  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.7  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.6  1993/01/05  22:35:36  harry
 * Process FIELD command seperately.  Add Id and Log info.
 *
 */

static char ident[] = "$Id: restore_state.c,v 1.13 2001/01/06 00:00:27 harry Exp $";

#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <state.h>

#define doit(i) \
    if (saved_state[i][*overlay] && \
	(cur_commands[i] == NULL || \
	 strcasecmp(cur_commands[i], \
		    saved_state[i][*overlay]))) \
	driver_(saved_state[i][*overlay], &zero, \
		strlen(saved_state[i][*overlay]));

static int cmdskip[] = {P_DUMP, P_EXIT, P_FIELD, P_FILE, P_FREE, P_HELP,
			    P_INPUT, P_LIFT_OVERLAY, P_LOOP, P_LOOP_DUMP, 
			    P_LOOP_QUIT, P_LOCKCOLORTABLE, P_NEW_PLOT, 
			    P_OVERLAY_PLOT, P_PLOT, P_PRINT, P_QUIT, 
			    P_REDRAW, P_SAVE, P_SAVE_WINDOW, P_STEP,
			    P_TRANSFORM_FILE, P_TYPE, P_WRITE_FRAME,
			    P_WRITECOLORTABLE, P_RESIZE, P_DRAW_LINE, -1};
static int skip[NVCMND], zero=1;

extern void driver_();

void restore_state_(overlay, flag)

int *overlay, *flag;

{

    int i, j, k;
/*
  One time initialization.
  */
    if (zero) {
	zero = 0;
	for (i=0; i<NVCMND; i++) skip[i] = 1;
	for (i=0; cmdskip[i]>= 0; i++)
	    skip[cmdskip[i]] = 0;
    }
    /*
      If DRAW_LINE, DRAW_LINE_MAP, DRAW_POINT, or DRAW_POINT_MAP exist - it is all we do.
    */
    if (saved_state[P_DRAW_LINE][*overlay]) {
	driver_(saved_state[P_DRAW_LINE][*overlay], &zero, strlen(saved_state[P_DRAW_LINE][*overlay]));
	*flag = 1;
	return;
    }
    if (saved_state[P_DRAW_POINT][*overlay]) {
	driver_(saved_state[P_DRAW_POINT][*overlay], &zero, strlen(saved_state[P_DRAW_POINT][*overlay]));
	*flag = 1;
	return;
    }
    if (saved_state[P_DRAW_LINE_MAP][*overlay]) {
	driver_(saved_state[P_DRAW_LINE_MAP][*overlay], &zero, strlen(saved_state[P_DRAW_LINE_MAP][*overlay]));
	*flag = 1;
	return;
    }
    if (saved_state[P_DRAW_POINT_MAP][*overlay]) {
	driver_(saved_state[P_DRAW_POINT_MAP][*overlay], &zero, strlen(saved_state[P_DRAW_POINT_MAP][*overlay]));
	*flag = 1;
	return;
    }
    *flag = 0;
    doit(P_TYPE);
    doit(P_TRANSFORM_FILE);
    doit(P_FILE);
    if (saved_state[P_FIELD][*overlay] &&
	strcasecmp(cur_commands[P_FIELD], saved_state[P_FIELD][*overlay])) {
	skip[P_CONTOUR_INTERVAL] = 0;
	skip[P_CONTOUR_VALUES] = 0;
	doit(P_FIELD);
    }
    else {
	skip[P_CONTOUR_INTERVAL] = 1;
	skip[P_CONTOUR_VALUES] = 1;
    }
    for (i=0; i<NVCMND; i++) {
	if (skip[i]) doit(i);
    }
}

