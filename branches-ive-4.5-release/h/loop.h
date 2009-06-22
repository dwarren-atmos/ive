/*
 * $Id: loop.h,v 1.3 2000/12/07 19:50:21 harry Exp $
 * Header file for loop information.
 *
 * $Log: loop.h,v $
 * Revision 1.3  2000/12/07 19:50:21  harry
 * Allow dumping of loops of 100 images or more.
 *
 * Revision 1.2  1993/12/15  18:00:33  harry
 * Changes to comments.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:10  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.3  1992/12/04  18:34:21  harry
 * Add loop_timer.
 *
 * Revision 2.2  1992/09/29  18:47:37  harry
 * Add widgets.
 *
 * Revision 2.1  1992/08/11  16:17:39  harry
 * Add parameters associated with loop widget.
 *
 * Revision 2.0  1992/07/01  21:16:24  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/06/10  23:07:26  harry
 * Remove initialization of num_pixmaps.
 *
 * Revision 1.1  1992/06/09  21:05:50  harry
 * Initial revision
 *
 */

int num_pixmaps;		/* Number of pixmaps in the loop */
int max_pixmaps;		/* Number of pixmaps in the pixmap array */
Pixmap *pixmap;			/* The pixmaps themselves */
Widget loop_form;		/* The loop control popup */	    
Widget loop_popup;		/* The loop popup */
Widget loop_widget;		/* The loop widget */
Window loop_win;		/* The loop window */
GC loop_gc;			/* The loop GC */
XtIntervalId loop_timer;	/* The loop timer */
/* XWindowAttributes win_info;	/* Loop window information */
Dimension loop_height, loop_width; /* Loop window dimensions */
int loop_depth;			/* Loop window depth */
int loop_frame;			/* Current loop frame */
int loop_run;			/* Loop run button condition */
int loop_status;		/* Loop status (on/off) */
enum loop_direction {FORWARD, BACKWARD, CYCLE}
    loop_direction, loop_cycle_direction;
				/* Loop direction */
int loop_start,			/* Start pause in millisec */
    loop_delay,			/* Inter-frame delay in millisec */
    loop_end;			/* End pause in millisec */


