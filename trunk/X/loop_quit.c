/*
 * $Id: loop_quit.c,v 1.3 2001/03/23 19:35:29 harryive Exp $
 * This routine quits the loop.
 *
 * $Log: loop_quit.c,v $
 * Revision 1.3  2001/03/23 19:35:29  harryive
 * Add "batch" mode to looping for use with Xvfb - in this mode their is no
 * loop window, but the pixmaps are still created.
 *
 * Revision 1.2  2000/06/12 21:20:30  warren
 * Fixed Dale's end of loop time stuck bug.
 *
 * Revision 1.1.1.1  1993/04/07 19:41:25  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.6  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.5  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.4  1992/12/23  19:06:24  harry
 * Zero out num_pixmaps before destroying the widget.
 *
 * Revision 1.3  1992/12/14  18:35:36  harry
 * Don't try to remove a timeout that was never set.
 *
 * Revision 1.2  1992/12/04  18:35:16  harry
 * Change alarm/signal to XtRemoveTimeOut.
 *
 * Revision 1.1  1992/09/29  18:44:52  harry
 * Initial revision
 *
 *
 */
static char ident[] = "$Id: loop_quit.c,v 1.3 2001/03/23 19:35:29 harryive Exp $";

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <signal.h>
#include <loop.h>

Widget xgks_widget;

void loop_quit_()

{
    Display *dpy = XtDisplay(xgks_widget);
    int i;

    if (loop_form == NULL) return;
    loop_status = 0;
    if (loop_timer) XtRemoveTimeOut(loop_timer);
    loop_timer = 0;
    for (i=0; i < num_pixmaps; ++i)
	XFreePixmap(dpy, pixmap[i]);
    do_loop_();
    num_pixmaps = 0;
    if (loop_popup) XtDestroyWidget(loop_popup);
    loop_form = NULL;
    loop_popup = NULL;
    (void)clear_loop_flags_();
}
