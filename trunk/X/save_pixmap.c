/*
 * $Id: save_pixmap.c,v 1.9 2001/12/28 23:43:25 davidive Exp $
 * This routine saves the pixmap of the graphics window, and displays it in
 * the looping window, creating the window if necessary.
 *
 * $Log: save_pixmap.c,v $
 * Revision 1.9  2001/12/28 23:43:25  davidive
 * Don't popup loop widget till done for use on 3d card X server
 *
 * Revision 1.8  2001/03/23 19:35:29  harryive
 * Add "batch" mode to looping for use with Xvfb - in this mode their is no
 * loop window, but the pixmaps are still created.
 *
 * Revision 1.7  2001/03/16 20:38:56  harryive
 * Add more delays.
 *
 * Revision 1.6  2000/12/07 19:50:14  harry
 * Allow dumping of loops of 100 images or more.
 *
 * Revision 1.5  1997/10/15 19:25:27  harry
 * Make sure loops finish a plot before we copy into the pixmap.
 *
 * Revision 1.4  1997/06/03 19:13:49  davidive
 * The big mouse slicing changeover. We have the new form, only requires you
 * set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 * always runs through init_points_ to catch all these things.
 * note: only actually works when two dimentions have points set. Needs to
 * be expanded into 1, 3 or 4.
 *
 * Revision 1.3  1996/05/30 17:24:41  harry
 * Add more Xsync calls.
 *
 * Revision 1.2  1995/05/26  15:20:10  harry
 * Add SAVE_WINDOW command and widget.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:25  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.6  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 2.5  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 2.4  1992/12/23  19:07:32  harry
 * Change callback from call_driver to loop_quit_call.
 *
 * Revision 2.3  1992/12/02  18:48:22  harry
 * Disallow resizes of the loop display.  Call XSync before grabbing
 * the pixmap.
 *
 * Revision 2.2  1992/09/29  18:50:36  harry
 * Change the loop window to be a widget.
 *
 * Revision 2.1  1992/08/11  16:19:32  harry
 * Use Display_Pixmap routine to display plot in loop window.
 *
 * Revision 2.0  1992/07/01  21:16:28  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/06/10  23:07:06  harry
 * Put initialization of num_pixmaps here.
 *
 * Revision 1.1  1992/06/09  21:05:29  harry
 * Initial revision
 *
 */
static char ident[] = "$Id: save_pixmap.c,v 1.9 2001/12/28 23:43:25 davidive Exp $";

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <X11/Xmu/Xmu.h>
#include <loop.h>
#include <ive.h>
#include <stdio.h>

int num_pixmaps=0;
int max_pixmaps=0;
GC loop_gc=NULL;

extern Widget xgks_widget, Toplevel;
extern void loop_input_call();
extern void loop_quit_call();

void save_pixmap_(int *batch)

{
    Display *dpy = XtDisplay(xgks_widget);
    Window win = XtWindow(xgks_widget);
    XSetWindowAttributes xswa;
    XGCValues xgcv;
    unsigned int nchildren;
    static int zero=0;
    XWindowAttributes watt;
    XExposeEvent xev;

    if (num_pixmaps==0) {
	Window parent = XtWindow(XtParent(xgks_widget));
	XWindowAttributes win_info;
	Position x, y;
	int xx, yy;

	if (max_pixmaps==0) {
	    max_pixmaps = 10;
	    pixmap = (Pixmap *)malloc(max_pixmaps*(sizeof (Pixmap*)));
	}
	XRaiseWindow(dpy, parent);
	XGetWindowAttributes(dpy, parent, &win_info);
 	XTranslateCoordinates(dpy, parent, win_info.root, 0, 0, 
			      &xx, &yy, &loop_win);
	x = xx-win_info.x;
	y = yy-win_info.y;
	loop_height = win_info.height;
	loop_width = win_info.width;
	loop_depth = win_info.depth;
	if (loop_gc == NULL) {
	    xgcv.graphics_exposures = False;
	    loop_gc = XCreateGC(dpy,
				RootWindowOfScreen(XtScreen(xgks_widget)),
				(unsigned long) GCGraphicsExposures,
				&xgcv);
	}
	if (*batch == 0) {
	    loop_popup = 
		XtVaCreatePopupShell(
				     "ive loop",
				     transientShellWidgetClass,
				     Toplevel, XmNheight, loop_height,
				     XmNwidth, loop_width, XmNx, x, XmNy, y,
				     NULL);
	    XtAddCallback(loop_popup, XmNdestroyCallback,
			  loop_quit_call, NULL);
	    XtAddCallback(loop_popup, XmNdestroyCallback, im_gone_callback,
			  &loop_popup);
	    loop_widget = XtVaCreateWidget("ive-loop",
					   xmDrawingAreaWidgetClass,
					   loop_popup,
					   XmCResizePolicy, XmRESIZE_NONE,
					   NULL);
	    XtAddCallback(loop_widget,XmNinputCallback,loop_input_call,NULL);
	    XtManageChild(loop_widget);
	    XtManageChild(loop_popup);
	    XtPopup(loop_popup, XtGrabNone);
	    loop_win = XtWindow(loop_widget);
	    XtPopdown(loop_popup);

	}
	else loop_popup = NULL;
    }

	// 3 is the magic number
    XSync(dpy, 0);
    XSync(dpy, 0);
    XSync(dpy, 0);
    update_all_();
    XSync(dpy, 0);
    XSync(dpy, 0);
    XSync(dpy, 0);
    usleep(100000);
    XSync(dpy, 0);
    if (num_pixmaps == max_pixmaps) {
	max_pixmaps+=10;
	pixmap = (Pixmap *) realloc(pixmap, max_pixmaps*(sizeof (Pixmap*)));
    }
    pixmap[num_pixmaps] = XCreatePixmap(dpy, win, loop_width,
					loop_height, loop_depth);
    XSync(dpy, 0);
    usleep(100000);
    XSync(dpy, 0);
    XCopyArea(dpy, win, pixmap[num_pixmaps], loop_gc, 0, 0, loop_width,
	      loop_height, 0, 0);
        XSync(dpy, 0);
	/*    if (*batch == 0) {
      XtPopup(loop_popup, XtGrabNone);
      Display_Pixmap(num_pixmaps);
      }*/
    num_pixmaps++;
}
