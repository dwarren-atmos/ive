/****************************************************************************
 * $Id: add_credits.c,v 1.87 2007-07-30 22:28:42 warren Exp $
 *
 * This routine puts a credit message into a widget.
 *
 * Call: add_credits(Display *dpy, Widget widget)
 *
 * dpy		Display of widget to write to
 * widget	Widget to write to
 *
 * $Log: add_credits.c,v $
 * Revision 1.87  2007-07-30 22:28:42  warren
 * Change release and prep for first svn version.
 *
 * Revision 1.86  2006/04/26 15:46:33  warren
 * update release info - new release as it finally works on Linux.
 *
 * Revision 1.85  2002/12/09 18:08:45  warren
 * Change credits to reflect release from beta.
 *
 * Revision 1.84  2002/05/30 21:41:39  warren
 * Update fix number
 *
 * Revision 1.83  2002/05/30 16:42:42  warren
 * add a "grayline" command to use when you will be printing in black and white,
 * and added an FPE handler when initializing windows to deal with an FPE out
 * of X.
 *
 * Revision 1.82  2001/11/17 00:16:47  warren
 * It's time for a new official add_credits (new rev) and included traj.inc
 * and color.inc in the new reset function.
 *
 * Revision 1.81  2001/03/29 18:54:48  warren
 * Increases fix #
 *
 * Revision 1.80  2001/03/16 20:35:11  warren
 * Change fix # to 10
 *
 * Revision 1.79  2000/11/29 17:39:24  warren
 * Add of file history -> new beta number
 *
 * Revision 1.78  2000/08/25 18:00:39  warren
 * increased fix #
 *
 * Revision 1.77  2000/08/02 20:31:08  warren
 * Increase rev for surface vectors.
 *
 * Revision 1.76  2000/07/20 20:17:03  warren
 * upped rev
 *
 * Revision 1.75  2000/05/10 22:57:08  warren
 * increased rev to fix 3
 *
 * Revision 1.74  2000/04/07 20:50:43  warren
 * Upped fix count
 *
 * Revision 1.73  2000/03/28 23:43:17  warren
 * upped rev and downed fix #
 *
 * Revision 1.72  2000/03/03 23:10:33  warren
 * Pop up color widget when filled plot is made and up rev
 *
 * Revision 1.71  2000/02/15 19:01:21  warren
 * Fixed rev to match feature changes
 *
 * Revision 1.70  2000/01/21 00:05:57  warren
 * Upped rev
 *
 * Revision 1.69  1999/12/22 00:38:01  warren
 * upped rev
 *
 * Revision 1.68  1999/10/26 17:41:42  warren
 * up number on fixes
 *
 * Revision 1.67  1999/08/06 22:58:32  warren
 * Update version
 *
 * Revision 1.66  1999/02/08 23:36:16  warren
 * uped to beta 8 and declared it release candidate.
 *
 * Revision 1.65  1999/02/04 00:28:04  harry
 * Add vector scaling.
 *
 * Revision 1.64  1998/12/11 19:36:15  warren
 * Upped rev number
 *
 * Revision 1.63  1998/11/04 21:24:22  warren
 * Fixed trajectories to actually use the trajectory color table.
 *
 * Revision 1.62  1998/09/08 18:18:25  warren
 * Uped beta number
 *
 * Revision 1.61  1998/07/17 19:50:33  warren
 * up rev number
 *
 * Revision 1.60  1998/03/04 19:13:35  davidive
 * First cut at trajectories and trajectory widget using colors to show depth
 * and arrows to show direction.
 *
 * Revision 1.59  1998/01/12 20:21:34  warren
 * up the version for Harry's patch
 *
 * Revision 1.58  1998/01/07 15:55:20  warren
 * fix alpha to not care about underflows correctly
 *
 * Revision 1.57  1997/12/31 23:30:58  warren
 * Fixed Dales color loxking problem, holton's crashes etc.
 *
 * Revision 1.56  1997/12/29 20:21:14  warren
 * Upped rev to go with last 2 changes
 *
 * Revision 1.55  1997/10/17 15:41:48  harry
 * Version 4.1.4 - fixes to looping.
 *
 * Revision 1.54  1997/09/04 21:31:10  harry
 * Go to 4.1.3 - fix in skewt.f
 *
 * Revision 1.53  1997/08/18 23:38:55  harry
 * Up rev number - fix in driver.F
 *
 * Revision 1.52  1997/08/08 18:34:01  warren
 * Update Rev Number
 *
 * Revision 1.51  1997/07/30 16:46:17  davidive
 * Fix gks window - pop front and back with control/mouse buttons
 * Rev up to 4.1.0 Released version
 *
 * Revision 1.50  1997/07/15 17:41:38  warren
 * Increase beta number, get rid of empty popup that was left after file
 * widgets were gone.
 *
 * Revision 1.49  1997/07/02 14:33:52  harry
 * Beta 10 for rev 4.1.
 *
 * Revision 1.48  1997/04/18 17:42:18  warren
 * Remembering to update rev finally
 *
 * Revision 1.47  1997/04/11 18:46:37  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.46  1997/02/21 21:18:12  warren
 * New beta release.
 *
 * Revision 1.45  1997/01/29 23:32:51  warren
 * Up the rev
 *
 * Revision 1.44  1997/01/14 23:39:04  warren
 * new beta
 *
 * Revision 1.43  1996/12/17  15:36:15  warren
 * look for beta transforms first if compiled with -DBETA and up rev.
 *
 * Revision 1.42  1996/10/09 19:57:36  harry
 * Start 4.1 with beta 0
 *
 * Revision 1.41  1996/08/15 18:59:37  harry
 * Version 4.0.4 - fix to setlineplt.f.
 *
 * Revision 1.40  1996/08/12 18:33:55  harry
 * Fix for 4.0.3 - incorrect usage of float_nozero in loop_setup_calls.c
 *
 * Revision 1.39  1996/08/11 22:51:57  harry
 * A variety of bug fixes to 4.0.1.
 *
 * Revision 1.38  1996/07/12  20:55:44  warren
 * Added environment var IVEPSDIR for mpage stuff.
 *
 * Revision 1.37  1996/06/04 22:59:11  harry
 * Minor typing error - this is the REAL 4.0.0!
 *
 * Revision 1.36  1996/06/04 22:33:51  harry
 * Rev 4.0.0 is ready for prime time!
 *
 * Revision 1.35  1996/05/30 18:31:47  warren
 * beta 12 11 didn't even get as afr as being committed.
 *
 * Revision 1.34  1996/05/15 22:59:56  warren
 * beta 9 -> 10
 *
 * Revision 1.33  1996/05/02 23:10:26  harry
 * Beta 8 -> beta 9
 *
 * Revision 1.32  1996/05/02 19:55:59  harry
 * 4.0 beta 7 => beta 8
 *
 * Revision 1.31  1996/04/23 18:16:55  warren
 * Changes for terrain
 *
 * Revision 1.30  1996/04/12 16:23:24  harry
 * Change the list of co-conspirators.
 *
 * Revision 1.29  1996/03/29 21:16:43  harry
 * Change to 4.0 Beta 6.
 *
 * Revision 1.28  1996/03/06 15:17:21  warren
 * New beta rev 4.5
 *
 * Revision 1.27  1996/02/26 15:17:41  warren
 * Upped rev to beta 4
 *
 * Revision 1.26  1996/02/22 15:19:39  warren
 * Beta2 -> 3
 *
 * Revision 1.25  1996/02/16 17:50:03  warren
 * update release
 *
 * Revision 1.24  1995/12/18  19:02:32  harry
 * Beta 0 -> Beta 1.
 *
 * Revision 1.23  1995/12/15 23:35:25  warren
 * Make 4.0 BETA and add code for later use in desensitising the sliders.
 *
 * Revision 1.22  1995/09/05  20:31:16  harry
 * Start of release 4.0 alpha.
 *
 * Revision 1.21  1995/08/24  22:26:30  warren
 * Make Beta Release
 *
 * Revision 1.20  1995/08/24  18:18:51  harry
 * Good-bye Yuri.
 *
 * Revision 1.19  1995/07/12  14:57:19  warren
 * Make this a beta
 *
 * Revision 1.18  1995/03/24  22:56:05  warren
 * 3.3 here we go.
 *
 * Revision 1.17  1995/02/01  16:46:29  warren
 * Official release of 3.2
 *
 * Revision 1.16  1995/01/30  16:06:05  warren
 * Up the version one for previous  update.
 *
 * Revision 1.15  1995/01/19  15:31:20  warren
 * I'm checking my first wave of fixes for the alpha so Christof can use them.
 *
 * Revision 1.14  1995/01/10  16:38:30  warren
 * I forgot to increase the version for the color section change.
 *
 * Revision 1.13  1994/12/29  19:57:30  warren
 * New gks flush routine and fixed alpha missing label bug
 *
 * Revision 1.12  1994/12/27  23:59:39  warren
 * new version with the color lock and tic mark bugs fixed
 *
 * Revision 1.11  1994/11/22  22:50:38  harry
 * Correct vector locking, add support for vector positioning, add
 * support for filled contour overlays.  Increment version to 3.2 beta 2.
 *
 * Revision 1.10  1994/10/08  18:59:48  warren
 * The annotation widget and updating it.
 *
 * Revision 1.9  1994/05/18  17:52:57  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.8  1994/04/20  16:58:05  harry
 * Start version 3.2 with 3.2 alpha 1.
 *
 * Revision 1.7  1994/03/15  19:56:01  harry
 * Finally - the real version 3.1.0!
 *
 * Revision 1.6  1993/10/22  16:26:10  yuri
 * Changed 3.1.0 "alpha" to "beta".
 *
 * Revision 1.5  1993/08/18  15:45:06  harry
 * Change version to 3.1.0 alpha.
 *
 * Revision 1.4  1993/08/13  22:54:34  harry
 * Change to real 3.0.0 version.
 *
 * Revision 1.3  1993/06/10  22:08:50  harry
 * Change the names in the credits and add a line over the E in IVE.
 * A moment of silence as we mourn the passing of "Bill the Cat" from the
 * credits of this fine piece of programming.
 *
 * Revision 1.2  1993/05/28  19:06:16  harry
 * Add Yuri to the credits.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:15  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.14  1993/03/18  19:25:30  warren
 * uped to 2.22
 *
 * Revision 2.13  1993/03/16  23:30:54  warren
 * removed beta for release
 *
 * Revision 2.12  1993/01/22  19:06:19  warren
 * added christoph removed bill re. dale
 *
 * Revision 2.11  1993/01/09  00:24:59  harry
 * Change version to 2.21 beta.
 *
 * Revision 2.10  1993/01/04  17:14:38  warren
 * changed version to 2.2 (due to getvar and transform change)
 *
 * Revision 2.9  1992/12/23  22:19:34  harry
 * Change 2.14 beta to 2.14
 *
 * Revision 2.8  1992/12/22  19:02:34  harry
 * Change version to 2.14 beta
 *
 * Revision 2.7  1992/12/18  20:08:09  warren
 * removed beta for 1.23
 *
 * Revision 2.6  1992/12/02  16:55:41  harry
 * Change rev to 2.13 beta
 *
 * Revision 2.5  1992/11/18  19:06:31  millerp
 * New version 2.12
 *
 * Revision 2.4  1992/11/16  22:37:51  harry
 * Change rev to 2.11
 *
 * Revision 2.3  1992/10/08  18:09:29  harry
 * Change rev from 2.10 beta to 2.10.
 *
 * Revision 2.2  1992/09/23  23:09:12  harry
 * Change calling sequence to get the correct colormap information.
 *
 * Revision 2.1  1992/09/03  17:31:16  harry
 * Change rev to 2.01
 *
 * Revision 2.0  1992/07/01  21:11:40  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.7  1992/06/25  23:12:12  harry
 * Change version to 2.00
 *
 * Revision 1.6  1992/06/17  21:25:17  warren
 * fixed vanishing DECstation Text.
 *
 * Revision 1.5  1992/06/09  17:35:01  harry
 * The latest and greatest in credits.
 *
 * Revision 1.4  1992/06/04  15:44:25  harry
 * More changes.
 *
 * Revision 1.3  1992/06/03  22:51:02  harry
 * More changes to the message.
 *
 * Revision 1.2  1992/06/03  19:44:44  harry
 * Change credit mesage.
 *
 * Revision 1.1  1992/06/03  19:38:58  harry
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: add_credits.c,v 1.87 2007-07-30 22:28:42 warren Exp $";

#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Xlib.h>
#include <stdio.h>

#define TFONT "*times-bold-r*24*"	/* Font for "IVE" */
#define VFONT "*times-bold-r*18*"	/* Font for version number (bold) */
#define FONT "*times-medium-r*16*"	/* Font for normal text */
#define IFONT "*times-medium-i*16*"	/* Font for italic text */
#define TFCOLOR "green4"		/* Color for "IVE" */
#define TBCOLOR "PaleGreen"		/* Background color for "IVE" */
#define DEFAULT_FONT "9x15"		/* Font to use if others not found */

static char *title = "IVE";
static char *version = "Version 4.8 - August 19 2014";
/*
   In credits strings, if a line starts with
	"\b" => line in italics
	"\t" => center the line
	*/
static char *credits[] = {
    "\b\tDeveloped in the",
    "",
    "\tDepartment of Atmospheric Sciences, University of Washington, Seattle",
    "",
    "\b\tby",
    "",
    "\tHarry Edmon, David Warren, Dale Durran",
    "\tChristoph Schaer, Pete Miller, Rajul Pandya,",
    "\tCurtis James and Alex Reinecke",
    "",
    "\b\tusing",
    "",
    "\tNCAR Graphics library from the National Center for Atmospheric Research",
    "\tudunits and netCDF libraries from the Unidata Program Center, UCAR",
    "\tlibpng from libpng.org and libharu (pdf library) from libharu.org",
    NULL};

void add_credits(dpy, widget) 
    Display *dpy;
    Widget widget;
{
    Window win=XtWindow(widget);
    XGCValues gc_val;
    int screen;
    GC gc, tgc;
    XFontStruct *font_info, *ifont_info, *tfont_info, *vfont_info;
    XWindowAttributes windowattr;
    Colormap cmap;
    int font_height, i, width, ystart;
    char *s;
    XColor tfc, tfc2, tbc, tbc2;
    /*
       Get graphics context for window
       */
    screen = DefaultScreen(dpy);
    gc_val.background = (unsigned long) WhitePixel(dpy, screen);
    gc_val.foreground = (unsigned long) BlackPixel(dpy, screen);
    gc_val.graphics_exposures = False;
    gc_val.function = GXcopy;
    gc = XCreateGC (dpy, win, GCGraphicsExposures|GCForeground|GCBackground|
		    GCFunction, 
		    &gc_val);
    /*
       Get graphics context for title "IVE"
       */
    XGetWindowAttributes(dpy, win, &windowattr);
    XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
    if (windowattr.depth > 2) {
	XAllocNamedColor(dpy, cmap, TFCOLOR, &tfc, &tfc2);
	XAllocNamedColor(dpy, cmap, TBCOLOR, &tbc, &tbc2);
	gc_val.foreground = tbc.pixel;
	gc_val.background = tbc.pixel;
	tgc = XCreateGC (dpy, win, 
			 GCGraphicsExposures|GCForeground|GCBackground|
			 GCFunction,
			 &gc_val);
    }
    else tgc = gc;
    /*
       Get fonts
       */
    if ((tfont_info = XLoadQueryFont(dpy, TFONT)) == NULL &&
	(tfont_info = XLoadQueryFont(dpy, DEFAULT_FONT)) == NULL) {
	fprintf(stderr, "add_credits: Cannot load font %s\n", TFONT);
	exit(1);
    }
    if ((vfont_info = XLoadQueryFont(dpy, VFONT)) == NULL &&
	(vfont_info = XLoadQueryFont(dpy, DEFAULT_FONT)) == NULL) {
	fprintf(stderr, "add_credits: Cannot load font %s\n", VFONT);
	exit(1);
    }
    
    if ((font_info = XLoadQueryFont(dpy, FONT)) == NULL &&
	(font_info = XLoadQueryFont(dpy, DEFAULT_FONT)) == NULL) {
	fprintf(stderr, "add_credits: Cannot load font %s\n", FONT);
	exit(1);
    }
    if ((ifont_info = XLoadQueryFont(dpy, IFONT)) == NULL &&
	(ifont_info = XLoadQueryFont(dpy, DEFAULT_FONT)) == NULL) {
	fprintf(stderr, "add_credits: Cannot load font %s\n", IFONT);
	exit(1);
    }
    /*
       Write title
       */
    XSetFont(dpy, tgc, tfont_info->fid);
    width = XTextWidth(tfont_info, title, strlen(title));
    if (windowattr.depth > 2) {
	/*
	   Put "pale green" rectangle behind title "IVE"
	   */
	XFillRectangle(dpy, win, tgc, (windowattr.width-width)/2-3,
		       60-tfont_info->ascent-3, width+6,
		       tfont_info->ascent+font_info->descent+3);
	XSetForeground(dpy, tgc, tfc.pixel);
    }
    XDrawString(dpy, win, tgc, (windowattr.width-width)/2,
		60, title, strlen(title));
    ystart = 60 + tfont_info->ascent + tfont_info->descent;
    /*
       Add line over E
       */
    i = XTextWidth(tfont_info, title, strlen(title)-1);
    XDrawString(dpy, win, tgc, (windowattr.width-width)/2+i+2,
                60-tfont_info->ascent-tfont_info->descent+2, "_", 1);
    /*
       Write version
       */
    XSetFont(dpy, gc, vfont_info->fid);
    width = XTextWidth(vfont_info, version, strlen(version));
    XDrawString(dpy, win, gc, (windowattr.width-width)/2,
		ystart, version, strlen(version));
    /*
       Write rest of credits
       */
    ystart += vfont_info->ascent + vfont_info->descent;
    font_height = font_info->ascent + font_info->descent;
    for (i=0; s=credits[i]; ++i) {
	if (*s == '\b') {	/* Italics */
	    s++;
	    XSetFont(dpy, gc, ifont_info->fid);
	}
	else XSetFont(dpy, gc, font_info->fid);

	if (*s == '\t') {	/* Center line */
	    s++;
	    width = XTextWidth(font_info, s, strlen(s));
	    XDrawString(dpy, win, gc, (windowattr.width-width)/2,
			ystart+(i+1)*font_height, s, strlen(s));
	}
	else XDrawString(dpy, win, gc, 10, ystart+(i+1)*font_height,
			 s, strlen(s));
    }
    /*
       Wrapup
       */
    XFreeGC(dpy, gc);
    if (windowattr.depth > 2) XFreeGC(dpy, tgc);
    
}
