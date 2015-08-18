/*
 *		Copyright IBM Corporation 1989
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of IBM not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 *
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL	61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 *
 * XGKS open workstation function
 * Open display wk->conn if necessary, then create and map the workstation 
 * window wk: workstation list pointer return: (0)---- open succeeds (21)-- 
 * open fails .
 */

/*LINTLIBRARY*/

#include "udposix.h"
#include <stdlib.h>
#include <sys/types.h>			/* for uid_t */
#include <unistd.h>			/* NeXTOS requires that this be */
					/* after <sys/types.h> */
#include <string.h>
#include <pwd.h>
#include <ctype.h>			/* for isupper() & tolower() */
#include <assert.h>
#include "gks_implem.h"
#include <X11/Xresource.h>
#include <X11/IntrinsicP.h>
#include "XgksWidgetP.h"


#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: xopws.c,v 2.7 2000/08/01 16:38:26 steve Exp $";
#endif

static int      InitDefaults();
static int      GetUsersDatabase();
static int      InsureConn();
static int      CreateWindow();
static int      CreateWindowWidget();
static char    *GetHomeDir();
static void     GetWindowGeometry();
static void     AllocGroundColors();
static void     GetWMHints();
static void     CreateGC();
static void     UpdateOpenWSTable();

/*
 * The following may be changed by applications programs to the name of
 * the application.
 */
extern char    *progname;
extern void XgksProcessEvents();

int
xXgksWidgetOpenWs(gw, wk)
        XgksWidget      gw;
        WS_STATE_PTR    wk;
{
        int             status;         /* success */


        /* Insure connection to display. */
        if (!(status = InsureConn(wk)))
          {
            
	    XrmDatabase     rDB;	/* resource database */
            /* Flush all events from the widget's display connection */
            XFlush(XtDisplay(gw));
            
            /* Create window -- using widget resources for defaults. */
            if (!(status = CreateWindowWidget(rDB, gw, wk)))
              {
                XEvent          xev;    /* X event structure */
                XWindowAttributes       WinAtt;
                
                
                /* Put window id in widget instance */
                gw->core.window = wk->win;
                
                /* Map window. */
                XMapWindow(wk->dpy, wk->win);
                XWindowEvent(wk->dpy, wk->win, ExposureMask, &xev);
                XSync(wk->dpy, 0);
                
                /* Get size of actual window obtained. */
                XGetWindowAttributes(wk->dpy, wk->win, &WinAtt);
                wk->wbound.x = WinAtt.width;
                wk->wbound.y = WinAtt.height;
                
                /* Update open-workstation table. */
                (void) UpdateOpenWSTable(wk);
                
                /* Select Input Events */
                XtInsertEventHandler((Widget)gw, wk->event_mask,
                                     FALSE, XgksProcessEvents,
                                     (Opaque) NULL,
                                     XtListTail);
                
                /*XSelectInput(wk->dpy, wk->win, wk->event_mask); */
                XSync(wk->dpy, 0);
              }
          }
        
        return status ? status : OK;
}

Gint
xXgksOpenWs(wk)
    WS_STATE_PTR    wk;
{
    int             status;		/* success */

    if (wk->ewstype != X_WIN) {
	status = INVALID;
    } else {

	/* Insure connection to display. */
	if (!(status = InsureConn(wk))) {
	    char           *basename;	/* UPC: base program name */
	    XrmDatabase     rDB;	/* resource database */

	    (void) xXgksSIGIOStart(wk);
	    (void) XgksSIGIO_OFF(wk->dpy);

	    /* Extract program name. */
	    if (progname == NULL)
		progname = "XGKS";
	    if ((basename = strrchr(progname, '/')) == (char *) NULL)
		basename = progname;
	    else
		basename += 1;

	    /* Initialize resource database (i.e. X-defaults). */
	    (void) InitDefaults(basename, wk->dpy, &rDB);

	    /* Create window -- using resource database for defaults. */
	    if (!(status = CreateWindow(basename, rDB, wk))) {
		XEvent          xev;	/* X event structure */
		XWindowAttributes WinAtt;	/* window attributes */

		/* Map window.  Wait for exposure-event. */
		XMapWindow(wk->dpy, wk->win);
		XWindowEvent(wk->dpy, wk->win, ExposureMask, &xev);
		XSync(wk->dpy, 0);

		/* Get size of actual window obtained. */
		XGetWindowAttributes(wk->dpy, wk->win, &WinAtt);
		wk->wbound.x = WinAtt.width;
		wk->wbound.y = WinAtt.height;

		/* Update open-workstation table. */
		(void) UpdateOpenWSTable(wk);

		/* Select Input Events */
		XSelectInput(wk->dpy, wk->win, wk->event_mask);
		XSync(wk->dpy, 0);
	    }

	    (void) XgksSIGIO_ON(wk->dpy);
	}
    }

    return status ? status : OK;
}


/*
 * Insure a connection to the display.
 */
    static
InsureConn(wk)
    WS_STATE_PTR    wk;
{
    int             status = 0;		/* success */
    int             i;
    WS_STATE_PTR    wk_p = NULL;

    /* Check if Display wk->conn has been opened  */
    for (i = 0; i < MAX_OPEN_WS; i++) {
	if (xgks_state.openedws[i].ws_id >= 0 &&
		xgks_state.openedws[i].ws->ewstype == X_WIN) {

	    WS_STATE_PTR    ws = xgks_state.openedws[i].ws;

	    if ((STRCMP(wk->conn, ws->conn) == 0) && (ws != wk)) {
		wk_p = ws;
		break;
	    }
	}
    }

    if (wk_p != NULL) {				/* Has been opened */
	wk->dpy = wk_p->dpy;
	XcCopy(wk, wk_p);			/* copy color information */
    } else {					/* Open a new display */
	if ((wk->dpy = XOpenDisplay(wk->conn)) == NULL) {
	    status = 26;
	} else {
	    char           *ptr = DisplayString(wk->dpy);

	    ufree((voidp)wk->conn);
	    if ((wk->conn = (char *) malloc((size_t) (STRLEN(ptr) + 1))) == 
		    NULL) {
		status = 300;
	    } else {
		STRCPY(wk->conn, ptr);

		/* Set color information. */
		XcInit(wk);

		XSelectInput(wk->dpy, DefaultRootWindow(wk->dpy), 0);
	    }
	}
    }

    return status;
}


/*
 *	Indicate whether a boolean resource is set.  Returns -1 if the
 *	resource doesn't exist, 0 if it's false, and 1 if it's true.
 */
    static int
BoolResource(prog, name, class, rDB)
    char           *prog;		/* program name */
    char           *name;		/* resource name */
    char           *class;		/* resource class */
    XrmDatabase     rDB;		/* resource database */
{
    int             ison = -1;		/* return status = doesn't exist */

    char            name_key[1024];
    char            class_key[1024];
    char           *str_type[20];
    XrmValue        value;

    (void) strcat(strcat(strcpy(name_key, prog), "."), name);
    (void) strcat(strcpy(class_key, "Xgks."), class);

    if (XrmGetResource(rDB, name_key, class_key, str_type, &value) == True) {
	char           *cp;

	(void) strncpy(name_key, value.addr, (int) value.size);
	name_key[value.size] = 0;

	for (cp = name_key; *cp != 0; ++cp) {
	    if (isupper(*cp))
		*cp = tolower(*cp);
	}

	ison =
	    strcmp(name_key, "on") == 0 ||
	    strcmp(name_key, "1") == 0 ||
	    strcmp(name_key, "yes") == 0 ||
	    strcmp(name_key, "set") == 0 ||
	    strcmp(name_key, "true") == 0;
    }
    return ison;
}


/*
 * Create an XGKS window -- with all its associated attributes.
 */
    static
    CreateWindowWidget(rDB, gw, wk)
	 XrmDatabase     rDB;		/* resource database */
	 XgksWidget      gw;
	 WS_STATE_PTR    wk;			/* workstation structure */
{
    int             status = 0;		/* success */
    Display        *dpy = wk->dpy;	/* for convenience */
    XSizeHints      SizeHints;		/* window size hints */
    XSetWindowAttributes
		    xswa;		/* window attributes */

    /*
     * NB: The GKS <-> X color mapping has already been initialized by
     * InsureConn().
     */

    /*
     * Get foreground and background colors (and set them in the
     * colormap).
     */
    (void) AllocGroundColors(wk, "XGKS", rDB, &wk->wsfg, &wk->wsbg);

    /*
     * Init pointer to GSET color-table and set values to NULL.  The
     * table will be alloc'ed and init'ed in gsetcolourrep on the
     * 1st call (DWO).
     */
    wk->set_colour_rep = (Gcobundl *) NULL;

    /* Get window geometry hints. */

    /* Set the window-event mask. */
    wk->event_mask = StructureNotifyMask | 
		     ExposureMask | 
		     KeyPressMask |
		     ButtonPressMask |
		     ButtonReleaseMask |
		     ButtonMotionMask;

    /*
     * Set the window-attributes structure.
     */
    xswa.event_mask = StructureNotifyMask | ExposureMask;
    xswa.background_pixel = XcPixelValue(wk, GKS_BACKGROUND_INDEX);
    xswa.border_pixel = XcPixelValue(wk, GKS_FOREGROUND_INDEX);
    xswa.do_not_propagate_mask = wk->event_mask &
	(KeyPressMask | KeyReleaseMask | ButtonPressMask |
	 ButtonReleaseMask | PointerMotionMask | Button1MotionMask |
	 Button2MotionMask | Button3MotionMask | Button4MotionMask |
	 Button5MotionMask | ButtonMotionMask);
    xswa.colormap = wk->clmp;
    xswa.backing_store	= Always;

    /* Create an idle (i.e. default) cursor. */
    xswa.cursor = XCreateFontCursor(dpy, XC_X_cursor);

    /* Create the window. */
    if (!(wk->win = XCreateWindow(dpy, gw->core.parent->core.window,
				  (int) gw->core.x, (int) gw->core.y,
				  (int) gw->core.width, (int) gw->core.height,
				  gw->core.border_width,		/* border width */
				  wk->vinfo.depth,
				  InputOutput,
				  DefaultVisual(dpy, DefaultScreen(dpy)),
			    (unsigned long) (CWDontPropagate |
					     CWEventMask | 
					     CWColormap | 
					     CWBackPixel |
					     CWBorderPixel |
					     CWCursor | 
					     CWBackingStore),
			    &xswa)))
    {
	status = 26;
    } else {
	Window          win = wk->win;      /* for convenience */
	XWMHints        WMHints;    /* window-manager hints */
	XClassHint      ClassHints; /* class hints */

	/* Create graphics-context for window. */
	(void) CreateGC(dpy, win, wk);

	/* Set foreground and background colors in graphics-context. */
	XSetForeground(dpy, wk->gc, XcPixelValue(wk, GKS_FOREGROUND_INDEX));
	XSetBackground(dpy, wk->gc, XcPixelValue(wk, GKS_BACKGROUND_INDEX));

	/* Set interactive cursors. */
	wk->locateCursor = XCreateFontCursor( dpy, XC_crosshair);
	wk->strokeCursor = XCreateFontCursor( dpy, XC_pencil);
	wk->valueCursor = XCreateFontCursor( dpy, XC_top_left_arrow);
	wk->choiceCursor = XCreateFontCursor( dpy, XC_hand1);
	wk->pickCursor = XCreateFontCursor( dpy, XC_top_left_arrow);
	wk->stringCursor = XCreateFontCursor( dpy, XC_xterm);
	wk->idleCursor = xswa.cursor;

	/* Set interactive beeping.  It's off by default. */
	wk->beep_on = 0;//BoolResource(name, "beep", "Beep", rDB) == 1;

	/*
	 * Initialize last-clipping rectangles to absurd values.  This
	 * will cause actual clipping window to be set.
	 */
	wk->last_pline_rectangle.x = 0;
	wk->last_pline_rectangle.y = 0;
	wk->last_pline_rectangle.width = 0;
	wk->last_pline_rectangle.height = 0;
	wk->last_pmarker_rectangle = wk->last_pline_rectangle;
	wk->last_farea_rectangle = wk->last_pline_rectangle;
	wk->last_text_rectangle = wk->last_pline_rectangle;

	wk->last_dash_index = 1;

	/* Set soft-clipping if appropriate.  It's off by default. */
	wk->soft_clipping_on = gw->xgks.soft_clipping;

	/* Save the setting of backing-store. */
	wk->backing_store_on	= (xswa.backing_store == Always);
    }

    return status;
}


/*
 * Create an XGKS window -- with all its associated attributes.
 */
    static
CreateWindow(name, rDB, wk)
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    WS_STATE_PTR    wk;			/* workstation structure */
{
    int             status = 0;		/* success */
    Display        *dpy = wk->dpy;	/* for convenience */
    XSizeHints      SizeHints;		/* window size hints */
    XSetWindowAttributes
		    xswa;		/* window attributes */

    /*
     * NB: The GKS <-> X color mapping has already been initialized by
     * InsureConn().
     */

    /*
     * Get foreground and background colors (and set them in the
     * colormap).
     */
    (void) AllocGroundColors(wk, "XGKS", rDB, &wk->wsfg, &wk->wsbg);

    /*
     * Init pointer to GSET color-table and set values to NULL.  The
     * table will be alloc'ed and init'ed in gsetcolourrep on the
     * 1st call (DWO).
     */
    wk->set_colour_rep = (Gcobundl *) NULL;

    /* Get window geometry hints. */
    (void) GetWindowGeometry(name, dpy, rDB, &SizeHints);

    /* Set the window-event mask. */
    wk->event_mask = StructureNotifyMask | 
		     ExposureMask | 
		     KeyPressMask |
		     ButtonPressMask |
		     ButtonReleaseMask |
		     ButtonMotionMask;

    /*
     * Set the window-attributes structure.
     */
    xswa.event_mask = StructureNotifyMask | ExposureMask;
    xswa.background_pixel = XcPixelValue(wk, GKS_BACKGROUND_INDEX);
    xswa.border_pixel = XcPixelValue(wk, GKS_FOREGROUND_INDEX);
    xswa.do_not_propagate_mask = wk->event_mask &
	(KeyPressMask | KeyReleaseMask | ButtonPressMask |
	 ButtonReleaseMask | PointerMotionMask | Button1MotionMask |
	 Button2MotionMask | Button3MotionMask | Button4MotionMask |
	 Button5MotionMask | ButtonMotionMask);
    xswa.colormap = wk->clmp;
    xswa.backing_store	= Always;
    //BoolResource(name, "backingstore", "Backingstore", rDB) == 0
    //	    ? NotUseful
    //	    : DoesBackingStore(DefaultScreenOfDisplay(dpy)) == NotUseful
    //		? NotUseful
    //		: Always;

    /* Create an idle (i.e. default) cursor. */
    xswa.cursor = XCreateFontCursor(dpy, XC_X_cursor);

    /* Create the window. */
    if (!(wk->win = XCreateWindow(dpy, DefaultRootWindow(dpy),
			    (int) SizeHints.x, (int) SizeHints.y,
			    (int) SizeHints.width, (int) SizeHints.height,
			    5,		/* border width */
			    wk->vinfo.depth,
			    InputOutput,
				  DefaultVisual(dpy, DefaultScreen(dpy)),
			    (unsigned long) (CWDontPropagate |
					     CWEventMask | 
					     CWColormap | 
					     CWBackPixel |
					     CWBorderPixel |
					     CWCursor | 
					     CWBackingStore),
			    &xswa)))
    {
	status = 26;
    } else {
	Window          win = wk->win;      /* for convenience */
	XWMHints        WMHints;    /* window-manager hints */
	XClassHint      ClassHints; /* class hints */

	/* Set standard window properties. */
	XSetStandardProperties(dpy, win, name, name, None,
			       (char **) NULL, 0, &SizeHints);

	/* Set window-manager hints. */
	(void) GetWMHints(dpy, name, rDB, &WMHints);
	XSetWMHints(dpy, win, &WMHints);

	/* Set class hints. */
	if ((ClassHints.res_name = getenv("RESOURCE_NAME")) == NULL)
	    ClassHints.res_name = name;
	ClassHints.res_class = name;
	(void) XSetClassHint(dpy, win, &ClassHints);

	/* Create graphics-context for window. */
	(void) CreateGC(dpy, win, wk);

	/* Set foreground and background colors in graphics-context. */
	XSetForeground(dpy, wk->gc, XcPixelValue(wk, GKS_FOREGROUND_INDEX));
	XSetBackground(dpy, wk->gc, XcPixelValue(wk, GKS_BACKGROUND_INDEX));

	/* Set interactive cursors. */
	wk->locateCursor = XCreateFontCursor( dpy, XC_crosshair);
	wk->strokeCursor = XCreateFontCursor( dpy, XC_pencil);
	wk->valueCursor = XCreateFontCursor( dpy, XC_top_left_arrow);
	wk->choiceCursor = XCreateFontCursor( dpy, XC_hand1);
	wk->pickCursor = XCreateFontCursor( dpy, XC_top_left_arrow);
	wk->stringCursor = XCreateFontCursor( dpy, XC_xterm);
	wk->idleCursor = xswa.cursor;

	/* Set interactive beeping.  It's off by default. */
	wk->beep_on = 0;//BoolResource(name, "beep", "Beep", rDB) == 1;

	/*
	 * Initialize last-clipping rectangles to absurd values.  This
	 * will cause actual clipping window to be set.
	 */
	wk->last_pline_rectangle.x = 0;
	wk->last_pline_rectangle.y = 0;
	wk->last_pline_rectangle.width = 0;
	wk->last_pline_rectangle.height = 0;
	wk->last_pmarker_rectangle = wk->last_pline_rectangle;
	wk->last_farea_rectangle = wk->last_pline_rectangle;
	wk->last_text_rectangle = wk->last_pline_rectangle;

	wk->last_dash_index = 1;

	/* Set soft-clipping if appropriate.  It's off by default. */
	wk->soft_clipping_on = 0;//BoolResource(name, "softclipping",
	//				    "Softclipping", rDB) == 1;

	/* Save the setting of backing-store. */
	wk->backing_store_on	= xswa.backing_store == Always;
    }

    return status;
}


/*
 * Create a graphics-context for a window.
 */
    static void
CreateGC(dpy, win, wk)
    Display        *dpy;
    Window          win;
    WS_STATE_PTR    wk;
{
    wk->gc = XCreateGC(dpy, win, (unsigned long) 0,
		       (XGCValues *) NULL);
    wk->plinegc = XCreateGC(dpy, win, (unsigned long) 0,
			    (XGCValues *) NULL);
    wk->pmarkgc = XCreateGC(dpy, win, (unsigned long) 0,
			    (XGCValues *) NULL);
    wk->fillareagc = XCreateGC(dpy, win, (unsigned long) 0,
			       (XGCValues *) NULL);
    wk->textgc = XCreateGC(dpy, win, (unsigned long) 0,
			   (XGCValues *) NULL);
}


/*
 * Update the open-workstation table by saving the window-identifier.
 *
 * I don't like the fact that this routine assumes that the appropriate
 * slot exists -- SRE.
 */
    static void
UpdateOpenWSTable(wk)
    WS_STATE_PTR    wk;
{
    int             i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (wk->ws_id == xgks_state.openedws[i].ws_id)
	    break;
    xgks_state.openedws[i].win = wk->win;
}


/*
 * Get window geometry defaults.
 */
    static void
GetWindowGeometry(name, dpy, rDB, SizeHints)
    char           *name;		/* program name */
    Display        *dpy;
    XrmDatabase     rDB;		/* resource database */
    XSizeHints     *SizeHints;		/* window-size hints */
{
    char            buf[1024];
    char           *str_type[20];
    XrmValue        value;

    SizeHints->flags = 0;

    /*
     * Set to program-specified values and then override with any
     * user-specified values.
     */
    SizeHints->width = 640;
    SizeHints->height = 512;
    SizeHints->x = (DisplayWidth(dpy, DefaultScreen(dpy)) -
		    SizeHints->width) >> 1;
    SizeHints->y = (DisplayHeight(dpy, DefaultScreen(dpy)) -
		    SizeHints->height) >> 1;
    SizeHints->flags |= PSize | PPosition;
    return;
    if (XrmGetResource(rDB, strcat(strcpy(buf, name), ".geometry"),
		       "Xgks.Geometry", str_type, &value) == True) {

	int             x, y;
	long            flags;
	unsigned        width, height;

	(void) strncpy(buf, value.addr, (int) value.size);

	flags = XParseGeometry(buf, &x, &y, &width, &height);

	if (WidthValue & flags && HeightValue & flags &&
	     width >= 1 && width <= DisplayWidth(dpy, DefaultScreen(dpy)) &&
	  height >= 1 && height <= DisplayHeight(dpy, DefaultScreen(dpy))) {

	    SizeHints->width = width;
	    SizeHints->height = height;
	    SizeHints->flags |= USSize;
	}
	if (XValue & flags && YValue & flags) {
	    if (XNegative & flags)
		x += DisplayWidth(dpy, DefaultScreen(dpy)) -
		    SizeHints->width;
	    if (YNegative & flags)
		y += DisplayHeight(dpy, DefaultScreen(dpy)) -
		    SizeHints->height;
	    SizeHints->x = x;
	    SizeHints->y = y;
	    SizeHints->flags |= USPosition;
	}
    }
}


    static int
setColor(wk, name, index)
    WS_STATE_PTR    wk;			/* workstation structure */
    char           *name;		/* color name */
    Gint	    index;		/* GKS colour index */
{
    int		    status = 0;		/* failure */
    XColor          Xrep;
    if (XParseColor(wk->dpy, wk->clmp, name, &Xrep)) {
	Gcobundl        GKSrep;
	GKSrep.red   = (double)Xrep.red   / 65535.0;
	GKSrep.green = (double)Xrep.green / 65535.0;
	GKSrep.blue  = (double)Xrep.blue  / 65535.0;
	if (XcSetColour(wk, index, &GKSrep))
	{
	    status = 1;			/* success */
	}
	else
	{
	    fprintf(stderr, "setColor(): Couldn't set color %s.\n", name);
	}
    }
    else
    {
	(void)fprintf(stderr, "setColor(): Color %s not known.\n", name);
    }
    return status;
}

    static void
setGroundColor(wk, name, rDB, specName, className, defaultName, index)
    WS_STATE_PTR    wk;			/* workstation structure */
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    char	   *specName;
    char	   *className;
    char	   *defaultName;
    Gint	    index;		/* GKS colour index */
{
  char            buf[1024],*b;
  char           *str_type[20];
  XrmValue        value={0,NULL};
  int i;
  XrmName names[101];
  XrmClass classes[101];
  XrmRepresentation fromType;
  
    /*
     * Set XGKS background color.
     */
  (void)setColor(wk, defaultName, index);
  return;

  bzero(buf,1024);
    for(i=0;i<20;i++)str_type[i]=NULL;
    strncpy(buf, name,1023);
    strncat(buf,specName,1023);
    b=buf;
    XrmStringToNameList(b,names);
    XrmStringToClassList(className,classes);
    //   if (XrmGetResource(rDB, b, className, str_type, &value) == False)
    if(XrmQGetResource(rDB, names, classes, &fromType, &value)==False)
    {
      (void)setColor(wk, defaultName, index);
    }
    else
      {
	(void) strncpy(buf,XrmQuarkToString(fromType),1023);
	printf("%s %s\n",buf,value.addr);
	//(void) strncpy(buf, value.addr, (int) value.size);
	//buf[value.size] = 0;
	if (!setColor(wk, buf, index))
	  (void)setColor(wk, defaultName, index);
      }
}


/*
 * Get foreground and background color defaults.
 */
    static void
AllocGroundColors(wk, name, rDB, fg, bg)
    WS_STATE_PTR    wk;			/* workstation structure */
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    Gint           *fg, *bg;		/* fore/back-ground indexes */
{
    char            buf[1024];
    char           *str_type[20];
    XrmValue        value;

    setGroundColor(wk, name, rDB, ".background", "Xgks.Background", "black", 
	GKS_BACKGROUND_INDEX);
    setGroundColor(wk, name, rDB, ".foreground", "Xgks.Foreground", "white",
	GKS_FOREGROUND_INDEX);

    //    if (wk->colorCount == 2 && (
    //	BoolResource(name, "invertmono", "Invertmono", rDB) == 1 ||
    //	BoolResource(name, "reverse", "Reverse", rDB) == 1))
    //{
    //   *fg = XcPixelValue(wk, GKS_BACKGROUND_INDEX);
    //  *bg = XcPixelValue(wk, GKS_FOREGROUND_INDEX);
    //} else {
    *bg = XcPixelValue(wk, GKS_BACKGROUND_INDEX);
    *fg = XcPixelValue(wk, GKS_FOREGROUND_INDEX);
    //}
}


/*
 * Get window-manager hints.
 */
    static void
GetWMHints(dpy, name, rDB, WMHints)
    Display        *dpy;		/* display */
    char           *name;		/* program name */
    XrmDatabase     rDB;		/* resource database */
    XWMHints       *WMHints;		/* window-manager hints */
{
    char            buf[1024];
    char           *str_type[20];
    XrmValue        value;

    WMHints->flags = 0;

    WMHints->input = True;
    WMHints->flags |= InputHint;

    WMHints->initial_state = NormalState;
    //	BoolResource(name, "iconic", "Iconic", rDB) == 1 ? IconicState
    //	: NormalState;
    WMHints->flags |= StateHint;
    return;
    if (XrmGetResource(rDB, strcat(strcpy(buf, name), ".icon.geometry"),
		       "Xgks.Icon.Geometry", str_type, &value) == True) {

	int             x, y;
	long            flags;
	unsigned        width, height;

	(void) strncpy(buf, value.addr, (int) value.size);

	flags = XParseGeometry(buf, &x, &y, &width, &height);

	if (XValue & flags && YValue & flags) {
	    if (XNegative & flags && !((WidthValue & flags)) ||
		    YNegative & flags && !((HeightValue & flags))) {

		(void) fprintf(stderr, "%s%s\n",
			       "GetWMHints: Negative X (Y) icon position ",
			       "requires height (width) spec.");
	    } else {
		if (XNegative & flags)
		    x += DisplayWidth(dpy, DefaultScreen(dpy)) - width;
		if (YNegative & flags)
		    y += DisplayHeight(dpy, DefaultScreen(dpy)) - height;

		WMHints->icon_x = x;
		WMHints->icon_y = y;
		WMHints->flags |= IconPositionHint;
	    }
	}
    }
}


/*
 * Initialize our local resource manager.  Taken from "X11R4/contrib/
 * examples/OReilly/Xlib/basecalc/basecalc.c".
 */
    static
InitDefaults(name, dpy, rDB)
    char           *name;		/* name of application */
    Display        *dpy;
    XrmDatabase    *rDB;		/* resource database */
{
    int             status = 1;		/* routine status = success */

    /* So we can use the resource manager data-merging functions */
    XrmInitialize();

    /* Clear resource database */
    *rDB = XrmGetStringDatabase("");
    
    /* Get server defaults, program defaults, and .Xdefaults and merge them */
    (void) GetUsersDatabase(name, rDB, dpy);

    return status;
}


/*
 * Merge X defaults from an application defaults file specified in terms of
 * a directory and a program class.
 */
    static void
GetAppDefaults(dir, class, rDB)
    const char	*dir;
    const char	*class;
    XrmDatabase	*rDB;
{
    char	path[1024];
    
    (void) strcpy(path, dir);
    (void) strcat(path, "/");
    (void) strcat(path, class);
    *rDB=XrmGetFileDatabase(path);
      //(void) XrmMergeDatabases(XrmGetFileDatabase(path), rDB);
}


/*
 * Get program's and user's defaults
 *
 * NB: Document any changes to the following algorithm in xgks.3.
 */
    static
GetUsersDatabase(prog, rDB, dpy)
    char           *prog;
    XrmDatabase    *rDB;		/* resource database */
    Display        *dpy;
{
    int             status = 1;		/* routine status = success */
    char            pathname[1024];

    /*
     * Cannonical application defaults file directory:
     */
    GetAppDefaults("/etc/X11/app-defaults", prog, rDB);

    /*
     * Environment variable defaults file directory:
     */
    {
	char	   *appresdir = getenv("XAPPLRESDIR");

	if (appresdir != NULL)
	    GetAppDefaults(appresdir, prog, rDB);
    }

    /*
     * MERGE server defaults, these are canonically created by xrdb, loaded 
     * as a property of the root window when the server initializes.  If not 
     * defined, then use the resources specified in ~/.Xdefaults.
     */
    {
	XrmDatabase     serverDB;

	char	*prop	= XResourceManagerString(dpy);

	if (prop != NULL) {
	    /*
	     * Use X-server defaults.
	     */
	    serverDB = XrmGetStringDatabase(prop);
	} else {
	    /*
	     * Read ~/.Xdefaults file.
	     */
	    (void) GetHomeDir(pathname);
	    (void) strcat(pathname, "/.Xdefaults");
	    serverDB = XrmGetFileDatabase(pathname);
	}

	XrmMergeDatabases(serverDB, rDB);
    }

    /*
     * Open XENVIRONMENT file, or if not defined, the
     * ~/.Xdefaults-<hostname>, and merge into existing data base
     */
    {
	XrmDatabase     homeDB;
	char           *user_server = getenv("XENVIRONMENT");

	if (user_server == NULL) {
	    char	*cp;

	    user_server = GetHomeDir(pathname);
	    (void) strcat(user_server, "/.Xdefaults-");
	    if ((cp = strchr(user_server, ':')) != NULL)
		*cp	= 0;
	}
	homeDB = XrmGetFileDatabase(user_server);
	XrmMergeDatabases(homeDB, rDB);
    }

    return status;
}


/*
 * Get the path of the user's home directory.
 */
    static char*
GetHomeDir(dest)
    char           *dest;
{
    uid_t           uid;
    struct passwd  *pw;
    register char  *ptr;

    if ((ptr = getenv("HOME")) != NULL) {
	(void) strcpy(dest, ptr);

    } else {
	if ((ptr = getenv("USER")) != NULL) {
	    pw = getpwnam(ptr);
	} else {
	    uid = getuid();
	    pw = getpwuid(uid);
	}
	if (pw) {
	    (void) strcpy(dest, pw->pw_dir);
	} else {
	    *dest = '\0';
	}
    }
    return dest;
}


/*
 *  xXgksClearWs(wk) --- clear the corresponding x-window
 */
xXgksClearWs(wk)
    WS_STATE_PTR    wk;
{
    if (wk->ewstype != X_WIN)
	return OK;

    (void) XgksSIGIO_OFF(wk->dpy);
    XClearArea(wk->dpy, wk->win, 0, 0, 0, 0, False);
    XSync(wk->dpy, 0);
    (void) XgksSIGIO_ON(wk->dpy);

    return OK;
}


/*
 * xXgksCloseWs(ws) --- close the corresponding x-window
 */
xXgksCloseWs(ws)
    WS_STATE_PTR    ws;
{
    if (ws->ewstype != X_WIN)
	return OK;

    (void) XgksSIGIO_OFF(ws->dpy);
    XUnmapWindow(ws->dpy, ws->win);
    XDestroyWindow(ws->dpy, ws->win);
    XFreeGC(ws->dpy, ws->gc);
    XSync(ws->dpy, 0);
    (void) XcEnd(ws);				/* free color-index
						 * mapping-thingy */
    (void) XgksSIGIO_ON(ws->dpy);

    return OK;
}


/*
 * xXgksHighLight(ws, bd) --- heighlight a primitive
 */
xXgksHighLight(ws, bd)
    Gpoint          bd[5];
    WS_STATE_PTR    ws;
{
    Display        *dpy;
    Window          win;
    GC              gc;

    XPoint          box[5];
    int             i;

    dpy = ws->dpy;
    win = ws->win;
    gc = ws->gc;

    if (ws->ewstype != X_WIN)
	return OK;

    (void) XgksSIGIO_OFF(ws->dpy);
    XSetFunction(dpy, gc, GXinvert);
    XSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinMiter);

    XSetFillStyle(dpy, gc, FillSolid);

    for (i = 0; i < 5; i++)
	NdcToX(ws, &bd[i], &box[i]);		/* compound-statement macro */

    XDrawLines(dpy, win, gc, box, 5, CoordModeOrigin);

    XFlush(ws->dpy);

    XSetFunction(dpy, gc, GXcopy);
    (void) XgksSIGIO_ON(ws->dpy);

    return OK;
}

/*--------------------------------------------------------
-- xXgksWidgetCloseWs(gw, ws)                           --
--              gw      : XgksWidget instance           --
--              ws      : workstation identifier        --
--                                                      --
--      Free any X related stuff created by XGKS.       --
--      Don't destroy the window here, let Xt do that.  --
--------------------------------------------------------*/
int
xXgksWidgetCloseWs(gw, ws)
        XgksWidget      gw;
        WS_STATE_PTR    ws;
{
        if (ws->ewstype != X_WIN)
                return OK;

        XFreeGC(ws->dpy, ws->gc);
        XSync(ws->dpy, 0);
        (void) XcEnd(ws);               /* free color-index * mapping-thingy */

        return OK;
}
