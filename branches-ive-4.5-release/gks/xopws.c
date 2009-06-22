/*=================================================================
===								===
=== FILE	: xopws.c					===
===								===
=== PART OF	: XgksWidget v2.4				===
===								===
=== CONTENTS	: Routines for handling stuff at the Xlib level	===
===		  when creating/destroying an Xgks Widget.	===
===		  This is a modified version of the file	===
===			$XGKS/lib/src/x/xopws.c			===
===		  from version 2.4 of XGKS.			===
===								===
=== AUTHOR	: Modifications for XgksWidget by		===
===			Neil Bowers, University of Leeds.	===
===		  original authors, copyrights etc below.	===
===								===
=================================================================*/

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

#include <stdlib.h>
#include <sys/types.h>			/* for uid_t */
#include <unistd.h>			/* NeXTOS requires that this be */
					/* after <sys/types.h> */
#include <string.h>
#include <pwd.h>
#include <ctype.h>			/* for isupper() & tolower() */
#include "gks_implem.h"
#include <X11/Xresource.h>
#include <X11/IntrinsicP.h>
#include "XgksWidgetP.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: xopws.c,v 1.4 1997/10/15 19:21:43 harryive Exp $";
#endif

static int	InsureConn();
static int	CreateWindow();
static void	AllocGroundColors();
static void	CreateGC();
static void	UpdateOpenWSTable();
extern void XgksProcessEvents(),draw_line(),get_slice(),mouse_traj,
  XtMoveWidget();


/*--------------------------------------------------------
-- xXgksWidgetOpenWs(gw, wk)				--
--		gw	: XgksWidget instance		--
--		wk	: pointer to workstation struct	--
--							--
--	Creates the window and related X stuff.		--
--------------------------------------------------------*/
int
xXgksWidgetOpenWs(gw, wk)
	XgksWidget	gw;
	WS_STATE_PTR	wk;
{
	int		status;		/* success */


	/* Insure connection to display. */
	if (!(status = InsureConn(wk)))
	  {
	    
	    /* Flush all events from the widget's display connection */
	    XFlush(XtDisplay(gw));
	    
	    /* Create window -- using widget resources for defaults. */
	    if (!(status = CreateWindow(gw, wk)))
	      {
		XEvent          xev;	/* X event structure */
		XWindowAttributes	WinAtt;
		
		
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


/*--------------------------------------------------------
-- InsureConn(wk)					--
--		wk	: pointer to workstation struct	--
--							--
--	Insure a connection to the display.		--
--------------------------------------------------------*/
static int
InsureConn(wk)
	WS_STATE_PTR	wk;
{
	int		status = 0;		/* success */
	int		i;
	WS_STATE_PTR	wk_p = NULL;


	/* Check if Display wk->conn has been opened  */
	for (i = 0; i < MAX_OPEN_WS; i++)
	{
		if (xgks_state.openedws[i].ws_id >= 0 &&
				xgks_state.openedws[i].ws->ewstype == X_WIN)
		{
			WS_STATE_PTR    ws = xgks_state.openedws[i].ws;

			if ((STRCMP(wk->conn, ws->conn) == 0) && (ws != wk))
			{
				wk_p = ws;
				break;
			}
		}
	}

	if (wk_p != NULL)
	{	/* Has been opened */
		wk->dpy = wk_p->dpy;
		wk->dclmp = wk_p->dclmp;
		wk->wclmp = wk_p->dclmp;
	}
	else
	{	/* Open a new display */
		if ((wk->dpy = XOpenDisplay(wk->conn)) == NULL)
			status = 26;
		else
		{
			char	*ptr = DisplayString(wk->dpy);

/*dbw fix for DECstation - remove free and only allocate if wk-conn is NULL */
/* 			ufree((voidp)wk->conn); */
			if (wk->conn == NULL) {
			    if ((wk->conn =
				 (char *) malloc((size_t) (STRLEN(ptr) + 1)))
				== NULL)
				status = 300;
			    else
			    {
				STRCPY(wk->conn, ptr);
			    }
			}

			/* Set the screen default colour map ID. */
			wk->dclmp = DefaultColormap(wk->dpy,
						    DefaultScreen(wk->dpy));
			wk->wclmp = wk->dclmp;

			XSelectInput(wk->dpy,
				     DefaultRootWindow(wk->dpy),
				     0);
		}
	}
	return status;
}


/*--------------------------------------------------------
-- CreateWindow(gw, wk)					--
--		gw	: XgksWidget instance		--
--		wk	: pointer to workstation struct	--
--							--
--	Create an XGKS window --			--
--	with all its associated attributes.		--
--------------------------------------------------------*/
static int
CreateWindow(gw, wk)
	XgksWidget	gw;
	WS_STATE_PTR	wk;		/* workstation structure */
{
	int			status		= 0;
	int			NumMatched;
	Display			*dpy		= wk->dpy;
	XSizeHints		SizeHints;
	XSetWindowAttributes	xswa;
	XVisualInfo		VisualTemplate;
	XVisualInfo		*VisualList;


	/* Initialize color-mapping. */
	wk->wscolour = DisplayCells(dpy, DefaultScreen(dpy));

	/* size of colour map */
	VisualTemplate.screen = DefaultScreen(dpy);
	VisualTemplate.visualid = DefaultVisual(dpy,
					DefaultScreen(dpy))->visualid;
	if ((VisualList = XGetVisualInfo(dpy, VisualScreenMask | VisualIDMask,
			&VisualTemplate, &NumMatched)) == NULL ||
			!XcInit(wk, VisualList))
		status = 26;
	else
	{
		/*
		* Get foreground and background colors (and set them in the
		* colormap).
		*/
		(void) AllocGroundColors(gw, wk, &wk->wsfg, &wk->wsbg);

		/*
		* Init pointer to table of set values to NULL.  The table will
		* be alloc'ed and init'ed in gsetcolourrep on the 1st call(DWO)
		* 
		* This may no longer be meaningful since I've changed the way
		* the mapping from GKS color-index to X-colorcell-index is
		* handled.  --SRE 2/1/90
		*/
		wk->set_colour_rep = (Gcobundl *) NULL;

		/* Set the window-event mask. */
		wk->event_mask = StructureNotifyMask | ExposureMask | 
				KeyPressMask | ButtonPressMask | OwnerGrabButtonMask |
				ButtonReleaseMask | ButtonMotionMask;

		/*
		* Set the window-attributes structure.
		*/
		xswa.event_mask = StructureNotifyMask | ExposureMask;
		xswa.background_pixel = XcPixelValue(wk, wk->wsbg);
		xswa.border_pixel = XcPixelValue(wk, wk->wsfg);
		xswa.do_not_propagate_mask = 0L/*wk->event_mask &
			(KeyPressMask | KeyReleaseMask | ButtonPressMask |
			ButtonReleaseMask | PointerMotionMask |
			Button1MotionMask | Button2MotionMask |
			Button3MotionMask | Button4MotionMask |
			Button5MotionMask | ButtonMotionMask)*/;
		xswa.colormap = wk->dclmp;	/* default colormap --SRE */
		if (gw->xgks.backing_store) {
		    xswa.backing_store = Always;
		    XChangeWindowAttributes(dpy, gw->core.parent->core.window,
					    CWBackingStore, &xswa);
		}
		else xswa.backing_store = NotUseful;
		/* Create the window. */
		wk->win = XCreateWindow(dpy,
				gw->core.parent->core.window,
				gw->core.x,	gw->core.y,
				gw->core.width,	gw->core.height,
				gw->core.border_width,
				DefaultDepth(dpy, DefaultScreen(dpy)),
				InputOutput,
				DefaultVisual(dpy, DefaultScreen(dpy)),
				(unsigned long)(CWDontPropagate | CWBackPixel |
					CWBorderPixel | CWEventMask |
					CWColormap | CWBackingStore),
				&xswa);
		if (wk->win == False)
			status = 26;
		else
		{
			Window		win = wk->win;


			/* Create graphics-context for window. */
			(void) CreateGC(dpy, win, wk);

			/* Set foreground and background colors in GC. */
			XSetForeground(dpy, wk->gc, XcPixelValue(wk,wk->wsfg));
			XSetBackground(dpy, wk->gc, XcPixelValue(wk,wk->wsbg));

			/*
			* Initialize last-clipping rectangles to absurd values.
			* This will cause actual clipping window to be set.
			*/
			wk->last_pline_rectangle.x = 0;
			wk->last_pline_rectangle.y = 0;
			wk->last_pline_rectangle.width = 0;
			wk->last_pline_rectangle.height = 0;
			wk->last_pmarker_rectangle = wk->last_pline_rectangle;
			wk->last_farea_rectangle = wk->last_pline_rectangle;
			wk->last_text_rectangle = wk->last_pline_rectangle;

			wk->last_dash_index = 1;

			/* Set soft-clipping from widget resource. */
			wk->soft_clipping_on = gw->xgks.soft_clipping;

			/* Save the setting of backing-store. */
			wk->backing_store_on = (xswa.backing_store == Always);
		}
	}

	if (VisualList != NULL)
		XFree((char *) VisualList);

	return status;
}


/*--------------------------------------------------------
-- CreateGC(dpy, win, wk)				--
--		dpy	: display pointer		--
--		win	: X window id for workstation	--
--		wk	: pointer to workstation struct	--
--							--
--	Create a GC for a (GKS) workstation.		--
--------------------------------------------------------*/
static void
CreateGC(dpy, win, wk)
	Display		*dpy;
	Window		win;
	WS_STATE_PTR	wk;
{
	wk->gc = XCreateGC(dpy,win,(unsigned long)0,(XGCValues *)NULL);
	wk->plinegc = XCreateGC(dpy,win,(unsigned long)0,(XGCValues *)NULL);
	wk->pmarkgc = XCreateGC(dpy,win,(unsigned long)0,(XGCValues *)NULL);
	wk->fillareagc = XCreateGC(dpy,win,(unsigned long)0,(XGCValues *)NULL);
	wk->textgc = XCreateGC(dpy,win,(unsigned long)0,(XGCValues *)NULL);
}


/*--------------------------------------------------------
-- UpdateOpenWSTable(wk)				--
--		wk	: pointer to workstation struct	--
--							--
--	Update the open-workstation table by saving the --
--	window-identifier.				--
--							--
--	I don't like the fact that this routine assumes	--
--	that the appropriate slot exists -- SRE.	--
--------------------------------------------------------*/
static void
UpdateOpenWSTable(wk)
	WS_STATE_PTR	wk;
{
	int		i;


	for (i = 0; i < MAX_OPEN_WS; i++)
		if (wk->ws_id == xgks_state.openedws[i].ws_id)
			break;
	xgks_state.openedws[i].win = wk->win;
}


/*--------------------------------------------------------
-- AllocGroundColors(gw, wk, fg, bg)			--
--		gw	: XgksWidget instance		--
--		wk	: pointer to workstation struct	--
--		fg	: pointer for foreground index	--
--		bg	: pointer for background index	--
--							--
--	Get foreground and background color defaults.	--
--	This is now done through Xt resource handling.	--
--------------------------------------------------------*/
static void
AllocGroundColors(gw, wk, fg, bg)
	XgksWidget	gw;
	WS_STATE_PTR	wk;
	Gint		*fg;
	Gint		*bg;
{
	XColor		Xrep;
	Gcobundl	GKSrep;


	if (gw->xgks.invert_mono)
	{
		*fg = 0;
		*bg = 1;
	}
	else
	{
		*fg = 1;
		*bg = 0;
	}

	/*
	* Set XGKS background color.
	*/

	Xrep.pixel = gw->core.background_pixel;
	XQueryColor(wk->dpy,
		DefaultColormap(wk->dpy,DefaultScreen(wk->dpy)),
		&Xrep);

	GKSrep.red	= (double)Xrep.red	/ 65535.0;
	GKSrep.green	= (double)Xrep.green	/ 65535.0;
	GKSrep.blue	= (double)Xrep.blue	/ 65535.0;

	(void) XcSetColour(wk, (Gint) 0, &GKSrep);

	/*
	* Set XGKS foreground color.
	*/

	Xrep.pixel = gw->xgks.foreground;
	XQueryColor(wk->dpy,
		DefaultColormap(wk->dpy,DefaultScreen(wk->dpy)),
		&Xrep);

	GKSrep.red	= (double)Xrep.red	/ 65535.0;
	GKSrep.green	= (double)Xrep.green	/ 65535.0;
	GKSrep.blue	= (double)Xrep.blue	/ 65535.0;

	(void) XcSetColour(wk, (Gint) 1, &GKSrep);
}


/*--------------------------------------------------------
-- xXgksWidgetCloseWs(gw, ws)				--
--		gw	: XgksWidget instance		--
--		ws	: workstation identifier	--
--							--
--	Free any X related stuff created by XGKS.	--
--	Don't destroy the window here, let Xt do that.	--
--------------------------------------------------------*/
int
xXgksWidgetCloseWs(gw, ws)
	XgksWidget	gw;
	WS_STATE_PTR	ws;
{
	if (ws->ewstype != X_WIN)
		return OK;

	XFreeGC(ws->dpy, ws->gc);
	XSync(ws->dpy, 0);
	(void) XcEnd(ws);		/* free color-index * mapping-thingy */

	return OK;
}
