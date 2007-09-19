/*=================================================================
===								===
=== FILE	: open_ws.c					===
===								===
=== PART OF	: XgksWidget v2.4				===
===								===
=== CONTENTS	: Routines for creating and destroying XGKS	===
===		  workstations, in a widget.			===
===		  This is a modified version of the file	===
===			$XGKS/lib/src/open_ws.c			===
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
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL    61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 *
 * gopenws - functions for opening a new workstation
 *            gopenws()
 *            gclosews()
 */

/*LINTLIBRARY*/

#include <string.h>
#include <stdlib.h>
#include "gks_implem.h"
#include <X11/IntrinsicP.h>
#include "XgksWidgetP.h"

#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: open_ws.c,v 1.3 2006/05/01 22:08:09 davidive Exp $";
#endif

extern void		XgksDelAllMoSeg();
extern void   gerrorhand(),gflushevents(),XgksDeleteAllSeg(),XgksIDevDelete(),
  XgksInitWssTrans(),XgksInitIDev(),XgksInitWssText(),XgksInitWssFillArea(),
  XgksInitWssPmarkers(),XgksInitWssPlines();
extern int xXgksWidgetOpenWs(), xXgksWidgetCloseWs();

/*
 * XgksOneOpenWs ()
 *          return TRUE if there is at least one open ws
 *          else return FALSE
 */
    static int
XgksOneOpenWs()
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++) {
	if (xgks_state.openedws[i].ws_id != INVALID)
	    return TRUE;
    }
    return FALSE;
}


/*
 * XgksDeleteOpenWs (ws_id)
 *   Gint ws_id;
 *
 * delete <ws_id> from openedws[] in gks state list by setting corresponding 
 * entry to INVALID
 */
    static void 
XgksDeleteOpenWs(ws_id)
    Gint            ws_id;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++) {
	if (xgks_state.openedws[i].ws_id == ws_id) {
	    xgks_state.openedws[i].ws_id = INVALID;
	    xgks_state.openedws[i].win = INVALID;
	    xgks_state.openedws[i].ws = NULL;
	    return;
	}
    }
}


/*
 * XgksSetWsPtr (ws_id, ws)
 *  Gint    ws_id;
 *  WS_STATE_PTR  ws;
 *
 * set the entry in openedws[].ws_id = ws_id  of gkslist to corresponding ptr
 */
    static int
XgksSetWsPtr(ws_id, ws)
    Gint            ws_id;
    WS_STATE_PTR    ws;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (xgks_state.openedws[i].ws_id == ws_id) {
	    xgks_state.openedws[i].ws = ws;
	    return OK;
	}
    return INVALID;
}


/*
 * XgksAllocNewWs (ws_id)
 *  Gint  ws_id;
 *
 * tries to allocate a slot in openedws[] in gks state list and assign ws_id 
 * into the slot
 *
 * return INVALID if there's no empty slot
 */
    static int
XgksAllocNewWs(ws_id)
    Gint            ws_id;
{
    Gint            i;

    for (i = 0; i < MAX_OPEN_WS; i++)
	if (xgks_state.openedws[i].ws_id == INVALID) {
	    xgks_state.openedws[i].ws_id = ws_id;
	    return OK;
	}
    return INVALID;
}


/* WS_STATE_PTR
 * XgksNewWs ()
 *
 *   Allocate a new ws entry
 */
    static WS_STATE_PTR 
XgksNewWs()
{

    WS_STATE_PTR    new;

    if ((new = (WS_STATE_PTR) malloc(sizeof(WS_STATE_ENTRY))) 
	    != NULL) {
	new->ws_id = 0;
	new->dpy = NULL;
	new->wsstate = GINACTIVE;
	new->ws_is_closing = FALSE;
	new->conn = NULL;
	new->mf.any = NULL;
	new->clip = xgks_state.cliprec.rec;
	new->primi_list.pid = CLIP_REC;
	new->primi_list.seg_cnt = 0;
	new->primi_list.primi.clip.rec = xgks_state.cliprec.rec;
	new->primi_list.primi.clip.segment = FALSE;
	new->primi_list.next = NULL;
	new->primi_insert_pt = &(new->primi_list);
	new->message_pt = NULL;			/* Initialize message ptr
						 * PTR# c1001 */
	new->seglist = NULL;
	new->seg_insertpt = NULL;
	new->seg_list_dirty = FALSE;
	new->size.x = WS_MAX_DCX;
	new->size.y = WS_MAX_DCY;
	new->primi_store = GYES;
	new->redrawfuncp = NULL;
	(void) XcNew(new);			/* birth color-index
						 * mapping-thingy */
    }
    return new;
}

 
/*--------------------------------------------------------
-- XgksWidgetOpenWs(gw, ws_id, connection, ws_type)	--
--		ws_id	: workstation identifier.	--
--	   connection	: Not implemented YET !!	--
--		ws_type	: Which workstation type.	--
--							--
--	The GKS function OPEN WORKSTATION.		--
--							--
--	Returns: 0, 8, 20, 21, 22, 23, 24, 26, 28, 42	--
--							--
--	See also: ANSI standard p.74			--
--------------------------------------------------------*/
Gint
XgksWidgetOpenWs(gw, ws_id, connection, ws_type)
	XgksWidget	gw;
	Gint		ws_id;
	Gchar		*connection;
	Gchar		*ws_type;
{
	/* declare internal functions */
  /*	void		XgksDeleteOpenWs();
			XgksSetWsPtr();
			XgksAllocNewWs();
	WS_STATE_PTR	XgksNewWs();
  */
	WS_STATE_PTR		ws;		/* workstation state list */
	EWSTYPE			ewstype;	/* corresponding ws_type */
	char			*getenv();
	int			status;


	/* check proper state */
	GKSERROR((xgks_state.gks_state == GGKCL), 8, errgopenws);

	/* check for valid ws_id  ..  ws_id must be positive */
	GKSERROR((!VALID_WSID(ws_id)), 20, errgopenws);

	/* Check for valid connection */
	if (connection == (char *) NULL)
		connection = getenv("DISPLAY");
	if (connection == (char *) NULL)
	  connection = DisplayString(ws->dpy);
	GKSERROR((STRLEN(connection) == 0), 21, errgopenws);

	/*
	* Check valid ws_type.  If type==NULL type = connection.
	*/
	if (ws_type == (char *) NULL)
		ws_type = connection;

	GKSERROR((STRLEN(ws_type) == 0), 23, errgopenws);

	ewstype = XgksWsTypeToEnum(ws_type);
	GKSERROR((ewstype == WST_INVALID), 23, errgopenws);

	/* check workstation already open */
	GKSERROR((OPEN_WSID(ws_id) != NULL), 24, errgopenws);

	/* update <openedws> array in gks_state list */
	GKSERROR((XgksAllocNewWs(ws_id) != 0), 42, errgopenws);

	/* create the new workstation list structure */
	GKSERROR((!(ws = XgksNewWs())), 300, errgopenws);

	ws->ws_id = ws_id;
	ws->wstype = (Gchar *) malloc((size_t) (STRLEN(ws_type) + 1));
	GKSERROR(ws->wstype == NULL, 300, errgopenws);
	STRCPY(ws->wstype, ws_type);
	ws->ewstype = ewstype;

	GKSERROR(((ws->conn = (Gchar *) malloc((size_t) (STRLEN(connection) 
			+ 1))) == NULL), 300, errgopenws);
	STRCPY((ws->conn), connection);

	/* add this ws ptr into openedws[] array */
	(void) XgksSetWsPtr(ws_id, ws);

	/* open the workstation as X-11 window */
	if ((status = xXgksWidgetOpenWs(gw,ws)) != 0)
	{
		ufree((voidp)ws->conn);
		ufree((voidp)ws);
		XgksDeleteOpenWs(ws_id);	/* update openedws[] array */
		(void) gerrorhand(status, errgopenws, xgks_state.gks_err_file);
		return status;
	}

	if (xgks_state.gks_state == GGKOP)	/* change state to 1 ws open */
		xgks_state.gks_state = GWSOP;

	XgksInitWssPlines(ws);		/* init POLYLINE BUNDLE TABLE */
	XgksInitWssPmarkers(ws);	/* POLYMARKER BUNDLE TABLE */
	(void) XgksInitWssFillArea(ws);	/* FILL AREA BUNDLE */
	XgksInitWssText(ws);		/* TEXT BUNDLE TABLE */

	/* Only meaningful for X_WIN */
	XgksInitIDev(ws);		/* initialize input devices */
	XgksInitWssTrans(ws);		/* workstation transformation */
	return OK;
}


/*--------------------------------------------------------
-- XgksWidgetCloseWs(gw, ws_id)				--
--		gw	: XgksWidget instance		--
--		ws_id	: Workstation to close		--
--							--
--	The GKS function CLOSE WORKSTATION.		--
--	Error 147 (EventQueue OverFlow) is not checked	--
--							--
--	Returns: 0, 7, 20, 25, 29, 147			--
--							--
--	See also: ANSI standard p.75			--
--------------------------------------------------------*/
Gint
XgksWidgetCloseWs(gw, ws_id)
	XgksWidget	gw;
	Gint		ws_id;
{
	/* declare internal functions */
  /*	void		XgksDeleteOpenWs();*/

	WS_STATE_PTR	ws;


	/* check gks in proper state */
	GKSERROR((xgks_state.gks_state==GGKOP || xgks_state.gks_state==GGKCL),
					7, errgclosews);

	/* check for invalid workstation id */
	GKSERROR((!VALID_WSID(ws_id)), 20, errgclosews);

	/* check for workstation not open yet */
	GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgclosews);

	/* check for workstation still active */
	GKSERROR((ws->wsstate == GACTIVE), 29, errgclosews);

	/*
	* delete all input devices associated with this ws
	* NOTE: This is what caused ptr c1120 (key presses as ws closes cause
	*       X error).  New events entered after the gflushevents but 
	*       before the ws was completely gone.  This left an event in the
	*       queue pointing to a non-existant ws.  Also - the devices on a
	*       workstation were never being handled when the workstation
	*       closed.  All the memory dynamically allocated for devices was
	*       being lost.  So... we call XgksIDevDelete to destroy the
	*       devices so no new events will enter and also free the memory
	*       used by the devices.  (XgksIDevDelete was written, but never
	*       called.  This seems to be the place it was meant for.) (DWO)
	*/
	XgksIDevDelete(ws);

	/* XgksDeleteAllSeg was moved after XgksIDevDelete to stop the devices
	* from flashing as the ws closes.  XgksDeleteAllSeg calls
	* XgksIDevDisable which erases all the devices, then deletes the
	* segments, then calls XgksIDevEnable to redraw all the devices.
	* This caused them to flash.  Now, the devices are deleted before the
	* segments.  Delete all segments from this ws, and at the same time
	* delete this ws from all segments associated with it
	* (XgksDelAllWsSeg would be a better name)
	*/
	XgksDeleteAllSeg(ws);

	/*
	* mark the ws as closing to prevent any interrupts to jump
	* in while the ws is in an unstable state.  (if a change
	* focus came in after the xXgksCloseWs, but before the
	* completion of gclosews, GKS used to try to inquire the
	* color map of an X window that was already gone)  DWO
	*/
	ws->ws_is_closing = TRUE;

	/* House keeping for workstation consistency */

	/* tell x-window system to destroy the window */
	(void) xXgksWidgetCloseWs(gw,ws);

	/* Flush the event queue for this workstation */
	(void) gflushevents(ws_id, GNCLASS, 0);

	/* take workstation out of list and free it up */
	/* remove ws from openedws array in gkslist */
	XgksDeleteOpenWs(ws_id);

	ufree((voidp)ws->conn);
	ufree((voidp)ws->wstype);
	ufree((voidp)ws);

	/* clean up stuff */
	if (XgksOneOpenWs() == FALSE)	/* that was the last workstation open */
	xgks_state.gks_state = GGKOP;	/* set state to gks open no ws open */

	/* everything went ok */
	return OK;
}
