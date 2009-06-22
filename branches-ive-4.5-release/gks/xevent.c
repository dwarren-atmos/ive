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
 *  XGKS -- Xevent interrupt handeling and process routines
 *
 * $Log: xevent.c,v $
 * Revision 1.24  2005/09/06 21:52:36  warren
 * fixed popups on linux
 *
 * Revision 1.23  2002/08/09 19:57:39  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.22  2000/07/20 18:55:49  harryive
 * Make sure gksokbox is non-NULL before deleting it.
 *
 * Revision 1.21  1998/12/31 16:01:21  warren
 * First cut of new trajectory look and feel. Trajectories are no longer
 * on the popup menu, just plot type.
 *
 * Revision 1.20  1997/10/22 19:34:47  davidive
 * Turned the particle movement part of trajectories into a user transform
 * and added unit checking to the default transform.
 *
 * Revision 1.19  1997/10/15 19:22:54  harry
 * OOPS - Take out automatic debugging.
 *
 * Revision 1.18  1997/10/15 19:21:42  harryive
 * Minor fix for backing store.
 *
 * Revision 1.17  1997/07/30 16:46:28  davidive
 * Fix gks window - pop front and back with control/mouse buttons
 * Rev up to 4.1.0 Released version
 *
 * Revision 1.16  1997/05/30 18:04:34  harry
 * Fix mouse slicing to auto-cancel when plot is changed.
 *
 * Revision 1.15  1997/05/08 20:04:37  harry
 * Make mouse slice act like mouse line drawing.
 *
 * Revision 1.14  1997/05/06 23:26:33  harry
 * Fixes to mouse slicing line drawing.
 *
 * Revision 1.13  1997/04/25 21:59:28  warren
 * Full changed to Unzoom where Unzoom goes up one.
 *
 * Revision 1.12  1996/12/12 19:37:17  warren
 * First crack at line on plot. Known to work on contour and line plots.
 *
 * Revision 1.11  1996/11/07 16:54:38  warren
 * Added a full option to allow windowing out on mouse slices.
 *
 * Revision 1.10  1996/10/23 15:49:05  harry
 * Fixes for picky SGI compiler.
 *
 */

/*LINTLIBRARY*/


#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include "gks_implem.h"
#include <Xm/XmAll.h>
#include <ive_for.h>    

#ifndef lint
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: xevent.c,v 1.24 2005/09/06 21:52:36 warren Exp $";
#endif
static void mouseProcessEvents();
#define sq(x) (x) * (x)

/*0=noive_gks_control 1=ive_gks_control p or n*/
int ive_gks_control=0;

/*
 * SIGIO response status: 
 *     0 => handling
 *    >0 => ignoring
 */
static int      SigCount = 0;
extern void draw_line(),get_slice(),mouse_traj(),XtMoveWidget(),
  XgksIDevDisable(), xXgksUpdateTrans(),XgksXReDrawWs(),XgksIDevEnable(),
  XgksIProcessXEvent();
/*
 * Do mouse stuff
 */
int gks_mouse_menu_tracker=0; /*0=nochoice, 1=slice, 2=window*/
static Widget gksokbox=NULL;
static struct {
    int x,y,ex,ey,dummy;
    Dimension height,width;
    Pixmap map;
    Window win;
    Display *dpy;
    int hor, vert, sandw;
}location={0,0,0,0,0,0,0,0,0,0,0,0,0};


void slice_menu_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    extern Widget xgks_widget;
    
    location.win=XtWindow(xgks_widget);
    location.dpy=XtDisplay(xgks_widget);
    XtVaGetValues(xgks_widget, XmNheight,&location.height, 
		  XmNwidth,&location.width, NULL);

    
    switch((unsigned long)data){
    case 0:
	location.hor = location.vert = location.sandw = 0;
	gks_mouse_menu_tracker=1;
	break;
    case 1:
	location.hor = 1;
	location.vert = location.sandw = 0;
	gks_mouse_menu_tracker=1;
	break;
    case 2:
	location.hor = location.sandw = 0;
	location.vert = 1;
	gks_mouse_menu_tracker=1;
	break;
    case 3:
	break;
    case 4: /*zoom*/
	location.hor = location.vert = location.sandw = 0;
	gks_mouse_menu_tracker=2;
	break;	
    case 5: /*unzoom*/
	driver_notwin("window=up")
	break;	
    case 6:
	location.hor = location.vert = location.sandw = 0;
	gks_mouse_menu_tracker=3;
	break;
    case 7:
	location.hor = 1;
	location.vert = location.sandw = 0;
	gks_mouse_menu_tracker=3;
	break;
    case 8:
	location.hor = location.sandw = 0;
	location.vert = 1;
	gks_mouse_menu_tracker=3;
	break;
    case 9:
	location.hor = location.sandw = location.vert = 0;
	gks_mouse_menu_tracker=4;
	break;
    case 10:
	location.hor = location.sandw = location.vert = 0;
	gks_mouse_menu_tracker=0;
	break;
    }
}

void gks_line_cancel(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    extern Widget xgks_widget;

    XtDestroyWidget(XtParent(w));
    gksokbox = NULL;
    XCopyArea(XtDisplay(xgks_widget), location.map, location.win,
	      DefaultGC(XtDisplay(xgks_widget), 
			DefaultScreen(XtDisplay(xgks_widget))),
	      0,0, location.width, location.height, 0,0);

    XSync(location.dpy,FALSE);

    location.x = location.ex = location.y = location.ey = 0.;
    gks_mouse_menu_tracker = 0;
    location.dummy = 0;
    if(location.map){
	XFreePixmap(XtDisplay(w),location.map);
	location.map = 0;
	XtRemoveEventHandler(xgks_widget,~NoEventMask,FALSE,
			     mouseProcessEvents,
			     NULL);
    }

}

void cancel_mouse_slicing_()

{
    if (gks_mouse_menu_tracker) {
	if (gksokbox)
	  gks_line_cancel(gksokbox, 0,(XmAnyCallbackStruct *) NULL);
	else gks_mouse_menu_tracker = 0;
    }
}

void gks_line_help(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    if (gks_mouse_menu_tracker == 1) {driver("help = draw_line");}
    else { if(gks_mouse_menu_tracker == 4){driver("help = start_traj");}
    else {driver("help = points");}}
}

void gks_line_ok(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    extern Widget xgks_widget;

    XtDestroyWidget(XtParent(w));
    gksokbox = NULL;

    if (gks_mouse_menu_tracker == 3) {

	XCopyArea(XtDisplay(xgks_widget), location.map, location.win,
		  DefaultGC(XtDisplay(xgks_widget), 
			    DefaultScreen(XtDisplay(xgks_widget))),
		  0,0, location.width, location.height, 0,0);

	(void)draw_line((float)location.x/(float)location.width,
			(float)location.y/(float)location.height,
			(float)location.ex/(float)location.width,
			(float)location.ey/(float)location.height);
    }
    else{
      (void)get_slice((float)location.x/(float)location.width,
		      (float)location.y/(float)location.height,
		      (float)location.ex/(float)location.width,
		      (float)location.ey/(float)location.height,0);
    }
    
    if(gks_mouse_menu_tracker != 4){
      location.x = location.ex = location.y = location.ey = 0.;
      gks_mouse_menu_tracker = 0;
      location.dummy = 0;
      if(location.map){
	XFreePixmap(XtDisplay(w),location.map);
	location.map = 0;
	XtRemoveEventHandler(xgks_widget,~NoEventMask,FALSE,
			     mouseProcessEvents,
			     NULL);
      }
    }
    
}

void gks_start_traj(x,y)
     int x,y;
{
  location.x = location.ex = x;
  location.y = location.ey = y;
  location.dummy=2;
  XDrawLine(location.dpy, location.win, 
	    DefaultGC(location.dpy, DefaultScreen(location.dpy)),
	    x+3,y+3,
	    x-3,y-3);
  XDrawLine(location.dpy, location.win, 
	    DefaultGC(location.dpy, DefaultScreen(location.dpy)),
	    x+3,y-3,
	    x-3,y+3);
  (void)mouse_traj((float)location.x/(float)location.width,
		   (float)location.y/(float)location.height);
  XSync(location.dpy, FALSE);
}



void gks_start_line(x,y)
     int x,y;
{
    Widget popup;
    XWindowAttributes	win_att;
    XmString str, NewString();
    extern Widget xgks_widget;
    int posx, posy, wid, hei;
    extern Widget Toplevel;

    if(location.map){
	XFreePixmap(location.dpy, location.map);
	location.map = 0;
    }
    location.map = 
      XCreatePixmap(location.dpy, 
		    location.win, location.width, location.height,
		    DefaultDepth(location.dpy, 
				 DefaultScreen(location.dpy)));
    
    XCopyArea(location.dpy, location.win, location.map, 
              DefaultGC(location.dpy, 
			DefaultScreen(location.dpy)),
              0, 0, location.width, location.height,  0, 0);
    XSync(location.dpy,FALSE);
    location.x = location.ex = x;
    location.y = location.ey = y;
    location.dummy=2;
    if (gks_mouse_menu_tracker == 1)
      popup = XmCreateDialogShell(Toplevel, "Slicedone", NULL, 0);
    else
      popup = XmCreateDialogShell(Toplevel, "Linedone", NULL, 0);
    if(gks_mouse_menu_tracker != 4){
      gksokbox=XmCreateMessageBox(popup,"Done",NULL,0);
      str = NewString("              Done");
      XtVaSetValues(gksokbox, XmNmessageString, str,NULL);
      XmStringFree(str);
      XtAddCallback(gksokbox, XmNcancelCallback,
		    (XtCallbackProc) gks_line_cancel, 0L);
      XtAddCallback(gksokbox, XmNhelpCallback, 
		    (XtCallbackProc)gks_line_help, 0L);
      XtAddCallback(gksokbox, XmNokCallback, 
		    (XtCallbackProc)gks_line_ok, 0L);
      XtVaGetValues(xgks_widget, XmNheight,&hei, XmNwidth, &wid,
      XmNx, &posx, XmNy, &posy, NULL);
/*    XtVaSetValues(popup, XmNx,posx-200, NULL);
    XtVaSetValues(gksokbox, XmNx,posx-200, NULL);
*/
      XGetWindowAttributes(location.dpy, location.win, &win_att);
      XtInsertEventHandler(xgks_widget,win_att.all_event_masks,
			   FALSE,mouseProcessEvents,
			   NULL,XtListHead);
      XtManageChild(gksokbox);
      XtManageChild(popup);
      XtMoveWidget(popup, 100,100);
    }
}

void gks_move_line(ws, x, y)
    WS_STATE_PTR ws;
     int x,y;
{
    extern Widget xgks_widget;
    XWindowAttributes	win_att;
    float d1, d2;
    int x1,x2,y1,y2;
    
    x1 = location.x<location.ex ? location.x:location.ex;
    y1 = location.y<location.ey ? location.y:location.ey;
    x2 = location.x>location.ex ? location.x:location.ex;
    y2 = location.y>location.ey ? location.y:location.ey;
    XCopyArea(ws->dpy, location.map, location.win,
	      DefaultGC(ws->dpy,DefaultScreen(ws->dpy)),
	      x1, y1, x2-x1+1, y2-y1+1, x1, y1);
    XSync(ws->dpy, FALSE);
	
    if(sq(location.x-x) + sq(location.y-y) <
       sq(location.ex-x) + sq(location.ey-y)){
	location.x = location.ex;
	location.y = location.ey;
    }
    if(location.vert)
	x = location.x;
    if(location.hor)
	y = location.y;
    location.ex = x;
    location.ey = y;
    XDrawLine(ws->dpy, location.win, 
	      DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
	      location.x,location.y,
	      location.ex,location.ey);
    XSync(ws->dpy, FALSE);
}

void gks_move_traj(ws, x, y)
    WS_STATE_PTR ws;
     int x,y;
{
    extern Widget xgks_widget;
    XWindowAttributes	win_att;
    float d1, d2;
    int x1,x2,y1,y2;
    
    XCopyArea(ws->dpy, location.map, location.win,
	      DefaultGC(ws->dpy,DefaultScreen(ws->dpy)),
	      location.x-6, location.y-6, 12, 12, location.x-6, location.y-6);
    XSync(ws->dpy, FALSE);
	
    location.x = x;
    location.y = y;

      XDrawLine(ws->dpy, location.win, 
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		location.x+3,location.y+3,
		location.x-3,location.y-3);
      XDrawLine(ws->dpy, location.win, 
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		location.x+3,location.y-3,
		location.x-3,location.y+3);
      (void)mouse_traj((float)location.x/(float)location.width,
		       (float)location.y/(float)location.height);
      XSync(ws->dpy, FALSE);
}

void gks_start_window(x,y)
     int x,y;
{
    extern Widget xgks_widget;

    XWindowAttributes	win_att;
    if(location.map){
	XFreePixmap(location.dpy, location.map);
	location.map = 0;
    }
    location.map = 
      XCreatePixmap(location.dpy, 
		    location.win, location.width, location.height,
		    DefaultDepth(location.dpy, 
				 DefaultScreen(location.dpy)));
    
    XCopyArea(location.dpy, location.win, location.map, 
              DefaultGC(location.dpy, 
			DefaultScreen(location.dpy)),
              0, 0, location.width, location.height,  0, 0);
    XSync(location.dpy,FALSE);
    location.x = location.ex = x,
    location.y = location.ey = y;
    location.dummy=0;
    XGetWindowAttributes(location.dpy, location.win, &win_att);
    XtInsertEventHandler(xgks_widget,win_att.all_event_masks,
			 FALSE,mouseProcessEvents,
			 NULL,XtListHead);
/*    while(gks_mouse_menu_tracker){
	mouseProcessEvents();
    }*/
}

static void special_button_motion(ws,event)
     WS_STATE_PTR    ws;
     XEvent *event;
{

    XButtonEvent *evb;
    int x1,y1,x2,y2;

    if(location.dummy == 1)return;
    if(!gks_mouse_menu_tracker)return;
    evb=(XButtonEvent *)event;
    if(!gks_mouse_menu_tracker)return;
    
    x1 = location.x<location.ex ? location.x:location.ex;
    y1 = location.y<location.ey ? location.y:location.ey;
    x2 = location.x>location.ex ? location.x:location.ex;
    y2 = location.y>location.ey ? location.y:location.ey;
    
    if(gks_mouse_menu_tracker==1 || gks_mouse_menu_tracker==3){
	if(x1)x1--;
	if(y1)y1--;
	x2++;
	y2++;
	
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, 
		  DefaultScreen(ws->dpy)),
		  x1, y1, x2  - x1, y2 - y1 , x1, y1);
	XSync(ws->dpy,FALSE);

	if(!location.vert)
	  location.ex=evb->x;
	if(!location.hor)
	  location.ey=evb->y;
	
	XDrawLine(ws->dpy, location.win, 
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  location.x,location.y,
		  location.ex,location.ey);
    }
    else if(gks_mouse_menu_tracker==2){
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y1, x2  - x1 + 1, 2, x1, y1);
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y1, 2, y2 - y1 + 1, x1, y1);
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y2, x2  - x1 + 1, 2, x1, y2);
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x2, y1, 2, y2 - y1 + 1, x2, y1);
	
	XSync(ws->dpy,FALSE);
	location.ex=evb->x;
	location.ey=evb->y;
	x1 = location.x<location.ex ? location.x:location.ex;
	y1 = location.y<location.ey ? location.y:location.ey;
	x2 = location.x>location.ex ? location.x:location.ex;
	y2 = location.y>location.ey ? location.y:location.ey;
	
		XDrawRectangle(ws->dpy, location.win,
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		x1, y1, x2-x1, y2-y1);
    }
    if(gks_mouse_menu_tracker==4){
      
      XCopyArea(ws->dpy, location.map, location.win,
		DefaultGC(ws->dpy,
			  DefaultScreen(ws->dpy)),
		location.x-6,location.y-6, (unsigned int)12, 
		(unsigned int)12, 
		location.x-6,location.y-6);


      XSync(ws->dpy,FALSE);
      location.x=evb->x;
      location.y=evb->y;
      
      
      XDrawLine(ws->dpy, location.win, 
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		location.x+3,location.y+3,
		location.x-3,location.y-3);
      XDrawLine(ws->dpy, location.win, 
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		location.x+3,location.y-3,
		location.x-3,location.y+3);
      /*      XCopyArea(XtDisplay(xgks_widget), location.map, location.win,
	      DefaultGC(XtDisplay(xgks_widget),
	      DefaultScreen(XtDisplay(xgks_widget))),
	      location.x-5,location.y-5, (unsigned int)10, 
	      (unsigned int)10, 
	      location.x-5,location.y-5);*/
      (void)mouse_traj((float)location.x/(float)location.width,
		       (float)location.y/(float)location.height);
      
    }
    XSync(ws->dpy,FALSE);
}

static void special_button_release(ws,event)
     WS_STATE_PTR    ws;
     XEvent *event;
{
    extern Widget xgks_widget;

    XButtonEvent *evb;
    int x1,y1,x2,y2;
    float realwidth, realheight, minwidth, minheight;
    float maxwidth, maxheight;
    if(location.dummy)return;
    evb=(XButtonEvent *)event;
    if(evb->button != 1)return;

    x1 = location.x<location.ex ? location.x:location.ex;
    y1 = location.y<location.ey ? location.y:location.ey;
    x2 = location.x>location.ex ? location.x:location.ex;
    y2 = location.y>location.ey ? location.y:location.ey;

    if(!location.vert)	
      location.ex=evb->x;
    if(!location.hor)
      location.ey=evb->y;

    if(x1)x1--;
    if(y1)y1--;
    x2++;
    y2++;

    if(!gks_mouse_menu_tracker)return;
    if(gks_mouse_menu_tracker==2){
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y1, x2  - x1 + 1, 1, x1, y1);
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y1, 1, y2 - y1 + 1, x1, y1);
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y2, x2  - x1 + 1, 1, x1, y2);
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x2, y1, 1, y2 - y1 + 1, x2, y1);
	XSync(ws->dpy,FALSE);

	XDrawRectangle(ws->dpy, location.win,
		       DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		       x1, y1, x2 - x1, y2 - y1);

	(void)get_slice((float)location.x/(float)location.width,
			(float)location.y/(float)location.height,
			(float)location.ex/(float)location.width,
			(float)location.ey/(float)location.height,1);


	location.x=location.y=location.ex=location.ey=0;
    }
    if(gks_mouse_menu_tracker==1 || gks_mouse_menu_tracker==3){
	XCopyArea(ws->dpy, location.map, location.win,
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  x1, y1, x2  - x1, y2 - y1 , x1, y1);
    
	XSync(ws->dpy,FALSE);
	
	
	XDrawLine(ws->dpy, location.win, 
		  DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		  location.x,location.y,location.ex,location.ey);

/*	if(gks_mouse_menu_tracker==1)
	    (void)get_slice((float)location.x/(float)location.width,
			    (float)location.y/(float)location.height,
			    (float)location.ex/(float)location.width,
			    (float)location.ey/(float)location.height,0);
/*	if(gks_mouse_menu_tracker==3)
	    (void)draw_line((float)location.x/(float)location.width,
			    (float)location.y/(float)location.height,
			    (float)location.ex/(float)location.width,
			    (float)location.ey/(float)location.height);
			    */  
    }
    
    if(gks_mouse_menu_tracker==4){
      
      XCopyArea(ws->dpy, location.map, location.win,
		DefaultGC(ws->dpy,
			  DefaultScreen(ws->dpy)),
		location.x-6,location.y-6, (unsigned int)12, 
		(unsigned int)12, 
		location.x-6,location.y-6);
      
      
      XSync(ws->dpy,FALSE);
      location.x=evb->x;
      location.y=evb->y;
      
      
      XDrawLine(ws->dpy, location.win, 
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		location.x+3,location.y+3,
		location.x-3,location.y-3);
      XDrawLine(ws->dpy, location.win, 
		DefaultGC(ws->dpy, DefaultScreen(ws->dpy)),
		location.x+3,location.y-3,
		location.x-3,location.y+3);
      (void)mouse_traj((float)location.x/(float)location.width,
		       (float)location.y/(float)location.height);
      
    }

    XSync(ws->dpy,FALSE);
    if(gks_mouse_menu_tracker == 2){
      gks_mouse_menu_tracker=0;
      location.dummy = 0;
      if(location.map){
	XFreePixmap(ws->dpy,location.map);
	location.map = 0;
	XtRemoveEventHandler(xgks_widget,~NoEventMask,FALSE,
			     mouseProcessEvents,
			     NULL);
      }
    }
}

static void special_button_stuff(ws,event)
     WS_STATE_PTR    ws;
     XEvent *event;
{
    KeySym ks;
    char *ksname;
    int nbytes, updown;
    char str[256+1];
    Display		*dpy	= ws->dpy;
    Window		root,win,*children;
    unsigned int                 numc;
    XWindowAttributes	win_att;
   
    XQueryTree(dpy, ws->win, &root, &win, &children, &numc);

    if(event->type ==KeyPress){
	nbytes = XLookupString ((XKeyEvent *)event, str, 256, &ks, NULL);
	ksname = XKeysymToString (ks);
	if(!ksname)return;
	if(!strncmp(ksname,"Control_L",9))
	  ive_gks_control=1;
	return;
    }
    
    if(event->type == ButtonPress){
	XButtonEvent *evb;
	evb=(XButtonEvent *)event;
	if(ive_gks_control ==0 && gks_mouse_menu_tracker==0)return;
	if(evb->button == 1 && gks_mouse_menu_tracker == 2){
	    location.dummy=1;
	    gks_start_window(evb->x, evb->y);
	    return;
	} 
	else if(evb->button == 1 && (gks_mouse_menu_tracker == 3 ||
				     gks_mouse_menu_tracker == 1)){
	  if(location.dummy != 2){
	    location.dummy=1;
	    gks_start_line(evb->x, evb->y);
	  }
	  else if(gks_mouse_menu_tracker == 4){
	    gks_start_traj(evb->x, evb->y);
	  }

	  else {
	    if(gks_mouse_menu_tracker == 4)
	      gks_move_traj(ws, evb->x, evb->y);
	    else
	      gks_move_line(ws, evb->x, evb->y);
	  }
	  return;
	}

	/*	if((evb->state & ControlMask) && ive_gks_control == 0)return;
	if(evb->button == 1)
	  XRaiseWindow(dpy, win);
	if(evb->button == 3)
	  XLowerWindow(dpy, win);
	  */
    }
    
    ive_gks_control=0;
    return;
}

/*
 * Redraw an XGKS window.
 */
    static int
redraw(ws)
    WS_STATE_PTR    ws;
{
    Display		*dpy	= ws->dpy;
    Window		win	= ws->win;
    XWindowAttributes	win_att;

    /*
     * Disable all input devices.
     */
    XgksIDevDisable(ws);

    /*
     * Get current window width and height values and update the 
     * transformation.
     */
    XGetWindowAttributes(dpy, win, &win_att);
    ws->wbound.x = win_att.width;
    ws->wbound.y = win_att.height;
    xXgksUpdateTrans(ws);

    /*
     * Redraw the window contents.
     */
    XClearArea(dpy, win, 0, 0, 0, 0, False);
#ifdef DEBUG
    (void) fprintf(stderr, "redraw: calling XgksXReDrawWs()\n");
#endif
    XgksXReDrawWs(ws);

    /* if necessary call user defined redraw notifying function */
    if (ws->redrawfuncp != NULL)
	(*(ws->redrawfuncp)) (ws->ws_id, GRD_X);

    XFlush(dpy);

    /*
     * Enable the input devices.
     */
    XgksIDevEnable(ws);

    return 0;
}


/*
 * The synchronous and asynchronous XEvent processing routine.
 *
 */
void
XgksProcessEvents(w, unused, xev, continue_to_dispatch)

Widget w;
XEvent *xev;
XtPointer unused;
Boolean *continue_to_dispatch;
{
    Gint            i;


    for (i = 0; i < MAX_OPEN_WS; i++) {
	Window		win	= xgks_state.openedws[i].win;
	WS_STATE_ENTRY	*ws	= xgks_state.openedws[i].ws;

	if (XtWindow(w) == win) {
#	    ifdef EVENTDEBUG
	    print_event(xev);
#	    endif
		
	    switch (xev->type) {
		/*
		 * Because a window exposure can result in multiple expose
		 * events (each indicating a sub-region to be redrawn) and
		 * XGKS can only redraw the entire window, every expose 
		 * event only sets the "redraw" flag for the window.
		 *
		 * NOTE: When a window is only moved, some servers 
		 * (Ultrix 4.2) do not send expose-events while other servers
		 * (AIX, MIT X11R4 under SunOS) do.  The result is unnecessary
		 * redraws on the latter servers.
		 */
	    case Expose:
	      if (((XExposeEvent *)xev)->count == 0) {
	if (gks_mouse_menu_tracker && gksokbox)
	  gks_line_cancel(gksokbox, 0, (XmAnyCallbackStruct *)NULL);
	    (void) redraw(ws);
		}
		break;

	    case KeyPress:
		special_button_stuff(ws, xev);
		XgksIProcessXEvent(xev, ws);
		break;
	    case KeyRelease:
		special_button_stuff(ws, xev);
		XgksIProcessXEvent(xev, ws);
		break;
	    case MotionNotify:
		XgksIProcessXEvent(xev, ws);
		break;
	    case ButtonPress:
		special_button_stuff(ws, xev);
		XgksIProcessXEvent(xev, ws);
		break;
	    case ButtonRelease:
		XgksIProcessXEvent(xev, ws);
		break;

	    default:
		break;
	    }
	    return;
	}
    }
}

    static void
mouseProcessEvents(w, unused, xev, continue_to_dispatch)
Widget w;
XEvent *xev;
XtPointer unused;
Boolean *continue_to_dispatch;
{
    Gint            i;
    int		    need_redraw[MAX_OPEN_WS];
/*    static void special_button_motion();
    static void special_button_stuff();
    static void special_button_release();*/

    for (i = 0; i < MAX_OPEN_WS; i++) {
	Window		win	= xgks_state.openedws[i].win;
	WS_STATE_ENTRY	*ws	= xgks_state.openedws[i].ws;
	
        if (XtWindow(w) == win) {
	    switch (xev->type) {
		
		/*
		 * Because a window exposure can result in multiple expose
		 * events (each indicating a sub-region to be redrawn) and
		 * XGKS can only redraw the entire window, every expose 
		 * event only sets the "redraw" flag for the window.
		 *
		 * NOTE: When a window is only moved, some servers 
		 * (Ultrix 4.2) do not send expose-events while other servers
		 * (AIX, MIT X11R4 under SunOS) do.  The result is unnecessary
		 * redraws on the latter servers.
		 */
	      case MotionNotify:
		special_button_motion(ws, xev);
		break;
	      case ButtonPress:
		special_button_stuff(ws, xev);
		break;
	      case ButtonRelease:
		special_button_release(ws, xev);
		break;
	      default:
		break;
	    }
	    return;
	}				/* event loop */
    }                                   /* workstation is X11 window */   
}					/* GKS workstation loop */

    void
xProcessEvents()
{
    fprintf(stderr, "Call to xProcessEvents in xgkswidget\n");
}

/*
 * Start the SIGIO interrupt system.
 */
    int
xXgksSIGIOStart(ws)
    WS_STATE_PTR    ws;
{
    Display        *dpy	= ws->dpy;
    pid_t           pid = getpid();

    if (dpy == NULL)				/* not opened yet */
	return INVALID;
    return 0;
}


    int
XgksSIGIO_OFF(dpy)
    Display        *dpy;
{
    return 0;

}


    int
XgksSIGIO_ON(dpy)
    Display        *dpy;
{
    return 0;
}

/*
#ifdef EVENTDEBUG*/
    static int
print_event(evnt)
    XEvent         *evnt;
{
    fprintf(stderr, "EVENT - ");
    switch (evnt->type) {
    case 0:
	(void) fprintf(stderr, "some kind of error");
	break;
    case 1:
	(void) fprintf(stderr, "some kind of reply");
	break;
    case KeyPress:
	(void) fprintf(stderr, "KeyPress");
	break;
    case KeyRelease:
	(void) fprintf(stderr, "KeyRelease");
	break;
    case ButtonPress:
	(void) fprintf(stderr, "ButtonPress");
	break;
    case ButtonRelease:
	(void) fprintf(stderr, "ButtonRelease");
	break;
    case MotionNotify:
	(void) fprintf(stderr, "MotionNotify");
	break;
    case EnterNotify:
	(void) fprintf(stderr, "EnterNotify");
	break;
    case LeaveNotify:
	(void) fprintf(stderr, "LeaveNotify");
	break;
    case FocusIn:
	(void) fprintf(stderr, "FocusIn");
	break;
    case FocusOut:
	(void) fprintf(stderr, "FocusOut");
	break;
    case KeymapNotify:
	(void) fprintf(stderr, "KeymapNotify");
	break;
    case Expose:
	(void) fprintf(stderr, "Expose");
	break;
    case GraphicsExpose:
	(void) fprintf(stderr, "GraphicsExpose");
	break;
    case NoExpose:
	(void) fprintf(stderr, "NoExpose");
	break;
    case VisibilityNotify:
	(void) fprintf(stderr, "VisibilityNotify");
	break;
    case CreateNotify:
	(void) fprintf(stderr, "CreateNotify");
	break;
    case DestroyNotify:
	(void) fprintf(stderr, "DestroyNotify");
	break;
    case UnmapNotify:
	(void) fprintf(stderr, "UnmapNotify");
	break;
    case MapNotify:
	(void) fprintf(stderr, "MapNotify");
	break;
    case MapRequest:
	(void) fprintf(stderr, "MapRequest");
	break;
    case ReparentNotify:
	(void) fprintf(stderr, "ReparentNotify");
	break;
    case ConfigureNotify:
	(void) fprintf(stderr, "ConfigureNotify");
	break;
    case ConfigureRequest:
	(void) fprintf(stderr, "ConfigureRequest");
	break;
    case GravityNotify:
	(void) fprintf(stderr, "GravityNotify");
	break;
    case ResizeRequest:
	(void) fprintf(stderr, "ResizeRequest");
	break;
    case CirculateNotify:
	(void) fprintf(stderr, "CirculateNotify");
	break;
    case CirculateRequest:
	(void) fprintf(stderr, "CirculateRequest");
	break;
    case PropertyNotify:
	(void) fprintf(stderr, "PropertyNotify");
	break;
    case SelectionClear:
	(void) fprintf(stderr, "SelectionClear");
	break;
    case SelectionRequest:
	(void) fprintf(stderr, "SelectionRequest");
	break;
    case SelectionNotify:
	(void) fprintf(stderr, "SelectionNotify");
	break;
    case ColormapNotify:
	(void) fprintf(stderr, "ColormapNotify");
	break;
    case ClientMessage:
	(void) fprintf(stderr, "ClientMessage");
	break;
    case MappingNotify:
	(void) fprintf(stderr, "MappingNotify");
	break;
    default:
	if (evnt->type >= LASTEvent) {
	    (void) fprintf(stderr, "extension event #%d", evnt->type);
	} else {
	    (void) fprintf(stderr, "\nEvent = %x", evnt->type);
	}
	break;
    }
    (void) fputc('\n', stderr);
    return 0;
}

void start_traj_mouse_()
{  
  XWindowAttributes	win_att;
  extern Widget xgks_widget;

    
  location.win=XtWindow(xgks_widget);
  location.dpy=XtDisplay(xgks_widget);
  XtVaGetValues(xgks_widget, XmNheight,&location.height, 
		XmNwidth,&location.width, NULL);
  location.hor = location.sandw = location.vert = 0;

  if(location.map){
    XFreePixmap(location.dpy, location.map);
    location.map = 0;
  }
  location.map = 
    XCreatePixmap(location.dpy, 
		  location.win, location.width, location.height,
		  DefaultDepth(location.dpy, 
			       DefaultScreen(location.dpy)));
  
  XCopyArea(location.dpy, location.win, location.map, 
	    DefaultGC(location.dpy, 
		      DefaultScreen(location.dpy)),
	    0, 0, location.width, location.height,  0, 0);
  XSync(location.dpy,FALSE);
  
  gks_mouse_menu_tracker=4;

  XGetWindowAttributes(location.dpy, location.win, &win_att);
  XtInsertEventHandler(xgks_widget,win_att.all_event_masks,
		       FALSE,mouseProcessEvents,
		       NULL,XtListHead);
}

void stop_traj_mouse_()
{
  extern Widget xgks_widget;
  if(gks_mouse_menu_tracker){
    location.x = location.ex = location.y = location.ey = 0.;
    gks_mouse_menu_tracker = 0;
    location.dummy = 0;
    if(location.map)
      XCopyArea(XtDisplay(xgks_widget), location.map, location.win,
		DefaultGC(XtDisplay(xgks_widget),
			  DefaultScreen(XtDisplay(xgks_widget))),
		location.x-6,location.y-6, (unsigned int)12, 
		(unsigned int)12, 
		location.x-6,location.y-6);
    XSync(XtDisplay(xgks_widget),FALSE);
    XFreePixmap(XtDisplay(xgks_widget),location.map);
    location.map = 0;
    XtRemoveEventHandler(xgks_widget,~NoEventMask,FALSE,
			 mouseProcessEvents,
			 NULL);
  }
  slice_menu_call((Widget)0, (caddr_t)10, (XmAnyCallbackStruct *)0);
}
 
void clear_traj_mouse_()
{
  extern Widget xgks_widget;


  XCopyArea(XtDisplay(xgks_widget), location.map, location.win,
	    DefaultGC(XtDisplay(xgks_widget),
		      DefaultScreen(XtDisplay(xgks_widget))),
	    location.x-6,location.y-6, (unsigned int)12, 
	    (unsigned int)12, 
	    location.x-6,location.y-6);
  XSync(XtDisplay(xgks_widget),FALSE);
  XSync(XtDisplay(xgks_widget),FALSE);
  XSync(XtDisplay(xgks_widget),FALSE);
  XtRemoveEventHandler(xgks_widget,~NoEventMask,FALSE,
		       mouseProcessEvents,
		       NULL);
}

