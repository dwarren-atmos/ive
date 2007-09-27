/*
 * $Id: loop_calls.c,v 1.4 2002/08/09 19:57:27 warren Exp $
 * This routine contains the loop control widget.
 *
 * $Log: loop_calls.c,v $
 * Revision 1.4  2002/08/09 19:57:27  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.3  1994/03/28 17:11:51  harry
 * Move the "one through" Xt loop into a seperate subroutine "update_all_".
 *
 * Revision 1.2  1993/07/23  20:08:02  warren
 * added lockcolortable to the loop ignore list. fixed the time slider callback
 * to compare the slider to the scaled times and added an array of scaled times.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:24  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.13  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.12  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.11  1992/12/23  19:05:30  harry
 * Only call loop_quit once!
 *
 * Revision 1.10  1992/12/14  18:34:38  harry
 * Add code for dump button.
 *
 * Revision 1.9  1992/12/04  18:34:39  harry
 * Change timer to loop_timer, move definition to loop.h.
 *
 * Revision 1.8  1992/12/03  00:24:31  harry
 * loop_alarm no longer needs to be global.
 *
 * Revision 1.7  1992/12/03  00:21:48  harry
 * Change timer calls from ualarm to XtAppAddTimeOut.
 *
 * Revision 1.6  1992/10/09  18:18:43  harry
 * Properly handle one frame loops.
 *
 * Revision 1.5  1992/10/06  18:13:16  harry
 * Add Id and Log information
 *
 *
 */

static char ident[] = "$Id: loop_calls.c,v 1.4 2002/08/09 19:57:27 warren Exp $";

#include<stdlib.h>
#include<stdio.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <Xm/XmAll.h>
#include <file_widgets.h>
#include <ive_for.h>
#include <loop.h>
#include <ive.h>

#define app_con XtWidgetToApplicationContext(ive_widget)

extern Widget ive_widget;
extern XmString NewString();
extern void init_dump();

void update_all_()
{
    XtInputMask mask;
    
    while(mask = XtAppPending(app_con)) {
	XtAppProcessEvent(app_con, mask);
    }
}

void Display_Pixmap(pix)
    int pix;	/* Pixmap to display */
{
    Display *dpy = XtDisplay(ive_widget);
    int i;

    if (num_pixmaps) XSetWindowBackgroundPixmap(dpy, loop_win, pixmap[pix]);
    if (num_pixmaps) XCopyArea (dpy, pixmap[pix], loop_win, loop_gc, 0, 0, 
	       loop_width, loop_height, 0, 0);
    XSync(dpy, 0);
}	

static void loop_alarm(data)
XtPointer *data;
{
    int time;

    for (;;) {
	if (num_pixmaps > 1) {
	    if (loop_frame == 0) {
		switch (loop_direction) {
		case CYCLE:
		    loop_cycle_direction = FORWARD;
		case FORWARD:
		    loop_frame = 1;
		    break;
		case BACKWARD:
		    loop_frame = num_pixmaps-1;
		}
	    }
	    else if (loop_frame == num_pixmaps-1) {
		switch (loop_direction) {
		case CYCLE:
		    loop_cycle_direction = BACKWARD;
		case BACKWARD:
		    loop_frame--;
		    break;
		case FORWARD:
		    loop_frame = 0;
		}
	    }
	    else 
		switch (loop_direction) {
		case CYCLE:
		    if(loop_cycle_direction == FORWARD) loop_frame++;
		    else loop_frame--;
		    break;
		case FORWARD:
		    loop_frame++;
		    break;
		case BACKWARD:
		    loop_frame--;
		}
	    Display_Pixmap(loop_frame);
	}
	if (loop_frame == 0) time = loop_start+loop_delay;
	else if (loop_frame == num_pixmaps-1) time = loop_end+loop_delay;
	else time = loop_delay;
	if (time == 0) {
	    update_all_();
	    if (loop_status == 0) return;
	}
	else {
	    loop_timer = XtAppAddTimeOut(app_con, time, (XtTimerCallbackProc)loop_alarm, NULL);
	    return;
	}
    }
}
    
void loop_toggle_call(w, data, call)
     Widget w;
     char *data;
     XmToggleButtonCallbackStruct *call;
{

    if(call->set){
	loop_run = loop_status = 1;
	loop_timer = XtAppAddTimeOut(app_con, 100, (XtTimerCallbackProc)loop_alarm, NULL);
    }
    else{
	XtRemoveTimeOut(loop_timer);
	loop_run = loop_status = 0;
    }
}

void loop_step_start_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    if (loop_run) return;
    switch(*data){
      case 'B':
/*	loop_direction = BACKWARD;*/
	if (loop_frame == 0) {
	    loop_frame = num_pixmaps-1;
	}
	else loop_frame--;
	break;
      case 'F':
/*	loop_direction = FORWARD;*/
	loop_frame = (loop_frame+1)%num_pixmaps;
    }
    Display_Pixmap(loop_frame);
    loop_status = 1;
    loop_timer = XtAppAddTimeOut(app_con, 500, (XtTimerCallbackProc)loop_alarm, NULL);
}

void loop_step_stop_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    if (loop_run) return;
    XtRemoveTimeOut(loop_timer);
    loop_status = 0;
}

void loop_ldir_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab, forw, back, cyc;
    forw = NewString(" Forward ");
    back = NewString("Backward ");
    cyc = NewString("Cycle    ");
    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(forw,oldlab)){
	XtVaSetValues(w,XmNlabelString,back,NULL);
	loop_direction = BACKWARD;
    }
    else if (XmStringCompare(back,oldlab)){
	XtVaSetValues(w,XmNlabelString,cyc,NULL);
	loop_direction = CYCLE;
    }
    else {
	XtVaSetValues(w,XmNlabelString,forw,NULL);
	loop_direction = FORWARD;
    }
    XmStringFree(oldlab);
    XmStringFree(forw);
    XmStringFree(back);
    XmStringFree(cyc);
}

void loop_scale_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
      int val;

      XmScaleGetValue(w,&val);
      
      switch(*data){
	case 'B':
	  loop_start = val;
	  break;
	case 'D':
	  loop_delay = val;
	  break;
	case 'E':
	  loop_end = val;
	  break;
      }
}

void loop_dump_call(w, data, call)
    Widget w;
    char *data;
    XmAnyCallbackStruct *call;
{
    XmString str;

    dump_widget.what = "loop";
    init_dump();
    str = NewString("loop");
    XtVaSetValues(dump_widget.What, XmNlabelString, str, NULL);
    XmStringFree(str);
}

void loop_quit_call(w, data, call)
    Widget w;
    char *data;
    XmAnyCallbackStruct *call;
{
    if (num_pixmaps) driver("loop_quit");
}

