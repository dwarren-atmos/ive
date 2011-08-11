/*
 * $Id: loop_control.c,v 1.5 2002/08/09 19:57:28 warren Exp $
 * This routine contains the loop control widget.
 *
 * $Log: loop_control.c,v $
 * Revision 1.5  2002/08/09 19:57:28  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.4  2001/12/28 23:43:22  davidive
 * Don't popup loop widget till done for use on 3d card X server
 *
 * Revision 1.3  1996/06/24 19:10:28  harry
 * Fix the loop_setup widget to die properly.
 *
 * Revision 1.2  1993/12/15  18:10:13  harry
 * Change default loop end delay time to 100 msec.
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
 * Revision 1.11  1992/12/23  19:05:48  harry
 * Call loop_quit_call rather than loop_driver.  Delete loop_setup widget
 * when control widget is started.
 *
 * Revision 1.10  1992/12/17  18:36:25  harry
 * Fix dump button.
 *
 * Revision 1.9  1992/12/14  18:35:17  harry
 * Add code for dump button.
 *
 * Revision 1.8  1992/12/03  00:24:49  harry
 * loop_alarm no longer needs to be global.
 *
 * Revision 1.7  1992/12/03  00:22:24  harry
 * Change direction button label with mouse button loop control.
 *
 * Revision 1.6  1992/11/17  17:21:29  warren
 * added ive_macros to includes
 *
 * Revision 1.5  1992/11/09  18:15:50  warren
 * fixed for MOTIF 1.2
 *
 * Revision 1.4  1992/10/06  18:13:30  harry
 * Add Id and Log information.
 *
 *
 */

static char ident[] = "$Id: loop_control.c,v 1.5 2002/08/09 19:57:28 warren Exp $";

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/XmAll.h>
#include <stdio.h>
#include <loop.h>
#include <ive.h>
#include <ive_macros.h>

extern void loop_toggle_call();
extern void loop_step_start_call();
extern void loop_step_stop_call();
extern void loop_ldir_call();
extern void loop_scale_call();
extern void loop_dump_call();
extern void loop_quit_call();
extern void Display_Pixmap();

static Widget toggle, backs, fors, ldir;
Widget loop_form = NULL;
extern Widget loop_setup_form, loop_popup;

void loop_input_call(w, data, call)
    Widget w;
    XtPointer data;
    XmDrawingAreaCallbackStruct *call;
{
    XEvent *event = call->event;
    XButtonEvent *xbutton = (XButtonEvent *)event;
    XmString label;

    if (call->reason == XmCR_INPUT) {
	if (event->type == ButtonPress) {
	    switch (xbutton->button) {
	    case Button1:
	      label = XmStringCreateLocalized("Backward ");

		XtVaSetValues(ldir, XmNlabelString, label, NULL);
		XmStringFree(label);
		loop_step_start_call(w, "B", call);
		break;
	    case Button3:
	      label = XmStringCreateLocalized(" Forward ");

		XtVaSetValues(ldir, XmNlabelString, label, NULL);
		XmStringFree(label);
		loop_step_start_call(w, "F", call);
	    }
	}
	else if (event->type == ButtonRelease) {
	    switch (xbutton->button) {
	    case Button1:
		loop_step_stop_call(w, "B", call);
		break;
	    case Button2:
		if (XmToggleButtonGetState(toggle))
		    XmToggleButtonSetState(toggle, FALSE, TRUE);
		else
		    XmToggleButtonSetState(toggle, TRUE, TRUE);
		break;
	    case Button3:
		loop_step_stop_call(w, "F", call);
	    }
	}
    }
}
	   
void do_loop_()
{
    XmString str;
    Arg args[8];
    Widget done, popup, quit, dump;
    Widget delay, startp, endp;
    Pixel pix;

    if (loop_setup_form) XtDestroyWidget(XtParent(XtParent(loop_setup_form)));
    if (loop_form){
        if(XtIsManaged(XtParent(loop_form))) {
	    XtUnmanageChild(XtParent(loop_form));
	    return;
	}
        else
	    XtManageChild(XtParent(loop_form));
    }

    else {
	/*make form but don't manage*/
	XtSetArg(args[0],XmNheight, 235);
	XtSetArg(args[1],XmNwidth, 225);
	XtSetArg(args[2],XmNautoUnmanage,FALSE);
	XtSetArg(args[3],XmNx, 0);
	XtSetArg(args[4],XmNy, 200);
	XtSetArg(args[5],XtNallowShellResize,TRUE);
	popup = XtCreatePopupShell("Loop Control",
				   transientShellWidgetClass,
				   XtParent(Box) ,args,6); 
	XtAddCallback(popup,XmNdestroyCallback, loop_quit_call, NULL);
	XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&loop_form);
	XtSetArg(args[2],XmNmarginWidth, 2);
	loop_form = XmCreateBulletinBoard(popup,"Loop Control",args,3);
	XtManageChild(loop_form);
	
	/*buttons*/

	XtVaCreateManagedWidget("Quit", xmLabelWidgetClass, loop_form,
				XmNx, 3, NULL);
	quit = XtVaCreateManagedWidget("", xmToggleButtonWidgetClass,
				       loop_form, XmNx, 5, XmNy, 30,
				       XmNmarginHeight, 0,
				       XmNmarginWidth, 0, 
				       XmNhighlightThickness,0,
				       XmNindicatorSize, 20, NULL);
	XtAddCallback(quit,XmNvalueChangedCallback,loop_quit_call,NULL);
	
	XtSetArg(args[0],XmNx,35);
	XtCreateManagedWidget("Step",xmLabelWidgetClass,loop_form,args,1);
	
	
	XtSetArg(args[0],XmNx,40);
	XtSetArg(args[1],XmNarrowDirection,XmARROW_LEFT);
	XtSetArg(args[2],XmNy,30);
	backs = XtCreateManagedWidget("Back Step",xmArrowButtonWidgetClass,
				      loop_form,args,3);
	
	XtAddCallback(backs,XmNarmCallback,loop_step_start_call,"B");
	XtAddCallback(backs,XmNdisarmCallback,loop_step_stop_call,"B");
	
	XtVaCreateManagedWidget("Run", xmLabelWidgetClass, loop_form, 
				XmNx, 63, NULL);
	toggle = XtVaCreateManagedWidget("", xmToggleButtonWidgetClass,
					 loop_form, XmNx, 65, XmNy, 30,
					 XmNmarginHeight, 0,
					 XmNmarginWidth, 0, 
					 XmNhighlightThickness,0,
					 XmNindicatorSize, 20, NULL);
	XtAddCallback(toggle,XmNvalueChangedCallback,loop_toggle_call,NULL);
	
	XtVaCreateManagedWidget("Step", xmLabelWidgetClass, loop_form,
				XmNx, 85, NULL);
	XtSetArg(args[0],XmNx,85);
	XtSetArg(args[1],XmNarrowDirection,XmARROW_RIGHT);
	fors = XtCreateManagedWidget("Forward Step",xmArrowButtonWidgetClass,
				     loop_form,args,3);
	
	XtAddCallback(fors,XmNarmCallback,loop_step_start_call,"F");
	XtAddCallback(fors,XmNdisarmCallback,loop_step_stop_call,"F");
	
	XtSetArg(args[0],XmNx,125);
	XtCreateManagedWidget("Direction",
			      xmLabelWidgetClass,loop_form,args,1);
	
	XtSetArg(args[0],XmNx,120);
	XtSetArg(args[1],XmNy,29);
	
	ldir = XtCreateManagedWidget(" Forward ",xmPushButtonWidgetClass,
				     loop_form,args,2);
	
	XtAddCallback(ldir,XmNactivateCallback,loop_ldir_call,NULL);

	XtVaCreateManagedWidget("Dump", xmLabelWidgetClass, loop_form,
				XmNx, 195, NULL);
	dump = XtVaCreateManagedWidget("", xmPushButtonWidgetClass,
				       loop_form, XmNx, 200, XmNy, 34,
				       XmNwidth, 15, XmNheight, 15,
				       XmNmarginHeight, 0,
				       XmNmarginWidth, 0, 
				       XmNhighlightThickness,0, NULL);
	XtAddCallback(dump, XmNactivateCallback, loop_dump_call, NULL);

	/* scales */
	
	str = XmStringCreateLocalized("Loop Delay (ms)");
	XtSetArg(args[0], XmNy, 60);
	XtSetArg(args[1],XmNwidth,220);
	XtSetArg(args[2],XmNminimum,0);
	XtSetArg(args[3],XmNmaximum,1000);
	XtSetArg(args[4],XmNorientation,XmHORIZONTAL);
	XtSetArg(args[5],XmNshowValue,TRUE);
	XtSetArg(args[6],XmNtitleString,str);
	delay = XtCreateManagedWidget("delay",xmScaleWidgetClass,
				      loop_form,args,7);
	
	XtAddCallback(delay,XmNvalueChangedCallback,loop_scale_call,"D");
	XtUnmanageChild(XtNameToWidget(delay,"Title"));
	XtVaCreateManagedWidget("Loop Delay (ms)", 
				xmLabelWidgetClass, loop_form,
				XmNy, 95,
				XmNx, 60,NULL);
	XmStringFree(str);
	str = XmStringCreateLocalized("Beginning Pause (ms)");
	
	XtSetArg(args[0], XmNy, 120);
	XtSetArg(args[1],XmNwidth,220);
	XtSetArg(args[2],XmNminimum,0);
	XtSetArg(args[3],XmNmaximum,2000);
	XtSetArg(args[4],XmNorientation,XmHORIZONTAL);
	XtSetArg(args[5],XmNshowValue,TRUE);
	XtSetArg(args[6],XmNtitleString,str);
	startp = XtCreateManagedWidget("delay",xmScaleWidgetClass,
				       loop_form,args,7);
	
	XtAddCallback(startp,XmNvalueChangedCallback,loop_scale_call,"B");
	
	XmStringFree(str);
	XtUnmanageChild(XtNameToWidget(startp,"Title"));
	XtVaCreateManagedWidget("Beginning Pause (ms)", 
				xmLabelWidgetClass, loop_form,
				XmNy, 155,
				XmNx, 40,NULL);
	str = XmStringCreateLocalized("End Pause (ms)");
	
	XtSetArg(args[0], XmNy, 180);
	XtSetArg(args[1],XmNwidth,220);
	XtSetArg(args[2],XmNminimum,0);
	XtSetArg(args[3],XmNmaximum,2000);
	XtSetArg(args[4],XmNorientation,XmHORIZONTAL);
	XtSetArg(args[5],XmNshowValue,TRUE);
	XtSetArg(args[6],XmNtitleString,str);
	endp = XtCreateManagedWidget("delay",xmScaleWidgetClass,
				     loop_form,args,7);
	
	XtAddCallback(endp,XmNvalueChangedCallback,loop_scale_call,"E");
	XtUnmanageChild(XtNameToWidget(endp,"Title"));
	XtVaCreateManagedWidget("End Pause (ms)", 
				xmLabelWidgetClass, loop_form,
				XmNy, 215,
				XmNx, 60,NULL);
	XmStringFree(str);
	
	XtManageChild(popup);
	XtPopup(popup,XtGrabNone);
    }
    loop_frame = num_pixmaps-1;
    loop_run = loop_status = 0;
    loop_direction = FORWARD;
    loop_start = loop_end = 0;
    loop_delay = 100;
    XtVaSetValues(delay, XmNvalue, loop_delay, NULL);
    XtPopup(loop_popup, XtGrabNone);
    Display_Pixmap(0);
}
