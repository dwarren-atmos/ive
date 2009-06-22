/*
 * $Id: loop_setup.c,v 1.10 1997/04/15 17:19:42 harry Exp $
 * This routine contains the loop setup widget.
 *
 * $Log: loop_setup.c,v $
 * Revision 1.10  1997/04/15 17:19:42  harry
 * Remove time_tag looping - now handles by multiple files.
 *
 * Revision 1.9  1997/04/11 18:46:41  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.8  1996/10/09 18:54:12  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.7  1996/06/24 19:10:29  harry
 * Fix the loop_setup widget to die properly.
 *
 * Revision 1.6  1996/06/13 22:28:51  harry
 * Clean-up some commented out code from the last change.
 *
 * Revision 1.5  1996/06/13 22:27:40  harry
 * Make into a form widget to handle different label fonts.
 *
 * Revision 1.4  1995/10/19 22:13:21  harry
 * Set defaults by clicking right mouse button.  Make all TextWidgets into
 * TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.3  1995/10/04  22:09:03  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.2  1994/02/01  02:06:23  warren
 * fixed dales problem of lock values changing with new var and added type in
 * for loop set up.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:20  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.5  1992/12/23  18:57:03  harry
 * Add start value to loop widget.
 *
 * Revision 1.4  1992/12/22  18:46:33  harry
 * Move some callbacks.
 *
 * Revision 1.3  1992/11/19  00:56:26  warren
 * magic dissapearing text at the correct times in text boxes.
 *
 * Revision 1.2  1992/10/08  23:16:31  harry
 * Use ui_update to set inital status of coordinate buttons.
 *
 * Revision 1.1  1992/10/07  22:17:00  harry
 * Initial revision
 *
 *
 */

static char ident[] = "$Id: loop_setup.c,v 1.10 1997/04/15 17:19:42 harry Exp $";

#include <Xm/XmAll.h>
#include <ive.h>
#include <ive_widgets.h>
#include <loop_setup.h>
#include <ive_text_enum.h>

extern Widget loop_form;
extern void getaarr_(),ui_update_(),loop_setup_update_();
Widget loop_setup_form=NULL;
void loop_coord_call(), loop_start_call(), loop_inc_call(),
    loop_endv_call(), loop_ready_call();

struct loop_setup loop_setup = {0, 0.0, 0.0, 0.0, {NULL, NULL, NULL, NULL},
				    NULL, NULL, NULL};

void init_loop(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    Widget popup, radio_box, loop_ready, loopstart, loopincr, loopend;
    Widget looptemp;
    Arg args[2];
    int lock[4], i;
    char name[4], dim_names[4][80];
    static int four=4;
    extern void loop_start_type_in(), loop_end_type_in();

    if (loop_setup_form){
	if (loop_form) XtDestroyWidget(XtParent(loop_form));
	XtDestroyWidget(XtParent(loop_setup_form));
    }
    else {
	/*make form but don't manage*/
	(void)getaarr_("dim_display_names",dim_names,&four,&i,17,80);
	popup = XtVaCreatePopupShell("Loop Setup",
				     transientShellWidgetClass,
				     XtParent(Box), XmNheight, 150,
				     XmNwidth, 240, XmNautoUnmanage, FALSE,
				     XmNx, 0, XmNy, 200,
				     XtNallowShellResize, TRUE, NULL);
	XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		      &loop_setup_form);
/*
   Extra Form Widget needed by Motif.
   */

	looptemp = XtVaCreateManagedWidget("Loop Setup",
					  xmFormWidgetClass,
					  popup,
					  XmNresizable, TRUE,
					  XmNautoUnmanage, FALSE, NULL);

	loop_setup_form = XtVaCreateWidget("Loop Setup",
					   xmFormWidgetClass,
					   looptemp,
					   XmNtopAttachment,
					   XmATTACH_FORM,
					   XmNleftAttachment,
					   XmATTACH_FORM,
					   XmNrightAttachment,
					   XmATTACH_FORM,
					   XmNmarginWidth, 5,
					   XmNborderWidth, 1,
					   XmNresizable, TRUE,
					   XmNresizePolicy,
					   XmRESIZE_ANY,
					   NULL);
	XtManageChild(loop_setup_form);

	/* row 1 */

	loop_ready = XtVaCreateManagedWidget("GO", xmPushButtonWidgetClass,
					     loop_setup_form, XmNx, 10,
					     XmNy, 10, NULL);
	XtAddCallback(loop_ready, XmNactivateCallback, loop_ready_call, NULL);
	radio_box = XmCreateRadioBox(loop_setup_form, "loop_coord", NULL, 0);
	XtVaSetValues(radio_box, XmNorientation, XmHORIZONTAL,
		      XmNleftAttachment, XmATTACH_WIDGET,
		      XmNleftWidget, loop_ready, 
		      XmNy, 7, NULL);
	for (i=0; i < 4; ++i) {
	    int j = strlen(dim_names[i]);
	    Widget last = loop_ready;

	    if (j > 3) j = 3;
	    strncpy(name, dim_names[i], j);
	    name[j] = '\0';
	    loop_setup.button[i] = XtVaCreateWidget(name, 
					      xmToggleButtonWidgetClass,
					      radio_box,
					      NULL);
	    last = loop_setup.button[i];
	    XtAddCallback(loop_setup.button[i], XmNarmCallback,
			  loop_coord_call, (XtPointer)(i+1));
	}
	XtManageChild(radio_box);

	/* row 2 */

	loopstart = XtVaCreateManagedWidget("Start Value", xmLabelWidgetClass,
				loop_setup_form, XmNx, 5, XmNy, 50, NULL);
	loop_setup.start_widget = XtVaCreateWidget("Start Value2",
			 	    xmScaleWidgetClass,
				    loop_setup_form, XmNy, 35,
				    XmNwidth, 150,
				    XmNorientation, XmHORIZONTAL, 
				    XmNshowValue, TRUE, NULL);
	XtAddRawEventHandler(loop_setup.start_widget,
			     ButtonPressMask,FALSE,
			     loop_start_type_in,0);
	XtAddCallback(loop_setup.start_widget, XmNvalueChangedCallback,
		      loop_start_call, NULL);
	
	/* row 3 */

	loopincr = XtVaCreateManagedWidget("Increment", xmLabelWidgetClass,
				loop_setup_form, XmNx, 10, XmNy, 85, NULL);
	loop_setup.incr_widget = XtVaCreateManagedWidget("Increment2",
				    xmTextFieldWidgetClass, loop_setup_form,
				    XmNcolumns, 10,
				    XmNleftAttachment,
				    XmATTACH_WIDGET,
				    XmNleftWidget, loopincr,
				    XmNleftOffset, 10,
				    XmNy, 80,
                                    XmNvalue, "0.0", NULL);
	XtVaSetValues(loop_setup.start_widget, XmNleftAttachment,
		XmATTACH_OPPOSITE_WIDGET,XmNleftWidget, loop_setup.incr_widget,
		NULL);
	XtAddCallback(loop_setup.incr_widget, XmNactivateCallback,
		      loop_inc_call, NULL);
	XtAddCallback(loop_setup.incr_widget, XmNactivateCallback,
		      cleanup_box_call, (XtPointer)INCR_WIDGET);
	XtAddCallback(loop_setup.incr_widget, XmNmodifyVerifyCallback,
		      check_num, (XtPointer)INCR_WIDGET);
	XtAddCallback(loop_setup.incr_widget, XmNmotionVerifyCallback,
		      text_box_motion, (XtPointer)INCR_WIDGET);
	
	/* row 4 */

	loopend = XtVaCreateManagedWidget("End Value", xmLabelWidgetClass,
				loop_setup_form, XmNx, 10, XmNy, 125, NULL);
	loop_setup.endv_widget = XtVaCreateWidget("End Value2",
			 	    xmScaleWidgetClass,
				    loop_setup_form, XmNy, 110,
				    XmNwidth, 150,
				    XmNleftAttachment,
				    XmATTACH_OPPOSITE_WIDGET,
				    XmNleftWidget, loop_setup.incr_widget,
				    XmNorientation, XmHORIZONTAL, 
				    XmNshowValue, TRUE, NULL);
	XtAddRawEventHandler(loop_setup.endv_widget,
			     ButtonPressMask,FALSE,
			     loop_end_type_in,0);
	XtAddCallback(loop_setup.endv_widget, XmNvalueChangedCallback,
		      loop_endv_call, NULL);
	i = LOOP;
	ui_update_(&i);
	loop_setup_update_(&loop_setup.coord, &loop_setup.start, 
			   &loop_setup.increment, &loop_setup.end_value);
	XtManageChild(popup);
	XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		      &loop_setup_form);
	XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
                  (XtPointer)LOOP_TEXT_BOXES);
	XtPopup(popup, XtGrabNone);
    }
}
