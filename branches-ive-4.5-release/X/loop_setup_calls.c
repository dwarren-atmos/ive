/*
 * $Id: loop_setup_calls.c,v 1.15 2002/08/09 19:57:28 warren Exp $
 * This routine contains the loop setup callbacks.
 *
 * $Log: loop_setup_calls.c,v $
 * Revision 1.15  2002/08/09 19:57:28  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.14  2001/08/15 22:23:21  davidive
 * Cleaned up code to reduce warnings about type mismatches, and added buttons
 * and callbacks to the 1d form widget to select which access to label against
 * for diagonal plots.
 *
 * Revision 1.13  1998/06/11 21:52:12  harry
 * Fix loop sliders to act like window sliders.
 *
 * Revision 1.12  1997/04/15 17:19:42  harry
 * Remove time_tag looping - now handles by multiple files.
 *
 * Revision 1.11  1997/04/11 18:46:42  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.10  1996/10/18 23:22:53  harry
 * Fix declarations and calls to make SGI compiler happy.
 *
 * Revision 1.9  1996/10/09 18:54:13  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.8  1996/08/12 18:33:56  harry
 * Fix for 4.0.3 - incorrect usage of float_nozero in loop_setup_calls.c
 *
 * Revision 1.7  1996/08/09 19:28:08  harry
 * Take out %.2f formats and replace with calls to float_nozero.
 *
 * Revision 1.6  1996/08/01 22:44:09  harry
 * Allow floating point numbers in type-in boxes on sliders.
 *
 * Revision 1.5  1995/10/19  22:13:23  harry
 * Set defaults by clicking right mouse button.  Make all TextWidgets into
 * TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.4  1995/10/04  22:09:05  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.3  1994/02/01  02:06:24  warren
 * fixed dales problem of lock values changing with new var and added type in
 * for loop set up.
 *
 * Revision 1.2  1993/08/18  15:46:13  harry
 * Change to new transform routines.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:21  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.4  1992/12/23  18:57:59  harry
 * Add start value processing to loop callbacks.
 *
 * Revision 1.3  1992/10/08  23:15:51  harry
 * Handle end value scale widget the same as the slicer.
 *
 * Revision 1.2  1992/10/08  18:22:24  harry
 * Print error message if coordinate has not been chosen.
 *
 * Revision 1.1  1992/10/07  22:17:22  harry
 * Initial revision
 *
 *
 */

static char ident[] = "$Id: loop_setup_calls.c,v 1.15 2002/08/09 19:57:28 warren Exp $";

#include <Xm/XmAll.h>
#include <Xm/Text.h>
#include <stdlib.h>
#include <math.h>
#ifdef Linux
extern double rint();
#endif

#include <ive.h>
#include <ive_for.h>
#include <loop_setup.h>
#define MAX(a,b) (a>b ? a:b)
#define MIN(a,b) (a<b ? a:b)

extern Widget loop_setup_form;
extern void getdarr_(),getrvar_(),scale_(),getrarr_();

void loop_coord_call(w, data, call)
    Widget w;
    int data;
    XmToggleButtonCallbackStruct *call;
{
    float min[4], max[4], rmin, rmax;
    int i;
    char retstr[80];
    static int four=4, one=1;
    static char *name[4] = {"xloc", "yloc", "zloc", "tloc"};

    loop_setup.coord = data;
    if (data--) {
	float zero=0.0;
	double intercept[4], slope[4];

	getdarr_("domain_slope", slope, &four, &i, 12);
	getdarr_("domain_intercept", intercept, &four, &i, 16);
	getrvar_(name[data], &loop_setup.start, &i, 4);
	scale_(&loop_setup.start, &one, slope+data, intercept+data, &zero);
	loop_setup.end_value = loop_setup.start;
	getrarr_("plmin_scaled", min, &four, &i, 12);
	getrarr_("plmax_scaled", max, &four, &i, 12);
	rmin = MIN(min[data], max[data]);
	rmax = MAX(min[data], max[data]);
	XtVaSetValues(loop_setup.start_widget,
		      XmNprocessingDirection,
		      min[data]<max[data]?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
		      XmNminimum, (int)(rint(rmin)),
		      XmNmaximum, (int)(rint(rmax)),
		      XmNvalue, (int)loop_setup.start, NULL);
	XtManageChild(loop_setup.start_widget);
	XtVaSetValues(loop_setup.endv_widget,
		      XmNprocessingDirection,
		      min[data]<max[data]?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
		      XmNminimum, (int)(rint(rmin)),
		      XmNmaximum, (int)(rint(rmax)),
		      XmNvalue, (int)loop_setup.end_value, NULL);
	XtManageChild(loop_setup.endv_widget);
    }
}

void loop_start_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    int val, ival;

    XmScaleGetValue(w, &val);
    loop_setup.start = fix_slicer_value(loop_setup.coord-1, (float) val);
    ival = (int)rint(loop_setup.start);
    XmScaleSetValue(loop_setup.start_widget,ival);
}

void loop_inc_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    char *str;

    str = XmTextGetString(w);
    sscanf(str, "%f", &loop_setup.increment);
}

void loop_endv_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    int val, ival;

    XmScaleGetValue(w, &val);
    loop_setup.end_value = fix_slicer_value(loop_setup.coord-1,(float) val);
    ival = (int)rint(loop_setup.end_value);
    XmScaleSetValue(loop_setup.endv_widget,ival);
}

void loop_ready_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    static char *name[4] = {"XLOC", "YLOC", "ZLOC", "TIME"};
    char buff[80];

    if (loop_setup.coord) {
	sprintf(buff, "loop=%s,%s,", name[loop_setup.coord-1],
		float_nozero(loop_setup.start));
	strcat(buff, (const char *)float_nozero(loop_setup.increment));
	strcat(buff, ",");
	strcat(buff, (const char *)float_nozero(loop_setup.end_value));
	driver(buff);
    }
    else
	(void)make_help_widget_("Must pick a coordinate to loop.");
}

void loop_setup_rescale_()
{
    if (loop_setup_form && XtIsManaged(loop_setup.endv_widget)) {
      loop_coord_call((Widget)NULL, loop_setup.coord, 
		      (XmToggleButtonCallbackStruct *)NULL);
    }
}

void loop_setup_update_(dim, start, inc, endnum)
    int *dim;
    float *start, *inc, *endnum;
{
    loop_setup.coord = *dim;
    loop_setup.start = *start;
    loop_setup.increment = *inc;
    loop_setup.end_value = *endnum;
    if (loop_setup_form) {
	if (*dim) {
	    XtVaSetValues(loop_setup.button[*dim-1], XmNset, TRUE, NULL);
	}
	loop_coord_call((Widget)NULL, *dim, 
			(XmToggleButtonCallbackStruct *)NULL);
	XtVaSetValues(loop_setup.start_widget, XmNvalue, (int) *start, NULL);
	XtVaSetValues(loop_setup.incr_widget, XmNvalue, float_nozero(*inc),
		      NULL);
	XtVaSetValues(loop_setup.endv_widget, XmNvalue, (int) *endnum, NULL);
    }
}

void loop_start_type_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    float val, ival;
    char *str;

    str = XmTextGetString(w);
    val = atof(str);
    XtDestroyWidget(w);
    XtManageChild(loop_setup.start_widget);
    ival = (int) rint(val);
    XmScaleSetValue(loop_setup.start_widget,ival);
    loop_setup.start = val;
}

void loop_endv_type_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    float val, ival;
    char *str;

    str = XmTextGetString(w);
    val = atof(str);
    XtDestroyWidget(w);
    XtManageChild(loop_setup.endv_widget);
    ival = (int) rint(val);
    XmScaleSetValue(loop_setup.endv_widget, ival);
    loop_setup.end_value = val;
}

void loop_start_type_in(widg,data,ev)
     Widget widg;
     int data;
     XButtonEvent *ev;
{
    Widget w;
    void check_num2();
    void text_box_motion();
    unsigned long root, child;
    int x,y, rx,ry;
    unsigned int ret;
  
    XQueryPointer(XtDisplay(widg),XtWindow(XtParent(widg)),(Window *)&root,
		  (Window *)&child,&rx,&ry,&x,&y,&ret);
    XtUnmanageChild(widg);
    
    w=XtVaCreateManagedWidget("VALUE",xmTextFieldWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,
		  (XtCallbackProc)loop_start_type_call,&w);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,NULL);

}

void loop_end_type_in(widg,data,ev)
     Widget widg;
     int data;
     XButtonEvent *ev;
{
    Widget w;
    void check_num2();
    void text_box_motion();
    unsigned long root, child;
    int x,y, rx,ry;
    unsigned int ret;
  
    XQueryPointer(XtDisplay(widg),XtWindow(XtParent(widg)),&root,&child,
                  &rx,&ry,&x,&y,&ret);
    XtUnmanageChild(widg);
    
    w=XtVaCreateManagedWidget("VALUE",xmTextWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,
		  (XtCallbackProc)loop_endv_type_call,&w);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,NULL);

}
