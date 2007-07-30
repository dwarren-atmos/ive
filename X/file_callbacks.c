/****************************************************************************
$Log: file_callbacks.c,v $
Revision 1.16  2002/08/09 19:57:25  warren
Fixed for Linux and tested with DEC sun etc.
most items inside #ifdef's, or were general cleanup/fixes

Revision 1.15  2001/08/15 22:23:20  davidive
Cleaned up code to reduce warnings about type mismatches, and added buttons
and callbacks to the 1d form widget to select which access to label against
for diagonal plots.

Revision 1.14  1998/09/04 20:51:40  harry
Add colorbar button to annotation widget.  Rewrite file widgets to
remember previous entries.

Revision 1.13  1998/07/16 22:54:28  warren
Fixed eps and dump windows so they disapear when closed.

Revision 1.12  1998/03/04 19:13:36  davidive
First cut at trajectories and trajectory widget using colors to show depth
and arrows to show direction.

Revision 1.11  1997/12/31 19:13:58  harry
Destroy the correct widget in file_ok_callback.

Revision 1.10  1997/12/29 20:19:28  warren
Fixed the calls to done_callback to only delete the correct widget instead
of ive_main.

Revision 1.9  1997/07/15 17:41:41  warren
Increase beta number, get rid of empty popup that was left after file
widgets were gone.

Revision 1.8  1997/04/30 18:46:09  harry
Add widget to write out color table.

Revision 1.7  1997/04/07 16:53:35  harry
Properly set the inital start and end times for multiple files.

Revision 1.6  1997/03/24 14:44:53  harry
Add widgets to file selection window for multiple files.

 * Revision 1.5  1995/02/07  18:26:19  warren
 * update for new dec c compiler and add missing null to XtVaSetValues call.
 *
 * Revision 1.4  1994/03/21  18:38:08  warren
 * fixed nondisapearing eps form and wrong number for current in multiprint form.
 *
 * Revision 1.3  1994/03/08  19:58:21  warren
 * fix spelling of component and make input windows go away with selection
 *
 * Revision 1.2  1993/07/30  22:48:45  warren
 * Added widget to eps button to save EPS files.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:18  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.7  1993/02/22  17:37:05  warren
 * changed free to XtFree
 *
 * Revision 2.6  1993/02/19  21:14:19  warren
 * made Xtfree XrFree
 *
 * Revision 2.5  1993/02/19  21:10:38  warren
 * fixed free problem
 *
 * Revision 2.4  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 2.3  1992/12/23  18:54:03  harry
 * Add log file callback.
 *
 * Revision 2.2  1992/12/14  18:37:42  harry
 * Add callbacks for dump and input widgets.
 *
 * Revision 2.1  1992/11/17  17:21:29  warren
 * added ive_macros to includes
 *
 * Revision 2.0  1992/07/01  21:11:22  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.1  1992/04/21  00:08:28  millerp
 * Initial revision
 *
 * Revision 1.100  1992/03/26  22:53:48  warren
 * Released version rereving to 1.100
 *
 * Revision 1.3  1992/03/24  19:23:16  warren
 * *** empty log message ***
 *
 * Revision 1.2  1992/03/18  20:27:24  warren
 * *** empty log message ***
 *
 * Revision 1.1  1991/06/12  19:57:50  warren
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: file_callbacks.c,v 1.16 2002/08/09 19:57:25 warren Exp $";

#ifdef __osf__
#define _XOPEN_SOURCE
#endif
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef Linux
#define S_IFDIR __S_IFDIR
#endif
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/XmAll.h>
#include <Xm/Text.h>
#include <X11/Xlib.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_macros.h>
#include <file_widgets.h>
#include <loop.h>

/*
  cdf_temp is set up in open_var_
  */
extern struct cdf_temp {
    int n;
    float *values;
} cdf_temp;

static int fix_file_value(value)
int value;
{
    int i;

    if (cdf_temp.values[0] < cdf_temp.values[1]) {
	for (i = 0; i < cdf_temp.n && cdf_temp.values[i] < value; i++);
	if (i == cdf_temp.n) i--;
	else if (value != cdf_temp.values[i]) {
	    if (value-cdf_temp.values[i-1] < cdf_temp.values[i]-value)
		value = cdf_temp.values[i-1];
	    else
		value = cdf_temp.values[i];
	}
    }
    else {
	for (i = 0; i < cdf_temp.n && cdf_temp.values[i] > value; i++);
	if (i == cdf_temp.n) i--;
	else if (value != cdf_temp.values[i]) {
	    if (cdf_temp.values[i-1]-value < value-cdf_temp.values[i])
		value = cdf_temp.values[i-1];
	    else
		value = cdf_temp.values[i];
	}
    }
    return(value);
}

dir_check(file)
char *file;
{
    struct stat buff;
    stat(file,&buff);
    if(buff.st_mode & S_IFDIR){
	printf("dir\n");
	    return(1);
	}
    else{
	    return(0);
	}
}

void update_fsb(struct fsb *fsb, XmFileSelectionBoxCallbackStruct *call)
{
    XmStringFree(fsb->directory);
    fsb->directory = XmStringCopy(call->dir);
    XmStringFree(fsb->dirSpec);
    fsb->dirSpec = XmStringCopy(call->value);
    XmStringFree(fsb->pattern);
    fsb->pattern = XmStringCopy(call->pattern);
}

extern XmString NewString();

void file_ok_callback(w, client, call)
     Widget w;
     Widget client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *old, *choice;
    Arg args[2];
    static Bool data=TRUE;
    char buff[512];
    int mr, i;
    static char *name[4] = {"X", "Y", "Z", "T"};

    update_fsb(&dir_widget.fsb, call);
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
    if (dir_widget.coord > 1) {
	if (XtIsManaged(dir_widget.start)){
	    sprintf(buff, "multiple_read=%s,%d,%d", name[dir_widget.coord-2],
		    dir_widget.start_val, dir_widget.end_val);
	    driver(buff);
	    XtUnmanageChild(dir_widget.startlab);
	    XtUnmanageChild(dir_widget.start);
	    XtUnmanageChild(dir_widget.endlab);
	    XtUnmanageChild(dir_widget.end);
	    free(cdf_temp.values);
	}
	else {
	    /* 
	       This is a kludge to do part of the processing now, and
	       part later while allowing reversability.
	       */
	    int flag, val=dir_widget.coord-2, one=1, found;
	    float dmin[4], dmax[4], delta[4];
	    char dom_units[80], dom_dunits[4], label[80];

	    getivar_("multiple_read", &mr, &i, 13);
	    getivar_("multiple_read_flag", &flag, &i, 18);
	    setivar_("multiple_read", &val, &i, 13);
	    setivar_("multiple_read_flag", &one, &i, 18);
	    found = open_var_(choice, dmin, dmax, dom_units, dom_dunits, delta,
			      label, &one, strlen(choice), 80, 80, 80);
	    setivar_("multiple_read", &mr, &i, 13);
	    setivar_("multiple_read_flag", &flag, &i, 18);
	    if (found > 0 && dmin[val] != dmax[val] ) {
		int min=(int) dmin[val], max=(int) dmax[val];

		if (mr > -1 && flag == 0) {
		    getivar_("multiple_read_start", &dir_widget.start_val,
			     &i, 19);
		    getivar_("multiple_read_end", &dir_widget.end_val,
			     &i, 17);
		    if (dir_widget.start_val < min )
			dir_widget.start_val = min;
		    else if (dir_widget.start_val > max)
			dir_widget.start_val = max;
		    if (dir_widget.end_val < min)
			dir_widget.end_val = min;
		    else if (dir_widget.end_val > max)
			dir_widget.end_val = max;
		}
		else {
		    dir_widget.start_val = min;
		    dir_widget.end_val = max;
		}
		XtVaSetValues(dir_widget.start,
			      XmNminimum, min, XmNmaximum, max,
			      XmNvalue, dir_widget.start_val, NULL);
		XtVaSetValues(dir_widget.end,
			      XmNminimum, min, XmNmaximum, max,
			      XmNvalue, dir_widget.end_val, NULL);
		XtManageChild(dir_widget.start);
		XtManageChild(dir_widget.startlab);
		XtManageChild(dir_widget.end);
		XtManageChild(dir_widget.endlab);
		XtFree(choice);
		return;
	    }
	}
    }
    else {
	getivar_("multiple_read", &mr, &i, 13);
	if (mr > -1)
	    driver("multiple_read=n");
    }
    sprintf(buff,"file=%s",choice);
    driver(buff);
    XtFree(choice);
    done_callback(w, client, call);
}

void trans_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *old, *choice;
    Arg args[2];
    static Bool data=TRUE;
    char buff[512];


    if(client == NULL){
	update_fsb(&trans_widget.fsb, call);
	XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
	sprintf(buff,"trans=%s",choice);
	driver(buff);
	XtFree((choice));
    }
    
    done_callback(w, XtParent(w), call);
}


void ctb_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *old, *choice;
    Arg args[2];
    static Bool data=TRUE;
    char buff[512];


    if(client == NULL){
	update_fsb(&ctb_widget.fsb, call);
	XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
	sprintf(buff,"READCOLORTABLE=%s",choice);
	driver(buff);
	free((choice));
    }
    
    done_callback(w, XtParent(w), call);
}

void wctb_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *old, *choice;
    Arg args[2];
    static Bool data=TRUE;
    char buff[512];


    if(client == NULL){
	update_fsb(&wctb_widget.fsb, call);
	XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
	sprintf(buff,"WRITECOLORTABLE=%s",choice);
	driver(buff);
	free((choice));
    }
    
    done_callback(w, XtParent(w), call);
}


void file_help_callback(w, client, call)
     Widget w;
     caddr_t *client;
     XmAnyCallbackStruct call;
{
    make_help_widget(
"To select a file  click on the file name then\n\
the OK button using the first mouse button, or\n\
double click on the file name. To change directory\n\
double click on the directory name in the left column.\n\
To change between data and constants file toggle the\n\
appropriate switch using the first mouse button");
}


void input_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *choice=NULL;
    char buff[512];

    update_fsb(&input_widget.fsb, call);
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
    sprintf(buff,"input=%s",choice);
    if(choice)XtFree((choice));
    driver(buff);
    done_callback(w, XtParent(w), call);
}

void dump_type_callback(w, client, call)
    Widget w;
    int *client;
    XmAnyCallbackStruct *call;
{
    XmString oldlab;
    static XmString xwd=NULL, gif=NULL;

    if (xwd == NULL)
	xwd = XmStringCreate("XWD", XmSTRING_DEFAULT_CHARSET);
    if (gif == NULL)
	gif = XmStringCreate("GIF", XmSTRING_DEFAULT_CHARSET);
    XtVaGetValues(w, XmNlabelString, &oldlab, NULL);
    if (XmStringCompare(xwd, oldlab)) {
	XtVaSetValues(w, XmNlabelString, gif, NULL);
	dump_widget.type = "GIF";
    }
    else {
	XtVaSetValues(w, XmNlabelString, xwd, NULL);
	dump_widget.type = "XWD";
    }
    XmStringFree(oldlab);
}

void dump_what_callback(w, client, call)
    Widget w;
    int *client;
    XmAnyCallbackStruct *call;
{
    XmString oldlab;
    static XmString plot=NULL, loop=NULL;

    if (plot == NULL)
	plot = XmStringCreate("plot", XmSTRING_DEFAULT_CHARSET);
    if (loop == NULL)
	loop = XmStringCreate("loop", XmSTRING_DEFAULT_CHARSET);
    XtVaGetValues(w, XmNlabelString, &oldlab, NULL);
    if (XmStringCompare(plot, oldlab) && loop_form) {
	XtVaSetValues(w, XmNlabelString, loop, NULL);
	dump_widget.what = "loop";
    }
    else {
	XtVaSetValues(w, XmNlabelString, plot, NULL);
	dump_widget.what = "plot";
    }
    XmStringFree(oldlab);
}


void dump_done_callback(w, client, call)
    Widget w;
    caddr_t client;
    XmAnyCallbackStruct *call;
{
    XtUnmanageChild(XtParent(XtParent(w)));
}

void dump_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *choice, *what;
    char buff[512];

    update_fsb(&dump_widget.fsb, call);
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
    if (strcmp(dump_widget.what, "plot") == 0) what = "dump";
    else what = "loop_dump";
    sprintf(buff,"%s=%s,%s",what, choice, dump_widget.type);
    driver(buff);
    free((choice));
    dump_done_callback(w, client, call);
}

void dump_help_callback(w, client, call)
    Widget w;
    caddr_t client;
    XmAnyCallbackStruct *call;
{
    if (strcmp(dump_widget.what, "plot")==0) {driver("help=dump");}
    else {driver("help=loop_dump");}
}

void log_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char *choice;
    char buff[512];

    update_fsb(&log_widget.fsb, call);
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
    sprintf(buff,"log=%s",choice);
    driver(buff);
    free((choice));
    done_callback(w, XtParent(w), call);
}

void eps_ok_callback(w, client, call)
     Widget w;
     int *client;
     XmFileSelectionBoxCallbackStruct *call;
{
    char buff[512];
    char *choice;

    update_fsb(&eps_widget.fsb, call);
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&choice);
    sprintf(buff,"write_frame=%s",choice);
    driver(buff);
    XFree(choice);
    done_callback(w, XtParent(XtParent(w)), call);
}

void eps_color_callback(w, client, call)
     Widget w;
     int *client;
     XmAnyCallbackStruct *call;
{

    XmString oldlab,on,off;
    on = NewString("Yes");
    off = NewString("No ");
    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(off,oldlab)){
        XtVaSetValues(w,XmNlabelString,on, NULL);
        driver("eps_color=on");
    }
    else{
        XtVaSetValues(w,XmNlabelString,off, NULL);
        driver("eps_color=off");
    }
    XmStringFree(oldlab);
}

void dir_coord_call(w, data, call)
    Widget w;
    int data;
    XmToggleButtonCallbackStruct *call;
{
    dir_widget.coord = data;
}

void dir_start_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    int val;

    XmScaleGetValue(w, &val);
    val = fix_file_value(val);
    XmScaleSetValue(dir_widget.start,val);
    dir_widget.start_val = val;
}

void dir_end_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    int val;

    XmScaleGetValue(w, &val);
    val = fix_file_value(val);
    XmScaleSetValue(dir_widget.end,val);
    dir_widget.end_val = val;
}

void dir_start_type_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    float val;
    int ival;
    char *str;

    str = XmTextGetString(w);
    val = atof(str);
    ival = (int)val;
    XtDestroyWidget(w);
    XtManageChild(dir_widget.start);
    XmScaleSetValue(dir_widget.start,ival);
    dir_widget.start_val = val;
}

void dir_endv_type_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    float val;
    int ival;
    char *str;

    str = XmTextGetString(w);
    val = atof(str);
    ival = (int) val;
    XtDestroyWidget(w);
    XtManageChild(dir_widget.end);
    XmScaleSetValue(dir_widget.end, ival);
    dir_widget.end_val = val;
}

void dir_start_type_in(widg,data,ev)
     Widget widg;
     int data;
     XButtonEvent *ev;
{
    Widget w;
    void check_num2();
    void text_box_motion();
    unsigned long root,child;
    int x,y, rx,ry;
    unsigned int ret;
  
    XQueryPointer(XtDisplay(widg),XtWindow(XtParent(widg)),&root,&child,
                  &rx,&ry,&x,&y,&ret);
    XtUnmanageChild(widg);
    
    w=XtVaCreateManagedWidget("VALUE",xmTextFieldWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,dir_start_type_call,&w);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,NULL);

}

void dir_end_type_in(widg,data,ev)
     Widget widg;
     int data;
     XButtonEvent *ev;
{
    Widget w;
    void check_num2();
    void text_box_motion();
    unsigned long root,child;
    int x,y, rx,ry;
    unsigned int ret;
  
    XQueryPointer(XtDisplay(widg),XtWindow(XtParent(widg)),&root,&child,
                  &rx,&ry,&x,&y,&ret);
    XtUnmanageChild(widg);
    
    w=XtVaCreateManagedWidget("VALUE",xmTextWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,dir_endv_type_call,&w);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,NULL);

}
