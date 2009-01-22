/****************************************************************************
$Log: forms_callbacks.c,v $
Revision 1.55  2002/08/09 19:57:26  warren
Fixed for Linux and tested with DEC sun etc.
most items inside #ifdef's, or were general cleanup/fixes

Revision 1.54  2002/06/14 19:43:50  harryive
Allow specification of terrain contours by value.

Revision 1.53  2002/05/22 17:56:16  davidive
First working version of trajectories colored by alternative fields
There is not a complete interface yet!!!

Revision 1.52  2001/11/15 21:36:43  davidive
added entry blanks for setting the mid val, step size and color mid for
trajectories

Revision 1.51  2001/08/15 22:23:20  davidive
Cleaned up code to reduce warnings about type mismatches, and added buttons
and callbacks to the 1d form widget to select which access to label against
for diagonal plots.

Revision 1.50  1999/02/04 00:28:05  harry
Add vector scaling.

Revision 1.49  1999/01/29 20:45:20  warren
new stuff to deal with the trajectory form

Revision 1.48  1998/12/17 23:16:58  warren
Cleanup of pointer casts to long instead of int, freeing XmStrings when no
longer in use, passing all args to sprinf's and making correctly casting NULLs

Revision 1.47  1998/09/04 20:51:43  harry
Add colorbar button to annotation widget.  Rewrite file widgets to
remember previous entries.

Revision 1.46  1997/10/22 19:34:33  davidive
Turned the particle movement part of trajectories into a user transform
and added unit checking to the default transform.

Revision 1.45  1997/06/27 21:06:25  harry
Designed multiplot printing.

Revision 1.44  1997/06/03 19:13:45  davidive
The big mouse slicing changeover. We have the new form, only requires you
set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
always runs through init_points_ to catch all these things.
note: only actually works when two dimentions have points set. Needs to
be expanded into 1, 3 or 4.

Revision 1.43  1997/02/06 17:06:48  warren
Fixed the color fill widget lock up problem.

 * Revision 1.42  1997/01/29  23:08:04  warren
 * Cleaned up and fixed Dales Color problem
 *
Revision 1.41  1996/12/23 20:57:57  warren
Landscape printing option

Revision 1.40  1996/10/25 18:09:10  harry
Missed a couple of calls to XtAddCallBack with only 3 arguments.

Revision 1.39  1996/10/18 23:22:51  harry
Fix declarations and calls to make SGI compiler happy.

 * Revision 1.38  1996/10/11  01:12:02  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
Revision 1.37  1996/08/09 19:28:06  harry
Take out %.2f formats and replace with calls to float_nozero.

Revision 1.36  1996/04/25 16:24:16  harry
Add Skew-T plots and the beginning of trajectory plots.

Revision 1.35  1996/04/23 18:16:56  warren
Changes for terrain

Revision 1.34  1996/04/01 20:23:08  warren
Terrain form and new terrain command (on / off)

Revision 1.33  1996/01/12 16:10:55  warren
Transfer of new pattern stuff from 3.x and beginning of color fixes
(color vectors, correct collors on the pulldowan menus)

Revision 1.32  1995/12/15 22:32:34  warren
moved patterns into 4.0 and implemented the list of fixes and changes from
Curtis.

 *Revision 1.31  1995/10/19 22:13:09  harry
 *Set defaults by clicking right mouse button.  Make all TextWidgets into
 *TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.30  1995/10/18  19:16:50  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.29  1995/07/10  17:16:03  warren
 * Finished renaming widgets so the help key will work. Also fixed the color
 * menus not to contain an _ in the boxes and fixed the second vector
 * componant update proble.
 *
 * Revision 1.28  1995/03/20  23:56:07  warren
 * Fixed the on off button to work correctly for plot labels
 *
 * Revision 1.27  1995/02/14  22:21:33  warren
 * Fixed color lock loop and did proper cast to int of XPointer for Alphas.
 *
 * Revision 1.26  1994/12/20  22:26:45  warren
 * made the rest of the scale widgets typing capable
 *
 * Revision 1.25  1994/11/22  22:46:33  harry
 * Correct vector locking, add support for vector positioning, add
 * support for filled contour overlays.
 *
 * Revision 1.24  1994/10/24  17:11:28  warren
 * Fixed tic labels normal button call to work.
 *
 * Revision 1.23  1994/10/08  19:42:33  warren
 * DEC compiler found a missing cast.
 *
 * Revision 1.22  1994/10/08  18:59:51  warren
 * The annotation widget and updating it.
 *
 * Revision 1.21  1994/09/14  18:19:11  harry
 * Big 1D flip uncommit.
 *
 * Revision 1.20  1994/06/20  16:35:30  harry
 * Change index to strchr, and rindex to strrchr.
 *
 * Revision 1.19  1994/05/06  17:09:02  yuri
 * Main vector lock commit + small correction in X/ive_ui_update.c file
 *
 * Revision 1.18  1994/04/19  23:01:35  yuri
 * Big 1D flip commit.
 *
 * Revision 1.17  1993/12/15  18:11:13  harry
 * Update map background widget - add default button and clipping menu.
 *
 * Revision 1.16  1993/11/29  19:37:50  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.15  1993/11/03  23:32:55  warren
 * General cleanup for beginning of port to alpha.
 *
 * Revision 1.14  1993/10/26  22:15:59  warren
 * permit e notation in text boxes
 *
 * Revision 1.13  1993/10/05  17:51:41  warren
 * propogate fix from 3.0 for commas -n contour values.
 *
 * Revision 1.12  1993/10/04  22:19:23  warren
 * Fixed vector sliders and do not allow multiple contour intervals.
 *
 * Revision 1.11  1993/09/08  23:14:26  warren
 * Print widget code.
 *
 * Revision 1.10  1993/08/17  18:33:51  warren
 * Beginning of printer forms.
 *
 * Revision 1.9  1993/07/29  00:29:30  warren
 * Added button for vector_lock command
 *
 * Revision 1.8  1993/07/09  16:36:08  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.7  1993/06/25  20:05:54  warren
 * Do updates tp map widget.
 *
 * Revision 1.6  1993/06/18  16:46:01  warren
 * cleanup, reorganization, fixes to line form widget calls.
 *
 * Revision 1.5  1993/06/05  00:20:59  warren
 * Fixes to slicer and forms updating needlessly and wrong.
 *
 * Revision 1.4  1993/06/01  23:59:02  warren
 * Fixed Vector component callback and Vector Interval Callback.
 *
 * Revision 1.3  1993/06/01  21:20:09  warren
 * added missing vector callback info
 *
 * Revision 1.2  1993/05/20  17:24:31  warren
 * Additions and changes to make the new forms active.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:19  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.16  1993/01/05  20:13:51  warren
 * added check_num2 that will work for surface
 *
 * Revision 2.15  1992/12/23  18:55:43  harry
 * Fix loop_text_boxes.
 *
 * Revision 2.14  1992/12/18  00:27:57  harry
 * Fix C error.
 *
 * Revision 2.13  1992/12/18  00:25:43  harry
 * Fix check_num to immediately return if call->event is NULL.
 * (i.e. changed not caused by type-in)
 *
 * Revision 2.12  1992/12/17  22:57:26  warren
 * swaped setting tracking box aand swap_fg to avoid infinite loop.
 *
 * Revision 2.11  1992/12/16  21:19:27  warren
 * fixed reverse video on loop widget
 *
 * Revision 2.10  1992/12/04  00:51:38  warren
 * fixed the reverse video for the loop widget
 *
 * Revision 2.9  1992/11/19  00:56:26  warren
 * magic dissapearing text at the correct times in text boxes.
 *
 * Revision 2.8  1992/11/17  20:02:02  millerp
 * removed extra }
 *
 * Revision 2.7  1992/11/17  19:39:31  warren
 * minor cleanup
 *
 * Revision 2.6  1992/11/17  17:21:29  warren
 * reverse video on text boxes that haven't been finished yet
 *
 * Revision 2.5  1992/11/09  18:15:50  warren
 * fixed for MOTIF 1.2
 *
 * Revision 2.4  1992/10/06  18:26:45  warren
 * added xtics_line and ytics_line and plot types scaler and vector
 *
 * Revision 2.3  1992/09/30  17:29:38  warren
 * removed sounding, average and vindex, added auto type selection to lock
 * buttons
 *
 * Revision 2.2  1992/09/25  21:16:02  warren
 * fixed axes scaling so they actually set something.
 *
 * Revision 2.1  1992/09/25  21:01:09  warren
 * changed ; to , inside commands
 *
 * Revision 2.0  1992/07/01  21:11:26  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.5  1992/06/25  19:16:32  warren
 * fixed axes scaling for line plots
 *
 * Revision 1.4  1992/06/15  19:00:51  warren
 * added average for line plots
 *
 * Revision 1.3  1992/06/09  17:09:31  warren
 * changable contour_opts button
 *
 * Revision 1.2  1992/05/06  21:52:00  warren
 * new map info form
 *
 * Revision 1.1  1992/04/21  00:08:28  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: forms_callbacks.c,v 1.55 2002/08/09 19:57:26 warren Exp $";
/*
 #define DBG 1
*/
#include<stdio.h>
#include<stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <Xm/XmAll.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#if (LSB == 5)
#include <lsb5/math.h>
#elif (LSB == 4)
#include <lsb4/math.h>
#elif (LSB == 3)
#include <lsb3/math.h>
#elif (LSB == 2)
#include <lsb2/math.h>
#elif (LSB == 1)
#include <lsb1/math.h>
#else
#include <math.h>
#endif
#define use_text_arrays 1
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>
#include <ive_widgets.h>
#include <properties.h>

XmString NewString();

static XmString on=NULL,off=NULL;
extern void update_all_(),getrvar_(),getivar_(),getrarr_();
char * float_nozero(number)
    float number;
{
    static char result[64];
    char *decimal;

    sprintf(result, "%f", number);
    if ((decimal=strchr(result, '.'))) {
	char *p = result + strlen(result)-1;
	for(; p > decimal, *p == '0'; p--);
	if (p == decimal) *p = '\0';
	else *(p+1) = '\0';
    }
    return(result);
}

void swap_fg(w)
     Widget w;
{
    unsigned long fg, bg, tmp;
    Arg args[2];
    
    XtSetArg(args[0], XmNforeground, &fg);
    XtSetArg(args[1], XmNbackground, &bg);
    XtGetValues(w, args, 2);
    XtSetArg(args[0], XmNforeground, bg);
    XtSetArg(args[1], XmNbackground, fg);
    XtSetValues(w, args, 2);
    (void)update_all_();
}

void form_cint_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    
    char *str,buff[256],buff2[256];
    Bool set;
    float rvar;
    int error;
    int ivar;
    


    if(data==1){
	(void)getrvar_("cont_int",&rvar, &error,8);
	strcpy(buff, float_nozero(rvar));
	XtVaSetValues(Properties.cont_values, XmNvalue, buff, NULL);
	snprintf(buff2,255,"contour_interval=%s",buff);
	driver(buff2);
    }
    else if (data == 2){
	int i;
	float rarr[40];
	(void)getivar_("num_vals", &ivar, &error, 8);
	if (ivar < 0) ivar = -ivar;
	if(ivar){
	    (void)getrarr_("cont_values", rarr, &ivar, &error, 11);
	    buff[0]='\0';
	    buff2[0]='\0';
	    strcpy(buff, float_nozero(rarr[0]));
	    snprintf(buff2,255,"contour_values=%s",buff);
	    for (i=1; i<ivar; i++){
		char *buff3;

		buff3 = float_nozero(rarr[i]);
		snprintf(buff,255,"%s,%s",buff,buff3);
		snprintf(buff2,255,"%s,%s",buff2,buff3);
	    }
	    XtVaSetValues(Properties.cont_values, XmNvalue, buff, NULL);
	    driver(buff2);
	}
	else{
	    snprintf(buff,255,"0");
	    XtVaSetValues(Properties.cont_values, XmNvalue, buff, NULL);
	    driver("contour_values=0");
	}
    }
    else{
	set = XmToggleButtonGetState(Properties.cont_int);
	str = XmTextFieldGetString(Properties.cont_values);
	if(str[0] == '\0')return;
	if(set == TRUE)
	  snprintf(buff,255,"contour_interval=%s",str);
	else
	  snprintf(buff,255,"contour_values=%s",str);
	driver(buff);
	free(str);
    }
}

void form_tcint_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    
    char *str,buff[256],buff2[256];
    Bool set;
    float rvar;
    int error;
    int ivar;
    
    if(data==1){
	(void)getrvar_("tcint",&rvar, &error,5);
	strcpy(buff, float_nozero(rvar));
	XtVaSetValues(terrain_forms.cont_values, XmNvalue, buff, NULL);
	sprintf(buff2,"tcontour_interval=%s",buff);
	driver(buff2);
    }
    else if (data == 2){
	int i;
	float rarr[40];
	(void)getivar_("tnum_vals", &ivar, &error, 9);
	if (ivar < 0) ivar = -ivar;
	if(ivar){
	    (void)getrarr_("tcont_values", rarr, &ivar, &error, 12);
	    buff[0]='\0';
	    buff2[0]='\0';
	    strcpy(buff, float_nozero(rarr[0]));
	    sprintf(buff2,"tcontour_values=%s",buff);
	    for (i=1; i<ivar; i++){
		char *buff3;

		buff3 = float_nozero(rarr[i]);
		sprintf(buff,"%s,%s",buff,buff3);
		sprintf(buff2,"%s,%s",buff2,buff3);
	    }
	    XtVaSetValues(terrain_forms.cont_values, XmNvalue, buff, NULL);
	    driver(buff2);
	}
	else{
	    sprintf(buff,"0");
	    XtVaSetValues(terrain_forms.cont_values, XmNvalue, buff, NULL);
	    driver("tcontour_values=0");
	}
    }
    else{
	set = XmToggleButtonGetState(terrain_forms.cont_int);
	str = XmTextFieldGetString(terrain_forms.cont_values);
	if(str[0] == '\0')return;
	if(set == TRUE)
	  sprintf(buff,"tcontour_interval=%s",str);
	else
	  sprintf(buff,"tcontour_values=%s",str);
	driver(buff);
	free(str);
    }
}

void form_terrain_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;
    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("terrain=on");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("terrain=off");
    }
    XmStringFree(oldlab);
}

void form_traj_ind_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;
    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("traj_ind=yes");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("traj_ind=no");
    }
    XmStringFree(oldlab);
}

void form_traj_2d_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;
    if(on==NULL)
      on = NewString("2D");
    if(off==NULL)
      off = NewString("3D ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("traj_2d=yes");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("traj_2d=no");
    }
    XmStringFree(oldlab);
}

void form_label_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;
    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("label=on");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("label=off");
    }
    XmStringFree(oldlab);
}

void form_label2_call(w, data, call) /* to get on off on annotation form*/
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab,str,off;
    off = NewString("Off");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	str = NewString("On ");
	XtVaSetValues(w,XmNlabelString,str,NULL);
	driver("label=on");
    }
    else{
	XtSetArg(args[0],XmNlabelString,on);
	str = NewString("Off");
	XtVaSetValues(w,XmNlabelString,str,NULL);
	driver("label=off");
    }
    XmStringFree(oldlab);
    XmStringFree(off);
    XmStringFree(str);
}

void form_colorbar_call(w, data, call) /* to get on off on annotation form*/
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab,str,off;
    off = NewString("Off");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	str = NewString("On ");
	XtVaSetValues(w,XmNlabelString,str,NULL);
	driver("color_bar=on");
    }
    else{
	XtSetArg(args[0],XmNlabelString,on);
	str = NewString("Off");
	XtVaSetValues(w,XmNlabelString,str,NULL);
	driver("color_bar=off");
    }
    XmStringFree(oldlab);
    XmStringFree(off);
    XmStringFree(str);
}
    
void tic_labels_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Boolean XmToggleButtonGetState();
    
    if(XmToggleButtonGetState(w))
      switch((long)data){
	case 0:
	  driver("tic_labels = off");
	  break;
	case 1:
	  driver("tic_labels=normal");
	  break;
	case 2:
	  driver("tic_labels=bold");
	  break;
      }
}
    
void form_line_width_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{

    char buff[80];
    snprintf(buff,79,"linewidth=%s",data);
    driver(buff);
}


void form_nunlablines_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
      char buff[80];
      int val;

      XmScaleGetValue(w,&val);
      if(val == 0)
	snprintf(buff,79,"linelabel=off");
      else
	snprintf(buff,79,"linelabel=on;nulbll=%d",val-1);
      driver(buff);
}

void form_numterr_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
      char buff[80];
      int val;

      XmScaleGetValue(w,&val);
      snprintf(buff,79,"terr_nulbll=%d",val);
      driver(buff);
}
  

void form_filltype_call(w, data, call)
     Widget w;
     long data;
     XmAnyCallbackStruct *call;
{
    static XmString first=NULL, all=NULL, never=NULL, pattern=NULL, solid=NULL;
    XmString oldlab;

    if(first==NULL)
	first = NewString("Except On Overlay");
    if(all==NULL)
	all = NewString("Always");
    if(never==NULL)
	never= NewString("Never");
    if(pattern==NULL)
      pattern= NewString("With Stipple");
    if(solid==NULL)
      solid= NewString("Solid");
    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(pattern,oldlab)){
	XtVaSetValues(w,XmNlabelString,solid,NULL);
	driver("fill_type=solid");
	}
    else if(XmStringCompare(solid,oldlab)){
	XtVaSetValues(w,XmNlabelString,pattern,NULL);
	driver("fill_type=pattern");
    }
    else if (XmStringCompare(first,oldlab)){
	XtVaSetValues(w,XmNlabelString,all,NULL);
	    driver("fill_contours=always");
    }
    else if (XmStringCompare(all,oldlab)){
	XtVaSetValues(w,XmNlabelString,never,NULL);
	    driver("fill_contours=never");
    }
    else if (XmStringCompare(never,oldlab)){
	XtVaSetValues(w,XmNlabelString,first,NULL);
	    driver("fill_contours=except_overlay");
    }
    XmStringFree(oldlab);
    
}


void form_zero_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;
    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");

    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(on,oldlab)){
	XtVaSetValues(w, XmNlabelString,off,NULL);
	driver("nozero=on");
    }
    else{
	XtVaSetValues(w, XmNlabelString,on,NULL);
	driver("nozero=off");
    }
    XmStringFree(oldlab);
}
 
void tic_scale_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    if(!strcmp(str,"Default"))
      snprintf(buff,79,"tic_scale=-1");
    else
      snprintf(buff,79,"tic_scale=%s",str);
    driver(buff);
}

void tic_decimals_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    snprintf(buff,79,"tic_decimals=%s,",str);
    driver(buff);
}
void tic_widthmaj_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    Boolean XmToggleButtonGetState();
    
    if(XmToggleButtonGetState(w))
      switch(data){
	case 0:
	  driver("tic_width = thin");
	  break;
	case 1:
	  driver("tic_width = thick");
	  break;
	case 2:
	  driver("tic_width = thickest");
	  break;
      }
}

void tic_widthmin_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    Boolean XmToggleButtonGetState();
    
    if(XmToggleButtonGetState(w))
      switch(data){
	case 0:
	  driver("tic_width = ,thin");
	  break;
	case 1:
	  driver("tic_width = ,thick");
	  break;
	case 2:
	  driver("tic_width = ,thickest");
	  break;
      }
}

void form_tick_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

   

    str=XmTextGetString(w);
    switch(*data){
      case 'X':
	  snprintf(buff,79,"xtics=%s,",str);
	break;
      case 'x':
	  snprintf(buff,79,"xtics=,%s",str);
	break;
      case 'Y':
	  snprintf(buff,79,"ytics=%s,",str);
	break;
      case 'y':
	  snprintf(buff,79,"ytics=,%s",str);
	break;
    }
    driver(buff);
}

void done_pparent_callback(w, client, call)
     Widget w;
     caddr_t client;
     XmAnyCallbackStruct *call;
{
    XtDestroyWidget(XtParent(XtParent(w)));
}


void form_drawmap_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;
    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL) 
      off = NewString("No ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("drawmap=on");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("drawmap=off");
    }
    XmStringFree(oldlab);
}

void form_mapoutlines_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;

    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("mapoutlines=co");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("mapoutlines=no");
    }
    XmStringFree(oldlab);
}

void form_defaultmap_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString oldlab;

    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(off,oldlab)){
	XtSetArg(args[0],XmNlabelString,on);
	XtSetValues(w,args,1);
	driver("default_map=on");
    }
    else{
	XtSetArg(args[0],XmNlabelString,off);
	XtSetValues(w,args,1);
	driver("default_map=off");
    }
    XmStringFree(oldlab);
}

void form_plat_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    snprintf(buff,79,"plat=%s",str);
    XtFree(str);
    driver(buff);
}

void form_plon_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    snprintf(buff,79,"plon=%s",str);
    XtFree(str);
    driver(buff);
}

void form_rota_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    snprintf(buff,79,"rota=%s",str);
    XtFree(str);
    driver(buff);
}

void form_glspace_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    snprintf(buff,79,"grid=%s",str);
    XtFree(str);
    driver(buff);
}

void cleanup_box_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    int form, box;
    XmTextSetInsertionPosition (w, 0);
    form = (data - data%100)/100;
    box = data%100;
    switch (form){
    case CONTOUR_TEXT_BOXES:
	if(contour_text_boxes[box]){
	    contour_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case MAP_TEXT_BOXES :
	if(map_text_boxes[box]){
	    map_text_boxes[box] = 0;
		swap_fg(w);
	}
	break;
    case VECTOR_TEXT_BOXES:
	    if(vector_text_boxes[box]){
		vector_text_boxes[box] = 0;
		swap_fg(w);
	    }
	break;
    case SKEWT_TEXT_BOXES:
	    if(skewt_text_boxes[-box]){
		skewt_text_boxes[-box] = 0;
		swap_fg(w);
	    }
	break;
    case LINE_TEXT_BOXES:
	if(line_text_boxes[box]){
	    line_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case LOOP_TEXT_BOXES:
	if(loop_text_boxes[box]){
	    loop_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case TWO_TEXT_BOXES:
	if(two_text_boxes[box]){
	    two_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case FOUR_TEXT_BOXES:
	if(four_text_boxes[box]){
	    four_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case SIX_TEXT_BOXES:
	if(six_text_boxes[box]){
	    six_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case ANNOTATION_TEXT_BOXES:
	if(annotation_text_boxes[box]){
	    annotation_text_boxes[box] = 0;
	    swap_fg(w);
	}
	    break;
    case STIPPLE_TEXT_BOXES:
	if(stipple_text_boxes[box]){
	    stipple_text_boxes[box] = 0;
	    swap_fg(w);
	}
	break;
    case TERRAIN_TEXT_BOXES:
	if(terrain_text_boxes[-box]){
	    terrain_text_boxes[-box] = 0;
	    swap_fg(w);
	}
	break;
    case POINT_TEXT_BOXES:
	if(point_text_boxes[-box]){
	    point_text_boxes[-box] = 0;
	    swap_fg(w);
	}
	break;
    case TRAJ_TEXT_BOXES:
	if(traj_text_boxes[-box]){
	    traj_text_boxes[-box] = 0;
	    swap_fg(w);
	}
	break;
    case COLOR_TEXT_BOXES:
	if(color_text_boxes[-box]){
	    color_text_boxes[-box] = 0;
	    swap_fg(w);
	}
	break;
    }
}

void cleanup_form_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    int  box;
	switch (data){
	  case CONTOUR_TEXT_BOXES:
	    for(box = 0; box < MAX_CONTOUR_BOXES; box++){
		contour_text_boxes[box] = 0;
	    }
	    break;
	  case MAP_TEXT_BOXES :
	    for(box = 0; box < MAX_MAP_BOXES; box++){
		map_text_boxes[box] = 0;
	    }
	    break;
	  case VECTOR_TEXT_BOXES:
	    for(box = 0; box < MAX_VECTOR_BOXES; box++){
		vector_text_boxes[box] = 0;
	    }
	    break;
	  case SKEWT_TEXT_BOXES:
	    for(box = 0; box < MAX_SKEWT_BOXES; box++){
		skewt_text_boxes[box] = 0;
	    }
	    break;
	  case LINE_TEXT_BOXES:
	    for(box = 0; box < MAX_LINE_BOXES; box++){
		line_text_boxes[box] = 0;
	    }
	    break;
	  case LOOP_TEXT_BOXES:
	    for(box = 0; box < MAX_LOOP_BOXES; box++){
		loop_text_boxes[box] = 0;
	    }
	    break;
	  case TWO_TEXT_BOXES:
	    for(box = 0; box < MAX_TWO_BOXES; box++){
		two_text_boxes[box] = 0;
	    }
	    break;
	  case FOUR_TEXT_BOXES:
	    for(box = 0; box < MAX_FOUR_BOXES; box++){
		four_text_boxes[box] = 0;
	    }
	    break;
	  case SIX_TEXT_BOXES:
	    for(box = 0; box < MAX_SIX_BOXES; box++){
		six_text_boxes[box] = 0;
	    }
	    break;
	  case ANNOTATION_TEXT_BOXES:
	    for(box = 0; box < MAX_ANNOTATION_BOXES; box++){
		annotation_text_boxes[box] = 0;
	    }
	    break;
	  case STIPPLE_TEXT_BOXES:
	    for(box = 0; box < MAX_STIPPLE_BOXES; box++){
		stipple_text_boxes[box] = 0;
	    }
	    break;
	  case POINT_TEXT_BOXES:
	    for(box = 0; box < MAX_POINT_BOXES; box++){
		point_text_boxes[box] = 0;
	    }
	    break;
	  case TRAJ_TEXT_BOXES:
	    for(box = 0; box < MAX_TRAJ_BOXES; box++){
		traj_text_boxes[box] = 0;
	    }
	    break;
	  case COLOR_TEXT_BOXES:
	    for(box = 0; box < MAX_COLOR_BOXES; box++){
		color_text_boxes[box] = 0;
	    }
	    break;
	}
}

void text_box_motion(w, data, call)
     Widget w;
     caddr_t data;
     XmTextVerifyCallbackStruct *call;
{
    int len = 0;
    int form, box;
    form = ((long)data - (long)data%100)/100;
    box = (long)data%100;
    switch (form){
    case CONTOUR_TEXT_BOXES:
      if(!contour_text_boxes[box]){
	contour_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case  MAP_TEXT_BOXES :
      if(!map_text_boxes[box]){
	map_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case VECTOR_TEXT_BOXES:
      if(!vector_text_boxes[box]){
	vector_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case SKEWT_TEXT_BOXES:
      if(!skewt_text_boxes[-box]){
	skewt_text_boxes[-box] = 1;
	swap_fg(w);
      }
      break;
    case LINE_TEXT_BOXES:
      if(!line_text_boxes[box]){
	line_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case LOOP_TEXT_BOXES:
      if(!loop_text_boxes[box]){
	loop_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case TWO_TEXT_BOXES:
      if(!two_text_boxes[box]){
	two_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case FOUR_TEXT_BOXES:
      if(!four_text_boxes[box]){
	four_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case SIX_TEXT_BOXES:
      if(!six_text_boxes[box]){
	six_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case ANNOTATION_TEXT_BOXES:
      if(!annotation_text_boxes[box]){
	annotation_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case STIPPLE_TEXT_BOXES:
      if(!stipple_text_boxes[box]){
	stipple_text_boxes[box] = 1;
	swap_fg(w);
      }
      break;
    case TERRAIN_TEXT_BOXES:
      if(!terrain_text_boxes[-box]){
	terrain_text_boxes[-box] = 1;
	swap_fg(w);
      }
      break;
    case POINT_TEXT_BOXES:
      if(!point_text_boxes[-box]){
	point_text_boxes[-box] = 1;
	swap_fg(w);
      }
      break;
    case TRAJ_TEXT_BOXES:
      if(!traj_text_boxes[-box]){
	traj_text_boxes[-box] = 1;
	swap_fg(w);
      }
      break;
    case COLOR_TEXT_BOXES:
      if(!color_text_boxes[-box]){
	color_text_boxes[-box] = 1;
	swap_fg(w);
      }
      break;
    }
}

void check_num(w, data, call)
     Widget w;
     int data;
     XmTextVerifyCallbackStruct *call;
{
    int len = 0;
    int form, box, firstchar;
    if(!XtIsRealized(w) || call->event == NULL)return;
    form = (data - data%100)/100;
    box = data%100;
    switch (form){
    case CONTOUR_TEXT_BOXES:
	if(!contour_text_boxes[box]){
		contour_text_boxes[box] = 1;
		swap_fg(w);
		if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	    }
	break;
    case MAP_TEXT_BOXES :
	if(!map_text_boxes[box]){
	    map_text_boxes[box] = 1;
		swap_fg(w);
	    if(call->text->length > 0){
		XmTextSetString(w,"");
	    }
	}
	break;
    case VECTOR_TEXT_BOXES:
	if(!vector_text_boxes[box]){
	    vector_text_boxes[box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	    }
	break;
    case SKEWT_TEXT_BOXES:
	if(!skewt_text_boxes[-box]){
	    skewt_text_boxes[-box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	    }
	break;
    case LINE_TEXT_BOXES:
	if(!line_text_boxes[box]){
	    line_text_boxes[box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		XmTextSetString(w,"");
	    }
	}
	break;
    case LOOP_TEXT_BOXES:
	if(!loop_text_boxes[box]){
	    loop_text_boxes[box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		XmTextSetString(w,"");
	    }
	}
	break;
    case ANNOTATION_TEXT_BOXES:
	if(!annotation_text_boxes[box]){
	    annotation_text_boxes[box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	}
	break;
    case STIPPLE_TEXT_BOXES:
	if(!stipple_text_boxes[box]){
	    stipple_text_boxes[box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	}
	break;
    case TERRAIN_TEXT_BOXES:
	if(!terrain_text_boxes[-box]){
	    terrain_text_boxes[-box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	}
	break;
    case POINT_TEXT_BOXES:
	if(!point_text_boxes[-box]){
	    point_text_boxes[-box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	}
	break;
    case TRAJ_TEXT_BOXES:
	if(!traj_text_boxes[-box]){
	    traj_text_boxes[-box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	}
	break;
    case COLOR_TEXT_BOXES:
	if(!color_text_boxes[-box]){
	    color_text_boxes[-box] = 1;
	    swap_fg(w);
	    if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	}
	break;
    }
    if (call->startPos < call->currInsert)
	return; /*hit backspace*/
	if (call->text->length == 0)
	    return; /*forward delete*/
    if(w == Properties.vcomp_1 || w == Properties.vcomp_2 ||
       w == Properties.scomp_1 || w == Properties.scomp_2 ||
       w == Properties.scomp_3 || w == Properties.traj_background){
	if((len = XmTextGetLastPosition(w))==256-16){
	    call->doit = FALSE;
	    return;
	}
	if(len + call->text->length >256-16){
	    call->text->ptr[256-16-len] = 0;
		call->text->length = strlen(call->text->ptr);
	}
	call->doit = TRUE;
	return;
    }
    if((call->text->ptr[0] == 'd' || call->text->ptr[0] == 'D') &&
       form != POINT_TEXT_BOXES){
	XtVaSetValues(w, XmNvalue, "default",NULL);
	call->doit = FALSE;
	return;
    }
    if(w == Properties.cont_values){
	Bool set;
	set = XmToggleButtonGetState(Properties.cont_int);
	if((len = XmTextGetLastPosition(w))==256-16){
	    call->doit = FALSE;
	    return;
	}
	if(len + call->text->length >256-16){
	    call->text->ptr[256-16-len] = 0;
	    call->text->length = strlen(call->text->ptr);
	}
	if(set && (strchr(call->text->ptr, ',') != NULL || 
		   strchr(call->text->ptr, ' ') != NULL)){
	    call->doit = FALSE;
	    return;
	}
	for (len = 0; len < call->text->length; len ++){
	    if(!isdigit(call->text->ptr[len]) && call->text->ptr[len] 
	       != '.'
	       && call->text->ptr[len] != '-' && call->text->ptr[len] 
	       != ',' && call->text->ptr[len] != 'e'){
		int i;
		for(i = len; i< (call->text->length -1); i++)
		    call->text->ptr[i] = call->text->ptr[i+1];
		call->text->length--;
		len--;
	    }
	}
    }
    else{
	if((len = XmTextGetLastPosition(w))==10){
	    call->doit = FALSE;
	    return;
	}
	if(len + call->text->length >10){
	    call->text->ptr[10-len] = 0;
	    call->text->length = strlen(call->text->ptr);
	}
	
	for (len = 0; len < call->text->length; len ++){
	    if(!isdigit(call->text->ptr[len]) && call->text->ptr[len] != '.'
	       && call->text->ptr[len] != '-' &&
	       call->text->ptr[len] != 'e'){
		int i;
		for(i = len; i< (call->text->length -1); i++)
		    call->text->ptr[i] = call->text->ptr[i+1];
		call->text->length--;
		len--;
	    }
	}
    }
}


void fix_option(w, data, call)
     Widget w;
     Widget *data;
     XmAnyCallbackStruct *call;
{
    Arg args[2];
    XtSetArg(args[0],XmNmenuHistory,*data);
    XtSetValues(XtParent(*data),args,1);
    XtSetArg(args[0],XmNmenuHistory,NULL);
    XtSetValues(XtParent(w),args,1);
}

void form_linlog(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    int linlog;
    int error;
    XmString str;
    static XmString linear=NULL;
    
    if(!linear)linear = (XmString)NewString("Linear");
    getivar_("linlog", &linlog, &error, 6);
    XtVaGetValues(w, XmNlabelString, &str, NULL);
    
    switch (linlog){
      case 1:
	if ( *data == 'h'){
	    if(XmStringCompare(str, linear)){
		driver("axes=linlin");
	    }
	    else{
		driver("axes=loglin");
	    }
	}
	else{
	    if(XmStringCompare(str, linear)){
		driver("axes=linlin");
	    }
	    else{
		driver("axes=linlog");
	    }
	}
	break;
      case 2:
	 if ( *data == 'h'){
	     if(XmStringCompare(str, linear)){
		 driver("axes=linlog");
	     }
	     else{
		 driver("axes=loglog");
	     }
	 }
	 else{
	     if(XmStringCompare(str, linear)){
		 driver("axes=linlin");
	     }
	     else{
		 driver("axes=linlog");
	     }
	 }
	break;
      case 3:
	if ( *data == 'h'){
	    if(XmStringCompare(str, linear)){
		driver("axes=linlin");
	    }
	    else{
		driver("axes=loglin");
	    }
	}
	else{
	    if(XmStringCompare(str, linear)){
		driver("axes=loglin");
	    }
	    else{
		driver("axes=loglog");
	    }
	}
	break;
       case 4:
	if ( *data == 'h'){
	    if(XmStringCompare(str, linear)){
		driver("axes=linlog");
	    }
	    else{
		driver("axes=loglog");
	    }
	}
	else{
	    if(XmStringCompare(str, linear)){
		driver("axes=loglin");
	    }
	    else{
	      driver("axes=loglog");
	  }
	}
	break;
    }
}


void form_axes_scale_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];
    Arg args[2];

    str=XmTextGetString(w);
    switch(*data){
      case 'O':
	snprintf(buff,79,"ordinate=,%s",str);
	break;
      case 'A':
	snprintf(buff,79,"abscissa=,%s",str);
	break;
      case 'o':
	snprintf(buff,79,"ordinate=%s,",str);
	break;
      case 'a':
	snprintf(buff,79,"abscissa=%s,",str);
	break;
      case 'T':
	snprintf(buff,79,"topo_scale=%s,",str);
	break;
    }
    driver(buff);
}

void form_vect_max_call(w, data, call)
     Widget w;
     void *data;
     XmAnyCallbackStruct *call;
{
    char *str,buff[80];

    str=XmTextGetString(w);
    snprintf(buff,79,"vector_max=%s,",str);
    driver(buff);
}

void form_vint_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char buff[80];
    int i,j;

    XmScaleGetValue(Properties.vintx,&i);
    XmScaleGetValue(Properties.vinty,&j);
    snprintf(buff,79,"vector_interval=%d,%d",i,j);
    driver(buff);
}

void form_traj_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char buff[80];
    int i,j;

    XmScaleGetValue(Properties.traj_ts,&i);
    XmScaleGetValue(Properties.traj_te,&j);
    snprintf(buff,79,"int_traj=%d,%d",i,j);
    driver(buff);
}

void form_stream_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    if(*data == 'S'){
	driver("streamline=on");
    }
    else{
	driver("streamline=off");
    }
	
}

void check_num2(w, data, call)
     Widget w;
     int data;
     XmTextVerifyCallbackStruct *call;
{
    int len = 0;
    if(!XtIsRealized(w) || call->event == NULL)return;

    if (call->startPos < call->currInsert)
      return; /*hit backspace*/
    if (call->text->length == 0)
      return; /*forward delete*/
    if((len = XmTextGetLastPosition(w))==10){
	call->doit = FALSE;
	return;
    }
    if(len + call->text->length >10){
	call->text->ptr[10-len] = 0;
	call->text->length = strlen(call->text->ptr);
    }
    
    for (len = 0; len < call->text->length; len ++){
	if(!isdigit(call->text->ptr[len]) && call->text->ptr[len] != '.'
	   && call->text->ptr[len] != '-' &&  call->text->ptr[len] != 's' &&
	   call->text->ptr[len] != 'S' ){
	    int i;
	    for(i = len; i< (call->text->length -1); i++)
	      call->text->ptr[i] = call->text->ptr[i+1];
	    call->text->length--;
	    len--;
	}
    }
}
void form_vcomp_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str1, *str2, buff[256];
    str1 = XmTextFieldGetString(Properties.vcomp_1);
    str2 = XmTextFieldGetString(Properties.vcomp_2);
    snprintf(buff,255,"vector_component=%s,%s",str1,str2);
    free(str1);
    free(str2);
    driver(buff);
}

void form_tcomp_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str1, *str2, *str3, buff[256];
    str1 = XmTextFieldGetString(Properties.tcomp_1);
    str2 = XmTextFieldGetString(Properties.tcomp_2);
    str3 = XmTextFieldGetString(Properties.tcomp_3);
    snprintf(buff,255,"trajectory_components=%s,%s,%s",str1, str2, str3);
    free(str1);
    free(str2);
    free(str3);
    driver(buff);
}

void form_traj_val_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str, buff[256];
    str = XmTextFieldGetString(Properties.traj_val);
    snprintf(buff,255,"traj_value=%s",str);
    free(str);
    driver(buff);
}

void form_traj_step_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str, buff[256];
    str = XmTextFieldGetString(Properties.traj_step);
    sprintf(buff,"traj_step=%s",str);
    free(str);
    driver(buff);
}

void form_traj_background_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str, buff[256];
    str = XmTextFieldGetString(Properties.traj_background);
    snprintf(buff,255,"traj_background=%s",str);
    free(str);
    driver(buff);
}

void form_scomp_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str, buff[256];
    str = XmTextFieldGetString(Properties.scomp_1);
    snprintf(buff,255,"skewt_component=%s",str);
    free(str);
    str = XmTextFieldGetString(Properties.scomp_2);
    snprintf(buff,255,"%s,%s",buff,str);
    free(str);
    str = XmTextFieldGetString(Properties.scomp_3);
    snprintf(buff,255,"%s,%s",buff,str);
    free(str);
    driver(buff);
}

void form_vect_lock_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    XmString oldlab;
    if(on==NULL)
	on = NewString("Yes");
    if(off==NULL)
	off = NewString("No ");

    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(off,oldlab)){
        XtVaSetValues(w,XmNlabelString,on,NULL);
        driver("vector_lock=on");
    }
    else{
        XtVaSetValues(w,XmNlabelString,off,NULL);
        driver("vector_lock=off");
    }
    XmStringFree(oldlab);
}

void form_vect_pos_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    static XmString head=NULL, center=NULL, tail=NULL;
    XmString oldlab;

    if(head==NULL)
	head = NewString("Head  ");
    if(center==NULL)
	center = NewString("Center");
    if(tail==NULL)
	tail = NewString("Tail  ");

    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(head,oldlab)){
        XtVaSetValues(w,XmNlabelString,center,NULL);
        driver("vector_pos=center");
    }
    else if (XmStringCompare(center,oldlab)){
        XtVaSetValues(w,XmNlabelString,tail,NULL);
        driver("vector_pos=tail");
    }
    else{
        XtVaSetValues(w,XmNlabelString,head,NULL);
        driver("vector_pos=head");
    }
    XmStringFree(oldlab);
}


void form_vect_scale_call(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    XmString oldlab;
    if(on==NULL)
	on = NewString("Yes");
    if(off==NULL)
	off = NewString("No ");

    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(off,oldlab)){
        XtVaSetValues(w,XmNlabelString,on,NULL);
        driver("scale_vector=on");
    }
    else{
        XtVaSetValues(w,XmNlabelString,off,NULL);
        driver("scale_vector=off");
    }
    XmStringFree(oldlab);
}
void form_color_lines_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    XmString oldlab;
    if(on==NULL)
      on = NewString("Yes");
    if(off==NULL)
      off = NewString("No ");
    XtVaGetValues(w,XmNlabelString,&oldlab,NULL);
    if (XmStringCompare(off,oldlab)){
	XtVaSetValues(w,XmNlabelString,on,NULL);
	driver("PRINT_COLOR=ON");
    }
    else{
	XtVaSetValues(w,XmNlabelString,off,NULL);
	driver("PRINT_COLOR=OFF");
    }
    XmStringFree(oldlab);
}

void form_printer_call(w, data, call)
     Widget w;
     caddr_t data;
     XmSelectionBoxCallbackStruct *call;
{
    char *choice,buff[256];
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,
		    &choice); 
    strcpy(printer_setup.current,choice);
    snprintf(buff,255,"printer_name=%s",choice);
    free(choice);
    driver(buff);
}

void numlines_type_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    int val;
    char *str, buff[256];

    str = XmTextGetString(w);
    val = atoi(str);
    XtDestroyWidget(w);
    XtManageChild(Properties.numlines);
    if(val == 0)
      snprintf(buff,255,"linelabel=off");
    else
      snprintf(buff,255,"linelabel=on;nulbll=%d",val-1);
    driver(buff);
    XmScaleSetValue(Properties.numlines, val);
}
    
void numterr_type_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
    int val;
    char *str, buff[256];

    str = XmTextGetString(w);
    val = atoi(str);
    if(val<0)val=0;
    if(val>6)val=6;
    XtDestroyWidget(w);
    XtManageChild(terrain_forms.numterr);
    snprintf(buff,255,"terr_nulbll=%d",val-1);
    driver(buff);
    XmScaleSetValue(terrain_forms.numterr, val);
}
    

void numlines_scale_type_in(widg,data,ev)
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
    
    w=XtVaCreateManagedWidget("VALUE",
			      xmTextFieldWidgetClass,XtParent(widg),
			      XmNcolumns,5,XmNx,x-15,XmNy,y-15,
			      NULL);
    XtAddCallback(w,XmNactivateCallback,
		  (XtCallbackProc)numlines_type_call, &w);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,NULL);
    XtAddEventHandler(w, ButtonPressMask, FALSE, check_default_handler, 0);
}

void numterr_scale_type_in(widg,data,ev)
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
    
    w=XtVaCreateManagedWidget("VALUE",
			      xmTextFieldWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,
		  (XtCallbackProc)numterr_type_call,&w);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,NULL);
    XtAddEventHandler(w, ButtonPressMask, FALSE, check_default_handler, 0);
}

void vint_type_call(w, data, call)
    Widget w;
    char *data;
    XmAnyCallbackStruct *call;
{
    int val;
    char *str, buff[256];

    str = XmTextGetString(w);
    val = atoi(str);
    XtDestroyWidget(w);
    if(*data == 'X'){
	XtManageChild(Properties.vintx);
	XmScaleSetValue(Properties.vintx, val);
	XmScaleGetValue(Properties.vinty,&val);
	snprintf(buff,255,"vector_interval=%s,%d",str,val);
    }
    else{
	XtManageChild(Properties.vinty);
	XmScaleSetValue(Properties.vinty, val);
	XmScaleGetValue(Properties.vintx,&val);
	snprintf(buff,255,"vector_interval=%d,%s",val,str);
    }
    driver(buff);
}

void traj_type_call(w, data, call)
    Widget w;
    char *data;
    XmAnyCallbackStruct *call;
{
    int val;
    char *str, buff[256];

    str = XmTextGetString(w);
    val = atoi(str);
    XtDestroyWidget(w);
    if(*data == 'S'){
	XtManageChild(Properties.traj_ts);
	XmScaleSetValue(Properties.traj_ts, val);
	XmScaleGetValue(Properties.traj_te,&val);
	snprintf(buff,255,"int_traj=%s,%d",str,val);
    }
    else{
	XtManageChild(Properties.traj_te);
	XmScaleSetValue(Properties.traj_te, val);
	XmScaleGetValue(Properties.traj_ts,&val);
	snprintf(buff,255,"int_traj=%d,%s",val,str);
    }
    driver(buff);
}



void vint_scale_type_in(widg,data,ev)
     Widget widg;
     char *data;
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
    
    w=XtVaCreateManagedWidget("VALUE",xmTextFieldWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,
		  (XtCallbackProc)vint_type_call,data);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2, NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion, NULL);
    XtAddEventHandler(w, ButtonPressMask, FALSE, check_default_handler, 0);
}

void traj_scale_type_in(widg,data,ev)
     Widget widg;
     char *data;
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
    
    w=XtVaCreateManagedWidget("VALUE",xmTextFieldWidgetClass,XtParent(widg),
                              XmNcolumns,5,XmNx,x-15,XmNy,y-15,
                              NULL);
    XtAddCallback(w,XmNactivateCallback,
		  (XtCallbackProc)traj_type_call,data);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2, NULL);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion, NULL);
    XtAddEventHandler(w, ButtonPressMask, FALSE, check_default_handler, 0);
}

void set_pat_callback(widg,data,ev)
    Widget widg;
    caddr_t data;
    XButtonEvent *ev;
{
    char *str, *str2, buff[256];
    
    switch((long)data){
    case 1: 
	str = XmTextFieldGetString(stipple.start1);
	str2 = XmTextFieldGetString(stipple.end1);
	snprintf(buff,255,"fillpattern=pat1,%s,%s",str,str2);
	driver(buff);
	break;
    case 2: 
	str = XmTextFieldGetString(stipple.start2);
	str2 = XmTextFieldGetString(stipple.end2);
	snprintf(buff,255,"fillpattern=pat2,%s,%s",str,str2);
	driver(buff);
	break;
    case 3: 
	str = XmTextFieldGetString(stipple.start3);
	str2 = XmTextFieldGetString(stipple.end3);
	snprintf(buff,255,"fillpattern=pat3,%s,%s",str,str2);
	driver(buff);
	break;
    case 4: 
	str = XmTextFieldGetString(stipple.start4);
	str2 = XmTextFieldGetString(stipple.end4);
	snprintf(buff,255,"fillpattern=pat4,%s,%s",str,str2);
	driver(buff);
	break;
    case 5: 
	str = XmTextFieldGetString(stipple.start5);
	str2 = XmTextFieldGetString(stipple.end5);
	snprintf(buff,255,"fillpattern=pat5,%s,%s",str,str2);
	driver(buff);
	break;
	/*    case 6: 
	      str = XmTextFieldGetString(stipple.start6);
	      str2 = XmTextFieldGetString(stipple.end6);
	      snprintf(buff,255,"fillpattern=pat5,%s,%s",str,str2);
	      driver(buff);
	      break;*/
    }
    XtFree(str);
    XtFree(str2);
}

void point_axis_call(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
  XmString str;
  char *st,*st2,buff[80];
  XtVaGetValues(w,XmNlabelString,&str,NULL);
  st = XmCvtXmStringToCT(str);
  st2=st+3;
  snprintf(buff,79,"point_axis=%s",st2);
  driver(buff);
}

