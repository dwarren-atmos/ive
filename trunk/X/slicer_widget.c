/******************************************************************************
 *$Log: slicer_widget.c,v $
 *Revision 1.36  2002/08/09 19:57:28  warren
 *Fixed for Linux and tested with DEC sun etc.
 *most items inside #ifdef's, or were general cleanup/fixes
 *
 *Revision 1.35  2001/10/16 19:55:34  harryive
 *Reverse order of tests in fix_slicer_value.
 *
 *Revision 1.34  1998/01/28 15:28:19  harry
 *Round off slider values to the nearest integer rather than truncating.
 *
 *Revision 1.33  1998/01/12 18:30:16  harry
 *Correct fix_slicer_value to properly handle scaled coordinates.
 *
 *Revision 1.32  1997/10/15 19:24:53  harry
 *Why do it twice when once is sufficient.
 *
 *Revision 1.31  1997/06/03 19:13:49  davidive
 *The big mouse slicing changeover. We have the new form, only requires you
 *set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 *always runs through init_points_ to catch all these things.
 *note: only actually works when two dimentions have points set. Needs to
 *be expanded into 1, 3 or 4.
 *
 *Revision 1.30  1997/04/18 22:12:35  warren
 *Null out all the widgets in the slicer when it is destroyed.
 *
 *Revision 1.29  1997/04/11 18:46:44  warren
 *Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 *Revision 1.28  1996/07/12 20:58:48  harry
 *Change data and domain slopes and intercepts to real*8.
 *
 *Revision 1.27  1996/04/25 16:24:26  harry
 *Add Skew-T plots and the beginning of trajectory plots.
 *
 *Revision 1.26  1996/04/23 18:16:58  warren
 *Changes for terrain
 *
 *Revision 1.25  1996/03/29 17:46:55  harry
 *Use widgets instead of gadgets per suggestions in Motif Programming Manual.
 *
 * Revision 1.24  1995/12/15  23:35:26  warren
 * Make 4.0 BETA and add code for later use in desensitising the sliders.
 *
 * Revision 1.23  1995/10/04  22:09:07  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.22  1995/07/10  17:16:18  warren
 * Finished renaming widgets so the help key will work. Also fixed the color
 * menus not to contain an _ in the boxes and fixed the second vector
 * componant update proble.
 *
 * Revision 1.21  1994/09/12  20:57:50  warren
 * pass *float instead of *int into scale and unscale.
 *
 * Revision 1.20  1994/06/20  16:34:51  harry
 * Correct definitions of MAX and MIN to match "standard" include files.
 *
 * Revision 1.19  1994/05/26  18:16:13  harry
 * Add #ifndef around definitions of MIN and MAX.
 *
 * Revision 1.18  1994/05/26  17:32:12  warren
 * change to includes and defines to work with SOL 2
 *
 * Revision 1.17  1994/02/01  02:06:25  warren
 * fixed dales problem of lock values changing with new var and added type in
 * for loop set up.
 *
 * Revision 1.16  1993/11/29  19:38:01  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.15  1993/11/03  23:02:21  warren
 * Fixed problem where slicer widget didn't change window.
 *
 * Revision 1.14  1993/08/18  15:46:15  harry
 * Change to new transform routines.
 *
 * Revision 1.13  1993/07/27  23:16:47  warren
 * fixed realloc so it has the starting pointer
 *
 * Revision 1.12  1993/07/23  20:08:03  warren
 * added lockcolortable to the loop ignore list. fixed the time slider callback
 * to compare the slider to the scaled times and added an array of scaled times.
 *
 * Revision 1.11  1993/07/12  22:16:04  warren
 * Fixed so you can get your widgets when you only have 1 time.
 *
 * Revision 1.10  1993/07/09  21:42:35  harry
 * Make sure the min slider is to the left of the max slider.
 *
 * Revision 1.9  1993/07/06  20:24:12  harry
 * Use "scaled loc" instead of the unscaled xloc,yloc,zloc,tloc to
 * correcty determine out of range fixed values in the sliders.
 *
 * Revision 1.8  1993/07/01  23:10:10  harry
 * Correct problem with fixed slider on maximum setting.
 *
 * Revision 1.7  1993/06/30  22:14:27  harry
 * Correct reset of "fixed" slider.
 *
 * Revision 1.6  1993/06/29  21:03:59  warren
 * Fixed Rajul's problem of crashing when reading a file with no global
 * mins and maxs. Only check sliders that exist.
 *
 * Revision 1.5  1993/06/25  23:48:02  warren
 * added missing include of sys/params.h.
 *
 * Revision 1.4  1993/06/25  19:18:41  warren
 * Fixed sliders so they are correct when changing datasets.
 *
 * Revision 1.3  1993/06/05  00:21:02  warren
 * Fixes to slicer and forms updating needlessly and wrong.
 *
 * Revision 1.2  1993/05/01  00:12:48  warren
 * Fixed to get correct values when min > max
 *
 * Revision 1.1.1.1  1993/04/07  19:41:23  harry
 * Initial CVS checkin of IVE.
 *
 *****************************************************************************/
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/XmAll.h>
#include <stdio.h>
#include <string.h>
#include <locale.h>
#include <sys/param.h>
#include <math.h>
#ifdef Linux
extern double rint();
#endif
#include <malloc.h>

#include <ive.h>
#include <ive_for.h>
#include <ive_macros.h>

static char ident[] = "$Id: slicer_widget.c,v 1.36 2002/08/09 19:57:28 warren Exp $";
#ifndef MAX
#define MAX(x, y) (((x) > (y))? (x):(y)) 
#endif
#ifndef MIN
#define MIN(x, y) (((x) < (y))? (x):(y))
#endif
#define app_con XtWidgetToApplicationContext(slicer_widget.info_2d)

extern XmString NewString();
extern void getdarr_(),scale_(),setivar_(),setiarr_(),getavar_(),getrarr_(),getaarr_();

struct {
    Widget info_2d, mins[4], maxs[4], fix[4], fix_lab[4];
}slicer_widget = {NULL,
		  NULL,NULL,NULL,NULL,
		  NULL,NULL,NULL,NULL,
		  NULL,NULL,NULL,NULL,
		  NULL,NULL,NULL,NULL};
float *fSlicer_Values[4];
int nSlicer_Values[4];

#define ButtonPressMask                 (1L<<2)  

void set_slicer_values_(values, n)
float **values;
int *n;
{
    int i;

    for (i=0; i < 4; i++) {
        fSlicer_Values[i] = values[i];
        nSlicer_Values[i]  = n[i];
    }
}

void slicer_gone_callback(w, client, call)
     Widget w;
     Widget *client;
     XmAnyCallbackStruct *call;
{
    int i;
    slicer_widget.info_2d=NULL;
    for(i =0; i<4; i++){
	slicer_widget.mins[i]=NULL;
	slicer_widget.maxs[i]=NULL;
	slicer_widget.fix[i]=NULL;
	slicer_widget.fix_lab[i]=NULL;
    }
}

float fix_slicer_value(int slicer, float value)
{
    static int four=4, one=1;
    static float zero=0.0;
    double intercept[4], slope [4];
    int error;
    int j;

    if (nSlicer_Values[slicer]) {
	float *SValues = (float *)malloc(sizeof(float)*nSlicer_Values[slicer]);

	memcpy(SValues, fSlicer_Values[slicer],
	       sizeof(float)*nSlicer_Values[slicer]);
	(void) getdarr_("domain_slope", slope, &four, &error, 12);
	(void) getdarr_("domain_intercept", intercept, &four, &error, 16);
	(void) scale_(SValues, nSlicer_Values+slicer,
		      slope+slicer, intercept+slicer, &zero);
	j = 0;
	if (SValues[0] < SValues[1]) {
	    while(j < nSlicer_Values[slicer] && SValues[j] < value) j++;
	    if(j == nSlicer_Values[slicer])j--;
	    if (value != SValues[j]) {
		if(value-SValues[j-1] < SValues[j]-value)
		    value = SValues[j-1];
		else
		    value = SValues[j];
	    }
	}
	else {
	    while(j < nSlicer_Values[slicer] && SValues[j] > value) j++;
	    if(j == nSlicer_Values[slicer])j--;
	    if (value != SValues[j]) {
		if(SValues[j-1]-value < value-SValues[j])
		    value = SValues[j-1];
		else
		    value = SValues[j];
	    }
	}
	free(SValues);
    }
    return (value);
}

void info_2d_(min,max,dim)
     float *min, *max;
     int *dim; 
{
    int tval, aval, bval;
    int i,j,xloc,yloc,test=0,change=0, ive_plock[4];
    Widget popup;
    char drstr[240], field[80];
    float dmin[4], dmax[4], scaled_loc[4], old_loc[4];
    XmString str;
    static char *name[4] = {"XLOC","YLOC","ZLOC","TIME"};    
    char name_label[5], dim_names[4][80];
    static int curdim, four=4;
    extern int fixed_index[4];
    extern void check_index();
    
    i=1;
    (void)setivar_("use_buttons", &i, &j, 11);
    i=0;
    for(j=0; j<4; j++)
      ive_plock[j]=0;
    (void)setiarr_("ive_plock", ive_plock, &four, &i, 9);
    (void)getavar_("field",field,&i,5,80);
    (void)getrarr_("scaled_loc",scaled_loc,&four,&i,10);
    (void)getrarr_("scaled_loc",old_loc,&four,&i,10);
    (void)getaarr_("dim_display_names",dim_names,&four,&i,17,80);
    (void)getrarr_("phmin",dmin,dim,&i,5);
    (void)getrarr_("phmax",dmax,dim,&i,5);
    if(slicer_widget.info_2d)test=1;
    if(test){
	popup = XtParent(slicer_widget.info_2d);
    }
    if(!test){
	for (i=0;i<*dim;i++){
	    slicer_widget.mins[i]= (Widget)NULL;
	    slicer_widget.maxs[i]= (Widget)NULL;
	    slicer_widget.fix[i] = (Widget)NULL;
	}
	
	popup = XtVaCreatePopupShell("Window/Slicer",
				     transientShellWidgetClass,
				     XtParent(Box),
				     XmNheight,
				     (min[*dim-1]!=max[*dim-1])? 206:160,
				     XmNwidth, 396,
				     XmNautoUnmanage,FALSE,
				     XmNy,
				     HeightOfScreen(XtScreen(Box))-
				     ((min[*dim-1]!=max[*dim-1])?215:173),
				     XtNallowShellResize,TRUE,
				     NULL);
	
	slicer_widget.info_2d = XtVaCreateManagedWidget("Window",
						   xmBulletinBoardWidgetClass,
							popup,
							XmNheight, 
					   (min[*dim-1]!=max[*dim-1])? 206:160,
							XmNwidth, 381,
							XmNautoUnmanage,FALSE,
							NULL);
	XtAddCallback(popup,XmNdestroyCallback,
		      (XtCallbackProc)slicer_gone_callback,NULL);
	XtAddCallback(slicer_widget.info_2d, XmNhelpCallback,
                  check_help_call,NULL);

	XtVaCreateManagedWidget ("Min Window Coords/value",
				 xmLabelWidgetClass,
				 slicer_widget.info_2d,
				 XmNx,10,
				 XmNy,0,
				 NULL);

	XtVaCreateManagedWidget ("Max Window Coords",
				 xmLabelWidgetClass,
				 slicer_widget.info_2d,
				 XmNx,225,
				 XmNy,0,
				 NULL);
	
    }
    /*sliders for window*/
    curdim = *dim;
    
    for (i = 0; i<*dim; i++){
	if((min[i] != max[i]) && (dmin[i] != dmax[i])){
	    if(test && slicer_widget.fix[i]);
	    else{
		XmString str;
		int j;
		str = NewString("Free ");
		if ((j=strlen(dim_names[i])) > 3) j = 3;
		strncpy(name_label, dim_names[i], j);
		name_label[j++] = ':';
		name_label[j] = '\0';
		slicer_widget.fix_lab[i]=
		  XtVaCreateManagedWidget(name_label,
					  xmLabelWidgetClass,
					  slicer_widget.info_2d,
					  XmNy,40*i+50,
					  XmNx,160,
					  NULL);
		slicer_widget.fix[i] = 
		  XtVaCreateManagedWidget(name[i],
					  xmPushButtonWidgetClass,
					  slicer_widget.info_2d,
					  XmNy,40*i+50,
					  XmNx,190,
					  XmNlabelString ,str,
					  NULL);
		
		XtAddEventHandler(slicer_widget.fix[i],ButtonPressMask,FALSE,
				  domain_units_call,name[i]);
		XtAddEventHandler(slicer_widget.fix_lab[i],ButtonPressMask,FALSE,
				  domain_units_call,name[i]);
		XtAddCallback(slicer_widget.fix[i],XmNactivateCallback,
			      fix_var,(XtPointer)(name+i));
		XmStringFree(str);
	    }
	    
	    if(test && slicer_widget.mins[i]){
		change=0;
		if(fixed_index[i] != 1){
		    XtVaGetValues(slicer_widget.mins[i],XmNvalue,&tval, NULL);
                    if(tval<(int)(rint(MIN(max[i],min[i])))) {
			tval=(int)(rint(MIN(max[i],min[i])));
			change=1;
		    }
                    else if(tval>(int)(rint(MAX(max[i],min[i])))){
			tval=(int)(rint(MAX(max[i],min[i])));
			change=1;
		    }
		    aval = (int)rint(max[i]);
		    bval = (int)rint(min[i]);
		    XtVaSetValues(slicer_widget.mins[i],
				  XmNprocessingDirection,
				  (max[i]>min[i])?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
				  XmNmaximum,MAX(aval,bval),
				  XmNminimum,MIN(aval,bval),
				  XmNvalue, tval,
				  NULL);
		}
	    }
	    else{
		change=1;
		str = NewString("");
		tval = (int)(rint((max[i]>min[i])?MIN(max[i],min[i]):
			     MAX(max[i],min[i])));
		aval = (int)rint(max[i]);
		bval = (int)rint(min[i]);
		slicer_widget.mins[i] = 
		  XtVaCreateManagedWidget(name[i],xmScaleWidgetClass,
				  slicer_widget.info_2d,
				  XmNtitleString,str,
				  XmNorientation,XmHORIZONTAL,
				  XmNshowValue,TRUE,
				  XmNx,10,
				  XmNy,40*i+30,
				  XmNwidth,150,
				  XmNprocessingDirection,
				  (max[i]>min[i])?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
					  XmNmaximum,MAX(aval,bval),
					  XmNminimum,MIN(aval,bval),
					  XmNvalue, tval,
					  NULL);
		XmStringFree(str);
		XtAddCallback(slicer_widget.mins[i],XmNvalueChangedCallback,
			      window_callback,(XtPointer)i);
		XtAddCallback(slicer_widget.mins[i],XmNvalueChangedCallback,
			      min_window_check_callback,&slicer_widget.maxs[i]);
		XtAddCallback(slicer_widget.mins[i],XmNdragCallback,
			      min_window_check_callback,&slicer_widget.maxs[i]);
		/* for poping boxes on top*/
		XtAddRawEventHandler(slicer_widget.mins[i],
				     ButtonPressMask,FALSE,
				     window_scale_type_in,(XtPointer)i);
		XtUnmanageChild(XtNameToWidget(slicer_widget.mins[i],"Title"));
	    }
	    
	    if(test && slicer_widget.maxs[i]){
                XtVaGetValues(slicer_widget.maxs[i],XmNvalue,&tval, NULL);
                if(tval<(int)(rint(MIN(max[i],min[i])))){
		    tval=(int)(rint(MIN(max[i],min[i])));
		    change=1;
		}
                else if(tval>(int)(rint(MAX(max[i],min[i])))){
		    tval=(int)(rint(MAX(max[i],min[i])));
		    change=1;
		}
		aval = (int)rint(max[i]);
		bval = (int)rint(min[i]);
		XtVaSetValues(slicer_widget.maxs[i],
			      XmNprocessingDirection,
			      (max[i]>min[i])?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
			      XmNmaximum,MAX(aval,bval),
			      XmNminimum,MIN(aval,bval),
			      XmNvalue, (int)(tval),
			      NULL);
		
	    }
	    else{
		change=1;
		str = NewString("");
		tval = (int)(rint((max[i]>min[i])?MAX(max[i],min[i]):
			     MIN(max[i],min[i])));
		aval = (int)rint(max[i]);
		bval = (int)rint(min[i]);
		slicer_widget.maxs[i] = 
		  XtVaCreateManagedWidget(name[i],xmScaleWidgetClass,
				  slicer_widget.info_2d,
				  XmNtitleString,str,
				  XmNorientation,XmHORIZONTAL,
				  XmNshowValue,TRUE,
				  XmNx,235,
				  XmNy,40*i+30,
				  XmNwidth,150,
				  XmNprocessingDirection,
			         (max[i]>min[i])?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
				  XmNmaximum,MAX(aval,bval),
				  XmNminimum,MIN(aval,bval),
				  XmNvalue, (int)(tval),
				  NULL);
		XmStringFree(str);
		XtAddCallback(slicer_widget.maxs[i],XmNvalueChangedCallback,
			      window_callback,(XtPointer)(i+4));
		XtAddCallback(slicer_widget.maxs[i],XmNvalueChangedCallback,
			      max_window_check_callback,&slicer_widget.mins[i]);
		XtAddCallback(slicer_widget.maxs[i],XmNdragCallback,
			      max_window_check_callback,&slicer_widget.mins[i]);
		/* for poping boxes on top*/
		XtAddRawEventHandler(slicer_widget.maxs[i],
				     ButtonPressMask,FALSE,
				     window_scale_type_in,(XtPointer)i);
		XtUnmanageChild(XtNameToWidget(slicer_widget.maxs[i],"Title"));
	    }
	    
	    if(fixed_index[i]==1 && slicer_widget.maxs[i]
	       && slicer_widget.mins[i]){

		str = NewString("Fixed");
		XtUnmanageChild(slicer_widget.maxs[i]);
		XtVaSetValues(slicer_widget.fix[i],XmNlabelString,str,NULL);
		scaled_loc[i] = MAX(MIN(max[i],min[i]), scaled_loc[i]);
		scaled_loc[i] = MIN(MAX(max[i],min[i]), scaled_loc[i]);
		aval = (int)rint(max[i]);
		bval = (int)rint(min[i]);
		XtVaSetValues(slicer_widget.mins[i],
			      XmNprocessingDirection,
			      (max[i]>min[i])?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
			      XmNmaximum,MAX(aval,bval),
			      XmNminimum,MIN(aval,bval),
			      XmNvalue, (int)(scaled_loc[i]),
			      NULL);
		if(scaled_loc[i] != old_loc[i]) {
		    int j=-i;
		    XtAppAddTimeOut(app_con,0,check_index,(XtPointer)j);
		}
		XmStringFree(str);
	    }
	    else {
		if(fixed_index[i] == 2){
		    str = NewString("Avg  ");
		   XtVaSetValues(slicer_widget.fix[i],XmNlabelString,str,NULL);
		   if(change) {
		       int j=-i;
		       XtAppAddTimeOut(app_con,0,check_index,(XtPointer)j);
		   }
	        }
	    }
            if (!fixed_index[i])
	      min_window_check_callback(slicer_widget.mins[i],
			      &slicer_widget.maxs[i], NULL);
          }
	else
	  {
	      int j=-i;

	      if(slicer_widget.mins[i] != NULL) 
		XtDestroyWidget(slicer_widget.mins[i]);
	      if(slicer_widget.maxs[i] != NULL)
		XtDestroyWidget(slicer_widget.maxs[i]);
	      if(slicer_widget.fix[i] != NULL)
		XtDestroyWidget(slicer_widget.fix[i]);
	      if(slicer_widget.fix_lab[i] != NULL)
		XtDestroyWidget(slicer_widget.fix_lab[i]);
	      slicer_widget.mins[i]=slicer_widget.maxs[i]=slicer_widget.fix[i]=
		slicer_widget.fix_lab[i]=NULL;
	      fixed_index[i] = 1;
	      XtAppAddTimeOut(app_con,0,check_index,(XtPointer)j);
	  }
    }
    if(!slicer_widget.mins[0] && !slicer_widget.mins[1] && 
       !slicer_widget.mins[2] && !slicer_widget.mins[3]){
	XtDestroyWidget(popup);
	slicer_widget.info_2d=NULL;
	fixed_index[0]=fixed_index[1]=fixed_index[2]=fixed_index[3]=0;
    }
    else{
	XtManageChild(popup);
    }
    setiarr_("lock",fixed_index, &four, &i, 4);
}

