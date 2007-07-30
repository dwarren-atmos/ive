/****************************************************************************
 *$Log: points_callbacks.c,v $
 *Revision 1.10  2005/09/06 20:21:58  warren
 *Move updates into current stream
 *
 *Revision 1.9  2003/07/02 20:23:03  warren
 *Fixes for slanted window dumps on alphas with 8 bit screens, call info_2d
 *with correct mins and maxs when returning to slicer widget from points.
 *
 *Revision 1.8  1999/12/22 00:32:07  warren
 *Fixed Dales crashing bug when changing from point slicer to normal. Also
 *have some ifdef linuxes in there in case we ever find a fortran compiler.
 *
 *Revision 1.7  1998/12/29 16:39:18  harry
 *Minor cleanup.
 *
 *Revision 1.6  1997/06/27 21:40:02  davidive
 *Fix arbitrary slice line labeling
 *
 *Revision 1.5  1997/06/03 19:13:47  davidive
 *The big mouse slicing changeover. We have the new form, only requires you
 *set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 *always runs through init_points_ to catch all these things.
 *note: only actually works when two dimentions have points set. Needs to
 *be expanded into 1, 3 or 4.
 *
 *Revision 1.4  1997/03/14 22:25:11  warren
 *Fix to use the scalled locations to set xloc,yloc,zloc and tloc
 *
 *Revision 1.3  1997/02/21 21:16:55  warren
 *Final fixes for point/mouse slicing and expanding into 2d. It is tested
 *with the z dimention free and seems to work with others, but still should
 *be looked at with known data for this.
 *
 * Revision 1.2  1997/01/29  23:08:06  warren
 * Cleaned up and fixed Dales Color problem
 *
 *Revision 1.1  1996/11/07 16:54:00  warren
 *First set of activated point slicing and widgets to go with it. Only 1d plots
 *are known to be correct. Use at your own risk for 2d. There is still a known
 *bug in the connection between the tic mark entry boxes and the plot when doing
 *point or mouse slicing.
 *
 */
static char ident[] = "$Id: points_callbacks.c,v 1.10 2005/09/06 20:21:58 warren Exp $";
#include<stdlib.h>
#include<stdio.h>
#include <string.h>
#include <Xm/XmAll.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_widgets.h>
#include <ive_text_enum.h>

void slicer_set_point(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
  XmString str;
  float val; 
  int i,point; 
  char *strv, buff[256], *lab; 

 
     
  strv = XmTextGetString(w); 
  val = atof(strv); 
  XtFree(strv);
  switch((int)data){
  case 1: 
    XtVaGetValues(point_slicer.x1lab, XmNlabelString, &str, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &lab);
    sprintf(buff,"%cpoint=%f,",*lab,val); 
    XtFree(lab);
       driver_notwin(buff);
       break; 
  case 2: 
    XtVaGetValues(point_slicer.x2lab, XmNlabelString, &str, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &lab);
    sprintf(buff,"%cpoint=,%f,",*lab,val); 
    XtFree(lab);
    driver_notwin(buff);
    break;
  case 3: 
    XtVaGetValues(point_slicer.y1lab, XmNlabelString, &str, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &lab);
    sprintf(buff,"%cpoint=%f,",*lab,val); 
    XtFree(lab);
    driver_notwin(buff);
    break; 
  case 4: 
    XtVaGetValues(point_slicer.y2lab, XmNlabelString, &str, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &lab);
    sprintf(buff,"%cpoint=,%f,",*lab,val); 
    XtFree(lab);
    driver_notwin(buff);
    break;
  }

}

void point_fix_call(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
  XmString str, str2;
  int i,point,val; 
  char buff[256], *lab; 
     
  switch((int)data){
  case 1:
    XtVaGetValues(point_slicer.slidelab1, XmNlabelString, &str, NULL);
    XtVaGetValues(w, XmNlabelString, &str2, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &lab);
    if(*lab == 'T')
      sprintf(buff,"time="); 
    else
      sprintf(buff,"%cloc=",*lab); 
    XtFree(lab);
    XmStringGetLtoR(str2, XmFONTLIST_DEFAULT_TAG, &lab);
    if(*lab == 'F')
      if(*(lab+1)=='r'){
	str=(XmString)NewString("Fixed");
	XtVaSetValues(w, XmNlabelString, str, NULL);
	XmScaleGetValue(point_slicer.slide1l,&val);
	sprintf(buff,"%s%d",buff,val);
      }
      else
	{
	  str=(XmString)NewString("Avg  ");
	  XtVaSetValues(w, XmNlabelString, str, NULL);
	  sprintf(buff,"%saverage",buff);
	}	
    else{
      str=(XmString)NewString("Free ");
      XtVaSetValues(w, XmNlabelString, str, NULL);
      sprintf(buff,"%sfree",buff);
    }
    driver_notwin(buff);
    break;
  case 2:
    XtVaGetValues(point_slicer.slidelab2, XmNlabelString, &str, NULL);
    XtVaGetValues(w, XmNlabelString, &str2, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &lab);
    if(*lab == 'T')
      sprintf(buff,"time="); 
    else
      sprintf(buff,"%cloc=",*lab); 
    XtFree(lab);
    XmStringGetLtoR(str2, XmFONTLIST_DEFAULT_TAG, &lab);
    if(*lab == 'F')
      if(*(lab+1)=='r'){
	str=(XmString)NewString("Fixed");
	XtVaSetValues(w, XmNlabelString, str, NULL);
	XmScaleGetValue(point_slicer.slide2l,&val);
	sprintf(buff,"%s%d",buff,val);
      }
      else
	{
	  str=(XmString)NewString("Avg  ");
	  XtVaSetValues(w, XmNlabelString, str, NULL);
	  sprintf(buff,"%saverage",buff);
	}	
    else{
      str=(XmString)NewString("Free ");
      XtVaSetValues(w, XmNlabelString, str, NULL);
      sprintf(buff,"%sfree",buff);
    }
    driver_notwin(buff);
    break;
  } 
}



static struct {
  float mins[4],maxs[4],loc[4];
  int fix[4];
  long clear;
}ive_keep_it_for_switch={0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0,0,0,0,0};

void switch_slicers_(data)
    int *data;
{
    int i=4, error, fix[4], dummy;
    float mins[4],maxs[4],loc[4];
    char buff[512];
    if(*data == 1){
      if(ive_keep_it_for_switch.clear == 1){
	i=4;
	(void)getrarr_("plmin_scaled",mins,&i,&error,13);
	(void)getrarr_("plmax_scaled",maxs,&i,&error,13);
	(void)info_2d_(mins, 
		       maxs, &i);
	sprintf(buff,"window=%f,%f,%f,%f,%f,%f,%f,%f",
		ive_keep_it_for_switch.mins[0],
		ive_keep_it_for_switch.maxs[0],
		ive_keep_it_for_switch.mins[1],
		ive_keep_it_for_switch.maxs[1],
		ive_keep_it_for_switch.mins[2],
		ive_keep_it_for_switch.maxs[2],
		ive_keep_it_for_switch.mins[3],
		ive_keep_it_for_switch.maxs[3]);
	driver(buff);
	if(ive_keep_it_for_switch.fix[0]){
	  sprintf(buff, "xloc = %f",ive_keep_it_for_switch.loc[0]);
	  driver(buff);
	}
	if(ive_keep_it_for_switch.fix[1]){
	  sprintf(buff, "yloc = %f",ive_keep_it_for_switch.loc[1]);
	  driver(buff);
	}
	if(ive_keep_it_for_switch.fix[2]){
	  sprintf(buff, "zloc = %f",ive_keep_it_for_switch.loc[2]);
	  driver(buff);
	}
	if(ive_keep_it_for_switch.fix[3]){
	  sprintf(buff, "time = %f",ive_keep_it_for_switch.loc[3]);
	  driver(buff);
	}
	i=4;
	(void)setiarr_("lock",ive_keep_it_for_switch.fix,&i,&error,4);
	ive_keep_it_for_switch.clear = 0;

	if(point_slicer.point_form)
	  XtDestroyWidget(XtParent(point_slicer.point_form));
	point_slicer.point_form=NULL;
	i=4;
	
	(void)do_props(0);
	ive_keep_it_for_switch.clear=0;
      }
      else{
	i=4;
	(void)getrarr_("plmin_scaled",mins,&i,&error,13);
	(void)getrarr_("plmax_scaled",maxs,&i,&error,13);
	(void)info_2d_(mins, 
		       maxs, &i);

	(void)getrarr_("plwmin_scaled",mins,&i,&error,13);
	(void)getrarr_("plwmax_scaled",maxs,&i,&error,13);
	(void)getiarr_("lock",fix,&i,&error,4);
	(void)getrarr_("scaled_loc",loc,&i,&error,10);
	sprintf(buff,"window=%f,%f,%f,%f,%f,%f,%f,%f",
		mins[0],maxs[0],mins[1],maxs[1],mins[2],
		maxs[2],mins[3],maxs[3]);
	driver(buff);
	if(fix[0]){
	  sprintf(buff, "xloc = %f",loc[0]);
	  driver(buff);
	}
	if(fix[1]){
	  sprintf(buff, "yloc = %f",loc[1]);
	  driver(buff);
	}
	if(fix[2]){
	sprintf(buff, "zloc = %f",loc[2]);
	driver(buff);
	}
	if(fix[3]){
	  sprintf(buff, "time = %f",loc[3]);
	  driver(buff);
	}
	if(point_slicer.point_form)
	  XtDestroyWidget(XtParent(point_slicer.point_form));
	point_slicer.point_form=NULL;
	/*	i=4;
	(void)info_2d_(mins, maxs, &i);*/
	(void)do_props(0);
	ive_keep_it_for_switch.clear=0;
      }
    }
    else{
      if(ive_keep_it_for_switch.clear==0){
	(void)getrarr_("plwmin_scaled",mins,&i,&error,13);
	(void)getrarr_("plwmax_scaled",maxs,&i,&error,13);
	(void)getiarr_("lock",fix,&i,&error,4);
	(void)getrarr_("scaled_loc",loc,&i,&error,10);
	for(i=0; i<4; i++){
	  ive_keep_it_for_switch.mins[i] = mins[i];
	  ive_keep_it_for_switch.maxs[i] = maxs[i];
	  ive_keep_it_for_switch.loc[i] = loc[i];
	  ive_keep_it_for_switch.fix[i] = fix[i];
	}	  
	ive_keep_it_for_switch.clear=1;
      }
    
      if(slicer_widget.info_2d){
	XtDestroyWidget(XtParent(slicer_widget.info_2d));
      }
      (void)init_points_();
    }
    dummy=WINDOW;
    ui_update_(&dummy);
}


void switch_slicers(w, data, call)
    Widget w;
    caddr_t data;
    XmAnyCallbackStruct *call;
{
  int dummy;
  dummy = (int)data;
  switch_slicers_(&dummy);
}

void point_window_callback(Widget w, caddr_t data, XmAnyCallbackStruct call){
  XmString str;
  char *dimention, buff[256];
  int val1, val2;
  if((int)data == 1){ 
    if(!point_slicer.slide1r)return;
    XtVaGetValues(point_slicer.slidelab1, XmNlabelString, &str, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &dimention);
    XmScaleGetValue(point_slicer.slide1l, &val1);
    if(!XtIsManaged((Widget)point_slicer.slide1r)){
      if(*dimention != 'T'){
	sprintf(buff,"%cloc=%d",*dimention,val1);
      }
      else{
	sprintf(buff,"Time=%d",val1);
      }
      driver_notwin(buff);
      return;
    }
    XmScaleGetValue(point_slicer.slide1r, &val2);
  }
  else{
    if(!point_slicer.slide2r)return;
    XtVaGetValues(point_slicer.slidelab2, XmNlabelString, &str, NULL);
    XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &dimention);
    XmScaleGetValue(point_slicer.slide2l, &val1);
    XmScaleGetValue(point_slicer.slide2r, &val2);
    if(!XtIsManaged((Widget)point_slicer.slide2r)){
      if(*dimention != 'T'){
	sprintf(buff,"%cloc=%d",*dimention,val1);
      }
      else{
	sprintf(buff,"Time=%d",val1);
      }
      driver_notwin(buff);
      return;
    }
  }
  
  switch(*dimention){
  case 'X':
    val1 = fix_slicer_value(0, val1);
    val2 = fix_slicer_value(0, val2);
    sprintf(buff,"window=%d,%d",val1,val2); 
    break;
  case 'Y':
    val1 = fix_slicer_value(1, val1);
    val2 = fix_slicer_value(1, val2);
    sprintf(buff,"window=,,%d,%d",val1,val2);
    break;
  case 'Z':
    val1 = fix_slicer_value(2, val1);
    val2 = fix_slicer_value(2, val2);
    sprintf(buff,"window=,,,,%d,%d",val1,val2); 
    break;
  case 'T':
    val1 = fix_slicer_value(3, val1);
    val2 = fix_slicer_value(3, val2);
    sprintf(buff,"window=,,,,,,%d,%d",val1,val2); 
    break;
  }
  driver(buff);
}

void point_min_window_check_callback(Widget w, caddr_t data, 
				     XmAnyCallbackStruct call){
  int val1, val2, min, max;
  unsigned char dir;

  if(!data)return;
  if(!XtIsManaged((Widget)data))return;
  XtVaGetValues(w, XmNprocessingDirection, &dir, NULL);
  XtVaGetValues(w, XmNminimum, &min,  NULL);
  XtVaGetValues(w, XmNmaximum, &max, NULL);
  XmScaleGetValue(w, &val1);
  XmScaleGetValue((Widget)data, &val2);
  
  if(dir == XmMAX_ON_RIGHT){
    if(val1 >= val2){
      if((val2=val1+1) <= max)
	XmScaleSetValue((Widget)data, val2);
      else 
	XmScaleSetValue(w, --val1);
    }
  }
  else if(dir == XmMAX_ON_LEFT){
    if(val1 <= val2){
      if((val2=val1-1) >= min)
	XmScaleSetValue((Widget)data, val2);
      else 
	XmScaleSetValue(w, ++val1);
    }
  }
  XSync(XtDisplay(w), FALSE);
}
void point_max_window_check_callback(Widget w, caddr_t data, 
				     XmAnyCallbackStruct call){
  int val1, val2, min, max;
  unsigned char dir;

  if(!data)return;
  if(!XtIsManaged((Widget)data))return;
  XtVaGetValues(w, XmNprocessingDirection, &dir, NULL);
  XtVaGetValues(w, XmNminimum, &min,  NULL);
  XtVaGetValues(w, XmNmaximum, &max, NULL);
  XmScaleGetValue(w, &val1);
  XmScaleGetValue((Widget)data, &val2);
  
  if(dir == XmMAX_ON_RIGHT){
    if(val1 <= val2){
      if((val2=val1-1) >= min)
	XmScaleSetValue((Widget)data, val2);
      else 
	XmScaleSetValue(w, ++val1);
    }
  }
  else if(dir == XmMAX_ON_LEFT){
    if(val1 >= val2){
      if((val2=val1+1) <= max)
	XmScaleSetValue((Widget)data, val2);
      else 
	XmScaleSetValue(w, --val1);
    }
  }
  XSync(XtDisplay(w), FALSE);
}

