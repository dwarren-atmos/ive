/***************************************************************************
 *
 *$Log: forms_setup.c,v $
 *Revision 1.23  2001/08/15 22:23:20  davidive
 *Cleaned up code to reduce warnings about type mismatches, and added buttons
 *and callbacks to the 1d form widget to select which access to label against
 *for diagonal plots.
 *
 *Revision 1.22  1999/01/29 20:45:21  warren
 *new stuff to deal with the trajectory form
 *
 *Revision 1.21  1998/12/31 16:01:07  warren
 *Firts cut of new trajectory look and feel. Trajectories are no longer
 *on the popup menu, just plot type.
 *
 *Revision 1.20  1997/10/30 23:07:02  davidive
 *Fix beginning and ending traj times
 *
 *Revision 1.19  1997/10/23 14:31:58  warren
 *Fix typo (time should be time2 in setup for trajectory form)
 *
 *Revision 1.18  1997/10/22 19:34:34  davidive
 *Turned the particle movement part of trajectories into a user transform
 *and added unit checking to the default transform.
 *
 *Revision 1.17  1997/06/12 15:28:06  davidive
 *Fix multidimentional 1 D plots to pay attention to min, max and tic mark
 *settings, and remember them.
 *
 *Revision 1.16  1996/10/11 01:12:03  warren
 *To fill or not to fill is no longer the only question. Now there is a seperate
 *question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 *Labels are not fixed yet.
 *
 *Revision 1.15  1996/07/24 17:07:00  harry
 *Change dependant to dependent (preferred spellin in Websters).
 *
 *Revision 1.14  1996/04/25 16:24:17  harry
 *Add Skew-T plots and the beginning of trajectory plots.
 *
 *Revision 1.13  1996/01/12 16:10:56  warren
 *Transfer of new pattern stuff from 3.x and beginning of color fixes
 *(color vectors, correct collors on the pulldowan menus)
 *
 *Revision 1.12  1995/12/15 22:32:36  warren
 *moved patterns into 4.0 and implemented the list of fixes and changes from
 *Curtis.
 *
 * Revision 1.11  1995/10/18  19:16:52  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.10  1994/11/22  22:46:35  harry
 * Correct vector locking, add support for vector positioning, add
 * support for filled contour overlays.
 *
 * Revision 1.9  1993/11/29  19:37:52  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.8  1993/10/18  23:06:51  warren
 * updates from 3.0 version and fixed printer forms.
 *
 * Revision 1.7  1993/10/13  00:02:57  warren
 * Update current version for fixes to 3.0 version related to the meaning of
 * default in the property sheet boxes.
 *
 * Revision 1.6  1993/08/11  21:43:37  warren
 * fixed broken togglebuttons
 *
 * Revision 1.5  1993/07/09  16:36:09  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.4  1993/07/08  00:04:33  warren
 * use %g not %.2f and try to commit again as it didn't the first time
 *
 * Revision 1.3  1993/06/22  19:34:36  warren
 * rearanged background and plottype on main widget and fixed associated calls
 * appropriately. Also fixed main to call driver correctly.
 *
 * Revision 1.2  1993/06/18  16:46:02  warren
 * cleanup, reorganization, fixes to line form widget calls.
 *
 * Revision 1.1  1993/05/20  17:30:29  warren
 * Additions and changes to make the new forms active.
 *
 ***************************************************************************/

static char ident[] = "$Id: forms_setup.c,v 1.23 2001/08/15 22:23:20 davidive Exp $";

#include <Xm/XmAll.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

#include <ive_for.h>
#include <ive.h>
#include <ive_text_enum.h>
#include <ive_macros.h>
#include <float.h>
#include <ive_widgets.h>
#include <properties.h>

#define bigone 1.0E37
extern void getivar_(),getrarr_(),getrvar_(),getlvar_(),getavar_();
extern struct {
  int plot_axis;
  int rec_axis;
  float slope;
  float intercept;
} angle_plot;

setup_contour_form()
{
    XmString str, NewString();
    char *chpt, buff[256], cvar[80];
    int i, j, ivar;
    int error, lvar;
    float rvar, rarr[40];
    static char *biga="A", *littlea="a", *bigo="O", *littleo="o";
    static char *bigx="X", *littlex="x", *bigy="Y", *littley="y";
    
    /********** cont int or vals **********/
    (void)getivar_("num_vals", &ivar, &error, 8);
    if(ivar>0){
	getrarr_("cont_values", rarr, &ivar, &error, 11);
	buff[0]='\0';
	if(ivar>0){
	    sprintf(buff,"%g",rarr[0]);
	}
	else {
	    ivar = 1;
	    sprintf(buff,"0");
	    driver("contour_values=0");
	}
	for (i=1; i<ivar; i++){
	    sprintf(buff,"%s,%g",buff,rarr[i]);
	}
	XtVaSetValues(Properties.cont_values, XmNvalue, buff, NULL);
	XtVaSetValues(Properties.cont_val, XmNset, TRUE, NULL);
	XtVaSetValues(Properties.cont_int, XmNset, FALSE, NULL);
    }
    else{
	getrvar_("cont_int", &rvar, &error, 8);
	sprintf(buff,"%g",rvar);
	XtVaSetValues(Properties.cont_values, XmNvalue, buff, NULL);
	XtVaSetValues(Properties.cont_val, XmNset, FALSE, NULL);
	XtVaSetValues(Properties.cont_int, XmNset, TRUE, NULL);
    }
    
    /**********num unlabeled lines**********/
    getlvar_("cllab", &lvar, &error, 5);
    if(!lvar)XmScaleSetValue(Properties.numlines,0);
    else{
	getivar_("nulbll", &ivar, &error, 6);
	XmScaleSetValue(Properties.numlines,ivar + 1);
    }
    
    /**********area fill **********/
    getivar_("filtyp", &ivar, &error, 6);
    if(ivar == SOLID)
	str = NewString("Solid");
    else if(ivar == PATTERN)
	str = NewString("With Stipple");

    XtVaSetValues(Properties.fill, XmNlabelString, str, NULL);
    XmStringFree(str);

    getivar_("fill_frequency", &ivar, &error, 14);
    switch(ivar){
    case NEVER:
	str = NewString("Never");
	break;
    case ALWAYS:   
	str = NewString("Always");
	break;
    case EXCEPT_OVERLAY:   
	str = NewString("Except On Overlay");
	break;
    }
    XtVaSetValues(Properties.freq, XmNlabelString, str, NULL);
    XmStringFree(str);

    /**********zero contour**********/
    getlvar_("no_zero", &lvar, &error, 7);
    if(!lvar){
	str = NewString("Yes");
    }
    else{
	str = NewString("No ");
    }
    XtVaSetValues(Properties.zero, XmNlabelString, str, NULL);
    XmStringFree(str);
    
    /**********lin log**********/
    (void)getivar_("linlog", &ivar, &error, 6);
    switch(ivar){
      case 1:
	XmToggleButtonSetState(Properties.ihaxeslin,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslin,  TRUE, TRUE);
	break;
      case 2:
	XmToggleButtonSetState(Properties.ihaxeslin,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslog,  TRUE, TRUE);
	break;
      case 3:
	XmToggleButtonSetState(Properties.ihaxeslog,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslin,  TRUE, TRUE);
	break;
      case 4:
	XmToggleButtonSetState(Properties.ihaxeslog,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslog,  TRUE, TRUE);
	break;
    }

    /**********ticmarcs**********/

    (void)getrvar_("xminor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.xminor, XmNvalue, buff, NULL);
    (void)getrvar_("xmajor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.xmajor, XmNvalue, buff, NULL);
    
    (void)getrvar_("yminor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.yminor, XmNvalue, buff, NULL);
    (void)getrvar_("ymajor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.ymajor, XmNvalue, buff, NULL);
}

setup_vector_form()
{
    XmString str, NewString();
    char *chpt,buff[256], cvar[80];
    int i, j, ivar;
    int error, lvar;
    float rvar, rarr[40];
    static char *biga="A", *littlea="a", *bigo="O", *littleo="o";
    static char *bigx="X", *littlex="x", *bigy="Y", *littley="y";
    
    /**********Vector/Streamlines**********/
    (void)getlvar_ ("drstrmln", &lvar, &error, 8);
    if (lvar) {
	XtVaSetValues(Properties.vect_strv,XmNset,FALSE,NULL);
	XtVaSetValues(Properties.vect_str,XmNset,TRUE,NULL);
    }
    else{
	XtVaSetValues(Properties.vect_str,XmNset,FALSE,NULL);
	XtVaSetValues(Properties.vect_strv,XmNset,TRUE,NULL);
    }
    
    /**********vect comp**********/
    buff[0]='\0';
    (void)getavar_ ("vcomp_1", buff, &error, 7, 256);
    XtVaSetValues(Properties.vcomp_1, XmNvalue, buff, NULL);
    buff[0]='\0';
    (void)getavar_ ("vcomp_2", buff, &error, 7, 256);
    XtVaSetValues(Properties.vcomp_2, XmNvalue, buff, NULL);

    /**********Vector max**********/
    (void)getrvar_("vmax", &rvar, &error, 4);
    sprintf(buff,"%g",rvar);
    XtVaSetValues(Properties.vect_max, XmNvalue, buff, NULL);
    
    /**********lin log**********/
    (void)getivar_("linlog", &ivar, &error, 6);
    switch(ivar){
      case 1:
	XmToggleButtonSetState(Properties.ihaxeslinv,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslinv,  TRUE, TRUE);
	break;
      case 2:
	XmToggleButtonSetState(Properties.ihaxeslinv,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslogv,  TRUE, TRUE);
	break;
      case 3:
	XmToggleButtonSetState(Properties.ihaxeslogv,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslinv,  TRUE, TRUE);
	break;
      case 4:
	XmToggleButtonSetState(Properties.ihaxeslogv,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.ivaxeslogv,  TRUE, TRUE);
	break;
    }

    /**********ticmarcs**********/

    (void)getrvar_("xminor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.xminorv, XmNvalue, buff, NULL);
    (void)getrvar_("xmajor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.xmajorv, XmNvalue, buff, NULL);
    
    (void)getrvar_("yminor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.yminorv, XmNvalue, buff, NULL);
    (void)getrvar_("ymajor", &rvar, &error, 6);
    if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
      sprintf(buff, "default");
    else
      sprintf(buff, "%g",rvar);
    XtVaSetValues(Properties.ymajorv, XmNvalue, buff, NULL);
}
setup_line_form(fix, buttons)
     int *fix, buttons;
{
    XmString str, NewString();
    char *chpt, buff[80], cvar[80];
    int i, j, ivar;
    int error,lvar;
    float rvar;
    static char *biga="A", *littlea="a", *bigo="O", *littleo="o";
    static char *bigx="X", *littlex="x", *bigy="Y", *littley="y";
    
    if(XtIsManaged(Properties.direction_radio_box))
      XtUnmanageChild(Properties.direction_radio_box);
    /**********label vars correctly**********/
    (void)getavar_("field", cvar, &error, 5, 80);
    if(cvar[0] == '\0')
      sprintf(buff, "Dependent Variable ?");
    else
      sprintf(buff, "Dependent Variable %s", cvar);
    str = NewString(buff);
    XtVaSetValues (Properties.varlab, XmNlabelString, str, NULL);
    XmStringFree(str);
    
    if(buttons){
      for(i=0; i<4; i++){
	if(!fix[i])break;
      }
      switch(i){
      case 0:
	sprintf(buff, "Independent Variable X");
	break;
      case 1:
	sprintf(buff, "Independent Variable Y");
	break;
      case 2:
	sprintf(buff, "Independent Variable Z");
	break;
      case 3:
	sprintf(buff, "Independent Variable T");
	break;
      }
      XtVaSetValues (Properties.lvarlab,XmNrightAttachment,XmATTACH_FORM,NULL);
    }
    else{
      for(i=0; i<4; i++){
	if(fix[i])break;
      }
      switch(i){
      case 0:
	str = NewString("X");
	XtVaSetValues(Properties.radio_1,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 0)
	  XtVaSetValues(Properties.radio_1,XmNset,TRUE,NULL);
	break;
      case 1:
	str = NewString("Y");
	XtVaSetValues(Properties.radio_1,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 1)
	  XtVaSetValues(Properties.radio_1,XmNset,TRUE,NULL);
	break;
      case 2:
	str = NewString("Z");
	XtVaSetValues(Properties.radio_1,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 2)
	  XtVaSetValues(Properties.radio_1,XmNset,TRUE,NULL);
	break;
      case 3:
	str = NewString("T");
	XtVaSetValues(Properties.radio_1,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 3)
	  XtVaSetValues(Properties.radio_1,XmNset,TRUE,NULL);
	break;
      }
      i++;
       for(i; i<4; i++){
	if(fix[i])break;
      }
      switch(i){
      case 1:
	str = NewString("Y");
	XtVaSetValues(Properties.radio_2,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 1)
	  XtVaSetValues(Properties.radio_2,XmNset,TRUE,NULL);
	break;
      case 2:
	str = NewString("Z");
	XtVaSetValues(Properties.radio_2,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 2)
	  XtVaSetValues(Properties.radio_2,XmNset,TRUE,NULL);
	break;
      case 3:
	str = NewString("T");
	XtVaSetValues(Properties.radio_2,XmNlabelString,str,NULL);
	XmStringFree(str);
	if(angle_plot.plot_axis == 3)
	  XtVaSetValues(Properties.radio_2,XmNset,TRUE,NULL);
	break;
      }
      if(angle_plot.plot_axis == -1)
	XtVaSetValues(Properties.radio_3,XmNset,TRUE,NULL);
      sprintf(buff, "Independent Variable");
      if(!XtIsManaged(Properties.direction_radio_box))
	XtManageChild(Properties.direction_radio_box);
      XtVaSetValues (Properties.lvarlab,XmNrightAttachment,
		     Properties.direction_radio_box,NULL);
    }
    str = NewString(buff);
    XtVaSetValues (Properties.lvarlab, XmNlabelString, str, NULL);
    XmStringFree(str);
    /**********linlog**********/
    (void)getivar_("linlog", &ivar, &error, 6);
    switch(ivar){
      case 1:
	XmToggleButtonSetState(Properties.daxeslin,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.laxeslin,  TRUE, TRUE);
	break;
      case 2:
	if(fix[2]){
	    XmToggleButtonSetState(Properties.daxeslog,  TRUE, TRUE);
	    XmToggleButtonSetState(Properties.laxeslin,  TRUE, TRUE);
	}
	else{
	    XmToggleButtonSetState(Properties.daxeslin,  TRUE, TRUE);
	    XmToggleButtonSetState(Properties.laxeslog,  TRUE, TRUE);
	}
	break;
      case 3:
	if(fix[2]){
	    XmToggleButtonSetState(Properties.daxeslin,  TRUE, TRUE);
	    XmToggleButtonSetState(Properties.laxeslog,  TRUE, TRUE);
	}
	else{
	    XmToggleButtonSetState(Properties.daxeslog,  TRUE, TRUE);
	    XmToggleButtonSetState(Properties.laxeslin,  TRUE, TRUE);
	}
	break;
      case 4:
	XmToggleButtonSetState(Properties.daxeslog,  TRUE, TRUE);
	XmToggleButtonSetState(Properties.laxeslog,  TRUE, TRUE);
	break;
    }
    /**********min max, tic ints**********/

    /*first remove all callbacks*/
    XtRemoveCallback(Properties.daxeslog,XmNarmCallback,
		     form_linlog,"h");
    XtRemoveCallback(Properties.daxeslog,XmNarmCallback,
		     form_linlog,"v");
    XtRemoveCallback(Properties.laxeslog,XmNarmCallback,
		     form_linlog,"h");
    XtRemoveCallback(Properties.laxeslog,XmNarmCallback,
		     form_linlog,"v");
    XtRemoveCallback(Properties.imin, XmNactivateCallback,
		     form_axes_scale_call, littleo);	
    XtRemoveCallback(Properties.imin, XmNactivateCallback,
		     form_axes_scale_call, littlea);	
    XtRemoveCallback(Properties.imax, XmNactivateCallback,
		     form_axes_scale_call, bigo);	
	XtRemoveCallback(Properties.imax, XmNactivateCallback,
			 form_axes_scale_call, biga);	
    XtRemoveCallback(Properties.dmin, XmNactivateCallback,
		     form_axes_scale_call, littlea);	
    XtRemoveCallback(Properties.dmin, XmNactivateCallback,
		     form_axes_scale_call, littleo);	
    XtRemoveCallback(Properties.dmax, XmNactivateCallback,
		     form_axes_scale_call, biga);	
    XtRemoveCallback(Properties.dmax, XmNactivateCallback,
		     form_axes_scale_call, bigo);	
    XtRemoveCallback(Properties.iminor, XmNactivateCallback,
		     form_tick_call, littley);
    XtRemoveCallback(Properties.iminor, XmNactivateCallback,
		     form_tick_call, littlex);
    XtRemoveCallback(Properties.imajor, XmNactivateCallback,
			form_tick_call, bigy);
    XtRemoveCallback(Properties.imajor, XmNactivateCallback,
		     form_tick_call, bigx);
    XtRemoveCallback(Properties.dminor, XmNactivateCallback,
		     form_tick_call, littlex);
    XtRemoveCallback(Properties.dminor, XmNactivateCallback,
		     form_tick_call, littley);
    XtRemoveCallback(Properties.dmajor, XmNactivateCallback,
		     form_tick_call, bigx);
    XtRemoveCallback(Properties.dmajor, XmNactivateCallback,
		     form_tick_call, bigy);
    XtRemoveCallback(Properties.imin, XmNactivateCallback,
                     form_axes_scale_call, littlea);        
    XtRemoveCallback(Properties.imax, XmNactivateCallback,
                     form_axes_scale_call, biga);
    XtRemoveCallback(Properties.dmin, XmNactivateCallback,
                     form_axes_scale_call, littleo);        
    XtRemoveCallback(Properties.dmax, XmNactivateCallback,
                     form_axes_scale_call, bigo);
    XtRemoveCallback(Properties.imin, XmNactivateCallback,
                     form_axes_scale_call, littleo);        
    XtRemoveCallback(Properties.imax, XmNactivateCallback,
                     form_axes_scale_call, bigo);
    XtRemoveCallback(Properties.dmin, XmNactivateCallback,
                     form_axes_scale_call, littlea);        
    XtRemoveCallback(Properties.dmax, XmNactivateCallback,
                     form_axes_scale_call, biga);
    
    if(fix[2] || !buttons){
	(void)getrvar_("abscissa_min", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.imin, XmNvalue, buff, NULL);

	(void)getrvar_("abscissa_max", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.imax, XmNvalue, buff, NULL);
		      
	(void)getrvar_("ordinate_min", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dmin, XmNvalue, buff, NULL);

	(void)getrvar_("ordinate_max", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dmax, XmNvalue, buff, NULL);

	XtAddCallback(Properties.imin, XmNactivateCallback,
		      form_axes_scale_call, littlea);	
	XtAddCallback(Properties.imax, XmNactivateCallback,
		      form_axes_scale_call, biga);
	XtAddCallback(Properties.dmin, XmNactivateCallback,
		      form_axes_scale_call, littleo);	
	XtAddCallback(Properties.dmax, XmNactivateCallback,
		      form_axes_scale_call, bigo);

	(void)getrvar_("xminor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.iminor, XmNvalue, buff, NULL);
	(void)getrvar_("xmajor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.imajor, XmNvalue, buff, NULL);
	
	(void)getrvar_("yminor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dminor, XmNvalue, buff, NULL);
	(void)getrvar_("ymajor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dmajor, XmNvalue, buff, NULL);

	XtAddCallback(Properties.iminor, XmNactivateCallback,
		     form_tick_call, littlex);	
	XtAddCallback(Properties.imajor, XmNactivateCallback,
		      form_tick_call, bigx);
	XtAddCallback(Properties.dminor, XmNactivateCallback,
		      form_tick_call, littley);	
	XtAddCallback(Properties.dmajor, XmNactivateCallback,
		      form_tick_call, bigy);
	XtAddCallback(Properties.laxeslog,XmNarmCallback,
		      form_linlog,"h");
	XtAddCallback(Properties.daxeslog,XmNarmCallback,
		      form_linlog,"v");
    }	
    else{
	(void)getrvar_("abscissa_min", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
		XtVaSetValues(Properties.dmin, XmNvalue, buff, NULL);
	(void)getrvar_("abscissa_max", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dmax, XmNvalue, buff, NULL);
		      
	(void)getrvar_("ordinate_min", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.imin, XmNvalue, buff, NULL);
	(void)getrvar_("ordinate_max", &rvar, &error, 12);
	if(rvar > bigone)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.imax, XmNvalue, buff, NULL);

	XtAddCallback(Properties.dmin, XmNactivateCallback,
		      form_axes_scale_call, littlea);	
	XtAddCallback(Properties.dmax, XmNactivateCallback,
		      form_axes_scale_call, biga);
	XtAddCallback(Properties.imin, XmNactivateCallback,
		      form_axes_scale_call, littleo);	
	XtAddCallback(Properties.imax, XmNactivateCallback,
		      form_axes_scale_call, bigo);

	(void)getrvar_("xminor", &rvar, &error, 6);
	if(rvar == 0.0)
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dminor, XmNvalue, buff, NULL);
	(void)getrvar_("xmajor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.dmajor, XmNvalue, buff, NULL);
	
	(void)getrvar_("yminor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.iminor, XmNvalue, buff, NULL);
	(void)getrvar_("ymajor", &rvar, &error, 6);
	if((rvar > -FLT_EPSILON) && (rvar <  FLT_EPSILON))
	  sprintf(buff, "default");
	else
	  sprintf(buff, "%g",rvar);
	XtVaSetValues(Properties.imajor, XmNvalue, buff, NULL);

	XtAddCallback(Properties.dminor, XmNactivateCallback,
		     form_tick_call, littlex);	
	XtAddCallback(Properties.dmajor, XmNactivateCallback,
		      form_tick_call, bigx);
	XtAddCallback(Properties.iminor, XmNactivateCallback,
		      form_tick_call, littley);	
	XtAddCallback(Properties.imajor, XmNactivateCallback,
		      form_tick_call, bigy);
	XtAddCallback(Properties.daxeslog,XmNarmCallback,
		      form_linlog,"h");
	XtAddCallback(Properties.laxeslog,XmNarmCallback,
		      form_linlog,"v");
    }
    return(1);
}

setup_skewt_form()
{
    char buff[256];
    int error;

    /**********skewt comp**********/
    buff[0]='\0';
    (void)getavar_ ("scomp_1", buff, &error, 7, 256);
    XtVaSetValues(Properties.scomp_1, XmNvalue, buff, NULL);
    buff[0]='\0';
    (void)getavar_ ("scomp_2", buff, &error, 7, 256);
    XtVaSetValues(Properties.scomp_2, XmNvalue, buff, NULL);
    buff[0]='\0';
    (void)getavar_ ("scomp_3", buff, &error, 7, 256);
    XtVaSetValues(Properties.scomp_3, XmNvalue, buff, NULL);
}

setup_traj_form()
{
    char buff[256];
    XmString str, NewString();
    float time1, time2, times1[4], times2[4];
    int time_ind, error, i=4;

    /**********traj comp**********/
    buff[0]='\0';
    (void)getavar_ ("trajcomp_1", buff, &error, 10, 256);
    XtVaSetValues(Properties.tcomp_1, XmNvalue, buff, NULL);
    buff[0]='\0';
    (void)getavar_ ("trajcomp_2", buff, &error, 10, 256);
    XtVaSetValues(Properties.tcomp_2, XmNvalue, buff, NULL);
    buff[0]='\0';
    (void)getavar_ ("trajcomp_3", buff, &error, 10, 256);
    XtVaSetValues(Properties.tcomp_3, XmNvalue, buff, NULL);
    
    /**********traj int**************/
    (void)getlvar_("time_ind", &time_ind, &error, 8);
    if(!XtIsManaged(Properties.traj_ts)){
      XtManageChild(Properties.traj_ts);
      XtManageChild(Properties.traj_te);
      XtManageChild(Properties.traj_ts_lab);
      XtManageChild(Properties.traj_te_lab);
    }
    (void)getrarr_("plmin_scaled", times1, &i, &error, 12, 4);
    
    (void)getrarr_("plmax_scaled", times2, &i, &error, 12, 4);
    (void)getlvar_("time_ind", &time_ind, &error, 8);
    
    (void)getrvar_("trajbeg", &time1, &error, 7);
    if(time1 < times1[3])time1 = times1[3];
    if(time1 > times2[3])time1 = times2[3];
    (void)getrvar_("trajend", &time2, &error, 7);
    if(time2 < times1[3])time2 = times1[3];
    if(time2 > times2[3])time2 = times2[3];
    
    if(time1==time2 && time2==0) {
      time2=times2[3];
      time1=times1[3];
    }
    
    XtVaSetValues(Properties.traj_ts,XmNmaximum, (int)times2[3],
		  XmNminimum, (int)times1[3],
		  XmNvalue,(int)time1,NULL);
    
    XtVaSetValues(Properties.traj_te,XmNmaximum, (int)times2[3],
		  XmNminimum, (int)times1[3],
		  XmNvalue,(int)time2,NULL);
    if(time_ind){
	str = NewString("Yes");
      XtSetSensitive(Properties.traj_ts, FALSE);
      XtSetSensitive(Properties.traj_ts, FALSE);
    }
    else{
      	str = NewString("No ");
      XtSetSensitive(Properties.traj_ts, TRUE);
      XtSetSensitive(Properties.traj_ts, TRUE);
    }
    XtVaSetValues(Properties.traj_ind,XmNlabelString,str,NULL);
    XmStringFree(str);
}
