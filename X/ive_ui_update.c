static char ident[] = "$Id: ive_ui_update.c,v 1.68 2002/11/27 00:30:52 warren Exp $";

/*****************************************************************************
 *$Log: ive_ui_update.c,v $
 *Revision 1.68  2002/11/27 00:30:52  warren
 *24 bit color enhancements are here. Works on Linux and Sun so far - dec to
 *be tested still.
 *
 *Revision 1.67  2002/08/09 19:57:27  warren
 *Fixed for Linux and tested with DEC sun etc.
 *most items inside #ifdef's, or were general cleanup/fixes
 *
 *Revision 1.66  2002/06/14 19:43:50  harryive
 *Allow specification of terrain contours by value.
 *
 *Revision 1.65  2002/03/11 20:13:57  davidive
 *Fixed ui_pudate to update trajectory step and value.
 *
 *Revision 1.64  2001/08/15 22:23:21  davidive
 *Cleaned up code to reduce warnings about type mismatches, and added buttons
 *and callbacks to the 1d form widget to select which access to label against
 *for diagonal plots.
 *
 *Revision 1.63  2001/03/14 20:21:13  warren
 *corrected severa longs to unsigned longs, changed creation of lock button to
 *not set the name, the set the label seperately, change XtFree to XmStringFree
 *where it should be.
 *
 *Revision 1.62  2001/03/13 21:35:42  davidive
 *Modified to update the free locked button on the color slide widget when the
 *locked_field is ' '
 *
 *Revision 1.61  2000/03/03 17:09:24  harry
 *Eliminate free of envvar.
 *
 *Revision 1.60  2000/02/11 23:14:24  harry
 *Allow surface 1d plots.  Fix Z slider for surface plots.
 *
 *Revision 1.59  1999/12/22 00:32:06  warren
 *Fixed Dales crashing bug when changing from point slicer to normal. Also
 *have some ifdef linuxes in there in case we ever find a fortran compiler.
 *
 *Revision 1.58  1999/02/04 00:28:08  harry
 *Add vector scaling.
 *
 *Revision 1.57  1999/01/29 20:45:24  warren
 *new stuff to deal with the trajectory form
 *
 *Revision 1.56  1998/12/31 16:01:09  warren
 *Firts cut of new trajectory look and feel. Trajectories are no longer
 *on the popup menu, just plot type.
 *
 *Revision 1.55  1998/09/04 20:51:45  harry
 *Add colorbar button to annotation widget.  Rewrite file widgets to
 *remember previous entries.
 *
 *Revision 1.54  1998/06/11 21:52:29  harry
 *"Except On Overlay" should have the same capitalization everywhere.
 *
 *Revision 1.53  1998/03/04 19:13:38  davidive
 *First cut at trajectories and trajectory widget using colors to show depth
 *and arrows to show direction.
 *
 *Revision 1.52  1997/12/31 23:31:02  warren
 *Fixed Dales color loxking problem, holton's crashes etc.
 *
 *Revision 1.51  1997/10/29 18:25:44  harry
 *Allow loops for any dimension.
 *
 *Revision 1.50  1997/10/22 19:34:39  davidive
 *Turned the particle movement part of trajectories into a user transform
 *and added unit checking to the default transform.
 *
 *Revision 1.49  1997/06/27 21:06:27  harry
 *Designed multiplot printing.
 *
 *Revision 1.48  1997/06/03 19:13:46  davidive
 *The big mouse slicing changeover. We have the new form, only requires you
 *set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 *always runs through init_points_ to catch all these things.
 *note: only actually works when two dimentions have points set. Needs to
 *be expanded into 1, 3 or 4.
 *
 *Revision 1.47  1997/04/16 16:51:47  warren
 *Final fix to color locking. Now we say "field" locked
 *
 *
 *Revision 1.46  1997/04/15 19:36:07  warren
 *Put in pieces to fix color lock indicator in the future.
 *
 *Revision 1.45  1997/04/15 17:19:42  harry
 *Remove time_tag looping - now handles by multiple files.
 *
 *Revision 1.44  1997/03/12 18:57:24  harry
 *Add menu for map outlines.
 *
 *Revision 1.43  1997/02/27 18:57:59  warren
 *Ive starts with an I not a V
 *
 *Revision 1.42  1997/02/21 21:16:55  warren
 *Final fixes for point/mouse slicing and expanding into 2d. It is tested
 *with the z dimention free and seems to work with others, but still should
 *be looked at with known data for this.
 *
 *Revision 1.41  1997/02/21 20:01:31  harry
 *Fix vector interval to be floating point and have values less than one.
 *
 *Revision 1.40  1996/11/07 16:53:59  warren
 *First set of activated point slicing and widgets to go with it. Only 1d plots
 *are known to be correct. Use at your own risk for 2d. There is still a known
 *bug in the connection between the tic mark entry boxes and the plot when doing
 *point or mouse slicing.
 *
 *Revision 1.39  1996/10/11 19:22:39  harry
 *Bug in VINT update - updated x slider with y value and never updated x.
 *Add call to VINT update when a LOC update occurs.
 *
 * Revision 1.38  1996/10/11  01:12:06  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 *Revision 1.37  1996/10/09 18:54:10  harry
 *Add the ability to loop on file names if time value is part of name,
 *and there is only one time per file.
 *
 *Revision 1.36  1996/08/09 19:28:07  harry
 *Take out %.2f formats and replace with calls to float_nozero.
 *
 *Revision 1.35  1996/07/12 20:58:45  harry
 *Change data and domain slopes and intercepts to real*8.
 *
 *Revision 1.34  1996/04/25 16:24:24  harry
 *Add Skew-T plots and the beginning of trajectory plots.
 *
 *Revision 1.33  1996/04/01 20:23:11  warren
 *Terrain form and new terrain command (on / off)
 *
 *Revision 1.32  1996/01/12 16:11:01  warren
 *Transfer of new pattern stuff from 3.x and beginning of color fixes
 *(color vectors, correct collors on the pulldowan menus)
 *
 *Revision 1.31  1995/12/15 22:32:41  warren
 *moved patterns into 4.0 and implemented the list of fixes and changes from
 *Curtis.
 *
 * Revision 1.30  1995/10/18  19:16:55  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.29  1995/06/23  21:11:24  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.28  1995/02/28  19:29:24  warren
 * Fixed problem on alpha's where the popup type in boxes didn't move the
 * sliders for the window.
 *
 * Revision 1.27  1994/12/20  22:26:49  warren
 * made the rest of the scale widgets typing capable
 *
 * Revision 1.26  1994/11/22  22:46:43  harry
 * Correct vector locking, add support for vector positioning, add
 * support for filled contour overlays.
 *
 * Revision 1.25  1994/10/08  18:59:55  warren
 * The annotation widget and updating it.
 *
 * Revision 1.24  1994/09/20  17:10:58  warren
 * all lock of coor to value: lock=contour val, color table entry.
 * Also fixe widget to update correctly
 *
 * Revision 1.23  1994/09/14  18:19:18  harry
 * Big 1D flip uncommit.
 *
 * Revision 1.22  1994/06/20  16:35:40  harry
 * Change index to strchr, and rindex to strrchr.
 *
 * Revision 1.21  1994/05/06  17:09:12  yuri
 * Main vector lock commit + small correction in X/ive_ui_update.c file
 *
 * Revision 1.20  1994/04/26  18:06:05  warren
 * Beginning of mouse slice interface (doesn't really do anything yet except
 * mark up your plot)
 *
 * Revision 1.19  1994/04/19  23:01:47  yuri
 * Big 1D flip commit.
 *
 * Revision 1.18  1994/04/14  23:31:04  harry
 * Add vector lock updating.
 *
 * Revision 1.17  1994/03/30  22:16:26  harry
 * I don't think the abscissa and ordinate tick marks should always be
 * zero.  What do you think?
 *
 * Revision 1.16  1993/12/30  00:48:32  warren
 * Update screen before returning from driver, and make sure char pointers in
 * ui_update are bigger than 0.
 *
 * Revision 1.15  1993/12/15  18:11:16  harry
 * Update map background widget - add default button and clipping menu.
 *
 * Revision 1.14  1993/11/29  19:37:59  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.setup_contour_form
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.13  1993/10/13  00:03:09  warren
 * Update current version for fixes to 3.0 version related to the meaning of
 * default in the property sheet boxes.
 *
 * Revision 1.12  1993/08/11  21:43:38  warren
 * fixed broken togglebuttons
 *
 * Revision 1.11  1993/07/21  20:30:19  warren
 * added missing break in switch statement fixind Dales vrashing problem.
 *
 * Revision 1.10  1993/07/09  16:36:09  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.9  1993/07/07  18:14:27  harry
 * Remove calls to check_index since they are no longer needed and they
 * are harmful.
 *
 * Revision 1.8  1993/06/29  00:13:38  warren
 * Changed the LOC update to update fixed_index as well.
 *
 * Revision 1.7  1993/06/25  20:05:55  warren
 * Do updates tp map widget.
 *
 * Revision 1.6  1993/06/22  19:34:40  warren
 * rearanged background and plottype on main widget and fixed associated calls
 * appropriately. Also fixed main to call driver correctly.
 *
 * Revision 1.5  1993/06/01  23:59:05  warren
 * Fixed Vector component callback and Vector Interval Callback.
 *
 * Revision 1.4  1993/05/27  16:38:17  warren
 * Recover files removed by yuri's commit
 *
 * Revision 1.2  1993/04/26  19:18:56  warren
 * See previous message
 *
 * Revision 1.1.1.1  1993/04/07  19:41:23  harry
 * Initial CVS checkin of IVE.
 *
 *****************************************************************************/
#include <X11/Intrinsic.h>
#include <Xm/XmAll.h>
#include <string.h>
#include <ive_widgets.h>
#include <properties.h>
#include <loop_setup.h>
#include <ive_text_enum.h>
#include <file_widgets.h>
#include <malloc.h>
#include <stdlib.h>

extern int fixed_index[4]; /*for slicing use*/
extern double epsilon_();
extern void getavar_(),getivar_(),getrarr_(),getrvar_(),getivar_(),getlvar_(),
  init_points_(),getiarr_(),getdarr_(),scale_(),set_landscape_widgets();
extern int get_button_name();
#define bigone 1.0E37
#ifndef MAX
#define MAX(x, y) (((x) > (y))? (x):(y)) 
#endif
#ifndef MIN
#define MIN(x, y) (((x) < (y))? (x):(y))
#endif
 

/* Definitions are in ive_widgets.h and ive_widgets.inc, 
   note: both must be updated simultaniously */

int ui_intit(val)
    float val;
{
    if(val >=0)
	return ((int)(val +.5));
    else
	return ((int)(val -.5));
}

void ui_update_(window)
     int *window;
{
    extern Widget Box;
    int i,hold,ivar,ival,iarr[4];
    long int error=0, lvar=0, lvar2=0;
    float f,rvar,wind[4],min[4],max[4],pat[2],rarr[40];
    double slope[4], intercept[4];
    char *junk,avar[256],retstr[80];
    static char *envvar=NULL,*envvar2=NULL,*envvar3=NULL;
    static int one=1, four=4;
    static float zero=0.0, fone=1.0;
    Arg args[2];
    XmString str,str2,str3;
    extern XmString NewString();
    char *float_nozero();

    switch(*window){
      case DIR:  
	getavar_("datfil",avar,&error,6,256);
	if(error)break;
	junk = (char *)strrchr(avar,'/');
	if((i=(long)junk - (long)avar) <= (long)strlen(avar) && i > 0)
	  *(junk+1)='\0';
	set_file_box(dir_widget.List, avar, &dir_widget.fsb);
	if (!dir_widget.List) {
	    if(envvar){
		junk = (char *)realloc(envvar,strlen(avar)+1+9);
		if((long)junk)envvar=junk;
		else break;
	    }
	    else{
		if((envvar=(char *)malloc(strlen(avar)+1+9))==(char *) 0)
		  break;
	    }
	    sprintf(envvar,"IVEDAT=%s",avar);
	    putenv(envvar);
	}
	break;
      case BUTTONS:
	getavar_("button_name",avar,&error,11,256);
	i=0;
	while(buttons[i] != NULL){
	    XtSetArg(args[0],XmNuserData,&junk);
	    XtGetValues(buttons[i], args, 1);
	      if(strcmp(junk,avar)){
		  XtSetArg(args[0],XmNset,FALSE);
		  XtSetValues(buttons[i],args,1);
	      }
	      else{
		  XtSetArg(args[0],XmNset,TRUE);
		  XtSetValues(buttons[i],args,1);
	      }
	    i++;
	}
	break;
      case CINT: 
	if(Properties.dep_form_c && XtIsManaged(Properties.dep_form_c)){
	    (void)getivar_("num_vals", &ivar, &error, 8);
	    if(error)break;
	    if(ivar>0){
		getrarr_("cont_values", rarr, &ivar, &error, 11);
		retstr[0]='\0';
		if(ivar){
		    strcpy(retstr, float_nozero(rarr[0]));
		}
		else {
		    ivar = 1;
		    sprintf(retstr,"0");
		}
		for (i=1; i<ivar; i++){
		    sprintf(retstr,"%s,%s",retstr,float_nozero(rarr[i]));
		}
		XtVaSetValues(Properties.cont_values, XmNvalue, retstr, NULL);
		XtVaSetValues(Properties.cont_val, XmNset, TRUE, NULL);
		XtVaSetValues(Properties.cont_int, XmNset, FALSE, NULL);
	    }
	    else{
		getrvar_("cont_int", &rvar, &error, 8);
		strcpy(retstr, float_nozero(rvar));
		XtVaSetValues(Properties.cont_values, XmNvalue, 
			      retstr, NULL);
		XtVaSetValues(Properties.cont_val, XmNset, FALSE, NULL);
		XtVaSetValues(Properties.cont_int, XmNset, TRUE, NULL);
	    }
	}
	break;
      case TCINT: 
	if(terrain_forms.terrain_form 
	   && XtIsManaged(terrain_forms.terrain_form)){
	    (void)getivar_("tnum_vals", &ivar, &error, 9);
	    if(error)break;
	    if(ivar>0){
		getrarr_("tcont_values", rarr, &ivar, &error, 12);
		retstr[0]='\0';
		if(ivar){
		    strcpy(retstr, float_nozero(rarr[0]));
		}
		else {
		    ivar = 1;
		    sprintf(retstr,"0");
		}
		for (i=1; i<ivar; i++){
		    sprintf(retstr,"%s,%s",retstr,float_nozero(rarr[i]));
		}
		XtVaSetValues(terrain_forms.cont_values, XmNvalue,
			      retstr, NULL);
		XtVaSetValues(terrain_forms.cont_val, XmNset, TRUE, NULL);
		XtVaSetValues(terrain_forms.cont_int, XmNset, FALSE, NULL);
	    }
	    else{
		getrvar_("tcint", &rvar, &error, 5);
		strcpy(retstr, float_nozero(rvar));
		XtVaSetValues(terrain_forms.cont_values, XmNvalue, 
			      retstr, NULL);
		XtVaSetValues(terrain_forms.cont_val, XmNset, FALSE, NULL);
		XtVaSetValues(terrain_forms.cont_int, XmNset, TRUE, NULL);
	    }
	}
	break;
    case TERR:
	if (terrain_forms.terrain_form){
	    getlvar_("terrflg",&lvar,&error,7);
	    if(lvar) str=XmStringCreateSimple("Yes");
	    else str=XmStringCreateSimple("No ");
	    XtVaSetValues(terrain_forms.show_ter,
			  XmNlabelString,str,
			  NULL);
	    XmStringFree(str);
	}
	break;
      case NUNLAB:
      case LABELLINE:
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c)){
	    getlvar_("cllab", &lvar, &error, 5);
	    if(!lvar)XmScaleSetValue(Properties.numlines,0);
	    else{
		getivar_("nulbll", &ivar, &error, 6);
		XmScaleSetValue(Properties.numlines,ivar + 1);
	    }
	}
	break;
      case NOZERO:
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c)){
	    getlvar_("no_zero",&lvar,&error,7);
	    if(!lvar){
		str = NewString("Yes");
	    }
	    else{
		str = NewString("No ");
	    }
	    XtVaSetValues(Properties.zero, XmNlabelString, str, NULL);
	    XmStringFree(str);
 	}
	break;
      case LABELPLOT:
	if (map_forms.map_form){
	    getlvar_("label",&lvar,&error,5);
	    if(lvar) str=XmStringCreateSimple("Yes");
	    else str=XmStringCreateSimple("No ");
	    XtVaSetValues(map_forms.labbutton,
			  XmNlabelString,str,
			  NULL);
	    XmStringFree(str);
	}
	if (Annotation.form){
	    getlvar_("label",&lvar,&error,5);
	    if(lvar) str=XmStringCreateSimple("On ");
	    else str=XmStringCreateSimple("Off");
	    XtVaSetValues(Annotation.label,
			  XmNlabelString,str,
			  NULL);
	    XmStringFree(str);
	}
	if (terrain_forms.terrain_form){
	    getlvar_("label",&lvar,&error,5);
	    if(lvar) str=XmStringCreateSimple("Yes");
	    else str=XmStringCreateSimple("No ");
	    XtVaSetValues(terrain_forms.labbutton,
			  XmNlabelString,str,
			  NULL);
	    XmStringFree(str);
	}
	break;
      case FILLTYPE:
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c)){
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
	}
	break;
      case AXES:
	getivar_("linlog",&ivar,&error,6);
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c)){
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
	    XSync(XtDisplay(Box), FALSE);	    
	}
	else if (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
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
	    XSync(XtDisplay(Box), FALSE);
	}
	else if (Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	    switch(ivar){
	      case 1:
		XmToggleButtonSetState(Properties.daxeslin,  TRUE, TRUE);
		XmToggleButtonSetState(Properties.laxeslin,  TRUE, TRUE);
		break;
	      case 2:
		if(fixed_index[2]){
		    XmToggleButtonSetState(Properties.daxeslog,  TRUE, TRUE);
		    XmToggleButtonSetState(Properties.laxeslin,  TRUE, TRUE);
		}
		else{
		    XmToggleButtonSetState(Properties.daxeslin,  TRUE, TRUE);
		    XmToggleButtonSetState(Properties.laxeslog,  TRUE, TRUE);
		}
		break;
	      case 3:
		if(fixed_index[2]){
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
	    XSync(XtDisplay(Box), FALSE);
	}	    
	break;
      case XTICS: 
	(void)getrvar_("xminor", &rvar, &error, 6);
	if((rvar > 0.0 - (float)epsilon_(&fone)) && 
	   (rvar <  0.0 + (float)epsilon_(&fone)))
	  sprintf(retstr, "default");
	else
	  strcpy(retstr, float_nozero(rvar));
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c))
	  XtVaSetValues(Properties.xminor, XmNvalue, retstr, NULL);
	else if 
	  (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v))
	    XtVaSetValues(Properties.xminorv, XmNvalue, retstr, NULL);
	else if
	  (Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	      if(fixed_index[2])
		XtVaSetValues(Properties.iminor, XmNvalue, 
			      retstr, NULL);
	      else
		XtVaSetValues(Properties.dminor, XmNvalue, 
			      retstr, NULL);
	  }
	(void)getrvar_("xmajor", &rvar, &error, 6);
	if((rvar > 0.0 -  (float)epsilon_(&fone)) && 
	   (rvar <  0.0 + (float)epsilon_(&fone)))
	  sprintf(retstr, "default");
	else
	  strcpy(retstr, float_nozero(rvar));
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c))
	  XtVaSetValues(Properties.xmajor, XmNvalue, retstr, NULL);
	else if (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v))
	  XtVaSetValues(Properties.xmajor, XmNvalue, retstr, NULL);
	else if(Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	    if(fixed_index[2])
	      XtVaSetValues(Properties.imajor, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.dmajor, XmNvalue, retstr, NULL);
	}
	break;
      case YTICS: 
	(void)getrvar_("yminor", &rvar, &error, 6);
	if((rvar > 0.0 - (float)epsilon_(&fone)) && 
	   (rvar <  0.0 + (float)epsilon_(&fone)))
	  sprintf(retstr, "default");
	else
	  strcpy(retstr, float_nozero(rvar));
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c))
	  XtVaSetValues(Properties.yminor, XmNvalue, retstr, NULL);
	else if (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v))
	  XtVaSetValues(Properties.yminor, XmNvalue, retstr, NULL);
	else if(Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	    if(fixed_index[2])
	      XtVaSetValues(Properties.dminor, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.iminor, XmNvalue, retstr, NULL);
	}
	(void)getrvar_("ymajor", &rvar, &error, 6);
	if((rvar > 0.0 - (float)epsilon_(&fone)) && 
	   (rvar < 0.0 + (float)epsilon_(&fone)))
	  sprintf(retstr, "default");
	else
	  strcpy(retstr, float_nozero(rvar));
	if(Properties.dep_form_c&&XtIsManaged(Properties.dep_form_c))
	  XtVaSetValues(Properties.ymajor, XmNvalue, retstr, NULL);
	else if (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v))
	  XtVaSetValues(Properties.ymajor, XmNvalue, retstr, NULL);
	else if(Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	    if(fixed_index[2])
	      XtVaSetValues(Properties.dmajor, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.imajor, XmNvalue, retstr, NULL);
	}
	break;
      case TICLABELS:
	if (Annotation.tic_labels){
	    getivar_("tic_labels",&ivar,&error,10);
	    switch(ivar){
	      case 0:	 /*norm*/
		XtVaSetValues(Annotation.tic_labels_off,
			      XmNset, FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_labels_norm,
			      XmNset, TRUE,
			      NULL);
		XtVaSetValues(Annotation.tic_labels_bold,
			      XmNset, FALSE,
			      NULL);
		break;
	      case 1:   /*bold*/
                XtVaSetValues(Annotation.tic_labels_off,
                              XmNset, FALSE,
                              NULL);
                XtVaSetValues(Annotation.tic_labels_norm,
                              XmNset, FALSE,
                              NULL);
                XtVaSetValues(Annotation.tic_labels_bold,
                              XmNset, TRUE,
                              NULL);
 		break;
	      case 2:   /*off*/
                XtVaSetValues(Annotation.tic_labels_off,
                              XmNset, TRUE,
                              NULL);
                XtVaSetValues(Annotation.tic_labels_norm,
                              XmNset, FALSE,
                              NULL);
                XtVaSetValues(Annotation.tic_labels_bold,
                              XmNset, FALSE,
                              NULL);
 		break;
	    }
	}
	break;
      case TICSCALE:
	if (Annotation.tic_scale){
	    getrvar_("tic_scale",&rvar,&error,9);
	    sprintf(avar,"%2g",rvar);
	    XmTextSetString(Annotation.tic_scale,avar);
	}
	break;
      case TICDECIMALS:
	if (Annotation.tic_decimals){
	    getivar_("tic_decimals",&ivar,&error,12);
	    if(ivar == -1)
	      strcpy(avar,"Default");
	    else
	      sprintf(avar,"%d",ivar);
	    XmTextSetString(Annotation.tic_decimals,avar);
	}
	break;
      case TICWIDTH:
	if (Annotation.tic_widthmaj){
	    getrvar_("tic_width_maj",&rvar,&error,13);
	    switch((int)rvar){
	      case 1:
		XtVaSetValues(Annotation.tic_widthmaj_thin,
			      XmNset,TRUE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmaj_thick,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmaj_thickest,
			      XmNset,FALSE,
			      NULL);
		break;
	      case 2:
		XtVaSetValues(Annotation.tic_widthmaj_thin,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmaj_thick,
			      XmNset,TRUE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmaj_thickest,
			      XmNset,FALSE,
			      NULL);
		break;
	      case 3:
		XtVaSetValues(Annotation.tic_widthmaj_thin,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmaj_thick,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmaj_thickest,
			      XmNset,TRUE,
			      NULL);
		break;
	    }
	}
	if (Annotation.tic_widthmin){
	    getrvar_("tic_width_min",&rvar,&error,13);
	    switch((int)rvar){
	      case 1:
		XtVaSetValues(Annotation.tic_widthmin_thin,
			      XmNset,TRUE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmin_thick,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmin_thickest,
			      XmNset,FALSE,
			      NULL);
		break;
	      case 2:
		XtVaSetValues(Annotation.tic_widthmin_thin,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmin_thick,
			      XmNset,TRUE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmin_thickest,
			      XmNset,FALSE,
			      NULL);
		break;
	      case 3:
		XtVaSetValues(Annotation.tic_widthmin_thin,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmin_thick,
			      XmNset,FALSE,
			      NULL);
		XtVaSetValues(Annotation.tic_widthmin_thickest,
			      XmNset,TRUE,
			      NULL);
		break;
	    }
	}
	break;
      case WINDOW:
	if (slicer_widget.info_2d){
	    int test;
	    int buttons;
	    (void)getivar_("use_buttons",&buttons, &error, 11);
	    ivar=4;
	    getrarr_("plwmin_scaled",wind,&ivar,&error,13);
	    if(!error) {
		for(i=0;i<4;i++)
		  if (slicer_widget.mins[i]!=NULL) 
		    if (XtIsManaged(slicer_widget.maxs[i])) {
		      ival = (int)wind[i];
		      XmScaleSetValue(slicer_widget.mins[i],ival);
		    }
	    }
	    getrarr_("plwmax_scaled",wind,&ivar,&error,13);
	    if(!error){
		for(i=0;i<4;i++)
		  if (slicer_widget.maxs[i]!=NULL){
		    ival = (int)wind[i];
		    XmScaleSetValue(slicer_widget.maxs[i],ival);
		  }
	    }
	}
	if(!buttons){
	    i = POINTS_UI;
	    ui_update_(&i);
	}
	break;
    case POINTS_UI:
      (void)init_points_();
      break;
    case TRANS:  
	getavar_("transform_path",avar,&error,14,256);
	if(error)break;
	junk = (char *)strrchr(avar,'/');
	if(((long)junk - (long)avar) <= (long)strlen(avar)) *(junk+1)='\0';
	set_file_box(trans_widget.List, avar, &trans_widget.fsb);
	if (!trans_widget.List) {
	    if(envvar2){
		junk = (char *)realloc(envvar2,strlen(avar)+1+9);
		if((long)junk)envvar2=junk;
		else break;
	    }
	    else{
		if((envvar2=(char *)malloc(strlen(avar)+1+11))==(char *) 0)
		  break;
	    }
	    sprintf(envvar2,"IVETRANS=%s",avar);
	    putenv(envvar2);
	}
	break;
      case CTB:  
	getavar_("clrtab",avar,&error,6,256);
	if(error)break;
	junk = (char *)strrchr(avar,'/');
	if(((long)junk - (long)avar) <= (long)strlen(avar)) *(junk+1)='\0';
	set_file_box(ctb_widget.List, avar, &ctb_widget.fsb);
	if (!ctb_widget.List) {
	    if(envvar3){
		junk = (char *)realloc(envvar3,strlen(avar)+1+9);
		if((long)junk)envvar3=junk;
		else break;
	    }
	    else{
		if((envvar3=(char *)malloc(strlen(avar)+1+9))==(char *) 0)
		  break;
	    }
	    sprintf(envvar3,"IVECTB=%s",avar);
	    putenv(envvar3);
	}
	break;
      case DRMAP:
	if(map_forms.map_form){
	    getlvar_("mapflg",&lvar,&error,6);
	    if(lvar) str=XmStringCreateSimple("Yes");
	    else str=XmStringCreateSimple("No ");
	    XtVaSetValues(map_forms.drawmap,
			  XmNlabelString,str,
			  NULL);
	    XmStringFree(str);
	}
	break;
      case MAPOUT:
	  getavar_("outlin",avar,&error,6,256);
	  if(!error && map_forms.map_form) {
	      if(!strcasecmp("no",avar))
		  XtVaSetValues(map_forms.outline,
				XmNmenuHistory, map_forms.out_no,
				NULL);
	      else if(!strcasecmp("co",avar))
		  XtVaSetValues(map_forms.outline,
				XmNmenuHistory, map_forms.out_co,
				NULL);
	      else if(!strcasecmp("us",avar))
		  XtVaSetValues(map_forms.outline,
				XmNmenuHistory, map_forms.out_us,
				NULL);
	      else if(!strcasecmp("po",avar))
		  XtVaSetValues(map_forms.outline,
				XmNmenuHistory, map_forms.out_po,
				NULL);
	      else if(!strcasecmp("m3",avar))
		  XtVaSetValues(map_forms.outline,
				XmNmenuHistory, map_forms.out_m3,
				NULL);
	      else if(!strcasecmp("m1",avar))
		  XtVaSetValues(map_forms.outline,
				XmNmenuHistory, map_forms.out_m1,
				NULL);
	  }
	  break;
      case DEFAULT_MAP_UI:
	if(map_forms.map_form){
	    getlvar_("defmap",&lvar,&error,6);
	    if(error)break;
	    if(lvar)
		str=XmStringCreateSimple("Yes");
	    else
		str=XmStringCreateSimple("No ");
	    XtVaSetValues(map_forms.defaultbutton,
			  XmNlabelString,str,
			  NULL);
	    XmStringFree(str);
	}
	break;
      case LAT:
	if(map_forms.map_form){
	    getrvar_("plat",&rvar,&error,4);
	    if(error)break;
	    strcpy(retstr, float_nozero(rvar));
	    XtVaSetValues(map_forms.plat,
			  XmNvalue,retstr,
			  NULL);
	}
	break;
      case LON:
	if(map_forms.map_form){
	    getrvar_("plon",&rvar,&error,4);
	    if(error)break;
	    strcpy(retstr, float_nozero(rvar));
	    XtVaSetValues(map_forms.plon,
			  XmNvalue,retstr,
			  NULL);
	}
	break;
      case ROT:
	if(map_forms.map_form){
	    getrvar_("rota",&rvar,&error,4);
	    if(error)break;
	    strcpy(retstr, float_nozero(rvar));
	    XtVaSetValues(map_forms.rota,
			  XmNvalue,retstr,
			  NULL);
	}
	break;
      case MAPCLIP_UI:
	getavar_("maplim", avar, &error, 6, 256);
	getlvar_("defmap", &lvar, &error, 6);
	getlvar_("exact_fit", &lvar2, &error, 9);
	if(!error && map_forms.map_form) {
	    if(!strcasecmp("cl", avar) || (lvar && lvar2)) {
		XtVaSetValues(map_forms.clip,
			    XmNmenuHistory, map_forms.clip_window, NULL);
	    }
	    else {
		XtVaSetValues(map_forms.clip,
			      XmNmenuHistory, map_forms.clip_last, NULL);
	    }
	}
	break;
      case GRID:
	if(map_forms.map_form){
	    getrvar_("grdspa",&rvar,&error,6);
	    if(error)break;
	    strcpy(retstr, float_nozero(rvar));
	    XtVaSetValues(map_forms.glspace,
			  XmNvalue,retstr,
			  NULL);
	}
	break;
      case ORDINATE:
	if (Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	    (void)getrvar_("ordinate_min", &rvar, &error, 12);
	    if (error) break;
            if(rvar > bigone)
	      sprintf(retstr, "default");
	    else
	      strcpy(retstr, float_nozero(rvar));
	    if(fixed_index[2])
	      XtVaSetValues(Properties.dmin, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.imin, XmNvalue, retstr, NULL);
	    
	    (void)getrvar_("ordinate_max", &rvar, &error, 12);
	    if (error) break;
	    if(rvar > bigone)
	      sprintf(retstr, "default");
	    else
	      strcpy(retstr, float_nozero(rvar));
	    if(fixed_index[2])
	      XtVaSetValues(Properties.dmax, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.imax, XmNvalue, retstr, NULL);
	}
	break;
      case ABSCISSA:
	if (Properties.dep_form_l&&XtIsManaged(Properties.dep_form_l)){
	    (void)getrvar_ ("abscissa_min", &rvar, &error, 12);
	    if (error) break;
            if(rvar >bigone)
	      sprintf(retstr, "default");
	    else
	      strcpy(retstr, float_nozero(rvar));
	    if(fixed_index[2])
	      XtVaSetValues(Properties.imin, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.dmin, XmNvalue, retstr, NULL);
	    
	    (void)getrvar_ ("abscissa_max", &rvar, &error, 12);
	    if (error) break;
            if(rvar >bigone)
	      sprintf(retstr, "default");
	    else
	      strcpy(retstr, float_nozero(rvar));
	    if(fixed_index[2])
	      XtVaSetValues(Properties.imax, XmNvalue, retstr, NULL);
	    else
	      XtVaSetValues(Properties.dmax, XmNvalue, retstr, NULL);
	}
	break;
/*      case TOPOSCAL:
	if (line_form) {
	    (void)getrvar_ ("topo_scale", &rvar, &error, 10);
	    if (error) break;
	    strcpy(retstr, float_nozero(rvar));
	    XmTextSetString (toposcale, retstr);
	}
	break;
*/
      case VINT:
	if (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
	    (void)getivar_ ("incx", &ival, &error, 4);
	    if (error) break;
	    XmScaleSetValue(Properties.vintx, ival);

	    (void)getivar_ ("incy", &ival, &error, 4);
	    if (error) break;
	    XmScaleSetValue(Properties.vinty,ival);
	}
	break;
      case STREAMLINE:
	if (Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
	    (void)getlvar_ ("drstrmln", &lvar, &error, 8);
	    if (lvar) {
		XtVaSetValues(Properties.vect_strv,XmNset,FALSE,NULL);
		XtVaSetValues(Properties.vect_str,XmNset,TRUE,NULL);
	    }
	    else{
		XtVaSetValues(Properties.vect_str,XmNset,FALSE,NULL);
		XtVaSetValues(Properties.vect_strv,XmNset,TRUE,NULL);
	    }
	}
	break;
      case VECT_COMP:
	if(Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
	    (void)getavar_ ("vcomp_1", avar, &error, 7, 256);
	    if(!error)
	      XtVaSetValues(Properties.vcomp_1, XmNvalue, avar, NULL);
	    
	    (void)getavar_ ("vcomp_2", avar, &error, 7, 256);
	    if(!error)
	      XtVaSetValues(Properties.vcomp_2, XmNvalue, avar, NULL);
	}
	break;
      case VLOCK_UI:
	if(Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
	    (void)getlvar_ ("veclock", &lvar, &error, 7);
	    if(!error) {
		if (lvar) str = XmStringCreateLtoR("Yes",
						   XmSTRING_DEFAULT_CHARSET);
	        else str = XmStringCreateLtoR("No ",
					      XmSTRING_DEFAULT_CHARSET);
		XtVaSetValues(Properties.vect_lock, XmNlabelString, str, NULL);
		XmStringFree(str);
	    }
	}
	break;
    case VPOS:
	if(Properties.vect_pos && XtIsManaged(Properties.dep_form_v)) {
	    (void)getivar_("vecpos", &ivar, &error, 6);
	    if (!error) {
		if (ivar < 0) str = XmStringCreateLtoR("Head  ",
					       XmSTRING_DEFAULT_CHARSET);
		else if (ivar == 0) str = XmStringCreateLtoR("Center",
					       XmSTRING_DEFAULT_CHARSET);
		else str = XmStringCreateLtoR("Tail  ",
					       XmSTRING_DEFAULT_CHARSET);
		XtVaSetValues(Properties.vect_pos, XmNlabelString, str, NULL);
		XmStringFree(str);
	    }
	}
	break;
      case VSCALE_UI:
	if(Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
	    (void)getlvar_ ("scale_vector", &lvar, &error, 12);
	    if(!error) {
		if (lvar) str = XmStringCreateLtoR("Yes",
						   XmSTRING_DEFAULT_CHARSET);
	        else str = XmStringCreateLtoR("No ",
					      XmSTRING_DEFAULT_CHARSET);
		XtVaSetValues(Properties.vect_scale, XmNlabelString, str,
			      NULL);
		XmStringFree(str);
	    }
	}
	break;
      case VMAX_UI:
	if(Properties.dep_form_v&&XtIsManaged(Properties.dep_form_v)){
	    (void)getrvar_("vmax", &rvar, &error, 4);
	    if(!error){
		strcpy(avar, float_nozero(rvar));
		XtVaSetValues(Properties.vect_max, XmNvalue, avar, NULL);
	    }
	}
	break;
      case SKEWT_COMP:
	if(Properties.dep_form_s&&XtIsManaged(Properties.dep_form_s)){
	    (void)getavar_ ("scomp_1", avar, &error, 7, 256);
	    if(!error)
	      XtVaSetValues(Properties.scomp_1, XmNvalue, avar, NULL);
	    
	    (void)getavar_ ("scomp_2", avar, &error, 7, 256);
	    if(!error)
	      XtVaSetValues(Properties.scomp_2, XmNvalue, avar, NULL);
	    
	    (void)getavar_ ("scomp_3", avar, &error, 7, 256);
	    if(!error)
	      XtVaSetValues(Properties.scomp_3, XmNvalue, avar, NULL);
	}
	break;
      case LOC:
      case LOOP:
	(void)getiarr_ ("lock", fixed_index, &four, &error, 4);
	(void)getrarr_ ("plmin_scaled", min, &four, &error, 12);
	(void)getrarr_ ("plmax_scaled", max, &four, &error, 12);
	if (loop_setup_form) {
	    for (i=0; i < 4; ++i) {
		if (min[i] != max[i]) 
		    XtManageChild(loop_setup.button[i]);
		else {
		    if (i+1==loop_setup.coord) {
			loop_setup.coord = 0;
			XtVaSetValues(loop_setup.button[i],
				      XmNset, FALSE, NULL);
			XtUnmanageChild(loop_setup.start_widget);
			XtUnmanageChild(loop_setup.endv_widget);
		    }
		    XtUnmanageChild(loop_setup.button[i]);
		}
	    }
	}
	if (*window == LOOP) break;
	(void)getdarr_ ("domain_slope", slope, &four, &error, 12);
	(void)getdarr_ ("domain_intercept", intercept, &four, &error, 16);
	str = NewString("Fixed");
	str2 = NewString("Free ");
        str3 =   NewString("Avg  ");
	if(slicer_widget.info_2d){
	    if(fixed_index[0] == 1){
		(void)getrvar_ ("xloc", &rvar, &error, 4);
		if(slicer_widget.maxs[0] && XtIsManaged(slicer_widget.maxs[0]))
		    XtUnmanageChild(slicer_widget.maxs[0]);
		if(slicer_widget.mins[0]) {
		    (void)scale_(&rvar, &one, slope, intercept, &zero);
		    ival = (int)rvar;
		    XmScaleSetValue(slicer_widget.mins[0],ival);
		}
		fixed_index[0] = 1;
		XtSetArg (args[0], XmNlabelString, str);
		if(slicer_widget.fix[0])
		    XtSetValues (slicer_widget.fix[0], args, 1);
	    }
	    else{
		if(!fixed_index[0])
		    XtSetArg (args[0], XmNlabelString, str2);
		else
		    XtSetArg (args[0], XmNlabelString, str3);
		if(slicer_widget.fix[0]){
		    XtSetValues (slicer_widget.fix[0], args, 1);
		    if(!XtIsManaged(slicer_widget.maxs[0]))
			XtManageChild(slicer_widget.maxs[0]);
		}
	    }
	    if(fixed_index[1] == 1){
		(void)getrvar_ ("yloc", &rvar, &error, 4);
		if(slicer_widget.maxs[1] && XtIsManaged(slicer_widget.maxs[1]))
		    XtUnmanageChild(slicer_widget.maxs[1]);
		if(slicer_widget.mins[1]) {
		    (void)scale_(&rvar, &one, slope+1, intercept+1, &zero);
		    ival = (int)rvar;
		    XmScaleSetValue(slicer_widget.mins[1],ival);
		}
		fixed_index[1] = 1;
		XtSetArg (args[0], XmNlabelString, str);
		if(slicer_widget.fix[1])
		    XtSetValues (slicer_widget.fix[1], args, 1);
	    }
	    else{
		if(!fixed_index[1])
		    XtSetArg (args[0], XmNlabelString, str2);
		else
		    XtSetArg (args[0], XmNlabelString, str3);
		if(slicer_widget.fix[1])
		    XtSetValues (slicer_widget.fix[1], args, 1);
		if(slicer_widget.maxs[1] && !XtIsManaged(slicer_widget.maxs[1]))
		    XtManageChild(slicer_widget.maxs[1]);
	    }
	    if(fixed_index[2] == 1){
		int surface;

		(void)getlvar_ ("surface", &surface, &error, 7);
		if(slicer_widget.maxs[2] && XtIsManaged(slicer_widget.maxs[2]))
		    XtUnmanageChild(slicer_widget.maxs[2]);
		if(slicer_widget.mins[2]) {
		    if (surface) {
			(void) getrarr_("plmin_scaled", min, &four, &error,
					12, 4);
			rvar = min[2];
		    }
		    else {
			(void)getrvar_ ("zloc", &rvar, &error, 4);
			(void)scale_(&rvar, &one, slope+2, intercept+2, &zero);
		    }
		    ival = (int)rvar;
		    XmScaleSetValue(slicer_widget.mins[2],ival);
		}
		fixed_index[2] = 1;
		XtSetArg (args[0], XmNlabelString, str);
		if(slicer_widget.fix[2])
		    XtSetValues (slicer_widget.fix[2], args, 1);
	    }
	    else{
		if(!fixed_index[2])
		    XtSetArg (args[0], XmNlabelString, str2);
		else
		    XtSetArg (args[0], XmNlabelString, str3);
		if(slicer_widget.fix[2])
		    XtSetValues (slicer_widget.fix[2], args, 1);
		if(slicer_widget.maxs[2] && !XtIsManaged(slicer_widget.maxs[2]))
		    XtManageChild(slicer_widget.maxs[2]);
	    }
	    if(fixed_index[3] == 1){
		(void)getrvar_ ("tloc", &rvar, &error, 4);
		if(slicer_widget.maxs[3] && XtIsManaged(slicer_widget.maxs[3]))
		    XtUnmanageChild(slicer_widget.maxs[3]);
		if(slicer_widget.mins[3]) {
		    (void)scale_(&rvar, &one, slope+3, intercept+3, &zero);
		    ival = (int)rvar;
		    XmScaleSetValue(slicer_widget.mins[3],ival);
		}
		fixed_index[3] = 1;
		XtSetArg (args[0], XmNlabelString, str);
		if(slicer_widget.fix[3])
		    XtSetValues (slicer_widget.fix[3], args, 1);
	    }
	    else{
		if(!fixed_index[3])
		    XtSetArg (args[0], XmNlabelString, str2);
		else
		    XtSetArg (args[0], XmNlabelString, str3);
		if(slicer_widget.fix[3])
		    XtSetValues (slicer_widget.fix[3], args, 1);
		if(slicer_widget.maxs[3] && !XtIsManaged(slicer_widget.maxs[3]))
		    XtManageChild(slicer_widget.maxs[3]);
	    }
	
	    i = WINDOW;
	    ui_update_(&i);
	}
	else{
	    i = POINTS_UI;
	    ui_update_(&i);
	}
	i = VINT;
	ui_update_(&i);
	break;
      case COLORTAB:
	if(Properties.color_form){
	    int lock_entry;
	    float  interl;
	    XmString str;
	    char fieldc[80], button[80];
	    
	    (void)getivar_("lock_entry", &ivar, &error, 10);
	    (void)getavar_("lock_field", fieldc, &error, 10, 80);
	    (void)getrvar_("lock_int",  &interl, &error, 8);

	    if(get_button_name(fieldc,button)){
		str = NewString("Free");
		if(Properties.lock_color_widget)
		  XtVaSetValues(Properties.lock_color_widget,XmNlabelString,
				str,NULL);
		XmStringFree(str);
	    }
	    else{
	      if(interl!=0){
		sprintf(avar,"%s Locked",button);
		str = NewString(avar);
	      }
	      else{
		str = NewString("Free");
	      }
	      XtVaSetValues(Properties.lock_color_widget,XmNlabelString,
			    str,NULL);
	      XmStringFree(str);
	    }  
	}
	break;
      case PLOTTYPE:
	(void)getavar_ ("plotyp2", avar, &error, 7, 256);
	if(!error){
	  if(!strcasecmp("scalor",avar)){
	    XtVaSetValues(main_widget.type_menu,
			  XmNmenuHistory, main_widget.Scalar,
			  NULL);
	  }
	  else if(!strcasecmp("vector",avar)){
	    XtVaSetValues(main_widget.type_menu,
			  XmNmenuHistory, main_widget.Vector,
			  NULL);
	  }
	  /*	  else if(!strcasecmp("skewt",avar)){
		  XtVaSetValues(main_widget.type_menu,
		  XmNmenuHistory, main_widget.Skewt,
		  NULL);
	  }*/
	  else if(!strcasecmp("trajectory",avar)){
	    XtVaSetValues(main_widget.type_menu,
			  XmNmenuHistory, main_widget.Trajectory,
			  NULL);
	  }
	}
	break;
      case MAPROJ_UI:
	(void)getavar_ ("proj", avar, &error, 4, 256);
	if(!error && map_forms.map_form){
	    if(!strcasecmp("nhst",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_nhst,
			      NULL);
	    }
	    else if(!strcasecmp("shst",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_shst,
			      NULL);
	    }
	    
	    else if(!strcasecmp("nhor",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_nhor,
			      NULL);
	    }
	    
	    else if(!strcasecmp("shor",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_shor,
			      NULL);
	    }
	    
	    else if(!strcasecmp("me",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_me,
			      NULL);
	    }
	    
	    else if(!strcasecmp("ce",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_ce,
			      NULL);
	    }
	    
	    else if(!strcasecmp("mo",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_mo,
			      NULL);
	    }
	    
	    else if(!strcasecmp("lc",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_lc,
			      NULL);
	    }
	    
	    else if(!strcasecmp("st",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_st,
			      NULL);
	    }
	    
	    else if(!strcasecmp("or",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_or,
			      NULL);
	    }
	    
	    else if(!strcasecmp("le",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_le,
			      NULL);
	    }
	    
	    else if(!strcasecmp("gn",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_gn,
			      NULL);
	    }
	    
	    else if(!strcasecmp("ae",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_ae,
			      NULL);
	    }
	    
	    else if(!strcasecmp("sv",avar)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_sv,
			      NULL);
	    }
	}
	break;
    case FILLPATTERN:
	ivar=2;
	error=0;
	(void)getrarr_("pat1", pat, &ivar, &error, 4);
	if(!error){
	    if(pat[0] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[0]);
	    XmTextFieldSetString(stipple.start1,avar);
	    if(pat[1] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[1]);
	    XmTextFieldSetString(stipple.end1,avar);
	}
	(void)getrarr_("pat2", pat, &ivar, &error, 4);
	if(!error){
	    if(pat[0] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[0]);
	    XmTextFieldSetString(stipple.start2,avar);
	    if(pat[1] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[1]);
	    XmTextFieldSetString(stipple.end2,avar);
	}
	(void)getrarr_("pat3", pat, &ivar, &error, 4);
	if(!error){
	    if(pat[0] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[0]);
	    XmTextFieldSetString(stipple.start3,avar);
	    if(pat[1] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[1]);
	    XmTextFieldSetString(stipple.end3,avar);
	}
	(void)getrarr_("pat4", pat, &ivar, &error, 4);
	if(!error){
	    if(pat[0] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[0]);
	    XmTextFieldSetString(stipple.start4,avar);
	    if(pat[1] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[1]);
	    XmTextFieldSetString(stipple.end4,avar);
	}
	(void)getrarr_("pat5", pat, &ivar, &error, 4);
	if(!error){
	    if(pat[0] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[0]);
	    XmTextFieldSetString(stipple.start5,avar);
	    if(pat[1] > 1E+37)
		sprintf(avar,"default");
	    else
		sprintf(avar,"%g",pat[1]);
	    XmTextFieldSetString(stipple.end5,avar);
	}
	break;
    case LANDSCAPE_UI:
	(void)getivar_("landscape", &i, &error, 9);
	(void)set_landscape_widgets(i);
	break;
    case UI_TRAJ_COMP:
      if(Properties.dep_form_t&&XtIsManaged(Properties.dep_form_t)){
	(void)getavar_ ("trajcomp_1", avar, &error, 10, 256);
	if(!error)
	  XtVaSetValues(Properties.tcomp_1, XmNvalue, avar, NULL);
	(void)getavar_ ("trajcomp_2", avar, &error, 10, 256);
	if(!error)
	  XtVaSetValues(Properties.tcomp_2, XmNvalue, avar, NULL);
	(void)getavar_ ("trajcomp_3", avar, &error, 10, 256);
	if(!error)
	  XtVaSetValues(Properties.tcomp_3, XmNvalue, avar, NULL);
	
      }
      break;
    case UI_TRAJ_ENDS:
      if(Properties.dep_form_t&&XtIsSensitive(Properties.dep_form_t)&&
	 Properties.dep_form_t&&XtIsSensitive(Properties.traj_ts)){
	float time1,time2;

	(void)getrarr_ ("plmin_scaled", min, &four, &error, 12,4);
	(void)getrarr_ ("plmax_scaled", max, &four, &error, 12,4);	

	(void)getrvar_("trajbeg", &time1, &error, 7);
	if(time1 < min[3])time1 = min[3];
	if(time1 > max[3])time1 = max[3];
	(void)getrvar_("trajend", &time2, &error, 7);
	if(time2 < min[3])time2 = min[3];
	if(time2 > max[3])time2 = max[3];
      
	if(time1==time2 && time2==0) {
	  time2=max[3];
	  time1=min[3];
	}
	
	XtVaSetValues(Properties.traj_ts,XmNmaximum, (int)max[3],
		      XmNminimum, (int)min[3],
		      XmNvalue,(int)time1,NULL);
	
	XtVaSetValues(Properties.traj_te,XmNmaximum, (int)max[3],
		      XmNminimum, (int)min[3],
		      XmNvalue,(int)time2,NULL);
      }
      break;  
    case TRAJ_IND_UI: 
      if(Properties.dep_form_t){
	int traj_ind;
	(void)getlvar_("time_ind", &traj_ind, &error,8);
	if(traj_ind && !error){
	  XtSetSensitive(Properties.traj_ts, FALSE);
	  XtSetSensitive(Properties.traj_te, FALSE);
	}
	else{
	  XtSetSensitive(Properties.traj_ts, TRUE);
	  XtSetSensitive(Properties.traj_te, TRUE);
	}
      }
      break;
    case TRAJ_2D_UI: 
      if(Properties.dep_form_t){
	int traj_2d;
	(void)getlvar_("traj_2d", &traj_2d, &error,7);
	if(traj_2d && !error){
	  XtSetSensitive(Properties.tcomp_3, FALSE);
	  XtSetSensitive(Properties.tcomp_3, FALSE);
	}
	else{
	  XtSetSensitive(Properties.tcomp_3, TRUE);
	  XtSetSensitive(Properties.tcomp_3, TRUE);
	}
      }
      break;
    case TRAJ_STEP_VAL_UI: 
      if(Properties.dep_form_t){
	int traj_step_set, traj_val_set;
	float traj_step, traj_val;
	(void)getlvar_("traj_step_set", &traj_step_set, &error,13);
	if(traj_step_set && !error){
	  (void)getrvar_("traj_step", &traj_step, &error,9);
	  if(!error){
	    if(!error){
	      strcpy(avar, float_nozero(traj_step));
	      XtVaSetValues(Properties.traj_step, XmNvalue, avar, NULL);
	    }
	  }
	    /*Set the value*/
	}
      
	(void)getlvar_("traj_val_set", &traj_val_set, &error,12);
	if(traj_val_set && !error){
	  (void)getrvar_("traj_val", &traj_val, &error,8);
	  if(!error){
	      strcpy(avar, float_nozero(traj_val));
	      XtVaSetValues(Properties.traj_val, XmNvalue, avar, NULL);
	  }
	}
      }
      break;
    }/*end switch(*window)*/
}

#define bill_width 25
#define bill_height 29
#define bill_x_hot 12
#define bill_y_hot 22

static unsigned char bill_bits[] = {
   0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x00, 0x00, 0x09, 0x06, 0x00,
   0x02, 0x12, 0x01, 0x00, 0x06, 0xaa, 0x04, 0x00, 0x0a, 0x92, 0x22, 0x00,
   0x34, 0x97, 0x3a, 0x00, 0xcc, 0xc8, 0x17, 0x00, 0x58, 0x50, 0x14, 0x00,
   0x70, 0x22, 0xc9, 0x01, 0x2e, 0x22, 0x29, 0x00, 0x61, 0x30, 0x68, 0x00,
   0xb4, 0xcf, 0x9f, 0x01, 0x1a, 0x30, 0x08, 0x00, 0x90, 0x50, 0x08, 0x00,
   0x10, 0x14, 0x09, 0x00, 0x20, 0x11, 0x08, 0x00, 0x58, 0x78, 0x0c, 0x00,
   0x84, 0xff, 0x33, 0x00, 0x10, 0xfe, 0x04, 0x00, 0x40, 0xd6, 0x04, 0x00,
   0x00, 0x92, 0x08, 0x00, 0x00, 0x92, 0x00, 0x00, 0x00, 0x92, 0x00, 0x00,
   0x00, 0x92, 0x00, 0x00, 0x00, 0x46, 0x01, 0x00, 0x00, 0x39, 0x01, 0x00,
   0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01, 0x00};

static unsigned char bill_mask_bits[] = {
   0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x00, 0x00, 0x09, 0x06, 0x00,
   0x02, 0x12, 0x01, 0x00, 0x06, 0xaa, 0x04, 0x00, 0x0e, 0x92, 0x22, 0x00,
   0x3c, 0x97, 0x3a, 0x00, 0xfc, 0xcf, 0x1f, 0x00, 0xf8, 0xdf, 0x1f, 0x00,
   0xf0, 0xff, 0xcf, 0x01, 0xee, 0xff, 0x2f, 0x00, 0xe1, 0xff, 0x6f, 0x00,
   0xf4, 0xff, 0x9f, 0x01, 0xfa, 0xff, 0x0f, 0x00, 0xf0, 0xff, 0x0f, 0x00,
   0xf0, 0xff, 0x0f, 0x00, 0xe0, 0xff, 0x0f, 0x00, 0xd8, 0xff, 0x0f, 0x00,
   0x84, 0xff, 0x33, 0x00, 0x10, 0xfe, 0x04, 0x00, 0x40, 0xfe, 0x04, 0x00,
   0x00, 0xfe, 0x08, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00,
   0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x01, 0x00, 0x00, 0xff, 0x01, 0x00,
   0x00, 0xff, 0x01, 0x00, 0x00, 0xff, 0x01, 0x00};

#include <X11/cursorfont.h>
Cursor bill_the_cat;
static int frozen = 0;
static XtAppContext app;
void init_cursors()
{
    Pixmap bill, bill_mask;
    XColor bg, fg, dummy;
    extern Widget Box;

    XLookupColor(XtDisplay(Box), 
		 DefaultColormap(XtDisplay(Box), 
				 DefaultScreen(XtDisplay(Box))),
		 "black", &dummy, &fg);
    
    XLookupColor(XtDisplay(Box), 
		 DefaultColormap(XtDisplay(Box), 
				 DefaultScreen(XtDisplay(Box))),
		     "orange", &dummy, &bg);
					   
    bill = XCreateBitmapFromData(XtDisplay(Box),XtWindow(Box),
				 (char *)bill_bits,  bill_width, 
				 bill_height);

	
    bill_mask = XCreateBitmapFromData(XtDisplay(Box),XtWindow(Box),
				      (char *)bill_mask_bits,  bill_width, 
				      bill_height);
    
    bill_the_cat = XCreatePixmapCursor(XtDisplay(Box), bill, bill_mask, 
				       &fg, &bg,  bill_x_hot, 
				       bill_y_hot);
    XFreePixmap(XtDisplay(Box), bill);
    XFreePixmap(XtDisplay(Box), bill_mask);

    app=XtWidgetToApplicationContext(Box);
}
    
void freeze_dpy_(){
    extern Widget Box, xgks_widget ;
    static int flag=0 ;

    if(frozen)return;
    XDefineCursor(XtDisplay(Box),XtWindow(Box),bill_the_cat);
    XDefineCursor(XtDisplay(Box),XtWindow(xgks_widget),bill_the_cat);
    if(slicer_widget.info_2d != NULL)
      XDefineCursor(XtDisplay(Box),XtWindow(slicer_widget.info_2d),
		    bill_the_cat);
/*    if(Properties.form != NULL && XtIsManaged(Properties.form))
      XDefineCursor(XtDisplay(Box),XtWindow(Properties.form),
		    bill_the_cat);*/
    flag=0;
    XSync(XtDisplay(Box), FALSE);

    frozen = 1;
}

void melt_dpy_(){
     extern Widget Box, xgks_widget ;

     XUndefineCursor(XtDisplay(Box),XtWindow(Box));
     XUndefineCursor(XtDisplay(Box),XtWindow(xgks_widget));
     if(slicer_widget.info_2d != NULL)
       XUndefineCursor(XtDisplay(Box),XtWindow(slicer_widget.info_2d));
/*     if(Properties.form != NULL && XtIsManaged(Properties.form))
       XUndefineCursor(XtDisplay(Box),XtWindow(Properties.form));*/
     frozen = 0;
     
}







