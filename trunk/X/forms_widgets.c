static char ident[] = "$Id: forms_widgets.c,v 1.64 2002/11/27 00:30:51 warren Exp $";

/****************************************************************************
 * This file contains the Contour, Line, Vector, Color (Slide, Toggle & Lock),
 * and Printer Setup widgets.  Routines in this file are:
 *
 *	do_contour, do_line, do_vector, do_skewt, do_props, do_pros_,
 *      init_color,
 *	get_printers, init_printersetup.
 *
 * $Log: forms_widgets.c,v $
 * Revision 1.64  2002/11/27 00:30:51  warren
 * 24 bit color enhancements are here. Works on Linux and Sun so far - dec to
 * be tested still.
 *
 * Revision 1.63  2002/08/09 19:57:26  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.62  2002/06/14 19:43:50  harryive
 * Allow specification of terrain contours by value.
 *
 * Revision 1.61  2002/05/22 17:56:16  davidive
 * First working version of trajectories colored by alternative fields
 * There is not a complete interface yet!!!
 *
 * Revision 1.60  2001/11/17 00:16:48  warren
 * It's time for a new official add_credits (new rev) and included traj.inc
 * and color.inc in the new reset function.
 *
 * Revision 1.59  2001/11/15 21:44:46  davidive
 * Forgot to turn on the XmNactivateCallback for the traj_step box with my
 * first checked in version
 *
 * Revision 1.57  2001/08/15 22:23:21  davidive
 * Cleaned up code to reduce warnings about type mismatches, and added buttons
 * and callbacks to the 1d form widget to select which access to label against
 * for diagonal plots.
 *
 * Revision 1.56  2001/03/14 20:21:10  warren
 * corrected severa longs to unsigned longs, changed creation of lock button to
 * not set the name, the set the label seperately, change XtFree to XmStringFree
 * where it should be.
 *
 * Revision 1.55  2001/03/13 21:35:41  davidive
 * Modified to update the free locked button on the color slide widget when the
 * locked_field is ' '
 *
 * Revision 1.54  2000/08/17 22:37:47  warren
 * Made X minor tics set x tix instead of y tics on vector form.
 *
 * Revision 1.53  2000/04/05 22:38:46  harry
 * Correct bugs in general surface plotting.
 *
 * Revision 1.52  2000/03/03 23:10:34  warren
 * Pop up color widget when filled plot is made and up rev
 *
 * Revision 1.51  2000/03/03 17:05:03  harry
 * Remove extra XmStringFree that causes Alpha version to crash.
 *
 * Revision 1.50  1999/02/10 23:13:28  harry
 * Get rid of unused command vector_scale.
 *
 * Revision 1.49  1999/02/04 00:28:06  harry
 * Add vector scaling.
 *
 * Revision 1.48  1999/01/29 20:45:23  warren
 * new stuff to deal with the trajectory form
 *
 * Revision 1.47  1998/12/17 23:16:59  warren
 * Cleanup of pointer casts to long instead of int, freeing XmStrings when no
 * longer in use, passing all args to sprinf's and making correctly casting NULLs
 *
 * Revision 1.46  1998/09/04 20:50:45  harry
 * Fix minor type problem.
 *
 * Revision 1.45  1998/09/03 18:35:56  warren
 * Allow File as a printer choice from the selection box.
 *
 * Revision 1.44  1997/10/22 19:34:35  davidive
 * Turned the particle movement part of trajectories into a user transform
 * and added unit checking to the default transform.
 *
 * Revision 1.43  1997/06/27 21:06:26  harry
 * Designed multiplot printing.
 *
 * Revision 1.42  1997/06/12 15:28:07  davidive
 * Fix multidimentional 1 D plots to pay attention to min, max and tic mark
 * settings, and remember them.
 *
 * Revision 1.41  1997/04/11 18:46:40  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.40  1997/04/09 22:37:53  warren
 * change done back to cancel and Ok on the printer for to Set Printer.
 * Also got rid of Done button on the Printer Setup widget.
 *
 * Revision 1.39  1997/04/08 00:29:48  harry
 * Can't raise a window that has not been mapped.
 *
 * Revision 1.38  1997/04/04 21:50:19  harry
 * Add XRaiseWindow for FILE and PROPERTIES window.  Add wording to FILE window.
 *
 * Revision 1.37  1997/02/21 20:01:30  harry
 * Fix vector interval to be floating point and have values less than one.
 *
 * Revision 1.36  1997/02/20 23:44:42  harry
 * Do not put in "lp" in list if /etc/printcap does not exist.
 *
 * Revision 1.35  1997/02/20 22:03:40  harry
 * Fix two bugs found on SGI: Missing NULL at end of XtVaSetValues, and
 * no return in get_printers when /etc/princap open fails.
 *
 * Revision 1.34  1997/02/07 00:07:18  warren
 * Fixed lack of typin bug for sliders and printer widget not remembering
 * to do color.
 *
 * Revision 1.33  1996/12/23 20:57:57  warren
 * Landscape printing option
 *
 * Revision 1.32  1996/11/07 16:53:56  warren
 * First set of activated point slicing and widgets to go with it. Only 1d plots
 * are known to be correct. Use at your own risk for 2d. There is still a known
 * bug in the connection between the tic mark entry boxes and the plot when doing
 * point or mouse slicing.
 *
 * Revision 1.31  1996/10/11 01:12:05  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 * Revision 1.30  1996/04/25 16:24:19  harry
 * Add Skew-T plots and the beginning of trajectory plots.
 *
 * Revision 1.29  1996/03/29 17:46:54  harry
 * Use widgets instead of gadgets per suggestions in Motif Programming Manual.
 *
 * Revision 1.28  1995/10/19  22:13:12  harry
 * Set defaults by clicking right mouse button.  Make all TextWidgets into
 * TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.27  1995/07/10  17:16:07  warren
 * Finished renaming widgets so the help key will work. Also fixed the color
 * menus not to contain an _ in the boxes and fixed the second vector
 * componant update proble.
 *
 * Revision 1.26  1994/12/20  22:26:47  warren
 * made the rest of the scale widgets typing capable
 *
 * Revision 1.25  1994/11/22  22:46:39  harry
 * Correct vector locking, add support for vector positioning, add
 * support for filled contour overlays.
 *
 * Revision 1.24  1994/09/20  17:10:56  warren
 * all lock of coor to value: lock=contour val, color table entry.
 * Also fixe widget to update correctly
 *
 * Revision 1.23  1994/09/14  18:19:15  harry
 * Big 1D flip uncommit.
 *
 * Revision 1.22  1994/06/20  16:35:34  harry
 * Change index to strchr, and rindex to strrchr.
 *
 * Revision 1.21  1994/05/24  21:12:18  harry
 * Upgrade comments in code.
 *
 * Revision 1.20  1994/05/18  17:53:11  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.19  1994/05/06  17:09:07  yuri
 * Main vector lock commit + small correction in X/ive_ui_update.c file
 *
 * Revision 1.18  1994/04/19  23:01:40  yuri
 * Big 1D flip commit.
 *
 * Revision 1.17  1994/03/15  19:38:42  warren
 * Changed Items to Printers.
 *
 * Revision 1.16  1994/03/08  19:58:22  warren
 * fix spelling of component and make input windows go away with selection
 *
 * Revision 1.15  1994/01/29  00:10:29  warren
 * Changed color to printer.
 *
 * Revision 1.14  1993/11/29  19:37:54  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.13  1993/11/03  23:33:00  warren
 * General cleanup for beginning of port to alpha.
 *
 * Revision 1.12  1993/08/17  18:33:52  warren
 * Beginning of printer forms.
 *
 * Revision 1.11  1993/07/29  00:30:02  warren
 * add button for vector_lock command
 *
 * Revision 1.10  1993/07/09  19:40:08  warren
 * Fixed annoying blank popup of property form when opening a new file.
 *
 * Revision 1.9  1993/07/07  23:02:15  warren
 * Update the dependant var when changing fields in 1d. Also use g floating point.
 *
 * Revision 1.8  1993/06/15  19:50:06  warren
 * Fixed second part of getting the wrong name for the dependand line var.
 *
 * Revision 1.7  1993/06/14  21:56:01  warren
 * Added more options for poping up forms
 *
 * Revision 1.6  1993/06/05  00:21:01  warren
 * Fixes to slicer and forms updating needlessly and wrong.
 *
 * Revision 1.5  1993/06/01  23:59:03  warren
 * Fixed Vector component callback and Vector Interval Callback.
 *
 * Revision 1.4  1993/06/01  21:20:10  warren
 * added missing vector callback info
 *
 * Revision 1.3  1993/05/20  17:26:27  warren
 * Additions and changes to make the new forms active.
 *
 * Revision 1.2  1993/04/26  19:16:22  warren
 * These are the files required to get the new forms. They don't do anything yet.
 *
*****************************************************************************/

#include <Xm/XmAll.h>
#include <X11/Intrinsic.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>
#include <ive_widgets.h>
#include <stdio.h>
#include <malloc.h>

extern void getrvar_(),getivar_(),getlvar_(),getavar_(),getiarr_(),
  setup_contour_form(),setup_line_form(),setup_skewt_form(),setup_traj_form(),
  setup_vector_form(), call_driver(),form_flip_call_l(),  form_flip_call_c(),
  numlines_scale_type_in(), vint_scale_type_in(),point_axis_call(),
  form_traj_step_call(), form_traj_val_call(), form_traj_background_call(),
  form_3val_call();
extern int get_button_name();

XmString NewString();

/***************************************************************************
  Widgets declared globally so that they can be set from the FORTRAN
  all other will be nonaccessable
***************************************************************************/

struct {
  /* dependant var */
  Widget form;
  Widget dep_form_c,ind_form_c;
  Widget dep_form_l, ind_form_l;
  Widget dep_form_v,ind_form_v;
  Widget dep_form_s,ind_form_s;
  Widget dep_form_t,ind_form_t;
  Widget dep_form_3d,ind_form_3d;
  Widget cont_int, cont_val, clab1, clab2, cont_row;       /*contour*/
  Widget cont_values;                                      /*contour*/
  Widget zero, zerolab;                                    /*contour*/
  Widget dmin, dmax, varlab, dminlab, dmaxlab;             /*line*/
  Widget dmajor, dminor, dmajorlab, dminorlab, dmarklab;   /*line*/
  Widget daxeslin, daxeslog,daxesrow,daxeslab;             /*line*/
  Widget Vect_c_lab, vcomp_1, vcomp_2;                     /*vector*/
  Widget vcomp_1_lab, vcomp_2_lab;                           /*vector*/
  Widget Tcomp_c_lab, tcomp_1_lab, tcomp_2_lab, tcomp_3_lab; /*trajectory*/
  Widget tcomp_1, tcomp_2, tcomp_3, Traj_c_lab, Wind_lab;    /*trajectory*/
  Widget traj_ts_lab, traj_ts, traj_te_lab, traj_te;         /*trajectory*/
  Widget traj_ind_lab, traj_ind;                             /*trajectory*/
  Widget traj_expl_lab, traj_help;                           /*trajectory*/
  Widget traj_2d;                                             /*trajectory*/
  Widget threed_value, threedlab, threed_row;                 /*3d*/
  Widget threed_radio, iso, scatter, wire, mark, translucence; /*3d*/
/* per plot */
  Widget vect_max, vect_lock, vect_maxlab, vect_locklab;     /*vector*/
  Widget vect_poslab, vect_pos;			             /*vector*/
  Widget vect_strlab, vect_strrow, vect_str, vect_strv;      /*vector*/
  Widget vect_scalelab, vect_scale;		             /*vector*/
  Widget fill, freq, filllab, numlines, linelab1, linelab2;  /*contour*/
  /* Independant vars */ 
  Widget xmajor,xminor,ymajor,yminor;                        /*contour*/
  Widget xmajorlab,xminorlab,ymajorlab, ivaxesrow;           /*contour*/
  Widget yminorlab, ihaxeslog, ivaxeslog, ihaxesrow;         /*contour*/
  Widget ihaxeslin, ivaxeslin, ihaxeslab, ivaxeslab;         /*contour*/
  Widget laxeslin, laxeslog, laxesrow, laxeslab;             /*line*/
  Widget imin, imax, iminlab, imaxlab,lvarlab;               /*line*/
  Widget imajor,iminor, imajorlab,iminorlab;                 /*line*/
  Widget direction_radio_box, radio_1, radio_2, radio_3;     /*line*/
  Widget skewt_c_lab, scomp_1, scomp_2, scomp_3;	     /*skewt*/
  Widget scomp_1_lab, scomp_2_lab, scomp_3_lab;              /*skewt*/
  Widget vintx, vinty, vintxlab, vintylab;                   /*vector*/
  Widget vintxlab2, vintylab2;                               /*vector*/
  Widget xmajorv,xminorv,ymajorv,yminorv;                    /*vect*/
  Widget xmajorlabv,xminorlabv,ymajorlabv, ivaxesrowv;       /*vect*/
  Widget yminorlabv, ihaxeslogv, ivaxeslogv, ihaxesrowv;     /*vect*/
  Widget ihaxeslinv, ivaxeslinv, ihaxeslabv, ivaxeslabv;     /*vect*/
  Widget vect_type, vect_type_menu, Free_Max; 
  Widget Vector_lockmax,Vector_lockscale;
  Widget xmajort,xminort,ymajort,yminort;                    /*traj*/
  Widget xmajorlabt,xminorlabt,ymajorlabt, yminorlabt;       /*traj*/
  Widget ihaxeslogt, ivaxeslogt;                             /*traj*/
  Widget ihaxeslabt, ivaxeslabt;                             /*traj*/
  Widget traj_steplab, traj_step, traj_vallab, traj_val;     /*traj*/
  Widget traj_backgroundlab, traj_background;                /*traj*/
  Widget color_form, lock_color_widget, color_left;          /*color*/
  Widget color_reset, color_bar, color_toggle, color_right;  /*color*/
  Widget which_coltab;
}Properties={
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
    NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};


XmString str;
static char vec_max_D[]="vector_max=D";
static char vector_lock_on[]="vector_lock=on";
static char vector_lock_off[]="vector_lock=off";
static char vector[]="type=vector";
Widget separator_c,separator_l;

void  do_contour(parent)
     Widget parent;
{
    int lock[4],four = 4,error;    
    
    
    /*make form but don't manage*/
    
    /*DEP FORM*/
    Properties.dep_form_c = XtVaCreateWidget("Properties",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_FORM,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizable,TRUE,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNmarginWidth,5,
					     XmNborderWidth,1,
					     NULL);

    /*row 1*/
    XtAddCallback(Properties.dep_form_c,XmNhelpCallback,
                  check_help_call,NULL);

    str = NewString("Contours");
    Properties.clab1 = 
      XtVaCreateManagedWidget("Contours",xmLabelWidgetClass,
			      Properties.dep_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,5,
			      NULL);
    XmStringFree(str);
    
    str = NewString("specified by");
    Properties.clab2 = 
      XtVaCreateManagedWidget("SpecBy",xmLabelWidgetClass,
			      Properties.dep_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.clab1,
			      XmNtopOffset, 5,
			      NULL);
    XmStringFree(str);

    Properties.cont_row = 
      XtVaCreateManagedWidget("Contour_interval", xmRowColumnWidgetClass,
			      Properties.dep_form_c,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.clab2,
			      XmNradioBehavior, True,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      XmNpacking,XmPACK_TIGHT,
			      NULL);

    str = NewString("Interval");
    Properties.cont_int =  
      XtVaCreateManagedWidget("Contour_Interval", 
			      xmToggleButtonWidgetClass,
			      Properties.cont_row,
			      XmNlabelString,str,
			      NULL);
    XmStringFree(str);

    str = NewString("Values");
    Properties.cont_val =  
      XtVaCreateManagedWidget("Contour_values", 
			      xmToggleButtonWidgetClass,
			      Properties.cont_row,
			      XmNlabelString,str,
			      NULL);
    XmStringFree(str);
    
    Properties.cont_values = 
      XtVaCreateManagedWidget("Contour_Values",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_c,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.cont_row,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      XmNrightAttachment, 
			      XmATTACH_FORM,
			      XmNleftOffset, 10,
			      XmNrightOffset, 10,
			      NULL);
    
    
    
    	
    XtAddCallback(Properties.cont_values,XmNactivateCallback,
		  form_cint_call,NULL);
    XtAddCallback(Properties.cont_int,XmNarmCallback,
		  form_cint_call,(XtPointer)1);
    XtAddCallback(Properties.cont_val,XmNarmCallback,
		  form_cint_call,(XtPointer)2);
    XtAddCallback(Properties.cont_values,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)CVALS);
    XtAddCallback(Properties.cont_values,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)CVALS);
    XtAddCallback(Properties.cont_values,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)CVALS);
    XtAddEventHandler(Properties.cont_values, ButtonPressMask, FALSE,
			 check_default_handler, 0);
    
    
    
    
    str = NewString("Label every");
    Properties.linelab1 = 
      XtVaCreateManagedWidget("Linelab1",xmLabelWidgetClass,
			      Properties.dep_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.cont_values,
			      XmNleftAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    str = NewString("line(s)");
    Properties.linelab2 = 
      XtVaCreateManagedWidget("Linelab2",xmLabelWidgetClass,
			      Properties.dep_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.cont_values,
			      XmNrightAttachment,XmATTACH_FORM,
			      XmNrightOffset, 10,
			      NULL);
    XmStringFree(str);
    

    str = NewString("");
     Properties.numlines = 
       XtVaCreateManagedWidget("NULBLL",xmScaleWidgetClass,
			       Properties.dep_form_c,
			       XmNtopAttachment,XmATTACH_WIDGET,
			       XmNtopWidget, Properties.cont_values,
			       XmNrightAttachment,XmATTACH_WIDGET,
			       XmNleftAttachment,XmATTACH_WIDGET,
			       XmNrightWidget, Properties.linelab2,
			       XmNleftWidget, Properties.linelab1,
			       XmNorientation,XmHORIZONTAL,
			       XmNtitleString,str,
			       XmNmaximum,6,
			       XmNminimum,0,
			       XmNshowValue, True,
			       XmNvalue,3,
			       NULL);
    
    XtAddCallback(Properties.numlines, XmNvalueChangedCallback, 
		  form_nunlablines_call,NULL);

    XtAddRawEventHandler(Properties.numlines, ButtonPressMask,FALSE,
			 numlines_scale_type_in,0);
    XtUnmanageChild(XtNameToWidget(Properties.numlines,"Title"));


    str = NewString("Fill Contours");
    Properties.filllab = XtVaCreateManagedWidget("FillTYPE",
						 xmLabelWidgetClass,
						 Properties.dep_form_c,
						 XmNtopAttachment, 
						 XmATTACH_WIDGET,
						 XmNtopWidget, 
						 Properties.numlines,
						 XmNleftAttachment, 
						 XmATTACH_FORM,
						 XmNtopOffset, 10,
						 XmNlabelString, str,
						 NULL);
    XmStringFree(str);
    
    str = NewString("Solid");
    Properties.fill =  
      XtVaCreateManagedWidget("FillType",
			      xmPushButtonWidgetClass,
			      Properties.dep_form_c,
			      XmNlabelString, str,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.numlines,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.filllab,
			      XmNtopOffset, 5,
			      NULL);

    XtAddCallback(Properties.fill, XmNactivateCallback,
		  form_filltype_call,NULL);
    XmStringFree(str);

    str = NewString("Never");
    Properties.freq =  
      XtVaCreateManagedWidget("FillFreq",
			      xmPushButtonWidgetClass,
			      Properties.dep_form_c,
			      XmNlabelString, str,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.numlines,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.fill,
			      XmNleftOffset, 5,
			      XmNtopOffset, 5,
			      NULL);
    
    XtAddCallback(Properties.freq, XmNactivateCallback,
		  form_filltype_call,NULL);
    XmStringFree(str);

    str = NewString("Plot Zero Contour");
    Properties.zerolab = XtVaCreateManagedWidget("Plot Zero Contour?",
						 xmLabelWidgetClass,
						 Properties.dep_form_c,
						 XmNtopAttachment, 
						 XmATTACH_WIDGET,
						 XmNtopWidget, 
						 Properties.filllab,
						 XmNleftAttachment, 
						 XmATTACH_FORM,
						 XmNtopOffset, 10,
						 XmNlabelString, str,
						 NULL);
    XmStringFree(str);
    
    str = NewString("Yes");
    Properties.zero =  
      XtVaCreateManagedWidget("NoZero",
			      xmPushButtonWidgetClass,
			      Properties.dep_form_c,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.filllab,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.zerolab,
			      XmNtopOffset, 5,
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);
	
    XtAddCallback(Properties.zero, XmNactivateCallback,
		  form_zero_call,NULL);

    
/********************END OF DEPENDANT VAR FORM********************/
/*****************AND END OF NONDEPENDANT VAR FORM****************/
	
    Properties.ind_form_c = XtVaCreateWidget("Properties",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNtopWidget,
					     Properties.dep_form_c,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizable, TRUE,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNborderWidth,1,
					     XmNmarginWidth,5,
					     NULL);
    
    XtAddCallback(Properties.ind_form_c, XmNhelpCallback,
                  check_help_call,NULL);
    /*row 1*/

    str = NewString("Horizontal Axis");
    Properties.ihaxeslab = 
      XtVaCreateManagedWidget("Abscissa lab",
			      xmLabelWidgetClass,
			      Properties.ind_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,
			      XmATTACH_FORM,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.ihaxesrow = 
      XtVaCreateManagedWidget("LINLOG", xmRowColumnWidgetClass,
			      Properties.ind_form_c,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.ihaxeslab,
			      XmNradioBehavior, True,
			      XmNpacking,XmPACK_TIGHT,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      NULL);

    str = NewString("Linear");
    Properties.ihaxeslin =  
      XtVaCreateManagedWidget("linlog ",
			      xmToggleButtonWidgetClass,
			      Properties.ihaxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);

    XtAddCallback(Properties.ihaxeslin,XmNarmCallback,form_linlog,(XtPointer)"h");
   
    str = NewString("Logarithmic");
    Properties.ihaxeslog =  
      XtVaCreateManagedWidget("LinLog ",
			      xmToggleButtonWidgetClass,
			      Properties.ihaxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.ihaxeslog,XmNarmCallback,form_linlog,(XtPointer)"h");
    

   /*row 2*/   
    
    str = NewString("Major tic\ninterval");
    Properties.xmajorlab = 
      XtVaCreateManagedWidget("Majlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ihaxesrow,
			      XmNtopOffset, 10,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);


    Properties.xmajor =     
      XtVaCreateManagedWidget("ABSCISSA",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_c,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ihaxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.xmajorlab,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.xmajor,XmNactivateCallback,form_tick_call,(XtPointer)"X");
    XtAddCallback(Properties.xmajor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)XMAJOR);
    XtAddCallback(Properties.xmajor,XmNmodifyVerifyCallback,check_num,(XtPointer)XMAJOR);
    XtAddCallback(Properties.xmajor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)XMAJOR);
    XtAddEventHandler(Properties.xmajor, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    Properties.xminor =     
      XtVaCreateManagedWidget("abscissa",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_c,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ihaxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.xmajor,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.xminor,XmNactivateCallback,form_tick_call,(XtPointer)"x");
    XtAddCallback(Properties.xminor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)XMINOR);
    XtAddCallback(Properties.xminor,XmNmodifyVerifyCallback,check_num,(XtPointer)XMINOR);
    XtAddCallback(Properties.xminor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)XMINOR);
    XtAddEventHandler(Properties.xminor, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    
    str = NewString("Minor tic\ninterval");
    Properties.xminorlab = 
      XtVaCreateManagedWidget("Minlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ihaxesrow,
			      XmNtopOffset, 10,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.xminor,
			      NULL);
    XmStringFree(str);

    

    /*row 3*/
    
    str = NewString("Vertical Axis ");
    Properties.ivaxeslab = 
      XtVaCreateManagedWidget("Vhaxes",
			      xmLabelWidgetClass,
			      Properties.ind_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.xmajor,
			      XmNtopOffset, 10,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.ivaxesrow = 
      XtVaCreateManagedWidget("LINLOG", xmRowColumnWidgetClass,
			      Properties.ind_form_c,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.xmajor,
			      XmNtopOffset, 5,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.ivaxeslab,
			      XmNradioBehavior, True,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      XmNpacking,XmPACK_TIGHT,
			      NULL);

    str = NewString("Linear");
    Properties.ivaxeslin =  
      XtVaCreateManagedWidget("linlog ",
			      xmToggleButtonWidgetClass,
			      Properties.ivaxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);

    XtAddCallback(Properties.ivaxeslin,XmNarmCallback,form_linlog,(XtPointer)"v");
   
    str = NewString("Logarithmic");
    Properties.ivaxeslog =  
      XtVaCreateManagedWidget("LinLog",
			      xmToggleButtonWidgetClass,
			      Properties.ivaxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);

    XtAddCallback(Properties.ivaxeslog,XmNarmCallback,form_linlog,(XtPointer)"v");




    str = NewString("Major tic\ninterval");
    Properties.ymajorlab = 
      XtVaCreateManagedWidget("Majlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ivaxesrow ,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

    Properties.ymajor =     
      XtVaCreateManagedWidget("ORDINATE",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_c,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ivaxesrow  ,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.ymajorlab,
			      XmNcolumns, 8,
			      XmNbottomOffset, 10,
			      NULL);

    XtAddCallback(Properties.ymajor,XmNactivateCallback,form_tick_call,(XtPointer)"Y");
    XtAddCallback(Properties.ymajor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)YMAJOR);
    XtAddCallback(Properties.ymajor,XmNmodifyVerifyCallback,check_num,(XtPointer)YMAJOR);
    XtAddCallback(Properties.ymajor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)YMAJOR);
    XtAddEventHandler(Properties.ymajor, ButtonPressMask, FALSE,
			 check_default_handler, 0);


    Properties.yminor =     
      XtVaCreateManagedWidget("ordinate",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_c,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ivaxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.ymajor,
			      XmNcolumns, 8,
			      XmNbottomOffset, 10,
			      NULL);

    XtAddCallback(Properties.yminor,XmNactivateCallback,form_tick_call,(XtPointer)"y");
    XtAddCallback(Properties.yminor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)YMINOR);
    XtAddCallback(Properties.yminor,XmNmodifyVerifyCallback,check_num,(XtPointer)YMINOR);
    XtAddCallback(Properties.yminor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)YMINOR);
    XtAddEventHandler(Properties.yminor, ButtonPressMask, FALSE,
			 check_default_handler, 0);


    str = NewString("Minor tic\ninterval");
    Properties.yminorlab = 
      XtVaCreateManagedWidget("Minlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_c,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.ivaxesrow  ,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.yminor,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

    

}


void  do_line(parent, fix)
     Widget parent;
     int fix[4];
{

    
    /*make form but don't manage*/
    
    /*DEP FORM*/
    Properties.dep_form_l = XtVaCreateWidget("Properties",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_FORM,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizable, TRUE,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNmarginWidth,5,
					     XmNborderWidth,1,
					     XmNwidth, 274,
					     NULL);
    
    /*row 1*/
   
    str = NewString("Dependant Variable XXX");
    Properties.varlab = 
      XtVaCreateManagedWidget("Varlab",xmLabelWidgetClass,
			      Properties.dep_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNrightAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    /*row 2*/

    str = NewString("Axis is");
    Properties.daxeslab = 
      XtVaCreateManagedWidget("Ihaxes",
			      xmLabelWidgetClass,
			      Properties.dep_form_l,

			      XmNlabelString,str,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget,  Properties.varlab,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.daxesrow = 
      XtVaCreateManagedWidget("DhaxesRow", xmRowColumnWidgetClass,
			      Properties.dep_form_l,
			      XmNtopAttachment,XmATTACH_WIDGET, 
			      XmNtopWidget, Properties.varlab,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.daxeslab,
			      XmNradioBehavior, True,
			      XmNpacking,XmPACK_TIGHT,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      NULL);

    str = NewString("Linear");
    Properties.daxeslin =  
      XtVaCreateManagedWidget("daxeslin ",
			      xmToggleButtonWidgetClass,
			      Properties.daxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);

   
    str = NewString("Logarithmic");
    Properties.daxeslog =  
      XtVaCreateManagedWidget("daxeslog ",
			      xmToggleButtonWidgetClass,
			      Properties.daxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);


    str = NewString("Lower    \nLimit    ");
    Properties.dminlab = 
      XtVaCreateManagedWidget("Minlab",xmLabelWidgetClass,
			      Properties.dep_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.daxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.dmin = 
      XtVaCreateManagedWidget("Dmin",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.daxesrow,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.dminlab,
			      XmNtopOffset, 5,
			      XmNcolumns, 8,
			      NULL);
    XtAddCallback(Properties.dmin,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)DMIN);
    XtAddCallback(Properties.dmin,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)DMIN);
    XtAddCallback(Properties.dmin,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)DMIN);
    XtAddEventHandler(Properties.dmin, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    Properties.dmax = 
      XtVaCreateManagedWidget("Dmax",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.daxesrow,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.dmin,
			      XmNtopOffset, 5,
			      XmNcolumns, 8,
			      XmNrightOffset, 10,
			      NULL);

    XtAddCallback(Properties.dmax,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)DMAX);
    XtAddCallback(Properties.dmax,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)DMAX);
    XtAddCallback(Properties.dmax,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)DMAX);
    XtAddEventHandler(Properties.dmax, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    str = NewString("Upper    \nLimit    ");
    Properties.dmaxlab = 
      XtVaCreateManagedWidget("Minlab",xmLabelWidgetClass,
			      Properties.dep_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.daxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.dmax,
			      NULL);
    XmStringFree(str);

    /*row 4*/


    str = NewString("Major tic\ninterval");
    Properties.dmajorlab = 
      XtVaCreateManagedWidget("dMajlab",
			      xmLabelWidgetClass,
			      Properties.dep_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.dmin,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.dmajor =     
      XtVaCreateManagedWidget("dMaj",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.dmin,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.dmajorlab,
			      XmNcolumns, 8,
			      NULL);


    XtAddCallback(Properties.dmajor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)DMAJOR);
    XtAddCallback(Properties.dmajor,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)DMAJOR);
    XtAddCallback(Properties.dmajor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)DMAJOR);
    XtAddEventHandler(Properties.dmajor, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    Properties.dminor =     
      XtVaCreateManagedWidget("dMin",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.dmin,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.dmajor,
			      XmNcolumns, 8,
			      XmNrightOffset, 10,
			      NULL);

    XtAddCallback(Properties.dminor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)DMINOR);
    XtAddCallback(Properties.dminor,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)DMINOR);
    XtAddCallback(Properties.dminor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)DMINOR);
    XtAddEventHandler(Properties.dminor, ButtonPressMask, FALSE,
			 check_default_handler, 0);


    str = NewString("Minor tic\ninterval");
    Properties.dminorlab = 
      XtVaCreateManagedWidget("Minlab",
			      xmLabelWidgetClass,
			      Properties.dep_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.dmin,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.dminor,
			      NULL);
    XmStringFree(str);
    
/* row 5*/


/********************END OF DEPENDANT VAR FORM********************/

    Properties.ind_form_l = XtVaCreateWidget("Properties",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNtopWidget,
					     Properties.dep_form_l,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNmarginWidth,5,
					     XmNborderWidth,1,
					     XmNwidth, 270,
					     NULL);
    
    XtAddCallback(Properties.ind_form_l, XmNhelpCallback,
                  check_help_call,NULL);
 /*row 1*/

    str = NewString("Indepenant Variable XXX");
    Properties.lvarlab =
      XtVaCreateManagedWidget("lVarlab",xmLabelWidgetClass,
			      Properties.ind_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_FORM,
			      /*XmNrightAttachment,XmATTACH_FORM,*/
			      XmNtopOffset,5,
			      NULL);
    XmStringFree(str);

    Properties.direction_radio_box = 
      XtVaCreateWidget("D_R_B",xmRowColumnWidgetClass,
		       Properties.ind_form_l,
		       XmNtopAttachment,XmATTACH_FORM,
		       XmNleftAttachment,XmATTACH_WIDGET,
		       XmNleftWidget,Properties.lvarlab,
		       XmNrightAttachment,XmATTACH_FORM,
		       XmNradioBehavior,True,
		       XmNorientation,XmHORIZONTAL,
		       XmNborderWidth,0,
		       XmNmarginHeight,0,
		       XmNpacking,XmPACK_TIGHT,
		       XmNmarginLeft,0,
		       NULL);


    Properties.radio_1 = 
      XtVaCreateManagedWidget("D_R_1",xmToggleButtonWidgetClass,
			      Properties.direction_radio_box,
			      XmNshowAsDefault, False,
			      XmNmarginLeft,0,
			      NULL);
    
    Properties.radio_2 = 
      XtVaCreateManagedWidget("D_R_2",xmToggleButtonWidgetClass,
			      Properties.direction_radio_box,
			      XmNshowAsDefault, False,
			      XmNmarginLeft,0,
			      NULL);

    str = NewString("Default");
    Properties.radio_3 = 
      XtVaCreateManagedWidget("D_R_3",xmToggleButtonWidgetClass,
			      Properties.direction_radio_box,
			      XmNshowAsDefault, True,
			      XmNlabelString,str,
			      XmNmarginLeft,0,
			      NULL);

    XtAddCallback(Properties.radio_1, XmNarmCallback,
                  point_axis_call,NULL);
    XtAddCallback(Properties.radio_2, XmNarmCallback,
		  point_axis_call,NULL);
    XtAddCallback(Properties.radio_3, XmNarmCallback,
                  point_axis_call,NULL);
    XmStringFree(str);
    
    /*row 2*/
    str = NewString("Axis is");
    Properties.laxeslab = 
      XtVaCreateManagedWidget("Laxeslab",
			      xmLabelWidgetClass,
			      Properties.ind_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.lvarlab,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.laxesrow = 
      XtVaCreateManagedWidget("DhaxesRow", xmRowColumnWidgetClass,
			      Properties.ind_form_l,
			      XmNtopAttachment,XmATTACH_WIDGET, 
			      XmNtopWidget, Properties.lvarlab,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.laxeslab,
			      XmNradioBehavior, True,
			      XmNpacking,XmPACK_TIGHT,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      NULL);

    str = NewString("Linear");
    Properties.laxeslin =  
      XtVaCreateManagedWidget("daxeslin ",
			      xmToggleButtonWidgetClass,
			      Properties.laxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);

   
    str = NewString("Logarithmic");
    Properties.laxeslog =  
      XtVaCreateManagedWidget("daxeslog ",
			      xmToggleButtonWidgetClass,
			      Properties.laxesrow, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);


    /*row 3*/

    str = NewString("Lower    \nLimit    ");
    Properties.iminlab = 
      XtVaCreateManagedWidget("iMinlab",xmLabelWidgetClass,
			      Properties.ind_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.laxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.imin = 
      XtVaCreateManagedWidget("Imin",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.laxesrow,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.iminlab,
			      XmNtopOffset, 5,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.imin,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)IMIN);
    XtAddCallback(Properties.imin,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)IMIN);
    XtAddCallback(Properties.imin,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)IMIN);
    XtAddEventHandler(Properties.imin, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    Properties.imax = 
      XtVaCreateManagedWidget("Imax",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.laxesrow,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.imin,
			      XmNtopOffset, 5,
			      XmNcolumns, 8,
			      XmNrightOffset, 10,
			      NULL);

    XtAddCallback(Properties.imax,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)IMAX);
    XtAddCallback(Properties.imax,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)IMAX);
    XtAddCallback(Properties.imax,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)IMAX);
    XtAddEventHandler(Properties.imax, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    str = NewString("Upper    \nLimit    ");
    Properties.imaxlab = 
      XtVaCreateManagedWidget("iMinlab",xmLabelWidgetClass,
			      Properties.ind_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.laxesrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.imax,
			      NULL);
    XmStringFree(str);

    /*row 4*/

    str = NewString("Major tic\ninterval");
    Properties.imajorlab = 
      XtVaCreateManagedWidget("iMajlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.imin,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      XmNtopOffset, 5,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

    Properties.imajor =     
      XtVaCreateManagedWidget("Maj",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.imin,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.imajorlab,
			      XmNcolumns, 8,
			      XmNtopOffset, 5,
			      XmNbottomOffset, 10,
			      NULL);

    XtAddCallback(Properties.imajor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)IMAJOR);
    XtAddCallback(Properties.imajor,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)IMAJOR);
    XtAddCallback(Properties.imajor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)IMAJOR);
    XtAddEventHandler(Properties.imajor, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    Properties.iminor =     
      XtVaCreateManagedWidget("Min",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_l,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.imin,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.imajor,
			      XmNcolumns, 8,
			      XmNtopOffset, 5,
			      XmNrightOffset, 10,
			      XmNbottomOffset, 10,
			      NULL);

    XtAddCallback(Properties.iminor,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)IMINOR);
    XtAddCallback(Properties.iminor,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)IMINOR);
    XtAddCallback(Properties.iminor,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)IMINOR);
    XtAddEventHandler(Properties.iminor, ButtonPressMask, FALSE,
			 check_default_handler, 0);

   str = NewString("Minor tic\ninterval");
    Properties.iminorlab = 
      XtVaCreateManagedWidget("Minlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_l,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.imin,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.iminor,
			      XmNtopOffset, 5,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

}
   

void  do_vector(parent)
     Widget parent;
{
  Pixel bot,top;
  
  int error = 0, veclock = 0;
  
  /*make form but don't manage*/
  
  /*DEP FORM*/
  Properties.dep_form_v = XtVaCreateWidget("Propertiesv",
					   xmFormWidgetClass,
					   parent,
					   XmNtopAttachment,
					   XmATTACH_FORM,
					   XmNleftAttachment,
					   XmATTACH_FORM,
					   XmNrightAttachment,
					   XmATTACH_FORM,
					   XmNresizable, TRUE,
					   XmNresizePolicy,
					   XmRESIZE_ANY,
					   XmNmarginWidth,5,
					   XmNborderWidth,1,
					   XmNwidth, 270,
					   NULL);
  
  
  /*row 1*/
  
  str = NewString("Vector");
  Properties.Vect_c_lab =
    XtVaCreateManagedWidget("Vect_c_lab",xmLabelWidgetClass,
			    Properties.dep_form_v,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_FORM,
			    XmNleftAttachment,XmATTACH_FORM,
			    XmNrightAttachment,XmATTACH_FORM,
			    NULL);
  XmStringFree(str);
  
  
  
  str = NewString("Display");
  Properties.vect_strlab = 
    XtVaCreateManagedWidget("Vect_strlab",xmLabelWidgetClass,
			    Properties.dep_form_v,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget, Properties.Vect_c_lab,
			    XmNleftAttachment,XmATTACH_FORM,
			    XmNtopOffset,5,
			    NULL);
  
  XmStringFree(str);

    Properties.vect_strrow = 
      XtVaCreateManagedWidget("Vect_strRow", xmRowColumnWidgetClass,
			      Properties.dep_form_v,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Vect_c_lab,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.vect_strlab,
			      XmNradioBehavior, True,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      XmNpacking,XmPACK_TIGHT,
			      NULL);

    str = NewString("Vectors");
    Properties.vect_strv =  
      XtVaCreateManagedWidget("Vect_Strv", 
			      xmToggleButtonWidgetClass,
			      Properties.vect_strrow,
			      XmNlabelString,str,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.vect_strv, XmNarmCallback, form_stream_call, (XtPointer)"V");

    str = NewString("Streamlines");
    Properties.vect_str =  
      XtVaCreateManagedWidget("Vect_Str", 
			      xmToggleButtonWidgetClass,
			      Properties.vect_strrow,
			      XmNlabelString,str,
			      NULL);

    XmStringFree(str);
    XtAddCallback(Properties.vect_str, XmNarmCallback, form_stream_call, (XtPointer)"S");

    str = NewString("Horizontal\ncomponent");
    Properties.vcomp_1_lab = 
      XtVaCreateManagedWidget("Vcomp_1_lab",xmLabelWidgetClass,
			      Properties.dep_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vect_strrow,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    Properties.vcomp_1 = 
      XtVaCreateManagedWidget("Vcomp_1",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_v,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vect_strrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.vcomp_1_lab,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.vcomp_1,XmNactivateCallback,
		  form_vcomp_call,(XtPointer)VCOMP_1);
    XtAddCallback(Properties.vcomp_1,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)VCOMP_1);
    XtAddCallback(Properties.vcomp_1,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)VCOMP_1);
    XtAddCallback(Properties.vcomp_1,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)VCOMP_1);
      

    Properties.vcomp_2 = 
      XtVaCreateManagedWidget("Vcomp_2",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_v,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vect_strrow,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.vcomp_1,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.vcomp_2,XmNactivateCallback,
		  form_vcomp_call,(XtPointer)VCOMP_2);
    XtAddCallback(Properties.vcomp_2,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)VCOMP_2);
    XtAddCallback(Properties.vcomp_2,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)VCOMP_2);
    XtAddCallback(Properties.vcomp_2,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)VCOMP_2);

    str = NewString("Vertical\ncomponent");
    Properties.vcomp_2_lab = 
      XtVaCreateManagedWidget("Vcomp_2_lab",xmLabelWidgetClass,
			      Properties.dep_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vect_strrow,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.vcomp_2,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    /*row 3*/

    str = NewString("Maximum\n value");
    Properties.vect_maxlab = 
      XtVaCreateManagedWidget("Vect_maxlab",xmLabelWidgetClass,
			      Properties.dep_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vcomp_1,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,5,
			      NULL);

    XmStringFree(str);

     Properties.vect_max =
      XtVaCreateManagedWidget("Vect_max",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_v,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      Properties.vcomp_1,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_WIDGET,
			      XmNleftWidget,
			      Properties.vect_maxlab,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.vect_max,XmNactivateCallback,
		  form_vect_max_call,NULL);
    XtAddCallback(Properties.vect_max,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)VMAX);
    XtAddCallback(Properties.vect_max,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)VMAX);
    XtAddCallback(Properties.vect_max,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)VMAX);
    XtAddEventHandler(Properties.vect_max, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    str = NewString("Locked");
    Properties.vect_locklab =
	XtVaCreateManagedWidget("Vect_locklab",xmLabelWidgetClass,
				Properties.dep_form_v,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.vcomp_1,
				XmNtopOffset, 10,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, Properties.vect_max,
				NULL);
    XmStringFree(str);
    str = NewString("No ");
    Properties.vect_lock =
	XtVaCreateManagedWidget("Vect_max", xmPushButtonWidgetClass,
				Properties.dep_form_v,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.vcomp_1,
				XmNtopOffset, 5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, Properties.vect_locklab,
				NULL);
    XmStringFree(str);
    XtAddCallback(Properties.vect_lock, XmNactivateCallback,
		  form_vect_lock_call,NULL);

    /* row 4*/

    str = NewString("Position of grid\npoint on vector");
    Properties.vect_poslab =
	XtVaCreateManagedWidget("Vect_poslab",xmLabelWidgetClass,
				Properties.dep_form_v,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.vect_maxlab,
				XmNleftAttachment, XmATTACH_FORM,
				XmNtopOffset, 5,
				NULL);
    XmStringFree(str);
    str = NewString("Center");
    Properties.vect_pos = 
	XtVaCreateManagedWidget("Vect_pos", xmPushButtonWidgetClass,
				Properties.dep_form_v,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.vect_maxlab,
				XmNtopOffset, 5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, Properties.vect_poslab,
				NULL);
    XmStringFree(str);
    XtAddCallback(Properties.vect_pos, XmNactivateCallback,
		  form_vect_pos_call,NULL);
	

    str = NewString("Scale");
    Properties.vect_scalelab =
	XtVaCreateManagedWidget("Vect_scalelab",xmLabelWidgetClass,
				Properties.dep_form_v,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.vect_maxlab,
				XmNtopOffset, 5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, Properties.vect_pos,
				NULL);
    XmStringFree(str);
    str = NewString("Yes");
    Properties.vect_scale = 
	XtVaCreateManagedWidget("Vect_scale", xmPushButtonWidgetClass,
				Properties.dep_form_v,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.vect_maxlab,
				XmNtopOffset, 5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, Properties.vect_scalelab,
				NULL);
    XmStringFree(str);
    XtAddCallback(Properties.vect_scale, XmNactivateCallback,
		  form_vect_scale_call,NULL);
	
/********************END OF DEPENDANT VAR FORM********************/
/*****************AND END OF NONDEPENDANT VAR FORM****************/
    Properties.ind_form_v = XtVaCreateWidget("Properties",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNtopWidget,
					     Properties.dep_form_v,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizable, TRUE,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNborderWidth,1,
					     XmNmarginWidth,5,
					     XmNwidth, 270,
					     NULL);
    
    XtAddCallback(Properties.ind_form_v, XmNhelpCallback,
                  check_help_call,NULL);
    /*row 1*/

    str = NewString("Horizontal Axis");
    Properties.ihaxeslabv = 
      XtVaCreateManagedWidget("Ihaxes",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,
			      XmATTACH_FORM,
			      XmNtopOffset, 5,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.ihaxesrowv = 
      XtVaCreateManagedWidget("IhaxesRow", xmRowColumnWidgetClass,
			      Properties.ind_form_v,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.ihaxeslabv,
			      XmNradioBehavior, True,
			      XmNpacking,XmPACK_TIGHT,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      NULL);

    str = NewString("Linear");
    Properties.ihaxeslinv =  
      XtVaCreateManagedWidget("ihaxeslin ",
			      xmToggleButtonWidgetClass,
			      Properties.ihaxesrowv, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.ihaxeslinv,XmNarmCallback,form_linlog,(XtPointer)"h");

   
    str = NewString("Logarithmic");
    Properties.ihaxeslogv =  
      XtVaCreateManagedWidget("ihaxeslog ",
			      xmToggleButtonWidgetClass,
			      Properties.ihaxesrowv, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.ihaxeslogv,XmNarmCallback,form_linlog,(XtPointer)"h");

    /*row 2*/
    
    
    str = NewString("Major tic\ninterval");
    Properties.xmajorlabv = 
      XtVaCreateManagedWidget("Majlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ihaxesrowv,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.xmajorv =     
      XtVaCreateManagedWidget("Majv",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_v,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ihaxesrowv,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.xmajorlabv,
			      XmNcolumns, 8,
			      NULL);
    XtAddCallback(Properties.xmajorv,XmNactivateCallback,form_tick_call,(XtPointer)"X");
    XtAddCallback(Properties.xmajorv,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)XMAJORV);
    XtAddCallback(Properties.xmajorv,XmNmodifyVerifyCallback,check_num,
		  (XtPointer)XMAJORV);
    XtAddCallback(Properties.xmajorv,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)XMAJORV);
    XtAddEventHandler(Properties.xmajorv, ButtonPressMask, FALSE,
		      check_default_handler, 0);


    Properties.xminorv =     
      XtVaCreateManagedWidget("Minv",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_v,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ihaxesrowv,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.xmajorv,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.xminorv,XmNactivateCallback,form_tick_call,(XtPointer)"x");
    XtAddCallback(Properties.xminorv,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)XMINORV);
    XtAddCallback(Properties.xminorv,XmNmodifyVerifyCallback,check_num,
		  (XtPointer)XMINORV);
    XtAddCallback(Properties.xminorv,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)XMINORV);
    XtAddEventHandler(Properties.xminorv, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    
    str = NewString("Minor tic\ninterval");
    Properties.xminorlabv = 
      XtVaCreateManagedWidget("Minlabv",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ihaxesrowv,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.xminorv,
			      NULL);
    XmStringFree(str);

    
   /*row 3*/   

    str = NewString("Show every");
    Properties.vintxlab = 
      XtVaCreateManagedWidget("Vintxlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,  XmATTACH_WIDGET,
			      XmNtopWidget,Properties.xmajorv,
			      XmNtopOffset, 10,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);
    
    str = NewString("gridpoint");
    Properties.vintxlab2 = 
      XtVaCreateManagedWidget("Vintxlab",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,  XmATTACH_WIDGET,
			      XmNtopWidget,Properties.xmajorv,
			      XmNtopOffset, 10,
			      XmNrightAttachment, 
			      XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    str = NewString("");
     Properties.vintx = 
       XtVaCreateManagedWidget("Vintx",xmScaleWidgetClass,
			       Properties.ind_form_v,
			       XmNtopAttachment,XmATTACH_WIDGET,
			       XmNtopWidget, Properties.xmajorv,
			       XmNrightAttachment,XmATTACH_WIDGET,
			       XmNleftAttachment,XmATTACH_WIDGET,
			       XmNrightWidget, Properties.vintxlab2,
			       XmNleftWidget, Properties.vintxlab,
			       XmNorientation,XmHORIZONTAL,
			       XmNtitleString,str,
			       XmNmaximum,20,
			       XmNminimum,0,
			       XmNshowValue, True,
			       XmNvalue,3,
			       NULL);

    XtAddCallback(Properties.vintx, XmNvalueChangedCallback, 
		  form_vint_call, NULL);

    XtAddRawEventHandler(Properties.vintx, ButtonPressMask,FALSE,
			 vint_scale_type_in,(XtPointer)"X");
    XtUnmanageChild(XtNameToWidget(Properties.vintx,"Title"));
    /*row 4*/
    
    str = NewString("Vertical Axis ");
    Properties.ivaxeslabv = 
      XtVaCreateManagedWidget("Vhaxesv",
			      xmLabelWidgetClass, Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vintx,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    Properties.ivaxesrowv = 
      XtVaCreateManagedWidget("IvaxesRowv", xmRowColumnWidgetClass,
			      Properties.ind_form_v,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.vintx,
			      XmNtopOffset, 5,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.ivaxeslabv,
			      XmNradioBehavior, True,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      XmNpacking,XmPACK_TIGHT,
			      NULL);

    str = NewString("Linear");
    Properties.ivaxeslinv=  
      XtVaCreateManagedWidget("ivaxeslinv",
			      xmToggleButtonWidgetClass,
			      Properties.ivaxesrowv, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.ivaxeslinv,XmNarmCallback,form_linlog,(XtPointer)"v");

   
    str = NewString("Logarithmic");
    Properties.ivaxeslogv =  
      XtVaCreateManagedWidget("ivaxeslogv",
			      xmToggleButtonWidgetClass,
			      Properties.ivaxesrowv, 
			      XmNlabelString, str,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.ivaxeslogv,XmNarmCallback,form_linlog,(XtPointer)"v");


    /*row 5*/

    


    str = NewString("Major tic\ninterval");
    Properties.ymajorlabv = 
      XtVaCreateManagedWidget("Majlabv",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ivaxesrowv,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_FORM,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

    Properties.ymajorv =     
      XtVaCreateManagedWidget("Majv",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_v,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ivaxesrowv,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.ymajorlabv,
			      XmNcolumns, 8,
			      XmNbottomOffset, 10,
			      NULL);
    XtAddCallback(Properties.ymajorv,XmNactivateCallback,form_tick_call,(XtPointer)"Y");
    XtAddCallback(Properties.ymajorv,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)YMAJORV);
    XtAddCallback(Properties.ymajorv,XmNmodifyVerifyCallback,check_num,
		  (XtPointer)YMAJORV);
    XtAddCallback(Properties.ymajorv,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)YMAJORV);
    XtAddEventHandler(Properties.ymajorv, ButtonPressMask, FALSE,
			 check_default_handler, 0);


    Properties.yminorv =     
      XtVaCreateManagedWidget("Minv",
			      xmTextFieldWidgetClass,
			      Properties.ind_form_v,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ivaxesrowv,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.ymajorv,
			      XmNcolumns, 8,
			      XmNbottomOffset, 10,
			      NULL);

    XtAddCallback(Properties.yminorv,XmNactivateCallback,form_tick_call,(XtPointer)"y");
    XtAddCallback(Properties.yminorv,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)YMINORV);
    XtAddCallback(Properties.yminorv,XmNmodifyVerifyCallback,check_num,
		  (XtPointer)YMINORV);
    XtAddCallback(Properties.yminorv,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)YMINORV);
    XtAddEventHandler(Properties.yminorv, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    str = NewString("Minor tic\ninterval");
    Properties.yminorlabv = 
      XtVaCreateManagedWidget("Minlabv",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.ivaxesrowv,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.yminorv,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

    /*row 6*/   

    str = NewString("Start every");
    Properties.vintylab = 
      XtVaCreateManagedWidget("Vintylab",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,  XmATTACH_WIDGET,
			      XmNtopWidget,Properties.ymajorv,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_FORM,
			      NULL);
    XmStringFree(str);
    
    str = NewString("gridpoint");
    Properties.vintylab2 = 
      XtVaCreateManagedWidget("Vintylab",
			      xmLabelWidgetClass,
			      Properties.ind_form_v,
			      XmNlabelString,str,
			      XmNtopAttachment,  XmATTACH_WIDGET,
			      XmNtopWidget,Properties.ymajorv,
			      XmNtopOffset, 10,
			      XmNrightAttachment, XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    str = NewString("");
     Properties.vinty = 
       XtVaCreateManagedWidget("Vinty",xmScaleWidgetClass,
			       Properties.ind_form_v,
			       XmNtopAttachment,XmATTACH_WIDGET,
			       XmNtopWidget, Properties.ymajorv,
			       XmNrightAttachment,XmATTACH_WIDGET,
			       XmNleftAttachment,XmATTACH_WIDGET,
			       XmNrightWidget, Properties.vintylab2,
			       XmNleftWidget, Properties.vintylab,
			       XmNorientation,XmHORIZONTAL,
			       XmNtitleString,str,
			       XmNmaximum,20,
			       XmNminimum,0,
			       XmNshowValue, True,
			       XmNvalue,3,
			       NULL);

    XtAddCallback(Properties.vinty, XmNvalueChangedCallback, 
		  form_vint_call, NULL);
    XtAddRawEventHandler(Properties.vinty, ButtonPressMask,FALSE,
			 vint_scale_type_in,(XtPointer)"Y");
    XtUnmanageChild(XtNameToWidget(Properties.vinty,"Title"));
	    
}
       
  
void  do_skewt(parent)
     Widget parent;
{
    Pixel bot,top;

    int error = 0;
    
    /*make form but don't manage*/
    
    /*DEP FORM*/
    Properties.dep_form_s = XtVaCreateWidget("Propertiesv",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_FORM,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizable, TRUE,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNmarginWidth,5,
					     XmNborderWidth,1,
					     NULL);


    /*row 1*/

    str = NewString("Skew T");
    Properties.skewt_c_lab =
      XtVaCreateManagedWidget("Skewt_c_lab",xmLabelWidgetClass,
			      Properties.dep_form_s,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNrightAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);

    /*row 2*/

    str = NewString("Pressure\ncomponent");
    Properties.scomp_1_lab = 
      XtVaCreateManagedWidget("Scomp_1_lab",xmLabelWidgetClass,
			      Properties.dep_form_s,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.skewt_c_lab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    Properties.scomp_1 = 
      XtVaCreateManagedWidget("Scomp_1",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_s,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.skewt_c_lab,
			      XmNtopOffset, 10,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNrightOffset,10,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.scomp_1,XmNactivateCallback,
		  form_scomp_call,(XtPointer)SCOMP_1);
    XtAddCallback(Properties.scomp_1,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)SCOMP_1);
    XtAddCallback(Properties.scomp_1,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)SCOMP_1);
    XtAddCallback(Properties.scomp_1,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)SCOMP_1);

    /*row 3*/

    str = NewString("Temperature\ncomponent");
    Properties.scomp_2_lab = 
      XtVaCreateManagedWidget("Scomp_2_lab",xmLabelWidgetClass,
			      Properties.dep_form_s,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.scomp_1_lab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    Properties.scomp_2 = 
      XtVaCreateManagedWidget("Scomp_2",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_s,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.scomp_1_lab,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.scomp_2_lab,
			      XmNleftOffset, 5,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNrightOffset,10,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.scomp_2,XmNactivateCallback,
		  form_scomp_call,(XtPointer)SCOMP_2);
    XtAddCallback(Properties.scomp_2,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)SCOMP_2);
    XtAddCallback(Properties.scomp_2,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)SCOMP_2);
    XtAddCallback(Properties.scomp_2,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)SCOMP_2);
      


    /*row 4*/

    str = NewString("Dew Point\ncomponent");
    Properties.scomp_3_lab = 
      XtVaCreateManagedWidget("Scomp_3_lab",xmLabelWidgetClass,
			      Properties.dep_form_s,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.scomp_2_lab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    Properties.scomp_3 = 
      XtVaCreateManagedWidget("Scomp_3",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_s,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.scomp_2_lab,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.scomp_2_lab,
			      XmNleftOffset, 5,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNrightOffset,10,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.scomp_3,XmNactivateCallback,
		  form_scomp_call,(XtPointer)SCOMP_3);
    XtAddCallback(Properties.scomp_3,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)SCOMP_3);
    XtAddCallback(Properties.scomp_3,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)SCOMP_3);
    XtAddCallback(Properties.scomp_3,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)SCOMP_3);
    XtVaSetValues(Properties.scomp_1,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, Properties.scomp_2_lab,
		  XmNleftOffset, 5,
		  NULL);
	
/********************END OF DEPENDANT VAR FORM********************/
/*****************AND END OF NONDEPENDANT VAR FORM****************/
    /* no ind_form_s at this time */
}


void  do_traj(parent)
     Widget parent;
{
    Pixel bot,top;

    int error = 0, veclock = 0;
    unsigned int lvar;

    /*make form but don't manage*/
    
    /*DEP FORM*/
    Properties.dep_form_t = XtVaCreateWidget("Propertiest",
					     xmFormWidgetClass,
					     parent,
					     XmNtopAttachment,
					     XmATTACH_FORM,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNresizable, TRUE,
					     XmNresizePolicy,
					     XmRESIZE_ANY,
					     XmNmarginWidth,5,
					     XmNborderWidth,1,
					     XmNwidth, 278,
					     NULL);


    /*row 1*/

    str = NewString("Trajectory Properties");
    Properties.Traj_c_lab =
      XtVaCreateManagedWidget("Traj_c_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_FORM,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNrightAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);


    /*row 2*/

    str = NewString("Wind Componants along axis");
    Properties.Wind_lab =
      XtVaCreateManagedWidget("Traj_c_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Traj_c_lab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNrightAttachment,XmATTACH_FORM,
			      NULL);
    XmStringFree(str);



    
    str = NewString("X");
    Properties.tcomp_1_lab = 
      XtVaCreateManagedWidget("Tcomp_1_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Wind_lab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    Properties.tcomp_1 = 
      XtVaCreateManagedWidget("Tcomp_1",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_t,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Wind_lab,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.tcomp_1_lab,
			      XmNcolumns, 8,
			      NULL);

    XtAddCallback(Properties.tcomp_1,XmNactivateCallback,
		  form_tcomp_call,(XtPointer)TCOMP_1);
    XtAddCallback(Properties.tcomp_1,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TCOMP_1);
    XtAddCallback(Properties.tcomp_1,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TCOMP_1);
    


    Properties.tcomp_3 = 
      XtVaCreateManagedWidget("Tcomp_3",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_t,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Wind_lab,
			      XmNtopOffset, 5,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNcolumns, 8,
			      NULL);

    str = NewString("Z");
    Properties.tcomp_3_lab = 
      XtVaCreateManagedWidget("Tcomp_3_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Wind_lab,
			      XmNrightAttachment,XmATTACH_WIDGET,
			      XmNrightWidget, Properties.tcomp_3,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);

    
    XtAddCallback(Properties.tcomp_3,XmNactivateCallback,
		  form_tcomp_call,(XtPointer)TCOMP_3);
    XtAddCallback(Properties.tcomp_3,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TCOMP_3);
    XtAddCallback(Properties.tcomp_3,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TCOMP_3);
    


    str = NewString("Y");
    Properties.tcomp_2_lab = 
      XtVaCreateManagedWidget("Tcomp_2_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Wind_lab,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, Properties.tcomp_1,
			      XmNtopOffset,10,
			      XmNleftOffset,20,
			      NULL);

    XmStringFree(str);


    Properties.tcomp_2 = 
      XtVaCreateManagedWidget("Tcomp_2",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_t,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.Wind_lab,
			      XmNtopOffset, 5,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.tcomp_2_lab,
			      XmNrightAttachment, XmATTACH_WIDGET,
			      XmNrightWidget, Properties.tcomp_3_lab,
			      XmNcolumns, 8,
			      XmNrightOffset,20,
			      NULL);
    
    XtAddCallback(Properties.tcomp_2,XmNactivateCallback,
		  form_tcomp_call,(XtPointer)TCOMP_2);
    XtAddCallback(Properties.tcomp_2,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TCOMP_2);
    XtAddCallback(Properties.tcomp_2,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TCOMP_2);
    

    str = NewString("Starting Time");
    Properties.traj_ts_lab = 
      XtVaCreateManagedWidget("Traj_ts_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.tcomp_2,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNrightAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);



    str = NewString("");
    Properties.traj_ts =       
      XtVaCreateManagedWidget("Traj_ts",xmScaleWidgetClass,
			       Properties.dep_form_t,
			       XmNtopAttachment,XmATTACH_WIDGET,
			       XmNtopWidget, Properties.traj_ts_lab,
			       XmNrightAttachment,XmATTACH_FORM,
			       XmNleftAttachment,XmATTACH_FORM,
			       XmNorientation,XmHORIZONTAL,
			       XmNtitleString,str,
			       XmNmaximum,100,
			       XmNminimum,0,
			       XmNshowValue, True,
			       NULL);

    XtAddCallback(Properties.traj_ts, XmNvalueChangedCallback, 
		  form_traj_call, NULL);
    XtAddRawEventHandler(Properties.traj_ts, ButtonPressMask,FALSE,
			 traj_scale_type_in,(XtPointer)"S");
    XtUnmanageChild(XtNameToWidget(Properties.traj_ts,"Title"));

    str = NewString("Ending Time");
    Properties.traj_te_lab = 
      XtVaCreateManagedWidget("Traj_te_lab",xmLabelWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_ts,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNrightAttachment,XmATTACH_FORM,
			      XmNtopOffset,10,
			      NULL);

    XmStringFree(str);
    str = NewString("");
    Properties.traj_te =       
      XtVaCreateManagedWidget("Traj_te",xmScaleWidgetClass,
			       Properties.dep_form_t,
			       XmNtopAttachment,XmATTACH_WIDGET,
			       XmNtopWidget, Properties.traj_te_lab,
			       XmNrightAttachment,XmATTACH_FORM,
			       XmNleftAttachment,XmATTACH_FORM,
			       XmNorientation,XmHORIZONTAL,
			       XmNtitleString,str,
			       XmNmaximum,100,
			       XmNminimum,0,
			       XmNshowValue, True,
			       NULL);

    XmStringFree(str);
    XtUnmanageChild(XtNameToWidget(Properties.traj_te,"Title"));
    XtAddCallback(Properties.traj_te, XmNvalueChangedCallback, 
		  form_traj_call, NULL);
    XtAddRawEventHandler(Properties.traj_te, ButtonPressMask,FALSE,
			 traj_scale_type_in,(XtPointer)"E");

    XtAddCallback(Properties.traj_ts, XmNvalueChangedCallback,
		  min_window_check_callback,&Properties.traj_te);
    XtAddCallback(Properties.traj_ts,XmNdragCallback,
		  min_window_check_callback,&Properties.traj_te);
    XtAddCallback(Properties.traj_te, XmNvalueChangedCallback,
		  max_window_check_callback,&Properties.traj_ts);
    XtAddCallback(Properties.traj_te,XmNdragCallback,
		  max_window_check_callback,&Properties.traj_ts);

   str = NewString("Time Independent Trajectories");
    Properties.traj_ind_lab =
      XtVaCreateManagedWidget("Traj_ind_lab", xmLabelWidgetClass,
			      Properties.dep_form_t,
     			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_te,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNlabelString, str,
			      XmNtopOffset, 10,
			      NULL);
    XmStringFree(str);

    str = NewString("No");
    Properties.traj_ind = 
	XtVaCreateManagedWidget("Traj_ind", xmPushButtonWidgetClass,
				Properties.dep_form_t,
				XmNlabelString,str,
				XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget, Properties.traj_te,
				XmNtopOffset, 10,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget, Properties.traj_ind_lab,
				NULL);
    XmStringFree(str);

    (void)getlvar_("traj_2d", &lvar, &error, 7);
    if(lvar)
	str = NewString("2D ");
    else
	str = NewString("3D ");

    Properties.traj_2d =
      XtVaCreateManagedWidget("Traj_2d",xmPushButtonWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_te,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.traj_ind,
			      XmNtopOffset,10,
			      XmNleftOffset,10,
			      NULL);
    XmStringFree(str);

    XtAddCallback(Properties.traj_ind, XmNactivateCallback,
		  form_traj_ind_call,NULL);

    XtAddCallback(Properties.traj_2d, XmNactivateCallback,
		  form_traj_2d_call,NULL);
    

    str = NewString("Reference value");
    Properties.traj_vallab =
      XtVaCreateManagedWidget("Traj_vallab", xmLabelWidgetClass,
			      Properties.dep_form_t,
     			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_ind_lab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNlabelString, str,
			      XmNtopOffset, 10,
			      NULL);
    XmStringFree(str);

    Properties.traj_val = 
      XtVaCreateManagedWidget("traj_val",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_t,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_ind_lab,
			      XmNtopOffset, 10,
			      XmNcolumns, 8,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.traj_vallab,
			      NULL);
    
    XtAddCallback(Properties.traj_val,XmNactivateCallback,
		  form_traj_val_call,(XtPointer)TRAJ_VAL);
    XtAddCallback(Properties.traj_val,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TRAJ_VAL);
    XtAddCallback(Properties.traj_val,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TRAJ_VAL);
    XtAddCallback(Properties.traj_val,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)TRAJ_VAL);
			      
    str = NewString("Step");
    Properties.traj_steplab =
      XtVaCreateManagedWidget("Traj_steplab", xmLabelWidgetClass,
			      Properties.dep_form_t,
     			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_ind_lab,
			      XmNlabelString, str,
			      XmNtopOffset, 10,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.traj_val,
			      NULL);
    XmStringFree(str);

    Properties.traj_step = 
      XtVaCreateManagedWidget("traj_step",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_t,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_ind_lab,
			      XmNtopOffset, 10,
			      XmNcolumns, 8,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.traj_steplab,
			      NULL);
    
    XtAddCallback(Properties.traj_step,XmNactivateCallback,
		  form_traj_step_call,(XtPointer)TRAJ_STEP);
    XtAddCallback(Properties.traj_step,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TRAJ_STEP);
    XtAddCallback(Properties.traj_step,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TRAJ_STEP);
    XtAddCallback(Properties.traj_step,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)TRAJ_STEP);
			      
    str = NewString("Field for Trajectory Color\n(Default is to use depth)");
    Properties.traj_backgroundlab =
      XtVaCreateManagedWidget("Traj_backgroundlab", xmLabelWidgetClass,
			      Properties.dep_form_t,
     			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_step,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNlabelString, str,
			      XmNtopOffset, 10,
			      NULL);
    XmStringFree(str);

    Properties.traj_background = 
      XtVaCreateManagedWidget("traj_background",
			      xmTextFieldWidgetClass,
			      Properties.dep_form_t,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_step,
			      XmNtopOffset, 10,
			      XmNcolumns, 8,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, Properties.traj_backgroundlab,
			      NULL);
    
    XtAddCallback(Properties.traj_background,XmNactivateCallback,
		  form_traj_background_call,(XtPointer)TRAJ_BACKGROUND);
    XtAddCallback(Properties.traj_background,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TRAJ_BACKGROUND);
    XtAddCallback(Properties.traj_background,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TRAJ_BACKGROUND);
    XtAddCallback(Properties.traj_background,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)TRAJ_BACKGROUND);
			      
    str = NewString("(Select point with mouse)");
    Properties.traj_expl_lab =
      XtVaCreateManagedWidget("Traj_expl_lab", xmLabelWidgetClass,
			      Properties.dep_form_t,
     			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_backgroundlab,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNbottomAttachment,XmATTACH_FORM,
			      XmNlabelString, str,
			      XmNtopOffset, 10,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);

  str = NewString("Help");
    Properties.traj_help =
      XtVaCreateManagedWidget("Traj_help", xmPushButtonWidgetClass,
			      Properties.dep_form_t,
			      XmNlabelString,str,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, Properties.traj_backgroundlab, 
			      XmNbottomAttachment,XmATTACH_FORM,
			      XmNrightAttachment, XmATTACH_FORM,
			      XmNtopOffset, 10,
			      XmNbottomOffset, 10,
			      NULL);
    XmStringFree(str);
    XtAddCallback(Properties.traj_help, XmNactivateCallback,
		  call_driver,"help=START_TRAJ");

/********************END OF DEPENDANT VAR FORM********************/
/*****************AND END OF NONDEPENDANT VAR FORM****************/
}
       
  
void do_props(force)
     int force;
{

    float rvar;
    int ivar=0,buttons;
    int lvar=0, error=0;
    char temparr[100];
    XmString str;
    int test=0;
    char plot_type[16], plot_type1[16], plot_type2[16], plot_type3[16];
    int fix[4], pfix[4];
    Widget popup;
    extern int check_points_line_();

    static char old_type[16]=
      {'\0','\0','\0','\0','\0','\0','\0','\0','\0','\0','\0','\0','\0',
	 '\0','\0','\0'};
    static int old_fix[4]= {0,0,0,0};
    
    for (ivar = 0; ivar<10; ivar++)
	plot_type1[ivar] = plot_type2[ivar] = '\0';
    
    (void)getavar_("plotyp1",plot_type1,&error,7,16);
    (void)getavar_("plotyp2",plot_type2,&error,7,16);
    (void)getavar_("plotyp3",plot_type3,&error,7,16);
    (void)getivar_("use_buttons",&buttons,&error,11);

    test=4;
    (void)getiarr_("lock",fix,&test,&error,4);
    (void)getiarr_("ive_plock",pfix,&test,&error,9);
    test=0;
    ivar=0;
    lvar=0;
    if(buttons){
	for (test=0; test<4; test++){
	    if(fix[test]==1) ivar++;
	    else if(fix[test]==2) lvar++;
	}
	
	if(ivar+lvar==1)
	  sprintf(plot_type,"3D");
	
	if((ivar == 2 && lvar == 0) || 
	   (ivar == 1 && lvar == 1) ||
	   (ivar == 0 && lvar == 2) ) {
	    if (*plot_type2 == 'S') sprintf(plot_type,"Contour");
	    else if (*plot_type2 == 'V') sprintf(plot_type, "Vector");
	    else if (*plot_type2 == 'T') sprintf(plot_type, "Trajectory");
	}
	else if((ivar == 3 && lvar == 0) || (ivar == 2  && lvar == 1) ||
		(ivar == 1  && lvar == 2) || (ivar == 0 && lvar == 3)) {
	    if (*plot_type1 == 'S') sprintf(plot_type, "SketT");
	    else sprintf(plot_type,"Line");
	}
	else 
	    sprintf(plot_type,"None");
    }
    else{
	ivar = check_points_line_();
	switch(ivar){
	case 1: 
	    if (*plot_type1 == 'S') sprintf(plot_type, "SketT");
	    else sprintf(plot_type,"Line");
	    break;
	case 2:
	    if (*plot_type2 == 'S') sprintf(plot_type,"Contour");
	    else if (*plot_type2 == 'V') sprintf(plot_type, "Vector");
	    else if (*plot_type2 == 'T') sprintf(plot_type, "Trajectory");
	    break;
	default:
	    sprintf(plot_type,"None");
	    break;
	}
    }

    if (Properties.form){
      if(XtIsManaged(XtParent(Properties.form)))
	if (!strcmp(plot_type,old_type))
	  return;
	else
	  test = 1;
    }
    else
      test = 0;
    
    if(!test){
      popup = XtVaCreatePopupShell("Properties",
				   transientShellWidgetClass,
				   XtParent(Box),
				   XmNheight, 315,
				   XmNwidth, 270,
				   XmNautoUnmanage,FALSE,
				   XmNx, 0,
				   XmNy, 
				   HeightOfScreen(XtScreen(Box))-(215+370),
				   XtNallowShellResize,TRUE,NULL);
      
      XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		    &Properties.form);
      XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		    (XtPointer)CONTOUR_TEXT_BOXES);
      XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		    (XtPointer)LINE_TEXT_BOXES);
      XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		    (XtPointer)VECTOR_TEXT_BOXES);
      XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		    (XtPointer)SKEWT_TEXT_BOXES);
      
      Properties.form = XtVaCreateManagedWidget("Properties",
						xmFormWidgetClass,
						popup,
						XmNheight, 315,
						XmNwidth, 270,
						XmNresizable, TRUE,
						XmNautoUnmanage,FALSE,NULL);
      
      XtAddCallback(Properties.form, XmNhelpCallback,
		    check_help_call,NULL);
      (void)do_contour(Properties.form);
      (void)do_line(Properties.form, fix);
      (void)do_vector(Properties.form);
      (void)do_skewt(Properties.form);
      (void)do_traj(Properties.form);
    }
    if(*plot_type == 'C' && *(plot_type+1) == 'o'){
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type2,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Scalar, NULL);
      if(!XtIsManaged(XtParent(Properties.form)))
	XtManageChild(XtParent(Properties.form));
      if(!XtIsManaged(Properties.dep_form_c) || force){
	setup_contour_form();
	XtManageChild(Properties.dep_form_c);
	XtManageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
      
      if(XtIsManaged(Properties.dep_form_t))
	XtUnmanageChild(Properties.dep_form_t);
    }
    else if(*plot_type == 'L'){
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type1,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Line, NULL);
      if(!XtIsManaged(XtParent(Properties.form))){
	XtManageChild(XtParent(Properties.form));
      }
      if(!XtIsManaged(Properties.dep_form_l) || force){
	if(buttons)
	  setup_line_form(fix,buttons);
	else
	  setup_line_form(pfix,buttons);
	XtManageChild(Properties.dep_form_l);
	XtManageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
      
      if(XtIsManaged(Properties.dep_form_t))
	XtUnmanageChild(Properties.dep_form_t);
    }
    else if(*plot_type == 'S' &&*(plot_type+1) == 'k' ) {
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type1,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Skewt, NULL);
      if(!XtIsManaged(XtParent(Properties.form))){
	XtManageChild(XtParent(Properties.form));
      }
      if(!XtIsManaged(Properties.dep_form_s) || force){
	setup_skewt_form();
	XtManageChild(Properties.dep_form_s);
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_t))
	XtUnmanageChild(Properties.dep_form_t);
      
    }
    else if(*plot_type == 'T') {
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type2,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Trajectory, NULL);
      if(!XtIsManaged(XtParent(Properties.form))){
	XtManageChild(XtParent(Properties.form));
      }
      if(!XtIsManaged(Properties.dep_form_t) || force){
	setup_traj_form();
	XtManageChild(Properties.dep_form_t);
	/*	    XtManageChild(Properties.ind_form_v);*/
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
      
    }
    else if(*plot_type == 'V'){
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type2,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Vector, NULL);
      if(!XtIsManaged(XtParent(Properties.form)))
	XtManageChild(XtParent(Properties.form));
      if(!XtIsManaged(Properties.dep_form_v) || force){
	setup_vector_form();
	XtManageChild(Properties.dep_form_v);
	XtManageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
    }
    else if(*plot_type == 'S' && *(plot_type+1) == 'U'){
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type3,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Surface, NULL);
      if(!XtIsManaged(XtParent(Properties.form)))
	XtManageChild(XtParent(Properties.form));
      if(!XtIsManaged(Properties.dep_form_3d) || force){
	setup_3d_form();
	XtManageChild(Properties.dep_form_3d);
	XtManageChild(Properties.ind_form_3d);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
    }
    else if(*plot_type == 'P'){
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type3,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Scatter, NULL);
      if(!XtIsManaged(XtParent(Properties.form)))
	XtManageChild(XtParent(Properties.form));
      if(!XtIsManaged(Properties.dep_form_3d) || force){
	setup_3d_form();
	XtManageChild(Properties.dep_form_3d);
	XtManageChild(Properties.ind_form_3d);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
    }
    else if(*plot_type == 'W'){
      XtVaSetValues(main_widget.type_menu, XmNsubMenuId, main_widget.type3,
		    NULL);
      XtVaSetValues(main_widget.type_menu, XmNmenuHistory,
		    main_widget.Wireframe, NULL);
      if(!XtIsManaged(XtParent(Properties.form)))
	XtManageChild(XtParent(Properties.form));
      if(!XtIsManaged(Properties.dep_form_3d) || force){
	setup_3d_form();
	XtManageChild(Properties.dep_form_3d);
	XtManageChild(Properties.ind_form_3d);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
    }
    else {
      if(XtIsManaged(Properties.dep_form_c)){
	XtUnmanageChild(Properties.dep_form_c);
	XtUnmanageChild(Properties.ind_form_c);
      }
      if(XtIsManaged(Properties.dep_form_l)){
	XtUnmanageChild(Properties.dep_form_l);
	XtUnmanageChild(Properties.ind_form_l);
      }
      if(XtIsManaged(Properties.dep_form_v)){
	XtUnmanageChild(Properties.dep_form_v);
	XtUnmanageChild(Properties.ind_form_v);
      }
      if(XtIsManaged(Properties.dep_form_s))
	XtUnmanageChild(Properties.dep_form_s);
      XtUnmanageChild(XtParent(Properties.form));
    }
    if (XtIsManaged(Properties.form) && XtIsManaged(XtParent(Properties.form)))
      XRaiseWindow(XtDisplay(Properties.form),
		   XtWindow(XtParent(Properties.form)));
    
}


void do_props_(force)
     int *force;
{
  do_props(*force);
}

/********************COLOR FORM********************/

int offset = 0;
void init_color(w,data,call)/*The callbacks for this form are in color_misc.c*/
     Widget w;              /*This is due to their being called from         */
     caddr_t data;          /*additional places                              */
     XmAnyCallbackStruct *call;
{
  Widget popup, lab1, lab2, lab3;
  int error, ival;
  float rval;
  char fieldc[80], button[80], avar[256];
  
  XmString str;
  char buff[10];
  extern void color_reset_callback();
  extern void color_lock_callback();
  extern void color_shift_callback();
  extern void color_table_callback();
  if (Properties.color_form){
    if(XtIsManaged(XtParent(Properties.color_form)))
      /*XtUnmanageChild(XtParent(Properties.color_form));*/
      return;
    else
      XtManageChild(XtParent(Properties.color_form));
    return;
  }
  
  /*make form but don't manage*/
  
  popup = XtVaCreatePopupShell("Color Options",
                               transientShellWidgetClass,
			       XtParent(Box),
			       XmNheight, 100,
			       XmNwidth, 230,
			       XmNautoUnmanage,FALSE,
			       XmNx, 0,
			       XmNy, HeightOfScreen(XtScreen(w))-(215 + 260),
			       XtNallowShellResize,TRUE, NULL); 
  XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		&Properties.color_form);
  
  Properties.color_form = XtVaCreateManagedWidget("Color Options",
						  xmFormWidgetClass,popup,
						  XmNheight, 100,
						  XmNwidth, 230, XmNautoUnmanage,
						  FALSE, NULL);
  XtManageChild(Properties.color_form);
  
  /*row 1*/
  lab1 =
    XtVaCreateManagedWidget("Shift Colors\n Up/Down (</>)",
			    xmLabelWidgetClass,
			    Properties.color_form,
			    XmNleftAttachment,XmATTACH_FORM,
			    XmNtopAttachment,XmATTACH_FORM,
			    NULL);
  
  Properties.color_left = XtVaCreateManagedWidget("Left",
						  xmArrowButtonWidgetClass,
						  Properties.color_form,
						  XmNleftAttachment,
						  XmATTACH_WIDGET,
						  XmNleftWidget,
						  lab1,
						  XmNleftOffset, 10,
						  XmNtopAttachment,
						  XmATTACH_FORM,
						  XmNarrowDirection,
						  XmARROW_LEFT, NULL);
  
  Properties.color_right = XtVaCreateManagedWidget("Right",
						   xmArrowButtonWidgetClass,
						   Properties.color_form,
						   XmNleftAttachment,
						   XmATTACH_WIDGET,
						   XmNleftWidget,
						   Properties.color_left,
						   XmNleftOffset, 10,
						   XmNtopAttachment,
						   XmATTACH_FORM,
						   XmNarrowDirection,
						   XmARROW_RIGHT, NULL);
  
  XtAddCallback(Properties.color_left,
		XmNactivateCallback,color_shift_callback,(XtPointer)"L");
  
  XtAddCallback(Properties.color_right,
		XmNactivateCallback,color_shift_callback,(XtPointer)"R");
  
  /*    Properties.color_shift = XtVaCreateManagedWidget("Shift",
	xmTextFieldWidgetClass,
	Properties.color_form,
	XmNleftAttachment,
	XmATTACH_WIDGET,
	XmNleftWidget,
	lab1,
	XmNleftOffset, 10,
	XmNtopAttachment,
	XmATTACH_FORM,
	XmNcolumns, 4,
	NULL);
	
	XtVaCreateManagedWidget("Intervals",
	xmLabelWidgetClass,
	Properties.color_form,
	XmNleftAttachment,XmATTACH_WIDGET,
	XmNleftWidget, Properties.color_shift,
	XmNleftOffset, 5,
	XmNtopAttachment,XmATTACH_FORM,
	XmNtopOffset, 5,
	NULL);
	
	XtAddCallback(Properties.color_shift,XmNactivateCallback,
	cleanup_box_call,(XtPointer)COLOR_SHIFT);
	XtAddCallback(Properties.color_shift,XmNmodifyVerifyCallback,check_num,(XtPointer)COLOR_SHIFT);
	XtAddCallback(Properties.color_shift,XmNmotionVerifyCallback,
	text_box_motion,(XtPointer)COLOR_SHIFT);
	XtAddCallback(Properties.color_shift,XmNactivateCallback,
	color_shift_callback,NULL);
  */
  /*row 2*/
  lab2 = 
    XtVaCreateManagedWidget("Lock Color\nTable:",xmLabelWidgetClass,
			    Properties.color_form,
			    XmNleftAttachment,
			    XmATTACH_FORM,
			    XmNtopAttachment,
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    lab1,
			    XmNtopOffset, 5,
			    NULL);
  
  (void)getrvar_("lock_int",  &rval, &error, 8);
  if(!error && rval != 0){
    (void)getavar_("lock_field", fieldc, &error, 10, 80);
    if(!get_button_name(fieldc,button)){
      sprintf(avar,"%s Locked",button);
      str = NewString(avar);
    }
    else{
      str = NewString("Free");
    }
  }
  else{
    str = NewString("Free");
  }
  Properties.lock_color_widget = 
    XtVaCreateManagedWidget("lock widget",xmPushButtonWidgetClass,
			    Properties.color_form, 
			    XmNleftAttachment,
			    XmATTACH_WIDGET,
			    XmNleftWidget, lab2,
			    XmNtopAttachment,
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    lab1,
			    XmNtopOffset, 5,
			    NULL);
  
  XtAddCallback(Properties.lock_color_widget,
		XmNactivateCallback,color_lock_callback,NULL);
  
  XtVaSetValues(Properties.lock_color_widget,XmNlabelString,
		str,NULL);
  
  XmStringFree(str);
  
  /// TEST FOR WHICH COLOR TABLE OPTION
  
  
  lab3 = 
    XtVaCreateManagedWidget("Color\nTable:",xmLabelWidgetClass,
			    Properties.color_form,
			    XmNleftAttachment,
			    XmATTACH_FORM,
			    XmNtopAttachment,
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    lab1,
			    XmNtopOffset, 5,
			    XmNleftOffset, 115,
			    NULL);
  
  Properties.which_coltab = 
    XtVaCreateManagedWidget("Which Color Table",xmPushButtonWidgetClass,
			    Properties.color_form, 
			    XmNleftAttachment,
			    XmATTACH_WIDGET,
			    XmNleftWidget, lab3,
			    XmNtopAttachment,
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    lab1,
			    XmNtopOffset, 5,
			    NULL);
  
  str = NewString("Standard");
  //str = NewString("Trajectory");
  
  XtAddCallback(Properties.which_coltab,XmNactivateCallback,color_table_callback,NULL);
  
  XtVaSetValues(Properties.which_coltab,XmNlabelString,str,NULL);
  XmStringFree(str);
  
  /// END TEST
  
  /*row 3*/
  Properties.color_reset = XtVaCreateManagedWidget("Reset",
						   xmPushButtonWidgetClass,
						   Properties.color_form, 
						   XmNleftAttachment,
						   XmATTACH_FORM,
						   XmNtopAttachment,
						   XmATTACH_WIDGET,
						   XmNtopWidget,
						   Properties.lock_color_widget,
						   XmNtopOffset, 10,
						   XmNleftOffset, 2,
						   NULL);
  
  XtAddCallback(Properties.color_reset,XmNactivateCallback,
		color_reset_callback,NULL);
  
  
  Properties.color_toggle = 
    XtVaCreateManagedWidget("Toggle Background",
			    xmPushButtonWidgetClass,
			    Properties.color_form,
			    XmNleftAttachment,
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.color_reset,
			    XmNleftOffset, 8,
			    XmNtopAttachment,
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.lock_color_widget,
			    XmNtopOffset, 10,
			    NULL);
  
  XtAddCallback(Properties.color_toggle,XmNactivateCallback,
		swap_colors_callback,NULL);
  
  Properties.color_bar = XtVaCreateManagedWidget("Color Bar",
						 xmPushButtonWidgetClass,
						 Properties.color_form,
						 XmNrightAttachment,
						 XmATTACH_FORM,
						 XmNtopAttachment,
						 XmATTACH_WIDGET,
						 XmNtopWidget,
						 Properties.lock_color_widget,
						 XmNtopOffset, 10,
						 XmNleftAttachment,
						 XmATTACH_WIDGET,
						 XmNleftWidget,
						 Properties.color_toggle,
						 XmNleftOffset, 8,
						 XmNrightOffset, 2,
						 NULL);
  
  XtAddCallback(Properties.color_bar,XmNactivateCallback,color_bar_callback,
		NULL);
  XtManageChild(popup);
}

init_color_(){
  init_color((Widget)NULL, (caddr_t)NULL, (XmAnyCallbackStruct *)NULL);
}

/********************PRINTER SETUP FORM********************/

struct {
  Widget form;
  Widget printer_name;
  Widget color_lab;
  Widget color;
  Widget landscape;
  char current[256];
}printer_setup={NULL,NULL,NULL,NULL,NULL,"\0"};

char **get_printers(){
  static char *ptr[100];
  char buff[256], *dummy;
  FILE *printcap;
  int i;
  
  
  if((printcap = fopen("/etc/printcap", "r")) == NULL){
    ptr[0] == NULL;
    return(ptr);
  }
  
  if((ptr[0] = (char *)malloc(5))==NULL){
    make_help_widget_("Out of memory in get_printers");
    ptr[1] = NULL;
    return(ptr);
  }
  
  strcpy(ptr[0],"File");
  i = 1;
  while (fgets(buff,255,printcap) != NULL){
    switch(buff[0]){
    case '#' :
    case ' ' :
    case '\t':
    case '\n':
      break;
    default  :
      /* must be a printer */
      dummy = (char *)strchr(buff, '|');
      if(dummy == NULL)
	dummy = (char *)strchr(buff, ':');
      if(dummy == NULL)break;
      *dummy = '\0';
      if((ptr[i] = (char *)malloc(strlen(buff)+1)) == NULL){
	make_help_widget_("Out of memory in get_printers");
	ptr[i] = NULL;
	return(ptr);
      }
      strcpy(ptr[i],buff);
      i++;
      break;
    }
  }
  ptr[i] = NULL;
  return(ptr);
}

void init_printsetup(w,data,call)
     Widget w;              /*This is due to their being called from         */
     caddr_t data;          /*additional places                              */
     XmAnyCallbackStruct *call;
{
  Widget popup, done;
  int i,j;
  int color_printer,error;
  XmString str;
  char **printers;
  char * current_printer;
  if(printer_setup.form)
    if(XtIsManaged(XtParent(printer_setup.form)))
      XtUnmanageChild(XtParent(Properties.color_form));
    else{
      XtManageChild(XtParent(printer_setup.form));
      return;
    }
  
  
  /*make form but don't manage*/
  
  popup = XtVaCreatePopupShell("Printer Options",
			       transientShellWidgetClass,
			       XtParent(Box) ,
			       XmNautoUnmanage,FALSE,
			       XmNy, 
			       HeightOfScreen(XtScreen(w))-(215 + 260),
			       NULL);
  XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		&printer_setup.form);
  
  str = NewString("Printer Setup");
  printer_setup.form = XtVaCreateManagedWidget("Printer Setup",
					       xmFormWidgetClass,
					       popup,
					       XmNlabelString, str,
					       XmNwidth,260,
					       XmNheight, 270,
					       NULL);
  XmStringFree(str);
  XtAddCallback(printer_setup.form, XmNhelpCallback,
		check_help_call,NULL);
  
  str = NewString("Printers");
  printer_setup.printer_name = 
    XtVaCreateManagedWidget("Printer",
			    xmSelectionBoxWidgetClass, printer_setup.form,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNselectionPolicy,XmBROWSE_SELECT,
			    XmNwidth,260,
			    XmNheight,230,
			    XmNlistLabelString,str,
			    XmNmarginHeight,10,
			    XmNmarginWidth,2,
			    NULL);
  XtVaCreateManagedWidget("Sep",xmSeparatorWidgetClass,printer_setup.form,
			  XmNleftAttachment, XmATTACH_FORM,
			  XmNrightAttachment, XmATTACH_FORM,
			  XmNtopAttachment, XmATTACH_WIDGET,
			  XmNtopWidget,printer_setup.printer_name,
			  NULL);
  
  
  XmStringFree(str);
  str = NewString("Set Printer");
  XtVaSetValues(printer_setup.printer_name, XmNokLabelString, str, NULL);
  
  XtVaGetValues(printer_setup.printer_name, XmNcancelButton, &done, NULL);
  XtUnmanageChild(done);
  /*    str = NewString("Done");
	XtVaSetValues(done, XmNlabelString, str, NULL);
  */
  
  
  XtVaSetValues(XmSelectionBoxGetChild(printer_setup.printer_name,
				       XmDIALOG_LIST),
		XmNheight, 100, NULL);
  XmStringFree(str);
  XtAddCallback(printer_setup.printer_name, XmNcancelCallback,
		done_pparent_callback,NULL);
  XtAddCallback(printer_setup.printer_name, XmNokCallback,
		form_printer_call,NULL);
  
  str = NewString("Color Lines?");
  printer_setup.color_lab = 
    XtVaCreateManagedWidget("ColorLab",
			    xmLabelWidgetClass, printer_setup.form,
			    XmNlabelString,str,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNleftOffset, 10,
			    XmNbottomOffset, 10,
			    NULL);
  XmStringFree(str);
  
  (void)getlvar_("color_printer", &color_printer, &error, 13);
  if(color_printer)
    str = NewString("Yes");
  else
    str = NewString("No ");
  printer_setup.color = 
    XtVaCreateManagedWidget("PrintColor",
			    xmPushButtonWidgetClass, 
			    printer_setup.form,
			    XmNlabelString,str,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, printer_setup.color_lab,
			    XmNleftOffset, 10,
			    XmNbottomOffset, 10,
			    NULL);
  XmStringFree(str);
  XtAddCallback(printer_setup.color, XmNactivateCallback,
		form_color_lines_call, NULL);
  
  (void)getivar_("landscape",&i,&j,9);
  if (i) str = NewString("Landscape");
  else str = NewString("Portrait");
  printer_setup.landscape = 
    XtVaCreateManagedWidget("Landscape",
			    xmPushButtonWidgetClass, 
			    printer_setup.form,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, printer_setup.color,
			    XmNlabelString,str,
			    XmNleftOffset, 30,
			    XmNbottomOffset, 10,
			    NULL);
  XmStringFree(str);
  XtAddCallback(printer_setup.landscape, XmNactivateCallback,
		form_landscape_callback, NULL);
  
  printers = get_printers();
  for(i = 0; printers[i] != NULL; i++){
    str = NewString(printers[i]);
    XmListAddItemUnselected(
			    XmSelectionBoxGetChild 
			    (printer_setup.printer_name, XmDIALOG_LIST),
			    str, 0);
    XmStringFree(str);
    free(printers[i]);
  }
  if(printer_setup.current[0] != '\0'){
    str = NewString(printer_setup.current);
    XtVaSetValues(printer_setup.printer_name,XmNtextString,
		  str, NULL);
    XmStringFree(str);
  }
  XtManageChild(popup);
}

void  do_3d(parent)
     Widget parent;
{
  int lock[4],four = 4,error;    
  
  
  /*make form but don't manage*/
  
  /*DEP FORM*/
  Properties.dep_form_3d = XtVaCreateWidget("Properties",
					    xmFormWidgetClass,
					    parent,
					    XmNtopAttachment,
					    XmATTACH_FORM,
					    XmNleftAttachment,
					    XmATTACH_FORM,
					    XmNrightAttachment,
					    XmATTACH_FORM,
					    XmNresizable,TRUE,
					    XmNresizePolicy,
					    XmRESIZE_ANY,
					    XmNmarginWidth,5,
					    XmNborderWidth,1,
					    NULL);
  
  /*row 1*/
  XtAddCallback(Properties.dep_form_3d,XmNhelpCallback,
		check_help_call,NULL);
  
  str = NewString("Surface");
  Properties.threedlab = 
    XtVaCreateManagedWidget("Surface Values",xmLabelWidgetClass,
			    Properties.dep_form_3d,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_FORM,
			    XmNleftAttachment,XmATTACH_FORM,
			    XmNtopOffset,5,
			    NULL);
  XmStringFree(str);
  
  Properties.threed_value = 
    XtVaCreateManagedWidget("Values",
			    xmTextFieldWidgetClass,
			    Properties.dep_form_c,
			    XmNtopAttachment, 
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.threedlab,
			    XmNleftAttachment, 
			    XmATTACH_FORM,
			    XmNrightAttachment, 
			    XmATTACH_FORM,
			    XmNleftOffset, 10,
			    XmNrightOffset, 10,
			    NULL);
  
  
  
  
  XtAddCallback(Properties.threed_value,XmNactivateCallback,
		form_3val_call,NULL);
  XtAddCallback(Properties.threed_value,XmNactivateCallback,
		cleanup_box_call,(XtPointer)ThreeVALS);
  XtAddCallback(Properties.threed_value,XmNmodifyVerifyCallback,
		check_num,(XtPointer)ThreeVALS);
  XtAddCallback(Properties.threed_value,XmNmotionVerifyCallback,
		text_box_motion,(XtPointer)ThreeVALS);
  XtAddEventHandler(Properties.threed_value, ButtonPressMask, FALSE,
		    check_default_handler, 0);
  
  
  
  
  /********************END OF DEPENDANT VAR FORM********************/
  /*****************AND END OF NONDEPENDANT VAR FORM****************/
  
  Properties.ind_form_3d = XtVaCreateWidget("Properties",
					    xmFormWidgetClass,
					    parent,
					    XmNtopAttachment,
					    XmATTACH_WIDGET,
					    XmNtopWidget,
					    Properties.dep_form_3d,
					    XmNleftAttachment,
					    XmATTACH_FORM,
					    XmNrightAttachment,
					    XmATTACH_FORM,
					    XmNresizable, TRUE,
					    XmNresizePolicy,
					    XmRESIZE_ANY,
					    XmNborderWidth,1,
					    XmNmarginWidth,5,
					    NULL);
  
  XtAddCallback(Properties.ind_form_3d, XmNhelpCallback,
		check_help_call,NULL);
  /*row 1*/
  
  str = NewString("Horizontal Axis");
  Properties.ihaxeslab = 
    XtVaCreateManagedWidget("Abscissa lab",
			    xmLabelWidgetClass,
			    Properties.ind_form_c,
			    XmNlabelString,str,
			    XmNtopAttachment,
			    XmATTACH_FORM,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_FORM,
			    NULL);
  XmStringFree(str);
  
  Properties.ihaxesrow = 
    XtVaCreateManagedWidget("LINLOG", xmRowColumnWidgetClass,
			    Properties.ind_form_c,
			    XmNtopAttachment,XmATTACH_FORM,
			    XmNleftAttachment,XmATTACH_WIDGET,
			    XmNleftWidget, Properties.ihaxeslab,
			    XmNradioBehavior, True,
			    XmNpacking,XmPACK_TIGHT,
			    XmNorientation, XmHORIZONTAL,
			    XmNisHomogeneous, True,
			    XmNentryClass,xmToggleButtonWidgetClass,
			    NULL);
  
  str = NewString("Linear");
  Properties.ihaxeslin =  
    XtVaCreateManagedWidget("linlog ",
			    xmToggleButtonWidgetClass,
			    Properties.ihaxesrow, 
			    XmNlabelString, str,
			    NULL);
  XmStringFree(str);
  
  XtAddCallback(Properties.ihaxeslin,XmNarmCallback,form_linlog,(XtPointer)"h");
  
  str = NewString("Logarithmic");
  Properties.ihaxeslog =  
    XtVaCreateManagedWidget("LinLog ",
			    xmToggleButtonWidgetClass,
			    Properties.ihaxesrow, 
			    XmNlabelString, str,
			    NULL);
  XmStringFree(str);
  XtAddCallback(Properties.ihaxeslog,XmNarmCallback,form_linlog,(XtPointer)"h");
  
  
  /*row 2*/   
  
  str = NewString("Major tic\ninterval");
  Properties.xmajorlab = 
    XtVaCreateManagedWidget("Majlab",
			    xmLabelWidgetClass,
			    Properties.ind_form_c,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ihaxesrow,
			    XmNtopOffset, 10,
			    XmNleftAttachment, 
			    XmATTACH_FORM,
			    NULL);
  XmStringFree(str);
  
  
  Properties.xmajor =     
    XtVaCreateManagedWidget("ABSCISSA",
			    xmTextFieldWidgetClass,
			    Properties.ind_form_c,
			    XmNtopAttachment, 
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ihaxesrow,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.xmajorlab,
			    XmNcolumns, 8,
			    NULL);
  
  XtAddCallback(Properties.xmajor,XmNactivateCallback,form_tick_call,(XtPointer)"X");
  XtAddCallback(Properties.xmajor,XmNactivateCallback,
		cleanup_box_call,(XtPointer)XMAJOR);
  XtAddCallback(Properties.xmajor,XmNmodifyVerifyCallback,check_num,(XtPointer)XMAJOR);
  XtAddCallback(Properties.xmajor,XmNmotionVerifyCallback,
		text_box_motion,(XtPointer)XMAJOR);
  XtAddEventHandler(Properties.xmajor, ButtonPressMask, FALSE,
		    check_default_handler, 0);
  
  Properties.xminor =     
    XtVaCreateManagedWidget("abscissa",
			    xmTextFieldWidgetClass,
			    Properties.ind_form_c,
			    XmNtopAttachment, 
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ihaxesrow,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.xmajor,
			    XmNcolumns, 8,
			    NULL);
  
  XtAddCallback(Properties.xminor,XmNactivateCallback,form_tick_call,(XtPointer)"x");
  XtAddCallback(Properties.xminor,XmNactivateCallback,
		cleanup_box_call,(XtPointer)XMINOR);
  XtAddCallback(Properties.xminor,XmNmodifyVerifyCallback,check_num,(XtPointer)XMINOR);
  XtAddCallback(Properties.xminor,XmNmotionVerifyCallback,
		text_box_motion,(XtPointer)XMINOR);
  XtAddEventHandler(Properties.xminor, ButtonPressMask, FALSE,
		    check_default_handler, 0);
  
  
  str = NewString("Minor tic\ninterval");
  Properties.xminorlab = 
    XtVaCreateManagedWidget("Minlab",
			    xmLabelWidgetClass,
			    Properties.ind_form_c,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ihaxesrow,
			    XmNtopOffset, 10,
			    XmNleftAttachment, 
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.xminor,
			    NULL);
  XmStringFree(str);
  
  
  
  /*row 3*/
  
  str = NewString("Vertical Axis ");
  Properties.ivaxeslab = 
    XtVaCreateManagedWidget("Vhaxes",
			    xmLabelWidgetClass,
			    Properties.ind_form_c,
			    XmNlabelString,str,
			    XmNtopAttachment,
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.xmajor,
			    XmNtopOffset, 10,
			    XmNleftAttachment, 
			    XmATTACH_FORM,
			    NULL);
  XmStringFree(str);
  
  Properties.ivaxesrow = 
    XtVaCreateManagedWidget("LINLOG", xmRowColumnWidgetClass,
			    Properties.ind_form_c,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget, Properties.xmajor,
			    XmNtopOffset, 5,
			    XmNleftAttachment,XmATTACH_WIDGET,
			    XmNleftWidget, Properties.ivaxeslab,
			    XmNradioBehavior, True,
			    XmNorientation, XmHORIZONTAL,
			    XmNisHomogeneous, True,
			    XmNentryClass,xmToggleButtonWidgetClass,
			    XmNpacking,XmPACK_TIGHT,
			    NULL);
  
  str = NewString("Linear");
  Properties.ivaxeslin =  
    XtVaCreateManagedWidget("linlog ",
			    xmToggleButtonWidgetClass,
			    Properties.ivaxesrow, 
			    XmNlabelString, str,
			    NULL);
  XmStringFree(str);
  
  XtAddCallback(Properties.ivaxeslin,XmNarmCallback,form_linlog,(XtPointer)"v");
  
  str = NewString("Logarithmic");
  Properties.ivaxeslog =  
    XtVaCreateManagedWidget("LinLog",
			    xmToggleButtonWidgetClass,
			    Properties.ivaxesrow, 
			    XmNlabelString, str,
			    NULL);
  XmStringFree(str);
  
  XtAddCallback(Properties.ivaxeslog,XmNarmCallback,form_linlog,(XtPointer)"v");
  
  
  
  
  str = NewString("Major tic\ninterval");
  Properties.ymajorlab = 
    XtVaCreateManagedWidget("Majlab",
			    xmLabelWidgetClass,
			    Properties.ind_form_c,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ivaxesrow ,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_FORM,
			    XmNbottomOffset, 10,
			    NULL);
  XmStringFree(str);
  
  Properties.ymajor =     
    XtVaCreateManagedWidget("ORDINATE",
			    xmTextFieldWidgetClass,
			    Properties.ind_form_c,
			    XmNtopAttachment, 
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ivaxesrow  ,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.ymajorlab,
			    XmNcolumns, 8,
			    XmNbottomOffset, 10,
			    NULL);
  
  XtAddCallback(Properties.ymajor,XmNactivateCallback,form_tick_call,(XtPointer)"Y");
  XtAddCallback(Properties.ymajor,XmNactivateCallback,
		cleanup_box_call,(XtPointer)YMAJOR);
  XtAddCallback(Properties.ymajor,XmNmodifyVerifyCallback,check_num,(XtPointer)YMAJOR);
  XtAddCallback(Properties.ymajor,XmNmotionVerifyCallback,
		text_box_motion,(XtPointer)YMAJOR);
  XtAddEventHandler(Properties.ymajor, ButtonPressMask, FALSE,
		    check_default_handler, 0);
  
  
  Properties.yminor =     
    XtVaCreateManagedWidget("ordinate",
			    xmTextFieldWidgetClass,
			    Properties.ind_form_c,
			    XmNtopAttachment, 
			    XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ivaxesrow,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.ymajor,
			    XmNcolumns, 8,
			    XmNbottomOffset, 10,
			    NULL);
  
  XtAddCallback(Properties.yminor,XmNactivateCallback,form_tick_call,(XtPointer)"y");
  XtAddCallback(Properties.yminor,XmNactivateCallback,
		cleanup_box_call,(XtPointer)YMINOR);
  XtAddCallback(Properties.yminor,XmNmodifyVerifyCallback,check_num,(XtPointer)YMINOR);
  XtAddCallback(Properties.yminor,XmNmotionVerifyCallback,
		text_box_motion,(XtPointer)YMINOR);
  XtAddEventHandler(Properties.yminor, ButtonPressMask, FALSE,
		    check_default_handler, 0);
  
  
  str = NewString("Minor tic\ninterval");
  Properties.yminorlab = 
    XtVaCreateManagedWidget("Minlab",
			    xmLabelWidgetClass,
			    Properties.ind_form_c,
			    XmNlabelString,str,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget,
			    Properties.ivaxesrow  ,
			    XmNtopOffset, 5,
			    XmNleftAttachment, 
			    XmATTACH_WIDGET,
			    XmNleftWidget,
			    Properties.yminor,
			    XmNbottomOffset, 10,
			    NULL);
  XmStringFree(str);
  
  
  
}

