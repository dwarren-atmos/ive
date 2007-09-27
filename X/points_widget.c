/*$Id: points_widget.c,v 1.6 2002/08/09 19:57:28 warren Exp $
 * This routine contains the points widget.
 *
 * $Log: points_widget.c,v $
 * Revision 1.6  2002/08/09 19:57:28  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.5  1997/06/05 15:55:27  harry
 * Fixes to point slicing.
 *
 * Revision 1.4  1997/06/03 19:13:48  davidive
 * The big mouse slicing changeover. We have the new form, only requires you
 * set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 * always runs through init_points_ to catch all these things.
 * note: only actually works when two dimentions have points set. Needs to
 * be expanded into 1, 3 or 4.
 *
 * Revision 1.3  1997/04/11 18:46:44  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.2  1997/02/21 21:16:55  warren
 * Final fixes for point/mouse slicing and expanding into 2d. It is tested
 * with the z dimention free and seems to work with others, but still should
 * be looked at with known data for this.
 *
 * Revision 1.1  1996/11/07 16:54:01  warren
 * First set of activated point slicing and widgets to go with it. Only 1d 
 * plots
 * are known to be correct. Use at your own risk for 2d. There is still a 
 * known bug in the connection between the tic mark entry boxes and the 
 * plot when doing point or mouse slicing.
 *
 *
 */

static char ident[] = "$Id: points_widget.c,v 1.6 2002/08/09 19:57:28 warren Exp $";

#ifndef MAX
#define MAX(x, y) (((x) > (y))? (x):(y)) 
#endif
#ifndef MIN
#define MIN(x, y) (((x) < (y))? (x):(y))
#endif

#include <math.h>
#ifdef Linux
#include <tgmath.h>
#endif
#include <Xm/XmAll.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_macros.h>
#include <ive_text_enum.h>

Widget point_form=NULL;
extern void getrarr_(),getrvar_(),getiarr_(),getdarr_(),scale_(),unscale_(),setrarr_();
struct { 
  Widget point_form, p1x,p1y,p2x,p2y,
    x1lab,y1lab,x2lab,y2lab,boxx1,boxy1,boxx2,boxy2,
    leftlab, rightlab, secondlab, dim1lab, dim2lab,
    slide1l, slide1r, slidelab1, button1,
    slide2l, slide2r, slidelab2, button2;
}point_slicer =
 {NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL,NULL,NULL,
  NULL,NULL,NULL,NULL,NULL};

void init_points_()
{
    extern void switch_slicers(),slicer_set_point(), check_num2(),
      point_fix_call();
    extern Widget Box;
    Widget popup,done;
    XmString str, NewString();
    double domain_intercept[4], domain_slope[4];
    int  ive_plock[4], lock[4], i, count,error, min1, max1, min2, max2, val1, val2;
    float point1[4],point2[4],point3[4],dmin[4],dmax[4],loc[4],swap,spval,
      dminus[4], dmaxus[4];
    char xlabstr[8],ylabstr[8], x1val[16], x2val[16], y1val[16], y2val[16],
      name1[8], name2[8], lock1[8], lock2[8];
    extern char *float_nozero();
    extern void points_scale_type_in(), points_select_callback(), 
	points_callback();

    count = 4;
    (void)getrarr_("point_1",point1,&count, &error, 7);
    (void)getrarr_("point_2",point2,&count, &error, 7);
    (void)getrarr_("point_3",point3,&count, &error, 7);
    (void)getrarr_("scaled_loc",loc,&count, &error, 10);
    (void)getrarr_("plmin_scaled",dmin,&count,&error,13);
    (void)getrarr_("plmax_scaled",dmax,&count,&error,13);
    (void)getrarr_("plwmin",dminus,&count,&error,6);
    (void)getrarr_("plwmax",dmaxus,&count,&error,6);
    (void)getrvar_("spval", &spval, &error, 5);
    (void)getiarr_("ive_plock",ive_plock,&count,&error,9);
    (void)getiarr_("lock",lock,&count,&error,5);
    (void)getdarr_("domain_slope", domain_slope, &count, &error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &count, &error,16);
    count=0;
    for (i=0;i <4; i++){
	int one=1;
	(void)scale_(point1+i, &one, domain_slope+i,
		     domain_intercept+i, &spval);
	(void)scale_(point2+i, &one, domain_slope+i,
		     domain_intercept+i, &spval);
	(void)scale_(point3+i, &one, domain_slope+i,
		     domain_intercept+i, &spval);
	if(ive_plock[i])count++;
    }
    if(count<2)return;

      

    if(count==2){
      /*then it's easy*/
      for(i=0; i<3; i++)
	if(ive_plock[i])break;
      if(i==0)
	sprintf(xlabstr,"%s =","X");
      else
	if(i==1)
	  sprintf(xlabstr,"%s =","Y");
	else
	  sprintf(xlabstr,"%s =","Z");
      
      sprintf(x1val,"%.2g", MIN(point1[i], MIN(point2[i],point3[i])));
      if(point1[i] <=  MIN(point2[i],point3[i]))
	error = 1;
      else
	if(point2[i]<=point3[i])
	  error=2;
	else
	  error=3;
      sprintf(x2val,"%.2g", MAX(point1[i], MAX(point2[i],point3[i])));
      if(point1[i] >=  MAX(point2[i],point3[i]))
	count = 1;
      else
	if(point2[i]>=point3[i])
	  count=2;
	else
	  count=3;
      for(i=i+1; i<4; i++)
	if(ive_plock[i])break;
      if(i==1)
	sprintf(ylabstr,"%s =","Y");
      else
	if(i==2)
	  sprintf(ylabstr,"%s =","Z");
	else
	  sprintf(ylabstr,"%s =","Time");
      
      switch(error){
      case 1: sprintf(y1val,"%.2g",point1[i]);break;
      case 2: sprintf(y1val,"%.2g",point2[i]);break;
      case 3: sprintf(y1val,"%.2g",point2[i]);break;
      }
      switch(count){
      case 1: sprintf(y2val,"%.2g",point1[i]);break;
      case 2: sprintf(y2val,"%.2g",point2[i]);break;
      case 3: sprintf(y2val,"%.2g",point2[i]);break;
      }

      /* get slider info */

      for(i=0; i<3; i++)
	if(!ive_plock[i])break;
      if(i==0)
	sprintf(name1,"%s","X");
      else
	if(i==1)
	  sprintf(name1,"%s","Y");
	else
	  sprintf(name1,"%s","Z");

      if(lock[i] ==0){
	sprintf(lock1,"Free  ");
	val1=(int)0xFFFFFF;
	point1[i]=dminus[i];
	point2[i]=dminus[i];
	point3[i]=dmaxus[i];
      }
      else{
	if(lock[i] ==1){
	  sprintf(lock1,"Fixed");
	  val1=(int)loc[i];	
	  point1[i]=loc[i];
	  point2[i]=loc[i];
	  point3[i]=loc[i];
	}
	else{
	  if(lock[i] ==2){
	    sprintf(lock1,"Avg   ");
	    val1=(int)0xFFFFFF;
	    point1[i]=dminus[i];
	    point2[i]=dminus[i];
	    point3[i]=dmaxus[i];
	  }
	}
      }
      min1 = (int)dmin[i];
      max1 = (int)dmax[i];


      for(i=i+1; i<4; i++)
	if(!ive_plock[i])break;
      if(i==1)
	sprintf(name2,"%s","Y");
      else
	if(i==2)
	  sprintf(name2,"%s","Z");
	else
	  sprintf(name2,"%s","T");
      if(lock[i] ==0){
	sprintf(lock2,"Free  ");
	val2=(int)0xFFFFFF;
	point1[i]=dminus[i];
	point2[i]=dminus[i];
	point3[i]=dmaxus[i];
      }
      else{
	if(lock[i] ==1){
	  sprintf(lock2,"Fixed");
	  val2=(int)loc[i];	
	  point1[i]=loc[i];
	  point2[i]=loc[i];
	  point3[i]=loc[i];
	}
	else{
	  if(lock[i] ==2){
	    sprintf(lock2,"Avg   ");
	    val2=(int)0xFFFFFF;
	    point1[i]=dminus[i];
	    point2[i]=dminus[i];
	    point3[i]=dmaxus[i];
	  }
	}
      }
      min2 = (int)dmin[i];
      max2 = (int)dmax[i];
    
    }
    count=4;
    for (i=0; i < 4; i++) {
	int one=1;
	(void)unscale_(point1+i, &one, domain_slope+i,
		       domain_intercept+i, &spval);
	(void)unscale_(point2+i, &one, domain_slope+i,
		       domain_intercept+i, &spval);
	(void)unscale_(point3+i, &one, domain_slope+i,
		       domain_intercept+i, &spval);
    }
    (void)setrarr_("point_1",point1,&count, &error, 7);
    (void)setrarr_("point_2",point2,&count, &error, 7);
    (void)setrarr_("point_3",point3,&count, &error, 7);

    if(point_slicer.point_form == NULL){
      popup = XtVaCreatePopupShell("Arbitrary Slicer",
				   transientShellWidgetClass,
				   XtParent(Box),
				   XmNheight, 160,
				   XmNwidth, 396,
				   XmNautoUnmanage,FALSE,
				   XmNy, HeightOfScreen(XtScreen(Box))-215,
				   XtNallowShellResize,FALSE,
				   NULL);
      
      point_slicer.point_form = XtVaCreateManagedWidget
	("Points", xmFormWidgetClass,
	 popup,	 XmNheight, 206,
	 XmNwidth, 381, XmNautoUnmanage,FALSE, NULL);
      
      XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		    &point_slicer.point_form);
      XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		    (XtPointer)POINT_TEXT_BOXES);
      XtAddCallback(point_slicer.point_form, XmNhelpCallback,
		    check_help_call,NULL);
      XtVaCreateManagedWidget("Left end point", xmLabelWidgetClass,
			      point_slicer.point_form,XmNtopAttachment,
			      XmATTACH_FORM,XmNleftAttachment,XmATTACH_FORM,
			      NULL);
      str = NewString(xlabstr);
      point_slicer.x1lab =XtVaCreateManagedWidget ("X1lab",
						   xmLabelWidgetClass,
						   point_slicer.point_form,
						   XmNtopAttachment, 
						   XmATTACH_FORM,
						   XmNleftAttachment,
						   XmATTACH_FORM,
						   XmNlabelString,str,
						   XmNtopOffset, 20, 
						   XmNleftOffset, 10, 
						   NULL);
      XmStringFree(str);
      point_slicer.p1x =XtVaCreateManagedWidget ("P1X",
						 xmTextFieldWidgetClass,
						 point_slicer.point_form,
						 XmNtopAttachment, 
						 XmATTACH_FORM,
						 XmNleftAttachment,
						 XmATTACH_WIDGET,
						 XmNlabelString,str,
						 XmNleftWidget,
						 point_slicer.x1lab,
						 XmNleftOffset,3,
						 XmNtopOffset, 20, 
						 XmNcolumns, 6,
						 XmNvalue,x1val,NULL);
      
      XtAddCallback(point_slicer.p1x,XmNactivateCallback,
		    slicer_set_point,(XtPointer)1);
      XtAddCallback(point_slicer.p1x,XmNactivateCallback,
		    cleanup_box_call,(XtPointer)POINT_X1);
      XtAddCallback(point_slicer.p1x,XmNmodifyVerifyCallback,
		    check_num,(XtPointer)POINT_X1);
      XtAddCallback(point_slicer.p1x,XmNmotionVerifyCallback,
		    text_box_motion,(XtPointer)POINT_X1);
      XtAddCallback(point_slicer.p1x,XmNhelpCallback,
		    call_driver,(XtPointer)"help xpoints");
      str = NewString(ylabstr);
      point_slicer.y1lab = XtVaCreateManagedWidget ("Y1lab",
						    xmLabelWidgetClass,
						    point_slicer.point_form,
						    XmNtopAttachment, 
						    XmATTACH_FORM,
						    XmNleftAttachment,
						    XmATTACH_WIDGET,
						    XmNleftWidget,
						    point_slicer.p1x,
						    XmNlabelString,str,
						    XmNleftOffset,10, 
						    XmNtopOffset, 20,NULL);
      
      XmStringFree(str);
      point_slicer.p1y =XtVaCreateManagedWidget ("P1Y",
						 xmTextFieldWidgetClass,
						 point_slicer.point_form,
						 XmNtopAttachment, 
						 XmATTACH_FORM,
						 XmNleftAttachment,
						 XmATTACH_WIDGET,
						 XmNlabelString,str,
						 XmNleftWidget,
						 point_slicer.y1lab,
						 XmNleftOffset,3,
						 XmNtopOffset, 20, 
						 XmNcolumns, 6,
						 XmNvalue,y1val,NULL);
      
      XtAddCallback(point_slicer.p1y,XmNactivateCallback,
		    slicer_set_point,(XtPointer)3);
      XtAddCallback(point_slicer.p1y,XmNactivateCallback,
		    cleanup_box_call,(XtPointer)POINT_Y1);
      XtAddCallback(point_slicer.p1y,XmNmodifyVerifyCallback,
		    check_num,(XtPointer)POINT_Y1);
      XtAddCallback(point_slicer.p1y,XmNmotionVerifyCallback,
		    text_box_motion,(XtPointer)POINT_Y1);
      XtAddCallback(point_slicer.p1y,XmNhelpCallback,
		    call_driver,(XtPointer)"help ypoints");

      XtVaCreateManagedWidget("Right end point", xmLabelWidgetClass,
			      point_slicer.point_form,XmNrightAttachment,
			      XmATTACH_FORM,XmNtopAttachment,XmATTACH_FORM,
			      XmNrightOffset,10,NULL);
      
      point_slicer.p2y =XtVaCreateManagedWidget ("P2Y",
						 xmTextFieldWidgetClass,
						 point_slicer.point_form,
						 XmNtopAttachment, 
						 XmATTACH_FORM,
						 XmNrightAttachment,
						 XmATTACH_FORM,
						 XmNlabelString,str,
						 XmNrightOffset,10,
						 XmNtopOffset, 20, 
						 XmNcolumns, 6,
						 XmNvalue,y2val,NULL);
      XtAddCallback(point_slicer.p2y,XmNactivateCallback,
		    slicer_set_point,(XtPointer)4);
      XtAddCallback(point_slicer.p2y,XmNactivateCallback,
		    cleanup_box_call,(XtPointer)POINT_Y2);
      XtAddCallback(point_slicer.p2y,XmNmodifyVerifyCallback,
		    check_num,(XtPointer)POINT_Y2);
      XtAddCallback(point_slicer.p2y,XmNmotionVerifyCallback,
		    text_box_motion,(XtPointer)POINT_Y2);
      XtAddCallback(point_slicer.p2y,XmNhelpCallback,
		    call_driver,(XtPointer)"help ypoints");
      
      str = NewString(ylabstr);
      point_slicer.y2lab = XtVaCreateManagedWidget ("Y2lab",
						    xmLabelWidgetClass,
						    point_slicer.point_form,
						    XmNtopAttachment, 
						    XmATTACH_FORM,
						    XmNrightAttachment,
						    XmATTACH_WIDGET,
						    XmNrightWidget,
						    point_slicer.p2y, 
						    XmNtopOffset, 20,
						    XmNlabelString,str,
						    XmNrightOffset,3, NULL);
      XmStringFree(str);
      point_slicer.p2x =XtVaCreateManagedWidget ("P2X",
						 xmTextFieldWidgetClass,
						 point_slicer.point_form,
						 XmNtopAttachment, 
						 XmATTACH_FORM,
						 XmNrightAttachment,
						 XmATTACH_WIDGET,
						 XmNlabelString,str,
						 XmNrightWidget,
						 point_slicer.y2lab,
						 XmNrightOffset,10,
						 XmNtopOffset, 20, 
						 XmNcolumns, 6,
						 XmNvalue,x2val,NULL);
      XtAddCallback(point_slicer.p2x,XmNactivateCallback,
		    slicer_set_point,(XtPointer)2);
      XtAddCallback(point_slicer.p2x,XmNactivateCallback,
		    cleanup_box_call,(XtPointer)POINT_X2);
      XtAddCallback(point_slicer.p2x,XmNmodifyVerifyCallback,
		    check_num,(XtPointer)POINT_X2);
      XtAddCallback(point_slicer.p2x,XmNmotionVerifyCallback,
		    text_box_motion,(XtPointer)POINT_X2);
      XtAddCallback(point_slicer.p2x,XmNhelpCallback,
		    call_driver,(XtPointer)"help xpoints");

      str = NewString(xlabstr);
      point_slicer.x2lab = XtVaCreateManagedWidget ("X2lab",
						    xmLabelWidgetClass,
						    point_slicer.point_form,
						    XmNtopAttachment, 
						    XmATTACH_FORM,
						    XmNtopOffset, 20,
						    XmNrightAttachment,
						    XmATTACH_WIDGET,
						    XmNrightWidget,
						    point_slicer.p2x,
						    XmNrightOffset,3,
						    XmNlabelString,str,
						    NULL);
      XmStringFree(str);




      /**********sliders**********/

      if (min1 != max1) {
	  str=NewString("");
	  point_slicer.slide1l = 	
	      XtVaCreateManagedWidget(name1,xmScaleWidgetClass,
				      point_slicer.point_form,
				      XmNtitleString,str,
				      XmNorientation,XmHORIZONTAL,
				      XmNshowValue,TRUE,
				      XmNwidth,150,
				      XmNprocessingDirection,
				      (max1>min1)?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 60,
				      XmNleftAttachment,
				      XmATTACH_FORM,
				      XmNmaximum,(int)MAX(max1,min1),
				      XmNminimum,(int)MIN(max1,min1),
				      XmNvalue,
				      (val1<(int)0xFFFFFF ? val1:min1),
				      XmNleftOffset, 10,
				      NULL);
	  XmStringFree(str);
	  XtUnmanageChild(XtNameToWidget(point_slicer.slide1l,"Title"));

	  point_slicer.slidelab1=
	      XtVaCreateManagedWidget(name1,
				      xmLabelWidgetClass,
				      point_slicer.point_form,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 80,
				      XmNleftAttachment,
				      XmATTACH_WIDGET,
				      XmNleftWidget,
				      point_slicer.slide1l,
				      XmNleftOffset, 10,
				      NULL);
	  str=NewString("");
	  point_slicer.slide1r = 	
	      XtVaCreateManagedWidget(name1,xmScaleWidgetClass,
				      point_slicer.point_form,
				      XmNtitleString,str,
				      XmNorientation,XmHORIZONTAL,
				      XmNshowValue,TRUE,
				      XmNwidth,150,
				      XmNprocessingDirection,
				      (max1>min1)?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 60,
				      XmNrightAttachment,
				      XmATTACH_FORM,
				      XmNrightOffset, 10,
				      XmNmaximum,MAX(max1,min1),
				      XmNminimum,MIN(max1,min1),
				      XmNvalue,
				      (val1<(int)0xFFFFFFFF ? val1:max1),
				      NULL);
	  XmStringFree(str);
	  XtUnmanageChild(XtNameToWidget(point_slicer.slide1r,"Title"));
	  str=NewString(lock1);
	  point_slicer.button1 = 
	      XtVaCreateManagedWidget(name1,
				      xmPushButtonWidgetClass,
				      point_slicer.point_form,
				      XmNlabelString ,str,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 80,
				      XmNleftAttachment,
				      XmATTACH_WIDGET,
				      XmNleftOffset, 10,
				      XmNleftWidget,
				      point_slicer.slidelab1,
				      NULL);
	  XmStringFree(str);
	  XtAddEventHandler(point_slicer.button1,ButtonPressMask,FALSE,
			    domain_units_call,name1);
	  XtAddEventHandler(point_slicer.slidelab1,ButtonPressMask,FALSE,
			    domain_units_call,name1);
	  XtAddCallback(point_slicer.button1,XmNactivateCallback,
			point_fix_call,(XtPointer)1);

	  XtAddCallback(point_slicer.slide1l,XmNvalueChangedCallback,
		(XtCallbackProc)point_min_window_check_callback,
		(XtCallbackProc)(XtPointer)point_slicer.slide1r);
	  XtAddCallback(point_slicer.slide1l,XmNvalueChangedCallback,
		(XtCallbackProc)point_window_callback,(XtPointer)1);
	  XtAddCallback(point_slicer.slide1l,XmNdragCallback,
		(XtCallbackProc)point_min_window_check_callback,
			(XtPointer)point_slicer.slide1r);
	  /* for poping boxes on top*/
	  /*      XtAddRawEventHandler(point_slicer.slide1l.mins[i],
		                       ButtonPressMask,FALSE,
		                       point_scale_type_in,(XtPointer)i);
		  */

	  XtAddCallback(point_slicer.slide1r,XmNvalueChangedCallback,
		       (XtCallbackProc)point_max_window_check_callback,
			(XtPointer)point_slicer.slide1l);
	  XtAddCallback(point_slicer.slide1r,XmNvalueChangedCallback,
		       (XtCallbackProc)point_window_callback,(XtPointer)1);
	  XtAddCallback(point_slicer.slide1r,XmNdragCallback,
		       (XtCallbackProc)point_max_window_check_callback,
			(XtPointer)point_slicer.slide1l);
	  /* for poping boxes on top*/
	  /*      XtAddRawEventHandler(point_slicer.slide1r.mins[i],
			   ButtonPressMask,FALSE,
			   point_scale_type_in,(XtPointer)i);

			   */
      }
      else point_slicer.slide1l = (Widget) NULL;

      if (min2 != max2) {
	  str=NewString("");
	  point_slicer.slide2l =  
	      XtVaCreateManagedWidget(name2,xmScaleWidgetClass,
				      point_slicer.point_form,
				      XmNtitleString,str,
				      XmNorientation,XmHORIZONTAL,
				      XmNshowValue,TRUE,
				      XmNwidth,150,
				      XmNprocessingDirection,
				      (max2>min2)?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 100,
				      XmNleftAttachment,
				      XmATTACH_FORM,
				      XmNmaximum,(int)MAX(max2,min2),
				      XmNminimum,(int)MIN(max2,min2),
				      XmNvalue,
				      (val2<(int)0xFFFFFF ? val2:min2),
				      XmNleftOffset, 10,
				      NULL);
	  XmStringFree(str);
	  XtUnmanageChild(XtNameToWidget(point_slicer.slide2l,"Title"));

	  point_slicer.slidelab2=
	      XtVaCreateManagedWidget(name2,
				      xmLabelWidgetClass,
				      point_slicer.point_form,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 120,
				      XmNleftAttachment,
				      XmATTACH_WIDGET,
				      XmNleftWidget,
				      point_slicer.slide2l,
				      XmNleftOffset, 10,
				      NULL);
	  str=NewString("");
	  point_slicer.slide2r = 	
	      XtVaCreateManagedWidget(name2,xmScaleWidgetClass,
				      point_slicer.point_form,
				      XmNtitleString,str,
				      XmNorientation,XmHORIZONTAL,
				      XmNshowValue,TRUE,
				      XmNwidth,150,
				      XmNprocessingDirection,
				      (max2>min2)?XmMAX_ON_RIGHT:XmMAX_ON_LEFT,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 100,
				      XmNrightAttachment,
				      XmATTACH_FORM,
				      XmNrightOffset, 10,
				      XmNmaximum,MAX(max2,min2),
				      XmNminimum,MIN(max2,min2),
				      XmNvalue,
				      (val2<(int)0xFFFFFF ? val2:max2),
				      NULL);
	  XmStringFree(str);
	  XtUnmanageChild(XtNameToWidget(point_slicer.slide2r,"Title"));
	  str=NewString(lock2);
	  point_slicer.button2 = 
	      XtVaCreateManagedWidget(name2,
				      xmPushButtonWidgetClass,
				      point_slicer.point_form,
				      XmNlabelString ,str,
				      XmNtopAttachment, 
				      XmATTACH_FORM,
				      XmNtopOffset, 120,
				      XmNleftAttachment,
				      XmATTACH_WIDGET,
				      XmNleftOffset, 10,
				      XmNrightOffset, 10,
				      XmNleftWidget,
				      point_slicer.slidelab2,
				      NULL);
	  XmStringFree(str);
	  XtAddEventHandler(point_slicer.button2,ButtonPressMask,FALSE,
			    domain_units_call,name2);
	  XtAddEventHandler(point_slicer.slidelab2,ButtonPressMask,FALSE,
			    domain_units_call,name2);
	  XtAddCallback(point_slicer.button2,XmNactivateCallback,
			point_fix_call,(XtPointer)2);

	  XtAddCallback(point_slicer.slide2l,XmNvalueChangedCallback,
		(XtCallbackProc)point_min_window_check_callback,
			(XtPointer)point_slicer.slide2r);
	  XtAddCallback(point_slicer.slide2l,XmNvalueChangedCallback,
		(XtCallbackProc)point_window_callback,(XtPointer)2);
	  XtAddCallback(point_slicer.slide2l,XmNdragCallback,
		(XtCallbackProc)point_min_window_check_callback,
			(XtPointer)point_slicer.slide2r);
	  /* for poping boxes on top*/
	  /*      XtAddRawEventHandler(point_slicer.slide2l.mins[i],
			   ButtonPressMask,FALSE,
			   point_scale_type_in,(XtPointer)i);

			   */
	  XtAddCallback(point_slicer.slide2r,XmNvalueChangedCallback,
		(XtCallbackProc)point_min_window_check_callback,
			(XtPointer)point_slicer.slide2l);
	  XtAddCallback(point_slicer.slide2r,XmNvalueChangedCallback,
		(XtCallbackProc)point_window_callback,(XtPointer)2);
	  XtAddCallback(point_slicer.slide2r,XmNdragCallback,
		(XtCallbackProc)point_min_window_check_callback,
			(XtPointer)point_slicer.slide2l);
	  /* for poping boxes on top*/
	  /*      XtAddRawEventHandler(point_slicer.slide2r.mins[i],
			   ButtonPressMask,FALSE,
			   point_scale_type_in,(XtPointer)i);

			   */

      }
      else point_slicer.slide2l = (Widget) NULL;

    done = XtVaCreateManagedWidget("Return to coordinate parallel slicing",
				   xmPushButtonWidgetClass,
				   point_slicer.point_form,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNbottomAttachment, XmATTACH_FORM,
				   NULL);
    XtAddCallback(done ,XmNactivateCallback,
		  switch_slicers,(XtPointer)1);

    XtManageChild(popup);
    }
    else{
      str = NewString(xlabstr);
      XtVaSetValues(point_slicer.x1lab, XmNlabelString,str, NULL);
      XtVaSetValues(point_slicer.x2lab, XmNlabelString,str, NULL);
      XmStringFree(str);

      str = NewString(ylabstr);
      XtVaSetValues(point_slicer.y1lab, XmNlabelString,str, NULL);
      XtVaSetValues(point_slicer.y2lab, XmNlabelString,str, NULL);
      XmStringFree(str);

      XtVaSetValues(point_slicer.p1x, XmNvalue, x1val, NULL);
      XtVaSetValues(point_slicer.p1y, XmNvalue, y1val, NULL);
      XtVaSetValues(point_slicer.p2x, XmNvalue, x2val, NULL);
      XtVaSetValues(point_slicer.p2y, XmNvalue, y2val, NULL);
      
      
      
    }  
    if (point_slicer.slide1l) {
	if(!strcmp(lock1,"Fixed")){
	    XtUnmanageChild(point_slicer.slide1r);
	    XtVaSetValues(point_slicer.slide1l, XmNmaximum,(int)MAX(max1,min1),
			  XmNminimum,(int)MIN(max1,min1),
			  XmNvalue, (val1<(int)0xFFFFFF ? val1:min1),
			  NULL);
	}
	else{
	    if(!XtIsManaged(point_slicer.slide1r))
		XtManageChild(point_slicer.slide1r);
	    switch(*name1){
	    case 'X':i=0;break;
	    case 'Y':i=1;break;
	    case 'Z':i=2;break;
	    case 'T':i=3;break;
	    }
	    XtVaSetValues(point_slicer.slide1l, XmNmaximum,(int)MAX(max1,min1),
			  XmNminimum,(int)MIN(max1,min1),
			  XmNvalue, ((int)dmin[i]<(int)0xFFFFFF ? 
				     (int)dmin[i]:min1),
			  NULL);
	    XtVaSetValues(point_slicer.slide1r, XmNmaximum,MAX(max1,min1),
			  XmNminimum,MIN(max1,min1),
			  XmNvalue, ((int)dmax[i]<(int)0xFFFFFF ? 
				     (int)dmax[i]:max1),
			  NULL);
	}
    }
    if (point_slicer.slide2l) {
	if(!strcmp(lock2,"Fixed")){
	    XtUnmanageChild(point_slicer.slide2r);
	    XtVaSetValues(point_slicer.slide2l, XmNmaximum,(int)MAX(max2,min2),
			  XmNminimum,(int)MIN(max2,min2),
			  XmNvalue, (val2<(int)0xFFFFFF ? val2:min2),
			  NULL);
	}      
	else{
	    if(!XtIsManaged(point_slicer.slide2r))
		XtManageChild(point_slicer.slide2r);
	    switch(*name2){
	    case 'X':i=0;break;
	    case 'Y':i=1;break;
	    case 'Z':i=2;break;
	    case 'T':i=3;break;
	    }
	    XtVaSetValues(point_slicer.slide2l, XmNmaximum,(int)MAX(max2,min2),
			  XmNminimum,(int)MIN(max2,min2),
			  XmNvalue, ((int)dmin[i]<(int)0xFFFFFF ?
				     (int)dmin[i]:min2),
			  NULL);
	    XtVaSetValues(point_slicer.slide2r, XmNmaximum,MAX(max2,min2),
			  XmNminimum,MIN(max2,min2),
			  XmNvalue, ((int)dmax[i]<(int)0xFFFFFF ?
				     (int)dmax[i]:max2),
			  NULL);
	}
    }
}


