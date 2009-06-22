/****************************************************************************
 * $Id: background.c,v 1.13 2006/04/24 21:36:00 warren Exp $
 *
 * This routine puts up the "background" widget, which is currently
 * just the map widget.
 *
 * Call: do_map(Widget w, caddr_t data, XmAnyCallbackStruct *call)
 *
 * This is a standard Xt callback routine.
 *
 * $Log: background.c,v $
 * Revision 1.13  2006/04/24 21:36:00  warren
 * Added a scroll bar to the fields area, fixed the background form so that
 * you can see everything with the new motif, fixed the call to getlvar in
 * annotation.c to look for color_bar instead of color.
 *
 * Revision 1.12  2001/03/16 20:33:57  harryive
 * Change long to int for getvar variables.
 *
 * Revision 1.11  2001/03/15 18:49:07  harry
 * Forgot one outline type in setting widget status.
 *
 * Revision 1.10  1997/04/11 18:46:38  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.9  1997/03/12  18:57:23  harry
 * Add menu for map outlines.
 *
 * Revision 1.8  1996/08/09 19:28:04  harry
 * Take out %.2f formats and replace with calls to float_nozero.
 *
 * Revision 1.7  1995/10/19  22:13:07  harry
 * Set defaults by clicking right mouse button.  Make all TextWidgets into
 * TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.6  1995/07/10  17:16:00  warren
 * Finished renaming widgets so the help key will work. Also fixed the color
 * menus not to contain an _ in the boxes and fixed the second vector
 * componant update proble.
 *
 * Revision 1.5  1994/05/18  17:53:00  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.4  1993/12/15  18:11:11  harry
 * Update map background widget - add default button and clipping menu.
 *
 * Revision 1.3  1993/11/29  19:37:47  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.2  1993/06/25  20:05:53  warren
 * Do updates tp map widget.
 *
 * Revision 1.1  1993/06/24  23:14:54  warren
 * First version of the Base Map form
 *
 ****************************************************************************/
static char ident[] = "$Id: background.c,v 1.13 2006/04/24 21:36:00 warren Exp $";

#include <strings.h>
#include <Xm/XmAll.h>
#include <X11/Intrinsic.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>

extern void getrvar_(),getlvar_(),getavar_(),call_defmap_();
XmString NewString();
/*
   This structure contains all the map widgets.
   */
struct{
    Widget map_form;
    Widget drawmap,outline,outline_menu,plat,plon,rota,glspace, proj;
    Widget out_no,out_co,out_us,out_ps,out_po,out_m3,out_m1;
    Widget proj_lc,proj_st,proj_or,proj_le,proj_gn,proj_ae,proj_sv;
    Widget proj_ce,proj_me,proj_mo,proj_nhst,proj_shst,proj_nhor,proj_shor;
    Widget proj_menu, labbutton, terbutton,defaultbutton;
    Widget clip, clip_menu, clip_window, clip_last;
}map_forms={NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	      NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	      NULL,NULL,NULL,NULL,NULL,NULL};

void do_map(w,data,call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
  Widget popup,proj,l1,l2,l3,l4,l5,l6,l7,l8,l9;
    XmString str;
    float rvar;
    int lvar, lvar2, error;
    char temparr[100];

    if (map_forms.map_form){
	/*
	   If the widget already exists, make it visible if it isn't and
	   vice versa.
	   */
        if(XtIsManaged(XtParent(map_forms.map_form)))
           XtUnmanageChild(XtParent(map_forms.map_form));
        else
          XtManageChild(XtParent(map_forms.map_form));
        return;
    }
    /*
       Create the widget.
       */
    popup = XtVaCreatePopupShell("Map Options",
				 transientShellWidgetClass,
				 XtParent(Box),
				 XmNheight, 395,
				 XmNwidth, 233,
				 XmNautoUnmanage,FALSE,
				 XmNx, 0,
				 XmNy, 
				 HeightOfScreen(XtScreen(w))-(215 + 260),
				 XtNallowShellResize,TRUE,
				 NULL);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &map_forms.map_form);
    XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
                  (XtPointer)MAP_TEXT_BOXES);

    map_forms.map_form = XtVaCreateManagedWidget("Base Map",
						 xmFormWidgetClass,
						 popup,
						 XmNheight, 395,
						 XmNwidth, 233,
						 XmNmarginWidth, 1,
						 XmNautoUnmanage,FALSE,
						 NULL);
    /*
       "Label Plot"
       */
    getlvar_("label",&lvar,&error,5);
    l1=XtVaCreateManagedWidget("Label Plot",xmLabelWidgetClass,
			    map_forms.map_form,
			    XmNtopAttachment,
			    XmATTACH_FORM,
			    XmNleftAttachment,
			    XmATTACH_FORM,
			    NULL);
    
    if(lvar)
      str = NewString("Yes");
    else
      str = NewString("No ");
    map_forms.labbutton = XtVaCreateManagedWidget("Label",
						  xmPushButtonWidgetClass,
						  map_forms.map_form, 
						  XmNlabelString, str,
						  XmNtopAttachment,
						  XmATTACH_FORM,
						  XmNleftAttachment,
						  XmATTACH_WIDGET,
						  XmNleftWidget,l1,
						  NULL);
    XmStringFree(str);

    XtAddCallback(map_forms.labbutton,XmNactivateCallback,
		  form_label_call,NULL);
    /*
       "Draw Map"
       */
    getlvar_("mapflg",&lvar,&error,6);
    l2=XtVaCreateManagedWidget("Draw Map",xmLabelWidgetClass,
			       map_forms.map_form,
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.labbutton,
			       XmNleftAttachment,
			       XmATTACH_FORM,
			       NULL);
    
    if(lvar)
      str = NewString("Yes");
    else
      str = NewString("No ");
    
    map_forms.drawmap = XtVaCreateManagedWidget("DrawMap",
						xmPushButtonWidgetClass,
						map_forms.map_form, 
						XmNlabelString, str,
						XmNtopAttachment,
						XmATTACH_WIDGET,
						XmNtopWidget,map_forms.labbutton,
						XmNleftAttachment,
						XmATTACH_WIDGET,
						XmNleftWidget,l2,
						NULL);
    XmStringFree(str);

    XtAddCallback(map_forms.drawmap,XmNactivateCallback,
		  form_drawmap_call,NULL);
    /*
       "Continental Outlines"
       */
    l3=XtVaCreateManagedWidget("Map Outlines",xmLabelWidgetClass,
			       map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.drawmap,
			       XmNleftAttachment,
			       XmATTACH_FORM,
			       NULL);
    
    map_forms.outline_menu=XmCreatePulldownMenu(map_forms.map_form,"_pulldown",
						NULL,0);
    map_forms.outline = XmCreateOptionMenu(map_forms.map_form,
					   "mapotl",NULL,0);
    XtVaSetValues(map_forms.outline, 
		  XmNtopAttachment,
		  XmATTACH_WIDGET,
		  XmNtopWidget,l3,
		  XmNleftAttachment,
		  XmATTACH_FORM,		  
		  XmNsubMenuId, map_forms.outline_menu, NULL);

    XtUnmanageChild(XmOptionLabelGadget(map_forms.outline));

    XtManageChild(map_forms.outline);

    map_forms.out_no = XtVaCreateManagedWidget("None",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    map_forms.out_co = XtVaCreateManagedWidget("Continental",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    map_forms.out_us = XtVaCreateManagedWidget("U.S. State",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    map_forms.out_po = XtVaCreateManagedWidget("Continental & International",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    map_forms.out_ps = XtVaCreateManagedWidget("U.S., Cont. & Inter'l",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    map_forms.out_m3 = XtVaCreateManagedWidget("N. American - 3 Mile Res.",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    map_forms.out_m1 = XtVaCreateManagedWidget("N. American - 1 Mile Res.",
					     xmPushButtonWidgetClass,
					     map_forms.outline_menu,
					     XmNmarginWidth,0,
					     NULL);

    XtAddCallback(map_forms.out_no,XmNactivateCallback,
		  call_driver,"mapoutl=no");
    XtAddCallback(map_forms.out_co,XmNactivateCallback,
		  call_driver,"mapoutl=co");
    XtAddCallback(map_forms.out_us,XmNactivateCallback,
		  call_driver,"mapoutl=us");
    XtAddCallback(map_forms.out_po,XmNactivateCallback,
		  call_driver,"mapoutl=po");
    XtAddCallback(map_forms.out_ps,XmNactivateCallback,
		  call_driver,"mapoutl=ps");
    XtAddCallback(map_forms.out_m3,XmNactivateCallback,
		  call_driver,"mapoutl=m3");
    XtAddCallback(map_forms.out_m1,XmNactivateCallback,
		  call_driver,"mapoutl=m1");
    /*
      Set initial value for map outline menu.
      */
    getavar_("outlin", temparr, &error, 6, 80);
    if (!error) {
	if(!strcasecmp("no",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_no,
			  NULL);
	else if(!strcasecmp("co",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_co,
			  NULL);
	else if(!strcasecmp("us",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_us,
			  NULL);
	else if(!strcasecmp("po",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_po,
			  NULL);
	else if(!strcasecmp("ps",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_ps,
			  NULL);
	else if(!strcasecmp("m3",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_m3,
			  NULL);
	else if(!strcasecmp("m1",temparr))
	    XtVaSetValues(map_forms.outline,
			  XmNmenuHistory, map_forms.out_m1,
			  NULL);
    }
    /*
       "Default Map Settings"
       */
    l4=XtVaCreateManagedWidget("Default Map Settings",xmLabelWidgetClass,
			       map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.outline,
			       XmNleftAttachment,
			       XmATTACH_FORM,		  
			       NULL);
    
    getlvar_("defmap",&lvar,&error,6);
    
    if(lvar) {
      str = NewString("Yes");
      call_defmap_();
    }
    else
      str = NewString("No ");
    
    map_forms.defaultbutton = XtVaCreateManagedWidget("Default_Map",
						      xmPushButtonWidgetClass,
						      map_forms.map_form,
						      XmNtopAttachment,
						      XmATTACH_WIDGET,
						      XmNtopWidget,map_forms.outline,
						      XmNleftAttachment,
						      XmATTACH_WIDGET,
						      XmNleftWidget,l4,
						      XmNlabelString, str,
						      NULL);
    XmStringFree(str);
    XtAddCallback(map_forms.defaultbutton,XmNactivateCallback,
		  form_defaultmap_call,NULL);
    /*
       Clipping menu
       */
    map_forms.clip_menu = XmCreatePulldownMenu(map_forms.map_form, "_pullclip",
					       NULL, 0);
    map_forms.clip = XmCreateOptionMenu(map_forms.map_form, "maplimits",
					NULL, 0);
    XtVaSetValues(map_forms.clip,
		  XmNtopAttachment,
		  XmATTACH_WIDGET,
		  XmNtopWidget,map_forms.defaultbutton,
		  XmNleftAttachment,
		  XmATTACH_FORM,
		  XmNsubMenuId, map_forms.clip_menu,
		  NULL);
    XtUnmanageChild(XmOptionLabelGadget(map_forms.clip));
    XtManageChild(map_forms.clip);
    map_forms.clip_window = XtCreateManagedWidget("Clip Map to Window",
						  xmPushButtonWidgetClass,
						  map_forms.clip_menu, NULL,0);
    map_forms.clip_last = XtCreateManagedWidget("Plot on last Map",
						xmPushButtonWidgetClass,
						map_forms.clip_menu, NULL,0);
    XtAddCallback(map_forms.clip_window,XmNactivateCallback,
		  call_driver,"maplim=cl");
    XtAddCallback(map_forms.clip_last,XmNactivateCallback,
		  call_driver,"maplim=li");
    
    (void) getavar_("maplim", temparr, &error, 6, 80);
    (void) getlvar_("exact_fit", &lvar2, &error, 9);
    if (!error) {
      if(!strcasecmp("cl",temparr) || (lvar && lvar2)) {
	XtVaSetValues(map_forms.clip,
		      XmNmenuHistory, map_forms.clip_window, NULL);
      }
      else {
	XtVaSetValues(map_forms.clip,
		      XmNmenuHistory, map_forms.clip_last, NULL);
      }
    }
    /*
       "Pole Latitude"
       */
    l5=XtVaCreateManagedWidget("Pole Latitude",xmLabelWidgetClass,
			       map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.clip,
			       XmNleftAttachment,
			       XmATTACH_FORM,
			       NULL);
   
    getrvar_("plat", &rvar, &error, 4);
    map_forms.plat = XtVaCreateManagedWidget("Plat",
					     xmTextFieldWidgetClass,
					     map_forms.map_form,
					     XmNcolumns,5,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNtopWidget,map_forms.clip,
					     XmNleftAttachment,
					     XmATTACH_WIDGET,
					     XmNleftWidget,l5,
					     XmNvalue,float_nozero(rvar),
					     NULL);
    XtAddCallback(map_forms.plat,XmNactivateCallback,form_plat_call,NULL);
    XtAddCallback(map_forms.plat,XmNactivateCallback,cleanup_box_call,(XtPointer)PLAT);
    XtAddCallback(map_forms.plat,XmNmodifyVerifyCallback,check_num,(XtPointer)PLAT);
    XtAddCallback(map_forms.plat,XmNmotionVerifyCallback,text_box_motion,(XtPointer)PLAT);
    XtAddEventHandler(map_forms.plat, ButtonPressMask, FALSE,
		      check_default_handler, 0);
    
    /*    
       "Pole Longitude"
       */
    l6=XtVaCreateManagedWidget("Pole Longitude",xmLabelWidgetClass,
			       map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.plat,
			       XmNleftAttachment,
			       XmATTACH_FORM,
			       NULL);
    

    getrvar_("plon", &rvar, &error, 4);
    map_forms.plon = XtVaCreateManagedWidget("Plon",
					     xmTextFieldWidgetClass,
					     map_forms.map_form, 
					     XmNcolumns,5,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNtopWidget,map_forms.plat,
					     XmNleftAttachment,
					     XmATTACH_WIDGET,
					     XmNleftWidget,l6,
					     XmNvalue,float_nozero(rvar),
					     NULL);

    XtAddCallback(map_forms.plon,XmNactivateCallback,form_plon_call,NULL);
    XtAddCallback(map_forms.plon,XmNactivateCallback,cleanup_box_call,(XtPointer)PLON);
    XtAddCallback(map_forms.plon,XmNmodifyVerifyCallback,check_num,(XtPointer)PLON);
    XtAddCallback(map_forms.plon,XmNmotionVerifyCallback,text_box_motion,(XtPointer)PLON);
    XtAddEventHandler(map_forms.plon, ButtonPressMask, FALSE,
		      check_default_handler, 0);
    /*
       "Rotation Angle"
       */
    l7=XtVaCreateManagedWidget("Rotation Angle",xmLabelWidgetClass,
			    map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.plon,
			       XmNleftAttachment,
			       XmATTACH_FORM,
			       NULL);
    
    getrvar_("rota", &rvar, &error, 4);
    map_forms.rota = XtVaCreateManagedWidget("Rota",
					     xmTextFieldWidgetClass,
					     map_forms.map_form, 
					     XmNcolumns,5,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNtopWidget,map_forms.plon,
					     XmNleftAttachment,
					     XmATTACH_WIDGET,
					     XmNleftWidget,l7,
					     XmNvalue,float_nozero(rvar),
					     NULL);

    XtAddCallback(map_forms.rota,XmNactivateCallback,form_rota_call,NULL);
    XtAddCallback(map_forms.rota,XmNactivateCallback,cleanup_box_call,(XtPointer)ROTA);
    XtAddCallback(map_forms.rota,XmNmodifyVerifyCallback,check_num,(XtPointer)ROTA);
    XtAddCallback(map_forms.rota,XmNmotionVerifyCallback,text_box_motion,(XtPointer)ROTA);
    XtAddEventHandler(map_forms.rota, ButtonPressMask, FALSE,
		      check_default_handler, 0);
    /*
       "Grid Line Spacing"
       */
    l8=XtVaCreateManagedWidget("Grid Line Spacing",xmLabelWidgetClass,
			       map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.rota,
			       XmNleftAttachment,
			       XmATTACH_FORM,
			       NULL);
    
    getrvar_("grdspa", &rvar, &error, 6);
    map_forms.glspace = XtVaCreateManagedWidget("Rota",
						xmTextFieldWidgetClass,
						map_forms.map_form, 
						XmNcolumns,5,
						XmNtopAttachment,
						XmATTACH_WIDGET,
						XmNtopWidget,map_forms.rota,
						XmNleftAttachment,
						XmATTACH_WIDGET,
						XmNleftWidget,l8,
						XmNvalue,float_nozero(rvar),
						NULL);

    XtAddCallback(map_forms.glspace,XmNactivateCallback,
		  form_glspace_call,NULL);
    XtAddCallback(map_forms.glspace,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)GLSPACE);
    XtAddCallback(map_forms.glspace,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)GLSPACE);
    XtAddCallback(map_forms.glspace,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)GLSPACE);
    XtAddEventHandler(map_forms.glspace, ButtonPressMask, FALSE,
		      check_default_handler, 0);
    /*
       Map projection menu
       */
    l9=XtVaCreateManagedWidget("Map Projection",xmLabelWidgetClass,
			       map_forms.map_form, 
			       XmNtopAttachment,
			       XmATTACH_WIDGET,
			       XmNtopWidget,map_forms.glspace,
			       XmNleftAttachment,
			       XmATTACH_WIDGET,
			       NULL);

    map_forms.proj_menu=XmCreatePulldownMenu(map_forms.map_form,"_pulldown",
					     NULL,0);

    map_forms.proj = XmCreateOptionMenu(map_forms.map_form,"mapproj",NULL,0);
    XtVaSetValues(map_forms.proj,
		  XmNtopAttachment,
		  XmATTACH_WIDGET,
		  XmNtopWidget,l9,
		  XmNleftAttachment,
		  XmATTACH_FORM,
		  XmNsubMenuId, map_forms.proj_menu,
		  NULL);
    
    XtUnmanageChild(XmOptionLabelGadget(map_forms.proj));

    XtManageChild(map_forms.proj);

    map_forms.proj_nhst = XtVaCreateManagedWidget("N. Hemisphere Stereographic",
						xmPushButtonWidgetClass,
						map_forms.proj_menu,
						  XmNmarginWidth,0,
						  NULL);
    map_forms.proj_shst = XtVaCreateManagedWidget("S. Hemisphere Stereographic",
						xmPushButtonWidgetClass,
						map_forms.proj_menu,
						  XmNmarginWidth,0,
						  NULL);
    map_forms.proj_nhor = XtVaCreateManagedWidget("N. Hemisphere Orthographic",
						xmPushButtonWidgetClass,
						map_forms.proj_menu,
						  XmNmarginWidth,0,
						  NULL);
    map_forms.proj_shor = XtVaCreateManagedWidget("S. Hemisphere Orthographic",
						xmPushButtonWidgetClass,
						map_forms.proj_menu,
						  XmNmarginWidth,0,
						  NULL);
    map_forms.proj_me = XtVaCreateManagedWidget("Mercator",
					      xmPushButtonWidgetClass,
					      map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    map_forms.proj_ce = XtVaCreateManagedWidget("Cylindrical Equidistant",
					      xmPushButtonWidgetClass,
					      map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    map_forms.proj_mo = XtVaCreateManagedWidget("Mollewide",
					      xmPushButtonWidgetClass,
					      map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    XtVaCreateManagedWidget("-OTHERS-",xmLabelWidgetClass,
                          map_forms.proj_menu,
			    XmNmarginWidth,0,
			    NULL);
    
    map_forms.proj_lc = XtVaCreateManagedWidget("LC",xmPushButtonWidgetClass,
					      map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    
    map_forms.proj_st = XtVaCreateManagedWidget("ST",xmPushButtonWidgetClass
					      ,map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    
    map_forms.proj_or = XtVaCreateManagedWidget("OR",xmPushButtonWidgetClass
					      ,map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    
    map_forms.proj_le = XtVaCreateManagedWidget("LE",xmPushButtonWidgetClass
					      ,map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    
    map_forms.proj_gn = XtVaCreateManagedWidget("GN",xmPushButtonWidgetClass
					      ,map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    
    map_forms.proj_ae = XtVaCreateManagedWidget("AE",xmPushButtonWidgetClass
					      ,map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
    
    map_forms.proj_sv = XtVaCreateManagedWidget("SV",xmPushButtonWidgetClass
					      ,map_forms.proj_menu,
						XmNmarginWidth,0,
						NULL);
   
    XtAddCallback(map_forms.proj_nhst,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=nhst");
    XtAddCallback(map_forms.proj_shst,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=shst");
    XtAddCallback(map_forms.proj_nhor,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=nhor");
    XtAddCallback(map_forms.proj_shor,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=shor");
    
    XtAddCallback(map_forms.proj_lc,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=lc");
    XtAddCallback(map_forms.proj_st,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=st");
    XtAddCallback(map_forms.proj_or,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=or");
    XtAddCallback(map_forms.proj_le,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=le");
    XtAddCallback(map_forms.proj_gn,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=gn");
    XtAddCallback(map_forms.proj_ae,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=ae");
    XtAddCallback(map_forms.proj_sv,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=sv");
    XtAddCallback(map_forms.proj_ce,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=ce");
    XtAddCallback(map_forms.proj_me,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=me");
    XtAddCallback(map_forms.proj_mo,XmNactivateCallback,
		  call_driver,(XtPointer)"maproj=mo");
    /*
       Set initial value for map projection menu
       */
    (void)getavar_ ("proj", temparr, &error, 4, 80);
    if(!error){
	if(!strcasecmp("nhst",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_nhst,
			  NULL);
	}
	else if(!strcasecmp("shst",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_shst,
			      NULL);
	}
	
	else if(!strcasecmp("nhor",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_nhor,
			  NULL);
	}
	
	else if(!strcasecmp("shor",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_shor,
			  NULL);
	}
	
	else if(!strcasecmp("me",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_me,
			  NULL);
	}
	
	else if(!strcasecmp("ce",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_ce,
			  NULL);
	}
	
	else if(!strcasecmp("mo",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_mo,
			  NULL);
	}
	
	else if(!strcasecmp("lc",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_lc,
			      NULL);
	}
	
	else if(!strcasecmp("st",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_st,
			  NULL);
	}
	
	else if(!strcasecmp("or",temparr)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_or,
			      NULL);
	    }
	
	else if(!strcasecmp("le",temparr)){
	    XtVaSetValues(map_forms.proj,
			  XmNmenuHistory, map_forms.proj_le,
			      NULL);
	}
	
	    else if(!strcasecmp("gn",temparr)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_gn,
			      NULL);
	    }
	
	    else if(!strcasecmp("ae",temparr)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_ae,
			      NULL);
	    }
	    
	    else if(!strcasecmp("sv",temparr)){
		XtVaSetValues(map_forms.proj,
			      XmNmenuHistory, map_forms.proj_sv,
			      NULL);
	    }
    }
    XtAddCallback(map_forms.map_form, XmNhelpCallback,
                  check_help_call,NULL);
    
    XtManageChild(popup);
}
