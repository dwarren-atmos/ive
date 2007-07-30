/****************************************************************************
 * $Id: terrain_form.c,v 1.8 2006/03/03 18:17:26 warren Exp $
 * $Log: terrain_form.c,v $
 * Revision 1.8  2006/03/03 18:17:26  warren
 * fixed newstring to not use strtok due to "NEVER USE THIS FUNCTION" warning in Linux manpage
 *
 * Revision 1.7  2002/12/26 23:30:43  warren
 * Fixed form so that it works with current MOTIF
 *
 * Revision 1.6  2002/08/09 19:57:28  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.5  2002/06/14 19:43:51  harryive
 * Allow specification of terrain contours by value.
 *
 * Revision 1.4  1997/04/11 18:46:45  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.3  1996/05/23 23:18:22  harry
 * Change terrain form so it properly resizes.
 *
 * Revision 1.2  1996/04/23 18:16:59  warren
 * Changes for terrain
 *
 * Revision 1.1  1996/04/01 20:23:13  warren
 * Terrain form and new terrain command (on / off)
 *
 *
 ****************************************************************************/
static char ident[] = "$Id: terrain_form.c,v 1.8 2006/03/03 18:17:26 warren Exp $";

#include <Xm/XmAll.h>
#include <X11/Intrinsic.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>

void getrvar_();
void getlvar_();
void getivar_();
XmString NewString();
/*
   This structure contains all the map widgets.
   */
struct{
    Widget terrain_form;
    Widget tcint,labbutton, show_ter,clab1,clab2,cont_row;
    Widget linelab1,linelab2,numterr,cont_values,cont_int,cont_val;
}terrain_forms={NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		NULL,NULL,NULL,NULL,NULL};

void do_terrain(w,data,call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Widget popup, tertemp;
    XmString str;
    float rvar;
    int ivar;
    long lvar, error;    
    char buff[256];

    if (terrain_forms.terrain_form){
	/*
	   If the widget already exists, make it visible if it isn't and
	   vice versa.
	   */
        if(XtIsManaged(XtParent(terrain_forms.terrain_form)))
           XtUnmanageChild(XtParent(terrain_forms.terrain_form));
        else
          XtManageChild(XtParent(terrain_forms.terrain_form));
        return;
    }
    /*
       Create the widget.
       */
    popup = XtVaCreatePopupShell("Terrain Options",
				 transientShellWidgetClass,
				 XtParent(Box),
				 XmNautoUnmanage,FALSE,
				 XmNx, 0,
				 XmNy, 
				 HeightOfScreen(XtScreen(w))-(215 + 260),
				 XtNallowShellResize,TRUE,
				 NULL);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &terrain_forms.terrain_form);
/*    XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
                  TERRAIN_TEXT_BOXES);*/
/*
   Extra Form Widget needed by Motif.
   */

    tertemp = XtVaCreateManagedWidget("Terrain Options",
				      xmFormWidgetClass,
				      popup,
				      XmNresizable, TRUE,
				      XmNautoUnmanage, FALSE, NULL);

    terrain_forms.terrain_form = XtVaCreateWidget("Terrain",
						  xmFormWidgetClass,
						  tertemp,
						  XmNmarginWidth, 5,
						  XmNborderWidth, 1,
						  XmNresizable, TRUE,
						  XmNresizePolicy,
						  XmRESIZE_ANY,
						  NULL);
    /*
       "Label Plot"
       */
    getlvar_("label",&lvar,&error,5);
    XtVaCreateManagedWidget("Label Plot",xmLabelWidgetClass,
			    terrain_forms.terrain_form,
			    XmNleftAttachment,XmATTACH_FORM,
			    XmNleftOffset,10,
			    XmNtopAttachment,
			    XmATTACH_FORM,
			    NULL);
    
    if(lvar)
      str = NewString("Yes");
    else
      str = NewString("No ");
    terrain_forms.labbutton = XtVaCreateManagedWidget("Label",
				      xmPushButtonWidgetClass,
				      terrain_forms.terrain_form, 
				      XmNlabelString, str,
				      XmNleftAttachment,XmATTACH_FORM,
						      XmNleftOffset,170,
						      XmNtopAttachment,
						      XmATTACH_FORM,
						      NULL);
    XmStringFree(str);

    XtAddCallback(terrain_forms.labbutton,XmNactivateCallback,
		  form_label_call,NULL);
    

    /*
       "Show terrain"
       */
    getlvar_("label",&lvar,&error,5);
    XtVaCreateManagedWidget("Plot Terrain",xmLabelWidgetClass,
			    terrain_forms.terrain_form,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget, terrain_forms.labbutton,
			    XmNtopOffset, 10,
			    NULL);
    
    if(lvar)
      str = NewString("Yes");
    else
      str = NewString("No ");
    terrain_forms.show_ter = XtVaCreateManagedWidget("ter",
					     xmPushButtonWidgetClass,
					     terrain_forms.terrain_form, 
					     XmNlabelString, str,
					     XmNtopAttachment,XmATTACH_WIDGET,
				     XmNtopWidget, terrain_forms.labbutton,
					     XmNleftAttachment,XmATTACH_FORM,
					     XmNleftOffset,170,
						     NULL);
    XmStringFree(str);

    XtAddCallback(terrain_forms.show_ter,XmNactivateCallback,
		  form_terrain_call,NULL);
    

    /*
       "Terrain Contour Interval"
       */
    /*    terrain_forms.clab1 = 
    XtVaCreateManagedWidget("Contours specified by",xmLabelWidgetClass,
			    terrain_forms.terrain_form,
			    XmNtopAttachment,XmATTACH_WIDGET,
			    XmNtopWidget, terrain_forms.show_ter,
			    XmNtopOffset, 50,
			    NULL);
    */
    str = NewString("Contours");
    terrain_forms.clab1 = 
      XtVaCreateManagedWidget("Tcontours",xmLabelWidgetClass,
			      terrain_forms.terrain_form,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,terrain_forms.show_ter,
			      XmNleftAttachment,XmATTACH_FORM,
			      XmNtopOffset, 5,
			      NULL);
    XmStringFree(str);
    
    str = NewString("specified by");
    terrain_forms.clab2 = 
      XtVaCreateManagedWidget("TSpecBy",xmLabelWidgetClass,
			      terrain_forms.terrain_form,
			      XmNlabelString,str,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,terrain_forms.show_ter,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNleftWidget, terrain_forms.clab1,
			      XmNtopOffset, 5,
			      NULL);
    XmStringFree(str);

    terrain_forms.cont_row = 
      XtVaCreateManagedWidget("Contour_interval", xmRowColumnWidgetClass,
			      terrain_forms.terrain_form,
			      XmNradioBehavior, True,
			      XmNorientation, XmHORIZONTAL,
			      XmNisHomogeneous, True,
			      XmNentryClass,xmToggleButtonWidgetClass,
			      XmNpacking,XmPACK_TIGHT,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNleftAttachment,XmATTACH_WIDGET,
			      XmNtopWidget, terrain_forms.show_ter,
			      XmNleftWidget, terrain_forms.clab2,
			      XmNtopOffset, 5,
			      NULL);

    str = NewString("Interval");
    terrain_forms.cont_int =  
      XtVaCreateManagedWidget("TContour_Interval", 
			      xmToggleButtonWidgetClass,
			      terrain_forms.cont_row,
			      XmNlabelString,str,
			      NULL);
    XmStringFree(str);

    str = NewString("Values");
    terrain_forms.cont_val =  
      XtVaCreateManagedWidget("TContour_values", 
			      xmToggleButtonWidgetClass,
			      terrain_forms.cont_row,
			      XmNlabelString,str,
			      NULL);
    XmStringFree(str);
    
    terrain_forms.cont_values = 
      XtVaCreateManagedWidget("TContour_Values",
			      xmTextFieldWidgetClass,
			      terrain_forms.terrain_form,
			      XmNtopAttachment, 
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      terrain_forms.cont_row,
			      XmNleftAttachment, 
			      XmATTACH_FORM,
			      XmNrightAttachment, 
			      XmATTACH_FORM,
			      XmNleftOffset, 10,
			      XmNrightOffset, 10,
			      NULL);

    XtAddCallback(terrain_forms.cont_values,XmNactivateCallback,
		  form_tcint_call,NULL);
    XtAddCallback(terrain_forms.cont_int,XmNarmCallback,
		  form_tcint_call,(XtPointer)1);
    XtAddCallback(terrain_forms.cont_val,XmNarmCallback,
		  form_tcint_call,(XtPointer)2);
    XtAddCallback(terrain_forms.cont_values,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)TERRCONT);
    XtAddCallback(terrain_forms.cont_values,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)TERRCONT);
    XtAddCallback(terrain_forms.cont_values,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)TERRCONT);
    XtAddEventHandler(terrain_forms.cont_values, ButtonPressMask, FALSE,
			 check_default_handler, 0);

    getivar_("tnum_vals", &ivar, &error, 9);
    if (ivar > 0) {
	int i;
	float rarr[40];

	getrarr_("tcont_values", rarr, &ivar, &error, 12);
	buff[0]='\0';
	if(ivar>0){
	    sprintf(buff,"%g",rarr[0]);
	}
	else {
	    ivar = 1;
	    sprintf(buff,"0");
	    driver("tcontour_values=0");
	}
	for (i=1; i<ivar; i++){
	    sprintf(buff,"%s,%g",buff,rarr[i]);
	}
	XtVaSetValues(terrain_forms.cont_values, XmNvalue, buff, NULL);
	XtVaSetValues(terrain_forms.cont_val, XmNset, TRUE, NULL);
	XtVaSetValues(terrain_forms.cont_int, XmNset, FALSE, NULL);
    }
    else{
	getrvar_("tcint", &rvar, &error, 5);
	sprintf(buff,"%g",rvar);
	XtVaSetValues(terrain_forms.cont_values, XmNvalue, buff, NULL);
	XtVaSetValues(terrain_forms.cont_val, XmNset, FALSE, NULL);
	XtVaSetValues(terrain_forms.cont_int, XmNset, TRUE, NULL);
    }


    /*Number of labled lines*/
    
    getivar_("terr_nulbll",&ivar,&error,11);
    str = NewString("Label every");
    terrain_forms.linelab1 = 
      XtVaCreateManagedWidget("Linelab1",xmLabelWidgetClass,
                              terrain_forms.terrain_form,
                              XmNlabelString,str,
	          XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget, terrain_forms.cont_values,
                              XmNleftAttachment,XmATTACH_FORM,
			      XmNleftOffset,10,
                              NULL);
    XmStringFree(str);
 
    str = NewString("line(s)");
    terrain_forms.linelab2 = 
      XtVaCreateManagedWidget("Linelab2",xmLabelWidgetClass,
                              terrain_forms.terrain_form,
                              XmNlabelString,str,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget, terrain_forms.cont_values,
                              XmNleftAttachment,XmATTACH_WIDGET,
                              XmNleftWidget, terrain_forms.linelab1,
			      XmNleftOffset,95,
                              NULL);
    XmStringFree(str);

    str = NewString("");
     terrain_forms.numterr = 
       XtVaCreateManagedWidget("TNULBLL",xmScaleWidgetClass,
                               terrain_forms.terrain_form,
                               XmNtopAttachment,XmATTACH_WIDGET,
                               XmNtopWidget, terrain_forms.cont_values,
                               XmNrightAttachment,XmATTACH_WIDGET,
                               XmNleftAttachment,XmATTACH_WIDGET,
                               XmNrightWidget, terrain_forms.linelab2,
                               XmNleftWidget, terrain_forms.linelab1,
                               XmNorientation,XmHORIZONTAL,
                               XmNtitleString,str,
                               XmNmaximum,6,
                               XmNminimum,0,
                               XmNshowValue, True,
                               XmNvalue,ivar,
                               NULL);
      
     XtUnmanageChild(XtNameToWidget(terrain_forms.numterr,"Title"));
    XtAddCallback(terrain_forms.numterr, XmNvalueChangedCallback, 
                  form_numterr_call,NULL);
 
    XtAddRawEventHandler(terrain_forms.numterr, ButtonPressMask,FALSE,
                         numterr_scale_type_in,(XtPointer)0);
 
    XtManageChild(popup);
    XtManageChild(terrain_forms.terrain_form);
}










