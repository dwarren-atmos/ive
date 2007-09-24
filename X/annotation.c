/****************************************************************************
 * $Log: annotation.c,v $
 * Revision 1.8  2006/04/24 21:36:00  warren
 * Added a scroll bar to the fields area, fixed the background form so that
 * you can see everything with the new motif, fixed the call to getlvar in
 * annotation.c to look for color_bar instead of color.
 *
 * Revision 1.7  1998/09/04 20:51:38  harry
 * Add colorbar button to annotation widget.  Rewrite file widgets to
 * remember previous entries.
 *
 * Revision 1.6  1997/04/11 18:46:38  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.5  1995/10/19 22:13:05  harry
 * Set defaults by clicking right mouse button.  Make all TextWidgets into
 * TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.4  1995/07/10  17:15:57  warren
 * Finished renaming widgets so the help key will work. Also fixed the color
 * menus not to contain an _ in the boxes and fixed the second vector
 * componant update proble.
 *
 * Revision 1.3  1995/05/05  22:09:05  harry
 * Take out uses of "str" after it has been freed.
 *
 * Revision 1.2  1994/12/29  19:57:31  warren
 * New gks flush routine and fixed alpha missing label bug
 *
 * Revision 1.1  1994/10/08  18:59:49  warren
 * The annotation widget and updating it.
 *
 ****************************************************************************/

/************
Callbacks can be found in form_callbacks
************/

#include <Xm/XmAll.h>
#include <X11/Intrinsic.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>

static char ident[] = "$Id: annotation.c,v 1.8 2006/04/24 21:36:00 warren Exp $";

extern void getrvar_();
extern void getlvar_();
extern XmString NewString();

extern void form_label2_call();
extern void form_colorbar_call();
extern void tic_labels_call();
extern void tic_scale_call();
extern void tic_decimals_call();
extern void tic_widthmaj_call();
extern void tic_widthmin_call();
extern Widget Box;
extern void getivar_();
/**********
   This structure contains all the annotation widgets.
   **********/
struct {
    Widget form;
    Widget ticlab;
    Widget tic_labels, tblab;
    Widget tic_labels_off, tic_labels_norm,tic_labels_bold;
    Widget tic_scale, tslab;
    Widget tic_widthmaj, twlabmaj, tic_widthmin, twlabmin;
    Widget tic_widthmaj_thin, tic_widthmaj_thick, tic_widthmaj_thickest;
    Widget tic_widthmin_thin, tic_widthmin_thick, tic_widthmin_thickest;
    Widget tic_decimals, tdlab;
    Widget generallab, sep;
    Widget label, colorlab, colorbar;
}Annotation={NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	     NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, NULL, NULL};

void do_annotation(w,data,call)
     Widget w;              /*This is due to their being called from         */
     caddr_t data;          /*additional places                              */
     XmAnyCallbackStruct *call;
{
    Widget popup;
    XmString one, two, three, str;
    int lvar, error;
    float rvar;
    char buff[20];

    if(Annotation.form){
	if(XtIsManaged(XtParent(Annotation.form)))
	  XtUnmanageChild(XtParent(Annotation.form));
	else
	  XtManageChild(XtParent(Annotation.form));
	return;
    }
    
    /*make form but don't manage*/
    
    popup = XtVaCreatePopupShell("Annotation",
                                 transientShellWidgetClass,
                                 XtParent(Box) ,
                                 XmNautoUnmanage,FALSE,
                                 XmNy, 
                                 HeightOfScreen(XtScreen(w))-(305),
				 XmNx,400,
                                 NULL);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
                  &Annotation.form);

    str = NewString("Annotation");
    Annotation.form = XtVaCreateManagedWidget("Annotation",
					      xmFormWidgetClass,
					      popup,
					      XmNlabelString, str,
					      XmNwidth,265,
					      XmNheight, 305,
					      NULL);
    XmStringFree(str);

    str = NewString("Ticmark Labels");
    Annotation.ticlab = XtVaCreateManagedWidget("Tcontrol",
						xmLabelWidgetClass,
						Annotation.form,
						XmNtopAttachment,
						XmATTACH_FORM,
						XmNleftAttachment, 
						XmATTACH_FORM,
						XmNrightAttachment,
						XmATTACH_FORM,
						XmNleftOffset, 5,
						XmNrightOffset, 5,
						XmNlabelString, str,
						NULL);
    XmStringFree(str);
    
    /**********Tic Bolding**********/
    str = NewString("Style");
    Annotation.tblab = XtVaCreateManagedWidget("Tlabels",
					       xmLabelWidgetClass,
					       Annotation.form,
					       XmNtopAttachment,
					       XmATTACH_WIDGET,
					       XmNleftAttachment, 
					       XmATTACH_FORM,
					       XmNtopWidget,
					       Annotation.ticlab,
					       XmNtopOffset, 5,
					       XmNlabelString, str,
					       NULL);
    XmStringFree(str);
    
    (void)getivar_("tic_labels", &lvar, &error, 10);    

    one = NewString("Off");
    two = NewString("Normal");
    three = NewString("Bold");
    Annotation.tic_labels = XmCreateRadioBox(Annotation.form,
					     "Tic_Labels", 
					     NULL,0);
    XtVaSetValues(Annotation.tic_labels, XmNtopAttachment,
		  XmATTACH_WIDGET, XmNleftAttachment, XmATTACH_FORM,
		  XmNtopWidget, Annotation.ticlab,
		  XmNtopOffset, 5,  XmNleftOffset, 35,
		  XmNorientation, XmHORIZONTAL,
		  NULL);

    XtManageChild(Annotation.tic_labels);
    Annotation.tic_labels_off = XtVaCreateManagedWidget("Off", 
						     xmToggleButtonWidgetClass,
							Annotation.tic_labels,
							XmNlabelString, one,
							XmNset,
							(lvar==2)?TRUE:FALSE,
							NULL);
    Annotation.tic_labels_norm = XtVaCreateManagedWidget("Normal", 
						     xmToggleButtonWidgetClass,
							 Annotation.tic_labels,
							 XmNlabelString, two,
							 XmNset,
							 (lvar==0)?TRUE:FALSE,
							 NULL);
    Annotation.tic_labels_bold = XtVaCreateManagedWidget("Bold", 
						     xmToggleButtonWidgetClass,
							 Annotation.tic_labels,
							 XmNlabelString, three,
							 XmNset,
							 (lvar==1)?TRUE:FALSE,
							 NULL);

    XtAddCallback(Annotation.tic_labels_off,XmNvalueChangedCallback,
		  tic_labels_call,(XtPointer)0);
    XtAddCallback(Annotation.tic_labels_norm,XmNvalueChangedCallback,
		  tic_labels_call,(XtPointer)1);
    XtAddCallback(Annotation.tic_labels_bold,XmNvalueChangedCallback,
		  tic_labels_call,(XtPointer)2);

    XtVaSetValues( Annotation.tic_labels_off, XmNlabelString, one,NULL);
	
    XmStringFree(one);
    XmStringFree(two);
    XmStringFree(three);

    /**********Tic Scaling**********/
    (void)getrvar_("tic_scale", &rvar, &error, 9);
    if(rvar == -1)
      strcpy(buff,"Default");
    else
      sprintf(buff, "%2g",rvar);
    str = NewString("Label Size");
    Annotation.tslab = XtVaCreateManagedWidget("Tscale",
					       xmLabelWidgetClass,
					       Annotation.form,
					       XmNtopAttachment,
					       XmATTACH_WIDGET,
					       XmNleftAttachment, 
					       XmATTACH_FORM,
					       XmNtopWidget,
					       Annotation.tic_labels,
					       XmNtopOffset, 5,
					       XmNlabelString, str,
					       NULL);
    XmStringFree(str);
    
    Annotation.tic_scale = XtVaCreateManagedWidget("Tic_scale",
						   xmTextFieldWidgetClass,
						   Annotation.form,
						   XmNtopAttachment,
						   XmATTACH_WIDGET,
						   XmNleftAttachment,
						   XmATTACH_FORM,
						   XmNlabelString, str,
						   XmNtopWidget,
						   Annotation.tic_labels,
						   XmNtopOffset, 5,
						   XmNleftOffset, 100,
						   XmNcolumns, 8,
						   XmNvalue,
						   buff,
						   NULL);

    XtAddCallback(Annotation.tic_scale,XmNactivateCallback,
                  tic_scale_call,NULL);
    XtAddCallback(Annotation.tic_scale,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)TSCALE);
    XtAddCallback(Annotation.tic_scale,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)TSCALE);
    XtAddCallback(Annotation.tic_scale,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)TSCALE);
    XtAddEventHandler(Annotation.tic_scale, ButtonPressMask, FALSE,
		      check_default_handler, 0);

    /**********Tic Decimal places**********/
    (void)getivar_("tic_decimals", &lvar, &error, 12);
    if(lvar < 0)
      sprintf(buff, "Default");
    else
      sprintf(buff, "%d",lvar);
    str = NewString("Decimal Places");
    Annotation.tdlab = XtVaCreateManagedWidget("Tdec",
					       xmLabelWidgetClass,
					       Annotation.form,
					       XmNtopAttachment,
					       XmATTACH_WIDGET,
					       XmNleftAttachment, 
					       XmATTACH_FORM,
					       XmNtopWidget,
					       Annotation.tic_scale,
					       XmNtopOffset, 5,
					       XmNlabelString, str,
					       NULL);
    XmStringFree(str);
    
    Annotation.tic_decimals = XtVaCreateManagedWidget("Tic_decimals",
						      xmTextFieldWidgetClass,
						      Annotation.form,
						      XmNtopAttachment,
						      XmATTACH_WIDGET,
						      XmNleftAttachment,
						      XmATTACH_FORM,
						      XmNlabelString, str,
						      XmNtopWidget,
						      Annotation.tic_scale,
						      XmNtopOffset, 5,
						      XmNleftOffset, 100,
						      XmNcolumns, 8,
						      XmNvalue,
						      buff,
						      NULL);
    
    XtAddCallback(Annotation.tic_decimals, XmNactivateCallback,
                  tic_decimals_call,NULL);
    XtAddCallback(Annotation.tic_decimals, XmNactivateCallback,
                  cleanup_box_call,(XtPointer)TMOTION);
    XtAddCallback(Annotation.tic_decimals, XmNmodifyVerifyCallback,
                  check_num,(XtPointer)TMOTION);
    XtAddCallback(Annotation.tic_decimals, XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)TMOTION);
    XtAddEventHandler(Annotation.tic_decimals, ButtonPressMask, FALSE,
		      check_default_handler, 0);

    /**********Tic Width**********/

    (void)getrvar_("tic_width_maj", &rvar, &error, 13);

    str = NewString("Width of Major Tics");
    Annotation.twlabmaj = XtVaCreateManagedWidget("Twid",
					       xmLabelWidgetClass,
					       Annotation.form,
					       XmNtopAttachment,
					       XmATTACH_WIDGET,
					       XmNleftAttachment, 
					       XmATTACH_FORM,
					       XmNtopWidget,
					       Annotation.tic_decimals,
					       XmNtopOffset, 5,
					       XmNlabelString, str,
					       NULL);
    XmStringFree(str);
    
    one = NewString("thin");
    two = NewString("thick");
    three = NewString("thickest");
    Annotation.tic_widthmaj = XmCreateRadioBox(Annotation.form, "Tic_Width", 
					       NULL,0);
    XtVaSetValues(Annotation.tic_widthmaj, XmNtopAttachment, XmATTACH_WIDGET,
		  XmNleftAttachment, XmATTACH_FORM, XmNleftOffset, 30,
		  XmNlabelString, str, XmNtopWidget, Annotation.twlabmaj,  
		  XmNtopOffset, 5, XmNcolumns, 8, XmNorientation, XmHORIZONTAL,
		  NULL);

    Annotation.tic_widthmaj_thin = XtVaCreateManagedWidget("One",
						xmToggleButtonWidgetClass,
						Annotation.tic_widthmaj,
						XmNlabelString, one,
						XmNset,
						((int)rvar==1)?TRUE:FALSE,
						NULL);
    Annotation.tic_widthmaj_thick = XtVaCreateManagedWidget("One",
						 xmToggleButtonWidgetClass,
						 Annotation.tic_widthmaj,
						 XmNlabelString, two,
						 XmNset,(lvar==0)?TRUE:FALSE,
						 NULL);
    Annotation.tic_widthmaj_thickest = XtVaCreateManagedWidget("One", 
						    xmToggleButtonWidgetClass,
						    Annotation.tic_widthmaj,
						    XmNlabelString, three,
						    XmNset,
						    (lvar==1)?TRUE:FALSE,
						    NULL);
    XtAddCallback(Annotation.tic_widthmaj_thin,XmNvalueChangedCallback,
		  tic_widthmaj_call,(XtPointer)0);
    XtAddCallback(Annotation.tic_widthmaj_thick,XmNvalueChangedCallback,
		  tic_widthmaj_call,(XtPointer)1);
    XtAddCallback(Annotation.tic_widthmaj_thickest,XmNvalueChangedCallback,
		  tic_widthmaj_call,(XtPointer)2);
    XtManageChild(Annotation.tic_widthmaj);
    str = NewString("Width of Minor Tics ");

    (void)getrvar_("tic_width_min", &rvar, &error, 13);
    Annotation.twlabmin = XtVaCreateManagedWidget("Twid",
						  xmLabelWidgetClass,
						  Annotation.form,
						  XmNtopAttachment,
						  XmATTACH_WIDGET,
						  XmNleftAttachment, 
						  XmATTACH_FORM,
						  XmNtopWidget,
						  Annotation.tic_widthmaj,
						  XmNtopOffset, 5,
						  XmNlabelString, str,
						  NULL);
    XmStringFree(str);
    
    
    Annotation.tic_widthmin = XmCreateRadioBox(Annotation.form,
					       "Tic_Width", NULL,0);

    XtVaSetValues(Annotation.tic_widthmin, XmNtopAttachment, XmATTACH_WIDGET,
		  XmNleftAttachment, XmATTACH_FORM, XmNleftOffset, 30,
		  XmNlabelString, str, XmNtopWidget, Annotation.twlabmin,
		  XmNtopOffset, 5, XmNcolumns, 8, XmNorientation, XmHORIZONTAL,
		  NULL);
    Annotation.tic_widthmin_thin = XtVaCreateManagedWidget("One",
						xmToggleButtonWidgetClass,
						Annotation.tic_widthmin,
						XmNlabelString, one,
						XmNset,
						((int)rvar==1)?TRUE:FALSE,
						NULL);
    Annotation.tic_widthmin_thick = XtVaCreateManagedWidget("One",
						 xmToggleButtonWidgetClass,
						 Annotation.tic_widthmin,
						 XmNlabelString, two,
						 XmNset,
						 ((int)rvar==2)?TRUE:FALSE,
						 NULL);
    Annotation.tic_widthmin_thickest = XtVaCreateManagedWidget("One", 
						    xmToggleButtonWidgetClass,
						    Annotation.tic_widthmin,
						    XmNlabelString, three,
						    XmNset,
						    ((int)rvar==3)?TRUE:FALSE,
						    NULL);
    XtAddCallback(Annotation.tic_widthmin_thin,XmNvalueChangedCallback,
		  tic_widthmin_call,(XtPointer)0);
    XtAddCallback(Annotation.tic_widthmin_thick,XmNvalueChangedCallback,
		  tic_widthmin_call,(XtPointer)1);
    XtAddCallback(Annotation.tic_widthmin_thickest,XmNvalueChangedCallback,
		  tic_widthmin_call,(XtPointer)2);


    XtManageChild(Annotation.tic_widthmin);
    XmStringFree(one);
    XmStringFree(two);
    XmStringFree(three);
    
    /********** line across window **********/
    
    Annotation.sep = XtVaCreateManagedWidget("Sep",
					     xmSeparatorWidgetClass,
					     Annotation.form,
					     XmNtopAttachment,
					     XmATTACH_WIDGET,
					     XmNleftAttachment,
					     XmATTACH_FORM,
					     XmNrightAttachment,
					     XmATTACH_FORM,
					     XmNtopWidget,
					     Annotation.tic_widthmin,
					     XmNtopOffset, 5,
					     NULL
					     );
    str = NewString("Colorbar");
    Annotation.colorlab = XtVaCreateManagedWidget("Ccontrol",
						    xmLabelWidgetClass,
						    Annotation.form,
						    XmNtopAttachment,
						    XmATTACH_WIDGET,
						    XmNleftAttachment, 
						    XmATTACH_FORM,
						    XmNlabelString, str,
						    XmNtopWidget,
						    Annotation.sep,
						    XmNtopOffset, 5,
						    XmNleftOffset, 5,
						    XmNrightOffset, 5,
						    NULL);
    XmStringFree(str);

    str = NewString("Other Text / Labels");
    Annotation.generallab = XtVaCreateManagedWidget("Gcontrol",
						    xmLabelWidgetClass,
						    Annotation.form,
						    XmNtopAttachment,
						    XmATTACH_WIDGET,
						    XmNrightAttachment,
						    XmATTACH_FORM,
						    XmNlabelString, str,
						    XmNtopWidget,
						    Annotation.sep,
						    XmNtopOffset, 5,
						    XmNleftOffset, 5,
						    XmNrightOffset, 5,
						    NULL);
    XmStringFree(str);

    /**********Label Plot**********/
    getlvar_("color_bar",&lvar,&error,9);
    if(lvar)
      str = NewString("On ");
    else
      str = NewString("Off");
    Annotation.colorbar = XtVaCreateManagedWidget("Color_bar",
					       xmPushButtonWidgetClass,
					       Annotation.form,
					       XmNtopAttachment,
					       XmATTACH_WIDGET,
					       XmNleftAttachment,
					       XmATTACH_FORM,
					       XmNleftOffset, 30,
					       XmNtopOffset, 5,
					       XmNlabelString, str,
					       XmNtopWidget,
					       Annotation.colorlab,
					       NULL);
    XmStringFree(str);

    XtAddCallback(Annotation.colorbar,XmNactivateCallback,
                  form_colorbar_call,NULL);
      

    XtAddCallback(Annotation.colorbar, XmNhelpCallback,
		  check_help_call,NULL);

    getlvar_("label",&lvar,&error,5);
    if(lvar)
      str = NewString("On ");
    else
      str = NewString("Off");
    Annotation.label = XtVaCreateManagedWidget("Label",
					       xmPushButtonWidgetClass,
					       Annotation.form,
					       XmNtopAttachment,
					       XmATTACH_WIDGET,
					       XmNrightAttachment,
					       XmATTACH_FORM,
					       XmNrightOffset, 30,
					       XmNtopOffset, 5,
					       XmNlabelString, str,
					       XmNtopWidget,
					       Annotation.generallab,
					       NULL);
    XmStringFree(str);

    XtAddCallback(Annotation.label,XmNactivateCallback,
                  form_label2_call,NULL);
      

    XtAddCallback(Annotation.form, XmNhelpCallback,
		  check_help_call,NULL);

      XtManageChild(popup);
}
