/****************************************************************************
 * $Log: pattern_form.c,v $
 * Revision 1.5  1997/04/11 18:46:44  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.4  1996/10/11 01:12:07  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 * Revision 1.3  1996/01/12 16:11:02  warren
 * Transfer of new pattern stuff from 3.x and beginning of color fixes
 * (color vectors, correct collors on the pulldowan menus)
 *
 * Revision 1.2  1995/12/15 22:32:43  warren
 * moved patterns into 4.0 and implemented the list of fixes and changes from
 * Curtis.
 *
 * Revision 1.1.2.1  1995/12/04 21:06:27  harry
 * Add pattern_form.c to IVE.
 *
 *****************************************************************************/
static char ident[] = "$Id: pattern_form.c,v 1.5 1997/04/11 18:46:44 warren Exp $";
#include <Xm/XmAll.h>
#include <X11/Intrinsic.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>
XmString NewString();

struct{
    Widget form, lablab, slab, elab;
    Widget label1, start1, end1;
    Widget label2, start2, end2;
    Widget label3, start3, end3;
    Widget label4, start4, end4;
    Widget label5, start5, end5;
/*    Widget label6, start6, end6;*/
}stipple;


void do_stipple_(i)
    int *i;
{
    extern Widget Box;
    extern void set_pat_callback();
    extern float getrarr_();
    Widget popup;
    XmString str;
    float pats[2];
    int error, dims;
    char buff[40];
    
    if(*i)
	if(stipple.form != (Widget)NULL){
	    if(XtIsManaged(XtParent(stipple.form)))
		return;
	    XtManageChild(XtParent(stipple.form));
	}
    if(!*i)
	if(stipple.form != (Widget)NULL){
	    XtDestroyWidget(XtParent(stipple.form));
	    return;
	}
    
    buff[0] = '\0';
    error=0;
    dims=2;

      
    popup = XtVaCreatePopupShell("Shading",
                                 transientShellWidgetClass,
                                 Box,
                                 XmNheight, 210,
                                 XmNwidth, 200,
                                 XmNautoUnmanage,FALSE,
                                 XmNx, 410,
                                 XmNy, HeightOfScreen(XtScreen(Box))-(215),
                                 XtNallowShellResize,TRUE,NULL);
        
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
                  &stipple.form);
    XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
                  STIPPLE_TEXT_BOXES);
   

    stipple.form = XtVaCreateManagedWidget("Shading",
                                         xmFormWidgetClass,
                                         popup,
                                         XmNheight, 120,
                                         XmNwidth, 175,
                                         XmNresizable, TRUE,
                                         XmNautoUnmanage,FALSE,
                                         NULL);
    XtAddCallback(stipple.form,XmNdestroyCallback,done_parent_callback,NULL);

    str=NewString("Shading\nLevel");
    stipple.lablab = XtVaCreateManagedWidget("Shading",
					     xmLabelWidgetClass,
					     stipple.form,
					     XmNlabelString,str,
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNleftAttachment, XmATTACH_FORM,
					     NULL);
    XmStringFree(str);
    str=NewString("Start\nValue");
    stipple.slab = XtVaCreateManagedWidget("Start value",
					   xmLabelWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget,stipple.lablab,
					   XmNleftOffset,10,
					   XmNlabelString,str,
					   NULL);
    XmStringFree(str);
    str=NewString("End\nValue");
    stipple.elab = XtVaCreateManagedWidget("End value",
					   xmLabelWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget,stipple.slab,
					   XmNleftOffset,30,
					   XmNlabelString,str,
					   NULL);
    XmStringFree(str);
    
    /*pat1*/
    
    stipple.label1  = XtVaCreateManagedWidget("Light",xmLabelWidgetClass,
					      stipple.form,
					      XmNtopAttachment,XmATTACH_WIDGET,
					      XmNtopWidget,stipple.lablab,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset,10,
					      XmNtopOffset,5,
					      NULL);
    
    getrarr_("pat1", pats, &dims, &error, 4);
    if(!error){
	if(pats[0] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[0]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.start1 = XtVaCreateManagedWidget("Start",xmTextFieldWidgetClass,
					     stipple.form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget,stipple.lablab,
					     XmNleftAttachment,XmATTACH_WIDGET,
					     XmNleftWidget, stipple.lablab,
					     XmNcolumns, 8,
					     XmNleftOffset,10,
					     XmNtopOffset,5,
					     XmNvalue, buff,
					     NULL);
    if(!error){
	if(pats[1] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[1]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.end1 = XtVaCreateManagedWidget("End",xmTextFieldWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,stipple.lablab,
					   XmNleftAttachment,XmATTACH_WIDGET,
					   XmNleftWidget, stipple.slab,
					   XmNcolumns, 8,
					   XmNleftOffset,30,
					   XmNtopOffset,5,
					   XmNvalue, buff,
					   NULL);

    XtAddCallback(stipple.start1,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPSTART1);
    XtAddCallback(stipple.start1,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPSTART1);
    XtAddCallback(stipple.start1,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPSTART1);
    XtAddCallback(stipple.end1,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPEND1);
    XtAddCallback(stipple.end1,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPEND1);
    XtAddCallback(stipple.end1,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPEND1);

    XtAddCallback(stipple.start1,XmNactivateCallback,
                  set_pat_callback,(XtPointer)1);
    XtAddCallback(stipple.end1,XmNactivateCallback,
                  set_pat_callback,(XtPointer)1);
    /*pat2*/
    
    stipple.label2  = XtVaCreateManagedWidget("Medium",xmLabelWidgetClass,
					      stipple.form,
					      XmNtopAttachment,XmATTACH_WIDGET,
					      XmNtopWidget,stipple.start1,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset,10,
					      NULL);
    
    getrarr_("pat2", pats, &dims, &error, 4);
    if(!error){
	if(pats[0] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[0]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.start2 = XtVaCreateManagedWidget("Start",xmTextFieldWidgetClass,
					     stipple.form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget,stipple.start1,
					     XmNleftAttachment,XmATTACH_WIDGET,
					     XmNleftWidget, stipple.lablab,
					     XmNcolumns, 8,
					     XmNleftOffset,10,
					     XmNvalue,buff,
					     NULL);

    if(!error){
	if(pats[1] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[1]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.end2 = XtVaCreateManagedWidget("End",xmTextFieldWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,stipple.start1,
					   XmNleftAttachment,XmATTACH_WIDGET,
					   XmNleftWidget, stipple.slab,
					   XmNcolumns, 8,
					   XmNleftOffset,30,
					   XmNvalue, buff,
					   NULL);
    
    XtAddCallback(stipple.start2,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPSTART2);
    XtAddCallback(stipple.start2,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPSTART2);
    XtAddCallback(stipple.start2,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPSTART2);
    XtAddCallback(stipple.end2,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPEND2);
    XtAddCallback(stipple.end2,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPEND2);
    XtAddCallback(stipple.end2,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPEND2);

    XtAddCallback(stipple.start2,XmNactivateCallback,
                  set_pat_callback,(XtPointer)2);
    XtAddCallback(stipple.end2,XmNactivateCallback,
                  set_pat_callback,(XtPointer)2);
    /*pat3*/
    
    stipple.label3  = XtVaCreateManagedWidget("Heavy",xmLabelWidgetClass,
					      stipple.form,
					      XmNtopAttachment,XmATTACH_WIDGET,
					      XmNtopWidget,stipple.start2,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset,10,
					      NULL);
    
    getrarr_("pat3", pats, &dims, &error, 4);
    if(!error){
	if(pats[0] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[0]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.start3 = XtVaCreateManagedWidget("Start",xmTextFieldWidgetClass,
					     stipple.form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget,stipple.start2,
					     XmNleftAttachment,XmATTACH_WIDGET,
					     XmNleftWidget, stipple.lablab,
					     XmNcolumns, 8,
					     XmNleftOffset,10,
					     XmNvalue,buff,
					     NULL);

    if(!error){
	if(pats[1] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[1]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.end3 = XtVaCreateManagedWidget("End",xmTextFieldWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,stipple.start2,
					   XmNleftAttachment,XmATTACH_WIDGET,
					   XmNleftWidget, stipple.slab,
					   XmNcolumns, 8,
					   XmNleftOffset,30,
					   XmNvalue, buff,
					   NULL);
    
    XtAddCallback(stipple.start3,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPSTART3);
    XtAddCallback(stipple.start3,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPSTART3);
    XtAddCallback(stipple.start3,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPSTART3);
    XtAddCallback(stipple.end3,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPEND3);
    XtAddCallback(stipple.end3,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPEND3);
    XtAddCallback(stipple.end3,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPEND3);

    XtAddCallback(stipple.start3,XmNactivateCallback,
                  set_pat_callback,(XtPointer)3);
    XtAddCallback(stipple.end3,XmNactivateCallback,
                  set_pat_callback,(XtPointer)3);
    /*pat4*/
    
    stipple.label4  = XtVaCreateManagedWidget("Dark",xmLabelWidgetClass,
					      stipple.form,
					      XmNtopAttachment,XmATTACH_WIDGET,
					      XmNtopWidget,stipple.start3,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset,10,
					      NULL);
    
    getrarr_("pat4", pats, &dims, &error, 4);
    if(!error){
	if(pats[0] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[0]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.start4 = XtVaCreateManagedWidget("Start",xmTextFieldWidgetClass,
					     stipple.form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget,stipple.start3,
					     XmNleftAttachment,XmATTACH_WIDGET,
					     XmNleftWidget, stipple.lablab,
					     XmNcolumns, 8,
					     XmNleftOffset,10,
					     XmNvalue, buff,
					     NULL);

    if(!error){
	if(pats[1] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[1]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.end4 = XtVaCreateManagedWidget("End",xmTextFieldWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,stipple.start3,
					   XmNleftAttachment,XmATTACH_WIDGET,
					   XmNleftWidget, stipple.slab,
					   XmNcolumns, 8,
					   XmNleftOffset,30,
					   XmNvalue, buff,
					   NULL);

    XtAddCallback(stipple.start4,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPSTART4);
    XtAddCallback(stipple.start4,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPSTART4);
    XtAddCallback(stipple.start4,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPSTART4);
    XtAddCallback(stipple.end4,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPEND4);
    XtAddCallback(stipple.end4,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPEND4);
    XtAddCallback(stipple.end4,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPEND4);


    XtAddCallback(stipple.start4,XmNactivateCallback,
                  set_pat_callback,(XtPointer)4);
    XtAddCallback(stipple.end4,XmNactivateCallback,
                  set_pat_callback,(XtPointer)4);
    /*pat5*/
    
    stipple.label5  = XtVaCreateManagedWidget("Black",xmLabelWidgetClass,
					      stipple.form,
					      XmNtopAttachment,XmATTACH_WIDGET,
					      XmNtopWidget,stipple.start4,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset,10,
					      NULL);
    
    getrarr_("pat5", pats, &dims, &error, 4);
    if(!error){
	if(pats[0] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[0]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.start5 = XtVaCreateManagedWidget("Start",xmTextFieldWidgetClass,
					     stipple.form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget,stipple.start4,
					     XmNleftAttachment,XmATTACH_WIDGET,
					     XmNleftWidget, stipple.lablab,
					     XmNcolumns, 8,
					     XmNleftOffset,10,
					     XmNvalue, buff,
					     NULL);

    if(!error){
	if(pats[1] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[1]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.end5 = XtVaCreateManagedWidget("End",xmTextFieldWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,stipple.start4,
					   XmNleftAttachment,XmATTACH_WIDGET,
					   XmNleftWidget, stipple.slab,
					   XmNcolumns, 8,
					   XmNleftOffset,30,
					   XmNvalue, buff,
					   NULL);

    XtAddCallback(stipple.start5,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPSTART5);
    XtAddCallback(stipple.start5,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPSTART5);
    XtAddCallback(stipple.start5,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPSTART5);
    XtAddCallback(stipple.end5,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPEND5);
    XtAddCallback(stipple.end5,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPEND5);
    XtAddCallback(stipple.end5,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPEND5);

    XtAddCallback(stipple.start5,XmNactivateCallback,
                  set_pat_callback,(XtPointer)5);
    XtAddCallback(stipple.end5,XmNactivateCallback,
                  set_pat_callback,(XtPointer)5);

    /*pat6*/
    
/*    stipple.label6  = XtVaCreateManagedWidget("Six",xmLabelWidgetClass,
					      stipple.form,
					      XmNtopAttachment,XmATTACH_WIDGET,
					      XmNtopWidget,stipple.start5,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset,10,
					      NULL);
    
    getrarr_("pat6", pats, &dims, &error, 4);
    if(!error){
	if(pats[0] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[0]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.start6 = XtVaCreateManagedWidget("Start",xmTextFieldWidgetClass,
					     stipple.form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget,stipple.start5,
					     XmNleftAttachment,XmATTACH_WIDGET,
					     XmNleftWidget, stipple.lablab,
					     XmNcolumns, 8,
					     XmNleftOffset,10,
					     XmNvalue, buff,
					     NULL);

    if(!error){
	if(pats[1] > 1E+37)
	    sprintf(buff,"default");
	else{
	    sprintf(buff,"%g",pats[1]);
	}
    }
    else
	sprintf(buff,"Error in value");
    stipple.end6 = XtVaCreateManagedWidget("End",xmTextFieldWidgetClass,
					   stipple.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,stipple.start5,
					   XmNleftAttachment,XmATTACH_WIDGET,
					   XmNleftWidget, stipple.slab,
					   XmNcolumns, 8,
					   XmNleftOffset,30,
					   XmNvalue, buff,
					   NULL);

    XtAddCallback(stipple.start6,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPSTART6);
    XtAddCallback(stipple.start6,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPSTART6);
    XtAddCallback(stipple.start6,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPSTART6);
    XtAddCallback(stipple.end6,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)STIPEND6);
    XtAddCallback(stipple.end6,XmNmodifyVerifyCallback,
                  check_num,(XtPointer)STIPEND6);
    XtAddCallback(stipple.end6,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)STIPEND6);

    XtAddCallback(stipple.start6,XmNactivateCallback,
                  set_pat_callback,(XtPointer)6);
    XtAddCallback(stipple.end6,XmNactivateCallback,
                  set_pat_callback,(XtPointer)6);
*/
    XtManageChild(popup);
}
