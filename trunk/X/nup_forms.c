/****************************************************************************
 * $Log: nup_forms.c,v $
 * Revision 1.7  1997/06/27 21:06:28  harry
 * Designed multiplot printing.
 *
 * Revision 1.6  1997/04/11 18:46:43  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.5  1995/08/22 21:45:20  warren
 * make sure we open a log file
 * null outall pointers at the start
 *
 * Revision 1.4  1994/04/11  23:33:06  warren
 * bring nup box near printer menu.
 *
 * Revision 1.3  1994/03/21  18:38:10  warren
 * fixed nondisapearing eps form and wrong number for current in multiprint form.
 *
 * Revision 1.2  1993/10/18  23:06:56  warren
 * updates from 3.0 version and fixed printer forms.
 *
 * Revision 1.1  1993/09/08  23:14:29  warren
 * Print widget code.
 *
 *****************************************************************************/
static char ident[] = "$Id: nup_forms.c,v 1.7 1997/06/27 21:06:28 harry Exp $";

#include <Xm/XmAll.h>
#include <X11/Intrinsic.h>
#include <ive.h>
#include <ive_for.h>
#include <ive_text_enum.h>
#include <ive_macros.h>
XmString NewString();
extern struct {
    int curseg, curoverlay[15], curlist, curplot, segs[8*15], maxplot;
  }segments_;

extern struct {
    Widget form;
    Widget printer_name;
    Widget color_lab;
    Widget color;
    Widget landscape;
    char current[256];
}printer_setup;

struct{
    Widget form;
    Widget two_1, two_2;
    Widget print, landscape;
}twoup={NULL,NULL,NULL,NULL,NULL};
struct{
    Widget form;
    Widget four_1, four_2, four_3, four_4;
    Widget print, landscape;
}fourup={NULL,NULL,NULL,NULL,NULL,NULL,NULL};
struct{
    Widget form;
    Widget six_1, six_2, six_3, six_4, six_5, six_6;
    Widget print, landscape;
}sixup={NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};

static XmString land=NULL, port=NULL;

void print_check_num(w, data, call)
     Widget w;
     int data;
     XmTextVerifyCallbackStruct *call;
{
    int len = 0;
    int form, box, firstchar;
    if(!XtIsRealized(w) || call->event == NULL)return;
    if(data){
	form = (data - data%100)/100;
	box = data%100;
	switch (form){
	  case TWO_TEXT_BOXES:
	    if(!two_text_boxes[box]){
		two_text_boxes[box] = 1;
		swap_fg(w);
		if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	    }
	    break;
	  case FOUR_TEXT_BOXES :
	    if(!four_text_boxes[box]){
		four_text_boxes[box] = 1;
		swap_fg(w);
		if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	    }
	    break;
	  case SIX_TEXT_BOXES:
	    if(!six_text_boxes[box]){
		six_text_boxes[box] = 1;
		swap_fg(w);
		if(call->text->length > 0){
		    XmTextSetString(w,"");
		}
	    }
	    break;
	}
	if (call->startPos < call->currInsert)
	  return; /*hit backspace*/
	if (call->text->length == 0)
	  return; /*forward delete*/
	if(call->text->ptr[0] == 'c' || call->text->ptr[0] == 'C'){
	    XtVaSetValues(w, XmNvalue, "current",NULL);
	    call->doit = FALSE;
	    return;
	}
	else{
	    if((len = XmTextGetLastPosition(w))==10){
		call->doit = FALSE;
		return;
	    }
	    if(len + call->text->length >10){
		call->text->ptr[10-len] = 0;
		call->text->length = strlen(call->text->ptr);
	    }
	    
	    for (len = 0; len < call->text->length; len ++){
	       if(!isdigit(call->text->ptr[len]) && 
		  call->text->ptr[len] != '-'){
		    int i;
		    for(i = len; i< (call->text->length -1); i++)
		      call->text->ptr[i] = call->text->ptr[i+1];
		    call->text->length--;
		    len--;
		}
	    }
	}
    }
}

void printbox_callback(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    char *str, buff[10];
    int i,j;
    XSync(XtDisplay(w),FALSE);
    str = XmTextFieldGetString(w);
    if(!strcmp(str,"current")){
	sprintf(buff,"%d",segments_.curplot);
	XmTextFieldSetString(w,buff);
    }
    else{
	for(i=0; str[i]!='\0',str[i]==' ';i++);
	if(str[i]=='-'){
	    i++;
	    j=atoi(str+i);
	    sprintf(buff,"%d", segments_.curplot - j);
	    XmTextFieldSetString(w,buff);
	}
    }
}

void form_print_callback(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    char *one, *two, *three, *four, *five, *six, buff[256];
    buff[0]='\0';
    one=two=three=four=five=six=NULL;
    switch(*data){
      case '2':
	one = XmTextFieldGetString(twoup.two_1);
	two = XmTextFieldGetString(twoup.two_2);
	sprintf( buff,"print = %s, %s", one, two);
	break;
      case '4':
	one =   XmTextFieldGetString(fourup.four_1);
	two =   XmTextFieldGetString(fourup.four_2);
	three = XmTextFieldGetString(fourup.four_3);
	four =  XmTextFieldGetString(fourup.four_4);
	sprintf(buff,"print = %s, %s, %s, %s", one, two, three, four);
	break;
      case '6':	
	one =   XmTextFieldGetString(sixup.six_1);
	two =   XmTextFieldGetString(sixup.six_2);
	three = XmTextFieldGetString(sixup.six_3);
	four =  XmTextFieldGetString(sixup.six_4);
	five =  XmTextFieldGetString(sixup.six_5);
	six =   XmTextFieldGetString(sixup.six_6);
	sprintf(buff,"print = %s, %s, %s, %s, %s, %s", 
		one, two, three, four, five, six);
	break;
    }
    if(buff[0] != '\0')driver(buff);
    
    if(one != NULL)free(one);
    if(two != NULL)free(two);
    if(three != NULL)free(three);
    if(four != NULL)free(four);
    if(five != NULL)free(five);
    if(six != NULL)free(six);

    XtDestroyWidget(XtParent(XtParent(w)));
}

void set_landscape_widgets(landscape)
    int landscape;

{
    XmString str;

    if (land == NULL) land = NewString("Landscape");
    if (port == NULL) port = NewString("Portrait");
    if (landscape) str = land;
    else str = port;
    if (printer_setup.form)
	XtVaSetValues(printer_setup.landscape, XmNlabelString, str, NULL);
    if (twoup.form) {
	XtVaSetValues(twoup.landscape, XmNlabelString, str, NULL);
	XtVaSetValues(twoup.two_2, XmNtopAttachment,
		      landscape?XmATTACH_FORM:XmATTACH_WIDGET,
		      XmNleftAttachment,
		      landscape?XmATTACH_WIDGET:XmATTACH_FORM,
		      landscape?XmNleftWidget:XmNtopWidget,
		      twoup.two_1,
		      XmNleftOffset, landscape?10:0,
		      NULL);
    }
    if (fourup.form) {
	XtVaSetValues(fourup.landscape, XmNlabelString, str, NULL);
	XtVaSetValues(fourup.four_2, XmNleftOffset, landscape?20:10, NULL);
	XtVaSetValues(fourup.four_3, XmNtopOffset, landscape?10:20, NULL);
	XtVaSetValues(fourup.four_4, XmNtopOffset, landscape?10:20,
		      XmNleftOffset, landscape?20:10, NULL);
	XtVaSetValues(fourup.print, XmNtopOffset, landscape?10:20, NULL);

	XtVaSetValues(fourup.landscape, XmNtopOffset, landscape?10:20, NULL);
    }
    if (sixup.form) {
	XtVaSetValues(sixup.landscape, XmNlabelString, str, NULL);
	XtVaSetValues(sixup.six_3, XmNtopAttachment,
		      landscape?XmATTACH_FORM:XmATTACH_WIDGET,
		      XmNleftAttachment,
		      landscape?XmATTACH_WIDGET:XmATTACH_FORM,
		      landscape?XmNleftWidget:XmNtopWidget,
		      sixup.six_2,
		      XmNleftOffset, landscape?10:0,
		      NULL);
	XtVaSetValues(sixup.six_4, XmNleftAttachment,
		      landscape?XmATTACH_FORM:XmATTACH_WIDGET,
		      XmNleftWidget, sixup.six_3,
		      XmNleftOffset, landscape?0:10,
		      NULL);
	XtVaSetValues(sixup.six_5, XmNtopWidget,
		      landscape?sixup.six_1:sixup.six_3,
		      XmNleftAttachment,
		      landscape?XmATTACH_WIDGET:XmATTACH_FORM,
		      XmNleftWidget, sixup.six_4,
		      XmNleftOffset, landscape?10:0,
		      NULL);
    }
}

void form_landscape_callback(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    int landscape, error;
    XmString str;

    getivar_("landscape", &landscape, &error, 9);
    if (landscape) {
	driver("LANDSCAPE=OFF");
	landscape = 0;
    }
    else {
	driver("LANDSCAPE=ON");
	landscape = 1;
    }
    set_landscape_widgets(landscape);
}

void do_2up(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Widget popup;
    XmString str;
    Position xp, yp;
    int landscape, error;


    if(twoup.form != (Widget)NULL){
	return;
    }
    

    getivar_("landscape", &landscape, &error, 9);
    XtVaGetValues(XtParent(w), XmNx, &xp,
		  XmNy, &yp,
		  NULL);
    xp += 50;
	
      
    popup = XtVaCreatePopupShell("2 Up",
				 transientShellWidgetClass,
				 XtParent(Box),
				 XmNautoUnmanage,FALSE,
				 XmNx, xp,
				 XmNy, yp,
				 XtNallowShellResize,TRUE,NULL);
        
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &twoup.form);
    XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		  (XtPointer)TWO_TEXT_BOXES);

    twoup.form = XtVaCreateManagedWidget("2 Up",
					 xmFormWidgetClass,
					 popup,
					 XmNresizable, TRUE,
					 XmNresizePolicy, XmRESIZE_ANY,
					 NULL);
    
    twoup.two_1 = XtVaCreateManagedWidget("1",
					  xmTextFieldWidgetClass,
					  twoup.form,
					  XmNcolumns, 8,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNshadowThickness,4,
					  NULL);
    
    twoup.two_2 = XtVaCreateManagedWidget("2",
				  xmTextFieldWidgetClass,
				  twoup.form,
				  XmNcolumns, 8,
				  XmNtopAttachment,
				  landscape?XmATTACH_FORM:XmATTACH_WIDGET,
				  XmNtopOffset, 10,
				  XmNleftAttachment,
				  landscape?XmATTACH_WIDGET:XmATTACH_FORM,
				  landscape?XmNleftWidget:XmNtopWidget,
				  twoup.two_1,
				  XmNleftOffset, landscape?10:0,
				  XmNshadowThickness,4,
				  NULL);

    str = NewString("Print");
    twoup.print = XtVaCreateManagedWidget("Print",
					  xmPushButtonWidgetClass,
					  twoup.form,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, twoup.two_2,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNlabelString, str,
					  NULL);
    XmStringFree(str);
    if (land == NULL) land = NewString("Landscape");
    if (port == NULL) port = NewString("Portrait");
    if (landscape) str = land;
    else str = port;
    twoup.landscape = XtVaCreateManagedWidget("Landscape",
					  xmPushButtonWidgetClass,
					  twoup.form,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, twoup.two_2,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget, twoup.print,
					  XmNlabelString, str,
					  NULL);
				  

    XtAddCallback(twoup.two_1,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(twoup.two_1,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)T1);
    XtAddCallback(twoup.two_1,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)T1);
    XtAddCallback(twoup.two_1,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)T1);

    XtAddCallback(twoup.two_2,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(twoup.two_2,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)T2);
    XtAddCallback(twoup.two_2,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)T2);
    XtAddCallback(twoup.two_2,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)T2);

    XtAddCallback(twoup.print, XmNactivateCallback,
		  form_print_callback, (XtPointer)"2");
    XtAddCallback(twoup.landscape, XmNactivateCallback,
		  form_landscape_callback, NULL);
    XtManageChild(popup);
}

void do_4up(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Widget popup;
    XmString str;
    Position xp, yp;
    int landscape, error;

    if(fourup.form != (Widget)NULL){
	return;
    }

    getivar_("landscape", &landscape, &error, 9);
    XtVaGetValues(XtParent(w), XmNx, &xp,
		  XmNy, &yp,
		  NULL);
    xp += 50;
	
    popup = XtVaCreatePopupShell("4 Up",
				 transientShellWidgetClass,
				 XtParent(Box),
				 XmNautoUnmanage,FALSE,
				 XmNx, xp,
				 XmNy, yp,
				 XtNallowShellResize,TRUE,NULL);
        
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &fourup.form);
    XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		  (XtPointer)FOUR_TEXT_BOXES);

    fourup.form = XtVaCreateManagedWidget("4 Up",
					  xmFormWidgetClass,
					  popup,
					  XmNresizable, TRUE,
					  XmNresizePolicy, XmRESIZE_ANY,
					  NULL);
    
    fourup.four_1 = XtVaCreateManagedWidget("1",
					    xmTextFieldWidgetClass,
					    fourup.form,
					    XmNcolumns, 8,
					    XmNtopAttachment, XmATTACH_FORM,
					    XmNtopOffset, 10,
					    XmNleftAttachment, XmATTACH_FORM,
					    XmNshadowThickness,4,
					    NULL);
    
    fourup.four_2 = XtVaCreateManagedWidget("2",
					    xmTextFieldWidgetClass,
					    fourup.form,
					    XmNcolumns, 8,
					    XmNtopAttachment, XmATTACH_FORM,
					    XmNtopOffset, 10,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftWidget, fourup.four_1,
					    XmNleftOffset, landscape?20:10,
					    XmNshadowThickness,4,
					    NULL);

    fourup.four_3 = XtVaCreateManagedWidget("3",
					    xmTextFieldWidgetClass,
					    fourup.form,
					    XmNcolumns, 8,
					    XmNtopAttachment, XmATTACH_WIDGET,
					    XmNtopWidget, fourup.four_1,
					    XmNtopOffset, landscape?10:20,
					    XmNleftAttachment, XmATTACH_FORM,
					    XmNshadowThickness,4,
					    NULL);
    
    fourup.four_4 = XtVaCreateManagedWidget("4",
					    xmTextFieldWidgetClass,
					    fourup.form,
					    XmNcolumns, 8,
					    XmNtopAttachment, XmATTACH_WIDGET,
					    XmNtopWidget, fourup.four_1,
					    XmNtopOffset,landscape?10:20,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftWidget, fourup.four_3,
					    XmNleftOffset, landscape?20:10,
					    XmNshadowThickness,4,
					    NULL);

    str = NewString("Print");
    fourup.print = XtVaCreateManagedWidget("Print",
					   xmPushButtonWidgetClass,
					   fourup.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget, fourup.four_4,
					   XmNtopOffset, landscape?10:20,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNlabelString, str,
					   XmNlabelString, str,
					   NULL);
    XmStringFree(str);
    if (land == NULL) land = NewString("Landscape");
    if (port == NULL) port = NewString("Portrait");
    if (landscape) str = land;
    else str = port;
    fourup.landscape = XtVaCreateManagedWidget("Landscape",
				       xmPushButtonWidgetClass,
				       fourup.form,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, fourup.four_4,
				       XmNtopOffset, landscape?10:20,
				       XmNleftAttachment, XmATTACH_WIDGET,
				       XmNleftWidget, fourup.print,
				       XmNlabelString, str,
				       NULL);
				  

    XtAddCallback(fourup.four_1,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(fourup.four_1,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)F1);
    XtAddCallback(fourup.four_1,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)F1);
    XtAddCallback(fourup.four_1,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)F1);

    XtAddCallback(fourup.four_2,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(fourup.four_2,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)F2);
    XtAddCallback(fourup.four_2,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)F2);
    XtAddCallback(fourup.four_2,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)F2);

    XtAddCallback(fourup.four_3,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(fourup.four_3,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)F3);
    XtAddCallback(fourup.four_3,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)F3);
    XtAddCallback(fourup.four_3,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)F3);

    XtAddCallback(fourup.four_4,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(fourup.four_4,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)F4);
    XtAddCallback(fourup.four_4,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)F4);
    XtAddCallback(fourup.four_4,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)F4);

    XtAddCallback(fourup.print, XmNactivateCallback,
		  form_print_callback, (XtPointer)"4");
    XtAddCallback(fourup.landscape, XmNactivateCallback,
		  form_landscape_callback, NULL);
    XtManageChild(popup);
}

void do_6up(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    Widget popup;
    XmString str;
    Position xp, yp;
    int landscape, error;

    if(sixup.form != (Widget)NULL){
	return;
    }

    getivar_("landscape", &landscape, &error, 9);
    XtVaGetValues(XtParent(w), XmNx, &xp,
		  XmNy, &yp,
		  NULL);
    xp += 50;
	
    popup = XtVaCreatePopupShell("6 Up",
				 transientShellWidgetClass,
				 XtParent(Box),
				 XmNautoUnmanage,FALSE,
				 XmNx, xp,
				 XmNy, yp,
				 XtNallowShellResize,TRUE,NULL);
        
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &sixup.form);
    XtAddCallback(popup,XmNdestroyCallback,cleanup_form_call,
		  (XtPointer)SIX_TEXT_BOXES);

    sixup.form = XtVaCreateManagedWidget("6 Up",
					 xmFormWidgetClass,
					 popup,
					 XmNresizable, TRUE,
					 XmNresizePolicy, XmRESIZE_ANY,
					 NULL);
    
    sixup.six_1 = XtVaCreateManagedWidget("1",
					  xmTextFieldWidgetClass,
					  sixup.form,
					  XmNcolumns, 8,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNshadowThickness,4,
					  NULL);
    
    sixup.six_2 = XtVaCreateManagedWidget("2",
					  xmTextFieldWidgetClass,
					  sixup.form,
					  XmNcolumns, 8,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget, sixup.six_1,
					  XmNleftOffset, 10,
					  XmNshadowThickness,4,
					  NULL);

    sixup.six_3 = XtVaCreateManagedWidget("3",
				  xmTextFieldWidgetClass,
				  sixup.form,
				  XmNcolumns, 8,
				  XmNtopAttachment,
				  landscape?XmATTACH_FORM:XmATTACH_WIDGET,
				  XmNtopOffset, 10,
				  XmNleftAttachment,
				  landscape?XmATTACH_WIDGET:XmATTACH_FORM,
				  landscape?XmNleftWidget:XmNtopWidget,
				  sixup.six_2,
				  XmNleftOffset, landscape?10:0,
				  XmNshadowThickness,4,
				  NULL);
    
    sixup.six_4 = XtVaCreateManagedWidget("4",
				  xmTextFieldWidgetClass,
				  sixup.form,
				  XmNcolumns, 8,
				  XmNtopAttachment, XmATTACH_WIDGET,
				  XmNtopWidget, sixup.six_1,
				  XmNtopOffset, 10,
				  XmNleftAttachment,
				  landscape?XmATTACH_FORM:XmATTACH_WIDGET,
				  XmNleftWidget, sixup.six_3,
				  XmNleftOffset, landscape?0:10,
				  XmNshadowThickness,4,
				  NULL);

    sixup.six_5 = XtVaCreateManagedWidget("5",
				  xmTextFieldWidgetClass,
				  sixup.form,
				  XmNcolumns, 8,
				  XmNtopAttachment, XmATTACH_WIDGET,
				  XmNtopWidget,
				  landscape?sixup.six_1:sixup.six_3,
				  XmNtopOffset, 10,
				  XmNleftAttachment,
				  landscape?XmATTACH_WIDGET:XmATTACH_FORM,
				  XmNleftWidget, sixup.six_4,
				  XmNleftOffset, landscape?10:0,
				  XmNshadowThickness,4,
				  NULL);
    sixup.six_6 = XtVaCreateManagedWidget("6",
				  xmTextFieldWidgetClass,
				  sixup.form,
				  XmNcolumns, 8,
				  XmNtopAttachment, XmATTACH_WIDGET,
				  XmNtopWidget, sixup.six_3,
				  XmNtopOffset, 10,
				  XmNleftAttachment, XmATTACH_WIDGET,
				  XmNleftWidget, sixup.six_5,
				  XmNleftOffset, 10,
				  XmNshadowThickness,4,
				  NULL);

    str = NewString("Print");
    sixup.print = XtVaCreateManagedWidget("Print",
					  xmPushButtonWidgetClass, 
					  sixup.form, 
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, sixup.six_6,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNlabelString, str,
					  NULL);
    XmStringFree(str);
    if (land == NULL) land = NewString("Landscape");
    if (port == NULL) port = NewString("Portrait");
    if (landscape) str = land;
    else str = port;
    sixup.landscape = XtVaCreateManagedWidget("Landscape",
					  xmPushButtonWidgetClass,
					  sixup.form,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, sixup.six_6,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget, sixup.print,
					  XmNlabelString, str,
					  NULL);
				  

    XtAddCallback(sixup.six_1,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(sixup.six_1,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)S1);
    XtAddCallback(sixup.six_1,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)S1);
    XtAddCallback(sixup.six_1,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)S1);

    XtAddCallback(sixup.six_2,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(sixup.six_2,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)S2);
    XtAddCallback(sixup.six_2,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)S2);
    XtAddCallback(sixup.six_2,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)S2);

    XtAddCallback(sixup.six_3,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(sixup.six_3,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)S3);
    XtAddCallback(sixup.six_3,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)S3);
    XtAddCallback(sixup.six_3,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)S3);

    XtAddCallback(sixup.six_4,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(sixup.six_4,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)S4);
    XtAddCallback(sixup.six_4,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)S4);
    XtAddCallback(sixup.six_4,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)S4);

    XtAddCallback(sixup.six_5,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(sixup.six_5,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)S5);
    XtAddCallback(sixup.six_5,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)S5);
    XtAddCallback(sixup.six_5,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)S5);

    XtAddCallback(sixup.six_6,XmNactivateCallback,
                  printbox_callback,NULL);
    XtAddCallback(sixup.six_6,XmNactivateCallback,
                  cleanup_box_call,(XtPointer)S6);
    XtAddCallback(sixup.six_6,XmNmodifyVerifyCallback,
                  print_check_num,(XtPointer)S6);
    XtAddCallback(sixup.six_6,XmNmotionVerifyCallback,
                  text_box_motion,(XtPointer)S6);
    XtAddCallback(sixup.landscape, XmNactivateCallback,
		  form_landscape_callback, NULL);

    XtAddCallback(sixup.print, XmNactivateCallback, form_print_callback, (XtPointer)"6");
    XtManageChild(popup);
}
