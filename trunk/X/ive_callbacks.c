/****************************************************************************
 *$Log: ive_callbacks.c,v $
 *Revision 1.40  2005/11/03 19:50:27  warren
 *Fixed crashing with large strange dataset
 *
 *Revision 1.39  2002/12/09 18:03:09  warren
 *Instead of checking number of plane every time, just check once and record
 *if it is true color in bool.
 *
 *Revision 1.38  2002/08/09 19:57:26  warren
 *Fixed for Linux and tested with DEC sun etc.
 *most items inside #ifdef's, or were general cleanup/fixes
 *
 *Revision 1.37  1998/06/12 21:38:55  harry
 *Fixes to check_in.
 *
 *Revision 1.36  1998/01/28 15:28:18  harry
 *Round off slider values to the nearest integer rather than truncating.
 *
 *Revision 1.35  1997/12/31 23:31:00  warren
 *Fixed Dales color loxking problem, holton's crashes etc.
 *
 *Revision 1.34  1997/07/15 17:41:43  warren
 *Increase beta number, get rid of empty popup that was left after file
 *widgets were gone.
 *
 *Revision 1.33  1997/06/03 19:13:46  davidive
 *The big mouse slicing changeover. We have the new form, only requires you
 *set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 *always runs through init_points_ to catch all these things.
 *note: only actually works when two dimentions have points set. Needs to
 *be expanded into 1, 3 or 4.
 *
 *Revision 1.32  1997/05/07 18:48:40  harry
 *Change ERROR line printout.
 *
 *Revision 1.31  1997/04/18 22:12:35  warren
 *Null out all the widgets in the slicer when it is destroyed.
 *
 *Revision 1.30  1997/04/18 22:09:19  harry
 *Write error messages to stderr.
 *
 *Revision 1.29  1997/04/11 18:46:41  warren
 *Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 *Revision 1.28  1997/02/07 00:07:18  warren
 *Fixed lack of typin bug for sliders and printer widget not remembering
 *to do color.
 *
 *Revision 1.27  1996/04/25 16:24:21  harry
 *Add Skew-T plots and the beginning of trajectory plots.
 *
 *Revision 1.26  1995/12/15 22:32:37  warren
 *moved patterns into 4.0 and implemented the list of fixes and changes from
 *Curtis.
 *
 *Revision 1.25  1995/10/19 22:13:16  harry
 *Set defaults by clicking right mouse button.  Make all TextWidgets into
 *TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.24  1995/10/04  22:09:00  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.23  1995/09/15  16:47:10  harry
 * Add support for timeout of warning messages.
 *
 * Revision 1.22  1995/06/23  21:11:18  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.21  1995/02/28  19:29:22  warren
 * Fixed problem on alpha's where the popup type in boxes didn't move the
 * sliders for the window.
 *
 * Revision 1.20  1994/09/14  18:19:16  harry
 * Big 1D flip uncommit.
 *
 * Revision 1.19  1994/05/18  17:53:16  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.18  1994/04/19  23:01:44  yuri
 * Big 1D flip commit.
 *
 * Revision 1.17  1994/03/28  19:59:31  warren
 * Fixed so colors change when they are told again.
 *
 * Revision 1.16  1994/01/10  23:33:15  warren
 * got rid of the annoying ! on the left of the help widget.
 *
 * Revision 1.15  1993/11/29  19:37:56  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.14  1993/11/03  23:02:16  warren
 * Fixed problem where slicer widget didn't change window.
 *
 * Revision 1.13  1993/10/18  23:06:53  warren
 * updates from 3.0 version and fixed printer forms.
 *
 * Revision 1.12  1993/08/18  15:46:10  harry
 * Change to new transform routines.
 *
 * Revision 1.11  1993/08/17  18:33:54  warren
 * Beginning of printer forms.
 *
 * Revision 1.10  1993/07/26  19:04:04  warren
 * Fixed bug in X part of set units.
 *
 * Revision 1.9  1993/07/23  20:08:01  warren
 * added lockcolortable to the loop ignore list. fixed the time slider callback
 * to compare the slider to the scaled times and added an array of scaled times.
 *
 * Revision 1.8  1993/06/29  21:04:00  warren
 * Fixed Rajul's problem of crashing when reading a file with no global
 * mins and maxs. Only check sliders that exist.
 *
 * Revision 1.7  1993/06/23  18:06:09  warren
 * Increased command line length for typed in commands.
 *
 * Revision 1.6  1993/06/16  17:08:46  yuri
 * type = scalar is the same as = line (?!) from now.
 *
 * Revision 1.5  1993/06/10  16:28:02  yuri
 * Vactor color stuff added.
 *
 * Revision 1.4  1993/05/28  20:12:52  yuri
 * 	Second try to incorporate.
 *
 * Revision 1.3  1993/05/27  16:38:16  warren
 * Recover files removed by yuri's commit
 *
 * Revision 1.1.1.1  1993/04/07  19:41:21  harry
 * Initial CVS checkin of IVE.
 *
 ****************************************************************************/
static char ident[] = "$Id: ive_callbacks.c,v 1.40 2005/11/03 19:50:27 warren Exp $" ;

void gesc_(){
/* defined to make ncar graphics link properly with xgks under sunos4.1.2*/
}

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#ifdef Linux
extern double rint();
#endif
#include <Xm/XmAll.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <xgks.h>

#include <ive.h>
#include <ive_for.h>
#include <ive_gks.h>
#include <ive_macros.h>

extern void getivar_(),getavar_(),getaarr_(),getrarr_(),ui_update_(),XtMoveWidget();
extern XmString NewString();

/***************************************************************************
  Widgets declared globally so that they can be set from the FORTRAN
  all other will be nonaccessable
***************************************************************************/
Widget Box;
WidgetList buttons = NULL,buttons2=NULL;
extern struct{
    Widget info_2d, mins[4], maxs[4], fix[4];
}slicer_widget;

void check_default_handler(w,data,ev)
    Widget w;
    char *data;
    XButtonEvent *ev;
{
    Widget widg;

    if (ev->button == Button3 &&
	XtIsSubclass((widg=XtWindowToWidget(XtDisplay(w), ev->window)),
		    xmTextFieldWidgetClass)) {
	XKeyEvent event;

	XmTextSetString(widg, "default");
	/*
	   Necessary to fake typing "Return" to cause XmNactivateCallback
	   to be called.
	   */
	event.type = KeyPress;
	event.send_event = True;
	event.display = ev->display;
	event.window = ev->window;
	event.root = ev->root;
	event.subwindow = ev->subwindow;
	event.time = ev->time;
	event.state = 0;
	event.keycode = XKeysymToKeycode(XtDisplay(w), XK_Return);
	XSendEvent(XtDisplay(w), ev->window, False, KeyPressMask, 
		   (XEvent *)&event);
    }
}

void set_field(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    Arg args[2];
    int i=0;
    char str[80];

    XtSetArg(args[0],XmNset,FALSE);
    while(buttons[i] != NULL){
	if(buttons[i] != w)
	  XtSetValues(buttons[i],args,1);
	i++;
    }
    sprintf(str,"field=%s",data);
    driver(str);
}

int get_current_field_(buff, len)
     char *buff;
     int len;
{
    Arg args[2];
    int i=0, test;
    char *junk;

    XtSetArg(args[0],XmNset,&test);
    XtSetArg(args[0],XmNuserData,&junk);
    while(buttons[i] != NULL){
	XtGetValues(buttons[i],args,2);
	if(test){
	    strncpy(buff,junk,len);
	    return(1);
	}
	i++;
    }
    return(0);
}

void show_widget(w, data, call)
     Widget w;
     Widget data;
     XmAnyCallbackStruct *call;
{
    XtManageChild(data);
}

void quit_ive(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct call;
{
#ifdef MEMDBG
    mnem_writestats();
#endif
    XtCloseDisplay(XtDisplay(w));
    exit(0);
}

void done_callback(w, client, call)
     Widget w;
     Widget client;
     XmAnyCallbackStruct *call;
{
    XtDestroyWidget(client);
}

void done_parent_callback(w, client, call)
     Widget w;
     caddr_t client;
     XmAnyCallbackStruct *call;
{
    XtUnmanageChild(XtParent(w));
}

void timeout_widget(w)
    Widget w;
{
    XtDestroyWidget(w);
    XtDestroyWidget(XtParent(w));
}

void del_callback(w, client, call)
     Widget w;
     caddr_t client;
     XmAnyCallbackStruct *call;
{
    if (client) XtRemoveTimeOut ((XtIntervalId) client);
    XtDestroyWidget(w);
}

void del_parent_callback(w, client, call)
     Widget w;
     caddr_t client;
     XmAnyCallbackStruct *call;
{
    XtDestroyWidget(XtParent(w));
}

void make_help_widget_(s, dummy)
     char *s;
     int dummy;
{
    Widget hbox, popup;
    Arg args[4];
    XmString xs;
    XtIntervalId timer;
    int warn, error;
    unsigned long lwarn;
      
    fprintf(stderr, "IVE ERROR - %s\n", s);
    xs = NewString(s);

    getivar_("warn_timeout", &warn, &error, 12);
    if (warn == 0) return;
    popup = XmCreateDialogShell(Box, "Error Message", NULL, 0);
    hbox=XtVaCreateManagedWidget("help", xmMessageBoxWidgetClass, popup,
				 XmNmessageString, xs,NULL);
    if (warn > 0) {
	lwarn = 1000*warn;
	timer = XtAppAddTimeOut(XtWidgetToApplicationContext(hbox),lwarn, 
				(XtTimerCallbackProc)timeout_widget, (XtPointer)hbox);
	XtAddCallback(hbox,XmNokCallback,
		      (XtCallbackProc)del_callback,(XtPointer)timer);
    }
    else XtAddCallback(hbox,XmNokCallback,(XtCallbackProc)del_callback,NULL);
    XtAddCallback(hbox,XmNokCallback,
		  (XtCallbackProc)del_parent_callback,NULL);
    XtUnmanageChild(XmMessageBoxGetChild(hbox,
				       XmDIALOG_HELP_BUTTON));    
    XtUnmanageChild(XmMessageBoxGetChild(hbox,
				       XmDIALOG_CANCEL_BUTTON));
    XmStringFree(xs);
		 
}

void make_value_widget_(s)
     char *s;
{
    Widget hbox, popup;
    Arg args[4];
    XmString xs;
      
    xs = NewString(s);

    popup = XmCreateDialogShell(Box, "Value info", NULL, 0);
    XtSetArg(args[0], XmNmessageString, xs);  
    XtSetArg(args[1], XmNdialogType, XmDIALOG_WARNING);  
    hbox=XmCreateMessageBox(popup,"help",args,2);
    XtAddCallback(hbox,XmNokCallback,(XtCallbackProc)del_callback,NULL);
    XtAddCallback(hbox,XmNokCallback,(XtCallbackProc)del_parent_callback,NULL);
    XtUnmanageChild(XmMessageBoxGetChild(hbox,XmDIALOG_HELP_BUTTON));    
    XtUnmanageChild(XmMessageBoxGetChild(hbox,XmDIALOG_CANCEL_BUTTON));
    XtManageChild(hbox);
    XmStringFree(xs);
		 
}
void make_result_widget_(s)
     char *s;
{
    Widget hbox, popup;
    Arg args[4];
    XmString xs;
      
    xs = NewString(s);

    popup = XmCreateDialogShell(Box, "Averaging info", NULL, 0);
    XtSetArg(args[0], XmNmessageString, xs);  
    XtSetArg(args[1], XmNdialogType, XmDIALOG_WARNING);  
    hbox=XmCreateMessageBox(popup,"help",args,2);
    XtAddCallback(hbox,XmNokCallback,(XtCallbackProc)del_callback,NULL);
    XtAddCallback(hbox,XmNokCallback,(XtCallbackProc)del_parent_callback,NULL);
    XtUnmanageChild(XmMessageBoxGetChild(hbox,XmDIALOG_HELP_BUTTON));    
    XtUnmanageChild(XmMessageBoxGetChild(hbox,XmDIALOG_CANCEL_BUTTON));
    XtManageChild(hbox);
    XmStringFree(xs);
		 
}


void make_info_widget_(s)
     char *s;
{
    static Widget ibox=(Widget)0;
    Arg args[2];
    XmString xs;

    if(ibox){
	XtUnmanageChild(ibox);    
	XtDestroyWidget(ibox);
	ibox = (Widget)0;
    }
	if(*s == '\0')
	  return;
    xs = NewString(s);
    XtSetArg(args[0], XmNmessageString, xs);  
    ibox=XmCreateWarningDialog(Box,"info",args,1);
    XtAddCallback(ibox,XmNokCallback,(XtCallbackProc)del_callback,NULL);
    XtUnmanageChild(XmMessageBoxGetChild(ibox,XmDIALOG_HELP_BUTTON));    
    XtUnmanageChild(XmMessageBoxGetChild(ibox,XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(ibox,XmDIALOG_OK_BUTTON));
    XtManageChild(ibox);
    XmStringFree(xs);
    XWarpPointer(XtDisplay(ibox), None, XtWindow(ibox), 
		 0, 0, 0, 0,
                 10, 10);

}

void set_data_units(w,data,call)
     Widget w;
     caddr_t data;
     XmSelectionBoxCallbackStruct *call;
{
    char buff[256],*str;
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&str);
    if(str[0] != '\0'){
	sprintf(buff,"data_units=%s",str);
	driver(buff);
    }
    free(str);
}

void set_units_cb(w,data,ev)
     Widget w;
     char *data;
     XButtonEvent *ev;
{
    Widget ask;
    Arg args[2];
    XmString tmp,tmp2;
    char old[256];
    int error;
    void call_driver();

    if(ev->button == Button3){
	(void)getavar_("field",old,&error,5,256);
	if(!strcmp(data,old)){
	    (void)getavar_("data_display_units",old,&error,18,256);
	    if(error){
	      make_help_widget_("Can't get current units",23);
		return;
	    }
	    tmp = NewString("Display Units");
	    tmp2 = NewString(old);
	    XtSetArg(args[0],XmNtextString,tmp2);
	    XtSetArg(args[1],XmNselectionLabelString,tmp);
	    ask = XmCreatePromptDialog(Box, "Set Units", args,2);
	    XtManageChild(ask);
	    XmStringFree(tmp);
	    XmStringFree(tmp2);
	    XtAddCallback(ask,XmNhelpCallback,call_driver,(XtPointer)"help data_units");
	    XtAddCallback(ask,XmNokCallback,(XtCallbackProc)set_data_units,NULL);
	}
    }
}


void set_domain_units_cb(w,data,call)
     Widget w;
     char *data;
     XmSelectionBoxCallbackStruct *call;
{
    char buff[256],*str;
    XmStringGetLtoR(call->value, XmSTRING_DEFAULT_CHARSET,&str);
    if(str[0] != '\0'){
	switch(*data){
	  case 'X':
	    sprintf(buff,"domain_units=%s,,,",str);
	    break;
	  case 'Y':
	    sprintf(buff,"domain_units=,%s,,",str);
	    break;
	  case 'Z':
	    sprintf(buff,"domain_units=,,%s,",str);
	    break;
	  case 'T':
	    sprintf(buff,"domain_units=,,,%s",str);
	    break;
	}
	printf("%s\n",buff);
	driver(buff);
    }
    free(str);
}

void domain_units_call(w,data,ev)
     Widget w;
     char *data;
     XButtonEvent *ev;
{
    Widget ask;
    Arg args[2];
    XmString tmp,tmp2;
    char old[320];/* 4*80 */
    int count;
    int error;
    void call_driver();

    if(ev->button == Button3){
	count=4;
	(void)getaarr_("domain_display_units",old,&count,&error,20,80);
	if(error){
	  make_help_widget_("Can't get current domain units",30);
	    return;
	}
	tmp = NewString("Display Units");
	switch(*data){
	  case 'X':
	    tmp2 = NewString(old);
	    break;
	  case 'Y':
	    tmp2 = NewString(old+80);
	    break;
	  case 'Z':
	    tmp2 = NewString(old+160);
	    break;
	  case 'T':
	    tmp2 = NewString(old+240);
	    break;
	}
	printf("units %s %s %s %s\n", old, old+80, old+160, old+240);
	XtSetArg(args[0],XmNtextString,tmp2);
	XtSetArg(args[1],XmNselectionLabelString,tmp);
	ask = XmCreatePromptDialog(Box, "Set Units", NULL,0);
	XtVaSetValues(ask,XmNtextString,tmp2,XmNselectionLabelString,tmp,NULL);
	XtManageChild(ask);
	XmStringFree(tmp);
	XmStringFree(tmp2);
	XtAddCallback(ask,XmNhelpCallback,call_driver,(XtPointer)"help domain_units");
	XtAddCallback(ask,XmNokCallback,(XtCallbackProc)set_domain_units_cb,(XtPointer)data);
    }
}


void make_field_buttons_(string,parent)
     char **string;
     Widget parent;
{
    static Widget holder;
    int ii,i=0,j=0,k=0;
    XmString str;
    Arg args[2];

    /*** buttons to buttons2 switch is here to help speedup the visual blip
         when changing data sets ***/

    if ( string == NULL && parent == NULL ) {
	if ( buttons != NULL ){
	    for (j=0; buttons[j] != NULL ;j++); 
	    XtUnmanageChildren(buttons,j);

	    for (j=0; buttons[j] != NULL ;j++) 
	      XtDestroyWidget(buttons[j]);
	    XtFree((char *)buttons);
	    buttons = (Widget *)NULL;
	}
    }
    else if(string == NULL){
	holder = parent;
    }
    else{
	
	
	while(*(string + ++i) != NULL);
	if((buttons2 = (WidgetList)XtMalloc((i+1)*sizeof(Widget)))==NULL){
	    fprintf(stderr,"sorry out of memory couldn't allocate\n");
	    exit(ENOMEM);
	}
	k=0;
	for(j=0; j<i;j++){
	    if(**(string+j) != '$' && **(string+j) != '\0'){
		XtSetArg(args[0],XmNuserData,*(string+j));
		buttons2[k] = XtCreateWidget(*(string+j),
					     xmToggleButtonWidgetClass,
					     holder,args,1);
		XtAddCallback(buttons2[k],XmNarmCallback,
			      (XtCallbackProc)set_field,
			      (XtPointer)*(string+j));
		XtAddEventHandler(buttons2[k],ButtonPressMask,FALSE,
				  (XtEventHandler)set_units_cb,
				  *(string+j));
		k++;
	    }
	}
	buttons2[k] = (Widget)NULL;
	if (buttons != NULL){
	    for (j=0; buttons[j] != NULL ;j++); 
	    XtUnmanageChildren(buttons,j);
	}
	XtManageChildren(buttons2, k);
	buttons2[i]=(Widget)NULL;
	if (buttons != NULL)
	  for (j=0; buttons[j] != NULL ;j++) 
	    XtDestroyWidget(buttons[j]);
	XtFree((char *)buttons);
	buttons=buttons2;

    }
}

void mixer_callback(w, data, call)
     Widget w;
     Widget data;
     XmAnyCallbackStruct *call;
{

    void  gks_color_mixer();
    Bool rgb=TRUE;
    if(data != NULL)rgb=FALSE;
    gks_color_mixer(w,user_colors_.min_user_color,user_colors_.max_user_color,&rgb);
}

char *com_holder[101];
int  com_ptr, first, top_com;

#include <X11/Xproto.h>

void
DoComHist(w,  data, event)
     Widget w;
     XKeyEvent *event;
     caddr_t data;
{
    KeySym ks;
    char *ksname;
    int nbytes, updown;/*0=nocontrol 1=control p or n*/
    static int control=0;
    char str[256+1];

    updown=0;
    if(event->type==KeyRelease){
	nbytes = XLookupString (event, str, 256, &ks, NULL);
	ksname = XKeysymToString (ks);
	if(*(ksname)=='C' && *(ksname+1)=='o' && *(ksname+2)=='n' &&
	   *(ksname+3)=='t' && *(ksname+4)=='r' && *(ksname+5)=='o' &&
	   *(ksname+6)=='l' && *(ksname+7)=='_' && *(ksname+8)=='L'){
	    control=0;
	}
	return;
    }
    if(event->type !=KeyPress)return;
    nbytes = XLookupString (event, str, 256, &ks, NULL);
    ksname = XKeysymToString (ks);

    switch(*ksname){
      case 'p' : if(control)updown=1;
      case 'U' : 
	if(updown || *(ksname+1)=='p'){
	    updown=0;
	    if(com_ptr<top_com - 1){
		com_ptr++;
		XmTextSetString(w,com_holder[com_ptr]);
	    }
	    else{
		XBell(XtDisplay(w),50);
	    }
	}
	break;
      case 'n' : if(control)updown=1;
      case 'D':
	if(updown || *(ksname+1)=='o' && *(ksname+2)=='w' && *(ksname+3)=='n'){
	    updown=0;
	    if(com_ptr>0){
		com_ptr--;
		XmTextSetString(w,com_holder[com_ptr]);
	    }
	    else{
		XmTextSetString(w,(char *)NULL);
		if(com_ptr == 0){
		    com_ptr--;
		}
		else{ /* must be -1 */
		    XBell(XtDisplay(w),50);
		}
	    }
	}
	break;
      case 'C':
	if(*(ksname+1)=='o' && *(ksname+2)=='n' && *(ksname+3)=='t' &&
	   *(ksname+4)=='r' && *(ksname+5)=='o' && *(ksname+6)=='l' &&
	   *(ksname+7)=='_' && *(ksname+8)=='L'){
	    control=1;
	}
	break;
    }
			
}

void command_callback(w,data,call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    char *str,*str2;
    static int first=0;
    int i;
    extern FILE *input_stream;
    if(!first)
      for(first=0;first<100;first++)
	com_holder[first] = (char *)NULL;
    str=XmTextGetString(w);
    i=0;
    for(i=0; str[i]==' ' && i<strlen(str); i++);
    if(str[i]=='\0'){
	XtFree(str);
	if(input_stream != NULL){
	    driver_notwin("step");
	}
	return;
    }
    if(top_com<99)top_com++;
    for(i=100; i>0; i--)
      com_holder[i] = com_holder[i-1];
    str2 = malloc(strlen(str)+1);
    if(str2){
	strcpy(str2,str);
	com_holder[0] = str2;
	com_ptr = -1;
    }
    driver_notwin(str);
    XtFree(str);
    XmTextSetString(w,"");
}

void command_callback_check(w, data, call)
     Widget w;
     caddr_t data;
     XmTextVerifyCallbackStruct *call;
{
    int len=0;
    
    if (call->startPos < call->currInsert)
      return; 
    if((len = XmTextGetLastPosition(w))==512){
	call->doit = FALSE;
	return;
    }
    if(len + call->text->length >512){
	call->text->ptr[512-len] = 0;
	call->text->length = strlen(call->text->ptr);
    }

  }

void call_driver(w, data, call)
     Widget w;
     char *data;
     XmCommandCallbackStruct *call;
{
    char str[256];
    sprintf(str,"%s",data);
    driver(str);
}


void call_driver_scv(w, data, call)
     Widget w;
     char *data;
     XmCommandCallbackStruct *call;
{
    char str[256];
    char st1[20];
    char str2[256], *p;
    int  error;
    sprintf(str,"%s",data);
/*    (void)getavar_("plotyp",st1,&error,6,20);
    if( ( (*st1) == 'L') || ( (*st1) == 'l') 
       || ( (*st1) == 'S') || ( (*st1) == 's'))
      { */
	  driver(str);
/*      }
    else
      {
	  strcpy(str2, "VECTOR_COLOR");
	  p = strstr(str,"=");
	  strcat(str2,p);
	  driver(str2);
      } */
}

void min_window_check_callback(w, data, call)
     Widget w;
     Widget *data;
     XmAnyCallbackStruct *call;
{
    int minval,maxval,tval;
    unsigned char dir;
    Arg args[2];

    if(!XtIsManaged(*data))return;
    XmScaleGetValue(w,&minval);
    XmScaleGetValue(*data,&maxval);
    XtSetArg(args[0],XmNprocessingDirection,&dir);
    XtGetValues(w,args,1);
    if(dir != XmMAX_ON_LEFT){
	XtSetArg(args[0],XmNmaximum, &tval);
	XtGetValues(*data,args,1);
	if(minval >= maxval){
	    if(minval<tval){
		maxval = minval + 1;
		XtSetArg(args[0],XmNvalue,maxval);
		XtSetValues(*data,args,1);
	    }
	    else{
		maxval = minval--;
		XtSetArg(args[0],XmNvalue,maxval);
		XtSetValues(*data,args,1);
		XtSetArg(args[0],XmNvalue,minval);
		XtSetValues(w,args,1);
	    }
	}
    }
    else{
	XtSetArg(args[0],XmNminimum, &tval);
	XtGetValues(*data,args,1);
	if(minval <= maxval){
	    if(minval<tval){
		maxval = minval - 1;
		XtSetArg(args[0],XmNvalue,maxval);
		XtSetValues(*data,args,1);
	    }
	    else{
		maxval = minval++;
		XtSetArg(args[0],XmNvalue,maxval);
		XtSetValues(*data,args,1);
		XtSetArg(args[0],XmNvalue,minval);
		XtSetValues(w,args,1);
	    }
	}
    }
}


void max_window_check_callback(w, data, call)
     Widget w;
     Widget *data;
     XmAnyCallbackStruct *call;
{
    int minval,maxval,tval;
    unsigned char dir;
    Arg args[2];

    XmScaleGetValue(w,&maxval);
    XmScaleGetValue(*data,&minval);
    XtSetArg(args[0],XmNprocessingDirection,&dir);
    XtGetValues(w,args,1);
    if(dir != XmMAX_ON_LEFT){
    XtSetArg(args[0],XmNminimum, &tval);
    XtGetValues(*data,args,1);
	if(maxval>tval){
	    if(minval >= maxval){
		minval = maxval - 1;
		XtSetArg(args[0],XmNvalue,minval);
		XtSetValues(*data,args,1);
	    }
	}
	else{
	    if(minval >= maxval){
		minval = maxval++;
		XtSetArg(args[0],XmNvalue,minval);
		XtSetValues(*data,args,1);
		XtSetArg(args[0],XmNvalue,maxval);
		XtSetValues(w,args,1);
	    }
	}
    }
    else{
    XtSetArg(args[0],XmNmaximum, &tval);
    XtGetValues(*data,args,1);
	if(maxval<tval){
	    if(minval <= maxval){
		minval = maxval + 1;
		XtSetArg(args[0],XmNvalue,minval);
		XtSetValues(*data,args,1);
	    }
	}
	else{
	    if(minval <= maxval){
		minval = maxval--;
		XtSetArg(args[0],XmNvalue,minval);
		XtSetValues(*data,args,1);
		XtSetArg(args[0],XmNvalue,maxval);
		XtSetValues(w,args,1);
	    }
	}
    }
}


int fixed_index[4] = {0,0,0,0}; /*for slicing use*/
static int vector = 0;
static float check_in_value[4] = {1.1e32, 1.1e32, 1.1e32, 1.1e32};

void check_index(in)
     int in;
{
    char buff[256];
    float val1, dummy[4];
    int ival1, ival2, i, fxcnt, avcnt; 
    int dims;
    int error;
    void getrvar_();
    int flag=0;
    static int LOOP=32;

    getivar_("maxdim",&dims,&error,6);
    getrarr_("plwmin_scaled",dummy,&dims,&error,13,4);
    /*
      If in is negative, check_in is being called by XtAppAddTimeOut
      (initial settings), and there is no real value.  Otherwise, the
      real value is in check_in_value
      */
    if (in < 0) {
	in = -in;
	flag++;
    }
	
    switch(in){
      case 0:
	if(fixed_index[0] == 1){
	    if(slicer_widget.mins[0])
	      {
		  if (flag) {
		      XmScaleGetValue(slicer_widget.mins[0],&ival1);
		      val1 = (float)ival1;
		  }
		  else {
		      if (check_in_value[0] >= 1e32)
			  check_in_value[0] = dummy[0];
		      val1 = check_in_value[0];
		  }
	   }
	    else
	      {val1 = dummy[0];}
	    sprintf(buff,"xloc=%f",val1);
	    driver_notwin(buff);
	}
	else if(fixed_index[0] == 2){
	    driver("xloc=average");
	}
	else {
	    if(slicer_widget.mins[0]){
		driver("xloc=free");
	    }
	}
	break;
      case 1:
	if(fixed_index[1]==1){
	    if(slicer_widget.mins[1])
	      {
		  if (flag) {
		      XmScaleGetValue(slicer_widget.mins[1],&ival1);
		      val1 = (float)ival1;
		  }
		  else {
		      if (check_in_value[1] >= 1e32)
			  check_in_value[1] = dummy[1];
		      val1 = check_in_value[1];
		  }
	      }
	    else
	      {val1 = dummy[1];}
	    sprintf(buff,"yloc=%f",val1);
	    driver_notwin(buff);
	}
	else if(fixed_index[1]==2){
	    driver("yloc=average");
	}
	else{
	    if(slicer_widget.mins[1]){
		driver("yloc=free");
	    }
	}
	break;
      case 2:
	if(fixed_index[2] == 1){
	    if(slicer_widget.mins[2])
	      {
		  if (flag) {
		      XmScaleGetValue(slicer_widget.mins[2],&ival1);
		      val1 = (float)ival1;
		  }
		  else {
		      if (check_in_value[2] >= 1e32)
			  check_in_value[2] = dummy[2];
		      val1 = check_in_value[2];
		  }
	      }
	    else
	      {val1 = dummy[2];}
	    sprintf(buff,"zloc=%f",val1);
	    driver_notwin(buff);
	}
	else if(fixed_index[2] == 2){
	    driver("zloc=average");
	}
	else{
	    if(slicer_widget.mins[2]){
		driver("zloc=free");
	    }
	}
	break;
      case 3:
	if(fixed_index[3]==1){
	    if(slicer_widget.mins[3])
	  {
	      if (flag) {
		  XmScaleGetValue(slicer_widget.mins[3],&ival1);
		  val1 = (float)ival1;
	      }
	      else {
		  if (check_in_value[3] >= 1e32)
		      check_in_value[3] = dummy[3];
		  val1 = check_in_value[3];
	      }
	  }
	    else
	  {val1 = dummy[3];}
	    sprintf(buff,"time=%f",val1);
	    driver_notwin(buff);
	}
	else if (fixed_index[3]==2){
	    driver("time=average");
	}
	else{
	    if(slicer_widget.mins[3]){
		driver("time=free");
	    }
	}
	break;
    }
    (void)ui_update_(&LOOP);
}

void window_callback(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    int minval, maxval,i,j, ival;
    float rminval, rmaxval;
    char buff[256];

    i = data;
	buff[0]='\0';
    if(slicer_widget.info_2d != NULL && slicer_widget.mins[i%4] != NULL){
	if(XtIsManaged(slicer_widget.maxs[i%4])){
	    XmScaleGetValue(slicer_widget.mins[i%4],&minval);
	    XmScaleGetValue(slicer_widget.maxs[i%4],&maxval);
	    rminval = fix_slicer_value(i%4, (float) minval);
	    rmaxval = fix_slicer_value(i%4, (float) maxval);
	    ival = (int)(rint(rminval));
	    XmScaleSetValue(slicer_widget.mins[i%4],ival);
	    ival = (int)(rint(rmaxval));
	    XmScaleSetValue(slicer_widget.maxs[i%4],ival);
	    switch (i){
	      case 0:
		sprintf(buff,"window=%f",rminval);
		break;
	      case 1:
		sprintf(buff,"window=,,%f",rminval);
		break;
	      case 2:
		sprintf(buff,"window=,,,,%f",rminval);
		break;
	      case 3:
		sprintf(buff,"window=,,,,,,%f",rminval);
		break;
	      case 4:
		sprintf(buff,"window=,%f",rmaxval);
		break;
	      case 5:
		sprintf(buff,"window=,,,%f",rmaxval);
		break;
	      case 6:
		sprintf(buff,"window=,,,,,%f",rmaxval);
		break;
	      case 7:
		sprintf(buff,"window=,,,,,,,%f",rmaxval);
		break;
	    }
	}
	else{
	    XmScaleGetValue(slicer_widget.mins[i%4],&minval);
	    rminval = minval;
	    check_in_value[i%4] = fix_slicer_value(i%4, rminval);
	    ival = (int)(rint(check_in_value[i%4]));
	    XmScaleSetValue(slicer_widget.mins[i%4],
			    ival);
	}
    }
    if(buff[0] != '\0'){
	driver(buff);
    }
    else
      check_index(i);
}


void fix_var(w, client, call)
     Widget w;
     char **client;
     XmToggleButtonCallbackStruct *call;
{
    int WINDOW, set, ival, i=4;
    int error;
    Arg args[1];
    float wmax[4];
    static XmString tfree=NULL, fixed=NULL, avg=NULL, oldlab;
    WINDOW = 14;
    if(!tfree){
	tfree =  NewString("Free ");
	fixed = NewString("Fixed");
	avg =   NewString("Avg  ");
    }
    XtSetArg(args[0],XmNlabelString,&oldlab);
    XtGetValues(w,args,1);
    if (XmStringCompare(tfree,oldlab)){
        XtSetArg(args[0],XmNlabelString,fixed);
        XtSetValues(w,args,1);
	set = 1;
    }
    else if (XmStringCompare(fixed,oldlab)){
        XtSetArg(args[0],XmNlabelString,avg);
        XtSetValues(w,args,1);
	set = 2;
    }
    else {
        XtSetArg(args[0],XmNlabelString,tfree);
        XtSetValues(w,args,1);
	set = 0;
    }
    
    if(set == 1){
        switch(**client){
          case 'X': fixed_index[0] = 1;
            if(XtIsManaged(slicer_widget.maxs[0]))XtUnmanageChild(slicer_widget.maxs[0]);
            break;
          case 'Y': fixed_index[1] = 1;
            if(XtIsManaged(slicer_widget.maxs[1]))XtUnmanageChild(slicer_widget.maxs[1]);
            break;
          case 'Z': fixed_index[2] = 1;
	    if(XtIsManaged(slicer_widget.maxs[2]))XtUnmanageChild(slicer_widget.maxs[2]);
            break;
          case 'T': fixed_index[3] = 1;
	    if(XtIsManaged(slicer_widget.maxs[3]))XtUnmanageChild(slicer_widget.maxs[3]);
            break;
        }
    }
    else if(set == 0){
	(void)getrarr_("plwmax_scaled",wmax,&i,&error,13,4);
        switch(**client){
          case 'X': fixed_index[0] = 0;
            if(slicer_widget.maxs[0] && !XtIsManaged(slicer_widget.maxs[0])){
		XtManageChild(slicer_widget.maxs[0]);
		ival = (int)wmax[0];
		XmScaleSetValue(slicer_widget.maxs[0],ival);
	    }
            break;
          case 'Y': fixed_index[1] = 0;
            if(slicer_widget.maxs[1] && !XtIsManaged(slicer_widget.maxs[1])){
		XtManageChild(slicer_widget.maxs[1]);
		ival = (int)wmax[1];
		XmScaleSetValue(slicer_widget.maxs[1],ival);
	    }
            break;
          case 'Z': fixed_index[2] = 0;
            if(slicer_widget.maxs[2] && !XtIsManaged(slicer_widget.maxs[2])){
		XtManageChild(slicer_widget.maxs[2]);
		ival = (int)wmax[2];
		XmScaleSetValue(slicer_widget.maxs[2],ival);
	    }
            break;
          case 'T': fixed_index[3] = 0;
            if(slicer_widget.maxs[3] && !XtIsManaged(slicer_widget.maxs[3])){
		XtManageChild(slicer_widget.maxs[3]);
		ival = (int)wmax[3];
		XmScaleSetValue(slicer_widget.maxs[3],ival);
	    }
            break;
	}
    }
    else {
	(void)getrarr_("plwmax_scaled",wmax,&i,&error,13,4);
	switch(**client){
	  case 'X': fixed_index[0] = 2;
	    if(slicer_widget.maxs[0] && !XtIsManaged(slicer_widget.maxs[0])){
		XtManageChild(slicer_widget.maxs[0]);
		ival = (int)wmax[0];
		XmScaleSetValue(slicer_widget.maxs[0],ival);
	    }
	    break;
	  case 'Y': fixed_index[1] = 2;
	    if(slicer_widget.maxs[1] && !XtIsManaged(slicer_widget.maxs[1])){
		XtManageChild(slicer_widget.maxs[1]);
		ival = (int)wmax[1];
		XmScaleSetValue(slicer_widget.maxs[1],ival);
	    }
	    break;
	  case 'Z': fixed_index[2] = 2;
	    if(slicer_widget.maxs[2] && !XtIsManaged(slicer_widget.maxs[2])){
		XtManageChild(slicer_widget.maxs[2]);
		ival = (int)wmax[2];
		XmScaleSetValue(slicer_widget.maxs[2],ival);
	    }
	    break;
	  case 'T': fixed_index[3] = 2;
	    if(slicer_widget.maxs[3] && !XtIsManaged(slicer_widget.maxs[3])){
		XtManageChild(slicer_widget.maxs[3]);
		ival = (int)wmax[3];
		XmScaleSetValue(slicer_widget.maxs[3],ival);
	    }
	    break;
	}
    }
    (void)ui_update_(&WINDOW);
    switch(**client){
      case 'X':
	check_index(0);
	break;
      case 'Y': 
	check_index(1);
	break;
      case 'Z': 
	check_index(2);
	break;
      case 'T': 
	check_index(3);
	break;
    }
}


void im_gone_callback(w, client, call)
     Widget w;
     Widget *client;
     XmAnyCallbackStruct *call;
{
    *client = NULL;
}

    
void not_implemented(){
  make_help_widget_("This function is not currently implemented",42);
}


void fix_contour_button(w, client, call)
     Widget w;
     caddr_t client;
     XmAnyCallbackStruct *call;
{
    return;
}
/*
    Arg args[2];
    char type[30];
    XmString st;
    char st1[20];
    extern Widget contour_opts;
    int  error;
    
    (void)getavar_("plotyp",st1,&error,6,20);

    switch(*st1){
      case 'L':
      case 'l':
        sprintf(type,"Line P.\nOptions");
        break;
      case '2':
        if(*(st1+3)=='C'){
            sprintf(type,"Contour\nOptions");
            vector=0;
        }
        else if(*(st1+3)=='V'){
            sprintf(type,"Vector\nOptions");
            vector=1;
        }
        break;
      case 'V':
      case 'v':
        sprintf(type,"Vector\nOptions");
        vector=1;
        break;
      case 'S':
      case 's':
        if(fixed_index[0]+fixed_index[1]+fixed_index[2]+ fixed_index[3]
           == 3){
            sprintf(type,"Line P.\nOptions");
            vector=0;
            break;
        }
      default: sprintf(type,"Contour\nOptions");
    }

    st = NewString(type);
    XtSetArg(args[0],XmNlabelString,st);
    XtSetValues(contour_opts,args,1);
    XmStringFree(st);
}
*/
static struct {
    Widget w;
    int i;
    int inuse;
} window_typein_stuff={NULL,0,0};

void window_value_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    char *str, buff[256];
    Arg args[2];
    Widget widg;

    str = XmTextGetString(w);
    widg = window_typein_stuff.w;
    if(*str == '\0'){
	XtDestroyWidget(w);
	XtManageChild(widg);
	window_typein_stuff.inuse=0;
	return;
    }

    if (slicer_widget.mins[data] && slicer_widget.maxs[data]){
	switch (data){
	  case 0:
	    if(widg == slicer_widget.maxs[data])
		  sprintf(buff,"window = ,%s",str);
	    else if(XtIsManaged(slicer_widget.maxs[data]))
	      sprintf(buff,"window = %s",str);
	    else
	      sprintf(buff,"xloc = %s",str);
	    break;
	  case 1:
	    if(widg == slicer_widget.maxs[data])
		  sprintf(buff,"window = ,,,%s",str);
	    else if(XtIsManaged(slicer_widget.maxs[data]))
	      sprintf(buff,"window = ,,%s",str);
	    else
	      sprintf(buff,"yloc = %s",str);
	    break;
	  case 2:
	    if(widg == slicer_widget.maxs[data])
		  sprintf(buff,"window = ,,,,,%s",str);
	    else if(XtIsManaged(slicer_widget.maxs[data]))
	      sprintf(buff,"window = ,,,,%s",str);
	    else
	      sprintf(buff,"zloc = %s",str);
	    break;
	  case 3:
	    if(widg == slicer_widget.maxs[data])
		  sprintf(buff,"window = ,,,,,,,%s",str);
	    else if(XtIsManaged(slicer_widget.maxs[data]))
	      sprintf(buff,"window = ,,,,,,%s",str);
	    else
	      sprintf(buff,"time = %s",str);
	    break;
	}
	XtDestroyWidget(w);
	XtManageChild(widg);
	driver_notwin(buff);
    }
    else{
	XtDestroyWidget(w);
	XtManageChild(widg);
	sprintf(buff,"One of the widgets slicer_widget.mins[%d] or slicer_widget.maxs[%d] is missing\nPlease notify the IVE support group of this error",data,data);
	make_help_widget_(buff,(int)strlen(buff));

    }
    window_typein_stuff.inuse=0;
}

void window_scale_type_in(widg,data,ev)
     Widget widg;
     int data;
     XButtonEvent *ev;
{
    Widget w;
    void check_num2();
    void text_box_motion();
    unsigned long root,child;
    int x,y, rx,ry;
    unsigned int ret;
    if(window_typein_stuff.inuse == 1)return;
    window_typein_stuff.w = widg;
    window_typein_stuff.i = data;
    window_typein_stuff.inuse = 1;
  
    XQueryPointer(XtDisplay(widg),XtWindow(XtParent(widg)),&root,&child,
		  &rx,&ry,&x,&y,&ret);
    XtUnmanageChild(widg);
    
    w=XtVaCreateManagedWidget("VALUE",xmTextFieldWidgetClass,XtParent(widg),
			      XmNcolumns,5,XmNx,x-15,XmNy,y-15,
			      NULL);
    XtAddCallback(w,XmNactivateCallback,(XtCallbackProc)window_value_call,
		  (XtPointer)data);
    XtAddCallback(w,XmNmodifyVerifyCallback,check_num2,(XtPointer)0);
    XtAddCallback(w,XmNmotionVerifyCallback,text_box_motion,(XtPointer)0);
    XtAddEventHandler(w, ButtonPressMask, FALSE, 
		      (XtEventHandler)check_default_handler, (XtPointer)0);
    /*note 0 for data is to avoid reverse videoing the type in box. It is
      not necessary in this case.*/

}

void ive_move_win_handler(w,data,ev)
     Widget w;
     char *data;
     XButtonEvent *ev;
{
    if(ev->button == Button2){
	XtMoveWidget(w, ev->x, ev->y);
    }
}

void add_win_handler(w)
     Widget w;
{
    XtAddEventHandler(XtParent(w), ButtonPressMask|ButtonMotionMask, 
		      FALSE, 
		      (XtEventHandler)ive_move_win_handler,NULL);
    XGrabPointer(XtDisplay(w), XtWindow(XtParent(w)), TRUE,
		 ButtonPressMask|ButtonMotionMask,
		 GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
}

void ive_beg_tear_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    XtAppAddTimeOut(XtWidgetToApplicationContext(w),0,
		    (XtTimerCallbackProc)add_win_handler,w);
}

void ive_end_tear_call(w, data, call)
     Widget w;
     int data;
     XmAnyCallbackStruct *call;
{
    XtRemoveEventHandler(w, ButtonPressMask|ButtonMotionMask, FALSE, 
			 (XtEventHandler)ive_move_win_handler,NULL);
    XUngrabPointer(XtDisplay(w),CurrentTime);
}

void check_help_call(w,data,call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    XKeyEvent *ev;
    Widget  widg;
    char buff[256];

    ev  = &call->event->xkey;
    if((int)ev->window >0)
      widg=XtWindowToWidget(XtDisplay(w),ev->window);
    else
      widg=w;
    sprintf(buff,"help=%s",XtName(widg));
    driver(buff);
}
void check_help_handler(w,data,ev)
     Widget w;
     char *data;
     XKeyEvent *ev;
{
    KeySym ks;
    char *ksname;
    char str[256];

    if(ev->type !=KeyPress)return;
    XLookupString (ev, str, 255, &ks, NULL);
    ksname = XKeysymToString (ks);
    if(!strcmp(ksname,"F1") || !strcmp(ksname,"Help")){
	Widget  widg;
	if(ev->subwindow >0)
	  widg=XtWindowToWidget(XtDisplay(w),ev->subwindow);
	else 
	  widg=w;
	printf("%s\n",XtName(widg));
    }
}

