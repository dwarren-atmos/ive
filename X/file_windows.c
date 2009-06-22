/****************************************************************************
$Log: file_windows.c,v $
Revision 1.18  2006-10-05 16:50:08  reinecke
Added IVELOG environmental variable

Revision 1.17  2005/12/20 18:10:12  warren
Added environment variables IVEINPUTDIR and IVEINPUTMASK

Revision 1.16  1998/09/04 20:51:41  harry
Add colorbar button to annotation widget.  Rewrite file widgets to
remember previous entries.

Revision 1.15  1997/12/31 19:13:59  harry
Destroy the correct widget in file_ok_callback.

Revision 1.14  1997/07/15 17:41:42  warren
Increase beta number, get rid of empty popup that was left after file
widgets were gone.

Revision 1.13  1997/04/30 18:46:09  harry
Add widget to write out color table.

Revision 1.12  1997/04/11 18:46:39  warren
Casts on all X stuff and special checks for IRIX to keep SGI happy.

Revision 1.11  1997/04/09 22:37:52  warren
change done back to cancel and Ok on the printer for to Set Printer.
Also got rid of Done button on the Printer Setup widget.

Revision 1.10  1997/04/04 21:50:19  harry
Add XRaiseWindow for FILE and PROPERTIES window.  Add wording to FILE window.

Revision 1.9  1997/03/24 14:44:53  harry
Add widgets to file selection window for multiple files.

Revision 1.8  1996/10/18 23:22:50  harry
Fix declarations and calls to make SGI compiler happy.

Revision 1.7  1996/03/29 20:35:01  harry
Fix minor bug in IVECTB processing.

Revision 1.6  1996/03/29 17:46:52  harry
Use widgets instead of gadgets per suggestions in Motif Programming Manual.

Revision 1.5  1995/02/07 18:26:24  warren
update for new dec c compiler and add missing null to XtVaSetValues call.

 * Revision 1.4  1994/11/22  22:46:30  harry
 * Correct vector locking, add support for vector positioning, add
 * support for filled contour overlays.
 *
 * Revision 1.3  1994/06/14  05:45:19  harry
 * Mods fo Solaris 2.
 *
 * Revision 1.2  1993/07/30  22:48:46  warren
 * Added widget to eps button to save EPS files.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:18  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.3  1992/12/23  18:55:03  harry
 * Add log file processing, change dump file default from XWD to GIF.
 *
 * Revision 2.2  1992/12/22  18:45:40  harry
 * Fix bug in set_file_window.
 *
 * Revision 2.1  1992/12/14  18:38:30  harry
 * Add widgets for dump and input.
 *
 * Revision 2.0  1992/07/01  21:11:24  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.5  1992/06/17  16:19:50  warren
 * added ;'s
 *
 * Revision 1.4  1992/06/17  16:16:20  warren
 * added default of /usr/local/lib/ive/ to trans and ctb startups.
 *
 * Revision 1.3  1992/06/11  22:54:21  warren
 * fixed fix.
 *
 * Revision 1.2  1992/06/11  22:42:15  warren
 * allow for ive in name instead of uwgap
 *
 * Revision 1.1  1992/04/21  00:08:28  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: file_windows.c,v 1.18 2006-10-05 16:50:08 reinecke Exp $";

#include <Xm/XmAll.h>
#include <sys/param.h>
#include <stdio.h>
#include <file_widgets.h>
#include <stdlib.h>

#ifndef LIBDIR
#define LIBDIR "/usr/local/lib/ive/"
#endif

struct dir_widget dir_widget = {0, 0, 1, NULL, NULL,
				{NULL, NULL, NULL, NULL, NULL},
				NULL, NULL, NULL, NULL, {NULL, NULL, NULL}};

extern XmString NewString();

void set_file_box(widget, file, fsb)
Widget widget;
char *file;
struct fsb *fsb;
{
    XmString str;
    char *p, q;

    XmStringFree(fsb->directory);
    XmStringFree(fsb->dirSpec);
    if ((p=strrchr(file, '/')) == NULL)
	fsb->directory = XmStringCreate("./", XmSTRING_DEFAULT_CHARSET);
    else {
	q = *p;
	*p = '\0';
	fsb->directory = XmStringCreate(file, XmSTRING_DEFAULT_CHARSET);
	*p = q;
    }
    fsb->dirSpec = XmStringCreate(file, XmSTRING_DEFAULT_CHARSET);
    if (widget) XtVaSetValues(widget, XmNdirectory, fsb->directory,
			      XmNdirSpec, fsb->dirSpec, NULL);
}    

void init_dir()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    void dir_start_type_in(), dir_start_call(),
	dir_end_type_in(), dir_end_call(), dir_coord_call();
    Widget popup, cancel, dirtemp, radio_box, dirstart, dirend, line;
    Arg args[8];
    void file_ok_callback();
    void done_callback();
    void call_driver();
    FILE *file;
    char *p;
    int i;

    if(dir_widget.List){
	XtManageChild(XtParent(XtParent(dir_widget.List)));
	XRaiseWindow(XtDisplay(dir_widget.List),
		     XtWindow(XtParent(XtParent(XtParent(dir_widget.List)))));
	return;
    }
    XtSetArg(args[0],XmNautoUnmanage,FALSE);
    XtSetArg(args[1],XtNallowShellResize,TRUE);
    popup = XtVaCreatePopupShell("File",transientShellWidgetClass,Box,
				 XmNautoUnmanage, FALSE,
				 XtNallowShellResize, TRUE,
/*				 XmNwidth, 254, XmNheight, 400, */
				 NULL);

/*
   Extra Form Widget needed by Motif.
   */
    
    dirtemp = XtVaCreateManagedWidget("File",
				      xmFormWidgetClass,
				      popup,
				      XmNresizable, TRUE,
				      XmNautoUnmanage, FALSE, NULL);

    dir_widget.dir_form = XtVaCreateWidget("File",
					   xmFormWidgetClass,
					   dirtemp,
					   XmNtopAttachment,
					   XmATTACH_FORM,
					   XmNbottomAttachment,
					   XmATTACH_FORM,
					   XmNleftAttachment,
					   XmATTACH_FORM,
					   XmNrightAttachment,
					   XmATTACH_FORM,
					   XmNmarginWidth, 5,
					   XmNborderWidth, 1,
					   XmNresizable, TRUE,
					   XmNresizePolicy,
					   XmRESIZE_ANY,
					   NULL);
    XtManageChild(dir_widget.dir_form);
    if (dir_widget.fsb.pattern == NULL) {
	char *file_curdir, *file_mask, *file_curlog; 

	file_curdir=(char *)getenv("IVEDAT");
	if(!file_curdir)file_curdir=(char *)getenv("ivedat");
	if(!file_curdir)file_curdir=(char *)getenv("UWGAPDAT");
	if(!file_curdir)file_curdir=(char *)getenv("uwgapdat");

	file_curlog=(char *)getenv("IVELOG");
	if(!file_curdir)file_curdir=(char *)getenv("ivelog");
    
	file_mask=(char *)getenv("IVEMASK");
	if(!file_mask)file_mask=(char *)getenv("ivemask");
	if(!file_mask)file_mask=(char *)getenv("UWGAPMASK");
	if(!file_mask)file_mask=(char *)getenv("uwgapmask");
	if(!file_mask)file_mask = "*";

	dir_widget.fsb.directory 
	    = XmStringCreate(file_curdir, XmSTRING_DEFAULT_CHARSET);
	dir_widget.fsb.dirSpec = XmStringCopy(dir_widget.fsb.directory);
	dir_widget.fsb.pattern
	    = XmStringCreate(file_mask, XmSTRING_DEFAULT_CHARSET);
    }
    /* setup and manage dir_widget.List */
	    
    XtSetArg(args[0],XmNdirectory, dir_widget.fsb.directory);
    XtSetArg(args[1],XmNdirSpec, dir_widget.fsb.dirSpec);
    XtSetArg(args[2],XmNpattern, dir_widget.fsb.pattern);
    
    dir_widget.List = 
      XmCreateFileSelectionBox(dir_widget.dir_form,"Dirlist",args,3);
    XtVaSetValues(dir_widget.List, XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(dir_widget.List);

    cancel = XmFileSelectionBoxGetChild(dir_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    XtSetArg(args[0],XmNlabelString,
	     XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
    XtSetValues(cancel,args,1);
    */  
    XtAddCallback(dir_widget.List,XmNokCallback,file_ok_callback,
		  (XtPointer)popup);
    XtAddCallback(dir_widget.List,XmNcancelCallback,done_callback,
		  (XtPointer)popup);
    XtAddCallback(dir_widget.List,XmNhelpCallback,call_driver,"HELP=FILE");
    line = XtVaCreateManagedWidget("Line", xmSeparatorWidgetClass,
				   dir_widget.dir_form,
				   XmNleftAttachment, XmATTACH_FORM,
				   XmNrightAttachment, XmATTACH_FORM,
				   XmNtopAttachment, XmATTACH_WIDGET,
				   XmNtopWidget, dir_widget.List, NULL);
/*
  Buttons for multiple file dimension.
  */
    
    dirtemp = XtVaCreateManagedWidget("File\nCoord", xmLabelWidgetClass,
			       dir_widget.dir_form,
			       XmNleftAttachment, XmATTACH_FORM,
			       XmNtopAttachment, XmATTACH_WIDGET,
			       XmNtopWidget, line, NULL);
    radio_box = XmCreateRadioBox(dir_widget.dir_form, "DirBox", NULL, 0);
    XtVaSetValues(radio_box, XmNorientation, XmHORIZONTAL,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, dirtemp,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, line, NULL);
    dir_widget.button[0] = XtVaCreateWidget("NONE", xmToggleButtonWidgetClass,
					    radio_box, NULL);
    dir_widget.button[1] = XtVaCreateWidget("X", xmToggleButtonWidgetClass,
				 radio_box, NULL);
    dir_widget.button[2] = XtVaCreateWidget("Y", xmToggleButtonWidgetClass,
				 radio_box, NULL);
    dir_widget.button[3] = XtVaCreateWidget("Z", xmToggleButtonWidgetClass,
				 radio_box, NULL);
    dir_widget.button[4] = XtVaCreateWidget("T", xmToggleButtonWidgetClass,
				 radio_box, NULL);
    for (i=0; i < 5; i++) {
	XtAddCallback(dir_widget.button[i], XmNarmCallback,
		      dir_coord_call, (XtPointer)(i+1));
	XtManageChild(dir_widget.button[i]);
    }
    XtVaSetValues(dir_widget.button[0], XmNset, TRUE, NULL);
    XtManageChild(radio_box);
    XtAddCallback(radio_box,XmNhelpCallback,call_driver,"HELP=MULTIPLE_READ");
/*
  Start value slider.
  */
    dir_widget.start = XtVaCreateWidget("Start Value2",
					xmScaleWidgetClass,
					dir_widget.dir_form,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, radio_box,
					XmNx, 90,
					XmNorientation, XmHORIZONTAL, 
					XmNshowValue, TRUE, NULL);
    dir_widget.startlab = 
	XtVaCreateWidget("Start Value", xmLabelWidgetClass,
			 dir_widget.dir_form,
			 XmNx, 0,
			 XmNrightAttachment, XmATTACH_WIDGET,
			 XmNrightWidget, dir_widget.start,
			 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			 XmNbottomWidget, dir_widget.start,
			 NULL);
    XtAddRawEventHandler(dir_widget.start,
			 ButtonPressMask,FALSE,
			 dir_start_type_in,0);
    XtAddCallback(dir_widget.start, XmNvalueChangedCallback,
		  dir_start_call, NULL);
/*
  End value.
  */

    dir_widget.end = XtVaCreateWidget("End Value2",
				      xmScaleWidgetClass,
				      dir_widget.dir_form,
				      XmNtopAttachment, XmATTACH_WIDGET,
				      XmNtopWidget, dir_widget.start,
				      XmNx, 90,
				      XmNorientation, XmHORIZONTAL, 
				      XmNshowValue, TRUE, NULL);
    dir_widget.endlab = 
	XtVaCreateWidget("End Value", xmLabelWidgetClass,
			 dir_widget.dir_form,
			 XmNrightAttachment, XmATTACH_WIDGET,
			 XmNrightWidget, dir_widget.end,
			 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			 XmNbottomWidget, dir_widget.end,
			 NULL);
    XtAddRawEventHandler(dir_widget.end,
			 ButtonPressMask,FALSE,
			 dir_end_type_in,0);
    XtAddCallback(dir_widget.end, XmNvalueChangedCallback,
		  dir_end_call, NULL);

    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&dir_widget.List);
    XRaiseWindow(XtDisplay(popup),XtWindow(popup));
      }

struct trans_widget trans_widget = {NULL, {NULL, NULL, NULL}};

void init_trans()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel;
    Arg args[8];
    void trans_ok_callback();
    void done_callback();
    void call_driver();
    FILE *file;

    if(trans_widget.List){
	XtManageChild(XtParent(XtParent(trans_widget.List)));
	return;
    }
    XtSetArg(args[0],XmNautoUnmanage,FALSE);
    XtSetArg(args[1],XtNallowShellResize,TRUE);
    popup = XtCreatePopupShell("Transform",transientShellWidgetClass,
			       Box,args,2);

    if (trans_widget.fsb.pattern == NULL) {
	char *trans_curdir;

	trans_curdir=(char *)getenv("IVETRANS");
	if(!trans_curdir)trans_curdir=(char *)getenv("ivetrans");
	if(!trans_curdir)trans_curdir=(char *)getenv("UWGAPTRANS");
	if(!trans_curdir)trans_curdir=(char *)getenv("uwgaptrans");
	if(!trans_curdir)trans_curdir=LIBDIR;

	trans_widget.fsb.directory 
	    = XmStringCreate(trans_curdir, XmSTRING_DEFAULT_CHARSET);
	trans_widget.fsb.dirSpec = XmStringCopy(trans_widget.fsb.directory);
	trans_widget.fsb.pattern
	    = XmStringCreate("*.o", XmSTRING_DEFAULT_CHARSET);
    }
    /* setup and manage trans_widget.List */
    XtSetArg(args[0],XmNdirectory, trans_widget.fsb.directory);
    XtSetArg(args[1],XmNdirSpec, trans_widget.fsb.dirSpec);
    XtSetArg(args[2],XmNpattern, trans_widget.fsb.pattern);
    
    trans_widget.List = 
      XmCreateFileSelectionBox(popup,"Translist",args,3);
    XtManageChild(trans_widget.List);

    cancel = XmFileSelectionBoxGetChild(trans_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    XtSetArg(args[0],XmNlabelString,
	     XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
    XtSetValues(cancel,args,1);
*/
    XtAddCallback(trans_widget.List,XmNokCallback,trans_ok_callback,NULL);
    XtAddCallback(trans_widget.List,XmNcancelCallback,done_callback,
		  (XtPointer)popup);
    XtAddCallback(trans_widget.List,XmNhelpCallback,call_driver,
		  "HELP=TRANSFORM");
    
    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &trans_widget.List);
      }


struct ctb_widget ctb_widget = {NULL, {NULL, NULL, NULL}};

void init_ctb()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel;
    Arg args[8];
    void ctb_ok_callback();
    void done_callback();
    void call_driver();
    FILE *file;

    if(ctb_widget.List){
	XtManageChild(XtParent(XtParent(ctb_widget.List)));
	return;
    }
    XtSetArg(args[0],XmNautoUnmanage,FALSE);
    XtSetArg(args[1],XtNallowShellResize,TRUE);
    popup = XtCreatePopupShell("Color table",transientShellWidgetClass,Box,
			       args,2);

    if (ctb_widget.fsb.pattern == NULL) {
	char *ctb_curdir;

	ctb_curdir=(char *)getenv("IVECTB");
	if(!ctb_curdir)ctb_curdir=(char *)getenv("ivectb");
	if(!ctb_curdir)ctb_curdir=(char *)getenv("UWGAPCTB");
	if(!ctb_curdir)ctb_curdir=(char *)getenv("uwgapctb");
	if(!ctb_curdir)ctb_curdir=LIBDIR;

	ctb_widget.fsb.directory 
	    = XmStringCreate(ctb_curdir, XmSTRING_DEFAULT_CHARSET);
	ctb_widget.fsb.dirSpec = XmStringCopy(ctb_widget.fsb.directory);
	ctb_widget.fsb.pattern 
	    = XmStringCreate("*.ctb", XmSTRING_DEFAULT_CHARSET);
    }

    /* setup and manage ctb_widget.List */
    XtSetArg(args[0],XmNdirectory, ctb_widget.fsb.directory);
    XtSetArg(args[1],XmNdirSpec, ctb_widget.fsb.dirSpec);
    XtSetArg(args[2],XmNpattern, ctb_widget.fsb.pattern);
    
    ctb_widget.List = 
      XmCreateFileSelectionBox(popup,"Ctblist",args,3);
    XtManageChild(ctb_widget.List);

    cancel = XmFileSelectionBoxGetChild(ctb_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    XtSetArg(args[0],XmNlabelString,
	     XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
    XtSetValues(cancel,args,1);
*/    
    XtAddCallback(ctb_widget.List,XmNokCallback,ctb_ok_callback,NULL);
    XtAddCallback(ctb_widget.List,XmNcancelCallback,done_callback,
		  (XtPointer)popup);
    XtAddCallback(ctb_widget.List,XmNhelpCallback,call_driver,"HELP=READ");
    
    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&ctb_widget.List);
      }

struct wctb_widget wctb_widget = {NULL, {NULL, NULL, NULL}};
void init_wctb()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel;
    Arg args[8];
    void wctb_ok_callback();
    void done_callback();
    void call_driver();
    FILE *file;

    if(wctb_widget.List){
	XtManageChild(XtParent(XtParent(wctb_widget.List)));
	return;
    }
    XtSetArg(args[0],XmNautoUnmanage,FALSE);
    XtSetArg(args[1],XtNallowShellResize,TRUE);
    popup = XtCreatePopupShell("WColor table",transientShellWidgetClass,Box,
			       args,2);

    if (wctb_widget.fsb.pattern == NULL) {
	char *wctb_curdir;

	wctb_curdir=(char *)getenv("IVECTB");
	if(!wctb_curdir)wctb_curdir=(char *)getenv("ivectb");
	if(!wctb_curdir)wctb_curdir=(char *)getenv("UWGAPCTB");
	if(!wctb_curdir)wctb_curdir=(char *)getenv("uwgapctb");
	if(!wctb_curdir)wctb_curdir=LIBDIR;

	wctb_widget.fsb.directory 
	    = XmStringCreate(wctb_curdir, XmSTRING_DEFAULT_CHARSET);
	wctb_widget.fsb.dirSpec = XmStringCopy(wctb_widget.fsb.directory);
	wctb_widget.fsb.pattern
	    = XmStringCreate("*.ctb", XmSTRING_DEFAULT_CHARSET);
    }
    /* setup and manage Ctb_list */
    XtSetArg(args[0],XmNdirectory, wctb_widget.fsb.directory);
    XtSetArg(args[1],XmNdirSpec, wctb_widget.fsb.dirSpec);
    XtSetArg(args[2],XmNpattern, wctb_widget.fsb.pattern);
    
    wctb_widget.List = 
      XmCreateFileSelectionBox(popup,"WCtblist",args,3);
    XtManageChild(wctb_widget.List);

    cancel = XmFileSelectionBoxGetChild(wctb_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    XtSetArg(args[0],XmNlabelString,
	     XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
    XtSetValues(cancel,args,1);
*/    
    XtAddCallback(wctb_widget.List,XmNokCallback,wctb_ok_callback,NULL);
    XtAddCallback(wctb_widget.List,XmNcancelCallback,done_callback,
		  (XtPointer)popup);
    XtAddCallback(wctb_widget.List,XmNhelpCallback,call_driver,
		  "HELP=WRITECOLORTABLE");
    
    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&wctb_widget.List);
}

struct input_widget input_widget = {NULL, {NULL, NULL, NULL}};
/*needed for ui_update*/

void init_input()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel;
    Arg args[8];
    XmString str;
    void input_ok_callback();
    void done_callback();
    void call_driver();
    FILE *file;
    char *file_curdir,*file_mask, *p, q;

    if(input_widget.List){
	XtManageChild(XtParent(XtParent(input_widget.List)));
	return;
    }
    file_curdir=(char *)getenv("IVEINPUTDIR");
    if(!file_curdir)file_curdir=(char *)getenv("iveinputdir");
    if(!file_curdir)file_curdir=".";
    
    file_mask=(char *)getenv("IVEINPUTMASK");
    if(!file_mask)file_mask=(char *)getenv("iveinputmask");
    if(!file_mask)file_mask = "*";

    XtSetArg(args[0],XmNautoUnmanage,FALSE);
    XtSetArg(args[1],XtNallowShellResize,TRUE);
    popup = XtCreatePopupShell("Input",transientShellWidgetClass,Box,args,2);

    if (input_widget.fsb.pattern == NULL){
      input_widget.fsb.pattern 
	= XmStringCreate(file_mask, XmSTRING_DEFAULT_CHARSET);
      input_widget.fsb.directory 
	= XmStringCreate(file_curdir, XmSTRING_DEFAULT_CHARSET);
      input_widget.fsb.dirSpec = XmStringCopy(input_widget.fsb.directory);
    }
    input_widget.List = 
	XtVaCreateManagedWidget("Inputlist", xmFileSelectionBoxWidgetClass,
				popup, XmNpattern, input_widget.fsb.pattern,
				XmNdirectory, input_widget.fsb.directory,
				XmNdirSpec, input_widget.fsb.dirSpec,
				NULL);

    cancel = XmFileSelectionBoxGetChild(input_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    XtSetArg(args[0],XmNlabelString,
	     XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
    XtSetValues(cancel,args,1);
*/    
    XtAddCallback(input_widget.List,XmNokCallback,input_ok_callback,NULL);
    XtAddCallback(input_widget.List,XmNcancelCallback,done_callback,
		  (XtPointer)popup);
    XtAddCallback(input_widget.List,XmNhelpCallback,call_driver,"HELP=INPUT");
    
    XtManageChild(popup);

    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &input_widget.List);
}

struct dump_widget dump_widget = {"GIF", "plot", NULL, NULL, NULL,
				{NULL, NULL, NULL}};

void init_dump()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel,bb;
    XmString str;
    Arg args[8];
    void dump_ok_callback(), dump_type_callback(), dump_what_callback();
    void dump_done_callback(), dump_help_callback();
    FILE *file;
    char *file_mask, *p;

    if(dump_widget.List){
	XtManageChild(XtParent(XtParent(dump_widget.List)));
	return;
    }
    popup = XtVaCreatePopupShell("Dump", transientShellWidgetClass,
				 Box,
				 XmNautoUnmanage, FALSE,
				 XtNallowShellResize, FALSE,
				 XmNwidth, 278, XmNheight, 420, NULL);
    XtSetArg(args[0], XmNmarginWidth, 2);
    XtSetArg(args[1], XmNpacking, XmPACK_NONE);
    bb = XmCreateBulletinBoard(popup, "Dump", args, 2);
    XtManageChild(bb);
    if (dump_widget.fsb.pattern == NULL)
	dump_widget.fsb.pattern 
	    = XmStringCreate("*", XmSTRING_DEFAULT_CHARSET);
    dump_widget.List = 
	XtVaCreateManagedWidget("Dumplist", xmFileSelectionBoxWidgetClass,
				bb, XmNwidth, 278,
				XmNheight, 372,
				XmNresizePolicy, XmRESIZE_NONE,
				XmNpattern, dump_widget.fsb.pattern,
				XmNdirectory, dump_widget.fsb.directory,
				XmNdirSpec, dump_widget.fsb.dirSpec,
				NULL);

    cancel = XmFileSelectionBoxGetChild(dump_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    XtSetArg(args[0],XmNlabelString,
	     XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
    XtSetValues(cancel,args,1);
*/    
    XtAddCallback(dump_widget.List, XmNokCallback,dump_ok_callback,NULL);
    XtAddCallback(dump_widget.List, XmNcancelCallback,dump_done_callback,NULL);
    XtAddCallback(dump_widget.List, XmNhelpCallback,dump_help_callback,NULL);

    /* row 2 */

    str = XmStringCreate("Type:", XmSTRING_DEFAULT_CHARSET);
    XtVaCreateManagedWidget("Type:", xmLabelWidgetClass, bb,
			    XmNx, 10, XmNy, 395,
			    XmNlabelString, str, NULL);
    XmStringFree(str);
    str = XmStringCreate(dump_widget.type, XmSTRING_DEFAULT_CHARSET);
    dump_widget.Type = XtVaCreateManagedWidget(dump_widget.type,
					       xmPushButtonWidgetClass,
					       bb,
					       XmNx, 50, XmNy, 390,
					       XmNlabelString, str, NULL);
    XtAddCallback(dump_widget.Type, XmNactivateCallback,
		  dump_type_callback, NULL);
    XmStringFree(str);
    str = XmStringCreate("What:", XmSTRING_DEFAULT_CHARSET);
    XtVaCreateManagedWidget("What:", xmLabelWidgetClass, bb,
			    XmNx, 140, XmNy, 395,
			    XmNlabelString, str, NULL);
    XmStringFree(str);
    str = XmStringCreate(dump_widget.what, XmSTRING_DEFAULT_CHARSET);
    dump_widget.What = XtVaCreateManagedWidget(dump_widget.what,
					       xmPushButtonWidgetClass,
					       bb,
					       XmNx, 180, XmNy, 390,
					       XmNlabelString, str, NULL);
    XmStringFree(str);
    XtAddCallback(dump_widget.What, XmNactivateCallback,
		  dump_what_callback, NULL);
    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&dump_widget.List);
}

struct log_widget log_widget = {NULL, {NULL, NULL, NULL}};
/*needed for ui_update*/

void init_log()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel;
    Arg args[8];
    XmString str;
    void log_ok_callback();
    void done_callback();
    void call_driver();
    FILE *file;
    char *file_mask, *p, q;

    if(log_widget.List){
	XtManageChild(XtParent(log_widget.List));
	return;
    }
    XtSetArg(args[0],XmNautoUnmanage,FALSE);
    XtSetArg(args[1],XtNallowShellResize,TRUE);
    popup = XtCreatePopupShell("Log",transientShellWidgetClass,Box,args,2);

    if (log_widget.fsb.pattern == NULL)
	log_widget.fsb.pattern 
	    = XmStringCreate("*", XmSTRING_DEFAULT_CHARSET);
    log_widget.List = 
	XtVaCreateManagedWidget("Loglist", xmFileSelectionBoxWidgetClass,
				popup, XmNpattern, log_widget.fsb.pattern,
				XmNdirectory, log_widget.fsb.directory,
				XmNdirSpec, log_widget.fsb.dirSpec,
				NULL);

    cancel = XmFileSelectionBoxGetChild(log_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
  XtSetArg(args[0],XmNlabelString,
  XmStringCreate("Done",XmSTRING_DEFAULT_CHARSET));
  XtSetValues(cancel,args,1);
*/
    XtAddCallback(log_widget.List,XmNokCallback,log_ok_callback,NULL);
    XtAddCallback(log_widget.List,XmNcancelCallback,done_callback,(XtPointer)popup);
    XtAddCallback(log_widget.List,XmNhelpCallback,call_driver,"HELP=LOG");
    
    XtManageChild(popup);

    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,
		  &log_widget.List);
}


struct eps_widget eps_widget = {{'\0'}, NULL, NULL, {NULL, NULL, NULL}};
/*needed for ui_update*/

void init_eps()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel,bb;
    XmString str;
    Arg args[8];
    void eps_ok_callback(), eps_color_callback();
    void dump_done_callback();
    void call_driver();
    FILE *file;
    char *file_mask, *p;

    if(eps_widget.List){
	XtManageChild(XtParent(XtParent(eps_widget.List)));
	return;
    }
    popup = XtVaCreatePopupShell("Eps", transientShellWidgetClass,
				 Box,
				 XmNautoUnmanage, FALSE,
				 XtNallowShellResize, FALSE,
				 XmNwidth, 278, XmNheight, 420, NULL);
    XtSetArg(args[0], XmNmarginWidth, 2);
    XtSetArg(args[1], XmNpacking, XmPACK_NONE);
    bb = XmCreateBulletinBoard(popup, "Eps", args, 2);
    XtManageChild(bb);
    if (eps_widget.fsb.pattern == NULL)
	eps_widget.fsb.pattern 
	    = XmStringCreate("*", XmSTRING_DEFAULT_CHARSET);
    eps_widget.List = 
	XtVaCreateManagedWidget("Epslist", xmFileSelectionBoxWidgetClass,
				bb, XmNwidth, 278,
				XmNheight, 372,
				XmNresizePolicy, XmRESIZE_NONE,
				XmNpattern, eps_widget.fsb.pattern,
				XmNdirectory, eps_widget.fsb.directory,
				XmNdirSpec, eps_widget.fsb.dirSpec,
				NULL);
    cancel = XmFileSelectionBoxGetChild(eps_widget.List,
					XmDIALOG_CANCEL_BUTTON);
/*
    str = NewString("Done");
    XtVaSetValues(cancel,XmNlabelString,str,NULL);
    XmStringFree(str);
*/
    
    XtAddCallback(eps_widget.List, XmNokCallback,eps_ok_callback,NULL);
    XtAddCallback(eps_widget.List, XmNcancelCallback,dump_done_callback,NULL);
    XtAddCallback(eps_widget.List, XmNhelpCallback,
		  call_driver, "help=writeframe");

    /* row 2 */

    str = XmStringCreate("Color Lines ", XmSTRING_DEFAULT_CHARSET);
    XtVaCreateManagedWidget("Color:", xmLabelWidgetClass, bb,
			    XmNx, 10, XmNy, 395,
			    XmNlabelString, str, NULL);
    XmStringFree(str);
    if(eps_widget.color[0] != '\0')
      str = NewString(eps_widget.color);
    else
       str = NewString("No ");
    eps_widget.color_w = XtVaCreateManagedWidget(eps_widget.color,
						 xmPushButtonWidgetClass,
						 bb,
						 XmNx, 85, XmNy, 390,
						 XmNlabelString, str, NULL);
    XtAddCallback(eps_widget.color_w, XmNactivateCallback,
		  eps_color_callback, NULL);
    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&eps_widget.List);
    XmStringFree(str);
}

struct pdf_widget pdf_widget = {NULL, {NULL, NULL, NULL}};
/*needed for ui_update*/

void init_pdf()
{
    extern Widget Box;
    void done_pparent_callback(),im_gone_callback();
    Widget popup,cancel,bb;
    Arg args[8];
    void pdf_ok_callback();
    void dump_done_callback();
    void call_driver();
    FILE *file;
    char *file_mask, *p;

    if(pdf_widget.List){
	XtManageChild(XtParent(XtParent(pdf_widget.List)));
	return;
    }
    popup = XtVaCreatePopupShell("PDF", transientShellWidgetClass,
				 Box,
				 XmNautoUnmanage, FALSE,
				 XtNallowShellResize, FALSE,
				 XmNwidth, 278, XmNheight, 420, NULL);
    XtSetArg(args[0], XmNmarginWidth, 2);
    XtSetArg(args[1], XmNpacking, XmPACK_NONE);
    bb = XmCreateBulletinBoard(popup, "PDF", args, 2);
    XtManageChild(bb);
    if (pdf_widget.fsb.pattern == NULL)
	pdf_widget.fsb.pattern 
	    = XmStringCreate("*.pdf", XmSTRING_DEFAULT_CHARSET);
    pdf_widget.List = 
	XtVaCreateManagedWidget("PDFlist", xmFileSelectionBoxWidgetClass,
				bb, XmNwidth, 278,
				XmNheight, 372,
				XmNresizePolicy, XmRESIZE_NONE,
				XmNpattern, eps_widget.fsb.pattern,
				XmNdirectory, eps_widget.fsb.directory,
				XmNdirSpec, eps_widget.fsb.dirSpec,
				NULL);
    cancel = XmFileSelectionBoxGetChild(pdf_widget.List,
					XmDIALOG_CANCEL_BUTTON);
    XtAddCallback(pdf_widget.List, XmNokCallback,pdf_ok_callback,NULL);
    XtAddCallback(pdf_widget.List, XmNcancelCallback,dump_done_callback,NULL);
    XtAddCallback(pdf_widget.List, XmNhelpCallback,
		  call_driver, "help=writeframe");

    XtManageChild(popup);
    XtAddCallback(popup,XmNdestroyCallback,im_gone_callback,&pdf_widget.List);
}



