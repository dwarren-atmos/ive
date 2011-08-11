/*
 * $Id: save_window.c,v 1.4 2006/08/01 19:55:08 warren Exp $
 * This routine saves the pixmap of the graphics window, and displays it in
 * a new window with a "Done" button.
 *
 * $Log: save_window.c,v $
 * Revision 1.4  2006/08/01 19:55:08  warren
 * Stay within the array bounds when creating saved pixmaps.
 *
 * Revision 1.3  2006/04/14 19:42:43  warren
 * Made save window work on linux despite lack of save under and backingstore.
 *
 * Revision 1.2  2000/01/27 21:19:31  warren
 * Show plot number in save window
 *
 * Revision 1.1  1995/05/26  15:20:12  harry
 * Add SAVE_WINDOW command and widget.
 *
 *
 */
static char ident[] = "$Id: save_window.c,v 1.4 2006/08/01 19:55:08 warren Exp $";

#include <malloc.h>
#include <Xm/XmAll.h>
#include <X11/Xmu/Xmu.h>
#include <ive.h>

static GC save_gc=NULL;
static Pixmap pm_arr[20]={(Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,
			  (Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,
			  (Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,
			  (Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,
			  (Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,
			  (Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL,(Pixmap)NULL};

extern Widget xgks_widget, Toplevel;
extern void getivar_(),update_all_();

void save_quit_call(w, data, call)
    Widget w;
    Widget *data;
    XmAnyCallbackStruct *call;
{
    XtDestroyWidget(*data);
}

void save_destroy_call(w, data, call)
    Widget w;
    Widget *data;
    XmAnyCallbackStruct *call;
{
    free(data);
}

void save_cleanup_call(w, data, call)
    Widget w;
    unsigned long data;
    XmAnyCallbackStruct *call;
{
  if(pm_arr[data] != (Pixmap)NULL){
    XFreePixmap(XtDisplay(w),pm_arr[data]);
    pm_arr[data]=(Pixmap)NULL;
  }
}

void save_repaint_call(w, data, call)
    Widget w;
    unsigned long data;
    XmAnyCallbackStruct *call;
{
  int height,width;
  XtVaGetValues(w,XmNheight, &height, XmNwidth,&width,NULL);
  XCopyArea(XtDisplay(w), pm_arr[data], XtWindow(w),
	    DefaultGC(XtDisplay(w), DefaultScreen(XtDisplay(w))),
	    0,0, width, height, 0,0);

}

void save_window_()

{
    Display *dpy = XtDisplay(xgks_widget);
    Window win = XtWindow(xgks_widget), root, parent, *children, swin;
    XSetWindowAttributes xswa;
    Widget widget, bwidget, form;
    Widget *save_win;
    unsigned long i;
    unsigned int nchildren;
    XWindowAttributes win_info;
    Position x, y;
    int xx, yy, height, width, depth, curplot, error;
    char curpltnum[40];
    save_win = (Widget *) malloc (sizeof(Widget));
    XQueryTree(dpy, win, &root, &parent, &children, &nchildren);
    XFree(children);
    XGetWindowAttributes(dpy, parent, &win_info);
    XTranslateCoordinates(dpy, parent, win_info.root, 0, 0, 
			  &xx, &yy, &swin);
    x = xx-win_info.x;
    y = yy-win_info.y;
    height = win_info.height;
    width = win_info.width;
    depth = win_info.depth;
    for(i=0; i<20; i++){
      if (pm_arr[i]==(Pixmap)0)
	break;
    }
    if(i==20 || pm_arr[i] != (Pixmap)0){
      make_help_widget_("Gone beyond max of 20 simultaneous saved windows");
      return;
    }
    pm_arr[i]=XCreatePixmap (dpy,win,width, height,DefaultDepth(dpy,DefaultScreen(dpy)));


    XRaiseWindow(dpy,XtWindow(XtParent(xgks_widget)));
    XSync(dpy, 0);
    XSync(dpy, 0);
    XSync(dpy, 0);
    XCopyArea(dpy, win, pm_arr[i], DefaultGC(dpy, DefaultScreen(dpy)), 0, 0, width,
	      height, 0, 0);
    (void)getivar_("curplot", &curplot, &error, 7);
    if(!error)
      sprintf(curpltnum, "IVE save_win frame %d",curplot);
    else
      sprintf(curpltnum, "IVE save_win frame unknown");
    *save_win = XtVaCreatePopupShell(
				     curpltnum,
				     transientShellWidgetClass,
				     Toplevel, XmNheight, height+30,
				     XmNwidth, width, XmNx, x, XmNy, y,
				     NULL);
    XtAddCallback(*save_win, XmNdestroyCallback, 
		  (XtCallbackProc)save_destroy_call,
		  save_win);
    XtAddCallback(*save_win, XmNdestroyCallback, 
		  (XtCallbackProc)save_cleanup_call,
		  (void *)i);
    form = XtVaCreateManagedWidget("saveform", xmFormWidgetClass,
				   *save_win, NULL);
    widget = XtVaCreateManagedWidget("ive_save", xmDrawingAreaWidgetClass,
				     form,
				     XmNheight, height, XmNwidth, width,
				     XmCResizePolicy, XmRESIZE_NONE, NULL);
    bwidget = XtVaCreateManagedWidget("Done", xmPushButtonWidgetClass,
				      form,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_WIDGET,
				      XmNtopWidget, widget,
				      NULL);
    XtAddCallback(bwidget, XmNactivateCallback, 
		 (XtCallbackProc) save_quit_call, save_win);
    XtAddCallback(widget, XmNexposeCallback, 
		  (XtCallbackProc)save_repaint_call, (void *)i);
    XtManageChild(*save_win);
    XtPopup(*save_win, XtGrabNone);
    XSync(dpy, 0);
    swin = XtWindow(widget);
    xswa.backing_store = Always;
    XChangeWindowAttributes(dpy, swin, (unsigned long) CWBackingStore,
			    &xswa);
    XCopyArea(dpy, win, swin, DefaultGC(dpy, DefaultScreen(dpy)), 0, 0, width,
	      height, 0, 0);
    XSync(dpy,0);
}
