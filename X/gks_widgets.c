/****************************************************************************
$Log: gks_widgets.c,v $
Revision 1.37  2002/12/26 22:48:11  warren
New gif encoder (faster and easier to use), also fixed gif encoding
to work from truecolor screens.

Revision 1.36  2002/12/09 18:03:09  warren
Instead of checking number of plane every time, just check once and record
if it is true color in bool.

Revision 1.35  2002/05/30 16:42:43  warren
add a "grayline" command to use when you will be printing in black and white,
and added an FPE handler when initializing windows to deal with an FPE out
of X.

Revision 1.34  2002/05/23 19:41:07  davidive
Fixed last crash the correct way. Don't write to dim array with index = -1

Revision 1.33  2002/05/22 23:53:19  davidive
Added 2 extra g_widg entries to quick fix due to g_widg[1 less tha max].w
getting misteriously changed to 0x1.

Revision 1.32  2001/10/15 21:53:33  davidive
More X cleanup so that True color at least doesn't crash us.

Revision 1.31  2001/09/25 17:16:52  davidive
Cleaned up X code for opening X, creating a colormap, and making suer we have
a pseudocolor visual.

Revision 1.30  2001/03/23 19:35:29  harryive
Add "batch" mode to looping for use with Xvfb - in this mode their is no
loop window, but the pixmaps are still created.

Revision 1.29  2001/01/23 22:39:10  davidive
Start up connection to Open GL Server if available, and change number of
colors copied from the default colortable to 10 from 119 (no need).

Revision 1.28  1998/12/31 16:01:08  warren
Firts cut of new trajectory look and feel. Trajectories are no longer
on the popup menu, just plot type.

Revision 1.27  1998/03/04 19:13:37  davidive
First cut at trajectories and trajectory widget using colors to show depth
and arrows to show direction.

Revision 1.26  1997/10/22 19:34:37  davidive
Turned the particle movement part of trajectories into a user transform
and added unit checking to the default transform.

Revision 1.25  1997/10/15 19:24:27  harry
Use XtConfigureWidget for resizes.

Revision 1.24  1997/07/30 16:46:18  davidive
Fix gks window - pop front and back with control/mouse buttons
Rev up to 4.1.0 Released version

Revision 1.23  1997/05/30 18:04:29  harry
Fix mouse slicing to auto-cancel when plot is changed.

Revision 1.22  1997/05/16 22:15:11  harry
More mouse slicing fixes.

Revision 1.21  1997/05/08 20:04:31  harry
Make mouse slice act like mouse line drawing.

Revision 1.20  1997/04/25 21:58:56  warren
Window and Full changed to Zoom and Unzoom where Unzoom goes up one.

Revision 1.19  1997/04/11 18:46:40  warren
Casts on all X stuff and special checks for IRIX to keep SGI happy.

Revision 1.18  1997/02/21 21:16:54  warren
Final fixes for point/mouse slicing and expanding into 2d. It is tested
with the z dimention free and seems to work with others, but still should
be looked at with known data for this.

Revision 1.17  1996/12/12 19:37:14  warren
First crack at line on plot. Known to work on contour and line plots.

Revision 1.16  1996/11/07 16:53:58  warren
First set of activated point slicing and widgets to go with it. Only 1d plots
are known to be correct. Use at your own risk for 2d. There is still a known
bug in the connection between the tic mark entry boxes and the plot when doing
point or mouse slicing.

 * Revision 1.15  1996/01/12  16:10:57  warren
 * Transfer of new pattern stuff from 3.x and beginning of color fixes
 * (color vectors, correct collors on the pulldowan menus)
 *
 * Revision 1.14  1995/10/18  19:16:54  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.13  1995/05/11  20:20:30  warren
 * Bring printer updates forward from 3.2
 *
 * Revision 1.12  1995/02/28  19:29:17  warren
 * Fixed problem on alpha's where the popup type in boxes didn't move the
 * sliders for the window.
 *
 * Revision 1.11  1994/12/29  19:57:33  warren
 * New gks flush routine and fixed alpha missing label bug
 *
 * Revision 1.10  1994/12/27  22:35:17  warren
 * added white_background call for the movielabels=off option as per Dale.
 *
 * Revision 1.9  1994/06/16  20:17:28  warren
 * update from 3.1
 *
 * Revision 1.8  1994/04/26  00:13:00  warren
 * remove >>> and <<< oops.
 *
 * Revision 1.7  1994/04/23  17:48:55  warren
 * update from 3.1
 *
 * Revision 1.6  1994/02/02  22:57:15  warren
 * only open GKS the first time through.
 *
 * Revision 1.5  1993/12/30  00:48:27  warren
 * Update screen before returning from driver, and make sure char pointers in
 * ui_update are bigger than 0.
 *
 * Revision 1.4  1993/12/22  22:24:34  warren
 * fixed so correct size window shows up on an alpha screen
 *
 * Revision 1.3  1993/11/03  23:33:04  warren
 * General cleanup for beginning of port to alpha.
 *
 * Revision 1.2  1993/07/06  17:35:52  warren
 * Add resize function.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:20  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.6  1992/09/14  19:23:57  warren
 * fixed for black and white
 *
 * Revision 2.5  1992/09/11  01:43:35  warren
 * fixed to alloc own colormap if needed
 *
 * Revision 2.4  1992/08/25  23:29:32  warren
 * fixed color allocation problem and added fallback to smaller color table
 *
 * Revision 2.3  1992/08/24  23:20:21  warren
 * check number of planes before setting colors
 *
 * Revision 2.2  1992/08/22  00:32:19  warren
 * set line colors at widget startup
 *
 * Revision 2.1  1992/08/21  17:32:08  warren
 * increased allocated colors.
 *
 * Revision 2.0  1992/07/01  21:11:30  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.4  1992/06/17  18:06:00  warren
 * got rid of gxconfig as it turned the window blue.
 *
 * Revision 1.3  1992/06/17  16:42:24  warren
 * *** empty log message ***
 *
 * Revision 1.2  1992/06/11  22:42:15  warren
 * allow for ive in name instead of uwgap
 *
 * Revision 1.1  1992/04/21  00:08:28  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: gks_widgets.c,v 1.37 2002/12/26 22:48:11 warren Exp $";

#include <X11/Intrinsic.h>
#include <Xm/XmAll.h>
#include "../gks/XgksWidget.h"
#include <xgks.h>
#include <ive_gks.h>
#include <ive_gl.h>
#include <X11/Xcms.h>
#include <stdlib.h>
#include <malloc.h>


extern void ive_3dinput();

int singleBufferAttributess[] = {
  GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
  GLX_RENDER_TYPE,   GLX_RGBA_BIT,
  GLX_RED_SIZE,      1,   /* Request a single buffered color buffer */
  GLX_GREEN_SIZE,    1,   /* with the maximum number of color bits  */
  GLX_BLUE_SIZE,     1,   /* for each component                     */
    None
};

int doubleBufferAttributes[] = {
  GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
  GLX_RENDER_TYPE,   GLX_RGBA_BIT,
  GLX_DOUBLEBUFFER,  True,  /* Request a double-buffered color buffer with */
  GLX_RED_SIZE,      1,     /* the maximum number of bits per component    */
  GLX_GREEN_SIZE,    1, 
  GLX_BLUE_SIZE,     1,
    None
};
int stereoAttributes[] = {
  GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
  GLX_RENDER_TYPE,   GLX_RGBA_BIT,
  GLX_DOUBLEBUFFER,  True,  /* Request a double-buffered color buffer with */
  GLX_RED_SIZE,      1,     /* the maximum number of bits per component    */
  GLX_GREEN_SIZE,    1, 
  GLX_BLUE_SIZE,     1,
  GLX_STEREO,
    None
};

static int snglBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 8, None};
static int dblBuf[] = {GLX_RGBA, GLX_DEPTH_SIZE, 8, GLX_DOUBLEBUFFER,None};

static int firsttime=1;
static struct{
    Widget w;
    int id;
}g_widg[10]={NULL,0,NULL,0,NULL,0,NULL,0,NULL,0,NULL,0,NULL,0,NULL,0,NULL,0,NULL,0};

extern void make_help_widget_(),XtConfigureWidget(),gupdatews(),gsetcolorrep(),
  ginqcolorrep(),getivar_(),gopengks(),gescsetCTB(),gescsetAllocColour(),
  gopenws(),xXgksFillArea();
extern int gactivatews();
Widget gl_widget;
static int GL_window_ID=99;

extern Colormap cmap;
int count_colors(w,cmap)
     Widget w;
     Colormap cmap;
{
    Display *dpy;
    int i;
    unsigned long plane_masks[1], pixels[NUM_COLORS + 40];
    extern int IVE_TRUE_COLOR;

    dpy=XtDisplay(w);
    if( VisualOfCCC(XcmsCCCOfColormap(dpy,cmap))->class != PseudoColor){
      IVE_TRUE_COLOR = 1;
      return(0);
    }
    IVE_TRUE_COLOR=0;
    i = NUM_COLORS + 24;
    while(i>0){
      XSync(dpy,FALSE);
      XSync(dpy,FALSE);
      XSync(dpy,FALSE);
      if (XAllocColorCells(dpy, cmap, 
			   False, plane_masks, 0, pixels, i)) break;
      i--;
    }
    XFreeColors(dpy,cmap,pixels, i, 0);
    XSync(dpy,FALSE);
    XSync(dpy,FALSE);
    XSync(dpy,FALSE);
    return(i - 24);
}




    
void resize_gks_(){
    char buf[160], **argv, **get_args();
    int argc, width, height, num, count;
    short x, y;

    argv = get_args(&argc);
    for(count=0; count<10, g_widg[count].id != WS_X;count++);
    
    if (argc < 2 || argc > 3) {
        sprintf(buf, 
		"Command %s requires one of \"FULL\", \"NORMAL\" or \"WIDTH,HEIGHT\"",argv[0]);
        (void) make_help_widget_(buf);
        return;
    }
    
    else if (argc == 3){
	width =  atoi(argv[1]);
	height = atoi(argv[2]);
    XtVaGetValues(XtParent(g_widg[count].w),
		   XmNx, &x,
		   XmNy, &y,
		   NULL);
    }
    
    else if (*argv[1] == 'n' || *argv[1] == 'N'||
	     *argv[1] == 'd' || *argv[1] == 'D'){
	width = 640;
	height = 480;
	x=400;
	y=30;
    }
    else if (*argv[1] == 'f' || *argv[1] == 'F'){
	width =  WidthOfScreen(XtScreen(g_widg[count].w));
	height = HeightOfScreen(XtScreen(g_widg[count].w));
	x=0;
	y=0;
    }
    else{
        sprintf(buf, "Command %s requiresone of \"FULL\", \"NORMAL\" or \"WIDTH,HEIGHT\"",argv[0]);
        (void) make_help_widget_(buf);
        return;
    }

    XtConfigureWidget(XtParent(g_widg[count].w), x, y, width, height, 0);
}

void ive_update_gks_(){
    int count;
    for (count=0; count<10; count++)
      if(g_widg[count].w != NULL){
	if(g_widg[count].id != GL_window_ID){
	  gupdatews(g_widg[count].id,GPERFORM);
	  XSync(XtDisplay(g_widg[count].w), FALSE);
	}
      }

}

void black_background_()
{
    Gcobundl black, white;
    int count;

    white.red=1.0;
    white.green=1.0;
    white.blue=1.0;
	
    black.red=0.0;
    black.green=0.0; 
    black.blue=0.0;
    gsetcolorrep(WS_X,1,&white);
    gsetcolorrep(WS_X,0,&black);
    for (count=0; count<10; count++)
      if(g_widg[count].w != NULL){
	XtVaSetValues(g_widg[count].w,XmNbackground, 
		      BlackPixel(XtDisplay(g_widg[count].w),0), NULL);
      }
}

void white_background_()
{
    Gcobundl black, white;
    int count;

    white.red=1.0;
    white.green=1.0;
    white.blue=1.0;

    black.red=0.0;
    black.green=0.0; 
    black.blue=0.0;
    gsetcolorrep(WS_X,0,&white);
    gsetcolorrep(WS_X,1,&black);
    for (count=0; count<10; count++)
      if(g_widg[count].w != NULL){
	XtVaSetValues(g_widg[count].w,XmNbackground, 
		      WhitePixel(XtDisplay(g_widg[count].w),0), NULL);
      }
}

void gks_flush_(){
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
}

void dump_color_table_(index)
      int *index;
{
    int i;
    Gcobundl co;
    struct ive_color *list;
    
    list = color_table_list[*index - 1];
    
    for(i=0; i<user_colors_.max_traj_color;i++){
	ginqcolorrep(WS_X,i,GREALIZED,&co);
	list[i].r = co.red;
	list[i].g = co.green;
	list[i].b = co.blue;
    }
}

struct ive_color *get_color_table_(ind)
     int * ind;
{
    return(color_table_list[*ind - 1]);
}

static Widget slice_menu_widget;
static Widget zoom_menu_widget;
static Widget traj_menu_widget;

static void postmenu(w, data, ev, dispatch)
     Widget w;
     XtPointer data;
     XEvent *ev;
     Boolean *dispatch;
{
    Widget        menu     = (Widget)data;
    XButtonEvent *btnEvent = (XButtonEvent *)ev;
    int           button, linlog, use_buttons, xaxis, yaxis, error;
    extern int gks_mouse_menu_tracker;

    if(btnEvent->state & ControlMask){
      if(btnEvent->button==3)XLowerWindow(XtDisplay(w),XtWindow(XtParent(w)));
      if(btnEvent->button==1)XRaiseWindow(XtDisplay(w),XtWindow(XtParent(w)));
      return;
    }
    XtVaGetValues(menu, XmNwhichButton, &button, NULL);
    if (btnEvent->button == button && gks_mouse_menu_tracker == 0)
      {
	  (void)getivar_("linlog",&linlog,&error,6);
	  (void)getivar_("use_buttons", &use_buttons, &error, 11);
	  (void)getivar_("xaxis",&xaxis,&error,5);
	  (void)getivar_("yaxis",&yaxis,&error,5);
	  if (!use_buttons || xaxis < 1 || xaxis > 4 
	      || yaxis < 1 || yaxis > 4) {
	      XtSetSensitive(slice_menu_widget, False);
	  }
	  else{
	      XtSetSensitive(zoom_menu_widget, True);
	      XtSetSensitive(slice_menu_widget, linlog==1);
	  }
          XmMenuPosition(menu, btnEvent);
          XtManageChild(menu);
      }
}

static void do_slice_popup(parent)
     Widget parent;
{
    Widget over_menu,line_menu,slice_menu, hline, vline, sliceany;
    Widget slice, window, line, full, lineany, hslice, vslice;
    Widget traj;
    XmString str, NewString();
    extern void slice_menu_call();
    void postmenu();

    /*main menu*/
    over_menu = XmCreatePopupMenu(parent, "_popup", NULL, 0);
    slice_menu = XmCreatePulldownMenu(over_menu,"Slice",
				      NULL,0);
    line_menu = XmCreatePulldownMenu(over_menu,"Line",
				     NULL,0);
    
    str = NewString("Slice");
    slice = XtVaCreateManagedWidget("Slice",
				    xmCascadeButtonWidgetClass,
				    over_menu,
				    XmNsubMenuId, 
				    slice_menu,
				    XmNlabelString,str,
				    NULL);
    XmStringFree(str);
    slice_menu_widget = slice;

    str = NewString("Line");
    line = XtVaCreateManagedWidget("Line",
				   xmCascadeButtonWidgetClass,
				   over_menu,
				   XmNsubMenuId, 
				   line_menu,
				   XmNlabelString,str,
				   NULL);
    XmStringFree(str);
    

    str = NewString("Zoom");
    window = XtVaCreateManagedWidget("Window",
				   xmPushButtonWidgetClass,
				   over_menu,
				   XmNlabelString,str,
				   NULL);
    XmStringFree(str);
    XtAddCallback(window, XmNactivateCallback, slice_menu_call, (XtPointer)4);
    zoom_menu_widget = window;

    /*    str = NewString("Trajectory");
    traj = XtVaCreateManagedWidget("Trajectory",
				   xmPushButtonWidgetClass,
				   over_menu,
				   XmNlabelString,str,
				   NULL);
    XmStringFree(str);
    XtAddCallback(traj, XmNactivateCallback, slice_menu_call, (XtPointer)9);*/
    /*    traj_menu_widget = traj;*/

    str = NewString("Unzoom");
    full = XtVaCreateManagedWidget("Unzoom",
				   xmPushButtonWidgetClass,
				   over_menu,
				   XmNlabelString,str,
				   NULL);
    XmStringFree(str);
    XtAddCallback(full, XmNactivateCallback, slice_menu_call, (XtPointer)5);
    

				  
    
    /*slice menu*/
    str = NewString("Any Slice"); 
    sliceany = XtVaCreateManagedWidget("Sl1",
				       xmPushButtonWidgetClass,
				       slice_menu,
				       XmNlabelString,str,
				       NULL);
    XmStringFree(str);
    XtAddCallback(sliceany, XmNactivateCallback, slice_menu_call, (XtPointer)0);

    str = NewString("Horiz.\nSlice");
    hslice = XtVaCreateManagedWidget("Sl2",
				     xmPushButtonWidgetClass,
				     slice_menu,
				     XmNlabelString,str,
				     NULL);
    XmStringFree(str);
    XtAddCallback(hslice, XmNactivateCallback, slice_menu_call, (XtPointer)1);

    str = NewString("Vert.\nSlice");
    vslice = XtVaCreateManagedWidget("Sl3",
				     xmPushButtonWidgetClass,
				     slice_menu,
				     XmNlabelString,str,
				     NULL);
    XmStringFree(str);
    XtAddCallback(vslice, XmNactivateCallback, slice_menu_call, (XtPointer)2);
    

/*end of slice menu*/

    /*line menu*/
    str = NewString("Any Line"); 
    lineany = XtVaCreateManagedWidget("Sl1",
				       xmPushButtonWidgetClass,
				       line_menu,
				       XmNlabelString,str,
				       NULL);
    XmStringFree(str);
    XtAddCallback(lineany, XmNactivateCallback, slice_menu_call, (XtPointer)6);

    str = NewString("Horiz.\nLine");
    hline = XtVaCreateManagedWidget("Sl2",
				     xmPushButtonWidgetClass,
				     line_menu,
				     XmNlabelString,str,
				     NULL);
    XmStringFree(str);
    XtAddCallback(hline, XmNactivateCallback, slice_menu_call, (XtPointer)7);

    str = NewString("Vert.\nLine");
    vline = XtVaCreateManagedWidget("Sl3",
				     xmPushButtonWidgetClass,
				     line_menu,
				     XmNlabelString,str,
				     NULL);
    XmStringFree(str);
    XtAddCallback(vline, XmNactivateCallback, slice_menu_call, (XtPointer)8);
    

    
/*end of line menu*/
    XtAddEventHandler(parent, ButtonPressMask, False, 
		      postmenu, over_menu);
    XtAddEventHandler(parent, KeyPressMask|ButtonPressMask, False,
    		      ive_3dinput, NULL);
  
}

extern Boolean Ive_Stereo;
void setup_3D(Widget widg)
{
  Display *dpy;
  Window xwin;
  GLXFBConfig  *fbConfigs;
  GLXContext context;
  int numReturned, dummy;
  extern void glinput(),updateViewport();

  IveDblBufferFlag=0;
  dpy = XtDisplay(widg);
  /* set up Open GL stuff */
  if(!glXQueryExtension(dpy,&dummy,&dummy)){
    printf("There will be no 3D. Your X server does not support it!\n");
    return;
  }
  xwin = XtWindow(widg);
  if(Ive_Stereo){
    fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy),
				   stereoAttributes, &numReturned );
      IveDblBufferFlag = 1;
  }
  else{
    fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy),
				   doubleBufferAttributes, &numReturned );
    
    if ( fbConfigs == NULL ) {  /* no double buffered configs available */
      fbConfigs = glXChooseFBConfig( dpy, DefaultScreen(dpy),
				     singleBufferAttributess, &numReturned );
    
    }
    else{
      IveDblBufferFlag = 1;
    }
  }
  if(!fbConfigs){
    printf("There will be no 3D. Your X server does not support it!\n");
    return;
  }
  IveGlxVisInfo = glXGetVisualFromFBConfig(dpy, fbConfigs[0]);

  IveGlxContext = glXCreateNewContext(dpy,fbConfigs[0],GLX_RGBA_TYPE,
				      NULL, True);
  if(!IveGlxContext){
    printf("no GL context\n");
  }
  XtVaSetValues(widg,XmNvisual,IveGlxVisInfo,NULL);
  //IveGlxWindow = glXCreateWindow(dpy, fbConfigs[0], xwin, NULL);
  //glXMakeContextCurrent(dpy, IveGlxWindow, IveGlxWindow, IveGlxContext);
  IveGlxWindow=xwin;
  //glXMakeCurrent(dpy,xwin,IveGlxContext);
  //glClearColor( 1.0, 1.0, 1.0, 1.0 );
  //glClear( GL_COLOR_BUFFER_BIT );
  //glFlush();
}



Widget init_xgks(parent,name, cmap)
     Widget parent;
     char *name;
     Colormap cmap;
{
    Widget gksshell, gksbox, xgks_widget;
    Gcobundl black, white; 
    Gcobundl r, br, y, g, bl, v, gr80, gr90, gr95, gr70;
    int avail_colors, i;
    static int cur_window_count=0;
    XWindowAttributes windowattr;
    XSetWindowAttributes newattr;
    Display *dpy;

    dpy = XtDisplay(parent);

    white.red=1.0;
    white.green=1.0;    /* 2 */
    white.blue=1.0;

    r.red=1.0;
    r.green=0.0;    /* 3 */
    r.blue=0.0;

    br.red=1.0;
    br.green=0.55511811;    /* 4 */
    br.blue=0.0;

    y.red=1.0;
    y.green=1.0;    /* 5 */
    y.blue=0.0;

    g.red=0.0;
    g.green=0.62204724;    /* 6 */
    g.blue=0.11811024;

    bl.red=0.0;
    bl.green=0.0;    /* 7 */
    bl.blue=0.99606299;

    v.red=0.70866142;
    v.green=0.0;    /* 8 */
    v.blue=1.0;
    
    black.red=0.0;
    black.green=0.0;    /* 9 */
    black.blue=0.0;
    
     gr80.red=0.8;
     gr80.green=0.8;    /* 10 */
     gr80.blue=0.8;
 
     gr90.red=0.899;
     gr90.green=0.899;    /* 11 */
     gr90.blue=0.899;
 
     gr95.red=0.956;
     gr95.green=0.956;    /* 12 */
     gr95.blue=0.956;
 
     gr70.red=0.65;
     gr70.green=0.65;    /* 13 */
     gr70.blue=0.65;
 

    if (DisplayCells(dpy,DefaultScreen(dpy)) > 2 ){
      avail_colors = count_colors(parent,cmap);
  }
    else 
      avail_colors = 0;
    if(avail_colors > 2 && avail_colors <110){
	printf("IVE: only using %d user colors\n", avail_colors-14);
    }

    gksshell = XtVaAppCreateShell(name,"IVE_OUT",
				  applicationShellWidgetClass,
				  dpy, XtNallowShellResize, TRUE,
				  XmNx, 400, XmNy, 30,
				  XmNcolormap,cmap, 
				  NULL);
    
    XtRealizeWidget(gksshell);
    if(cur_window_count==0){
	gopengks(stderr,0);
    /*tell gks to allocate private colours*/
	if (DisplayCells(dpy,DefaultScreen(dpy)) > 2 ){
	    gescsetCTB(cmap);
	    if(avail_colors >0){
	      gescsetAllocColour(TRUE,avail_colors);
	    }
	    else{
	      gescsetAllocColour(FALSE,0);
	    }
	    if(avail_colors == 0)avail_colors=145;
	    user_colors_.min_user_color = 14;
	    user_colors_.max_user_color = (avail_colors - 15)/2 + 
	      user_colors_.min_user_color;
	    user_colors_.min_traj_color = user_colors_.max_user_color + 1;
	    user_colors_.max_traj_color = avail_colors - 1;
	    
	}
	for (i=0; i<MAXLIST; i++)
	  if((color_table_list[i] =
	      (struct ive_color *)malloc(user_colors_.max_traj_color *
					 sizeof(struct ive_color))) ==NULL){
	    /*	  if((color_table_list[i] =
		  (struct ive_color *)malloc((user_colors_.max_traj_color - 
		  user_colors_.min_user_color) *
		  sizeof(struct ive_color))) ==NULL){
		  (void)make_help_widget_
		  ("Sorry, out of memory making color tables");*/
	  }
	/*	if((traj_table =
		(struct ive_color *)malloc((user_colors_.max_traj_color - 
	    user_colors_.min_traj_color) *
					sizeof(struct ive_color))) ==NULL){
				       (void)make_help_widget_
	  ("Sorry, out of memory making traj color tables");
	    }*/
    }
    else{
	user_colors_.min_user_color = 0;
	user_colors_.max_user_color = 1;
	for (i=0; i<MAXLIST; i++)
	  if((color_table_list[i] =
	      (struct ive_color *)malloc(2 * sizeof(struct ive_color)))
	     ==NULL){
	      (void)make_help_widget_
		("Sorry, out of memory making color tables");
	  }
    }    
    xgks_widget = XtVaCreateManagedWidget("xgks_widget",
					  xgksWidgetClass,gksshell,
					  XtNwsId, WS_X,
					  XmNwidth, 640,
					  XmNheight, 480,
					  XtNbackingStore, Always,
					  XmNcolormap,cmap  ,
					  XmNbackground, 
					  WhitePixel(XtDisplay(gksshell),0),
					  NULL);
                                          

    if (gactivatews(WS_X) != 0)
                fprintf(stderr,"XgksError: activate_ws failed\n");
    gsetcolorrep(WS_X,0,&white);
    gsetcolorrep(WS_X,1,&black);
    /* colors for lines */
    if (avail_colors > 10 ){
	gsetcolorrep(WS_X,2,&white);
	gsetcolorrep(WS_X,3,&r);
	gsetcolorrep(WS_X,4,&br);
	gsetcolorrep(WS_X,5,&y);
	gsetcolorrep(WS_X,6,&g);
	gsetcolorrep(WS_X,7,&bl);
	gsetcolorrep(WS_X,8,&v);
	gsetcolorrep(WS_X,9,&black);
        gsetcolorrep(WS_X,10,&gr95);
        gsetcolorrep(WS_X,11,&gr90);
        gsetcolorrep(WS_X,12,&gr80);
        gsetcolorrep(WS_X,13,&gr70);
    }
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
    XSync(XtDisplay(gksshell),FALSE);

    gopenws(WS_WISS,"WISS","WISS");
    if (gactivatews(WS_WISS) != 0)
                fprintf(stderr,"XgksError: activate_ws failed\n");
    
    if(cur_window_count<10){
	g_widg[cur_window_count].w = xgks_widget;
	g_widg[cur_window_count++].id = WS_X;
    }
    for(i = cur_window_count; i<10; i++){
      g_widg[i].w = NULL;
      g_widg[i].id = 0;
    }
    XtVaSetValues(gksshell,
		  XmNwidth, 640,
		  XmNheight, 480,
		  NULL);
    do_slice_popup(xgks_widget);
    xXgksFillArea(NULL,NULL);
    
    setup_3D(xgks_widget);
    return(xgks_widget);
}


void graylines_()
{
  Gcobundl cobund;
  int i;
    
  for(i=3; i<9; i++){
    cobund.red=1.0 - 2.0/(float)(i);
    cobund.green=1.0 - 2.0/(float)(i);
    cobund.blue=1.0 - 2.0/(float)(i);
    
    gsetcolorrep(WS_X,i,&cobund);
    /*    printf("color %d is now (%.3f,%.3f,%.3f)\n",i,cobund.red,cobund.green,cobund.blue);*/
  }
  gupdatews(WS_X,GPERFORM);                /*now!!!*/
}



