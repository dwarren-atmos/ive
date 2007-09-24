/****************************************************************************
 * $Id: color_cb.c,v 1.12 2002/12/09 18:03:08 warren Exp $
 *
 * $Log: color_cb.c,v $
 * Revision 1.12  2002/12/09 18:03:08  warren
 * Instead of checking number of plane every time, just check once and record
 * if it is true color in bool.
 *
 * Revision 1.11  2002/11/27 00:30:50  warren
 * 24 bit color enhancements are here. Works on Linux and Sun so far - dec to
 * be tested still.
 *
 * Revision 1.10  2002/08/09 19:57:25  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * This file contains the callbacks for the "Color Options"
 * (aka Color Table -> Slide Lock & Toggle) widget.
 *
 * $Log: color_cb.c,v $
 * Revision 1.12  2002/12/09 18:03:08  warren
 * Instead of checking number of plane every time, just check once and record
 * if it is true color in bool.
 *
 * Revision 1.11  2002/11/27 00:30:50  warren
 * 24 bit color enhancements are here. Works on Linux and Sun so far - dec to
 * be tested still.
 *
 * Revision 1.9  2001/08/15 22:23:19  davidive
 * Cleaned up code to reduce warnings about type mismatches, and added buttons
 * and callbacks to the 1d form widget to select which access to label against
 * for diagonal plots.
 *
 * Revision 1.8  2001/03/14 20:21:09  warren
 * corrected severa longs to unsigned longs, changed creation of lock button to
 * not set the name, the set the label seperately, change XtFree to XmStringFree
 * where it should be.
 *
 * Revision 1.7  2001/03/13 21:35:40  davidive
 * Modified to update the free locked button on the color slide widget when the
 * locked_field is ' '
 *
 * Revision 1.6  1997/04/18 17:40:36  warren
 * Can't check windows for the word Locked any more, so check for Free.
 *
 * Revision 1.5  1997/04/11 18:46:39  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.4  1996/02/16 17:49:16  warren
 * fixed HSL mixer to work. Also fixed CDE crash where we crash bringing up
 * mixer after the colorbar
 *
 * Revision 1.3  1994/05/18  17:53:02  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.2  1993/06/02  23:34:51  yuri
 * removed spurious call to driver.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:17  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.15  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.14  1992/11/17  17:21:29  warren
 * added ive_macros to includes
 *
 * Revision 1.13  1992/10/15  23:01:21  warren
 * added color locking stuff
 *
 * Revision 1.12  1992/10/06  19:01:49  warren
 * 0 to NULL
 *
 * Revision 1.11  1992/09/11  01:43:35  warren
 * fixed to alloc own colormap if needed
 *
 * Revision 1.10  1992/08/27  19:17:40  warren
 * removed print statement
 *
 * Revision 1.9  1992/08/27  18:10:02  harry
 * Fix calculation of moved.
 *
 * Revision 1.8  1992/08/27  16:12:43  warren
 * dales new color stuff
 *
 * Revision 1.6  1992/08/22  00:05:20  warren
 * reduced to 3 rows as per Dale.
 *
 * Revision 1.5  1992/08/21  21:37:07  warren
 * resized to look beter on 16" screen.
 *
 * Revision 1.4  1992/08/21  21:29:18  warren
 * *** empty log message ***
 *
 * Revision 1.3  1992/08/21  21:21:13  warren
 * fixed color bar for 128 colors 2 - 130
 *
 * Revision 1.2  1992/08/21  16:25:22  warren
 * fixed error message
 *
 * Revision 1.1  1992/08/21  16:03:01  warren
 * Initial revision
 *
*****************************************************************************/
static char ident[] = "$Id: color_cb.c,v 1.12 2002/12/09 18:03:08 warren Exp $";
#include <Xm/XmAll.h>
#include <xgks.h>
#include <ive_gks.h>
#include <ive_for.h>
#include <ive_color.h>
#include <malloc.h>

extern Pixel IveGetPixel();
extern Widget Box;
extern void im_gone_callback(),make_help_widget_(),IVE_TO_X(),gredrawsegws(),
  ginqcolourrep(),gsetcolourrep(),gupdatews();
extern XmString NewString();
extern Colormap cmap; 
static int repress=0;

static Widget colorholder=(Widget)NULL;
static Widget *colorholdercells=(Widget *)NULL;
static int moved=0;
static int colortabletodo=0;
void cleanup_color_bar(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
  if(colorholdercells){
    free(colorholdercells);
    colorholdercells=(Widget *)NULL;
  }
  if(colorholder)colorholder=(Widget)NULL;
}

/*
   "color_shift_callback" is called when an arrow button on the
   "Slide Color Table" widget is pressed.  "data" will contain an "R" for
   the right arrow and an "L" for the left arrow.
   */
void color_shift_callback(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    XColor bundle,extrab;
    int i;
    int minclr,maxclr;
      
    if(colortabletodo){
      minclr = user_colors_.min_traj_color;
      maxclr = user_colors_.max_traj_color;
    }
    else{
      minclr = user_colors_.min_user_color;
      maxclr = user_colors_.max_user_color;
    }
    if(IVE_TRUE_COLOR)
      {
        /* this should work for either true or direct color*/
        unsigned long *pix;
	int modval;
	modval = maxclr - minclr +1;
        pix = (unsigned long *)malloc(modval * sizeof(unsigned long));
        if(pix == (unsigned long *)NULL){
	  (void)make_help_widget_("Can't allocate memory - shift colors");
	  return;
	}
	switch(*data){
	case 'R':		/* Shift table to the right */
	  for(i=modval; i>0; i--) /*i=modval is same as i = 0*/
	    pix[i%modval] = IveGetPixel(minclr +
					((i-1)%modval));
	  if (++moved > maxclr - 
	      minclr) {
	    moved = 0;
	  }
	  break; 
	case 'L':		/* Shift table to the right */
	  for(i=0; i< modval; i++)
	    pix[i%modval] = IveGetPixel(minclr +
					((i+1)%modval));
	  if (--moved < -(maxclr - 
	      minclr)) {
	    moved = 0;
	  }
	  break;
	}
        for(i=0; i< modval; i++)          
          (void)IVE_TO_X((minclr + i), pix[i]);
	if(!repress){
	  (void)gredrawsegws(WS_X);
	  if(colorholdercells){
	    for(i=0; i< modval; i++)
	      XtVaSetValues(colorholdercells[i],
			    XmNbackground,
			    IveGetPixel(minclr+i),NULL);
	    XSync(XtDisplay(w),FALSE);
	    XSync(XtDisplay(w),FALSE);
	    XSync(XtDisplay(w),FALSE);
	  }
	}
      }
    else{
      bundle.flags=DoRed|DoGreen|DoBlue;
      extrab.flags=DoRed|DoGreen|DoBlue;
      switch(*data){
      case 'R':		/* Shift table to the right */
	i=maxclr;
	extrab.pixel=IveGetPixel(maxclr);
	XQueryColor(XtDisplay(w), cmap, &extrab);
	while(i>minclr){
	  bundle.pixel=IveGetPixel(i-1);
	  XQueryColor(XtDisplay(w), cmap, &bundle);
	  bundle.pixel=IveGetPixel( i);
	  XStoreColor(XtDisplay(w), cmap, &bundle);
	    i--;
	}
	extrab.pixel=IveGetPixel(minclr);
	XStoreColor(XtDisplay(w), cmap, &extrab);
	if (++moved > maxclr - 
	    minclr) {
	  moved = 0;
	}
	break;
      case 'L':		/* Shift table to the left */
	i=minclr;
	extrab.pixel=IveGetPixel(minclr);
	XQueryColor(XtDisplay(w), cmap, &extrab);
	while(i<maxclr){
	  bundle.pixel=IveGetPixel(i+1);
	  XQueryColor(XtDisplay(w), cmap, &bundle);
	  bundle.pixel=IveGetPixel(i);
	  XStoreColor(XtDisplay(w), cmap, &bundle);
	  i++;
	}
	extrab.pixel=IveGetPixel(maxclr);
	XStoreColor(XtDisplay(w), cmap, &extrab);
	if (--moved < -(maxclr - 
			minclr)) {
	  moved = 0;
	}
	break;
      }
    }
}
/*
   "color_lock_callback" is called when the "Lock Color Table" button
   is pressed.  It toggles the button between "Locked" and "Free".
*/
void color_lock_callback(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString F,old;
    F = (XmString)NewString("Free");
    XtVaGetValues(w,XmNlabelString,&old,NULL);
    if (XmStringCompare(F,old)){	/* Locked -> Free */
	driver("lockcolortable=lock");
    }
    else{				/* Free -> Locked */
	driver("lockcolortable=free");
    }
    XmStringFree(old);
    XmStringFree(F);

}
/*color_table_callback chooses between mucking with the standard or traj color tables*/
void color_table_callback(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    Arg args[1];
    XmString F,old;
    F = (XmString)NewString("Standard");
    XtVaGetValues(w,XmNlabelString,&old,NULL);
    if (XmStringCompare(F,old)){	/* Standard */
      colortabletodo=1; /*use traj*/
      XmStringFree(F);
      F = (XmString)NewString("Trajectory");
      XtVaSetValues(w,XmNlabelString,F,NULL);
    }
    else{
      colortabletodo=0 ;/*use standard*/
      XtVaSetValues(w,XmNlabelString,F,NULL);
    }
    XmStringFree(old);
    XmStringFree(F);

}
/*
   "color_reset_callback" is called when the "Reset" button is pressed.
   It resets the color table to its original state by repeated calls to
   color_shift_callback.
   */
void color_reset_callback(w, data, call)
     Widget w;
     char *data;
     XmAnyCallbackStruct *call;
{
    int minclr,maxclr;
      
    if(colortabletodo){
      minclr = user_colors_.min_traj_color;
      maxclr = user_colors_.max_traj_color;
    }
    else{
      minclr = user_colors_.min_user_color;
      maxclr = user_colors_.max_user_color;
    }
  repress=1;
  while(moved>0){
    color_shift_callback(w, "L", call);
  }
  while(moved<0){
    color_shift_callback(w, "R", call);
  }
  if(IVE_TRUE_COLOR)
  
  (void)gredrawsegws(WS_X);
  if(colorholdercells){
    int i;
    for(i=0; i< maxclr-minclr ; i++)
      XtVaSetValues(colorholdercells[i],
		    XmNbackground,
		    IveGetPixel(minclr+i),NULL);
    
    XSync(XtDisplay(w),FALSE);
    XSync(XtDisplay(w),FALSE);
    XSync(XtDisplay(w),FALSE);
  }
  repress=0;
}
/*
   "swap_colors_callback" is called when the "Toggle Background" button is
   pressed.  It swaps colors 0 and 1 to swap the foreground and background
   colors.
   */

void swap_colors_callback(w, data, call)
     Widget w;
     Widget data;
     XmAnyCallbackStruct *call;
{
    extern Widget xgks_widget;
    Gcobundl co0,co1;
    int fore,back;
    Arg args[3];
    
    ginqcolorrep(WS_X,0,GREALIZED,&co0);        /*get gks color*/
    ginqcolorrep(WS_X,1,GREALIZED,&co1);        /*get gks color*/

    gsetcolourrep(WS_X,0,&co1);       /*set new gks colour0*/
    gsetcolourrep(WS_X,1,&co0);       /*set new gks colour1*/
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
    
    ginqcolorrep(WS_X,0,GREALIZED,&co0);        /*get gks color*/
    ginqcolorrep(WS_X,1,GREALIZED,&co1);        /*get gks color*/
    XSync(XtDisplay(w),FALSE);
    XSync(XtDisplay(w),FALSE);
    XSync(XtDisplay(w),FALSE);
}
/*
   "color_bar_callback" is called when the "Color Bar" button is pushed.
   It displays the current GKS colors.  This callback is also called from 
   the "Color Table" menu under the item "Display Table".
*/
void color_bar_callback(w, data, call)
    Widget w;
    Widget data;
    XmAnyCallbackStruct *call;
{
    static int here=0;
    int i;
    int status;
    char buff[4];
    Widget color_bar;
    int minclr,maxclr;
    
    if(colortabletodo){
      minclr = user_colors_.min_traj_color;
      maxclr = user_colors_.max_traj_color;
    }
    else{
      minclr = user_colors_.min_user_color;
      maxclr = user_colors_.max_user_color;
    }

    if (colorholder){
      /*
	If the widget already exists, make it visible if it isn't and
	vice versa.
      */
      if(XtIsManaged(XtParent(colorholder)))
	XtUnmanageChild(XtParent(colorholder));
      else
	  XtManageChild(XtParent(colorholder));
      return;
    }
    /*
      Create the widget.
    */
    if (DisplayCells(XtDisplay(Box),DefaultScreen(XtDisplay(Box))) > 2)
      {
	if(!colorholdercells)
	color_bar = XtVaCreatePopupShell("Color Bar",
					 transientShellWidgetClass,
					 XtParent(Box) ,
					 XmNy,
					 HeightOfScreen(XtScreen(Box))-115,
					 XmNautoUnmanage,FALSE,
					 XmNallowShellResize,True,NULL);
	
	XtAddCallback(color_bar,XmNdestroyCallback,
		      (XtCallbackProc)cleanup_color_bar,
		      (XtPointer)NULL);
	
	colorholder = XtVaCreateManagedWidget("Color Holder",
					      xmRowColumnWidgetClass,
					      color_bar,
					      XmNorientation,XmHORIZONTAL,
					      XmNpacking,XmPACK_COLUMN,
					      XmNnumColumns,4,NULL);
	/*
	  Make each inividual color box.
	*/
	colorholdercells = (Widget *)malloc((maxclr -
					     minclr +1 )*
					    sizeof(Widget));
	
	for(i=minclr;
	    i<=maxclr; i++){
	  sprintf(buff,"%3d",i);
	  colorholdercells[i - minclr] = 
	    XtVaCreateManagedWidget(buff,xmLabelWidgetClass,colorholder,
				    XmNheight,30,XmNmarginTop,5,
				    XmNbackground,IveGetPixel(i),NULL);
	}
	XtManageChild(color_bar);
      }
    else
      {
	(void)make_help_widget_(
				"You must have at least eight\nbitplanes to show a color bar");
      }
}

void ive_update_colorbar(){
  /*only called for 24 bit color - prbably crash under 8 */
  int i;
  char buff[4];
    int minclr,maxclr;
      
    if(colortabletodo){
      minclr = user_colors_.min_traj_color;
      maxclr = user_colors_.max_traj_color;
    }
    else{
      minclr = user_colors_.min_user_color;
      maxclr = user_colors_.max_user_color;
    }

  if(colorholdercells){
    for(i=minclr;
	i<=maxclr; i++){
      XtVaSetValues(colorholdercells[i - minclr],
		    XmNbackground,IveGetPixel(i),NULL);
    }
  }
  (void)gredrawsegws(WS_X);
}

