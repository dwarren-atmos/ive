/****************************************************************************
 * $Id: colormix.c,v 1.10 2002/12/09 18:03:08 warren Exp $
 *
 * This contains the routines for the colormixer.
 *
 * $Log: colormix.c,v $
 * Revision 1.10  2002/12/09 18:03:08  warren
 * Instead of checking number of plane every time, just check once and record
 * if it is true color in bool.
 *
 * Revision 1.9  2002/11/27 00:30:50  warren
 * 24 bit color enhancements are here. Works on Linux and Sun so far - dec to
 * be tested still.
 *
 * Revision 1.8  2002/08/09 19:57:25  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.7  1997/04/11 18:46:39  warren
 * Casts on all X stuff and special checks for IRIX to keep SGI happy.
 *
 * Revision 1.6  1996/02/16 17:49:17  warren
 * fixed HSL mixer to work. Also fixed CDE crash where we crash bringing up
 * mixer after the colorbar
 *
 * Revision 1.5  1994/06/20  16:35:27  harry
 * Change index to strchr, and rindex to strrchr.
 *
 * Revision 1.4  1994/05/26  17:32:06  warren
 * change to includes and defines to work with SOL 2
 *
 * Revision 1.3  1994/05/18  17:53:05  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.2  1994/03/28  19:59:29  warren
 * Fixed so colors change when they are told again.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:17  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.3  1993/02/16  16:56:34  warren
 * changed var index color_index to avoid confusion with function
 *
 * Revision 2.2  1992/11/17  17:21:29  warren
 * added ive_macros to includes
 *
 * Revision 2.1  1992/08/20  23:24:27  warren
 * fixed so the mixer comes up centered in the table.
 *
 * Revision 2.0  1992/07/01  21:11:18  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.1  1992/04/21  00:08:28  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: colormix.c,v 1.10 2002/12/09 18:03:08 warren Exp $";

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/X.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <stdio.h>
#include <xgks.h>
#include <string.h>
#include <stdlib.h>
#if (LSB == 5)
#include <lsb5/math.h>
#elif (LSB == 4)
#include <lsb4/math.h>
#elif (LSB == 3)
#include <lsb3/math.h>
#elif (LSB == 2)
#include <lsb2/math.h>
#elif (LSB == 1)
#include <lsb1/math.h>
#else
#include <math.h>
#endif
#include <ive_gks.h>
#include <ive_color.h>
#include <ive_macros.h>
#ifdef Linux
typedef char *caddr_t;
#endif

/* 
  The following only exist while the color mixer is present
*/
XmString NewString();
static    Widget red,green,blue,entry;
/*static    Window original,new;*/
static Widget original,new;
static    int Maximum;
static    XColor color1, color2;
static    Colormap map;
static    GC gc_original, gc_new;
static    float saturation,value,hue;
static    Bool Truecolor=FALSE, RMOD = FALSE;

extern void ginqcolourrep(),gsetcolourrep(),gupdatews(),ive_update_colorbar(),
  update_all_(),getivar_(),make_help_widget_();
extern unsigned long IveGetPixel();

float
getmax(a,b,c)
     float a,b,c;
{
    if(a>b){
	return((a > c) ? a : c);
    }
    return((b > c) ? b : c);
}

float
getmin(a,b,c)
     float a,b,c;
{
    if(a<b){
	return((a < c) ? a : c);
    }
    return((b < c) ? b : c);
}

rgb2hsv(r,g,b)    /*color model conversion*/
     float r,g,b;
{
    int rval=0;
    float h,max,min;
    max=getmax(r,g,b);
    min=getmin(r,g,b);
    value=max;
    if (max==0 | max==min){
	saturation=0;
	hue=0;
	rval=1;
    }
    else{
	saturation=(max-min)/max;
	if(r==max){
	    hue = (g-b)/(max-min);
	}
	else{
	    if(g==max){
		hue = 2 + (b-r)/(max-min);
	    }
	    else{
		hue = 4 + (r-g)/(max-min);
	    }
	}
	hue *= 60.0;
	if(hue<0.0)hue += 360.0;
	return(rval); /* if rval than h is undefined */
    }
}

hsv2rgb(r,g,b)         /*color model conversion*/
     float *r,*g,*b;
{
    float fl,h,x,y,z;  /*temp vars*/

    if(saturation==0.0 ){
	hue=0.0;
	*r=value;
	*g=value;
	*b=value;
    }
    else{
	h = (hue <= 360.0) ? hue/60 : 0;
	fl = h - (float)floor((double)h);  /*find just the fractional part*/
	x = value*(1-saturation);
	y = value*(1-(saturation*fl));
	z = value*(1-(saturation*(1-fl)));
	switch((int)h){
	  case 0 : *r = value;
	           *g = z;
	           *b = x;
	            break;
	  case 1 : *r = y;
	           *g = value;
	           *b = x;
	            break;
	  case 2 : *r = x;
	           *g = value;
	           *b = z;
	            break;
	  case 3 : *r = x;
	           *g = y;
	           *b = value;
	            break;
	  case 4 : *r = z;
	           *g = x;
	           *b = value;
	            break;
	  case 5 : *r = value;
	           *g = x;
	           *b = y;
	}
    }
    return(0);
}




float check_nan(num) 
/*make sure num != NaN*/
float num;
{
    if(isnan((double)num))return(1.0);
    else return(num);
}

void insert_color(w, data, call) 
     /* insert a new color */
     Widget w;
     Widget *data;
     XmAnyCallbackStruct call;
{
    void new_entry();
     static int color_index;
     int prevind,curind;
     Gcobundl bundle;
     Arg args[2];

     XtSetArg(args[0],XmNvalue,&color_index);
     XtGetValues(entry,args,1);               /*get colour index*/
     curind=Maximum;
     while(curind>color_index){
	 prevind=curind-1;
	 ginqcolourrep(WS_X,prevind,GREALIZED,&bundle);
	 gsetcolourrep(WS_X,curind,&bundle);
	 curind--;
     }
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
    XSync(XtDisplay(w),FALSE);   /*make sure display is updated now*/
    XmScaleSetValue(*data,color_index);
    new_entry(entry,&color_index,NULL);
    if (IVE_TRUE_COLOR){
      (void)ive_update_colorbar();
    }
    update_all_();
 }

void delete_color(w, data, call) 
     /* insert a new color */
     Widget w;
     Widget *data;
     XmAnyCallbackStruct call;
{
    void new_entry();
    int color_index,nextind,oldindex;
    Gcobundl bundle;
    Arg args[2];
    
    XtSetArg(args[0],XmNvalue,&color_index);
    XtGetValues(entry,args,1); 
    oldindex=color_index;
    /*get colour index*/
    while(color_index<Maximum){
	nextind=color_index+1;
	ginqcolourrep(WS_X,nextind,GREALIZED,&bundle);
	gsetcolourrep(WS_X,color_index,&bundle);
	color_index++;
    }
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
    XSync(XtDisplay(w),FALSE);   /*make sure display is updated now*/
    XmScaleSetValue(*data,oldindex);
    new_entry(entry,&oldindex,NULL);
    if (IVE_TRUE_COLOR){
      (void)ive_update_colorbar();
    }
    update_all_();
 }
    

void slide_changed(w, data, call) 
/* update the colors when the sliders change*/
     Widget w;
     char *data;
     XmScaleCallbackStruct *call;
{
    int status;
    float r,g,b;
    Arg args[1];
    switch(*data){
      case 'r':
	if (RMOD){   /*if RMOD then do rgb colors else do hsv colors*/
	    color2.red = 256*call->value; /*sliders go 0 - 255*/
	                              /*X goes 0 - 65535 */
	}
	else{
	    hue=(float)call->value;
	    hsv2rgb(&r,
		    &g,
		    &b);
	    color2.red   =  (short)(r*65535.0);
	    color2.green =  (short)(g*65535.0);
	    color2.blue  =  (short)(b*65535.0);
	}
	break; 
      case 'g':
	if (RMOD){
	    color2.green = 256*call->value;
	}
	else{
	    saturation=(float)call->value/100.0;
	    hsv2rgb(&r,
		    &g,
		    &b);
	    color2.red   =  (short)(r*65535.0);
	    color2.green =  (short)(g*65535.0);
	    color2.blue  =  (short)(b*65535.0);
	}
	break;
      case 'b':
	if (RMOD){
	    color2.blue =256* call->value;
	}
	else{
	    value=(float)call->value/100.0;
	    hsv2rgb(&r,
		    &g,
		    &b);
	    color2.red   =  (short)(r*65535.0);
	    color2.green =  (short)(g*65535.0);
	    color2.blue  =  (short)(b*65535.0);
	}
    }
    color2.flags=DoRed|DoGreen|DoBlue;
    color2.pad = 0;
    if (IVE_TRUE_COLOR){
      XFreeColors(XtDisplay(w), map, &color2.pixel, 1,0);
      (void)XAllocColor(XtDisplay(w), map, &color2);
      XtVaSetValues(new, XmNbackground, color2.pixel,NULL);
    }
    else{
#if defined (IRIX)
      XStoreColor(XtDisplay(w),map,&color2);
#else
      status=XStoreColor(XtDisplay(w),map,&color2);
#endif
    }
    XSync(XtDisplay(w),FALSE);   /*make sure display is updated now*/
    update_all_();
 }

void new_entry(w, data, call)
     Widget w;
     int *data;
     XmScaleCallbackStruct *call;
{
     Arg args[1];
     int error;
     Gcobundl bundle;

     if(data){
	 ginqcolourrep(WS_X,*data,GREALIZED,&bundle);/*get gks color*/
     }else{
	 ginqcolourrep(WS_X,call->value,GREALIZED,&bundle);/*get gks color*/
     }
     color1.red=(short)(check_nan(bundle.red)*65535);
     color2.red=(short)(check_nan(bundle.red)*65535);
     XtSetArg(args[0],XmNvalue,(int)(check_nan(bundle.green)*255));/*then set*/
     color1.green=(short)(check_nan(bundle.green)*65535);
     color2.green=(short)(check_nan(bundle.green)*65535);
     XtSetArg(args[0],XmNvalue,(int)(check_nan(bundle.blue)*255));
     color1.blue=(short)(check_nan(bundle.blue)*65535);
     color2.blue=(short)(check_nan(bundle.blue)*65535);
     color1.flags=DoRed|DoGreen|DoBlue;
     color2.flags=DoRed|DoGreen|DoBlue;
     if (IVE_TRUE_COLOR){
       XFreeColors(XtDisplay(w), map, &color1.pixel, 1,0);
       (void)XAllocColor(XtDisplay(w), map, &color1);
       XFreeColors(XtDisplay(w), map, &color2.pixel, 1,0);
       (void)XAllocColor(XtDisplay(w), map, &color2);
       XtVaSetValues(original, XmNbackground, color1.pixel, NULL);
       XtVaSetValues(new, XmNbackground, color2.pixel, NULL);
     }
     else{
       XStoreColor(XtDisplay(w),map,&color1);
       XStoreColor(XtDisplay(w),map,&color2);     
     }
     if(!RMOD)rgb2hsv(bundle.red, bundle.green, bundle.blue);    /*get hsv*/

     if(RMOD){
	 XtSetArg(args[0],XmNvalue,(int)(check_nan(bundle.red)*255)); 
     }
     else{
	 XtSetArg(args[0],XmNvalue,(int)hue);
     }
     XtSetValues(red,args,1);                                      
     if(RMOD){
	 XtSetArg(args[0],XmNvalue,(int)(check_nan(bundle.green)*255)); 
     }
     else{
	 XtSetArg(args[0],XmNvalue,(int)(saturation*100.0));
     }
     XtSetValues(green,args,1);                                    
     if(RMOD){
	 XtSetArg(args[0],XmNvalue,(int)(check_nan(bundle.blue)*255)); 
     }
     else{
	 XtSetArg(args[0],XmNvalue,(int)(value*100.0));
     }
     XtSetValues(blue,args,1);
     XSync(XtDisplay(w),FALSE);
     update_all_();

 }

void quit_color(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
/*    XUnmapWindow(XtDisplay(w),new);
    XUnmapWindow(XtDisplay(w),original);
*/
  if (DisplayCells(XtDisplay(w),DefaultScreen(XtDisplay(w))) <9){
    XFreeColors(XtDisplay(w),map,&color1.pixel,1,0);
    XFreeColors(XtDisplay(w),map,&color2.pixel,1,0);
  }
  XtDestroyWidget(XtParent(w));
}

void apply_color(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    extern Widget xgks_widget;
    int error,color_index;
    Gcobundl bundle;
    unsigned long pix,back;
    XColor inout;
    Arg args[1];

/*X colors 0 to 65535 gks colors 0 to 1*/
    bundle.red=(float)color2.red/65535.0;
    bundle.green=(float)color2.green/65535.0;
    bundle.blue=(float)color2.blue/65535.0;  /*create gks colour bundle*/
    XtSetArg(args[0],XmNvalue,&color_index);
    XtGetValues(entry,args,1);               /*get colour index*/
    
    gsetcolourrep(WS_X,color_index,&bundle);       /*set new gks colour*/
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
    ginqcolourrep(WS_X,color_index,GREALIZED,&bundle);
    gupdatews(WS_X,GPERFORM);                /*now!!!*/
    XSync(XtDisplay(w),FALSE);
    XSync(XtDisplay(w),FALSE);
    XSync(XtDisplay(w),FALSE);
    if (IVE_TRUE_COLOR){
      (void)ive_update_colorbar();
    }
    update_all_();
}


void do_reset_colors(w, data, call)
     Widget w;
     caddr_t data;
     XmAnyCallbackStruct *call;
{
    static XmScaleCallbackStruct sc_call;
    Arg args[1];
    if(!RMOD)rgb2hsv((float)color1.red/65535.0,
		     (float)color1.green/65535.0,
		     (float)color1.blue/65535.0);
    if(RMOD){
	XtSetArg(args[0],XmNvalue,color1.red/256);
    }
    else{
	XtSetArg(args[0],XmNvalue,(int)hue);
    }
    XtSetValues(red,args,1);
    sc_call.reason=0;
    sc_call.event = call->event;
    sc_call.value = args[0].value;
    slide_changed(red,"r",&sc_call);

    if(RMOD){
	XtSetArg(args[0],XmNvalue,color1.green/256);
    }
    else{
	XtSetArg(args[0],XmNvalue,(int)(saturation*100.0));
    }
    XtSetValues(green,args,1);
    sc_call.value = args[0].value;
    slide_changed(green,"g",&sc_call);
    if(RMOD){
	XtSetArg(args[0],XmNvalue,color1.blue/256);
    }
    else{
	XtSetArg(args[0],XmNvalue,(int)(value*100.0));
    }
    XtSetValues(blue,args,1);
    sc_call.value = args[0].value;
    slide_changed(blue,"b",&sc_call);
    update_all_();

}

int get_screen_number(w)
     Widget w;
{
    char *dpy,*scrptr;
    
    dpy = DisplayString(XtDisplay(w));
    if((scrptr = strrchr(dpy,'.'))==NULL){
	return(0);
    }
    else{
	return((int)atoi(++scrptr));
    }
}

	
    

void gks_color_mixer(parent,min,max,RGB)
    /*this is what makes the windows*/
    Widget parent;
    int min,max;
    Bool *RGB;
{
    static XmScaleCallbackStruct sc_call;
    int i,center;
    Widget mixer,apply,done,reset,insert,delete;
    XmString str;
    XGCValues gvals;
    unsigned long pixvals[2];
    unsigned long masks[1];
    int status;
    Gcobundl bundle;

   if (DisplayCells(XtDisplay(parent),DefaultScreen(XtDisplay(parent))) > 2)
    {
      Maximum=max;
      (void)getivar_("zero_color", &center, &status,10);
      if(status || center == -1)
	center = (max-min)/2+min;
      ginqcolourrep(WS_X,center,GREALIZED,&bundle);
      RMOD = *RGB;
      mixer = XmCreateFormDialog(parent,"Mixer",NULL,0);
      XtVaSetValues(mixer,XmNwidth,260,XmNheight,425,XmNautoUnmanage,FALSE,
		      NULL);
      if(!RMOD){
	rgb2hsv(bundle.red,bundle.green,bundle.blue);
	/*red*/
	str=NewString("Hue");
	red = XtVaCreateManagedWidget("red",xmScaleWidgetClass,
				      mixer,XmNorientation,XmHORIZONTAL,
				      XmNtitleString,str,
				      XmNshowValue,TRUE,
				      XmNtopAttachment,XmATTACH_FORM,
				      XmNtopOffset,110,
				      XmNwidth,260,
				      XmNvalue,(int)hue,
				      XmNmaximum,360,NULL);
	XmStringFree(str);  
	/*green*/
	str=NewString("Saturation");
	green = XtVaCreateManagedWidget("green",xmScaleWidgetClass,
					mixer,XmNorientation,XmHORIZONTAL,
					XmNtitleString,str,
					XmNshowValue,TRUE,
					XmNtopAttachment,XmATTACH_WIDGET,
					XmNtopWidget,red,
					XmNwidth,260,
					XmNvalue,(int)(saturation*100),
					XmNmaximum,100,NULL);
	XmStringFree(str);  
	/*blue*/
	str=NewString("Value");
	blue = XtVaCreateManagedWidget("blue",xmScaleWidgetClass,
				       mixer,XmNorientation,XmHORIZONTAL,
				       XmNtitleString,str,
				       XmNshowValue,TRUE,
				       XmNtopAttachment,XmATTACH_WIDGET,
				       XmNtopWidget,green,
				       XmNwidth,260,
				       XmNvalue,(int)(value*100),
				       XmNmaximum,100,NULL);
	XmStringFree(str);  
      }
      else{
	
	/*red*/
	str=NewString("Red");
	red = XtVaCreateManagedWidget("red",xmScaleWidgetClass,
				      mixer,XmNorientation,XmHORIZONTAL,
				      XmNtitleString,str,
				      XmNshowValue,TRUE,
				      XmNtopAttachment,XmATTACH_FORM,
				      XmNtopOffset,110,
				      XmNwidth,260,
				     XmNvalue,(int)(check_nan(bundle.red)*255),
				      XmNmaximum,255,NULL);
	XmStringFree(str);  
	    
	/*green*/
	str=NewString("Green");
	green = XtVaCreateManagedWidget("green",xmScaleWidgetClass,
					mixer,XmNorientation,XmHORIZONTAL,
					XmNtitleString,str,
					XmNshowValue,TRUE,
					XmNtopAttachment,XmATTACH_WIDGET,
					XmNtopWidget, red,
					XmNwidth,260,
				   XmNvalue,(int)(check_nan(bundle.green)*255),
					XmNmaximum,255,NULL);
	    XmStringFree(str);  
	    
	    
	    /*blue*/
	    str=NewString("Blue");
	    blue = XtVaCreateManagedWidget("blue",xmScaleWidgetClass,
					   mixer,XmNorientation,XmHORIZONTAL,
					   XmNtitleString,str,
					   XmNshowValue,TRUE,
					   XmNtopAttachment,XmATTACH_WIDGET,
					   XmNtopWidget,green,
					   XmNwidth,260,
				    XmNvalue,(int)(check_nan(bundle.blue)*255),
					   XmNmaximum,255,NULL);
	    XmStringFree(str);  
	    
	    
      }
	
      /*entry*/  
      str=NewString("Color Table Entry");
      entry = XtVaCreateManagedWidget("entry",xmScaleWidgetClass,
				      mixer,XmNtitleString,str,
				      XmNorientation,XmHORIZONTAL,
				      XmNshowValue,TRUE,
				      XmNtopAttachment,XmATTACH_WIDGET,
				      XmNtopWidget,blue,
				      XmNwidth,260,
				      XmNmaximum,max,
				      XmNminimum,min,
				      XmNvalue,center, NULL);
      XmStringFree(str);
      
      apply = XtVaCreateManagedWidget("Apply",xmPushButtonWidgetClass,
				      mixer, XmNbottomAttachment,
				      XmATTACH_FORM,
				      XmNleftAttachment,XmATTACH_FORM,NULL);
      
      reset = XtVaCreateManagedWidget("Reset",xmPushButtonWidgetClass,
				      mixer,XmNleftAttachment,
				      XmATTACH_WIDGET,
				      XmNbottomAttachment,XmATTACH_FORM,
				      XmNleftWidget,apply,NULL);
      
      done = XtVaCreateManagedWidget("Done",xmPushButtonWidgetClass,
				     mixer,XmNbottomAttachment,XmATTACH_FORM,
				     XmNrightAttachment,XmATTACH_FORM,
				     XmNleftAttachment,XmATTACH_WIDGET,
				     XmNleftWidget,reset,NULL);
	
      insert = XtVaCreateManagedWidget("Insert",xmPushButtonWidgetClass,
				       mixer,XmNbottomAttachment,
				       XmATTACH_WIDGET,
				       XmNleftAttachment,XmATTACH_FORM,
				       XmNbottomWidget,reset,NULL);
      
      delete = XtVaCreateManagedWidget("Delete",xmPushButtonWidgetClass,
				       mixer,XmNbottomAttachment,
				       XmATTACH_WIDGET,
				       XmNrightAttachment,XmATTACH_FORM,
					 XmNbottomWidget,done,NULL);
      XtAddCallback(red, XmNvalueChangedCallback,  
		    (XtCallbackProc)slide_changed,"red");
      XtAddCallback(red, XmNdragCallback,  (XtCallbackProc)slide_changed,"red");
      XtAddCallback(green, XmNvalueChangedCallback,  
		    (XtCallbackProc)slide_changed,"green");
      XtAddCallback(green, XmNdragCallback,  
		    (XtCallbackProc)slide_changed,"green");
      XtAddCallback(blue, XmNvalueChangedCallback,
		    (XtCallbackProc) slide_changed,"blue");
      XtAddCallback(blue, XmNdragCallback,  (XtCallbackProc)slide_changed,"blue");
      XtAddCallback(entry, XmNvalueChangedCallback, 
		    (XtCallbackProc)new_entry,NULL);
      XtAddCallback(entry, XmNdragCallback, 
		    (XtCallbackProc)new_entry,NULL);
      XtAddCallback(apply,XmNactivateCallback, 
		    (XtCallbackProc)apply_color,NULL);
      XtAddCallback(reset,XmNactivateCallback, 
		    (XtCallbackProc)do_reset_colors,NULL);
      XtAddCallback(done,XmNactivateCallback, (XtCallbackProc)quit_color,NULL);
      XtAddCallback(insert,XmNactivateCallback, 
		    (XtCallbackProc)insert_color,&entry);
      XtAddCallback(delete,XmNactivateCallback, 
		    (XtCallbackProc)delete_color,&entry);
      
      
      XtVaGetValues(parent,XmNcolormap,&map,NULL);
      
     if (IVE_TRUE_COLOR){
       color1.pixel=(unsigned long)IveGetPixel((max-min)/2+min);
       color2.pixel=color1.pixel;
     }
     else{
      status = XAllocColorCells(XtDisplay(parent),map,FALSE,masks,0,pixvals,2);
     }
     str = NewString("                      \n                      \n                      \n                      \n                      \n                      \n                      \n");
     original=XtVaCreateManagedWidget("_",xmPushButtonWidgetClass,
				      mixer,XmNtopAttachment,XmATTACH_FORM,
				      XmNleftAttachment,XmATTACH_FORM,
				      XmNleftOffset,30,
				      XmNwidth, 100,
				      XmNborderWidth,0,
				      XmNlabelString,str,
				      XmNbackground, color1.pixel, NULL);
     new=XtVaCreateManagedWidget("_",xmPushButtonWidgetClass,
				 mixer,XmNtopAttachment,XmATTACH_FORM,
				 XmNrightAttachment,XmATTACH_FORM,
				 XmNwidth, 100,
				 XmNborderWidth,0,
				 XmNrightOffset,30,
				 XmNlabelString,str,
				 XmNbackground, color2.pixel, NULL);
     
     color1.flags=DoRed|DoGreen|DoBlue;
     color2.flags=DoRed|DoGreen|DoBlue;
     
     /*	XQueryColor(XtDisplay(mixer),map, &color1);
	XQueryColor(XtDisplay(mixer),map, &color2);*/
     
     color2.pixel= pixvals[1];
     color2.red=(int)(check_nan(bundle.red)*65535.0);
     color2.blue=(int)(check_nan(bundle.blue)*65535.0);
     color2.green=(int)(check_nan(bundle.green)*65535.0);
     
     color1.pixel= pixvals[0];
     color1.red=(int)(check_nan(bundle.red)*65535.0);
     color1.blue=(int)(check_nan(bundle.blue)*65535.0);
     color1.green=(int)(check_nan(bundle.green)*65535.0);
     if (DisplayCells(XtDisplay(parent),
		      DefaultScreen(XtDisplay(parent))) < 9){
#if defined(IRIX)
       XStoreColor(XtDisplay(mixer),map,&color1);
       XStoreColor(XtDisplay(mixer),map,&color2);
#else	
       status=XStoreColor(XtDisplay(mixer),map,&color1);
       status=XStoreColor(XtDisplay(mixer),map,&color2);
#endif
     }
     else{
       (void)XAllocColor(XtDisplay(mixer), map, &color1);
       (void)XAllocColor(XtDisplay(mixer), map, &color2);
       XtVaSetValues(original, XmNbackground, color1.pixel, NULL);
       XtVaSetValues(new, XmNbackground, color2.pixel, NULL);
     }
     XtManageChild (mixer);
     update_all_();
    }
    else{
      
      make_help_widget(
		       "You must have at least eight\n\
bitplanes to use the color mixer");
      
    }
}
