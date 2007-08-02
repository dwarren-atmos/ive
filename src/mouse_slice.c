/*
  $Id: mouse_slice.c,v 1.21 2007-06-21 20:58:30 reinecke Exp $
  
  $Log: mouse_slice.c,v $
  Revision 1.21  2007-06-21 20:58:30  reinecke
  Commiting local modifications for transfer to svn

  Revision 1.20  2007/05/01 17:57:11  warren
  fixes for mouse slicing and line drawing on default map projections.

  Revision 1.19  2005/09/06 20:22:14  warren
  Move updates into current stream

  Revision 1.18.2.1  2005/09/06 17:55:23  warren
  fixed trajectories to deal with maps. Also first part of bold arrows for
  trajectories

  Revision 1.18  2002/08/09 19:57:59  warren
  Fixed for Linux and tested with DEC sun etc.
  most items inside #ifdef's, or were general cleanup/fixes
  
  Revision 1.17  1999/12/22 00:33:19  warren
  Fixed Dales crashing bug when changing from point slicer to normal. Also
  have some ifdef linuxes in there in case we ever find a fortran compiler.
  
  Revision 1.16  1998/07/28 21:55:41  harryive
  Yes, Virgina, windows do have borders!
  
  Revision 1.15  1997/10/30 23:07:51  davidive
  convert traj to c to dynamically allocate memory, also place trajectory
  inside black lines one each side.
  
  Revision 1.14  1997/10/22 19:35:07  davidive
  Turned the particle movement part of trajectories into a user transform
  and added unit checking to the default transform.
  
  Revision 1.13  1997/10/17 19:30:26  harry
  Fix mouse_slice for use in maps.
  
  Revision 1.12  1997/06/27 21:40:13  davidive
  Fix arbitrary slice line labeling
  
  Revision 1.11  1997/06/03 19:14:21  davidive
  The big mouse slicing changeover. We have the new form, only requires you
  set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
  always runs through init_points_ to catch all these things.
  note: only actually works when two dimentions have points set. Needs to
  be expanded into 1, 3 or 4.
  
  Revision 1.10  1997/05/16 22:15:17  harry
  More mouse slicing fixes.
  
*/

static char rcsid[] = "$Id: mouse_slice.c,v 1.21 2007-06-21 20:58:30 reinecke Exp $";

#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))
#include <X11/Intrinsic.h>
#include <Xm/XmAll.h>
#include <math.h>
#include <window.h>
#include <ive_for.h>


void get_slice(x1, y1, x2, y2, slice)
     float x1, y1, x2, y2;
     int slice; /*slice or window flag 0=slice 1=window*/
{
  double domain_intercept[4], domain_slope[4];
 float vpl,vpr,vpb,vpt,wdl,wdr,wdb,wdt, trade, spval=0.0; 
    float x1w, x2w, y1w, y2w; /*world coord*/
    float x3w, x4w, y3w, y4w; /*extras for 2 dimention line*/
    float wmin[4], wmax[4];
    float point1[4], point2[4], point3[4];
    int   linlog, mapflg, error, i, xaxis, yaxis, ax[2], use_buttons;
    int   defmap, exact_fit;

    if(x1 == x2 && y1 == y2)return;
        
    (void)getset_(&vpl, &vpr, &vpb, &vpt, &wdl, &wdr, &wdb, &wdt, &linlog);
    i=4;
    (void)getdarr_("domain_slope", domain_slope, &i, &error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, &error,16);
    (void)getivar_("use_buttons", &use_buttons, &error, 11);

    y1 = 1-y1;
    y2 = 1-y2;
    /* gks is backwards from X*/
    switch(linlog){
    case 1:
	break;
    case 2:
	wdb = (float)log10(wdb);
	wdt = (float)log10(wdt);
	break;
    case 3:
	wdl = (float)log10(wdl);
	wdr = (float)log10(wdr);
	break;
    case 4:
	wdl = (float)log10(wdl);
	wdr = (float)log10(wdr);
	wdb = (float)log10(wdb);
	wdt = (float)log10(wdt);
    }	
    if(use_buttons){
	xaxis = window_points_.xaxis;
	yaxis = window_points_.yaxis;
    }
    else{
/*      Note: only need to set ax1 and ax2 as we can only have one combined
              dimention
*/	
/*	float point1[4],point2[4],point3[4]; already declared */
	i=4;
	(void)getrarr_("point_1", point1, &i, &error,7);
	(void)getrarr_("point_2", point2, &i, &error,7);
	(void)getrarr_("point_3", point3, &i, &error,7);
	if (window_points_.xaxis == 0)
	    xaxis = window_points_.xaxis;
	else{
	    xaxis = 5;
	}
	if (window_points_.yaxis == 0)
	    yaxis = window_points_.yaxis;
	else{
	    yaxis = 5;
	}
	ax[0] = ax[1] = 0;
	x1w = x2w = x3w = x4w = 0.;
	    if(xaxis== 5){
		if(yaxis != 1 && (point1[0] != point2 [0] || point2[0] != 
				  point3[0] || point1[0] != point3 [0]))
		    ax[0] = 1;
		if(yaxis != 2 && (point1[1] != point2 [1] || point2[1] != 
				  point3[1] || point1[1] != point3 [1])){
		    if(ax[0] == 0) ax[0] = 2;
		    else ax[1] = 2;
		}
		if(yaxis != 3 && (point1[2] != point2 [2] || point2[2] != 
				  point3[2] || point1[2] != point3 [2])){
		    if(ax[0] == 0) ax[0] = 3;
		    else ax[1] = 3;
		}
		if(yaxis != 4 && (point1[3] != point2 [3] || point2[3] != 
				  point3[3] || point1[3] != point3 [3]))
		    ax[1] = 4;
	    }
	if(yaxis== 5){
	    if(xaxis != 1 && (point1[0] != point2 [0] || point2[0] != 
			      point3[0] || point1[0] != point3 [0]))
		ax[0] == 1;
	    if(xaxis != 2 && (point1[1] != point2 [1] || point2[1] != 
			      point3[1] || point1[1] != point3 [1])){
		    if(ax[0] == 0) ax[0] = 2;
		    else ax[1] = 2;
		}
	    if(xaxis != 3 && (point1[2] != point2 [2] || point2[2] != 
			      point3[2] || point1[2] != point3 [2])){
		    if(ax[0] == 0) ax[0] = 2;
		    else ax[1] = 3;
		}
	    if(xaxis != 4 && (point1[3] != point2 [3] || point2[3] != 
			      point3[3] || point1[3] != point3 [3]))
		ax[1] = 4;
	}
	(void)getrarr_("plwmin", wmin, &i, &error, 6);
	(void)getrarr_("plwmax", wmax, &i, &error, 6);
    }

    
    if(xaxis == 5){
	x1w = MIN(wdl,wdr) + (x1-vpl)/(vpr-vpl)*
	    (wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	x2w = MIN(wdl,wdr) + (x2-vpl)/(vpr-vpl)*
	    (wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	x3w = MIN(wdl,wdr) + (x1-vpl)/(vpr-vpl)*
	    (wmax[ax[1] - 1] - wmin[ax[1] - 1]);
	x4w = MIN(wdl,wdr) + (x2-vpl)/(vpr-vpl)*
	    (wmax[ax[1] - 1] - wmin[ax[1] - 1]);
	    }
    else{
	x1w = MIN(wdl,wdr) + (x1-vpl)/(vpr-vpl)*(MAX(wdl,wdr) - MIN(wdl,wdr));
	x2w = MIN(wdl,wdr) + (x2-vpl)/(vpr-vpl)*(MAX(wdl,wdr) - MIN(wdl,wdr));
	}
    if(yaxis == 5){
	y1w = MIN(wdb,wdt) + (y1-vpb)/(vpt-vpb)*
	    (wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	y2w = MIN(wdb,wdt) + (y2-vpb)/(vpt-vpb)*
	    (wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	y3w = MIN(wdb,wdt) + (y1-vpb)/(vpt-vpb)*
	    (wmax[ax[1] - 1] - wmin[ax[1] - 1]);
	y3w = MIN(wdb,wdt) + (y2-vpb)/(vpt-vpb)*
	    (wmax[ax[1] - 1] - wmin[ax[1] - 1]);
    }
    else{
	y1w = MIN(wdb,wdt) + (y1-vpb)/(vpt-vpb)*(MAX(wdb,wdt) - MIN(wdb,wdt));
	y2w = MIN(wdb,wdt) + (y2-vpb)/(vpt-vpb)*(MAX(wdb,wdt) - MIN(wdb,wdt));
    }
    
    switch(linlog){
    case 1:
	break;
    case 2:
	wdb = (float)pow(10,wdb);
	wdt = (float)pow(10,wdt);
	break;
    case 3:
	wdl = (float)pow(10,wdl);
	wdr = (float)pow(10,wdr);
	break;
    case 4:
	wdl = (float)pow(10,wdl);
	wdr = (float)pow(10,wdr);
	wdb = (float)pow(10,wdb);
	wdt = (float)pow(10,wdt);
    }	
    switch(linlog){
    case 1:
	break;
    case 2:
	y1w = (float)pow(10,y1w);
	y2w = (float)pow(10,y2w);
	break;
    case 3:
	x1w = (float)pow(10,x1w);
	x2w = (float)pow(10,x2w);
	break;
    case 4:
	x1w = (float)pow(10,x1w);
	x2w = (float)pow(10,x2w);
	y1w = (float)pow(10,y1w);
	y2w = (float)pow(10,y2w);
    }	
    if(x1w < wdl && x1w < wdr) x1w  = MIN(wdl,wdr);
    if(x2w < wdl && x2w < wdr) x2w  = MIN(wdl,wdr);
    if(x1w > wdl && x1w > wdr) x1w  = MAX(wdl,wdr);
    if(x2w > wdl && x2w > wdr) x2w  = MAX(wdl,wdr);
    if(y1w < wdb && y1w < wdt) y1w  = MIN(wdb,wdt);
    if(y2w < wdb && y2w < wdt) y2w  = MIN(wdb,wdt);
    if(y1w > wdb && y1w > wdt) y1w  = MAX(wdb,wdt);
    if(y2w > wdb && y2w > wdt) y2w  = MAX(wdb,wdt);

    if(x1w == x2w && y1w == y2w)return;
    
    (void)getlvar_("mapflg", &mapflg, &error, 6);
    (void)getlvar_("defmap", &defmap, &error, 6);
    (void)getlvar_("exact_fit", &exact_fit, &error, 9);
    if(mapflg && (!defmap || !exact_fit)){
	float lat[4], lon[4], x[4], y[4];
	int npts=4;
	(void)maptri_(&x1w,&y1w,&lat[0],&lon[0]);
	(void)maptri_(&x2w,&y2w,&lat[3],&lon[3]);
	(void)maptri_(&x1w,&y2w,&lat[1],&lon[1]);
	(void)maptri_(&x2w,&y1w,&lat[2],&lon[2]);
	(void)lonlat_2_phys_trans_(x, y, lon, lat, &npts);
	if((x[0]!=x[1]) || (x[2]!=x[3]) || 
	   (y[0]!=y[2]) || (y[1]!=y[3])){
	  /* We need to call mapset so.. (as long as it's not a slice) */
	  if(!slice){		
	    (void)make_help_widget_("Nonrectangular window has been chosen\nIVE only supports rectangular slices within the data space");
	    return;
	  }
	  else{
	    char buff[256];
	    sprintf(buff,"map_window=  %f, %f, %f, %f",
		    MIN(x1w,x2w),MIN(y1w,y2w),MAX(x1w,x2w),MAX(y1w,y2w));
	    /*need smallest val first*/
	    driver(buff);
	    return;
	  }
	}
	x1w=MIN(x[0], x[2]);
	x2w=MAX(x[0], x[2]);
	y1w=MIN(y[0], y[1]);
	y2w=MAX(y[0], y[1]);
    }
    

    if(!slice) {
	char buffx[256], buffy[256];
	/*	int error; already declared*/
	driver_notwin("point_setup");
	switch(xaxis){
	case 0: 
	    (void)getavar_("field", buffx, &error, 256);
	    (void)make_help_widget_("Sorry, mouse slicing the field is not yet supported");
	    return;
	    break;
	case 1:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope,domain_intercept,&spval);
	    (void)scale_(&x2w,&i,domain_slope,domain_intercept,&spval);
	    sprintf(buffx,"xpoint=%f,%f", x1w,x2w);
	    break;
	case 2:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope+1,domain_intercept+1,&spval);
	    (void)scale_(&x2w,&i,domain_slope+1,domain_intercept+1,&spval);
	    sprintf(buffx,"ypoint=%f,%f", x1w,x2w);
	    break;
	case 3:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope+2,domain_intercept+2,&spval);
	    (void)scale_(&x2w,&i,domain_slope+2,domain_intercept+2,&spval);
	    sprintf(buffx,"zpoint=%f,%f", x1w,x2w);
	    break;
	case 4:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope+3,domain_intercept+3,&spval);
	    (void)scale_(&x2w,&i,domain_slope+3,domain_intercept+3,&spval);
	    sprintf(buffx,"tpoint=%f,%f", x1w,x2w);
	    break;
	case 5:
	    (void)make_help_widget_("Sorry, mouse slicing mixed domains is not yet supported");
	    return;
	    break;
	}
	switch(yaxis){
	case 0: 
	    (void)getavar_("field", buffy, &error, 5, 256);
	    (void)make_help_widget_("Sorry, mouse slice the field is not yet supported");
	    return;
	    break;
	case 1:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope,domain_intercept,&spval);
	    (void)scale_(&y2w,&i,domain_slope,domain_intercept,&spval);
	    sprintf(buffy,"xpoint=%f,%f", y1w,y2w);
	    return;
	    break;
	case 2:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope+1,domain_intercept+1,&spval);
	    (void)scale_(&y2w,&i,domain_slope+1,domain_intercept+1,&spval);
	    sprintf(buffy,"ypoint=%f,%f", y1w,y2w);
	    break;
	case 3:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope+2,domain_intercept+2,&spval);
	    (void)scale_(&y2w,&i,domain_slope+2,domain_intercept+2,&spval);
	    sprintf(buffy,"zpoint=%f,%f", y1w,y2w);
	    break;
	case 4:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope+3,domain_intercept+3,&spval);
	    (void)scale_(&y2w,&i,domain_slope+3,domain_intercept+3,&spval);
	    sprintf(buffy,"tpoint=%f,%f", y1w,y2w);
	    break;
	case 5:
	    (void)make_help_widget_("Sorry, mouse slicing mixed domains is not yet supported");
	    return;
	    break;
	}
	driver_notwin(buffx);
	driver_notwin(buffy);
	driver("switch_slicer=points");
    }
    else{
	char buffx[256], buffy[256];
	/*	int error; already declared*/
	if(wdb > wdt){
	    if(y1w<y2w){
		trade = y1w;
		y1w = y2w;
		y2w = trade;
	    }
	}
	else
	{
	    if(y1w>y2w){
		trade = y1w;
		y1w = y2w;
		y2w = trade;
	    }
	}	    
	if(wdl > wdr){
	    if(x1w<x2w){
		trade = x1w;
		x1w = x2w;
		x2w = trade;
	    }
	}
	else
	{
	    if(x1w>x2w){
		trade = x1w;
		x1w = x2w;
		x2w = trade;
	    }
	}	    

	switch(xaxis){
	case 0: 
/*	    (void)getavar_('field', buffx, &error, 256);
	    (void)make_help_widget_("Sorry, mouse windowing the field is not yet supported");*/
	    buffx[0] = '\0';
	    break;
	case 1:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope,domain_intercept,&spval);
	    (void)scale_(&x2w,&i,domain_slope,domain_intercept,&spval);
	    sprintf(buffx,"window=%f,%f", x1w,x2w);
	    break;
	case 2:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope+1,domain_intercept+1,&spval);
	    (void)scale_(&x2w,&i,domain_slope+1,domain_intercept+1,&spval);
	    sprintf(buffx,"window=,,%f,%f", x1w,x2w);
	    break;
	 case 3:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope+2,domain_intercept+2,&spval);
	    (void)scale_(&x2w,&i,domain_slope+2,domain_intercept+2,&spval);
	    sprintf(buffx,"window=,,,,%f,%f", x1w,x2w);
	    break;
	case 4:
	    i=1;
	    (void)scale_(&x1w,&i,domain_slope+3,domain_intercept+3,&spval);
	    (void)scale_(&x2w,&i,domain_slope+3,domain_intercept+3,&spval);
	    sprintf(buffx,"window=,,,,,,%f,%f", x1w,x2w);
	    break;
	case 5:
	    (void)scale_(&x1w,&i,domain_slope+3,domain_intercept+(ax[0]-1),
			 &spval);
	    (void)scale_(&x2w,&i,domain_slope+3,domain_intercept+(ax[0]-1),
			 &spval);
	    (void)scale_(&x3w,&i,domain_slope+3,domain_intercept+(ax[1]-1),
			 &spval);
	    (void)scale_(&x4w,&i,domain_slope+3,domain_intercept+(ax[1]-1),
			 &spval);
	    sprintf(buffx,"window=");
	    switch (ax[0]){
	    case 1:
		switch(ax[1]){
		case 2:
	    sprintf(buffx,"%s%f,%f,%f,%f",buffx,x1w,x2w,x3w,x4w);
		    break;
		case 3:
		    sprintf(buffx,"%s%f,%f,,,%f,%f",buffx,x1w,x2w,x3w,x4w);
		    break;
		case 4:
		    sprintf(buffx,"%s%f,%f,,,,,%f,%f",buffx,x1w,x2w,x3w,x4w);
		    break;
		}
		break;
	    case 2:
		switch(ax[1]){
		case 3:
		    sprintf(buffx,"%s,,%f,%f,%f,%f",buffx,x1w,x2w,x3w,x4w);
		    break;
		case 4:
		    sprintf(buffx,"%s,,%f,%f,,,%f,%f",buffx,x1w,x2w,x3w,x4w);
		    break;
		}
		break;
	    case 3:
		switch(ax[1]){
		case 4:
		    sprintf(buffx,"%s,,,,%f,%f,%f,%f",buffx,x1w,x2w,x3w,x4w);
		    break;
		}
		break;
	    }
	    break;
	}
	switch(yaxis){
	case 0: 
/*	    (void)getavar_('field', buffy, &error, 256);
	    (void)make_help_widget_("Sorry, mouse windowing the field is not yet supported"); */
	    buffy[0] = '\0';
	    break;
	case 1:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope,domain_intercept,&spval);
	    (void)scale_(&y2w,&i,domain_slope,domain_intercept,&spval);
	    sprintf(buffy,"window=%f,%f", y1w,y2w);
	    return;
	    break;
	 case 2:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope+1,domain_intercept+1,&spval);
	    (void)scale_(&y2w,&i,domain_slope+1,domain_intercept+1,&spval);
	    sprintf(buffy,"window=,,%f,%f", y1w,y2w);
	    break;
	case 3:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope+2,domain_intercept+2,&spval);
	    (void)scale_(&y2w,&i,domain_slope+2,domain_intercept+2,&spval);
	    sprintf(buffy,"window=,,,,%f,%f", y1w,y2w);
	    break;
	case 4:
	    i=1;
	    (void)scale_(&y1w,&i,domain_slope+3,domain_intercept+3,&spval);
	    (void)scale_(&y2w,&i,domain_slope+3,domain_intercept+3,&spval);
	    sprintf(buffy,"window=,,,,,,%f,%f", y1w,y2w);
	    break;
	case 5:
	    sprintf(buffy,"window=");
	    switch (ax[0]){
	    case 1:
		switch(ax[1]){
		case 2:
		    sprintf(buffy,"%s%f,%f,%f,%f",buffy,x1w,x2w,x3w,x4w);
		    break;
		case 3:
		    sprintf(buffy,"%s%f,%f,,,%f,%f",buffy,x1w,x2w,x3w,x4w);
		    break;
		case 4:
		    sprintf(buffy,"%s%f,%f,,,,,%f,%f",buffy,x1w,x2w,x3w,x4w);
		    break;
		}
		break;
	    case 2:
		switch(ax[1]){
		case 3:
		    sprintf(buffy,"%s,,%f,%f,%f,%f",buffy,x1w,x2w,x3w,x4w);
		    break;
		case 4:
		    sprintf(buffy,"%s,,%f,%f,,,%f,%f",buffy,x1w,x2w,x3w,x4w);
		    break;
		}
		break;
	    case 3:
		switch(ax[1]){
		case 4:
		    sprintf(buffy,"%s,,,,%f,%f,%f,%f",buffy,x1w,x2w,x3w,x4w);
		    break;
		}
		break;
	    }
	    break;
	}
	if(buffx[0] != '\0')driver_notwin(buffx);
	if(buffy[0] != '\0')driver_notwin(buffy);
    }
}

void draw_line(x1, y1, x2, y2)
     float x1, y1, x2, y2;
{
    char buff[80];
    float vpl,vpr,vpb,vpt,wdl,wdr,wdb,wdt, spval=0.0; 
    int   linlog, mapflg, defmap,exact_fit,error, i, xaxis, yaxis, ax[2], use_buttons;
    float x1w, x2w, y1w, y2w; /*world coord*/
    float x3w, x4w, y3w, y4w; /*extras for 2 dimention line*/
    float wmin[4], wmax[4];
    double domain_intercept[4], domain_slope[4];
    float point1[4], point2[4], point3[4];
    
    if(x1 == x2 && y1 == y2)return;
        
    (void)getset_(&vpl, &vpr, &vpb, &vpt, &wdl, &wdr, &wdb, &wdt, &linlog);
    i=4;
    (void)getdarr_("domain_slope", domain_slope, &i, &error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, &error,16);
    (void)getivar_("use_buttons", &use_buttons, &error, 11);
    switch(linlog){
    case 1:
	break;
    case 2:
	wdb = (float)log10(wdb);
	wdt = (float)log10(wdt);
	y1 = (float)log10(y1);
	y2 = (float)log10(y2);
	break;
    case 3:
	wdl = (float)log10(wdl);
	wdr = (float)log10(wdr);
	x1 = (float)log10(x1);
	x2 = (float)log10(x2);
	break;
    case 4:
	wdl = (float)log10(wdl);
	wdr = (float)log10(wdr);
	wdb = (float)log10(wdb);
	wdt = (float)log10(wdt);
	x1 = (float)log10(x1);
	x2 = (float)log10(x2);
	y1 = (float)log10(y1);
	y2 = (float)log10(y2);
    }	
    if(use_buttons){
	xaxis = window_points_.xaxis;
	yaxis = window_points_.yaxis;
    }
    else{
/*      Note: only need to set ax1 and ax2 as we can only have one combined
              dimention
*/	
/*	float point1[4],point2[4],point3[4]; already declared*/
	(void)getrarr_("point_1", point1, &i, &error,7);
	(void)getrarr_("point_2", point2, &i, &error,7);
	(void)getrarr_("point_3", point3, &i, &error,7);
	
	if (window_points_.xaxis == 0)
	    xaxis = window_points_.xaxis;
	else{
	    xaxis = 5;
	}
	if (window_points_.yaxis == 0)
	    yaxis = window_points_.yaxis;
	else{
	    yaxis = 5;
	}
	ax[0] = ax[1] = 0;
	x1w = x2w = x3w = x4w = 0.;
	    if(xaxis== 5){
		if(yaxis != 1 && (point1[0] != point2 [0] || point2[0] != 
				  point3[0] || point1[0] != point3 [0]))
		    ax[0] = 1;
		if(yaxis != 2 && (point1[1] != point2 [1] || point2[1] != 
				  point3[1] || point1[1] != point3 [1])){
		    if(ax[0] == 0) ax[0] = 2;
		    else ax[1] = 2;
		}
		if(yaxis != 3 && (point1[2] != point2 [2] || point2[2] != 
				  point3[2] || point1[2] != point3 [2])){
		    if(ax[0] == 0) ax[0] = 3;
		    else ax[1] = 3;
		}
		if(yaxis != 4 && (point1[3] != point2 [3] || point2[3] != 
				  point3[3] || point1[3] != point3 [3]))
		    ax[1] = 4;
	    }
	if(yaxis== 5){
	    if(xaxis != 1 && (point1[0] != point2 [0] || point2[0] != 
			      point3[0] || point1[0] != point3 [0]))
		ax[0] == 1;
	    if(xaxis != 2 && (point1[1] != point2 [1] || point2[1] != 
			      point3[1] || point1[1] != point3 [1])){
		    if(ax[0] == 0) ax[0] = 2;
		    else ax[1] = 2;
		}
	    if(xaxis != 3 && (point1[2] != point2 [2] || point2[2] != 
			      point3[2] || point1[2] != point3 [2])){
		    if(ax[0] == 0) ax[0] = 2;
		    else ax[1] = 3;
		}
	    if(xaxis != 4 && (point1[3] != point2 [3] || point2[3] != 
			      point3[3] || point1[3] != point3 [3]))
		ax[1] = 4;
	}
	(void)getrarr_("plwmin_scaled", wmin, &i, &error, 13);
	(void)getrarr_("plwmax_scaled", wmax, &i, &error, 13);
    }

    y1=1-y1;
    y2=1-y2; /* X has y at 0 at the top */

    
    if(xaxis == 5){
	x1w = wdl + (x1-vpl)/(vpr-vpl)*(wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	x2w = wdl + (x2-vpl)/(vpr-vpl)*(wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	x3w = wdl + (x1-vpl)/(vpr-vpl)*(wmax[ax[1] - 1] - wmin[ax[1] - 1]);
	x4w = wdl + (x2-vpl)/(vpr-vpl)*(wmax[ax[1] - 1] - wmin[ax[1] - 1]);
	    }
    else{
	x1w = wdl + (x1-vpl)/(vpr-vpl)*(wdr - wdl);
	x2w = wdl + (x2-vpl)/(vpr-vpl)*(wdr - wdl);
	}
    if(yaxis == 5){
	y1w = wdb + (y1-vpb)/(vpt-vpb)*(wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	y2w = wdb + (y2-vpb)/(vpt-vpb)*(wmax[ax[0] - 1] - wmin[ax[0] - 1]);
	y3w = wdb + (y1-vpb)/(vpt-vpb)*(wmax[ax[1] - 1] - wmin[ax[1] - 1]);
	y4w = wdb + (y2-vpb)/(vpt-vpb)*(wmax[ax[1] - 1] - wmin[ax[1] - 1]);
    }
    else{
	y1w = wdb + (y1-vpb)/(vpt-vpb)*(wdt - wdb);
	y2w = wdb + (y2-vpb)/(vpt-vpb)*(wdt - wdb);
    }
    
    switch(linlog){
    case 1:
	break;
    case 2:
	y1w = (float)pow(10,y1w);
	y2w = (float)pow(10,y2w);
	if(yaxis == 5){
	    y3w = (float)pow(10,y3w);
	    y4w = (float)pow(10,y4w);
	}	    
	break;
    case 3:
	x1w = (float)pow(10,x1w);
	x2w = (float)pow(10,x2w);
	if(xaxis == 5){
	    x3w = (float)pow(10,x3w);
	    x4w = (float)pow(10,x4w);
	}	    
	break;
    case 4:
	x1w = (float)pow(10,x1w);
	x2w = (float)pow(10,x2w);
	y1w = (float)pow(10,y1w);
	y2w = (float)pow(10,y2w);
	if(yaxis == 5){
	    y3w = (float)pow(10,y3w);
	    y4w = (float)pow(10,y4w);
	}	    
	if(xaxis == 5){
	    x3w = (float)pow(10,x3w);
	    x4w = (float)pow(10,x4w);
	}	    
	break;
    }	
    if(x1w < wdl && x1w < wdr) x1w  = MIN(wdl,wdr);
    if(x2w < wdl && x2w < wdr) x2w  = MIN(wdl,wdr);
    if(x1w > wdl && x1w > wdr) x1w  = MAX(wdl,wdr);
    if(x2w > wdl && x2w > wdr) x2w  = MAX(wdl,wdr);
    if(y1w < wdb && y1w < wdt) y1w  = MIN(wdb,wdt);
    if(y2w < wdb && y2w < wdt) y2w  = MIN(wdb,wdt);
    if(y1w > wdb && y1w > wdt) y1w  = MAX(wdb,wdt);
    if(y2w > wdb && y2w > wdt) y2w  = MAX(wdb,wdt);
    
    if(x1w == x2w && y1w == y2w)return;
    
    /*    (void)getlvar_("mapflg", &mapflg, &error, 6);
	  if(mapflg){*/
    (void)getlvar_("mapflg", &mapflg, &error, 6);
    (void)getlvar_("defmap", &defmap, &error, 6);
    (void)getlvar_("exact_fit", &exact_fit, &error, 9);
    if(mapflg && (!defmap || !exact_fit)){
	float lat[4], lon[4], x[4], y[4];
	int npts=4;
	/* 
	 *x[0],y[0]      x[2],y[2]    x1w,y1w
	 *x[1],y[1]      x[3],y[3]                 x2w,y2w
	 */
	(void)maptri_(&x1w,&y1w,&lat[0],&lon[0]);
	(void)maptri_(&x2w,&y2w,&lat[3],&lon[3]);
	(void)maptri_(&x1w,&y2w,&lat[1],&lon[1]);
	(void)maptri_(&x2w,&y1w,&lat[2],&lon[2]);
	(void)lonlat_2_phys_trans_(x, y, lon, lat, &npts);
	if((x[0]!=x[1]) || (x[2]!=x[3]) || (y[0]!=y[2]) || (y[1]!=y[3])){
	    (void)make_help_widget_("Nonrectangular window has been chosen\n\
IVE only supports rectangular windows within the data space");
	    return;
	}
	x1w=MIN(x[0],x[2]);
	x2w=MAX(x[0],x[2]);
	y1w=MIN(y[0],y[1]);
	y2w=MAX(y[0],y[1]);
    }
    switch(xaxis){
    case 0: 
      /* field pre scaled */
      break;
    case 1:
      i=1;
      (void)scale_(&x1w,&i,domain_slope,domain_intercept,&spval);
      (void)scale_(&x2w,&i,domain_slope,domain_intercept,&spval);
      break;
    case 2:
      i=1;
      (void)scale_(&x1w,&i,domain_slope+1,domain_intercept+1,&spval);
      (void)scale_(&x2w,&i,domain_slope+1,domain_intercept+1,&spval);
      break;
    case 3:
      i=1;
      (void)scale_(&x1w,&i,domain_slope+2,domain_intercept+2,&spval);
      (void)scale_(&x2w,&i,domain_slope+2,domain_intercept+2,&spval);
      break;
    case 4:
      i=1;
      (void)scale_(&x1w,&i,domain_slope+3,domain_intercept+3,&spval);
      (void)scale_(&x2w,&i,domain_slope+3,domain_intercept+3,&spval);
      break;
    case 5:
      break;
    }
    switch(yaxis){
    case 0: 
      /* filed pre scaled */
      break;
    case 1:
      i=1;
      (void)scale_(&y1w,&i,domain_slope,domain_intercept,&spval);
      (void)scale_(&y2w,&i,domain_slope,domain_intercept,&spval);
      break;
    case 2:
      i=1;
      (void)scale_(&y1w,&i,domain_slope+1,domain_intercept+1,&spval);
      (void)scale_(&y2w,&i,domain_slope+1,domain_intercept+1,&spval);
      break;
    case 3:
      i=1;
      (void)scale_(&y1w,&i,domain_slope+2,domain_intercept+2,&spval);
      (void)scale_(&y2w,&i,domain_slope+2,domain_intercept+2,&spval);
      break;
    case 4:
      i=1;
      (void)scale_(&y1w,&i,domain_slope+3,domain_intercept+3,&spval);
      (void)scale_(&y2w,&i,domain_slope+3,domain_intercept+3,&spval);
      break;
    case 5:
      break;
    }
    sprintf(buff,"draw_line=%f,%f,%f,%f",x1w, y1w, x2w, y2w);
    driver(buff);
}


void mouse_traj(x, y)
     float x, y;
{
  double domain_intercept[4], domain_slope[4];
  extern float cfux_(), cfuy_();
  float vpl,vpr,vpb,vpt,wdl,wdr,wdb,wdt, trade, spval=0.0; 
  float x1w, x2w, y1w, y2w; /*world coord*/
  float myx, myy,lon,lat,xx,yy;
  float wmin[4], wmax[4];
  float point[4],xloc,yloc,zloc,time;
  int   linlog, mapflg, error, i, xaxis, yaxis, ax[2], use_buttons;
  int   defmap, exact_fit;
  char buff[256];
  
  
  (void)getset_(&vpl, &vpr, &vpb, &vpt, &wdl, &wdr, &wdb, &wdt, &linlog);
  i=4;
  (void)getdarr_("domain_slope", domain_slope, &i, &error,12);
  (void)getdarr_("domain_intercept", domain_intercept, &i, &error,16);
  (void)getivar_("use_buttons", &use_buttons, &error, 11);
  (void)getlvar_("mapflg", &mapflg, &error, 6);
  (void)getlvar_("defmap", &defmap, &error, 6);
  (void)getlvar_("exact_fit", &exact_fit, &error, 9);
  (void)getrvar_("xloc",&xloc, &error,4);
  (void)getrvar_("yloc",&yloc, &error,4);
  (void)getrvar_("zloc",&zloc, &error,4);
  (void)getrvar_("time",&time, &error,4);
  y = 1-y;
  /* gks is backwards from X*/
//  printf("wdl %f, wdr %f, wdb %f, wdt %f\n",wdl,wdr,wdb,wdt);
  myx=wdl+(((x-vpl)/(vpr-vpl))*(wdr-wdl));
  myy=wdb+(((y-vpb)/(vpt-vpb))*(wdt-wdb));
//  printf("myx %f myy %f - pre linlog\n",myx,myy);

  switch(linlog){
  case 1:
    break;
  case 2:
    myx = (float)log10(myx);
    break;
  case 3:
    myy = (float)log10(myy);
    break;
  case 4:
    myx = (float)log10(myx);
    myy = (float)log10(myy);
  }	
  
  //if(mapflg && (!defmap || !exact_fit)){
  if(mapflg){
    i=1;
//    printf("myx %f myy %f - post linlog\n",myx,myy);
    (void)maptri_(&myx,&myy,&lat,&lon);
//   printf("lon %f lat %f - after maptri\n",lon,lat);
    (void)lonlat_2_phys_trans_(&xx, &yy, &lon, &lat, &i);
//   printf("xx %f yy %f - after lonlat_2_phys\n",xx,yy);
    myx=xx;
    myy=yy;
  }
  if(use_buttons){
    point[0]=xloc;
    point[1]=yloc;
    point[2]=zloc;
    point[3]=time;
    xaxis = window_points_.xaxis;
    yaxis = window_points_.yaxis;
    point[xaxis-1] = myx;
    point[yaxis-1] = myy;
    i=1;
    (void)scale_(point,&i,domain_slope,domain_intercept,&spval);
    (void)scale_(point+1,&i,domain_slope+1,domain_intercept+1,&spval);
    (void)scale_(point+2,&i,domain_slope+2,domain_intercept+2,&spval);
    (void)scale_(point+3,&i,domain_slope+3,domain_intercept+3,&spval);
    sprintf(buff,"type=TRAJECTORY;start_traj= %f, %f, %f, %f", 
	    point[0], point[1], point[2], point[3]);
    printf("%s\n",buff);
  }
  else{
    (void)make_help_widget_
      ("can't do trajectories on non orthoganol plots yet");
    return;
  }
  
  driver_notwin(buff);
}
