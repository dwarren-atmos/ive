static char rcsid[] = "$Id: draw_point.c,v 1.5 2007-06-21 20:58:30 reinecke Exp $";
/* $Id $
   $Log: draw_point.c,v $
   Revision 1.5  2007-06-21 20:58:30  reinecke
   Commiting local modifications for transfer to svn

   Revision 1.4  2007/01/03 23:50:26  reinecke
   Added ; after two new printf statements.

   Revision 1.3  2007/01/03 23:41:05  warren
   Print lon lat of point in terminal

   Revision 1.2  2006-10-05 16:44:00  reinecke
   fixed integer numbers of different point types (where negative, should be positive)

   Revision 1.1  2006/08/23 16:53:21  warren
   You need this to make the draw_point command work. Also added an optional scaling factor.

*/
  
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>
#ifndef MEMDBG
#include <stdlib.h>
#endif
#include <window.h>
#include <xgks.h>

void draw_point_(error)
    int *error;
{
    char *ptr, **argv, **get_args(), cmdstr[768];
    float x, y, xt,yt;
    Gpoint pt[1];
    int npts, i, argc, xaxis, yaxis, mapflg, ax[2], use_buttons, type, color, no=0, yes=1 ;
	int defmap, exact_fit;
    double domain_intercept[4], domain_slope[4];
    float x1w, x2w, y1w, y2w; /*world coord*/
    float x3w, x4w, y3w, y4w; /*extras for 2 dimention line*/
    float point1[3], point2[3], point3[3];
    float spval=0.0, scale=2.5;
    int minus_one=-1;
    argv = get_args(&argc);
    
    if(argc < 3 ){
	(void)make_help_widget_("draw_point requires 1 points \
consisting of an xy pair, an optional point type and an optional scale factor:\n draw_line=x, y,[dot,plus,star,circle,x],[2]");
	*error=1;
        return;
    }
    


    i=4;
    (void)getdarr_("domain_slope", domain_slope, &i, error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, error,16);
    (void)getivar_("use_buttons", &use_buttons, error, 11);
    (void)getivar_("hicolor", &color, error, 7);
    (void)getlvar_("mapflg",&mapflg,error,6);
    (void)getlvar_("defmap",&defmap,error,6);
    (void)getlvar_("exact_fit",&exact_fit,error,9);

    xaxis = window_points_.xaxis;
    yaxis = window_points_.yaxis;
    i=0;
    npts=1;
    x = (float)atof(argv[1]);
    y = (float)atof(argv[2]);
    if(argc==3)
      type=4;
    else
      switch(argv[3][0]){
      case 'd':
      case 'D': type=1;
	break;
      case 'p':
      case 'P': type=2;
	break;
      case 's':
      case 'S': type=3;
	break;
      case 'c':
      case 'C': type=4;
	break;
      case 'x':
      case 'X': type=5;
	break;
      default:
	type = 4;
      }
    if(argc>4)
      scale= scale*(float)atof(argv[4]);

    if(xaxis != 0)
	(void)unscale_(&x,&npts, domain_slope +(xaxis-1), 
		       domain_intercept + (xaxis-1), &spval);

    if(yaxis != 0)
	(void)unscale_(&y,&npts, domain_slope +(yaxis-1), 
		       domain_intercept +(yaxis-1), &spval);

    (void)setlvar_("savflg", &yes, error, 6);
    (void)makseg_(&yes);

    (void)phys_2_lonlat_trans_(&x,&y,&xt,&yt,&npts);
    printf("Lon/LAT: %f, %f\n",xt,yt);

    if(mapflg & !(defmap || exact_fit)){
      (void)maptrn_(&yt,&xt,&x,&y);
	}

    gsetmarkersize(scale);
    gsetmarkertype(type);
    gsetmarkercolourind(color);
    pt[0].x=x;
    pt[0].y=y;
    gpolymarker(1,pt);
    /*    point_(&x,&y);*/
    (void)gcloseseg();
}

    void draw_point_map_(error)
    int *error;
{
    char *ptr, **argv, **get_args(), cmdstr[768];
    float x, y, xt,yt;
    Gpoint pt[1];
    int npts, i, argc, xaxis, yaxis, mapflg, ax[2], use_buttons, type, color,no=0, yes=1 ;
	int defmap, exact_fit;
	float ireal, jreal;
    double domain_intercept[4], domain_slope[4];
    float x1w, x2w, y1w, y2w; /*world coord*/
    float x3w, x4w, y3w, y4w; /*extras for 2 dimention line*/
    float point1[3], point2[3], point3[3];
    float spval=0.0,scale=2.5;
    int minus_one=-1;
    argv = get_args(&argc);
    
    if(argc < 3 ){
	(void)make_help_widget_("draw_point requires 1 points \
consisting of an xy pair, an optional point type and an optional scale factor:\n draw_line_map=x, y,[dot,plus,star,circle,x],[2]");
	*error=1;
        return;
    }
    
   (void)getlvar_("mapflg",&mapflg,error,6);
    if(!mapflg){
      (void)make_help_widget_("Not plotting on a map - usr draw_point instead");
      error=1;
      return;
    }


    i=4;
    (void)getdarr_("domain_slope", domain_slope, &i, error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, error,16);
    (void)getivar_("use_buttons", &use_buttons, error, 11);
    (void)getlvar_("mapflg",&mapflg,error,6);
    (void)getlvar_("defmap",&defmap,error,6);
    (void)getlvar_("exact_fit",&exact_fit,error,9);
    (void)getivar_("hicolor", &color, error, 7);

    xaxis = window_points_.xaxis;
    yaxis = window_points_.yaxis;
    i=0;
    npts=1;
    xt = (float)atof(argv[1]);
    yt = (float)atof(argv[2]);
    if(argc==3)
      type=4;
    else
      switch(argv[3][0]){
      case 'd':
      case 'D': type=1;
	break;
      case 'p':
      case 'P': type=2;
	break;
      case 's':
      case 'S': type=3;
	break;
      case 'c':
      case 'C': type=4;
	break;
      case 'x':
      case 'X': type=5;
	break;
      default:
	type=1;
      }
    if(argc>4)
      scale= scale*(float)atof(argv[4]);

    (void)setlvar_("savflg", &yes, error, 6);
    (void)makseg_(&yes);

    if(!(defmap || exact_fit)){
      (void)maptrn_(&yt,&xt,&x,&y);
	}
	else{
      (void)lonlat_2_phys_trans_(&x,&y,&xt,&yt,&npts);
	}
    (void)phys_2_index_trans_(&x,&ireal,&npts,&npts,&npts);
    (void)phys_2_index_trans_(&y,&jreal,&npts,&npts,&npts);
    printf("I: %6.2f, J: %6.2f\n",ireal,jreal);

    gsetmarkersize(scale);
    gsetmarkertype(type);
    gsetmarkercolourind(color);
    pt[0].x=x;
    pt[0].y=y;
    gpolymarker(1,pt);
    (void)gcloseseg();
}

