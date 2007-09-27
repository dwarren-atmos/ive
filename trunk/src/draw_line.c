static char rcsid[] = "$Id: draw_line.c,v 1.8 2007-05-01 17:57:10 warren Exp $";
/* $Id: draw_line.c,v 1.8 2007-05-01 17:57:10 warren Exp $
   $Log: draw_line.c,v $
   Revision 1.8  2007-05-01 17:57:10  warren
   fixes for mouse slicing and line drawing on default map projections.

   Revision 1.7  2006/08/22 21:31:29  warren
   added draw_line_map command for entering line end points in long,lat

   Revision 1.6  2006/08/22 21:10:40  warren
   First crack on draw_line for maps. Enter in same coordinate system as on
   window sliders.

   Revision 1.5  2001/03/13 21:36:21  davidive
   Fixed includes for sun and memory debugging

   Revision 1.4  2000/05/09 19:53:45  harry
   Seperate contour and line colors.  Make patterns work like colors.

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

extern void getavar_(),getdarr_(),getivar_(),getlvar_(),unscale_(),setlvar_(),makseg_(),
  phys_2_lonlat_trans_(),maptrn_(),drlineplt_(),add_to_list_(),lonlat_2_phys_trans_(),
  make_help_widget_(),gcloseseg();
void draw_line_(error)
    int *error;
{
    char *ptr, **argv, **get_args(), cmdstr[768];
    float *x, *y, *xt,*yt;
    int npts, i, argc, xaxis, yaxis, mapflg,defmap, exact_fit, ax[2], use_buttons, no=0, yes=1 ;
    double domain_intercept[4], domain_slope[4];
    float x1w, x2w, y1w, y2w; /*world coord*/
    float x3w, x4w, y3w, y4w; /*extras for 2 dimention line*/
    float point1[3], point2[3], point3[3];
    float spval=0.0;
    int minus_one=-1;
            
    argv = get_args(&argc);
    
    if(argc < 5 || (argc-1)%2 != 0){
	(void)make_help_widget_("draw_line requires at least 2 points \
consisting of xy pairs:\n draw_line=x1, y1, x2, yx ....");
	*error=1;
        return;
    }
    

    x = (float *)malloc((sizeof(float))*((argc-1)/2));
    y = (float *)malloc((sizeof(float))*((argc-1)/2));

    i=4;
    (void)getdarr_("domain_slope", domain_slope, &i, error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, error,16);
    (void)getivar_("use_buttons", &use_buttons, error, 11);
    (void)getlvar_("mapflg",&mapflg,error,6);
    (void)getlvar_("defmap",&defmap,error,6);
    (void)getlvar_("exact_fit",&exact_fit,error,9);

        xaxis = window_points_.xaxis;
        yaxis = window_points_.yaxis;
    i=0;
    npts=1;
    while (npts<argc){
	    x[i] = (float)atof(argv[npts]);
	    npts++;
	    y[i] = (float)atof(argv[npts]);
	    npts++;
	    i++;
    }
    npts = (argc-1)/2;
    if(xaxis != 0)
	(void)unscale_(x,&npts, domain_slope +(xaxis-1), 
		       domain_intercept + (xaxis-1), &spval);
    if(yaxis != 0)
	(void)unscale_(y,&npts, domain_slope +(yaxis-1), 
		       domain_intercept +(yaxis-1), &spval);
    (void)setlvar_("savflg", &yes, error, 6);
    (void)makseg_(&yes);
/*    (void)curved_(x, y, &npts); */

    if(mapflg){
      xt = (float *)malloc((sizeof(float))*((argc-1)/2));
      yt = (float *)malloc((sizeof(float))*((argc-1)/2));
      (void)phys_2_lonlat_trans_(x,y,xt,yt,&npts);

	  printf("draw_line_map=%f,%f,%f,%f\n",xt[0],yt[0],xt[1],yt[1]);
	}

    if(mapflg & !(defmap)){

/*      xt = (float *)malloc((sizeof(float))*((argc-1)/2));
      yt = (float *)malloc((sizeof(float))*((argc-1)/2));
      (void)phys_2_lonlat_trans_(x,y,xt,yt,&npts);
*/
      for(i=0; i<npts;i++){
		(void)maptrn_(yt+i,xt+i,x+i,y+i);
      }
    }
    drlineplt_(x, y, &npts, &yes, error, &minus_one, &minus_one);
    if (!error) {
	getavar_("cmdstr", cmdstr, error, 6, 768);
	add_to_list_(cmdstr, 768);
    }
    (void)gcloseseg();
}

void draw_line_map_(error)
    int *error;
{
    char *ptr, **argv, **get_args(), cmdstr[768];
    float *x, *y, *xt,*yt;
    int npts, i, argc, xaxis, yaxis, mapflg, ax[2], use_buttons, no=0, yes=1 ;
	int defmap, exact_fit;
    double domain_intercept[4], domain_slope[4];
    float x1w, x2w, y1w, y2w; /*world coord*/
    float x3w, x4w, y3w, y4w; /*extras for 2 dimention line*/
    float point1[3], point2[3], point3[3];
    float spval=0.0;
    int minus_one=-1;
            
    argv = get_args(&argc);
    
    if(argc < 5 || (argc-1)%2 != 0){
	(void)make_help_widget_("draw_line requires at least 2 points \
consisting of xy pairs:\n draw_line=x1, y1, x2, yx ....");
	*error=1;
        return;
    }
    

    (void)getlvar_("mapflg",&mapflg,error,6);
    (void)getlvar_("defmap",&defmap,error,6);
    (void)getlvar_("exact_fit",&exact_fit,error,9);

    if(!mapflg){
      (void)make_help_widget_("Not plotting on a map - usr draw_line instead");
      *error=1;
      return;
    }

    x = (float *)malloc((sizeof(float))*((argc-1)/2));
    y = (float *)malloc((sizeof(float))*((argc-1)/2));
    xt = (float *)malloc((sizeof(float))*((argc-1)/2));
    yt = (float *)malloc((sizeof(float))*((argc-1)/2));

    i=4;
    (void)getdarr_("domain_slope", domain_slope, &i, error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, error,16);
    (void)getivar_("use_buttons", &use_buttons, error, 11);

        xaxis = window_points_.xaxis;
        yaxis = window_points_.yaxis;

    i=0;
    npts=1;
    while (npts<argc){
	    xt[i] = (float)atof(argv[npts]);
	    npts++;
	    yt[i] = (float)atof(argv[npts]);
	    npts++;
	    i++;
    }

    npts = (argc-1)/2;
    if(!(defmap)){
      for(i=0; i<npts;i++){
		(void)maptrn_(yt+i,xt+i,x+i,y+i);
	  }
	}
    else{
      (void)lonlat_2_phys_trans_(x,y,xt,yt,&npts);
	}


    (void)setlvar_("savflg", &yes, error, 6);
    (void)makseg_(&yes);
    (void)drlineplt_(x, y, &npts, &yes, error, &minus_one, &minus_one);
    if (!error) {
	getavar_("cmdstr", cmdstr, error, 6, 768);
	add_to_list_(cmdstr, 768);
    }
    (void)gcloseseg();
}


