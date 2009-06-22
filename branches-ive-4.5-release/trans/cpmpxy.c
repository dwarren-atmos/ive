static char *rcsid = "$Id: cpmpxy.c,v 1.7 1999/12/22 00:33:34 warren Exp $";

#include <math.h>
#include <stdio.h>

extern void make_help_widget_(),phys_2_lonlat_trans_(),maptrn_();
#define ABS(x)    ((x < 0)? -x:x)
#define MAX(x, y) ((x > y)? x:y) 
#define MIN(x, y) ((x < y)? x:y) 
#define EPSLON  .00001
#define MAXLAT  89.0
#define ORVAL  1.0e12
#define SMALLEST_FLOAT 1.0e-28

void cpmpxy_(imap, xinp, yinp, xotp, yotp)
     /*-----------------------------------------------------------------------
      *     $Id: cpmpxy.c,v 1.7 1999/12/22 00:33:34 warren Exp $
      *     Purpose:
      *     This routine is used by the analysis package to properly map
      *     contour plots.
      *     Arguments:
      *        imap  int  input  flag indicating which coordinate transform
      *                          will be applied.
      *                          imap = 1  mapping for horizontal slabs 
      *                                    overlain on map projections.
      *                          imap = 2  Same as 4 except no windowing.
      *                          imap = 3  mapping for vertical slabs(not used)
      *                          imap = 4  standard mapping
      *                          imap = 5  just return the values (clipping)
      *						(not used)
      *     If imap = 0, return info on mapping in xinp.
      *     If imap < 0 => inverse transform.
      *
      *     History:
      *     $Log: cpmpxy.c,v $
      *     Revision 1.7  1999/12/22 00:33:34  warren
      *     Fixed Dales crashing bug when changing from point slicer to normal. Also
      *     have some ifdef linuxes in there in case we ever find a fortran compiler.
      *
 * Revision 1.6  1996/02/12  23:51:57  harry
 * Fix to repond properly to imap zero.
 *
      *     Revision 1.5  1996/01/10 17:08:23  harry
      *     Add KLUDGE for color fill in stereographic projections.
      *
 * Revision 1.4  1995/09/05  20:31:51  harry
 * Modifications to transforms for new slicer.
 *
 * Revision 1.3  1993/08/18  15:48:17  harry
 * Change to new transform routines.
 *
 * Revision 1.2  1993/08/05  17:59:44  warren
 * Christoph's map stuff
 *
 * Revision 1.1.1.1  1993/04/07  19:41:26  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.2  1992/10/20  18:07:58  millerp
 * Fixed possible division by zero error in mapfac.
 *
 * Revision 1.1  1992/09/08  21:51:17  millerp
 * Initial revision
 *
      * Revision 1.1  1992/04/21  00:23:37  millerp
      * Initial revision
      *
      *-----------------------------------------------------------------------
      *     
      *     
      *     Argument declarations.
      */     
     int   *imap;
     float *xinp, *yinp, *xotp, *yotp;
{
    struct point {
	float x,y;
    } *phtmp, *phtmp1;
    struct point2{
	struct point **pt;
	int numx;
	int numy;
    }extern phpts; 

    struct minmax {
	float minx;
	float maxx;
	float miny;
	float maxy;
    } extern fixmap; /* used only for horizontal plots on a map */

    extern struct {
	float x1; 
	float x2;
	float y1;
	float y2;
	int   imin, imax, jmin, jmax;
	int   xaxis;
	int   yaxis;
	int   window_axes;
	int   reverse;
    } window_points_;

    float dx, dy, dxx, dyy, aa, ab, ac, ad, lon, lat, x, y;
    int ixinp, iyinp, iyinp1;
    char dummy[80];
    static int one=1;

    if (*imap == 0) {
	if (*xinp == 1 || *xinp == 2 || *xinp == 4) *yinp = 1;
	else *yinp = 0;
	return;
    }
    /*
       Forward transforms.
       */

    /*
     * find points
     */

    if (window_points_.reverse) {
	x = *yinp;
	y = *xinp;
    }
    else {
	x = *xinp;
	y = *yinp;
    }
    ixinp = MAX((int)x, 1);
    iyinp = MAX((int)y, 1);
    dx = x - (float)ixinp;
    dy = y - (float)iyinp;
    dxx = .25 *(dx - 1.);
    dyy = .25 *(dy - 1.);
    ixinp += window_points_.imin-1;
    iyinp += window_points_.jmin-1;

    if(ixinp >phpts.numx - 1){
	sprintf(dummy,"numx = %d , ixinp = %d",phpts.numx, ixinp);
	(void)make_help_widget_(dummy);
	*xotp = 0;
	*yotp = 0;
	return;
    }
    phtmp = phpts.pt[ixinp];

    if(ixinp<phpts.numx - 1)
      phtmp1 = phpts.pt[ixinp + 1];
    else
      phtmp1 = phtmp;

    if(iyinp<phpts.numy -1)
      iyinp1 = iyinp + 1;
    else
      iyinp1 =iyinp ;

    /*
     * Do 4 pt bessel
     */

      x = (1.-dy) * ((1.-dx) * phtmp[iyinp].x + dx * 
	      phtmp1[iyinp].x) + dy * ((1.-dx) * phtmp[iyinp1].x + 
              dx * phtmp1[iyinp1].x);
     
      y = (1.-dy) * ((1.-dx) * phtmp[iyinp].y + dx * 
	      phtmp1[iyinp].y) + dy * ((1.-dx) * phtmp[iyinp1].y + 
              dx * phtmp1[iyinp1].y);
     
     /*
      * Do 16 pt bessel
      */
/*
      aa = xval(ixinp,iyinp-1) + dx *(xval(ixinp+1,iyinp-1) - 
	   xval(ixinp,iyinp-1) + dxx *(xval(ixinp+2,iyinp-1) - 
	   xval(ixinp+1,iyinp-1) + xval(ixinp-1,iyinp-1) - 
	   xval(ixinp,iyinp-1)));

      ab = xval(ixinp,iyinp) + dx*(xval(ixinp+1,iyinp) - xval(ixinp,iyinp) + 
	   dxx *(xval(ixinp+2,iyinp) - xval(ixinp+1,iyinp) + 
           xval(ixinp-1,iyinp) - xval(ixinp,iyinp)));

      ac = xval(ixinp,iyinp+1) + dx *(xval(ixinp+1,iyinp+1) - 
           xval(ixinp,iyinp+1) + dxx *(xval(ixinp+2,iyinp+1) - 
           xval(ixinp+1,iyinp+1) + xval(ixinp-1,iyinp+1) - 
           xval(ixinp,iyinp+1)));

      ad = xval(ixinp,iyinp+2) + dx *(xval(ixinp+1,iyinp+2) -
           xval(ixinp,iyinp+2) + dxx *(xval(ixinp+2,iyinp+2) - 
           xval(ixinp+1,iyinp+2) + xval(ixinp-1,iyinp+2) - 
           xval(ixinp,iyinp+2)));

      x = ab + dy *(ac - ab + dyy *(ad - ac + aa - ab));

      aa = yval(ixinp,iyinp-1) + dx *(yval(ixinp+1,iyinp-1) - 
	   yval(ixinp,iyinp-1) + dxx *(yval(ixinp+2,iyinp-1) - 
	   yval(ixinp+1,iyinp-1) + yval(ixinp-1,iyinp-1) - 
	   yval(ixinp,iyinp-1)));

      ab = yval(ixinp,iyinp) + dx*(yval(ixinp+1,iyinp) - yval(ixinp,iyinp) + 
	   dxx *(yval(ixinp+2,iyinp) - yval(ixinp+1,iyinp) + 
           yval(ixinp-1,iyinp) - yval(ixinp,iyinp)));

      ac = yval(ixinp,iyinp+1) + dx *(yval(ixinp+1,iyinp+1) - 
           yval(ixinp,iyinp+1) + dxx *(yval(ixinp+2,iyinp+1) - 
           yval(ixinp+1,iyinp+1) + yval(ixinp-1,iyinp+1) - 
           yval(ixinp,iyinp+1)));

      ad = yval(ixinp,iyinp+2) + dx *(yval(ixinp+1,iyinp+2) -
           yval(ixinp,iyinp+2) + dxx *(yval(ixinp+2,iyinp+2) - 
           yval(ixinp+1,iyinp+2) + yval(ixinp-1,iyinp+2) - 
           yval(ixinp,iyinp+2)));

      y = ab + dy *(ac - ab + dyy *(ad - ac + aa - ab));
*/
    if (window_points_.reverse) {
	*xotp = y;
	*yotp = x;
    }
    else {
	*xotp = x;
	*yotp = y;
    }
    if ( *imap == 2) return;

    if (window_points_.x1 < window_points_.x2) {
	if (*xotp < window_points_.x1 || *xotp > window_points_.x2) {
	    *xotp = ORVAL;
	    return;
	}
    }
    else {
	if (*xotp > window_points_.x1 || *xotp < window_points_.x2) {
	    *xotp = ORVAL;
	    return;
	}
    }
    if (window_points_.y1 < window_points_.y2) {
	if (*yotp < window_points_.y1 || *yotp > window_points_.y2) {
	    *xotp = ORVAL;
	    return;
	}
    }
    else {
	if (*yotp > window_points_.y1 || *yotp < window_points_.y2) {
	    *xotp = ORVAL;
	    return;
	}
    }

	
    if ( *imap == 1 ){
	/*     
	 *Mapping for horizontal slabs overlayed on map projections.
	 *We must convert from array indicies to longitude and
	 *latitude.
	 */
	if(*xotp < fixmap.minx || *xotp > fixmap.maxx ||
	   *yotp < fixmap.miny || *yotp > fixmap.maxy){
	    *xotp = ORVAL;
	    return;
	}
	lon = *xotp;
	lat = *yotp;
/* phys_2_lonlat is called to convert to lon-lat,
   if phys_2_lonlat is not present -> identity */

        (void)phys_2_lonlat_trans_(&lon,&lat,xotp,yotp,&one);
	lon = *xotp;
/*
   This is a kludge to permit full hemispheric color filled Stereographic
   plots.
   */
	lat = *yotp;
	(void)maptrn_(&lat, &lon, xotp, yotp);
    }
}

void mapfac_ (xx, yy, uu, vv, sfx, sfy, mx, my, imap, mxf, myf,length,uc)
     /*-----------------------------------------------------------------------
      *     Purpose:
      *        This subroutine replaces the functions mxf and myf found in 
      *        the NCAR Graphics routine velvct. It is needed to properly
      *        display vectors on map projections. Two function calls are 
      *        replaced with one subroutine call to improve efficiency.
      *     History:
      *-----------------------------------------------------------------------
      *
      *
      *     Argument declarations.
      */
     int    *mx, *my, *imap, *mxf, *myf, *uc;
     float  *xx, *yy, *uu, *vv, *sfx, *sfy, *length;
{
    float lon, lat, cfct, fact1, fact2, u, v, x1, x2, y1, y2, new_epslon;
    static int one=1;
    
    if ( *imap == 1 ){
	
	/*
	 *Convert the array indices (xx, yy) to (longitude, latitude).
	 */

	*imap = 2;
	cpmpxy_ (imap, xx, yy, &lon, &lat);
	*imap = 1;

	/*
	 *Check for points at the poles.
	 */
	if ( ABS(lat) >= MAXLAT ) {
            *mxf = 0;
	    *myf = 0;
	    return;
	}
	
	cfct = (float)acos ( (acos(-1.0) / 180.0) * (double)lat);
	
	/*
	 * Find the coordinates of (lon, lat) in the map projection.
	 */
	(void)phys_2_lonlat_trans_(&lon,&lat,&x1,&y1,&one);
	/*x1 = lon, y1 = lat*/
	(void)maptrn_(&y1, &x1, &x1, &y1);
	if ( (x1 == ORVAL) || (y1 == ORVAL) ){
	    printf("mapfac: %f %f is out of bounds\n", lon, lat);
	    *mxf = 0;
	    *myf = 0;
            return;
	}
	
	/*
	 *Find the coordinates of a particle originally at 
	 *(lon, lat) after a short period of time. 
	 */

	/*EPSLON is dependent on the length of the vector, set it
	* conveniently: */
	new_epslon=EPSLON * (100.0/ *length);
/* ph2ll inserted by David Bresch, uc=0 means use ph2ll for vectors */
	if (*uc == 0 ) {
/* add increment in the ph-plane: */
	    lat = lat + new_epslon * *vv;
	    lon = lon + new_epslon * *uu / cfct;
	    (void)phys_2_lonlat_trans_(&lon,&lat,&x2,&y2,&one);
	}
	else {
/* add increment in the lat/lon-plane: */
	    (void)phys_2_lonlat_trans_(&lat,&lon,&y2,&x2,&one);
	    y2 = y2 + new_epslon * *vv;
	    x2 = x2 + new_epslon * *uu / cfct;
	}
	(void)maptrn_ (&y2, &x2, &x2, &y2);

	/*
	 *Find the velocity wrt the map projection.
	 */
	
	fact1 = 
	  (float)sqrt((double)((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1)) );
	if ( ABS(fact1) <= SMALLEST_FLOAT ){
	    /* Avoid division by 0 below. In this case the velocity
	       components are so small that the vector magnitude will be
	       nearly 0.
	     */
            *mxf = 0;
	    *myf = 0;
	    return;
	}
	    
	fact2 = (float)sqrt((double)(*uu * *uu + *vv * *vv));
	u = ( (x2 - x1) / fact1 ) * fact2;
	v = ( (y2 - y1) / fact1 ) * fact2;
	*mxf = *mx + (int)(*sfx * u);
	*myf = *my + (int)(*sfy * v);
    }
    else{
	*mxf = *mx + (int)(*sfx * *uu);
	*myf = *my + (int)(*sfy * *vv);
    } 
}
