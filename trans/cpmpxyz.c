#include <math.h>
#include <stdio.h>
#include <ive_macros.h>
#include <missing.h>
#include <window.h>

extern void make_help_widget_(),phys_2_lonlat_trans_(),maptrn_();
#define ABS(x)    ((x < 0)? -x:x)
#ifndef MAX
#define MAX(x, y) ((x > y)? x:y) 
#define MIN(x, y) ((x < y)? x:y) 
#endif
#define EPSLON  .00001
#define MAXLAT  89.0
#define ORVAL  1.0e12
#define SMALLEST_FLOAT 1.0e-28
static struct point {
    float x,y,z;
}*phtmp1, *phtmp2, *phtmp3, *phtmp4, *phtmp5, *phtmp6, *phtmp7, *phtmp8;
#define p3(x,y,z) (phpts3.pt + (x) +(((y) + ((z)+phpts3.numy))*phpts3.numx))
struct point3{
    struct point *pt;
    int numx;
    int numy;
    int numz;
} ;
extern struct point3 phpts3;
extern struct wpt3 window_points3_;

void cpmpxyz_(imap, xinp, yinp, zinp, xotp, yotp, zotp)
/*-----------------------------------------------------------------------
  
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
 */
     
     int   *imap;
     float *xinp, *yinp, *zinp, *xotp, *yotp, *zotp;
{
  float dx, dy, dz, lon, lat, x, y, z, a, b, c, d;
  int ixinp, ixinp1, iyinp, iyinp1, izinp, izinp1;
  char dummy[80];

    if (*imap == 0) {
	if (*xinp == 1 || *xinp == 2 || *xinp == 4) *yinp = 1;
	else *yinp = 0;
	return;
    }
    /*
       Forward transforms.
       */
    //Don't wory about reverse - doesn't really make sense in 3d and should alwasy be off
    x = *xinp;
    y = *yinp;
    z = *zinp;
    ixinp = MAX((int)x, 1);
    iyinp = MAX((int)y, 1);
    izinp = MAX((int)z, 1);
    dx = x - (float)ixinp;
    dy = y - (float)iyinp;
    dz = z - (float)izinp;
    ixinp += window_points3_.imin-1;
    iyinp += window_points3_.jmin-1;
    izinp += window_points3_.kmin-1;
    if(ixinp<phpts3.numx - 1)ixinp1 = ixinp+1;
    else ixinp1=ixinp;
    if(iyinp<phpts3.numy - 1)iyinp1 = iyinp+1;
    else iyinp1=iyinp;
    if(izinp<phpts3.numz - 1)izinp1 = izinp+1;
    else izinp1=izinp;

    if(ixinp >phpts3.numx - 1){
      sprintf(dummy,"numx = %d , ixinp = %d",phpts3.numx, ixinp);
      (void)make_help_widget_(dummy);
      *xotp = 0;
      *yotp = 0;
      *zotp = 0;
      return;
    }
    phtmp1 = p3(ixinp,iyinp,izinp);
    phtmp2 = p3(ixinp1,iyinp,izinp);
    phtmp3 = p3(ixinp1,iyinp1,izinp);
    phtmp4 = p3(ixinp,iyinp1,izinp);
    phtmp5 = p3(ixinp,iyinp,izinp1);
    phtmp6 = p3(ixinp1,iyinp,izinp1);
    phtmp7 = p3(ixinp1,iyinp1,izinp1);
    phtmp8 = p3(ixinp,iyinp1,izinp1);

    /*
     8-----7
    /\     /\
   /  4---/--\
   5-----6    3
   \  /   \  /
    \/     \/
     1-----2
    */
    //Do weighted average of 2 4pt bessels???

    //lets try linear for fun
    /*
    a=(dx*phtmp1->x)+((1-dx)*phtmp2->x);
    b=(dx*phtmp5->x)+((1-dx)*phtmp6->x);
    c=(dx*phtmp8->x)+((1-dx)*phtmp7->x);
    d=(dx*phtmp4->x)+((1-dx)*phtmp3->x);
    x=(a+b+c+d)/4.;


    a=(dy*phtmp1->y)+((1-dy)*phtmp4->y);
    b=(dy*phtmp5->y)+((1-dy)*phtmp8->y);
    c=(dy*phtmp6->y)+((1-dy)*phtmp7->y);
    d=(dy*phtmp2->y)+((1-dy)*phtmp3->y);
    y=(a+b+c+d)/4.;

    a=(dz*phtmp1->z)+((1-dz)*phtmp5->z);
    b=(dz*phtmp2->z)+((1-dz)*phtmp6->z);
    c=(dz*phtmp3->z)+((1-dz)*phtmp7->z);
    d=(dz*phtmp4->z)+((1-dz)*phtmp8->z);
    z=(a+b+c+d)/4.;
    */
    
    a = (1.-dz) * ((1.-dy) * phtmp1->x + dy*
		   phtmp4->x + dz *((1.-dy)*phtmp5->x)+
		   dy*phtmp8->x);
    b = (1.-dz) * ((1.-dy) * phtmp2->x + dy*
		   phtmp3->x + dz *((1.-dy)*phtmp6->x)+
		   dy*phtmp7->x);

    x = dx*a+(1-dx)*b;


    a = (1.-dz) * ((1.-dy) * phtmp1->y + dy*
		   phtmp4->y + dz *((1.-dy)*phtmp5->y)+
		   dy*phtmp8->y);
    b = (1.-dz) * ((1.-dy) * phtmp2->y + dy*
		   phtmp3->y + dz *((1.-dy)*phtmp6->y)+
		   dy*phtmp7->y);

    y = dx*a+(1-dx)*b;


    a = (1.-dz) * ((1.-dy) * phtmp1->z + dy*
		   phtmp4->z + dz *((1.-dy)*phtmp5->z)+
		   dy*phtmp8->z);
    b = (1.-dz) * ((1.-dy) * phtmp2->z + dy*
		   phtmp3->z + dz *((1.-dy)*phtmp6->z)+
		   dy*phtmp7->z);

    z = dx*a+(1-dx)*b;


    
    

    *xotp=x;
    *yotp=y;
    *zotp=z;

    if ( *imap == 2) return;
    
    if (window_points3_.x1 < window_points3_.x2) {
      if (*xotp < window_points3_.x1 || *xotp > window_points3_.x2) {
	*xotp = ORVAL;
	return;
      }
    }
    else {
      if (*xotp > window_points3_.x1 || *xotp < window_points3_.x2) {
	*xotp = ORVAL;
	return;
      }
    }
    if (window_points3_.y1 < window_points3_.y2) {
      if (*yotp < window_points3_.y1 || *yotp > window_points3_.y2) {
	*xotp = ORVAL;
	return;
      }
    }
    else {
      if (*yotp > window_points3_.y1 || *yotp < window_points3_.y2) {
            *xotp = ORVAL;
	    return;
	}
    }
    if (window_points3_.z1 < window_points3_.z2) {
      if (*zotp < window_points3_.z1 || *zotp > window_points3_.z2) {
	*xotp = ORVAL;
	return;
      }
    }
    else {
      if (*zotp > window_points3_.z1 || *zotp < window_points3_.z2) {
	*xotp = ORVAL;
	return;
      }
    }
  
}

     
