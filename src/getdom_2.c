#include <math.h>

#define MAX(a,b) ((a)>(b) ? (a):(b))
#define MIN(a,b) ((a)>(b) ? (b):(a))
#define ABS(x)  ( (x) > 0. ? (x) : -(x))

     extern struct {
	 int cpmin1;
	 int cpmin2;
	 int cpmin3;
	 int cpmin4;
	 int cpmax1;
	 int cpmax2;
	 int cpmax3;
	int cpmax4;
     }  cpdims_;

/* $Id: getdom_2.c,v 1.7 1994/01/28 23:11:22 harry Exp $
 * Purpose: Special case for Dale's terrain. Addition for getdom.f
 * This routine calculates the maximum and minimum z index value for 
 * x/z and y/z slices.
 *
 *   $Log: getdom_2.c,v $
 *   Revision 1.7  1994/01/28 23:11:22  harry
 *   Correct calculation of maximum index value.
 *
 * Revision 1.6  1993/12/29  23:32:18  yuri
 * Updated for the 3D->2D average case.
 *
 * Revision 1.5  1993/11/29  19:39:21  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.4  1993/10/29  19:09:49  yuri
 * Correction of LOG lines.
 *
 * Revision 1.3  1993/10/29  18:50:33  yuri
 * Just added LOg and Id lines.
 * *
 *  New/old getdom_2.
 */

extern void getiarr_(),getrarr_(),getrvar_(),phys_2_index_trans_();

void getdom_2_(iwi, jwi, kwi, nxw, nyw, nzw)
     int *iwi, *jwi, *kwi, *nxw, *nyw, *nzw;
{
    float plwmin[4], plwmax[4], real_min[4], real_max[4], d1, d2, phtmp[4];
    int iflag[4],  dims = 4,  i, j, local_max, one = 1;
    int error = 0;
    float cmptmp[4], tloc;
    int lock[4];

    iflag[0] = iflag[1] = iflag[2] = 1; iflag[3] = 0; 

    (void)getiarr_("lock", lock, &dims, &error, 4);
    (void)getrarr_("plwmin", plwmin, &dims, &error, 6, 4);
    (void)getrarr_("plwmax", plwmax, &dims, &error, 6, 4);
    for (i = 0; i < dims; i++){
	real_max[i] = (float)MAX(plwmax[i], plwmin[i]);
	real_min[i] = (float)MIN(plwmax[i], plwmin[i]);
    }
#ifdef DEBUG
    printf(" max= %d %d %d %d\n",cpdims_.cpmax1,cpdims_.cpmax2,
	   cpdims_.cpmax3,cpdims_.cpmax4);
#endif

    /* t is always locked (?) here */
    (void) getrvar_ ("tloc", &tloc, &error,4);
    phtmp[3] = tloc;
 
    /* min for xz case*/
    if( (lock[1] == 1)  ||  (lock[1] == 2))
      {
	  phtmp[1] = plwmin[1];
	  d1 = (real_max[0] - real_min[0]) / (float)  (cpdims_.cpmax1 - 1);
	  
	  phtmp[2] = plwmin[2];
	
	  phtmp[0] = real_min[0]; 
	  (void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
	  local_max = floor(cmptmp[2]);
	  for (i = 1; i <  cpdims_.cpmax1; i++)
	    {
		phtmp[0] = real_min[0] + d1 *  i; 
		(void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
		local_max = MIN(local_max,  floor(cmptmp[2]));
	    }
	  *kwi = MAX(local_max,1);
	  
	  phtmp[2] = plwmax[2];
	  local_max = cpdims_.cpmax3;
	  for (i = 0; i <  cpdims_.cpmax1; i++)
	    {
		phtmp[0] = real_min[0] + d1 * i; 
		(void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
		local_max = MAX(local_max,  ceil(cmptmp[2]));
	    }
	  *nzw = MIN(local_max, cpdims_.cpmax3) - *kwi + 1;
      }		     /* end of  xz case*/
 
 
    /* min for yz case*/
    else if( (lock[0] == 1)  ||  (lock[0] == 2))
      {

	  phtmp[0] = plwmin[0];
	  d1 = (real_max[1] - real_min[1]) / (float)  (cpdims_.cpmax2 - 1);
	  
	  phtmp[2] = plwmin[2];
	
	  phtmp[1] = real_min[1] ; 
	  (void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
	  local_max = floor(cmptmp[2]);
	  for (i = 1; i <  cpdims_.cpmax2; i++)
	    {
		phtmp[1] = real_min[1] + d1 *  i; 
		(void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
		local_max = MIN(local_max,  floor(cmptmp[2]));
	    }
	  *kwi = MAX(local_max,1);
	  
	  phtmp[2] = plwmax[2];
	  phtmp[1] = real_min[1] ; 
	  (void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
	  local_max = floor(cmptmp[2]);
	  for (i = 1; i <  cpdims_.cpmax2; i++)
	    {
		phtmp[1] = real_min[1] + d1 * i; 
		(void) phys_2_index_trans_(phtmp, cmptmp, iflag, &dims, &one);
		local_max = MAX(local_max,  ceil(cmptmp[2]));
	    }
	  *nzw = MIN(local_max, cpdims_.cpmax3) - *kwi + 1;
      }		     /* end of  xz case*/
 
#ifdef DEBUG
    printf(" result= %d %d %d %d %d %d\n", *iwi, *jwi, *kwi, *nxw, *nyw, *nzw);
#endif
}


