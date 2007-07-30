#include <float.h>
#include <stdlib.h>
#include <math.h>


double average (slab, numx, special)
     float *slab, *special;
     int *numx;
/***********************************************************************
  This function computes the average of all values in slab.  It ignores
  any values in slab == special, if special != 0.
***********************************************************************/
{
    float sum;
    int i, npts;
    
    sum = 0.0;
    if (*special)
      for (i = 0, npts = 0; i < *numx; i++) {
	  if (slab[i] != *special) {
	      sum += slab[i];
	      npts++;
	  }
      }
    else 
      for (npts = 0; npts < *numx; npts++)
	sum += slab[npts];
    
    if (npts != 0) 
      return ( (double)(sum/(float)npts) );
    else
      return ( (double)*special );
    
}    

double epsilon_ (value)
     float *value;
/***********************************************************************
  This function returns the smallest number epsilon such that 
         value + epsilon != value.
  This is a fortran-callable routine.
***********************************************************************/
{
    return ( (double)(*value * FLT_EPSILON) );
}


float find_min(slab, numx, special)
     float *slab, special;
     int numx;
/***********************************************************************
  This function returns the minimum value in the array slab. It ignores
  any values in slab == special, if special != 0.
***********************************************************************/
{
    int i;
    float val;
    val = FLT_MAX;
    if (special)
	for(i=0; i<numx; i++) 
	  val = (slab[i]<val && slab[i] != special) ? slab[i] : val;
    else
	for(i=0; i<numx; i++) 
	  val = (slab[i]<val) ? slab[i] : val;
    
    return(val);
}


float find_max(slab, numx, special)
     float *slab, special;
     int numx;
/***********************************************************************
  This function returns the maximum value in the array slab. It ignores
  any values in slab == special, if special != 0.
***********************************************************************/
{
    int i;
    float val;
    val = -FLT_MAX;
    if (special)
      for(i=0; i<numx; i++)
	val = (slab[i]>val && slab[i] != special) ? slab[i] : val;
    else
      for(i=0; i<numx; i++) 
	val = (slab[i]>val) ? slab[i] : val;
    
    return(val);
}

/**********************************************************************
  FORTRAN VERSIONS
**********************************************************************/
double find_min_(slab, numx, special)
     float *slab, *special;
     int *numx;
{
    return((double)find_min(slab,*numx,*special));
}

double find_max_(slab, numx, special)
     float *slab, *special;
     int *numx;
{
    return((double)find_max(slab,*numx,*special));
}

