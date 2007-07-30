static char rcsid[] = "$Id: fixdata.c,v 1.1 2000/12/06 22:47:58 warren Exp $";
/* Purpose
 * To catch and fix NaNs once and for all
 *
 * $Log: fixdata.c,v $
 * Revision 1.1  2000/12/06 22:47:58  warren
 * Fix fpe data once and for all
 *
 */

#include <math.h>
#ifdef SOLARIS
#include <ieeefp.h>
#endif

void ive_fixdata_(data, missing, len)
     float *data, *missing; 
     int *len;
{
  int i;
  for(i=0; i<*len; i++){
    if(isnanf(*(data+i))){
      printf ("fixing one at %d:%f\n",i,data[i]);
      *(data+i) = *missing;
      printf ("fixed one at %d\n",i);
    }
  }
}
