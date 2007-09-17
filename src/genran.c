/*
-----------------------------------------------------------------------
   $Id: genran.c,v 1.2 1998/12/17 23:17:19 warren Exp $
   Purpose:
      This routine generates a random number whose value is less than 
      limit.
   History:
   $Log: genran.c,v $
   Revision 1.2  1998/12/17 23:17:19  warren
   Cleanup of pointer casts to long instead of int, freeing XmStrings when no
   longer in use, passing all args to sprinf's and making correctly casting NULLs

 * Revision 1.1.1.1  1993/04/07  19:40:41  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.0  1992/07/01  21:16:44  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.3  1992/05/01  23:05:27  millerp
 * Changed to use LONG_MAX.
 *
 * Revision 1.2  1992/05/01  17:36:12  millerp
 * Made MAX_RAND a define statement to avoid floating-point exception.
 *
 * Revision 1.1  1992/04/21  00:23:37  millerp
 * Initial revision
 *
-----------------------------------------------------------------------
*/
#include <time.h> 
#include <math.h> 
#include <limits.h>
#include <stdlib.h>

int myiverandomnum;
#ifdef linux
extern void srandom (unsigned int __seed) __THROW;
extern long int random (void) __THROW;
#endif

int *genran_ (limit)
     int *limit;
{
    long divisor ;
    time_t  now ;
    unsigned long t;
    

    t = (unsigned long)time(&now);
    srandom (t) ;
    divisor = LONG_MAX / (long) *limit ;
    myiverandomnum = (int)(random() / divisor);
    return (&myiverandomnum) ;

}

