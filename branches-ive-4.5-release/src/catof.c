/*
-----------------------------------------------------------------------
   $Id: catof.c,v 1.5 1993/10/27 16:26:27 warren Exp $
   Purpose:
   
This function is called by a fortran routine. It returns the
         floating point value of the input character string.
   Arguments:
      instr  char  input  character string to be converted from
                             character to floating point.
   History:
   $Log: catof.c,v $
   Revision 1.5  1993/10/27 16:26:27  warren
   For stupid SUN compiler. Their treatment of floats between C and FORTRAN is
   almost as bad as system V printing.

 * Revision 1.4  1993/10/27  16:21:38  warren
 * declared missing ptr and moved the giant comment block above the function
 * declaration so you can see it and the variables at the same time.
 *
 * Revision 1.3  1993/10/26  23:19:44  warren
 * put in missing argument. (shouldn't comment C like fortran)
 *
 * Revision 1.2  1993/10/26  22:16:36  warren
 * permit e notation in text boxes and remove duplicate declaration of error.
 *
 * Revision 1.1.1.1  1993/04/07  19:39:48  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.1  1993/02/23  18:47:24  harry
 * Change #ifdef for older Sun Compilers for Fortran <-> C.
 *
 * Revision 2.0  1992/07/01  21:16:37  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/05/18  22:37:27  millerp
 * Added bogus argument for fortran.
 *
 * Revision 1.1  1992/04/21  00:23:37  millerp
 * Initial revision
 *
-----------------------------------------------------------------------
*/
#include <math.h>
double strtod(/* char *s**ptr*/);
#ifdef FLOATFUNCTIONTYPE
   FLOATFUNCTIONTYPE  catof_(instr,  error, lenstr)
#else
   float catof_(instr, error, lenstr)
#endif
/* Argument declarations */
   char *instr  ; 
   int  *error;
   int   lenstr ;  /* fortran attempting to pass VAX descriptors. */
/*                        */
   {
       char *ptr;
       double a=strtod(instr,&ptr);
       if(*ptr != '\0' && *ptr != ' ')*error=1;
#ifdef RETURNFLOAT
       RETURNFLOAT((float)a);
#else
   return ((float)a) ;
#endif
   }

