#include <math.h>

   int catoi_(instr, lenstr)
/*
-----------------------------------------------------------------------
   $Id: catoi.c,v 1.1.1.1 1993/04/07 19:39:56 harry Exp $
   Purpose:
      This function is called by a fortran routine. It returns the
         integer value of the input character string.
   Arguments:
      instr  char  input  character string to be converted from 
                             character to integer.
   History:
   $Log: catoi.c,v $
   Revision 1.1.1.1  1993/04/07 19:39:56  harry
   Initial CVS checkin of IVE.

 * Revision 2.0  1992/07/01  21:16:38  millerp
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
/* Argument declarations */
   char *instr  ;
   int   lenstr ;  /* fortran attempting to pass VAX descriptors. */
/*                        */
   {
   return (atoi(instr)) ;
   }

