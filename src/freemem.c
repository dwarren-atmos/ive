/*
-----------------------------------------------------------------------
   $Id: freemem.c,v 1.3 2005/09/06 20:22:14 warren Exp $
   Purpose:
      This function is called by a fortran routine. It frees a
         block of memory previously allocated, and sets the pointer
         value to zero.
   History:
   $Log: freemem.c,v $
   Revision 1.3  2005/09/06 20:22:14  warren
   Move updates into current stream

   Revision 1.2.10.1  2005/09/06 17:55:22  warren
   fixed trajectories to deal with maps. Also first part of bold arrows for
   trajectories

   Revision 1.2  1993/11/29 19:39:13  warren
   Changes to 3.1 to make it work on alphas.
   FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
   C: Change int to long where necessary.
   Files: Quite a few .f files are now .F

 * Revision 1.1.1.1  1993/04/07  19:40:34  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.3  1993/02/18  00:12:50  warren
 * replaced version RCS ate
 *
 * Revision 2.2  1993/02/03  22:43:24  warren
 * fixed freemem to only free memory allocated with getmem
 * added freemem_c for other memory
 *
 * Revision 2.1  1992/11/04  00:25:14  millerp
 * Do not attemp to free memory if prt is null.
 *
 * Revision 2.0  1992/07/01  21:16:40  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.1  1992/04/21  00:23:37  millerp
 * Initial revision
 *
c-----------------------------------------------------------------------
*/
#include <stdlib.h>



void freemem_ (ptr)
     void **ptr;
     
{
  if(ptr==NULL || *ptr==NULL)return;
  if (*ptr) {
        /*Free memory only if ptr is not null.
         */
        free(*ptr);
        *ptr = NULL ;
    }
}

