#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif
#ifdef __osf__
#define memalign(size1,size2) malloc(size2)
#endif
/*
-----------------------------------------------------------------------
   $Id: getmem.c,v 1.2 1993/11/03 23:47:36 warren Exp $
   Purpose:
      This routine is called from a fortran routine. It allocates
      size words of memory, and returns a pointer to the memory
      allocated.
   History:
   $Log: getmem.c,v $
   Revision 1.2  1993/11/03 23:47:36  warren
   general cleanup for port to alpha including getting rid of uchar and ushort.

 * Revision 1.1.1.1  1993/04/07  19:40:44  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.3  1993/02/18  00:17:15  warren
 * *** empty log message ***
 *
 * Revision 2.2  1993/02/18  00:15:44  warren
 * replaced version RCS ate.
 *
 * Revision 2.1  1993/02/03  23:03:33  warren
 * getmem now keeps track of what it allocated.
 * >> adeded routine keepmem to remove memory from getmem's list^C
 * added getmem_cleanmem for the garbage collector
 *
 * Revision 2.0  1992/07/01  21:16:46  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.1  1992/04/21  00:23:37  millerp
 * Initial revision
 *
-----------------------------------------------------------------------
*/
#include <stdlib.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#endif

void 
*getmem_(size)

int  *size;

{
    void *ptr;
    /*    ptr = (int *) memalign(sizeof(float), sizeof(float) * (*size));*/
    ptr = malloc(sizeof(float) * (*size));
    return( ptr );
}
