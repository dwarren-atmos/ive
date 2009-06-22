#include <ctype.h>
#include <string.h>
#include <strings.h>

void upcase_(string, lenstr, length)

/*
-----------------------------------------------------------------------
   $Id: upcase.c,v 1.3 1997/04/11 18:48:21 warren Exp $
   Purpose:
      This function is called by a fortran routine. Given a character 
      string, it returns the same string in upper case. 
   Arguments:
      See below
   History:
   $Log: upcase.c,v $
   Revision 1.3  1997/04/11 18:48:21  warren
   Fixed for sgi and also to update color widget more frequently.

   Revision 1.2  1996/04/12 18:32:38  harry
   Add Fortran routine rindex, since Digital Unix does not have one.

   Revision 1.1.1.1  1993/04/07 19:40:48  harry
   Initial CVS checkin of IVE.

 * Revision 2.1  1992/11/19  00:24:13  millerp
 * Added fortran-callable strcasecmp and strcmp.
 *
 * Revision 2.0  1992/07/01  21:16:51  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.3  1992/05/27  23:49:53  millerp
 * Changed function types to void.
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

     char *string; /* character string to be converted to 
                           uppercase. */
     int  *lenstr; /* length of string. */
     int   length ; /* fortran attempting to pass VAX descriptors. */
{
     int i ;
     for ( i = 0 ; i < *lenstr ; i++ )
       string[i] = toupper(string[i]) ;
     
     return ;
     
}

void locase_(string, lenstr, length)

/*
-----------------------------------------------------------------------
   Purpose:
      This function is called by a fortran routine. Given a character 
      string, it returns the same string in lower case. 
   Arguments:
      See below.
-----------------------------------------------------------------------
*/
/* Argument declarations */

     char *string; /* character string to be converted to 
                           lower case. */
     int  *lenstr; /* length of string. */
     int   length; /* fortran attempting to pass VAX descriptors. */
{
     int i ;
     for ( i = 0 ; i < *lenstr ; i++ )
       string[i] = tolower(string[i]) ;
     
     return ;
     
}

int strcasecmp_ (string1, string2, len1, len2)
char *string1, *string2;
int *len1, *len2;
/***********************************************************************
  Purpose:
     This is a fortran-callable interface to the C function strcasecmp. 
     The function returns an integer > 0, == 0, or < 0, depending upon 
     whether string1 is lexicographically > string2, == string2, 
     or < string2.
  Arguments:
     string1  first string.
     string2  second string.
     len1     dummy argument passed by fortran.
     len2     dummy argument passed by fortran.
***********************************************************************/
{
    return (strcasecmp(string1, string2));
}

int strcmp_ (string1, string2, len1, len2)
char *string1, *string2;
int *len1, *len2;
/***********************************************************************
  Purpose:
     This is a fortran-callable interface to the C function strcmp. 
     The function returns an integer > 0, == 0, or < 0, depending upon 
     whether string1 is lexicographically > string2, == string2, 
     or < string2.
  Arguments:
     string1  first string.
     string2  second string.
     len1     dummy argument passed by fortran.
     len2     dummy argument passed by fortran.
***********************************************************************/
{
    return (strcmp(string1, string2));
}

int rindex_ (string1, string2, len1, len2)
char *string1, *string2;
int len1, len2;
/***********************************************************************
  Purpose:
     This is a fortran-callable interface to the C function rindex.
     The function returns the index of the last occurrence of 
     string2 in string1, or zero if it doex not occur.
  Arguments:
     string1  string.
     string2  sub-string.
     len1     dummy argument passed by fortran.
     len2     dummy argument passed by fortran.
***********************************************************************/
{
    int i;

    for (i=len1-len2; i >= 0; i--)
        if (strncmp(string1, string2, len2) == 0) return i+1;
    return 0;
}
