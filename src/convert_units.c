static char ident[] = "@(#)$Id: convert_units.c,v 1.3 1996/07/12 20:59:00 harry Exp $" ;

/***********************************************************************
  Purpose : 
     This is a fortran-callable function to provide a fortran interface
     to the Unidata unit conversion utility. If the returned function
     value == 0, the units conversion was sucessful, otherwise it was
     not.
  Arguments : 
     See below.
  History:
  $Log: convert_units.c,v $
  Revision 1.3  1996/07/12 20:59:00  harry
  Change data and domain slopes and intercepts to real*8.

 * Revision 1.2  1993/12/22  00:15:45  harry
 * Moved call of utInit to ive_main.c
 *
 * Revision 1.1.1.1  1993/04/07  19:40:17  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.2  1993/03/18  22:58:10  harry
 * Get correct status from utScan calls.
 * If unknown units but the units are the same, set to identity transformation.
 *
 * Revision 2.1  1993/02/10  18:12:42  warren
 * changed status from pointer to int and removed malloc.
 *
 * Revision 2.0  1992/07/01  21:28:08  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/06/10  17:55:18  millerp
 * Set status-0, slope=1, intercept=0 if either from_units or to_units
 * are blank.
 *
 * Revision 1.1  1992/05/29  22:38:51  millerp
 * Initial revision
 *
***********************************************************************/

#include <stdio.h>
#include <udunits.h>

int convert_units_ (from_units, to_units, slope, intercept, 
		     len_from_units, len_to_units) 
     char *from_units ;  /* input
			    units to be converted from. */
     char *to_units ;    /* input
			    units to be converted to. */
     double *slope ;      /* output
			    the slope of the units transformation 
			    function. */
     double *intercept ;  /* output
			    the intercept of the units transformation 
			    function. */
     int len_from_units ; /* input
			     extra argument sent by fortran */
     int len_to_units ;   /* input
			     extra argument sent by fortran */
{
    int status ;
    static char string[256];
    utUnit unit1, unit2 ;
    void make_help_widget_() ;


    /* First check for blank strings. If any of the input strings are
       blank, set slope and intercept and return.
     */
    status = charcount (from_units) ;
    if ( status  == 0 ) {
	*slope = 1.0;
	*intercept = 0.0;
	return ( (int) status ) ;
    }
    
    status = charcount (to_units) ;
    if ( status == 0 ) {
	*slope = 1.0;
	*intercept = 0.0;
	return ( (int) status ) ;
    }
    
    /*
       Convert units strings to binary structure representation.
       */
    status = utScan(from_units, &unit1) ;
    if (status == 0) status = utScan(to_units, &unit2) ;
    if ( status != 0 ) {
	switch ( status ) {
	case UT_ENOINIT :
	    make_help_widget_ ("Error in convert_units: Package has not been initialized.") ;
	    break ;
	case UT_EUNKNOWN :
	    if (strcasecmp(from_units, to_units) == 0) {
		*slope = 1.0;
		*intercept = 0.0;
		return( (int) 0 );
	    }
	    make_help_widget_ ("Error in convert_units: An unknown unit was specified.") ;
	    break ;
	case UT_ESYNTAX : 
	    make_help_widget_ ("Error in convert_units: The units-file contains a syntax error.") ;
	    break ;
	default :
	    break ;
	}
    }
    else {
	/* 
	   Find the coeffecients of the conversion function.
	   */
	status = utConvert(&unit1, &unit2, slope, intercept) ;
	if ( status != 0) {
	    switch ( status ) {
	    case UT_ENOINIT : 
		make_help_widget_ ("Error in convert_units: Package has not been initialized.") ;
		break ;
	    case UT_EINVALID : 
		make_help_widget_ ("Error in convert_units: One of the unit-structures is invalid.") ;
		break ;
	    case UT_ECONVERT :
		sprintf (string, "Error in convert_units: Units %s and %s are not convertable.", from_units, to_units);
		make_help_widget_ (string) ;
		break ;
	    default :
		break ;
	    }
	}
    }
    /* 
       Return function value.
       */
    return ( (int) status ) ;
}


/* This routine returns the number of nonblank characters in the 
   input string.
*/

#include <string.h>

int charcount (string)
     char *string;  /* String to be examined */
{
    int i, nchars = 0 ;

    for ( i = 0; i < strlen(string); i++ ) {
        if ( (string[i] != '\n') && (string[i] != ' ') )
	  nchars++ ;
    }
    return (nchars) ;
}
