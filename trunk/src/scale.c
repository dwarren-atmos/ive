static char ident[] = "$Id: scale.c,v 1.2 1996/07/12 20:59:04 harry Exp $" ;

/***********************************************************************
  Purpose : 
     This is a fortran-callable function to transform the input array 
     according to the function:
     data_out = data_in * slope + intercept
  Arguments : 
     See below.
  History:
  $Log: scale.c,v $
  Revision 1.2  1996/07/12 20:59:04  harry
  Change data and domain slopes and intercepts to real*8.

 * Revision 1.1.1.1  1993/04/07  19:40:47  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.1  1992/10/16  17:06:32  warren
 * removed bogus devide by 0 error messge
 *
 * Revision 2.0  1992/07/01  21:28:06  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.3  1992/06/15  19:12:31  millerp
 * Added spval check.
 *
 * Revision 1.2  1992/06/10  17:53:45  millerp
 * Fixed from data++ to data + i so memory location pointed to by
 * data is not changed.
 *
 * Revision 1.1  1992/05/29  22:38:51  millerp
 * Initial revision
 *
***********************************************************************/


void scale_ (data, npts, slope, intercept, spval)
     float *data ;      /* in/out
			   data array to be transformed. */
     int   *npts ;      /* input
			   number of points in data. */
     double *slope ;    /* input
			   the slope of the transformation 
			   function. */
     double *intercept ;/* input
			   the intercept of the transformation 
			   function. */
     float *spval;      /* input
			   the special value. If spval != 0.0, all 
			   points == spval are not scaled, since these 
			   points are not plotted. */
{
    int i ;

    
    if ( *spval == 0.0 ) 
      /* Case where special value feature is not used.
       */
      for ( i = 0 ; i < *npts ; i++){
	data[i] = data[i] * (*slope) + (*intercept) ;
      }
    else 
      /* Case where special value feature is used.
       */
      for ( i = 0 ; i < *npts ; i++)
	if ( data[i] != *spval )
	  data[i] = data[i]  * (*slope) + (*intercept) ;
}


/***********************************************************************
  Purpose : 
     This is a fortran-callable function to transform the input array 
     according to the function:
     data_out = (data_in - intercept) / slope
     ie: it is the inverse of the function scale, found above.
  Arguments : 
     See below.
  History:
  $Log: scale.c,v $
  Revision 1.2  1996/07/12 20:59:04  harry
  Change data and domain slopes and intercepts to real*8.

 * Revision 1.1.1.1  1993/04/07  19:40:47  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.1  1992/10/16  17:06:32  warren
 * removed bogus devide by 0 error messge
 *
 * Revision 2.0  1992/07/01  21:28:06  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.3  1992/06/15  19:12:31  millerp
 * Added spval check.
 *
 * Revision 1.2  1992/06/10  17:53:45  millerp
 * Fixed from data++ to data + i so memory location pointed to by
 * data is not changed.
 *
 * Revision 1.1  1992/05/29  22:38:51  millerp
 * Initial revision
 *
***********************************************************************/


void unscale_ (data, npts, slope, intercept, spval)
     float *data ;      /* in/out
			   data array to be transformed. */
     int   *npts ;      /* input
			   number of points in data. */
     double *slope ;    /* input
			   the slope of the transformation 
			   function. */
     double *intercept ;/* input
			   the intercept of the transformation 
			   function. */
     float *spval;      /* input
			   the special value. If spval != 0.0, all 
			   points == spval are not scaled, since these 
			   points are not plotted. */
{
    int i ;
    void make_help_widget_() ;    

    
    if ( *slope == 0.0 ) {
/*	(void)make_help_widget_ ("Attempt to divide by zero in function UNSCALE. Data is unchanged.");*/
	return;
    }

    if ( *spval == 0.0 ) 
      /* Case where special value feature is not used.
       */
      for ( i = 0 ; i < *npts ; i++)
	data[i] = ( data[i] - (*intercept) ) / *slope ;
    else 
      /* Case where special value feature is used.
       */
      for ( i = 0 ; i < *npts ; i++)
	if ( data[i] != *spval )
	  data[i] = ( data[i] - (*intercept) ) / *slope ;
}
