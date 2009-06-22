/***********************************************************************
static char ident[] = "$Id: change_units.c,v 1.6 2003/12/24 18:50:08 davidive Exp $"
Purpose:
   This routine changes the data display units for the variable 
   specified by var_id. On sucessful completion, this routine returns
   1, otherwise it returns 0. If the new unit is "DEFAULT", the routine
   resets the data_display_units to the original value and return it
   in value.
Arguments:
   See below.
History:
$Log: change_units.c,v $
Revision 1.6  2003/12/24 18:50:08  davidive
Now if someone calls force_change_units in new_file - it remains thorugh
reading of data from the file. We guess that if they force the units, they
must know what they are doing -wrf.

Revision 1.5  2002/05/22 17:56:36  davidive
First working version of trajectories colored by alternative fields
There is not a complete interface yet!!!

Revision 1.4  1999/04/13 23:11:47  harry
strncpy does not have 2 arguments, but 3 - I say - 3 arguments.

Revision 1.3  1999/03/23 19:10:19  warren
Added command to set units on any field reguardless of current units.
This is primarily for fields derived by IVE math.

Revision 1.2  1996/11/06 20:54:50  harry
Maintain domain units across different data files.

 * Revision 1.1.1.1  1993/04/07  19:41:05  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.1  1992/09/24  23:10:07  harry
 * Add DEFAULT handling.
 *
 * Revision 2.0  1992/07/01  21:12:19  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/06/01  16:39:04  millerp
 * *** empty log message ***
 *
 * Revision 1.1  1992/06/01  16:15:35  millerp
 * Initial revision
 *

***********************************************************************/

#include "cdfinfo.h"
#include <string.h>

cdf_info var_file;
#define VAR_INFO var_file.vars[*var_id]

void *change_units_ (var_id, value, len1)
   int *var_id;  /* input  the variable id. */
   char *value;  /* input  the new data display units. */
   int len1;     /* input  bogus argument sent from fortran. */
{
    int i;
    
    /* Check for valid variable id.
     */
    if (*var_id < 0 || *var_id >= var_file.nvars) 
      return ((void *) 0);

    for ( i = 0; i < MAX_NC_NAME; i++) 
      VAR_INFO.data_display_units[i] =  '\0';
    
    if (strcmp(value, "DEFAULT"))
	strcpy (VAR_INFO.data_display_units, value);
    else {
	strcpy (VAR_INFO.data_display_units, VAR_INFO.data_display_units_orig);
	for (i=0; i < len1; i++) value[i] = '\0';
	strcpy (value, VAR_INFO.data_display_units_orig);
    }
    
    return ( (void *) 1);
}

void *force_change_units_ (var_id, value, len1)
   int *var_id;  /* input  the variable id. */
   char *value;  /* input  the new data display units. */
   int len1;     /* input  bogus argument sent from fortran. */
{
    int i;
    
    /* Check for valid variable id.
     */
    if (*var_id < 0 || *var_id >= var_file.nvars) 
      return ((void *) 0);

    for ( i = 0; i < MAX_NC_NAME; i++) {
      VAR_INFO.data_units[i] =  '\0';
      VAR_INFO.data_display_units[i] =  '\0';
    }

    strncpy (VAR_INFO.data_units, value, len1);
    strncpy (VAR_INFO.data_display_units, value, len1);
    VAR_INFO.units_forced=1;
    
    return ( (void *) 1);
}

void *change_domain_units_ (dim, value, len1)
   int *dim;	 /* input  dimension */
   char *value;  /* input  the new data display units. */
   int len1;     /* input  bogus argument sent from fortran. */
{
    int i, n=*dim-1;
    
    for ( i = 0; i < MAX_NC_NAME; i++) 
      var_file.domain_display_units[n][i] =  '\0';
    
    if (strcmp(value, "DEFAULT"))
	strcpy (var_file.domain_display_units[n], value);
    else {
	strcpy (var_file.domain_display_units[n],
		var_file.domain_display_units_orig[n]);
	for (i=0; i < len1; i++) value[i] = '\0';
	strcpy (value, var_file.domain_display_units_orig[n]);
    }
    
    return ( (void *) 1);
}

void *ive_get_units_ (var_id, unit, len)
   int *var_id;  /* input  the variable id. */
   char *unit;  /* output unit name */
   int len;      /*fortran char len var*/
{
    int i;
    /* Check for valid variable id.
     */
    if (*var_id < 0 || *var_id >= var_file.nvars) 
      return ((void *) 0);

    strncpy(unit,VAR_INFO.data_units, len-1);
    return ((void *)1);
}

void *ive_get_display_units_ (var_id, unit, len)
   int *var_id;  /* input  the variable id. */
   char *unit;  /* output unit name */
   int len;      /*fortran char len var*/
{
    int i;
    /* Check for valid variable id.
     */
    if (*var_id < 0 || *var_id >= var_file.nvars) 
      return ((void *) 0);

    strncpy(unit,VAR_INFO.data_display_units,len-1);
    return ((void *)1);
}

