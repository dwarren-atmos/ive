/****************************************************************************
$Log: add_field.c,v $
Revision 1.15  2003/12/24 18:50:07  davidive
Now if someone calls force_change_units in new_file - it remains thorugh
reading of data from the file. We guess that if they force the units, they
must know what they are doing -wrf.

Revision 1.14  1997/02/07 18:39:00  harry
Remove PARTIAL_READ and expand MULTIPLE_READ.

Revision 1.13  1997/01/29 18:02:00  harry
Be sure to initialize the read_dim variable.

Revision 1.12  1996/03/05 18:21:25  harry
Isolate the use of "long" for dimension sizes to the netcdf routines.
Everywhere else we will use "int" or "integer".  Also change use of
long to nclong as a netCDF type as required by netCDF version 2.4.

Revision 1.11  1996/03/04 23:49:08  harry
Change dims to long.

 * Revision 1.10  1995/10/18  19:17:48  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.9  1994/02/17  21:32:24  harry
 * Allow new dimension entries to be created.
 *
 * Revision 1.8  1993/12/09  18:35:39  harry
 * Make sure that vatts.disp_units and display_units are always pointing
 * to the same memory location.
 *
 * Revision 1.7  1993/11/29  19:38:30  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.6  1993/10/13  00:03:25  warren
 * Update current version for fixes to 3.0 version related to the meaning of
 * default in the property sheet boxes.
 *
 * Revision 1.5  1993/09/23  23:01:45  harry
 * Include dimension names in the call.
 *
 * Revision 1.4  1993/08/18  15:46:41  harry
 * Change to new transform routines.
 *
 * Revision 1.3  1993/07/09  16:35:48  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.2  1993/06/29  16:31:19  harry
 * Remove obsolete routine read_d_var, correct add_field to set
 * data_display_orig, change free_var to copy attributes, change
 * get/set_var_atts to not overflow character variables.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:07  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.13  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 2.12  1993/02/03  23:01:01  warren
 * added call to keepmem in read_d_var and add_var
 *
 * Revision 2.11  1993/02/01  23:40:15  warren
 * added in property sheet setup
 *
 * Revision 2.10  1993/01/08  19:38:50  warren
 * zero out att structure in addvar as well.
 *
 * Revision 2.9  1993/01/07  00:49:49  warren
 * fixed spurious .
 *
 * Revision 2.8  1993/01/07  00:43:48  warren
 * initialize vatts structure to defaults.
 *
 * Revision 2.7  1993/01/05  17:04:29  millerp
 * Fixed misspelled variable name.
 *
 * Revision 2.6  1993/01/04  18:58:04  warren
 * fixed to only keep one real copy of display units
 *
 * Revision 2.5  1993/01/01  00:35:55  warren
 * removed superfluous catnull's
 *
 * Revision 2.4  1992/12/30  18:00:17  warren
 * removed superfluous vars
 *
 * Revision 2.3  1992/12/29  20:26:09  warren
 * made additions for garbage collection and for freeing
 *
 * Revision 2.2  1992/09/24  23:10:55  harry
 * Keep track of original display units value.
 *
 * Revision 2.1  1992/07/13  21:43:52  millerp
 * Eliminated call to catnull. Now make sure all strings are null-terminated
 * before calling this routine.
 *
 * Revision 2.0  1992/07/01  21:12:12  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.5  1992/06/22  22:45:53  millerp
 * Added function add_field
 *
 * Revision 1.4  1992/05/29  20:20:06  millerp
 * Added code to handle attributes specifying units.
 *
 * Revision 1.3  1992/04/28  18:53:47  millerp
 * Added call to getivar to allow recursive calls.
 *
 * Revision 1.2  1992/04/23  20:41:09  millerp
 * reverse order that stag, varmin, varmax arrays are read into VAR_INFO.
 *
 * Revision 1.1  1992/04/21  00:11:26  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: add_field.c,v 1.15 2003/12/24 18:50:07 davidive Exp $";

#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif
#ifdef __osf__
#define memalign(size1,size2) malloc(size2)
#endif
#define bigone 1.0E38

/*
  This subroutine returns a pointer to the specified variable, reading
  it from the netcdf file if necessary.
  */

#include "cdfinfo.h"
#include <string.h>
#include <malloc.h>
#include <stdio.h>
#include <ctype.h>

extern void make_help_widget_();
cdf_info var_file;

/*
  The following is an abbreviation for the var_info structure for the 
  variable in question.
  */

#define VAR_INFO var_file.vars[*var_id]


void *
add_field_(addr, var_id, varnam, ndims, dims, stag, min, max, misdat, 
	    data_units, data_display_units, dim_names,
	    len1, len2, len3, len4)

/* This function adds a variable and its attrbutes to the data structure. It is
   called when an expression is evaluated in subroutine getvar.
   */

char *varnam ;
float **addr;
int   *var_id,		/* Id of requested variable */
      *ndims,		/* Number of dimensions in variable */
      *dims;		/* Array of dimensions */
float *stag,            /* staggering info */
      *min,             /* min physical coordinates */
      *max,             /* max physical coordinates */
      *misdat;          /* missing data value */
char *data_units,        
     *data_display_units,
     *dim_names;	/* Name of dimensions for this variable */
int len1, len2, len3, len4; /* Bogus arguments sent from fortran. */
{
    int i, j, k, tnumvar;
    int error;
    var_info *test;
    dim_info *dtes;

    if ( *addr == NULL ) 
      return((void *) NULL);

    /* Cat nulls to the character strings in case they are not already 
       null-terminated.
       */
    /*    (void)catnull_ (data_units, &len2, &error, &len2);
	  (void)catnull_ (data_display_units, &len3, &error, &len3);
    */   

    test = (var_info *) 
	realloc(var_file.vars, sizeof(var_info) * (var_file.nvars+1));
    if(!test){
	(void)make_help_widget_("Memory cannot currently be allocated for\
this field.\nPlease try again later.");
	if(*addr)free(*addr);
	return((void *)NULL);
    }
    else{
	k = (var_file.vars == test);
	var_file.vars = test;
	if (!k)
	    for (i=0; i < var_file.nvars; ++i)
		var_file.vars[i].vatts.disp_units
		    = var_file.vars[i].data_display_units;	
    }
    *var_id = var_file.nvars++;
    strncpy (VAR_INFO.name, varnam, MAX_NC_NAME);
    VAR_INFO.ndims = *ndims;
    VAR_INFO.in_file = 0;
    VAR_INFO.natts = 0;
    VAR_INFO.datatype = NC_FLOAT;
    VAR_INFO.misdat = *misdat;
    VAR_INFO.values = (void *)*addr;
    VAR_INFO.dims = (vardim_info *) 
	malloc(sizeof(vardim_info) * *ndims);

    /* Arrays come backwards for C, so reverse the 
       order here before loading into VAR_INFO.
       */
    for (i=0, j = (*ndims>4?3:*ndims-1); i < (*ndims>4?4:*ndims); ++i, j--) {
      VAR_INFO.dims[i].id = -1;
      for (k=0; k < var_file.ndims; ++k) {
	    if (strcmp(var_file.dims[k].name, dim_names+j*len4) == 0) {
	      VAR_INFO.dims[i].id = k;
	      break;
	    }
      }
      if (VAR_INFO.dims[i].id == -1) {
	dtes = (dim_info *) realloc(var_file.dims,
				    sizeof(dim_info)*(var_file.ndims+1));
	if (!dtes) {
	  (void)make_help_widget_("Memory cannot currently be allocated\
 for this field.\nPlease try again later.");
	  if (*addr) free(*addr);
	  return((void *)NULL);
	}
	var_file.dims = dtes;
	strcpy(var_file.dims[var_file.ndims].name, dim_names+j*len4);
	var_file.dims[var_file.ndims].size = dims[j];
	VAR_INFO.dims[i].id = var_file.ndims++;
      }
      VAR_INFO.dims[i].size = dims[j];
      VAR_INFO.dims[i].stagger = stag[j];
      VAR_INFO.dims[i].min = min[j];
      VAR_INFO.dims[i].max = max[j];
    }
    for ( k = 0; k < MAX_NC_NAME; k++ ) {
      VAR_INFO.data_units[k] = '\0';
      VAR_INFO.data_display_units[k] = '\0';
      VAR_INFO.data_display_units_orig[k] = '\0';
    }
    strncpy (VAR_INFO.data_units, data_units,MAX_NC_NAME);
    strncpy (VAR_INFO.data_display_units, data_display_units,MAX_NC_NAME);
    strncpy (VAR_INFO.data_display_units_orig, 
	     data_display_units,MAX_NC_NAME);
    VAR_INFO.vatts.disp_units = VAR_INFO.data_display_units;
    
    for(j=0; VAR_INFO.name[j] != '\0'; j++)
      VAR_INFO.button_name[j]=toupper(VAR_INFO.name[j]);
    VAR_INFO.button_name[j]='\0';
    VAR_INFO.temporary = 1;
    for(j=0; j<40; j++)
      VAR_INFO.vatts.cont_values[j]=0;
    VAR_INFO.vatts.cont_int = 0;
    VAR_INFO.vatts.major_tic = 0;
    VAR_INFO.vatts.minor_tic = 0;
    VAR_INFO.vatts.lmax = bigone;
    VAR_INFO.vatts.lmin = bigone;
    VAR_INFO.vatts.no_zero = 0;
    VAR_INFO.vatts.lin = 1;
    VAR_INFO.vatts.num_vals = 0;
    VAR_INFO.vatts.num_labs = 2;
    VAR_INFO.vatts.do_labs = 1;
    
    printf("VAR: %s UNITS: %s DISP: %s\n", 
	   VAR_INFO.name, 
	   VAR_INFO.data_units, VAR_INFO.data_display_units);
    /* Return pointer to data */
    return (VAR_INFO.values);
}
