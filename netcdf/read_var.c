/*****************************************************************************
$Log: read_var.c,v $
Revision 1.31  2006/05/01 22:08:17  davidive
Changes for gcc 4.x from Orion Poplawski at Colorado Research Associates
(Joan Alexander)

Revision 1.30  2005/11/03 19:50:43  warren
Fixed crashing with large strange dataset

Revision 1.29  2003/12/24 18:50:08  davidive
Now if someone calls force_change_units in new_file - it remains thorugh
reading of data from the file. We guess that if they force the units, they
must know what they are doing -wrf.

Revision 1.28  2002/08/09 19:57:49  warren
Fixed for Linux and tested with DEC sun etc.
most items inside #ifdef's, or were general cleanup/fixes

Revision 1.27  2000/07/20 20:12:04  harry
Allow for a global missing data attribute.

Revision 1.26  1997/05/28 17:47:32  harry
More changes for NetCDF 3.3.

Revision 1.25  1997/05/28 13:49:24  harry
Change for released version 3.3 of NetCDF.

Revision 1.24  1997/04/09 19:36:12  harry
Fix to work properly when re-reading a multiple file variable.

Revision 1.23  1997/03/24 14:45:12  harry
Add support for widgets to file selection window for multiple files.

Revision 1.22  1997/03/05 16:58:03  harry
Make MULTIPLE_READ more flexible with the file naming conventions.

 * Revision 1.21  1997/02/19  19:26:21  harry
 * Fix min and max to be equal when delta = 0 and dim = 1.
 *
Revision 1.20  1997/02/13 17:18:45  harry
Fix IVE version of nc_get_vara_float.

Revision 1.19  1997/02/07 18:39:01  harry
Remove PARTIAL_READ and expand MULTIPLE_READ.

Revision 1.18  1997/01/29 18:41:13  harry
Fix read_var when called on a variable already read in (set dims properly).

Revision 1.17  1997/01/17 20:38:10  harry
First changes for MULTIPLE_READ and PARTIAL_READ.

Revision 1.16  1997/01/17 17:26:12  harry
Do not allocate start and length if reading a scalar variable (i.e.
zero dimension).

Revision 1.15  1996/03/14 21:56:01  harry
ncattget returns -1 for error, not non-zero.

Revision 1.14  1996/03/05 18:21:32  harry
Isolate the use of "long" for dimension sizes to the netcdf routines.
Everywhere else we will use "int" or "integer".  Also change use of
long to nclong as a netCDF type as required by netCDF version 2.4.

Revision 1.13  1996/02/14 23:16:26  harry
Fix bug in handling variables with less than 4 dimensions.

Revision 1.12  1995/11/20 17:54:11  harry
Add routine ncattgetf to return the value of an attribute as a float no
matter how it appears in the netCDF file.

 * Revision 1.11  1995/10/04  22:09:27  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.10  1994/11/03  00:04:32  harry
 * Correct recursive call to read_var and properly allocate start.
 *
 * Revision 1.9  1994/02/17  21:33:06  harry
 * Add dim_names to the calling sequece of read_var and remove subroutine
 * get_dim_names.
 *
 * Revision 1.8  1993/12/15  18:08:53  harry
 * Remove variable name overloading so Sun debugger will not get confused.
 *
 * Revision 1.7  1993/11/29  19:38:32  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.6  1993/11/03  23:41:51  warren
 * define memalign for alpha
 *
 * Revision 1.5  1993/10/13  00:03:31  warren
 * Update current version for fixes to 3.0 version related to the meaning of
 * default in the property sheet boxes.
 *
 * Revision 1.4  1993/09/28  20:28:07  harry
 * Make sure that the units are consistent in the attributes listing, and
 * zero out data_display_orig before the netcdf call.
 *
 * Revision 1.3  1993/09/23  21:53:35  harry
 * Make sure a dimension variable is read in if necessary.
 *
 * Revision 1.2  1993/08/18  15:46:49  harry
 * Change to new transform routines.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:08  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.4  1993/01/05  17:04:29  millerp
 * Fixed misspelled variable name.
 *
 * Revision 2.3  1993/01/04  18:58:04  warren
 * fixed to only keep one real copy of display units
 *
 * Revision 2.2  1992/12/08  23:50:46  millerp
 * Add checks for null pointers returned from malloc.
 *
 * Revision 2.1  1992/09/24  23:11:22  harry
 * Keep track of original display units value.
 *
 * Revision 2.0  1992/07/01  21:12:14  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.3  1992/06/09  17:19:11  harry
 * Sort frtime for Unidata files.
 *
 * Revision 1.2  1992/05/29  20:20:23  millerp
 * Added code to handle attributes specifying units.
 *
 * Revision 1.1  1992/04/21  00:11:26  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: read_var.c,v 1.31 2006/05/01 22:08:17 davidive Exp $" ;

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
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>

extern void make_help_widget_(),setlvar_();
extern void getrarr_(),make_help_widget_(),setlvar_();
#ifdef NC_FILL_BYTE
#define SIZE_T size_t
#else
#define SIZE_T long
#endif

cdf_info var_file;

/*
   Attribute names for per variable attributes.
*/

static char dname[4] = {'x', 'y', 'z', 't'};
static char minstr[] = " _min";
static char ominstr[] = " min";
static char omaxstr[] = " max";
static char ostag[] = " stag";

/*
  The following is an abbreviation for the var_info structure for the 
  variable in question.
  */

#define VAR_INFO var_file.vars[*var_id]
float *
read_var_(var_id, ndims, dims, stag, min, max, misdat,
	  data_units, data_display_units, dim_names,
	  len1, len2, len3)

int   *var_id,		  /* Id of requested variable */
      *ndims,		  /* Number of dimensions in variable */
      *dims;		  /* Array of dimensions */
float *stag,              /* staggering info */
      *min,               /* min physical coordinates */
      *max,               /* max physical coordinates */
      *misdat;            /* missing data value */
char *data_units,        
     *data_display_units,
     *dim_names;	  /* name of the dimensions */
int len1, len2, len3;
{
    int i, j, k, n=1, status, flag, read_from_nc=0, four=4;
    SIZE_T *start, *length;	/* Needed for hyperslice access */
    float *vt, *pt;
    float delta[4], dommin[4], dommax[4];


    /*
      Check for valid variable id.
      */
    if (*var_id < 0 || *var_id >= var_file.nvars) {
	return((float *) 0);
    }
    /* 
      Get dimensions.
      */
    (void)getrarr_("grid_delta", delta, &four, &i, 10);
    (void)getrarr_("plmin", dommin, &four, &i, 5);
    (void)getrarr_("plmax", dommax, &four, &i, 5);
    *ndims = VAR_INFO.ndims;
    if (VAR_INFO.values == 0) {
	/*
	  This variable has not yet been read.
	  Read per dimension attributes.
	  */
	if (var_file.file_dim > -1 && *ndims == 4)
	    VAR_INFO.dims[3-var_file.file_dim] =
		var_file.vars[var_file.file_varid].dims[0];
        VAR_INFO.vatts.lmin = VAR_INFO.vatts.lmax = bigone;
	read_from_nc = 1;
	flag = ncopts;
	ncopts = 0;
	for (i=0; i < (*ndims>4?4:*ndims); ++i){
	    int jfor = *ndims-1-i;
	    /*
	       New style attribute (min only).
	       */
	    VAR_INFO.dims[i].stagger = 0;
	    minstr[0] = dname[jfor];
	    if (var_file.file_dim > -1 && *ndims == 4 &&
		i == 3-var_file.file_dim) {
		VAR_INFO.dims[i].min = dommin[jfor];
		VAR_INFO.dims[i].max = dommax[jfor];
	    }
	    else if (ncattgetf(var_file.id, *var_id, minstr,
			    &VAR_INFO.dims[i].min) != -1) {
                if (delta[jfor] == 0.)
                    VAR_INFO.dims[i].max = dommax[jfor];
		else VAR_INFO.dims[i].max = VAR_INFO.dims[i].min 
                    + delta[jfor]*(VAR_INFO.dims[i].size-1);
	    }
	    else {
		/*
		   Old style attributes.
		   */
		ominstr[0] = dname[jfor];
		if (ncattgetf(var_file.id, *var_id, ominstr,
			     &VAR_INFO.dims[i].min) != -1) {
		    omaxstr[0] = dname[jfor];
		    if (ncattgetf(var_file.id, *var_id, omaxstr,
				 &VAR_INFO.dims[i].max) == -1)
			VAR_INFO.dims[i].max = dommax[jfor];
		    ostag[0] = dname[jfor];
		    ncattgetf(var_file.id, *var_id, ostag,
			     &VAR_INFO.dims[i].stagger);
		}
		else {
		    /*
		       First, assume the variable is within the
		       domain boundary.
		       */
		    VAR_INFO.dims[i].min = dommin[jfor];
		    if (delta[jfor] == 0.) {
			if (VAR_INFO.dims[i].size == 1)
			    VAR_INFO.dims[i].max = dommin[jfor];
			else
			    VAR_INFO.dims[i].max = dommax[jfor];
		    }
		    else VAR_INFO.dims[i].max = dommin[jfor]
			+ delta[jfor]*(VAR_INFO.dims[i].size-1);
		    /* 
		       Check for a variable with the same name as the dimension
		       to use for physical limits.  Do not let a variable try
		       to reference itself in this search.
		       */
		    for (j=0; j < var_file.nvars; ++j) {
			if (j != *var_id &&
			    strcasecmp(var_file.vars[j].name, 
				       var_file.dims[VAR_INFO.dims[i].id].name)
			    == 0) {
			    float *data = (float *)var_file.vars[j].values;
			    if (data == NULL) {
				int ndim2, dim2[3];
				float sta2[3], mi2[3], ma2[3], misda2;

				data = (float *) read_var_(&j, &ndim2, dim2,
						       sta2, mi2, ma2, &misda2,
						       NULL, NULL, NULL,
						       0, 0, 0);
			    }
			    VAR_INFO.dims[i].min = data[0];
			    VAR_INFO.dims[i].max =
				data[VAR_INFO.dims[i].size-1];
			}
		    }
		}
	    }
	}
	
	/* 
	  Get missing data value.
	  */
	status = ncattgetf(var_file.id,*var_id,"missing_value",
			  &VAR_INFO.misdat);
	if(status == -1) status = ncattgetf(var_file.id,*var_id,"_FillValue",
					   &VAR_INFO.misdat);
	if(status == -1) status = ncattgetf(var_file.id,*var_id,"missing_data",
			  &VAR_INFO.misdat);
	if(status == -1) VAR_INFO.misdat=var_file.misdat;
	
	/* Get data units and data display units. If units display units are
	   not found, set display units = units.
	 */
	if(!VAR_INFO.units_forced){
	  for ( j = 0; j < MAX_NC_NAME; j++) {
	    VAR_INFO.data_units[j] =  '\0';
	    VAR_INFO.data_display_units_orig[j] = '\0';
	  }
	  status = ncattget(var_file.id, *var_id, "units",
			    VAR_INFO.data_units);
	  status = ncattget(var_file.id, *var_id, "display_units",
			    VAR_INFO.data_display_units_orig);
	  if ( status == -1 ) 
	    strcpy (VAR_INFO.data_display_units_orig,VAR_INFO.data_units);
	  if (VAR_INFO.data_display_units[0] == '\0')
	    strcpy(VAR_INFO.data_display_units,
		   VAR_INFO.data_display_units_orig);
	}
    }
    
    /*
      Case where we read all values in one file.
      */
    if (var_file.file_dim == -1 || *ndims < 4) {
	for (i=0; i < *ndims; i++)
	    dims[*ndims-1-i] = VAR_INFO.dims[i].size;
	if (VAR_INFO.values == NULL) {
	/*
	  Allocate space for the variable.
	  */
	    if (*ndims) {
		start = (SIZE_T *) malloc(sizeof(long) * *ndims);
		length = (SIZE_T *) malloc(sizeof(long) * *ndims);
		if (start == NULL || length == NULL) {
		    if (start) free(start);
		    (void)make_help_widget_("Memory cannot be allocated for \
this field.\nPlease use FREE to delete a field from memory and try again.");
		    return ((float *) 0);
		}
	    }
	    for (i=0; i < *ndims; ++i) {
		n *= VAR_INFO.dims[i].size;
		length[i] = VAR_INFO.dims[i].size;
		start[i] = 0;
	    }
	    VAR_INFO.values =
		(float *) memalign(sizeof(float), sizeof(float) * n);
	    if (VAR_INFO.values == NULL) {
		(void)make_help_widget_("Memory cannot be allocated for this\
field.\nPlease use FREE to delete a field from memory and try again.");
		if (*ndims) {
		    free (start);
		    free (length);
		}
		return ((float *) 0);
	    }
	    i = nc_get_vara_float(var_file.id, *var_id, start, length,
				  VAR_INFO.values);
	    if (*ndims) {
		free(start);
		free(length);
	    }
	    ncopts = flag;
	    /*
	       Free allocated space on error.
	       */
	    if (i != 0) {
		free(VAR_INFO.values);
		VAR_INFO.values = 0;
		return((float *) 0);
	    }
	}
    }
    /*
       Case where we read all values from multiple files.
       */
    else {
	for (i=0; i < *ndims; i++) {
	    if (i == 3-var_file.file_dim)
		dims[*ndims-1-i] = VAR_INFO.dims[3-var_file.file_dim].size;
	    else dims[*ndims-1-i] = VAR_INFO.dims[i].size;
	}
	if (VAR_INFO.values == NULL) {

	    float *temp;
	
	    n = 1;
	    start = (SIZE_T *) malloc(sizeof(long) * *ndims);
	    length = (SIZE_T *) malloc(sizeof(long) * *ndims);
	    if (start == NULL || length == NULL) {
		if (start) free(start);
		(void)make_help_widget_("Memory cannot be allocated for this\
field.\nPlease use FREE to delete a field from memory and try again.");
		return ((float *) 0);
	    }
	    for (i=0; i < *ndims; ++i) {
		if (i == 3-var_file.file_dim) length[i] = 1;
		else {
		    length[i] = VAR_INFO.dims[i].size;
		    n *= VAR_INFO.dims[i].size;
		}

		start[i] = 0;
	    }
	    VAR_INFO.values =
		(float *) memalign(sizeof(float), sizeof(float)*
				   n*VAR_INFO.dims[3-var_file.file_dim].size);
	    if (VAR_INFO.values == NULL) {
		(void)make_help_widget_("Memory cannot be allocated for this\
field.\nPlease use FREE to delete a field from memory and try again.");
		free (start);
		free (length);
		return ((float *) 0);
	    }
	    if (var_file.file_dim != 0) {
		if ((temp=(float *) memalign(sizeof(float), sizeof(float)*n))
		    == NULL) {
		    (void)make_help_widget_("Memory cannot be allocated for \
this field.\nPlease use FREE to delete a field from memory and try again.");
		    free (start);
		    free (length);
		    free (VAR_INFO.values);
		    VAR_INFO.values = 0;
		    return ((float *) 0);
		}
	    }
	    for (i=0; i < VAR_INFO.dims[3-var_file.file_dim].size; ++i) {

		ncclose(var_file.id);
		if ((var_file.id = ncopen(var_file.file_names+i*256,
					  NC_NOWRITE)) == -1) {
		    char error[1100];

		    sprintf(error, "Cannot open %s",
			    var_file.file_names+i*256);
		    make_help_widget_(error, strlen(error));
		    VAR_INFO.values = 0;
		    return ((float *) 0);
		}
		if (var_file.file_dim == 3)
		    nc_get_vara_float(var_file.id, *var_id, start, length,
				      VAR_INFO.values+n*i);
		else {
		    nc_get_vara_float(var_file.id, *var_id, start, length,
				      temp);
		    pt = temp;
		    if (var_file.file_dim == 2) {
			for (k = 0; k < VAR_INFO.dims[0].size; k++){
			    vt = VAR_INFO.values + (k*VAR_INFO.dims[1].size+i)
				*VAR_INFO.dims[2].size*VAR_INFO.dims[3].size;
			    for (j=0;
				 j<VAR_INFO.dims[2].size*VAR_INFO.dims[3].size;
				 j++)
				*(vt++) = *(pt++);
			}
		    }
		    else if (var_file.file_dim == 1) {
			for (k=0; 
			     k < VAR_INFO.dims[0].size*VAR_INFO.dims[1].size;
			     k++) {
			    vt = VAR_INFO.values + (k*VAR_INFO.dims[2].size+i)
				*VAR_INFO.dims[3].size;
			    for (j=0; j < VAR_INFO.dims[3].size; j++)
				*(vt++) = *(pt++);
			}
		    }
		    else {
			vt = VAR_INFO.values+i;
			for (k=0;
			     k < VAR_INFO.dims[0].size*VAR_INFO.dims[1].size
			         *VAR_INFO.dims[2].size;
			     k++, vt+= VAR_INFO.dims[3].size)
			    *(vt) = *(pt++);
		    }
		}
	    }
	    free(start);
	    free(length);
	    ncopts = flag;
	}
    }
    /*
      Return requested information. Put array info in correct order
      for fortran.
      */
    for (i = *ndims-1, j=0; i >=0; i--,j++) {
	if (dim_names)
	    strncpy(dim_names+j*len3, var_file.dims[VAR_INFO.dims[i].id].name,
		    (len3 < MAX_NC_NAME? len3: MAX_NC_NAME));
	stag[j] = VAR_INFO.dims[i].stagger;
	min[j] = VAR_INFO.dims[i].min;
	max[j] = VAR_INFO.dims[i].max;
    }
    *misdat = VAR_INFO.misdat;
    if (data_units)
	strncpy(data_units, VAR_INFO.data_units,
		((len1 < MAX_NC_NAME && len1>0)? len1: MAX_NC_NAME)) ;
    if (data_display_units)
	strncpy(data_display_units, VAR_INFO.data_display_units,
		((len2 < MAX_NC_NAME && len2>0)? len2: MAX_NC_NAME)) ;
    setlvar_("read_from_nc", &read_from_nc, &i, 12);
    return (VAR_INFO.values);
}

#ifndef NC_FILL_BYTE

/* This routine is part of netcdf version 3. */

#include <errno.h>

int nc_get_vara_float(ncid, varid, start, count, ip)
    int ncid, varid;
    long start[], count[];
    float *ip;
    
{
    int i, j, n;
    
    n = 1;
    for (i=0; i < var_file.vars[varid].ndims; i ++)
	n *= var_file.vars[varid].dims[i].size;
    switch (var_file.vars[varid].datatype) {
    case NC_FLOAT:
	i = ncvarget(ncid, varid, start, count, (void *) ip);
	return i;
    case NC_BYTE:
    case NC_CHAR:
    {
	char *ctmp = (char *) memalign(sizeof(long), sizeof(char) *n);

	if (ctmp == NULL) return errno;

	i = ncvarget(ncid, varid, start, count, (void *) ctmp);
	if (i == 0) {
	    for (j=0; j < n; j++) {
		ip[j] = ctmp[j];
	    }
	}
	free(ctmp);
	return i;
    }
    case NC_SHORT:
    {
	short *stmp = (short *) memalign(sizeof(long), sizeof(short) *n);
	
	if (stmp == NULL) return errno;
	i = ncvarget(ncid, varid, start, count, (void *) stmp);
	if (i == 0) {
	    for (j=0; j < n; j++) {
		ip[j] = stmp[j];
	    }
	}
	free(stmp);
	return i;
    }
    case NC_LONG:
    {
	nclong *ltmp = (nclong *) memalign(sizeof(long), sizeof(nclong) *n);
	
	if (ltmp == NULL) return errno;
	i = ncvarget(ncid, varid, start, count, (void *) ltmp);
	if (i == 0) {
	    for (j=0; j < n; j++) {
		ip[j] = ltmp[j];
	    }
	}
	free(ltmp);
	return i;
    }
    case NC_DOUBLE:
    {
	double *dtmp = (double *) memalign(sizeof(double), sizeof(double) *n);
		
	if (dtmp == NULL) return errno;
	i = ncvarget(ncid, varid, start, count, (void *) dtmp);
	if (i == 0) {
	    for (j=0; j < n; j++) {
		ip[j] = dtmp[j];
	    }
	}
	free(dtmp);
	return i;
    }
    }
}

#endif NETCDF3
