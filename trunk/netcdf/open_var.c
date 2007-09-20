/****************************************************************************
$Log: open_var.c,v $
Revision 1.30  2006-10-05 16:29:42  reinecke
added miss and missing to possible names for missing values

Revision 1.29  2003/12/24 18:50:08  davidive
Now if someone calls force_change_units in new_file - it remains thorugh
reading of data from the file. We guess that if they force the units, they
must know what they are doing -wrf.

Revision 1.28  2002/05/22 17:56:36  davidive
First working version of trajectories colored by alternative fields
There is not a complete interface yet!!!

Revision 1.27  2000/07/20 20:12:03  harry
Allow for a global missing data attribute.

Revision 1.26  1998/12/17 23:17:13  warren
Cleanup of pointer casts to long instead of int, freeing XmStrings when no
longer in use, passing all args to sprinf's and making correctly casting NULLs

Revision 1.25  1997/07/22 19:50:39  harry
Fix multiple reading to allow user to specify coordinate transform.

Revision 1.24  1997/03/24 14:45:11  harry
Add support for widgets to file selection window for multiple files.

Revision 1.23  1997/03/05 16:58:03  harry
Make MULTIPLE_READ more flexible with the file naming conventions.

Revision 1.22  1997/02/07 20:16:53  harry
Yet more MULTIPLE_READ changes.

Revision 1.21  1997/02/07 18:39:01  harry
Remove PARTIAL_READ and expand MULTIPLE_READ.

Revision 1.20  1997/01/17 20:38:10  harry
First changes for MULTIPLE_READ and PARTIAL_READ.

Revision 1.19  1996/11/06 20:54:51  harry
Maintain domain units across different data files.

Revision 1.18  1996/08/01 22:43:36  harry
Fix problem with units.

Revision 1.17  1996/03/05 18:21:30  harry
Isolate the use of "long" for dimension sizes to the netcdf routines.
Everywhere else we will use "int" or "integer".  Also change use of
long to nclong as a netCDF type as required by netCDF version 2.4.

Revision 1.16  1995/11/20 17:54:10  harry
Add routine ncattgetf to return the value of an attribute as a float no
matter how it appears in the netCDF file.

 * Revision 1.15  1995/10/04  22:09:25  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.14  1995/05/26  15:18:00  harry
 * Add hooks to allow default transforms to limit time slider if possible.
 *
 * Revision 1.13  1994/11/18  20:44:35  harry
 * Fix for partial specification of domain by attributes.
 *
 * Revision 1.12  1994/11/03  00:03:31  harry
 * Expand the search for domain limits to all variables instead of just
 * the 4-d ones.
 *
 * Revision 1.11  1994/02/17  21:33:04  harry
 * Add dim_names to the calling sequece of read_var and remove subroutine
 * get_dim_names.
 *
 * Revision 1.10  1994/01/07  18:47:10  harry
 * Allow the domain to be specified by attributes for some dimensions and
 * a dimension variable for others rather than the all or nothing approach.
 *
 * Revision 1.9  1993/11/20  00:15:02  harry
 * vatts.disp_units will point to freed memory if the variable in a new
 * file has the same name as one in the old file.  This is NOT good and
 * has been fixed.
 *
 * Revision 1.8  1993/10/04  22:19:58  warren
 * added number of unlabeled lines to attributes.
 *
 * Revision 1.7  1993/09/30  15:52:22  warren
 * On line 284 changed:
 * var_file.dims[var_file.vars[i].dims[j].id]
 * to
 * var_file.dims[var_file.vars[i].dims[j].id].name
 *
 * Revision 1.6  1993/09/28  20:28:05  harry
 * Make sure that the units are consistent in the attributes listing, and
 * zero out data_display_orig before the netcdf call.
 *
 * Revision 1.5  1993/08/18  15:46:47  harry
 * Change to new transform routines.
 *
 * Revision 1.4  1993/07/09  16:35:50  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.3  1993/07/06  20:29:39  harry
 * Correct minor bug in last change.
 *
 * Revision 1.2  1993/07/06  20:24:34  harry
 * Retain properties across data sets.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:06  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.8  1993/02/22  21:15:14  warren
 * 0 out var_file.vars[i].dims and var_file.vars[i].values when freed
 *
 * Revision 2.7  1993/02/19  22:46:05  harry
 * Fix variable/dimension coding to ignore if domain set via
 * attributes.
 *
 * Revision 2.6  1993/02/01  23:40:15  warren
 * added in property sheet setup
 *
 * Revision 2.5  1993/01/07  22:32:54  harry
 * Correct memory bugs in handling frtime_flag at the end.
 *
 * Revision 2.4  1993/01/07  00:48:08  warren
 * fixed spurious .
 *
 * Revision 2.3  1993/01/07  00:41:55  warren
 * initialize vatts structure to defaults.
 *
 * Revision 2.2  1993/01/07  00:17:54  harry
 * Mark $frtime_sorted as permanent.
 *
 * Revision 2.1  1992/12/29  20:26:09  warren
 * made additions for garbage collection and for freeing
 *
 * Revision 2.0  1992/07/01  21:12:10  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.6  1992/06/17  16:26:37  millerp
 * Fixed to accept time as a variable and dimension.
 *
 * Revision 1.5  1992/06/09  17:19:37  harry
 * For Unidata files: if the forecast times are not in order in the file
 * read them in a slice at a time and end up with a sorted array
 * in memory.
 *
 * Revision 1.4  1992/06/01  19:46:48  harry
 * Fix section for "Unidata files" to properly pick up the units of the
 * dimension variables.
 *
 * Revision 1.3  1992/06/01  16:15:52  millerp
 * Changed open status back to NC_NOWRITE.
 *
 * Revision 1.2  1992/05/29  20:19:13  millerp
 * Added code to handle global attributes specifying units.
 *
 * Revision 1.1  1992/04/21  00:11:26  millerp
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: open_var.c,v 1.30 2006-10-05 16:29:42 reinecke Exp $" ;

/*
  This subroutine opens the specified var netcdf file, reads in the header
  information and returns the netCDF id.
  */
#ifdef linux
#undef  _POSIX_SOURCE
#endif
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "cdfinfo.h"
#include <malloc.h>

extern void *read_var_();
extern int getvid_();
extern void getivar_(),add_field_(),set_button_name(),keep_var_(),setavar_(),setivar_();
cdf_info var_file;

/*
   Structure cdf_dimvar_ (common block cdf_dimvar) is set up
   by IVE to contain the values of a variable with the same name as the
   fourth dimension of the largest variable in the netCDF file.  If the
   size of the time domain is set by attributes, or if a dimension
   variable does not exist, size will be zero, and data will be NULL.
   This is used by the default transform to set up the time slider
   restrictions.
   */

struct cdf_dimvar_ {
    float *data[4];
    int size[4];
} cdf_dimvar_;

/*
  Structure cdf_temp is used by file_callbacks for the multiple file
  sliders.
  */
struct cdf_temp {
    int n;
    float *values;
} cdf_temp;

/*
   Attribute names for global attributes.
   */

static char dname[4] = {'x', 'y', 'z', 't'};
static char minstr[] = " _min";
static char maxstr[] = " _max";
static char unitstr[] = " _units";
static char dunitstr[] = " _display_units";
static char deltastr[] = " _delta";
static char labstr[] = " _label";
/*
   These are old and for compatability only.
   */
static char ominstr[] = "dom min";
static char omaxstr[] = "dom max";

int
open_var_(name, dmin, dmax, domain_units, domain_display_units, 
	  delta, label, temp, len1, len2, len3, len4)

char *name;		/* Name of netcdf file */
float *dmin, *dmax;	/* Domain limits */
char *domain_units, *domain_display_units;
float *delta;		/* Grid intervals */
char *label;		/* Labels for coordinates */
int *temp;		/* Is this a temporary open just to get
			   the limits for multiple files. */
int len1, len2, len3, len4;

{
    static int first=1;
    int file_dim;	/* Dimension used for file names if data set is
			   in multiple files */
    int i, j, k, status, dim[MAX_VAR_DIMS], oldncopts, len ;
    char data_units[80], data_display_units[80];
    int find_domain[4]; /* Did we find the domain info yet? */
    nc_type datatype;
    cdf_info var_file_old;

    /* Get current value of error handler, and turn off netCDF error
       handlers. Remember to reset ncopts before leaving this routine.
       */
    oldncopts = ncopts;
    ncopts = 0;
    var_file_old.nvars = 0;
    if (!*temp) 
	for ( i=0; i < 4; i++) {
	    cdf_dimvar_.data[i] = NULL;
	    cdf_dimvar_.size[i] = 0;
	}
    /*
      For first call, set up netcdf not to bomb on error and set
      id to -1 (no file open).
      */
    if (first) {
	first = 0;
	var_file.id = -1;
    }
    /*
      If file is open, free all allocated memory for dimensions and variables
      and close it.
      */
    if (var_file.id != -1) {
	if (!*temp) {
	    free(var_file.dims);
	    for (i=0; i < var_file.nvars; i++) {
		if(var_file.vars[i].dims != (vardim_info *)0){
		    free (var_file.vars[i].dims);
		    var_file.vars[i].dims = (vardim_info *) 0;
		}
		if (var_file.vars[i].values != (void *)0){
		    free(var_file.vars[i].values);
		    var_file.vars[i].values = (void *)0;
		}
	    }
	    if (var_file.file_dim > -1) free(var_file.file_names);
	    ncclose(var_file.id);
	}
    }
    var_file_old = var_file;
    if (name == (char *)0) {
	if (var_file_old.nvars > 0) free(var_file_old.vars);
	var_file.id = -1;
	ncopts = oldncopts;
	return(0);
    }
    for (i=0; i < 4; i++) {
	domain_units[i*len3] = '\0';
	var_file.domain_display_units[i][0] = '\0';
	var_file.domain_display_units_orig[i][0] = '\0';
    }
    /*
      Open the netcdf file and get header information.
      */
    if ((var_file.id = ncopen(name, NC_NOWRITE)) == -1) {
	if (*temp) var_file = var_file_old;
	else if (var_file_old.nvars > 0) free(var_file_old.vars);
	ncopts = oldncopts;
	return(-1);
    }
    ncinquire(var_file.id, &var_file.ndims, &var_file.nvars,
	      &var_file.ngatts, &var_file.recdim);
    /*
      Look for global missing data attribute.
    */
    status = ncattgetf(var_file.id,NC_GLOBAL,"missing_value",
		       &var_file.misdat);
    if(status == -1) status = ncattgetf(var_file.id,NC_GLOBAL,"_FillValue",
					&var_file.misdat);
    if(status == -1) status = ncattgetf(var_file.id,NC_GLOBAL,"missing_data",
					&var_file.misdat);
    if(status == -1) status = ncattgetf(var_file.id,NC_GLOBAL,"missing",
					&var_file.misdat);
    if(status == -1) status = ncattgetf(var_file.id,NC_GLOBAL,"miss",
					&var_file.misdat);
    if(status == -1) var_file.misdat=0.0;

    /*
      Get the names and sizes of the dimensions.
      */
    var_file.dims = 
	(dim_info *) malloc(var_file.ndims * sizeof(dim_info));
    for (i=0; i < var_file.ndims; ++i) {
	long size;

	ncdiminq(var_file.id, i, var_file.dims[i].name, 
		 &size);
	var_file.dims[i].size = size;
    }
    /*
       Check the per dimension global attributes.
       */
    for (i=0; i < 4; ++i){
	minstr[0] = dname[i];
	find_domain[i] = 1;	
	if (ncattgetf(var_file.id,NC_GLOBAL,minstr,dmin+i) == -1) {
	    ominstr[3] = dname[i];
	    if (ncattgetf(var_file.id,NC_GLOBAL,ominstr,dmin+i) == -1) {
		dmin[i]=0.0;
		find_domain[i] = 0;
	    }
	}
	maxstr[0] = dname[i];
	if (ncattgetf(var_file.id,NC_GLOBAL,maxstr,dmax+i) == -1) {
	    omaxstr[3] = dname[i];
	    if (ncattgetf(var_file.id,NC_GLOBAL,omaxstr,dmax+i) == -1)
		dmax[i]=0.0;
	}
	unitstr[0] = dname[i];
	status = ncattget(var_file.id, NC_GLOBAL, unitstr, 
			   domain_units+i*len2);
	dunitstr[0] = dname[i];
     	if(ncattget(var_file.id, NC_GLOBAL, dunitstr, 
		     var_file.domain_display_units_orig[i]) == -1)
	    strcpy (var_file.domain_display_units_orig[i], 
		    domain_units+i*len2);
	deltastr[0] = dname[i];
     	if (ncattgetf(var_file.id, NC_GLOBAL, deltastr, delta+i) == -1)
	    delta[i] = 0.;
	labstr[0] = dname[i];
     	if (ncattget(var_file.id, NC_GLOBAL, labstr, label+i*len4) == -1) {
	    *(label+i*len4) = toupper(dname[i]);
	    *(label+i*len4+1) = '\0';
	}
    }
    /*
      Get the names and sizes of the variables.
      */
    var_file.vars = 
      (var_info *) malloc(var_file.nvars * sizeof(var_info));
    for (i=0; i < var_file.nvars; ++i) {
	ncvarinq(var_file.id, i, var_file.vars[i].name,
		 &var_file.vars[i].datatype, &var_file.vars[i].ndims,
		 dim, &var_file.vars[i].natts);
	var_file.vars[i].dims = (vardim_info *) malloc(var_file.vars[i].ndims
						       * sizeof(vardim_info));
	for (j=0; j < var_file.vars[i].ndims; ++j) {
	    var_file.vars[i].dims[j].id = dim[j];
	    var_file.vars[i].dims[j].size = var_file.dims[dim[j]].size;
	}
	var_file.vars[i].values = 0;
	var_file.vars[i].in_file = 1;
	for(j=0; var_file.vars[i].name[j] != '\0'; j++)
	  var_file.vars[i].button_name[j]=toupper(var_file.vars[i].name[j]);
	var_file.vars[i].button_name[j]='\0';
	/*
	  Report a variable with the "no_button" attribute set to 1 or
	  with the same name as a dimension as a blank.
	  */
	if (ncattinq(var_file.id, i, "no_button", &datatype, &len) != -1
	   && len == 1 && datatype == NC_LONG) {
	    nclong value;

	    ncattget(var_file.id, i, "no_button", (void *) &value);
	    if (value) var_file.vars[i].button_name[0] = '\0';
	}
	else
	    for (j=0; j < var_file.ndims; ++j)
		if (strcmp(var_file.dims[j].name,var_file.vars[i].name) == 0) {
		    var_file.vars[i].button_name[0] = '\0';
		    break;
		}
	var_file.vars[i].temporary = 0;
	var_file.vars[i].units_forced=0;
	if (!*temp) {
	    for (j=0; j < var_file_old.nvars; j++) {
		if (strcmp(var_file.vars[i].name,
			   var_file_old.vars[j].name) == 0) {
		    var_file.vars[i].vatts = var_file_old.vars[j].vatts;
		    strcpy(var_file.vars[i].data_display_units,
			   var_file_old.vars[j].data_display_units);
		    break;
		}
	    }
	    var_file.vars[i].vatts.disp_units = 
		var_file.vars[i].data_display_units;
	    if (j >= var_file_old.nvars) {
		var_file.vars[i].data_display_units[0] = '\0';
		for(j=0; j<40; j++)
		    var_file.vars[i].vatts.cont_values[j]=0;
		var_file.vars[i].vatts.cont_int = 0;
		var_file.vars[i].vatts.major_tic = 0;
		var_file.vars[i].vatts.minor_tic = 0;
		var_file.vars[i].vatts.lmax = 0;
		var_file.vars[i].vatts.lmin = 0;
		var_file.vars[i].vatts.no_zero = 0;
		var_file.vars[i].vatts.lin = 1;
		var_file.vars[i].vatts.num_vals = 0;
		var_file.vars[i].vatts.num_labs = 2;
		var_file.vars[i].vatts.do_labs = 1;
		var_file.vars[i].vatts.traj_val = -1e19;
		var_file.vars[i].vatts.traj_step = 0;
	    }
	}
    }
    ncopts = oldncopts;
    if (find_domain[0]+find_domain[1]+find_domain[2]+find_domain[3] < 4) {
	/*
	  Missing domain information, look the largest variable
	  whose dimensions have the same name as a variable, or lacking that
	  the largest variable.
	  */
	int maxdim[4], dim_var, maxndim = 0;

	for (i=0; i < var_file.nvars; ++i) {
	    if (var_file.vars[i].ndims > maxndim) {
		maxndim = var_file.vars[i].ndims;
		for (j=0; j < 4; ++j) {
		    if (find_domain[j] == 0) {
			maxdim[j] = 0;
			dmin[j] = 0.;
			dmax[j] = 0.;
		    }
		}
	    }
	    if (var_file.vars[i].ndims == maxndim) {

		for (j=0; j < maxndim; ++j) {

		    int new_max=0, jfor = maxndim-j-1;
		    
		    if (find_domain[jfor]) continue;
		    if (var_file.vars[i].dims[j].size > maxdim[jfor]) {
			maxdim[jfor] = var_file.vars[i].dims[j].size;
			new_max = 1;
		    }
		    /*
		      Look for variable with same name as dimension if
		      this is the largest dimension so far or if no
		      such variable has been found for this dimension.
		      */
		    if (new_max || (dmin[jfor] == 0. && 
				    dmax[jfor] == 0.)) {
			dim_var = -1;
			for (k=0; k < var_file.nvars; ++k) {
			    if (strcmp(var_file.vars[k].name, 
			       var_file.dims[var_file.vars[i].dims[j].id].name)
				== 0) {
				dim_var = k;
				break;
			    }
			}
			if (dim_var != -1) {
			    int ndims, dims[3];
			    float stag[3], min[3], max[3], misdat, *data;
			    
			    data = (float *) read_var_(&dim_var, &ndims, dims,
						       stag, min, max, &misdat,
						       data_units,
						       data_display_units,
						       NULL, 80, 80, 0);
			    if(data != NULL){
			      dmin[jfor] = data[0];
			      dmax[jfor] = data[dims[0]-1];
			      if (data_units[0])
				strcpy(domain_units+(jfor)*len2, data_units);
			      if (data_display_units[0]) 
				strcpy(
				      var_file.domain_display_units_orig[jfor],
				      data_display_units);
			      cdf_dimvar_.size[jfor] = dims[0];
			      cdf_dimvar_.data[jfor] = data;
			    }
			}
		    }
		}
	    }
	}
	/*
	   For each coordinate: if no domain information was specified,
	   and there is no dimension variable, use the size of the largest
	   array and set the delta to 1.
	   */
	for (j=0; j < maxndim; ++j) {
	    if (find_domain[maxndim-j-1] == 0 &&
		maxdim[j] && dmin[j] == 0. && dmax[j] == 0.) {
		dmin[j] = 1.;
		dmax[j] = maxdim[j];
		delta[j] = 1.;
	    }
	}
    }
    if (!*temp && var_file_old.nvars) {
	for (i=0; i < 4; i++) {
	    if (domain_units[i*len3] 
		&& var_file_old.domain_display_units[i][0])
		strcpy(var_file.domain_display_units[i],
		       var_file_old.domain_display_units[i]);
	}
    }
    for (i=0; i < 4; i++) {
	if (var_file.domain_display_units[i][0] == '\0') 
	    strcpy(var_file.domain_display_units[i],
		   var_file.domain_display_units_orig[i]);
	strcpy(domain_display_units+i*len4, var_file.domain_display_units[i]);
    }
    if (!*temp && var_file_old.nvars > 0) free(var_file_old.vars);
    /*
       Handle case where the "data set" is spread over multiple file.
       File name format: prefix.value.suffix or
                         prefix_value_suffix
       */
    getivar_("multiple_read", &file_dim, &i,13);
    if ((var_file.file_dim=file_dim) > -1) {
	char cvar[32], *p, *q, file_prefix[256], file_suffix[256];
	int length, flength;
	float file_coordinate_trans_();

	dmin[var_file.file_dim] =
	    file_coordinate_trans_(dmin+var_file.file_dim, &var_file.file_dim,
				   &var_file.id);
	sprintf(cvar, "%d", (int)dmin[var_file.file_dim]);
	length = strlen(cvar);
	flength = strlen(name);
	if ((p=strstr(name, cvar))) {
	    /*
	       Make sure we have the last one.
	       */
	    while ((p+length<name+flength) && (q=strstr(p+length,cvar))) p=q;
	    /*
	      Look for leading zeroes.
	      */
	    while (p > name && *(p-1) == '0') {
		p--;
		length++;
	    }
	    /*
	       Filename starts with string.
	       */
	    if (p == name || *(p-1) == '/') {
		if (p != name) strncpy(file_prefix, name, p-name);
		file_prefix[p-name] = '\0';
		if (name+flength == p+length) file_suffix[0] = '\0';
		else if (*(p+length) == '.' || *(p+length) == '_')
		    strcpy(file_suffix, p+length);
		else var_file.file_dim = -1;
	    }
	    /*
	       Filename ends with string.
	       */
	    else if (name+flength == p+length) {
		if (*(p-1) == '.' || *(p-1) == '_') {
		    strncpy(file_prefix, name, p-name);
		    file_prefix[p-name] = '\0';
		    file_suffix[0] = '\0';
		}
		else var_file.file_dim = -1;
	    }
	    /*
	       String in middle of name.
	       */
	    else if ((*(p-1) == '.' || *(p-1) == '_') &&
		     (*(p+length) == '.' || *(p+length) == '_')) {
		strncpy(file_prefix, name, p-name);
		file_prefix[p-name] = '\0';
		strcpy(file_suffix, p+length);
	    }
	    else var_file.file_dim = -1;
	}
	/*
	   If the file name was correct, find all others with the same
	   prefix and suffix.
	   */
	if (var_file.file_dim != -1) {
	    float *values = (float *) malloc(sizeof(float));
	    float zero = 0.0;
	    int plen = strlen(file_prefix);
	    int slen = strlen(file_suffix);
	    int nvalues=0, one=1, start, end, mr_start, mr_end, mr_flag;
	    FILE *list;
	    char command[256];

	    var_file.file_names = (char *) malloc(256);
	    getivar_("multiple_read_flag", &mr_flag, &i, 18);
	    if (mr_flag == 0) {
		getivar_("multiple_read_start", &mr_start, &i, 19);
		getivar_("multiple_read_end", &mr_end, &i, 17);
		if (mr_start > mr_end) {
		    start = mr_end;
		    end = mr_start;
		}
		else {
		    start = mr_start;
		    end = mr_end;
		}
	    }
	    sprintf(command, "/bin/ls %s*%s", file_prefix,
		    file_suffix);
	    list = popen(command, "r");
	    while (fgets(name, 256, list)) {
		char *c = name+plen;
		int nlen = strlen(name)-1;
		int v = 0, good = 1, neg=1;

		name[nlen] = '\0';
		if (*c == '-') {
		    neg = -1;
		    c++;
		}
		for (; good && c < name+nlen-slen; c++) {
		    if (*c < '0' || *c > '9') good=0;
		    v = v*10 + *c - '0';
		}
		if (!good) continue;
		v *= neg;
		if (mr_flag == 0 && (v < start || v > end)) continue;
		values = (float *) realloc(values, ++nvalues * sizeof(float));
		var_file.file_names = (char *) realloc(var_file.file_names,
						       nvalues*256);
		for (i=0; i < nvalues-1 && values[i] < v; i++);
		for (j=i; j < nvalues-1; j++) {
		    values[j+1] = values[j];
		    strcpy(var_file.file_names+256*(j+1),
			   var_file.file_names+256*j);
		}
		values[i] = v;
		strcpy(var_file.file_names+256*i, name);
	    }
	    pclose(list);
	    if (mr_flag < 0 || mr_flag == 0 && mr_start > mr_end)
		for (i=0; i < (int) (nvalues/2); i++) {
		    float t = values[i];

		    values[i] = values[nvalues-1-i];
		    values[nvalues-1-i] = t;
		    strcpy(name, var_file.file_names+256*i);
		    strcpy(var_file.file_names+256*i,
			   var_file.file_names+256*(nvalues-1-i));
		    strcpy(var_file.file_names+256*(nvalues-1-i), name);
		}
	    /*
	       Create new variable and adjust domain limits.
	       */
	    if (*(label+var_file.file_dim*len4) == '\0') {
		var_file.file_dimname[0], dname[var_file.file_dim];
		var_file.file_dimname[1] = '\0';
	    }
	    else
		strcpy(var_file.file_dimname, label+var_file.file_dim*len4);
	    dmin[var_file.file_dim] = values[0];
	    dmax[var_file.file_dim] = values[nvalues-1];
	    if (*temp) {
		cdf_temp.n = nvalues;
		cdf_temp.values = values;
	    }
	    else {
		/*
		  If this variable exists, get rid of it.
		  */
		if ((i=getvid_(var_file.file_dimname,
			       strlen(var_file.file_dimname))) >= 0) {
		    free(var_file.vars[i].values);
		    var_file.vars[i].name[0] = '\0';
		    var_file.vars[i].button_name[0] = '\0';
		}
		add_field_(&values, &var_file.file_varid,
			   var_file.file_dimname,
			   &one, &nvalues, &zero, &values,
			   &values+nvalues-1,
			   &zero, domain_units+var_file.file_dim*len2,
			   domain_display_units+var_file.file_dim*len3,
			   var_file.file_dimname,
			   strlen(var_file.file_dimname), len2, len3,
			   strlen(var_file.file_dimname));
		set_button_name(var_file.file_dimname, "");
		keep_var_(&var_file.file_varid);
		cdf_dimvar_.size[var_file.file_dim] = nvalues;
		cdf_dimvar_.data[var_file.file_dim] = values;
		sprintf(name, "%s*%s", file_prefix, file_suffix);
		setavar_("datfil", name, &i, 6, strlen(name));
	    }
	}
    }
    if (*temp) {
	i = var_file.id;
	ncclose(var_file.id);
	free(var_file.dims);
	for (i=0; i < var_file.nvars; i++) {
	    if(var_file.vars[i].dims != (vardim_info *)0)
		free (var_file.vars[i].dims);
	    if (var_file.vars[i].values != (void *)0)
		free(var_file.vars[i].values);
	}
	if (var_file.file_dim > -1) free(var_file.file_names);
	if (var_file.nvars > 0) free(var_file.vars);
	var_file = var_file_old;
	return(i);
    }
    setivar_("multiple_read_real", &var_file.file_dim, &i, 18);
    return (var_file.id);
}
