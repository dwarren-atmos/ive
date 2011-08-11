/****************************************************************************
$Log: cdfinfo.h,v $
Revision 1.14  2003/12/24 18:50:07  davidive
Now if someone calls force_change_units in new_file - it remains thorugh
reading of data from the file. We guess that if they force the units, they
must know what they are doing -wrf.

Revision 1.13  2002/05/22 17:56:35  davidive
First working version of trajectories colored by alternative fields
There is not a complete interface yet!!!

Revision 1.12  2000/07/20 20:12:02  harry
Allow for a global missing data attribute.

Revision 1.11  1997/03/05 16:58:02  harry
Make MULTIPLE_READ more flexible with the file naming conventions.

Revision 1.10  1997/02/07 18:39:01  harry
Remove PARTIAL_READ and expand MULTIPLE_READ.

Revision 1.9  1997/01/17 20:38:09  harry
First changes for MULTIPLE_READ and PARTIAL_READ.

Revision 1.8  1996/11/06 20:54:49  harry
Maintain domain units across different data files.

Revision 1.7  1996/03/05 18:21:27  harry
Isolate the use of "long" for dimension sizes to the netcdf routines.
Everywhere else we will use "int" or "integer".  Also change use of
long to nclong as a netCDF type as required by netCDF version 2.4.

Revision 1.6  1995/11/20 17:54:07  harry
Add routine ncattgetf to return the value of an attribute as a float no
matter how it appears in the netCDF file.

 * Revision 1.5  1993/11/29  19:38:31  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.4  1993/10/04  22:19:57  warren
 * added number of unlabeled lines to attributes.
 *
 * Revision 1.3  1993/08/18  15:46:43  harry
 * Change to new transform routines.
 *
 * Revision 1.2  1993/07/09  16:35:49  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:04  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.7  1993/02/23  18:53:17  harry
 * Add missing ;
 *
 * Revision 2.6  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 2.5  1993/02/16  20:22:31  warren
 * added stuff for malloc debug library
 *
 * Revision 2.4  1993/01/04  18:58:04  warren
 * fixed to only keep one real copy of display units
 *
 * Revision 2.3  1993/01/01  00:49:02  warren
 * added more attributes
 *
 * Revision 2.2  1992/12/29  20:29:17  warren
 * add stuff for marking vars as from file and temporary as well as added
 * property list for vars.
 *
 * Revision 2.1  1992/09/24  23:09:05  harry
 * Keep track of original display units for variables.
 *
 * Revision 2.0  1992/07/01  21:12:16  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/05/29  20:20:43  millerp
 * Added variables for units.
 *
 * Revision 1.1  1992/04/21  00:11:26  millerp
 * Initial revision
 *
 * Revision 1.101  1992/04/03  00:38:58  harry
 * Add dimension id into variable structure.
 *
 * Revision 1.100  1992/03/27  22:34:11  harry
 * Released version rereving to 1.100
 *
 * Revision 1.5  1991/12/12  18:50:33  harry
 * Change vardim_info to store dimension size instead of id.
 *
 * Revision 1.4  1991/12/11  17:44:02  harry
 * Remove domain limits
 *
 * Revision 1.3  1991/12/10  22:45:13  harry
 * Add vardim_info structure.
 *
 * Revision 1.2  1991/06/12  21:24:58  warren
 * put $Id: cdfinfo.h,v 1.14 2003/12/24 18:50:07 davidive Exp $ inside cpomments istead of a staic char *.
 *
 * Revision 1.1  1991/06/12  20:19:52  warren
 * Initial revision
 *
****************************************************************************/
/*$Id: cdfinfo.h,v 1.14 2003/12/24 18:50:07 davidive Exp $*/

#ifndef _NETCDF_
#include <netcdf.h>
#include <udunits.h>
#endif


/*
  Structure to hold dimension information.
  */

typedef struct {
    char name[NC_MAX_NAME+1];	/* Dimension name */
    int size;			/* Dimension size */
} dim_info;

typedef struct {
    int id;			/* Dimension id */
    int size;			/* Dimension size */
    float stagger;		/* Stagger for this dimension */
    float min, max;		/* Min/max values for this dimension */
} vardim_info;

/*
  Structure to hold IVE variable attributes.
  */

typedef struct {
    float cont_int;                /* contour interval */
    float cont_values[40];         /* specified contour levels */
    float major_tic;               /* major tick mark interval */
    float minor_tic;               /* minor tick mark interval */
    float lmax;                    /* max value for line plots */
    float lmin;                    /* min value for line plots */
    int no_zero;                   /* true false no zero contour */
    int lin;                       /* true lin false log */
    int num_vals;		   /* number of contour values
				      negative => use cont_int */
    int num_labs;		   /* number of labeled lines */
    int do_labs;                   /* labeled lines y/n*/
    char *disp_units;              /* display unit name */
             /*added for trajectory tracking*/
    float traj_val;               /*Value at which the colors are centered*/
    float traj_step;              /*step size for trajectory color change*/
  }var_attributes;

/*
  Structure to hold variable information.
  */

typedef struct {
  int  temporary;               /* true if var should be garbage collected*/
  char name[NC_MAX_NAME+1];	  /* Variable name */
  char button_name[NC_MAX_NAME+1];/* Variable button name */
  nc_type datatype;		  /* Variable type */
  int ndims;			  /* Number of dimensions */
  int natts;			  /* Number of attributes */
  float misdat;		  /* Missing data */
  vardim_info *dims;		  /* Per dimension information */
  char data_units[NC_MAX_NAME+1] ;
  char data_display_units[NC_MAX_NAME+1] ;
  char data_display_units_orig[NC_MAX_NAME+1] ;
  int in_file;                /* is it from a file 1 or calc 0 */
  float *values;		/* Data values - set when read, 0 otherwise */
  var_attributes vatts;       /* IVE var attribute structure */
  int units_forced;          /* For those who force change their units so
				read_var doesn't undo it - aka wrf model*/
} var_info;


typedef struct {
    int id;			/* NetCDF ID */
    int ndims;			/* Number of dimensions */
    int nvars;			/* Number of variables */
    int ngatts;			/* Number of global attributes */
    int recdim;			/* ID of unlimited dimension */
    char domain_display_units[4][NC_MAX_NAME+1] ;
    char domain_display_units_orig[4][NC_MAX_NAME+1] ;
    dim_info *dims;		/* Dimensions */
    var_info *vars;		/* Variables */
    int file_dim;		/* Dimension for file tag on mult. file sets */
    char *file_names;		/* File names for file_dim */
    char file_dimname[256];	/* Name for file_dim */
    int file_varid;		/* Varid for file_dim */
    float misdat;		/* Global missing data attribute. */
} cdf_info;

void *read_var();
int open_var();
int ncattgetf();

