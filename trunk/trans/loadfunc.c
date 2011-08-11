/*
 * This routine loads in the transform routines.  It also contains the
 * default transforms.
 *
 * $Log: loadfunc.c,v $
 * Revision 1.52  2007-01-09 18:49:57  warren
 * call read_file correctly in new_field_trans_  (missing has to be a float passwd by
 * reference instead of a pointer)
 *
 * Revision 1.51  2006/02/14 20:15:02  warren
 * Dereference return of getvarid_ and use ceilf and floorf to make linux happy.
 *
 * Revision 1.50  2005/09/06 20:22:21  warren
 * Move updates into current stream
 *
 * Revision 1.49  2003/12/22 23:14:53  warren
 * fixed loadfunc from released version
 *
 * Revision 1.48.2.1  2003/12/22 23:13:23  warren
 * readvar takes a vid not a string, so fixed loadfunc to call correctly
 * CVS----------------------------------------------------------------------
 *
 * Revision 1.48  2002/12/27 18:45:15  warren
 * Added physuv_2_lonlatuv_trans for dealing with rotated data in vector plots
 *  -from zurich-
 *
 * Revision 1.47  2001/05/23 19:39:01  harryive
 * new_field should call read_var_, not getvar_.
 *
 * Revision 1.46  2001/02/13 00:26:19  harry
 * Fix problem in default user transform slicer for 1d.
 *
 * Revision 1.45  1999/12/22 00:33:36  warren
 * Fixed Dales crashing bug when changing from point slicer to normal. Also
 * have some ifdef linuxes in there in case we ever find a fortran compiler.
 *
 * Revision 1.44  1998/09/03 19:19:52  harry
 * Add in 2d line plots - correct bugs in other averaging.
 *
 * Revision 1.43  1998/06/12 22:04:18  harry
 * Fix bugs in 1d averaging.
 *
 * Revision 1.42  1998/01/07 15:58:15  warren
 * fix alpha to ignore underflows correctly
 *
 * Revision 1.41  1997/12/18 20:26:04  harry
 * Fix pointer problem in derivative.  Add variable field_delta.
 *
 * Revision 1.40  1997/10/22 19:35:18  davidive
 * Turned the particle movement part of trajectories into a user transform
 * and added unit checking to the default transform.
 *
 * Revision 1.39  1997/10/17 19:30:52  harry
 * Fix default lonlat_2_phys transform to handle longitudes above 180.
 *
 * Revision 1.38  1997/07/25 21:00:57  harry
 * No code changed, changes in comments only.
 *
 * Revision 1.37  1997/07/22 19:50:51  harry
 * Fix multiple reading to allow user to specify coordinate transform.
 *
 * Revision 1.36  1997/04/11 18:48:35  warren
 * Fixed for new sgi OS.
 *
 * Revision 1.35  1997/02/21 20:02:26  harry
 * Fix vector interval to be floating point and have values less than one.
 * Handling of vector interval is now in the slicer.
 *
 * Revision 1.34  1996/10/25 17:34:52  harry
 * Add changes for SGI IRIX.
 *
 * Revision 1.33  1996/10/11 16:42:26  harry
 * Fixes for SunOS.
 *
 * Revision 1.32  1996/10/11  01:13:28  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 * Revision 1.31  1996/07/18 20:22:28  harry
 * Add the 3d slicer transform.
 *
 * Revision 1.30  1996/04/25 16:26:02  harry
 * Add more comments to calc_field.
 *
 * Revision 1.29  1996/03/05 18:22:19  harry
 * Isolate the use of "long" for dimension sizes to the netcdf routines.
 * Everywhere else we will use "int" or "integer".  Also change use of
 * long to nclong as a netCDF type as required by netCDF version 2.4.
 *
 * Revision 1.28  1996/01/18 17:44:24  warren
 * changed _heading_ to heading_ for solaris and osf/1
 *
 * Revision 1.27  1995/12/18 00:23:07  warren
 * Ultrix doesn't like multiple declarations
 *
 * Revision 1.26  1995/12/08 17:12:54  harry
 * Add derivative computation for default transform.
 *
 * Revision 1.25  1995/10/18 19:24:29  warren
 * Continued from last commit which hung up sending mail.
 *
 * Revision 1.24  1995/10/05  21:59:24  harry
 * Correct some comments - no code changes.
 *
 * Revision 1.23  1995/10/04  22:10:16  harry
 * Add new netCDF attributes and specification of staggering.  Add
 * changes to slicer and plot labels to reflect user specified labels.
 *
 * Revision 1.22  1995/09/05  20:31:56  harry
 * Modifications to transforms for new slicer.
 *
 * Revision 1.21  1995/08/24  20:09:13  harry
 * Add David Bresch and Chris Schaer's heading transform routine.
 *
 * Revision 1.20  1995/05/26  15:18:27  harry
 * Allow default transforms to limit time slider if possible.
 *
 * Revision 1.19  1994/09/12  17:04:33  harry
 * Add code to check for incorrect binaries types and print an error message.
 *
 * Revision 1.18  1994/06/23  21:33:49  harry
 * Add ifdef for Solaris dynamic loading, which is the same as the alpha.
 *
 * Revision 1.17  1994/05/26  17:48:23  warren
 * last sol2 changes
 *
 * Revision 1.16  1994/02/17  21:37:58  harry
 * Change calls to read_var to getvar and add new parameter dim_names.
 *
 * Revision 1.15  1993/12/16  22:59:30  harry
 * Change in comment.
 *
 * Revision 1.14  1993/12/15  17:59:19  harry
 * Minor change to a comment line.
 *
 * Revision 1.13  1993/12/09  20:15:20  harry
 * Correct Alpha port and add default_map function.
 *
 * Revision 1.12  1993/12/07  21:03:32  warren
 * Final changes for the alpha (alpha doesn't preappend _ on symbols in shared
 * libraries)
 *
 * Revision 1.11  1993/11/29  19:40:17  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.10  1993/11/24  19:18:38  harry
 * Change typo in comments.
 *
 * Revision 1.9  1993/11/24  19:01:57  harry
 * Put in additional comments as suggested by Dale.  No code changes
 * (other than a variable rename).
 *
 * Revision 1.8  1993/11/03  23:48:33  warren
 * Added loadfunc for alpha
 *
 * Revision 1.7  1993/09/24  17:01:38  warren
 * fixed typo.
 *
 * Revision 1.6  1993/09/23  23:06:50  harry
 * Correct error in calc_field call.
 *
 * Revision 1.5  1993/09/23  23:02:30  harry
 * Include dimension names in call to calc_field.
 *
 * Revision 1.4  1993/08/18  15:48:20  harry
 * Change to new transform routines.
 *
 * Revision 1.3  1993/06/28  23:15:52  yuri
 * Harry said undo it back.
 *
 * Revision 1.2  1993/06/28  22:04:03  yuri
 * Corrected case when vxter_ doesn't exist.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:27  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.9  1993/03/12  16:28:59  davidb
 * added routines ph2ll and ll2ph
 *
 * Revision 2.8  1993/02/22  21:16:30  warren
 * added ifdef for memdbg
 *
 * Revision 2.7  1992/09/08  21:51:17  millerp
 * Remove iwi, jwi from hxter argument list.
 *
 * Revision 2.6  1992/07/22  22:59:39  millerp
 * Change malloc to valloc.
 *
***********************************************************************/
static char ident[] = "$Id: loadfunc.c,v 1.52 2007-01-09 18:49:57 warren Exp $" ;
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <malloc.h>
#include <unistd.h>

#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif
#ifdef __osf__
#define memalign(size1,size2) malloc(size2)
#endif
#if (LSB == 5)
#include <lsb5/math.h>
#elif (LSB == 4)
#include <lsb4/math.h>
#elif (LSB == 3)
#include <lsb3/math.h>
#elif (LSB == 2)
#include <lsb2/math.h>
#elif (LSB == 1)
#include <lsb1/math.h>
#else
#include <math.h>
#endif

#include <string.h>
#include <strings.h>
#include <stdio.h>

#ifdef ultrix
#include <sys/exec.h>
#include <nlist.h>
#else /* BSD */
#ifndef sparc
#include <a.out.h>
#endif
#endif /* mips */
/* sprintf string for linker system call and 
 * alignment required by /bin/ld -A */
#ifdef __alpha
#include <signal.h>
#endif
#include <netcdf.h>
#include <udunits.h>


#ifdef mips
#  ifdef ultrix
#    define LINKBUF "ld -S -G 0 -N -A %s -T %x %s -o %s %s "
#  else /* RISC/OS */
#    define LINKBUF "/bsd43/bin/ld -systype bsd43 -G 0 -N -A %s -T %x %s -e %s -o %s %s -lc" 
#  endif /* ultrix */
#  define ALIGN SCNROUND

#else /* normal BSD */

#  define LINKBUF "/bin/ld -N -x -A %s -T %x %s -e _%s -o %s %s -lc"
#  define ALIGN 1

#endif /* mips */

#define ROUND(x,s) ((((x)-1) & ~((s)-1)) + (s))

#ifdef STATIC
#define NEW_FILE new_file_
#define NEW_FIELD new_field_
#define INDEX_2_PHYS index_2_phys_
#define PHYS_2_INDEX phys_2_index_
#define HORIZ_TER horiz_ter_
#define VERT_TER vert_ter_
#define PHYS_2_LONLAT phys_2_lonlat_
#define LONLAT_2_PHYS lonlat_2_phys_
#define DEFAULT_MAP default_map_
#define DATA_SLICER_1D data_slicer_1d_
#define DATA_SLICER_2D data_slicer_2d_
#define DATA_SLICER_3D data_slicer_3d_
#define CALC_FIELD calc_field_
#define RUN_SUB run_sub_
#define HEADING heading_
#define FILE_COORD file_coordinate_
#define MOVE_PARCEL move_parcel_
#define EXISTS(x) 1
#define PHYSUV_2_LONLATUV_TRANS physuv_2_lonlatuv_trans_
#define DIMS_PLACEMENT dims_placement_
#else
#define NEW_FILE (*new_file)
#define NEW_FIELD (*new_field)
#define INDEX_2_PHYS (*index_2_phys)
#define PHYS_2_INDEX (*phys_2_index)
#define HORIZ_TER (*horiz_ter)
#define VERT_TER (*vert_ter)
#define PHYS_2_LONLAT (*phys_2_lonlat)
#define LONLAT_2_PHYS (*lonlat_2_phys)
#define DEFAULT_MAP (*default_map)
#define DATA_SLICER_1D (*data_slicer_1d)
#define DATA_SLICER_2D (*data_slicer_2d)
#define DATA_SLICER_3D (*data_slicer_3d)
#define CALC_FIELD (*calc_field)
#define RUN_SUB (*run_sub)
#define HEADING (*heading)
#define FILE_COORD (*file_coordinate)
#define MOVE_PARCEL (*move_parcel)
#define EXISTS(x) x
#define PHYSUV_2_LONLATUV_TRANS (*physuv_2_lonlatuv_trans)
#define DIMS_PLACEMENT (*dims_placement)
#endif

int NEW_FILE(), NEW_FIELD(), INDEX_2_PHYS(), PHYS_2_INDEX();
int HORIZ_TER(), VERT_TER(), PHYS_2_LONLAT(), LONLAT_2_PHYS();
int DEFAULT_MAP();
int HEADING();
float *DATA_SLICER_1D(), *DATA_SLICER_2D(), *DATA_SLICER_3D(), *CALC_FIELD();
float *CALC_FIELD(), FILE_COORD();
void  *MOVE_PARCEL(), *PHYSUV_2_LONLATUV_TRANS(), *RUN_SUB(), *DIMS_PLACEMENT();

/* BEGIN INTERFACE ROUTINES AND DEFAULT TRANSFORMS */

extern void getrarr_(),getivar_(),setrarr_(),getiarr_(),make_help_widget_(),getavar_(),getaarr_();
  extern int getvid_();
extern void *read_var_();
/*
 * Common variables
 */

static float *coord[4],		/* An array containing pointers to arrays
				   holding the "dimension variables".  The
				   use of dimension variables is optional,
				   but allows one to specify irregular grid
				   intervals along each spatial coordinate. */
             inmax[4],		/* Maximum array indices of the data. */
             phmin[4],		/* Minimum physical_space locations for
				   each dimension */
             phmax[4],		/* Maximum physical_space locations for
				   each dimension */
             delta[4];		/* If non-zero => physical distance between
				   any two array indicies is constant and delta
				   is the value.  If zero => physical distance
				   is not constant, transforms use coord to
				   do translations */

/*
 * new_file_ : This routine is called every time a file is read in.
 * The routine can be used to read other information from the open
 * netCDF file.  It also can provide an array of coord values that limit the
 * values the window sliders can be set to.
 *
 * Arguments:
 * ncid	 	int *	  input	 Id of currently opened netCDF file.
 * exact_coord	float **  output Pointer to array of discrtet coord values
 *				 that each window slider should use.
 * n		int *	  output Number of values in exact_coord.  If this
 *				 is 0 => ignore values in exact_coord
 *				 => no restrictions on window slider.
 * coord_dep	int[4][4] output States the functional dependencies
 *                               between the coordinates, 1 => dependent,
 *				 0 => independent e.g. if z is a function of
 *				 x, y and z, then coord_dep[2][0],
 *				 coord_dep[2][1] and coord_dep[2][2]
 *				 will be one.  A coordinate is always 
 *				 assumed to be dependent on itself.
 */

void
new_file_trans_(ncid, exact_coord, n, coord_dep)
int *ncid, n[4], coord_dep[4][4];
float *exact_coord[4];

{
    int i, j;

    for (i=0; i < 4; ++i) 
	for (j=0; j < 4; ++j) coord_dep[i][j] = (i==j);
    if(EXISTS(new_file)) {
	NEW_FILE(ncid, exact_coord, n, coord_dep);
	return;
    }
{
    /*
       Structure cdf_dimvar_ (common block cdf_dimvar) is set up
       by IVE to contain the values of a variable with the same name as the
       dimension of the largest variable in the netCDF file.  If the
       size of the domain is set by attributes, or if a dimension
       variable does not exist, size will be zero, and data will be NULL.
       */

    extern struct cdf_dimvar_ {
	float *data[4];
	int size[4];
    } cdf_dimvar_;

    for (i=0; i < 4; ++i) {
	exact_coord[i] = cdf_dimvar_.data[i];
	n[i] = cdf_dimvar_.size[i];
    }
}
}

/*
 * new_field_ : This routine is called every time a new field is set.
 * The routine can be used to store necessary information to perform
 * the other transformations.  It also can use used to reorder the
 * data array.
 * 
 * Note: before IVE calls new_field it has already called
 * getvar to read in the data for the new field.  As a
 * result of this call, IVE will have also read in
 * ndims, dims, stagger, phmin, phmax, missing, data_units
 * and display_units---provided this information appears
 * in the netCDF.  The transform routines can access this
 * data by calling getrarr, getlvar or getaarr.

 * Arguments:
 * name		char *	input	The name of the current field.
 * field	float *	input	Values of "field".
 * inmax1	int *	input	Size of dimensions in "Fortran"
 * - inmax4			order (i.e., A(inmax1,...inmax4)
 *				will hold the data with the first
 *				index varying most rapidly)
 * len		int	input	Length of name in characters.
 */

void
new_field_trans_(name, field, inmax1, inmax2, inmax3, inmax4, len)
char *name;
float **field;
int *inmax1, *inmax2, *inmax3, *inmax4;
long len;

{
    if (EXISTS(new_field)) {
	NEW_FIELD(name, field, inmax1, inmax2, inmax3, inmax4, len);
	return;
    }
{
    char dnames[4][MAX_NC_NAME],	/* Names of "field" dimensions */
         data_units[81], display_units[81], dim_names[4][81]; /* For getvar_ */
    float missing, stagger[4];		/* For getvar_ call */
    int zero=0, four=4,			/* Constants for Fortran calls */
        i, j,				/* Scratch */
        min[4], max[4], nd,     	/* For getvar_ call */
        n,				/* Variable number for the dimension */
        ndims,				/* Number of dimensions in "field" */
        flag,				/* For getvar_ call */
	dims[4];			/* For getvar_ call */
    int error;   			/* Error flag for getrarr_ */
    int varid;
    /*
       Save the physical and index limits for use in later calculations
       by other transform routines.
       */
    getrarr_("phmin", phmin, &four, &error, 5);
    getrarr_("phmax", phmax, &four, &error, 5);
    getrarr_("grid_delta", delta, &four, &error, 10);
    inmax[0] = *inmax1;
    inmax[1] = *inmax2;
    inmax[2] = *inmax3;
    inmax[3] = *inmax4;
    /*
       Get the names of the dimensions for this field.
       */
    getaarr_("data_dim_names", dnames, &four, &error, 14, MAX_NC_NAME);
    getivar_("num_dims", &ndims, &error, 8);
    /*
       Get values (if they exist) in the 1D array associated with each
       dimension to see which dimensions are evenly spaced.
       delta is zero if they are not.
       */
    for (i=0; i < ndims; ++i) {
        /*
           Check that the data array depth exceeds one along this coordinate
           */
	if (inmax[i] != 1) {
            /*
               delta is the distance between grid point in this dimension if
               the points are evenly spaced.
               */
	    if(delta[i] == 0.0) delta[i] = (phmax[i]-phmin[i])/(inmax[i]-1);
            /*
               Get the memory address of the values of the variable with
               the same name as the dimension.
               If such a variable does not exist, coord[i] == NULL.
               */
	    flag = 0;
//	    printf("i=%d varname=%s vid=%d\n",i,dnames[i],getvid_(dnames[i],strlen(dnames[i])));
	    varid=getvid_(dnames[i],strlen(dnames[i]));
            if ((coord[i] =
                 (float *)read_var_(&varid,  &nd, dims, stagger,  min, max, &missing,
				    data_units, display_units, 
				    dim_names,
				    80, 80, 80))
                != NULL) {
                /*
                   Check to see if the dimension is evenly spaced by getting
                   the first value and then comparing the other values
                   to the value calculated using the delta computed above.
                   If the dimension is not evenly spaced, set delta to 0.
                   */
		for(j=1; j < inmax[i]; ++j) {
		    if (coord[i][0]+j*delta[i] != coord[i][j]) {
			delta[i] = 0.;
			break;
		    }
		}
	    }
	}
    }
    (void) setrarr_("field_delta", delta, &four, &error, 11);
}
}

/*
 * index_2_phys_ : This routine translates array indices into physical
 * coordinates.
 *
 * Arguments:
 * phys		float *	output	Array of physical coordinates.
 * index	float *	input	Array of indicies.
 * flag		int *	input	Flags that indicate which coordinates
 *				to compute.  If iflag[i] = 1 index[j][i]
 *				will be converted to phys[j][i], otherwise
 *				the value of phys[j][i] is not changes.
 *				If iflag[i] comes back as -1 => insufficient
 *				information to compute this coordinate.
 * ndims	int *	input	Number of dimensions in each point.
 * npts		int *	input	Number of points to convert.
 */

void
index_2_phys_trans_(phys, index, flag, ndims, npts)
int *flag, *ndims, *npts;
float *phys, *index;

{
    if (EXISTS(index_2_phys)) {
#if defined(__alpha)
	      signal(SIGFPE, SIG_IGN);
#endif
	INDEX_2_PHYS(phys, index, flag, ndims, npts);
	return;
    }
{
    int i, j, k, idx;

    for (idx=0, i=0; i < *npts; i++) {
	for(j=0; j<*ndims; j++, idx++) {
	    if(flag[j]) {
                /*
                   Trivial case where dimension does not exist (depth of
                   array is unity along this dimension)
                   */
		if (inmax[j] == 1) phys[idx] = phmin[j];
                /*
                   Case where interpolation must be performed between the grid
                   point locations given in a 1D "dimension variable" array.
                   */
		else if (delta[j] == 0.) {
		    k = index[idx];
		    if (k < inmax[j])
			phys[idx] = coord[j][k-1] + 
			    (index[idx]-k)*(coord[j][k]-coord[j][k-1]);
		    else
			phys[idx] = phmax[j];
		}
		/*
		  Case of evenly spaced physical coord.
		  */
		else phys[idx] = phmin[j] + (index[idx]-1)*delta[j];
	    }
	}
    }
}
}

/*
 * phys_2_index_ : This routine translates physical coordinates into 
 * array indices.
 *
 * Arguments:
 * phys		float *	input	Array of physical coordinates.
 * index	float *	output	Array of indicies.  A negative index =>
 *				this point gets the missing data value
 *				(e.g. is under the terrain).
 * flag		int *	input	Flags that indicate which coordinates
 *				to compute.  If iflag[i] = 1 phys[j][i]
 *				will be converted to index[j][i], otherwise
 *				the value of index[j][i] is not changes.
 *				If iflag[i] comes back as -1 => insufficient
 *				information to compute this coordinate.
 *	ndims	int *	input	Number of dimensions in each point.
 *	npts	int *	input	Number of points to convert.
 */

void phys_2_index_trans_(phys, index, flag, ndims, npts)
int *flag, *ndims, *npts;
float *phys, *index;

{
#if defined(__alpha)
	      signal(SIGFPE, SIG_IGN);
#endif
    if (EXISTS(phys_2_index)) {
	PHYS_2_INDEX(phys, index, flag, ndims, npts);
	return;
    }
{
    int i, j, k, idx;
    
    for (idx=0, i=0; i < *npts; i++) {
	for(j=0; j<*ndims; j++, idx++) {
	    if(flag[j]) {
		/*
                   Trivial case where dimension does not exist (depth of
                   array is unity along this dimension)
                   */
		if (inmax[j] == 1) index[idx] = 1.;
                /*
                   Case where interpolation must be performed between the grid
                   point locations given in a 1D "dimension variable" array.
                   */
		else if (delta[j] == 0.) {
		    /*
		      Physical coord. increasing with index.
		      */
		    if (phmin[j] < phmax[j]) {
			if (phys[idx] <= phmin[j]) index[idx] = 1;
			else if (phys[idx] >= phmax[j])
			    index[idx] = inmax[j];
			else {
			    for (k=1; k < inmax[j] &&
				 phys[idx] > coord[j][k]; ++k);
			    index[idx] =(float)k - (phys[idx]-coord[j][k])/
				(coord[j][k-1]-coord[j][k]) + 1.;
			}
		    }
		    /*
		      Physical coord. decreasing with index.
		      */
		    else {
			if (phys[idx] >= phmin[j]) index[idx] = 1;
			else if (phys[idx] <= phmax[j])
			    index[idx] = inmax[j];
			else {
			    for (k=1; k < inmax[j] &&
				 phys[idx] < coord[j][k]; ++k);
			    index[idx] =(float)k - (phys[idx]-coord[j][k])/
				(coord[j][k-1]-coord[j][k]) + 1.;
			}
		    }
		}
		/*
		  Case of evenly spaced physical coord.
		  */
		else index[idx] = (phys[idx]-phmin[j])/delta[j] + 1.;
	    }
	}
    }
}
}

/*
 * horiz_ter_ : This routine fills an array with terrain heights in the
 * windowed domain for horizontal cross sections.
 *
 * Arguments:
 *	topo	float *		Array containing the terrain heights (output).
 *	nxw	int *		Number of x points.
 *	nyw	int *		Number of y points.
 *	stagi	float *		Grid staggering along x dimension.
 *	stagj	float *		Grid staggering along y dimension.
 *	zero	float *		Minimum terrain height.
 *	error	int *	        Error flag 0 => no errors (output).
 */

void
horiz_ter_trans_(topo, nxw, nyw, stagi, stagj, zero, error)
int *nxw, *nyw;
int *error;
float *topo, *stagi, *stagj, *zero;
{
    if (EXISTS(horiz_ter)) {
	HORIZ_TER(topo, nxw, nyw, stagi, stagj, zero, error);
	return;
    }
{
    int i;

    for (i=0; i < *nxw * *nyw; topo[i++] = *zero);
    *error = 0;
}
}

/*
 * vert_ter_ : This routine fills an array with terrain heights in the
 * windowed domain for vertical cross sections.
 *
 * Arguments:
 *	xter	float **	Pointer to array of x-locations for topo_ht
 *	yter	float **	Pointer to array of y-locations for topo_ht
 *	topo_ht float **	Pointer to array containing 
 *				the terrain heights.
 *	nter	int *		Number of points in topo_ht (output)
 *	pt1	float *		Minimum physical coordinates for slice.
 *	pt2	float *		Maximum physical coordinates for slice.
 *	zero	float *		Minimum terrain height.
 */

void
vert_ter_trans_(xter, yter, topo_ht, nter, pt1, pt2, zero)
int *nter;
float **xter, **yter, **topo_ht, *pt1, *pt2, *zero;
{
    if (EXISTS(vert_ter)) {
	VERT_TER(xter, yter, topo_ht, nter, pt1, pt2, zero);
	return;
    }
{
    int i;

    *nter = 2;
    *xter = (float *)malloc(2*sizeof(float));
    *yter = (float *)malloc(2*sizeof(float));
    *topo_ht = (float *)malloc(2*sizeof(float));
    (*xter)[0] = pt1[0];
    (*xter)[1] = pt2[0];
    (*yter)[0] = pt1[1];
    (*yter)[1] = pt2[1];
    (*topo_ht)[0] = (*topo_ht)[1] = *zero;
}
}

/*
 * phys_2_lonlat : This routine converts physical coordinates to longitude-
 * latitude coordinates.
 *
 * Arguments:
 *	x	float *		Physical x coordinate array.
 *	y	float *		Physical y coordinate array.
 *	lon	float *		Longitude array (output).
 *	lat	float *		Latitude array (output).
 *	npts	int *		Number of points to convert.
 */

void
phys_2_lonlat_trans_(x, y, lon, lat, npts)
float *x, *y, *lon, *lat;
int *npts;
{
    if (EXISTS(phys_2_lonlat)) {
	PHYS_2_LONLAT(x, y, lon, lat, npts);
	return;
    }
{
    int i;

    for (i=0; i < *npts; ++i) {
	lon[i] = x[i];
	lat[i] = y[i];
    }
}
}

/*
 * lonlat_2_phys : This routine converts longitude-latitude coordinates
 * to physical coordinates.
 *
 * Arguments:
 *	x	float *		Physical x coordinate array (output).
 *	y	float *		Physical y coordinate array (output).
 *	lon	float *		Longitude array.
 *	lat	float *		Latitude array.
 *	npts	int *		Number of points to convert.
 */

void
lonlat_2_phys_trans_(x, y, lon, lat, npts)
float *x, *y, *lon, *lat;
int *npts;
{
    if (EXISTS(lonlat_2_phys)) {
	LONLAT_2_PHYS(x, y, lon, lat, npts);
	return;
    }
{
    int i;

    for (i=0; i < *npts; ++i) {
	if (phmax[0] > 180 && lon[i] < phmin[0]) x[i] = lon[i]+360;
	else x[i] = lon[i];
	y[i] = lat[i];
    }
}
}

/*
 * default_map_ : This routine returns the map settings when the default
 * map is specified.
 *
 * Arguments:
 * proj		char *	output	EZMAP map projection.
 * plon		float *	output	Projection longitude.
 * plat		float *	output	Projection latitude.
 * rota		float * output	Projection rotation.
 * limit	char *	output	limit and plm1-4 are the arguments for
 * plm1		float *	output	  the MAPSET call which specifies the
 * plm2		float *	output	  rectagular portion of the u/v plane
 * plm3		float *	output	  to be drawn.
 * plm4		float *	output
 * exact_fit	int *	output	If 1 => the contour plot will fit exactly
 *				  in this map with no transform.  Thus IVE
 *				  will just call conpack within the map
 *				  window without calling phys_2_lonlat.
 * len1		int	input	Maximum length of proj in characters.
 * len2		int	input	Maximum length of limit in characters.
 */

void
default_map_trans_(proj, plon, plat, rota, limit, plm1, plm2, plm3, plm4,
                   exact_fit, len1, len2)
char *proj, *limit;
float *plon, *plat, *rota, *plm1, *plm2, *plm3, *plm4;
int *exact_fit;
long len1, len2;
{
    if (EXISTS(default_map)) {
	DEFAULT_MAP(proj, plon, plat, rota, limit, plm1, plm2, plm3, plm4,
		    exact_fit, len1, len2);
	return;
    }
{
    /*
       The default is the current window in a Cylindrical Equidistant
       Projection
       */
    strcpy(proj, "CE");
    *plon = 0.;
    *plat = 0.;
    *rota = 0.;
    /*
       Setting limit to CL tells IVE to clip to the current window using
       phys_2_lonlat.
       */
    strcpy(limit, "CL");
    *exact_fit = 0;
}
}

/*
 * data_slicer_1d_ : This routine returns the computational points at
 * which a 1d slice should be taken.
 *
 * Arguments:
 * endpt	float *	input	2 endpoints of the line in physical space
 * ni		int *	output	Number of points
 * da		int *	input	axis coordinate of averages
 * nda		int *	output	Number of points in the da-directions.
 */

float *
data_slicer_1d_trans_(endpt, ni, da, nda)

float *endpt;
int *ni, *da, *nda;
{
    if (EXISTS(data_slicer_1d)) {
	return(DATA_SLICER_1D(endpt, ni, da, nda));
    }
{
#ifndef MAX
#define MAX(x, y) ((x) > (y)? (x):(y))
#endif
#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif
    /*
       Let's do the obvious, just divide the slice evenly, using the
       number of computation points represented by the slice.
       */
    struct point4a {
	float v[4];
    } *pendpt = (struct point4a *) endpt, cendpt[2], *pslice, *slice, *p;
    int iflag[4], four=4, two=2, i, j, k, l, error, cpmin[4];
    float dv[4], cmin, cmax;

    for (i=0; i < 4; i++) iflag[i] = 1;
    phys_2_index_trans_((float *)endpt, (float *)cendpt, iflag, &four, &two);
    *ni = 1;
    for (i=0; i < 4; i++) 
	if (i != da[0]-1 && i != da[1]-1)
	    *ni += abs(cendpt[1].v[i]-cendpt[0].v[i]);
    for (i=0; i < 4; i++) dv[i] = (pendpt[1].v[i]-pendpt[0].v[i])/ (*ni - 1);
    if (*da == 0) {
	pslice = (struct point4a *) memalign(sizeof(struct point4a),
					   sizeof(struct point4a)**ni);
	slice = (struct point4a *) memalign(sizeof(struct point4a),
					    sizeof(struct point4a)**ni);
	p = pslice;
	for (i=0; i < *ni; i++, p++) {
	    for (j=0; j < 4; j++) p->v[j] = pendpt[0].v[j] + i*dv[j];
	}
	j = *ni;
    }
    /*
      One dimensional average.
    */
    else if (da[1] == 0) {
	float dda;

	dv[*da-1] = 0;
	getiarr_("cpmin",cpmin,&four,&error,5,4);
	if (cendpt[0].v[*da-1] < 0 ) cmin = cpmin[*da-1];
	else cmin = cendpt[0].v[*da-1];
	if (cendpt[1].v[*da-1] < 0 ) cmax = cpmin[*da-1];
	else cmax = cendpt[1].v[*da-1];
	if (cmax == cmin) *nda = 1;
	else {
	    *nda = ceilf(cmax) - floorf(cmin) + 1;
	    dda = (pendpt[1].v[*da-1] - pendpt[0].v[*da-1])/ (*nda - 1);
	}
	pslice = (struct point4a *) memalign(sizeof(struct point4a),
				     sizeof(struct point4a)*(*ni * *nda));
	slice = (struct point4a *) memalign(sizeof(struct point4a),
				    sizeof(struct point4a)*(*ni * *nda));
	p = pslice;
	for (i=0; i < *ni; i++) {
	    struct point4a *p0 = p;

	    for (j=0; j < 4; j++) p->v[j] = pendpt[0].v[j] + i*dv[j];
	    p++;
	    for (k=1; k < *nda; k++, p++) {
		memcpy(p, p0, sizeof(struct point4a));
		p->v[*da-1] = pendpt[0].v[*da-1] + k*dda;
	    }
	}
	j = *ni * *nda;
    }
    else {
	float da2, dda[2];

	dv[da[0]-1] = 0;
	dv[da[1]-1] = 0;
	getiarr_("cpmin",cpmin,&four,&error,5,4);
	for (i=0; i < 2; i++) {
	    if (cendpt[0].v[da[i]-1] < 0 ) cmin = cpmin[da[i]-1];
	    else cmin = cendpt[0].v[da[i]-1];
	    if (cendpt[1].v[da[i]-1] < 0 ) cmax = cpmin[da[i]-1];
	    else cmax = cendpt[1].v[da[i]-1];
	    if (cmax == cmin) nda[i] = 1;
	    else {
		nda[i] = ceilf(cmax) - floorf(cmin) + 1;
		dda[i] = (pendpt[1].v[da[i]-1] - pendpt[0].v[da[i]-1])
		    / (nda[i] - 1);
	    }
	}
	pslice = (struct point4a *) memalign(sizeof(struct point4a),
			    sizeof(struct point4a)*(*ni * nda[0] * nda[1]));
	slice = (struct point4a *) memalign(sizeof(struct point4a),
			    sizeof(struct point4a)*(*ni * nda[0] * nda[1]));
	p = pslice;
	for (i=0; i < *ni; i++) {
	    struct point4a *p0 = p;

	    for (j=0; j < 4; j++) p->v[j] = pendpt[0].v[j] + i*dv[j];
	    for (k=0; k < nda[1]; k++) {
		da2 = pendpt[0].v[da[1]-1] + k*dda[1];
		for (l=0; l < nda[0]; l++, p++) {
		    memcpy(p, p0, sizeof(struct point4a));
		    p->v[da[1]-1] = da2;
		    p->v[da[0]-1] = pendpt[0].v[da[0]-1] + l*dda[0];
		}
	    }
	}
	j = *ni * nda[0] * nda[1];
    }
    phys_2_index_trans_((float *)pslice, (float *)slice, iflag, &four, &j);
    free(pslice);
    return((float *) slice);
}
}
/*
 * data_slicer_2d_ : This routine returns the computational points at
 * which a 2d slice should be taken.
 *
 * Arguments:
 * corner	float *	input	4 corner points of slice in physical space.
 * ii		int *	input	axis coordinate of ni.
 * jj		int *	input	axis coordinate of nj.
 * ni		int *	output	Number of points in the x-direction.
 * nj		int *	output	Number of points in the y-direction.
 * da		int *	input	axis coordinate of averages
 * nda		int *	output	Number of points in the da-directions.
 * ri		float *	input	Multiply normal # x-points by this
 * rj		float *	input	Multiply normal # y-points by this
 *
 * The corners will be:     2    3
 *                      jj
 *                          0    1
 *                            ii
 */

float *
data_slicer_2d_trans_(corner, ii, jj, ni, nj, da, nda, ri, rj)

float *corner, *ri, *rj;
int *ii, *jj, *ni, *nj, *da, *nda;
{
    if (EXISTS(data_slicer_2d)) {
	return(DATA_SLICER_2D(corner, ii, jj, ni, nj, da, nda, ri, rj));
    }
{
    /*
       Use pslicer2d_
       */
    float *pslicer2d_();

    return(pslicer2d_(corner, ii, jj, ni, nj, da, nda, ri, rj));
}
}

/*
 * data_slicer_3d_ : This routine returns the computational points at
 * which a 3d slice should be taken.
 *
 * Arguments:
 * corner	float *	input	2 corner points of slice in physical space.
 * ii		int *	input	axis coordinate of ni.
 * jj		int *	input	axis coordinate of nj.
 * kk		int *	input	axis coordinate of nk.
 * ni		int *	output	Number of points in the x-direction.
 * nj		int *	output	Number of points in the y-direction.
 * nk		int *	output	Number of points in the z-direction.
 * da		int *	input	axis coordinate of average direction
 * nda		int *	output	Number of points in the da-direction
 *
 */

float *
data_slicer_3d_trans_(corner, ii, jj, kk, ni, nj, nk, da, nda)

float *corner;
int *ii, *jj, *kk, *ni, *nj, *nk, *da, *nda;
{
    if (EXISTS(data_slicer_3d)) {
	return(DATA_SLICER_3D(corner, ii, jj, kk, ni, nj, nk, da, nda));
    }
{
    /*
       Use pslicer3d_
       */
    float *pslicer3d_();

    return(pslicer3d_(corner, ii, jj, kk, ni, nj, nk, da, nda));
}
}

/*
 * calc_field_ : This routine is used to calculate user-derived field.
 * The return value of the routine is a pointer to the field values.
 * NULL return => cannot calculate.
 *
 * Arguments:
 *	name	char *		The name of the field to derive.
 *	ndims	int *		Number of dimensions in field (output).
 *	dims	int *		Number of points in the field in Fortran
 *				order (x, y, z, t) (output).
 *	stag	real *		Grid staggering per dimension (output).
 *	min	real *		Physical space minimum per dimension (output).
 *	max	real *		Physical space maximum per dimension (output).
 *	missing	float *		Missing data value, zero => none (output).
 *	data_units
 *		char *		Units for field (output).
 *	data_display_units
 *		char *		Units to use to display field (output).
 *	dim_names
 *		char *		Names of the dimensions.
 *	len1	int		Number of characters in name.
 *	len2	int		Number of characters in data_units.
 *	len3	int		Number of characters in data_display_units.
 *	len4	int		Number of characters in dim_names.
 */

float *
calc_field_trans_(name, ndims, dims, stag, min, max, missing, data_units,
	    data_display_units, dim_names, len1, len2, len3, len4)
     int *ndims, *dims;
     long len1, len2, len3, len4;
     float *stag, *min, *max, *missing;
     char *name, *data_units, *data_display_units, *dim_names;
{
    if (EXISTS(calc_field)) {
	return(CALC_FIELD(name, ndims, dims, stag, min, max, missing,
			  data_units, data_display_units, dim_names,
			  len1, len2, len3, len4));
    }
{
    float *derivative_();

    /*
       This is where you put the code to do your own calculation.

       Here is an example.  Say you have temperature and pressure fields,
       and you want to calculate the potential temperature:

       THETA = T(1000/P)**0.277

       There are two ways to code this.  This first uses the ability of
       the IVE routine "getvar" to do the math for you:

       if (strcmp(name, "THETA")) {
           int flag;

           return(getvar_("T*(1000/P)^0.277", ndims, dims, stag, min,
	                  max, missing, data_units, data_display_units,
			  dim_names, &flag, 16, len2, len3, len4);
       }
       
       The second way is to grab the variables and do it yourself
       (this code assumes T and P are on the same grid):

       if (strcmp(name, "THETA")) {
           float *t, *p, *result, pmissing;
	   int flag, i, num;

           if ((p = getvar_("P", ndims, dims, stag, min, max,
	                    pmissing, data_units, data_display_units,
			    dim_names, &flag, 1, len2, len3, len4)) == NULL) {
	       make_help_widget_("THETA - cannot get P");
	       return NULL;
	   }
           if ((t = getvar_("T", ndims, dims, stag, min, max,
	                    missing, data_units, data_display_units, dim_names,
			    &flag, 1, len2, len3, len4)) == NULL) {
	       make_help_widget_("THETA - cannot get T");
	       return NULL;
	   }
	   num = dims[0] * dims[1] * dims[2] * dims[3];
	   if ((result=(float *)memalign(sizeof(float), sizeof(float)*num))
	       == NULL) {
	       make_help_widget_("THETA - cannot allocate memory");
	       return NULL;
	   }
	   for (i=0; i < num; i++) {
	       if (t[i] == missing || p[i] == pmiss) result[i] = missing;
	       else result[i] =
	           pow((double)t[i]*(1000./p[i]), (double) 0.277);
	   return(result);
       }
       */
    /*
       "derivative" is a routine in IVE that calculates the derivative
       of a field, if the syntax of "name" is D[field:dir:type].
       If "name" is not in that format, or if derivative cannot do the
       calculation, it returns NULL.
       */

    return(derivative_(name, ndims, dims, stag, min, max, missing,
		       data_units, data_display_units, dim_names,
		       len1, len2, len3, len4));
}
}
/*
 * calc_field_ : This routine is used to calculate user-derived field.
 * The return value of the routine is a pointer to the field values.
 * NULL return => cannot calculate.
 *
 * Arguments:
 *	name	char *		The name of the field to derive.
 *	ndims	int *		Number of dimensions in field (output).
 *	dims	int *		Number of points in the field in Fortran
 *				order (x, y, z, t) (output).
 *	stag	real *		Grid staggering per dimension (output).
 *	min	real *		Physical space minimum per dimension (output).
 *	max	real *		Physical space maximum per dimension (output).
 *	missing	float *		Missing data value, zero => none (output).
 *	data_units
 *		char *		Units for field (output).
 *	data_display_units
 *		char *		Units to use to display field (output).
 *	dim_names
 *		char *		Names of the dimensions.
 *	len1	int		Number of characters in name.
 *	len2	int		Number of characters in data_units.
 *	len3	int		Number of characters in data_display_units.
 *	len4	int		Number of characters in dim_names.
 */

void 
run_sub_(name, len)
long len;
char *name;
{
  if (EXISTS(run_sub)) {
    RUN_SUB(name,  len);
  }
  else
    {
      char message[256];
      snprintf(message,256,"run_sub: %s",name);
      make_help_widget_(message);
    }
}

/*
 * heading_ : This routine allows the user to change headings on plots
 *
 * Arguments:
 *      where   int *           Wherefrom called
 *      which   int *           Which heading (first (1) or second (2) line)
 *      line    char *          The line to be changed.
 *
 * written by david@atmos.umnw.ethz.ch
 */

void
heading_trans_(where, which, line, len)
     int  *where;
     int  *which;
     char *line;
     long len; /*stupid fortran length int*/
{
    if (EXISTS(heading)) {
	HEADING(where, which, line, len);
	return;
    }
}

/*
 * file_coordinate_: This routine allows the user to change the current
 * value of the file coordinate from what IVE thinks it should be.
 *
 * Arguments:
 *	value	float *		Current value of the file coordinate
 *	coord	int *		Index of file coordinate (0-3)
 *      ncid    int *           Id of currently opened netCDF file.
 */

float
file_coordinate_trans_(value, coord, ncid)
    float *value;
    int *coord, *ncid;
{
    if (EXISTS(file_coordinate)) {
      return(FILE_COORD(value, coord, ncid));
    }
    {
      return(*value);
    }
}

/*
 * move_parcel_: This routine returns the new point of a parcel after
 * it has been moved for a duration of deltaT from it's originating
 * point.
 *
 * Arguments:
 *	retpt	float *		output for new point (phys) [4]
 *	curpt	float *		current point (phys)        [4]
 *      uwind   float *         uwind at current point
 *      vwind   float *         vwind at current point
 *      wwind   float *         wwind at current point
 *      detlaT  float *         time change amount
 *      repeat  int *           Is this not the first time ?
 *      error   int *           Return true for error
 */
void
move_parcel_trans_(retpt, curpt, uwind, vwind, wwind, detlaT, repeat, error)
    float *retpt, *curpt, *uwind, *vwind, *wwind, *detlaT;
    int *repeat, *error;
{
  if (EXISTS(move_parcel)) {
    MOVE_PARCEL(retpt, curpt, uwind, vwind, wwind, detlaT, repeat, error);
		}
  else{
    char uunits[80], vunits[80], wunits[80], dunits[4][80], chunit[160];
    int err, i=4;
    double slope, intercept;
    utUnit unit1, unit2 ;

    *error=0;
    if(!*repeat){
      (void)getavar_("traj_units1", uunits, &err, 11, 80);
      (void)getavar_("traj_units2", vunits, &err, 11, 80);
      (void)getavar_("traj_units3", wunits, &err, 11, 80);
      (void)getaarr_("domain_units", dunits, &i, &err, 12, 80);
      if(dunits[0][0] != '\0' && dunits[0][0] !=' '&&
	 uunits[0] != '\0' && uunits[0] !=' '){
	sprintf(chunit,"%s/%s", dunits[0],dunits[3]);
	err = utScan(uunits, &unit1) ;
	if ( err != 0 ) {
	  (void)make_help_widget_("Error setting up check of \"U\" units");
	  *error=1;
	  return;
	}
	err = utScan(chunit, &unit2) ;
	if ( err != 0 ) {
	  (void)make_help_widget_("Error setting up check of \"X/T\" units");
	  *error=1;
	  return;
	}
	err = utConvert(&unit1, &unit2, &slope, &intercept) ;
	if(err != 0){
	  (void)make_help_widget_("can\'t convert between U and spacial units\n\
you must privide your own move_packet transform");
	  *error=1;
	  return;
	}
      }
      if(dunits[1][0] != '\0' && dunits[1][0] !=' '&&
	 vunits[0] != '\0' && vunits[0] !=' '){
	sprintf(chunit,"%s/%s", dunits[1],dunits[3]);
	err = utScan(vunits, &unit1) ;
	if ( err != 0 ) {
	  (void)make_help_widget_("Error setting up check of \"V\" units");
	  *error=1;
	  return;
	}
	err = utScan(chunit, &unit2) ;
	if ( err != 0 ) {
	  (void)make_help_widget_("Error setting up check of \"Y/T\" units");
	  *error=1;
	  return;
	}
	err = utConvert(&unit1, &unit2, &slope, &intercept) ;
	if(err != 0){
	  (void)make_help_widget_("can\'t convert between V and spacial units\n\
you must privide your own move_packet transform");
	  *error=1;
	  return;
	}
      }
      if(dunits[2][0] != '\0' && dunits[2][0] !=' '&&
	 wunits[0] != '\0' && wunits[0] !=' '){
	sprintf(chunit,"%s/%s", dunits[2],dunits[3]);
	err = utScan(wunits, &unit1) ;
	if ( err != 0 ) {
	  (void)make_help_widget_("Error setting up check of \"W\" units");
	  *error=1;
	  return;
	}
	err = utScan(chunit, &unit2) ;
	if ( err != 0 ) {
	  (void)make_help_widget_("Error setting up check of \"Z/T\" units");
	  *error=1;
	  return;
	}
	err = utConvert(&unit1, &unit2, &slope, &intercept) ;
	if(err != 0){
	  (void)make_help_widget_("can\'t convert between W and spacial units\n\
you must privide your own move_packet transform");
	  *error=1;
	  return;
	}
      }
    }
    
    retpt[3] = curpt[3] + *detlaT; /*time*/
    retpt[2] = curpt[2] + (*detlaT)*(*wwind); /*w*/
    retpt[1] = curpt[1] + (*detlaT)*(*vwind); /*v*/
    retpt[0] = curpt[0] + (*detlaT)*(*uwind); /*u*/
  }
}

void
physuv_2_lonlatuv_trans_(data1,data2,nu,nv,imap)
float *data1, *data2;
int *nu, *nv, *imap;
{
  if (EXISTS(physuv_2_lonlatuv_trans)) {
    PHYSUV_2_LONLATUV_TRANS(data1,data2,nu,nv,imap);
  }
  return;
}

/*
 * dims_placement_trans: This routine allows the user to change the current
 * value of the file coordinate from what IVE thinks it should be.
 *
 * Arguments:
 *	numdims	int *		totalnumber of dims
 *      dims2go int[10] *       Id of currently opened netCDF file.
 *      name char *             name of field
 */

void
dims_placement_trans_(numdims, dims2go, name)
     int *numdims, *dims2go;
     char *name;
{
  if (EXISTS(dims_placement)) {
    long len = strlen(name);
    DIMS_PLACEMENT(numdims, dims2go, name, len);
      }
  else
    if(*numdims==5){
      dims2go[0]=-1;
      dims2go[1]= 3;
      dims2go[2]= 2;
      dims2go[3]= 1;
      dims2go[4]= 0;
    }
}


/* END INTERFACE ROUTINES AND DEFAULT TRANSFORMS */

#ifdef STATIC
int
load_func_(progname, objfile)
     char *progname, *objfile;
{
    return(0);
}
#else

static int
check_obj(name, type)
    char *name, *type;
{
    char message[256], *found_type;
    unsigned char magic[4];
    int fd;

    if ((fd=open(name, 0)) < 0) {
	sprintf(message, "Cannot open transform file %s", name);
	make_help_widget_(message);
	return(-1);
    }
    if (read(fd, magic, 4) != 4) {
	close(fd);
	sprintf(message, "Bad transform file %s", name);
	make_help_widget_(message);
	return(-1);
    }
    if(strcmp(type, "ultrix")) close(fd);
    else lseek(fd,0,0);
    if(magic[0] == 0x62 && magic[1] == 0x1) found_type = "ultrix";
    else if (magic[0] == 0x83 && magic[1] == 0x1) found_type = "alpha";
    else if (magic[0] == 0x81 && magic[1] == 0x3) found_type = "sunos";
    else if (magic[0] == 0x7f && strncmp((char *)magic+1, "ELF", 3) == 0)
	found_type = "ELF";
    else found_type = "unknown";
    if (strcmp(type, found_type)) {
	sprintf(message, "Bad trans file type, wanted %s, found %s",
		type, found_type);
	make_help_widget_(message);
	return(-1);
    }
    return(fd);
}
	
#ifdef ultrix
load_func_(progname,objfile)
     char *progname, *objfile;
{
    static caddr_t base=0, base2=0, addr;
    static u_long oldsize=0;
    static int first = 0;
    register u_long readsize, offset, totsize=0, newsize=0;
    int fd;
    struct exec header;
    char libs[64], linkbuf[256], *tmpfile;
    extern caddr_t sbrk();
    extern int close(), lseek(), open(), read(), strlen(), system(), unlink();
    extern void bzero();
    struct nlist nl[100];
    
    new_file = NULL;
    new_field = NULL;
    index_2_phys = NULL;
    phys_2_index = NULL;
    horiz_ter = NULL;
    vert_ter = NULL;
    phys_2_lonlat = NULL;
    lonlat_2_phys = NULL;
    default_map = NULL;
    data_slicer_2d = NULL;
    data_slicer_3d = NULL;
    calc_field = NULL;
    heading = NULL;
    file_coordinate = NULL;
    move_parcel = NULL;
    physuv_2_lonlatuv_trans = NULL;
    if (strcasecmp(objfile, "default") == 0) return 0;
    libs[0]=NULL;
    
    /* get a temporary file for loader output
     * setenv TMPDIR for non-default directory */
    if(base){
	free(base);
	base = 0;
    }
    if ((fd = check_obj(objfile, "ultrix")) < 0) return 2;
    tmpfile = tempnam(NULL, "dyn");
    
    /* decipher a.out header */
    if (read(fd, (char *)&header, sizeof(header)) != sizeof(header)){
        (void) close(fd);
        (void) unlink(tmpfile);
        free(tmpfile);
        (void) fprintf(stderr,"%s: couldn't read header from file %s\n",
                       progname,tmpfile);
        perror("read");
        return 3;
    }
    readsize = ROUND(header.a_text,4) + ROUND(header.a_data,4);
    totsize += readsize + header.a_bss;
    totsize = ROUND(totsize,512); /* XXX (BSD magic number) */
    
    base = (caddr_t)valloc(totsize);
    
    /* zero bss segment */
    bzero(base+readsize, header.a_bss);
    close(fd);
    
#   ifdef DEBUG
    (void) printf("base=0x%lx  size=%lu\n",
		  base,totsize);
#   endif /* DEBUG */
    
    (void) sprintf(linkbuf, LINKBUF,
		   progname,base,objfile,tmpfile,libs);
    
#   ifdef DEBUG
    (void) puts(linkbuf);
#   endif /* DEBUG */
    
    if (system(linkbuf)){
        (void) unlink(tmpfile);
        free(tmpfile);
        (void) fprintf(stderr,"%s: ld returned error, cannot load functions\n",
                       progname);
	if(!first){
	    free(base);
	    base=0;
	}
        return 1;
    }
    
    if ((fd = open(tmpfile,0)) < 0){  /* open O_RDONLY */
        (void) unlink(tmpfile);
        free(tmpfile);
        (void) fprintf(stderr,"%s: couldn't open loader output file %s\n",
                       progname,tmpfile);
        perror("open");
	if(!first){
	    free(base);
	    base=0;
	}
        return 2;
    }
    
    
    
/* decipher a.out header */
    if (read(fd, (char *)&header, sizeof(header)) != sizeof(header)){
        (void) close(fd);
        (void) unlink(tmpfile);
        free(tmpfile);
        (void) fprintf(stderr,"%s: couldn't read header from file %s\n",
                       progname,tmpfile);
        perror("read");
	if(!first){
	    free(base);
	    base=0;
	}
        return 3;
    }
    readsize = ROUND(header.a_text,4) + ROUND(header.a_data,4);
    newsize += readsize + header.a_bss;
    newsize = ROUND(totsize,512); /* XXX (BSD magic number) */
    
    if(newsize != totsize){
	base2 = (caddr_t)realloc(base,totsize);
	if(base2 != base){
	    perror("memory allocation error in loadfunc");
	    free (base2);
	    return 4;
	}
	/* zero bss segment */
	bzero(base+readsize, header.a_bss);
    }
    
    
#   ifdef DEBUG
(void) printf("text=0x%x  data=0x%x  bss=0x%x\n",
              header.a_text,header.a_data,header.a_bss);
#   endif /* DEBUG */


    /* seek to actual data and load it into memory */
#   ifdef mips
      offset = N_TXTOFF(header.ex_f,header.ex_o);
#   else /* BSD */
        offset = N_TXTOFF(header);
#   endif /* mips */

#   ifdef DEBUG
(void) printf("N_TXTOFF=0x%lx  ENTRY=0x%x\n",
              offset,header.a_entry);
#   endif /* DEBUG */

    if (lseek(fd, offset, 0) != offset){  /* lseek L_SET */
        (void) close(fd);
        (void) unlink(tmpfile);
        free(tmpfile);
        (void) fprintf(stderr,"%s: couldn't position file %s at N_TXTOFF\n",
                       progname,tmpfile);
        perror("lseek");
        return 6;
    }
    if (read(fd, base, readsize) != readsize){
        (void) close(fd);
        (void) unlink(tmpfile);
        free(tmpfile);
        (void) fprintf(stderr,"%s: couldn't read data from file %s\n",
                       progname,tmpfile);
        perror("read");
        return 7;
    }

    (void) close(fd);
nl[0].n_name = "new_file_";
nl[1].n_name = "new_field_";
nl[2].n_name = "index_2_phys_";
nl[3].n_name = "phys_2_index_";
nl[4].n_name = "horiz_ter_";
nl[5].n_name = "vert_ter_";
nl[6].n_name = "phys_2_lonlat_";
nl[7].n_name = "lonlat_2_phys_";
nl[8].n_name = "default_map_";
nl[9].n_name = "calc_field_";
nl[10].n_name = "heading_";
nl[11].n_name = "data_slicer_2d_";
nl[12].n_name = "data_slicer_3d_";
nl[13].n_name = "file_coordinate_";
nl[14].n_name = "move_parcel_";
nl[15].n_name = "run_sub_";
nl[16].n_name = "physuv_2_lonlatuv_trans_";
nl[17].n_name = "dims_placement_trans_";
nl[18].n_name = "";
    if(nlist(tmpfile,nl) != -1){
	int i;
	new_file = (int (*)())nl[0].n_value;
	new_field = (int (*)())nl[1].n_value;
	index_2_phys = (int (*)())nl[2].n_value;
	phys_2_index = (int (*)())nl[3].n_value;
	horiz_ter = (int (*)())nl[4].n_value;
	vert_ter = (int (*)())nl[5].n_value;
	phys_2_lonlat = (int (*)())nl[6].n_value;
	lonlat_2_phys = (int (*)())nl[7].n_value;
	default_map = (int (*)())nl[8].n_value;
	calc_field = (int (*)())nl[9].n_value;
        heading = (int (*)())nl[10].n_value;
	data_slicer_2d = (float (*)())nl[11].n_value;
	data_slicer_3d = (float (*)())nl[12].n_value;
	file_coordinate = (float (*)())nl[13].n_value;
	move_parcel = (void *(*)())nl[14].n_value;
	run_sub = (void *(*)())nl[15].n_value;
	physuv_2_lonlatuv_trans = (void *(*)())nl[16].n_value;
	dims_placement_trans_ = (void *(*)())nl[17].n_value;

    }
    if (unlink(tmpfile)){
        (void) fprintf(stderr,"%s: couldn't delete file %s\n",
                       progname,tmpfile);
        perror("unlink");
    }
    free(tmpfile);

    return 0;
}
#endif /*ultrix*/
#if defined(sparc) && !defined(SOLARIS)
#include <dlfcn.h>

    int
load_func_(progname, objfile)
     char *progname, *objfile;
{
    static void *dlp=NULL;

    if(dlp) {
	dlclose(dlp);
	new_file = NULL;
	new_field = NULL;
	index_2_phys = NULL;
	phys_2_index = NULL;
	horiz_ter = NULL;
	vert_ter = NULL;
	phys_2_lonlat = NULL;
	lonlat_2_phys = NULL;
	default_map = NULL;
	data_slicer_2d = NULL;
	data_slicer_3d = NULL;
	calc_field = NULL;
        heading = NULL;
	file_coordinate = NULL;
	move_parcel = NULL;
	run_sub= NULL;
	physuv_2_lonlatuv_trans = NULL;
	dims_placement_trans_=NULL;	    
    }
    if (strcasecmp(objfile, "default") == 0) return 0;
    if (check_obj(objfile, "sunos") < 0) return 2;
    if ((dlp  = dlopen(objfile, 1)) == NULL) {
        (void) fputs(progname,stderr);
        (void) fputs(": error loading image\n",stderr);
        (void) fputs(dlerror(),stderr);
        return 1;
    }

    return 0;
}
#endif /*sparc*/
#if defined(__alpha) || defined(SOLARIS) || defined(IRIX) || defined(linux)
#include <dlfcn.h>

    int
    load_func_(progname, objfile)
	 char *progname, *objfile;
    {
      static void *dlp=NULL;
#if defined(IRIX)
#else
#endif
      if(dlp) {
	dlclose(dlp);
	new_file = NULL;
	new_field = NULL;
	index_2_phys = NULL;
	phys_2_index = NULL;
	horiz_ter = NULL;
	vert_ter = NULL;
	phys_2_lonlat = NULL;
	lonlat_2_phys = NULL;
	default_map = NULL;
	data_slicer_2d = NULL;
	data_slicer_3d = NULL;
	calc_field = NULL;
        heading = NULL;
	file_coordinate = NULL;
	move_parcel = NULL;
	run_sub = NULL;
	physuv_2_lonlatuv_trans = NULL;
	dims_placement = NULL;
    }
    if (strcasecmp(objfile, "default") == 0) return 0;
    if (check_obj(objfile, 
#if defined(__alpha)
		  "alpha"
#else
		  "ELF"
#endif
		  ) < 0) return 2;
    if ((dlp  = dlopen(objfile, RTLD_LAZY)) == NULL) {
        (void) fputs(progname,stderr);
        (void) fputs(": error loading image\n",stderr);
        (void) fputs(dlerror(),stderr);
        return 1;
    }

    if ((new_file = (int (*)())dlsym(dlp, "new_file_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  new_file procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((new_field = (int (*)())dlsym(dlp, "new_field_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding new_field procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((index_2_phys = (int (*)())dlsym(dlp, "index_2_phys_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding index_2_phys procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((phys_2_index = (int (*)())dlsym(dlp, "phys_2_index_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding phys_2_index procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((horiz_ter = (int (*)())dlsym(dlp, "horiz_ter_")) == NULL) {
 /*       (void) fputs(progname,stderr);
        (void) fputs(": error finding horiz_ter procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((vert_ter = (int (*)())dlsym(dlp, "vert_ter_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding vert_ter procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((phys_2_lonlat = (int (*)())dlsym(dlp, "phys_2_lonlat_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding _phys_2_lonlat procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((lonlat_2_phys = (int (*)())dlsym(dlp, "lonlat_2_phys_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _lonlat_2_phys procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((default_map = (int (*)())dlsym(dlp, "default_map_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _default_map procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((data_slicer_2d = (float * (*)())dlsym(dlp, "data_slicer_2d_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _data_slicer_2d procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((data_slicer_3d = (float * (*)())dlsym(dlp, "data_slicer_3d_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _data_slicer_3d procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((calc_field = (float * (*)())dlsym(dlp, "calc_field_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _calc_field procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((heading = (int (*)())dlsym(dlp, "heading_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _heading procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((file_coordinate=(float(*)())dlsym(dlp, "file_coordinate_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _file_coordinate procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((move_parcel=(void *(*)())dlsym(dlp, "move_parcel_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding  _file_coordinate procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((run_sub=(void *(*)())dlsym(dlp, "run_sub_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding run_sub procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
    if ((physuv_2_lonlatuv_trans=(void *(*)())dlsym(dlp, "physuv_2_lonlatuv_trans_")) == NULL) {
/*        (void) fputs(progname,stderr);
        (void) fputs(": error finding physuv_2_lonlatuv_trans procedure\n",stderr);
        (void) fputs(dlerror(),stderr);*/
    }
   return 0;
}
#endif /*alpha*/
#endif /*STATIC*/
