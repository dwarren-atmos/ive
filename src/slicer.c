static char rcsid[] = "$Id: slicer.c,v 1.35 2001/08/15 22:25:48 davidive Exp $";
/* $Id: slicer.c,v 1.35 2001/08/15 22:25:48 davidive Exp $
 * Purpose:
 *   This routine is used to compute a plane in space through a 
 *   four dimentional data set. This code was inspired by Fundamentals
 *   of Interactive Computer Graphics - J. D. Foley and A. Van Dam
 *   revision 1 page 512.  It returns a pointer to the slice
 *   requested.
 *
 *   data:      the data set to slice [*ny][*nx][*nt][*nz]
 *   dims:      number of dimensions of the data
 *   pt?:       the 3 points that define a plane (note: if all three are
 *                                              colinear we have a line)
 *		On exit pt1 is the first point, pt2 is the last point
 *		of the first row (used by terrain routines) if the
 *		yaxis is z.
 *   nj:	number of points in first dimension of slice
 *   ni:	number of points in second dimension of slice
 *   special:   special value
 *   phys:      If one - do evenly space slice in physical space.
 *              If two - do at physical points of last slice.
 *   surface:   If one - surface plot.
 *
 *   $Log: slicer.c,v $
 *   Revision 1.35  2001/08/15 22:25:48  davidive
 *   Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
 *   Added code to set up which axis to use and to convert the labels in 1d plots.
 *
 *   Revision 1.34  2001/01/04 22:45:37  davidive
 *   fixed reset_new_slice to force a new slice, but not reset any of the values
 *   in slab_d as that was causing bad contours to occur.
 *
 *   Revision 1.33  2000/12/08 00:20:30  warren
 *   fixed bug where you select field, plot, select new field, select old field
 *   and plot -> gives wrong contours.
 *
 *   Revision 1.32  2000/08/02 19:59:44  harry
 *   Fix surface vector plotting.
 *
 *   Revision 1.31  2000/04/11 15:26:52  harry
 *   Fix another surface slice bug.
 *
 *   Revision 1.30  2000/04/05 22:38:55  harry
 *   Correct bugs in general surface plotting.
 *
 *   Revision 1.29  1998/09/18 17:34:50  harry
 *   Fix declaration of corner.
 *
 *   Revision 1.28  1998/07/16 22:10:58  harryive
 *   Add two dimensional slicing with 2 averaged dimensions.
 *
 *   Revision 1.27  1997/03/25 18:43:18  harry
 *   Make sure fixmap structure is properly set (i.e. min < max).
 *
 *   Revision 1.26  1997/03/14 17:12:03  harry
 *   Fix vector interval for surface plots.
 *
 *   Revision 1.25  1997/02/21 20:02:11  harry
 *   Fix vector interval to be floating point and have values less than one.
 *   Handling of vector interval is now in the slicer.
 *
 *   Revision 1.24  1997/01/28 22:02:08  harry
 *   Fix surface plotting problem.
 *
 *   Revision 1.23  1996/10/16 18:27:15  harry
 *   Serious error in an arbitrary slice that "comes in the bottom and goes
 *   out the top".
 *
 *   Revision 1.22  1996/08/09 22:53:12  harry
 *   Fix problem with "reversed" axes and windowing.
 *
 *   Revision 1.21  1996/06/24 19:51:59  harry
 *   Fix bug in arbitrary angle vertical 2d slicer.
 *
 *   Revision 1.20  1996/06/21 16:07:40  harry
 *   Add new option to slicer (phys = 2) to properly get values for
 *   vectors.  By using this option, we get the second component of the
 *   vector on the same grid as the first component.
 *
 *   Revision 1.19  1996/06/19 23:09:34  harry
 *   Correctly reset spval, including the local copy.
 *
 *   Revision 1.18  1996/03/27 23:54:03  harry
 *   Slicer should check new variable "field_pointer" to find out name of
 *   field currently being sliced instead of "field" to properly handle
 *   vectors and other future plots (like Skew-T).
 *
 *   Revision 1.17  1996/02/23 22:31:32  harry
 *   Fix for reset_spval feature for new slicer.
 *
 * Revision 1.16  1995/10/18  19:18:27  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.15  1995/09/05  20:34:11  harry
 * Add new slicer.  Good luck!
 *
 * Revision 1.14  1995/03/22  21:06:02  harry
 * Fix coordinates to properly overlay different windows.
 *
 * Revision 1.13  1994/05/26  18:16:30  harry
 * Add #ifndef around definitions of MIN and MAX.
 *
 * Revision 1.12  1994/05/26  17:39:31  warren
 * more changes for sol2
 *
 * Revision 1.11  1994/02/16  19:19:07  yuri
 * Backing up to slicer in physical space.
 *
 * Revision 1.8  1994/01/26  00:19:59  yuri
 * First step in preparing to commit FLIP_AXES stuff.
 * (Main commit will be done after IVE-3-2-0 arrival.)
 *
 * Revision 1.7  1993/11/29  19:39:44  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.6  1993/10/29  18:17:44  yuri
 * Propagating of new getdom file to IVE-3-1-? version.
 *
 * Revision 1.5  1993/10/21  23:16:03  yuri
 * In Z-T case now IVE makes slice in computational space.
 *
 * Revision 1.3  1993/07/15  22:13:17  warren
 * Fixed scaling for default 1d ticmarks. Also fixed plotting under terrain
 * problem.
 *
 * Revision 1.2  1993/06/28  22:58:30  harry
 * Give more intelligent error messages in the slicer.  Remove error
 * messages from get2dc.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:37  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.12  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.11  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.10  1992/12/03  00:03:08  millerp
 * Fix surface plots.
 *
 * Revision 1.9  1992/11/24  16:34:39  millerp
 * Set xaxis, yaxis for all slice orientations.
 *
 * Revision 1.8  1992/11/23  16:32:54  millerp
 * Fixed up surface slice procedure.
 *
 * Revision 1.7  1992/11/17  17:33:45  warren
 * add include ../ive_macros.h
 *
 * Revision 1.6  1992/11/06  17:43:57  millerp
 * Change sign of istepsize, dstepsize as appropriate, based on the
 * slice orientation and axis values.
 *
 * Revision 1.5  1992/11/05  17:03:38  millerp
 * Remove unused variables.
 *
 * Revision 1.4  1992/10/23  17:23:25  millerp
 * Made slicer & getdom compatable.
 *
 * Revision 1.3  1992/10/02  21:21:41  warren
 * changed window_points to match the new common block
 *
 * Revision 1.2  1992/09/17  15:42:49  warren
 * changed the slicing syntax to use xloc, yloc, zloc and time instead of
 * hloc and vloc. this include changes to the common blocks and command set.
 *
 * Revision 1.1  1992/09/08  19:58:02  millerp
 * Initial revision
 *
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <ive_macros.h>
#include <missing.h>
#include <window.h>

#ifndef MAX
#define MAX(x, y) ((x) > (y)? (x):(y))
#endif
#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif
#define slab_dep(x,y) *(slab_d.slab + (x) + ((y) * nis))
#define wslab_dep(x,y) *(wslab + (x) + ((y) * *ni))

extern void make_help_widget_(),getavar_(),getrarr_(),getiarr_(),getlvar_(),
  setrvar_(),setlvar_(),phys_2_index_trans_(),index_2_phys_trans_(),
  getdvar_(),scale_();
extern int convert();

static int nis, njs, force_new_slice=0;

struct {
    int locked, dimavg1, dimavg2, rconst, phys;
    float A, B, C, D, avgmin[2], avgmax[2], vlocked, *slab;
    int dfree, stepby, other;
    int xaxis, yaxis;
    float slope, intercept, *data, ri, rj;
    char datfil[256], field[256];
} slab_d = {-1, -1, -1, -1, -1, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0};

struct point {
    float x,y;
};
struct point2{
    struct point **pt;
    int numx;
    int numy;
} ;
struct point2 phpts = {(struct point **)0,
		       0,
		       0};
struct minmax {
    float minx;
    float maxx;
    float miny;
    float maxy;
} fixmap; /* used only for horizontal plots on a map */

extern double interp_();

struct wpt wp;

extern struct {
  int plot_axis;
  int rec_axis;
  float slope;
  float intercept;
} angle_plot;

float 
  *slicer_(data, nx, ny, nz, nt,
	   dims, pt1, pt2, pt3, nj, ni, special, phys, surface)
float *data, *pt1, *pt2, *pt3, *special;
int *dims, *nx, *ny, *nz, *nt, *ni, *nj, *phys, *surface;
{
    char datfil[256], field[256];
    double dmax[4],dmin[4],dfmin[4],dfmax[4];
    float *wslab, min[4], max[4], fullmin[4], fullmax[4];
    float ctest[4], ptest[4], c1[4], c2[4];
    float tester, slope=0, intercept=0, pstart, pend;
    float A, B, C, D;/*Ax +By +Cz +D = 0 is a plane*/
    float pother;
    int cpmin[4], cpmax[4], coord_dep[4][4], indep[4];
    int dcounter, icounter, iflag[4], i, j, v[4], locked, rconst, zeroes;
    int error, stepby, other, dfree, good;
    int ii, jj, same_slice, ix, iy;
    static int one=1;
    struct point4{
	float x,y,z,t;
    } *cmpt, *phpt;
    struct point4a{
	float v[4];
    } *cmpta, *phpta;
    struct point *phtmp;

    if( *dims != 4 ) {
	(void)make_help_widget_("slicer: must be 4 dimensional field");
	return((float *)0);
    }
    /* we only do space in 3 or 4 dimentions for now */

    (void)getavar_("datfil", datfil, &error,6,256);
    (void)getavar_("field_pointer", field, &error,13,256);
    (void)getrarr_("plwmin",min,dims,&error,6,4);
    (void)getrarr_("plwmax",max,dims,&error,6,4);
    (void)getrarr_("plmin",fullmin,dims,&error,5,4);
    (void)getrarr_("plmax",fullmax,dims,&error,5,4);
    (void)getiarr_("cpmin",cpmin,dims,&error,5,4);
    (void)getiarr_("cpmax",cpmax,dims,&error,5,4);
    i = 16;
    (void)getiarr_("coord_dep",coord_dep,&i,&error,9,16);
    for (i = 0; i < 4; ++i) {
	indep[i] = 1;
	for(j = 0; j < 4; ++j) {
	    if (i != j && coord_dep[i][j]) indep[i] = 0;
	}
    }

    for(icounter = 0; icounter<*dims; icounter++){
	dmax[icounter] = (double)MAX(max[icounter],min[icounter]);
	dmin[icounter] = (double)MIN(max[icounter],min[icounter]);
	dfmax[icounter] = (double)MAX(fullmax[icounter],fullmin[icounter]);
	dfmin[icounter] = (double)MIN(fullmax[icounter],fullmin[icounter]);
    }
    /*
       Break problem down to plane in 3 dimensions!

       Thus, in everything below, locked is a constant.

       */
    if (pt1[3] == pt2[3] && pt2[3] == pt3[3]) {		/* Time locked */
	v[0] = 0;
	v[1] = 1;
	v[2] = 2;
	locked = 3;
    }
    else if (pt1[2] == pt2[2] && pt2[2] == pt3[2]) {	/* Z locked */
	v[0] = 0;
	v[1] = 1;
	locked = 2;
	v[2] = 3;
    }	
    else if (pt1[1] == pt2[1] && pt2[1] == pt3[1]) {	/* Y locked */
	v[0] = 0;
	locked = 1;
	v[1] = 2;
	v[2] = 3;
    }
    else {						/* X locked */
	locked = 0;
	v[0] = 1;
	v[1] = 2;
	v[2] = 3;
    }
    A = pt1[v[1]]*(pt2[v[2]]-pt3[v[2]]) + pt2[v[1]]*(pt3[v[2]]-pt1[v[2]]) + 
      pt3[v[1]]*(pt1[v[2]]-pt2[v[2]]);
    B = pt1[v[0]]*(pt2[v[2]]-pt3[v[2]]) + pt2[v[0]]*(pt3[v[2]]-pt1[v[2]]) + 
      pt3[v[0]]*(pt1[v[2]]-pt2[v[2]]);
    C = pt1[v[0]]*(pt2[v[1]]-pt3[v[1]]) + pt2[v[0]]*(pt3[v[1]]-pt1[v[1]]) + 
      pt3[v[0]]*(pt1[v[1]]-pt2[v[1]]);
    D = pt1[v[0]]*((pt2[v[1]]*pt3[v[2]]) - (pt3[v[1]]*pt2[v[2]])) - 
      pt2[v[0]]*((pt1[v[1]]*pt3[v[2]]) - (pt3[v[1]]*pt1[v[2]])) +
	pt3[v[0]]*((pt1[v[1]]*pt2[v[2]]) - (pt2[v[1]]*pt1[v[2]]));

    if (slab_d.phys == *phys &&
	(slab_d.phys != 1 || (slab_d.ri == pt1[4] && slab_d.rj == pt2[4])) &&
	slab_d.data == data &&
	strcmp(slab_d.datfil, datfil) == 0 &&
	strcmp(slab_d.field, field) == 0 &&
	slab_d.A == A && slab_d.B == B && slab_d.C == C &&
	slab_d.D == D && slab_d.locked == locked &&
	slab_d.vlocked == pt1[locked] &&
	!force_new_slice){
      same_slice = 1;
    }
    else {
      force_new_slice=0;
	if (*phys != 2) {
	    if(phpts.numx>0){
		if(phpts.numy>0)
		    for(i=0; i<phpts.numx; i++)
			free(phpts.pt[i]);
		free(phpts.pt);
		phpts.numx = 0;
		phpts.numy = 0;
	    }
	}
	if (slab_d.slab != 0) {
	    free(slab_d.slab);
	    slab_d.slab = 0;
	}
	slab_d.phys = *phys;
	if (*phys == 1) {
	    slab_d.ri = pt1[4];
	    slab_d.rj = pt2[4];
	}
	else {
	    slab_d.ri = slab_d.rj = 1.;
	}
	slab_d.data = data;
	slab_d.A = A;
	slab_d.B = B;
	slab_d.C = C;
	slab_d.D = D;
	slab_d.locked = -1;	/* In case something goes wrong below */
	slab_d.dimavg1 = -1;
	slab_d.dimavg2 = -1;
	slab_d.vlocked = pt1[locked];
	strcpy(slab_d.datfil, datfil);
	strcpy(slab_d.field, field);
	same_slice = 0;
        getlvar_("reset_spval", &i, &error, 11);
        if (i) {
	    int izero = 0;

	    *special = 0.;
            setrvar_("spval", special, &error, 5);
            setlvar_("reset_spval", &izero, &error, 11);
        }
    }

    /* this yields Ax - By + Cz -D = 0 */

    /* Find out how many of A, B and C are zero. */

    zeroes = !A + !B + !C;

/****************************************************************************
  This is the code for 2D plots orthogonal to the coordinate axes.

  In this section we have one coordinate (named rconst) that is a constant
  value of "slope", and two free variables with the most rapidly varying
  one being "ii", and the other "jj".  In this code, "ii" is the first in the
  sequence x,y,z,t.
****************************************************************************/

    if (zeroes == 2) {

	if (!same_slice) {
	  if (A) {	/* v[0] = D/A slice constant v[0] */
	    slope = D/A;
	    rconst = v[0];
	    ii = v[1];
	    jj = v[2];
	  }
	  else if (B) {	/* v[1] = -D/B slice constant v[1] */
	    slope = -D/B;
	    rconst = v[1];
	    ii = v[0];
	    jj = v[2];
	  }
	  else {		/* v[2] = D/C slice constant v[2] */
	    slope = D/C;
	    rconst = v[2];
	    ii = v[0];
	    jj = v[1];
	  }
	  
	  njs = cpmax[jj] - cpmin[jj] + 1;
	  nis = cpmax[ii] - cpmin[ii] + 1;
	  
	  if(nis <2 || njs < 2) {
	    (void)make_help_widget_("slicer: empty slice");
	    return (float *)0;
	  }		
	  /*
	    See if computational slicing is possible.
	    
	    We have two locked coordinates: locked & rconst in phys space
	    We have two free coordinates: ii & jj in comp space
	    
	    Call convert with one point to see if it will work.
	  */
	  if (*phys == 0) {
	    for (icounter=0; icounter < 4; icounter++) iflag[icounter] = 1;
	    ctest[ii] = cpmin[ii];
	    ctest[jj] = cpmin[jj];
	    ctest[locked] = ctest[rconst] = MISSING;
	    ptest[locked] = dfmin[locked];
	    ptest[rconst] = dfmin[rconst];
	    ptest[ii] = ptest[jj] = MISSING;
	    if (*surface) {
	      ctest[2] = 1;
	      ptest[2] = MISSING;
	    }
	  }
	  if (*phys == 0 && convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
	    
	    /*get memory*/
	    if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	      {
		(void)make_help_widget_
		  ("slicer: can't allocate memory - phpt");
		return((float *) 0);
	      }
	    phpta = (struct point4a *)phpt;
	    if ((cmpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	      {
		(void)make_help_widget_
		  ("slicer: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	      }
	    cmpta = (struct point4a *)cmpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	      {
		(void)make_help_widget_
		  ("slicer: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	      }
	    if ((phpts.pt =  
		 (struct point **)malloc(nis * sizeof(struct point *))) == 
		( struct point **)0)
	      {
		(void)make_help_widget_
		  ("slicer: can't allocate memory - phpts");
		free((char *)slab_d.slab);
		slab_d.slab = 0;
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	      }
	    for(icounter=0; icounter<nis; icounter++)
	      if ((phpts.pt[icounter] =  
		   (struct point *)malloc(njs * sizeof(struct point))) == 
		  ( struct point *)0)
		{
		  (void)make_help_widget_
		    ("slicer: can't allocate memory - phpts");
		  for(j=0; j < icounter; j++) free((char *)phpts.pt[j]);
		  free((char *)phpts.pt);
		  free((char *)slab_d.slab);
		  slab_d.slab = 0;
		  free((char *)phpt);
		  free((char *)cmpt);
		  return((float *) 0);
		}
	
	    phpts.numx = nis;
	    phpts.numy = njs;
	    /*find line*/
	    for(icounter = 0 ; icounter < nis; icounter++){
		cmpta[icounter].v[ii] = (float) (cpmin[ii] + icounter);
		phpta[icounter].v[rconst] = slope;
		phpta[icounter].v[locked] = pt1[locked];
		if (*surface) cmpta[icounter].v[2] = 1;
	    }

	    /* move line through jj */

	    for(dcounter = 0 ; dcounter < njs; dcounter++){
		
		for(icounter = 0 ; icounter < nis; icounter++){
		    cmpta[icounter].v[jj] = (float) (cpmin[jj] + dcounter);
		}
		cmpta[0].v[rconst] = cmpta[0].v[locked] = MISSING;
		phpta[0].v[ii] = phpta[0].v[jj] = MISSING;
		if (*surface) {
		    cmpta[0].v[2] = 1;
		    phpta[0].v[2] = MISSING;
		}
		convert(phpt, cmpt, iflag, *dims, coord_dep, nis);
		
		for(icounter = 0 ; icounter < nis; icounter++){
		    slab_dep(icounter,dcounter) = 
			(float)interp_(data,nx,ny,nz,nt,		
				       &cmpt[icounter].x,&cmpt[icounter].y,
				       &cmpt[icounter].z,&cmpt[icounter].t,
				       special);
		    phtmp = phpts.pt[icounter];
		    phtmp[dcounter].x = phpta[icounter].v[ii];
		    phtmp[dcounter].y = phpta[icounter].v[jj];
		}
	    }
	    free(cmpt);
	    free(phpt);
	  }
	/*
	  Use physical points from last slice.
	  */
	else if (*phys == 2) {
	    nis = phpts.numx;
	    njs = phpts.numy;
	    if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((cmpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpta = (struct point4a *)cmpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    for (icounter = 0; icounter < nis; icounter++) {
		phpta[icounter].v[rconst] = slope;
		phpta[icounter].v[locked] = pt1[locked];
	    }
	    iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
	    for (dcounter = 0; dcounter < njs; dcounter++) {
		for (icounter = 0; icounter < nis; icounter++) {
		    phtmp = phpts.pt[icounter];
		    phpta[icounter].v[ii] = phtmp[dcounter].x;
		    phpta[icounter].v[jj] = phtmp[dcounter].y;
		}
		if (*surface) {
		    for (icounter = 0; icounter < nis; icounter++)
			cmpta[icounter].v[2] = 1;
		    cmpta[0].v[0] = cmpta[0].v[1] = cmpta[0].v[3] = MISSING;
		    phpta[0].v[2] = MISSING;
		    convert(phpt, cmpt, iflag, *dims, coord_dep, nis);
		}
		else
		    phys_2_index_trans_(phpt, cmpt, iflag, dims, &nis);
		for (icounter = 0; icounter < nis; icounter++)
		    slab_dep(icounter,dcounter) = 
			(float)interp_(data,nx,ny,nz,nt,		
				       &cmpt[icounter].x,&cmpt[icounter].y,
				       &cmpt[icounter].z,&cmpt[icounter].t,
				       special);
	    }
	    free(cmpt);
	    free(phpt);
	}
	/*
	   Call physical slicer or, if not physical and the convert
	   did not work, depend on the
	   user transform to do our work for us.
	   */
	else {
	    struct point4a corner[4];
	    struct point4 *compute, *pslicer2d_(), *data_slicer_2d_trans_();
	    int ii1 = ii+1, jj1 = jj+1, zero=0;

	    corner[0].v[locked] = corner[1].v[locked] = 
		corner[2].v[locked] = corner[3].v[locked] = pt1[locked];
	    corner[0].v[rconst] = corner[1].v[rconst] = 
		corner[2].v[rconst] = corner[3].v[rconst] = slope;
	    corner[0].v[ii] = corner[2].v[ii] = fullmin[ii];
	    corner[1].v[ii] = corner[3].v[ii] = fullmax[ii];
	    corner[0].v[jj] = corner[1].v[jj] = fullmin[jj];
	    corner[2].v[jj] = corner[3].v[jj] = fullmax[jj];
	    if (*phys) compute = pslicer2d_(corner, &ii1, &jj1, &nis, &njs,
					    &zero, &zero,
					    &slab_d.ri, &slab_d.rj);
	    else compute = data_slicer_2d_trans_(corner, &ii1, &jj1, &nis,
						 &njs, &zero, &zero,
						 &slab_d.ri, &slab_d.rj);
	    if (compute == 0) return((float *)0);
	    if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - phpt");
		free((char *)compute);
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)compute);
		return((float *) 0);
	    }
	    if ((phpts.pt =  
		 (struct point **)malloc(nis * sizeof(struct point *))) == 
		( struct point **)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - phpts");
		free((char *)slab_d.slab);
		slab_d.slab = 0;
		free((char *)phpt);
		free((char *)compute);
		return((float *) 0);
	    }
	    for(i=0; i<nis; i++)
		if ((phpts.pt[i] =  
		     (struct point *)malloc(njs * sizeof(struct point))) == 
		    ( struct point *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpts");
		    for(j=0; j < i; j++) free((char *)phpts.pt[j]);
		    free((char *)phpts.pt);
		    free((char *)slab_d.slab);
		    slab_d.slab = 0;
		    free((char *)phpt);
		    free((char *)compute);
		    return((float *) 0);
		}
	
	    phpts.numx = nis;
	    phpts.numy = njs;
	    iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
	    /*find line*/
	    /* move line through jj */
	    for(dcounter = 0 ; dcounter < njs; dcounter++){
	    
		cmpt = compute + nis*dcounter;
		(void)index_2_phys_trans_(phpt, cmpt, iflag, dims, &nis);

		for(icounter = 0 ; icounter < nis; icounter++){
		    slab_dep(icounter,dcounter) = 
			(float)interp_(data,nx,ny,nz,nt,		
				       &cmpt[icounter].x,&cmpt[icounter].y,
				       &cmpt[icounter].z,&cmpt[icounter].t,
				       special);
		    phtmp = phpts.pt[icounter];
		    phtmp[dcounter].x = phpta[icounter].v[ii];
		    phtmp[dcounter].y = phpta[icounter].v[jj];
		}
	    }
	    free(compute);
	    free(phpt);
	}
	slab_d.xaxis = ii+1;
	slab_d.yaxis = jj+1;
	slab_d.locked = locked;
	slab_d.rconst = rconst;
        }
	/*
	   Figure out where in the slice is the window.
	   */
	wp.imin = nis;
	wp.imax = -1;
	wp.jmin = njs;
	wp.jmax = -1;
	wp.x1 = min[slab_d.xaxis-1];
	wp.x2 = max[slab_d.xaxis-1];
	wp.y1 = min[slab_d.yaxis-1];
	wp.y2 = max[slab_d.yaxis-1];
	for ( icounter = 0; icounter < nis; icounter++ ) {
	    phtmp = phpts.pt[icounter];
	    for ( dcounter = 0; dcounter < njs; dcounter++ ) {
		if (min[slab_d.xaxis-1] < max[slab_d.xaxis-1]) {
		    if (phtmp[dcounter].x > min[slab_d.xaxis-1]
			&& icounter < wp.imin)
			wp.imin = icounter;
		    if (phtmp[dcounter].x < max[slab_d.xaxis-1]
			&& icounter > wp.imax)
			wp.imax = icounter;
		}
		else {
		    if (phtmp[dcounter].x < min[slab_d.xaxis-1]
			&& icounter < wp.imin)
			wp.imin = icounter;
		    if (phtmp[dcounter].x > max[slab_d.xaxis-1]
			&& icounter > wp.imax)
			wp.imax = icounter;
		}
		if (min[slab_d.yaxis-1] < max[slab_d.yaxis-1]) {
		    if (phtmp[dcounter].y > min[slab_d.yaxis-1]
			&& dcounter < wp.jmin)
			wp.jmin = dcounter;
		    if (phtmp[dcounter].y < max[slab_d.yaxis-1]
			&& dcounter > wp.jmax)
			wp.jmax = dcounter;
		}
		else {
		    if (phtmp[dcounter].y < min[slab_d.yaxis-1]
			&& dcounter < wp.jmin)
			wp.jmin = dcounter;
		    if (phtmp[dcounter].y > max[slab_d.yaxis-1]
			&& dcounter > wp.jmax)
			wp.jmax = dcounter;
		}
	    }
	}
	wp.imin = MAX(wp.imin-1, 0);
	wp.imax = MIN(wp.imax+1, nis-1);
	wp.jmin = MAX(wp.jmin-1, 0);
	wp.jmax = MIN(wp.jmax+1, njs-1);
	for (i=0; i<4; i++) {
	    pt1[i] = min[i];
	    pt3[i] = max[i];
	}
	pt3[slab_d.locked] = pt1[slab_d.locked] = pt2[slab_d.locked];
	pt3[slab_d.rconst] = pt1[slab_d.rconst] = pt2[slab_d.rconst];
    }
	
/****************************************************************************
  This is the code for 2D plots orthogonal to 1 of the coordinate axes.

  In this section we have one coordinate (named other) that is a function
  of another (named stepby), and a free coordinate (named dfree).  The
  decision about which coordinate is stepby and which is other depends on 
  the number of computational points in the slice (the largest is chosen
  as stepby).
****************************************************************************/

    else if (zeroes == 1) {
	if (!same_slice) {
	for(i=0; i < 4; i++) iflag[i] = 1;
	if(!A){ /* v[1] = (C/B)v[2] -D/B or
		   v[2] = (B/C)v[1] + D/C and v[0] is free*/
	    dfree = v[0];
	    ix = v[1]; 
	    iy = v[2];
	    slope = B/C;
	    intercept = D/C;
	}
	else if(!B){ /* v[0] = -(C/A)v[2] + D/A or
		        v[2] = -(A/C)v[0] + D/C and v[1] is free*/
	    dfree = v[1];
	    ix = v[0];
	    iy = v[2];
	    slope = -A/C;
	    intercept = D/C;
	}
	else if(!C){ /* v[0] = (B/A)v[1] + (D/A) or
			v[1] = -(D/B) + (A/B)v[0]  and v[2] is free*/
	    dfree = v[2];
	    ix = v[0];
	    iy = v[1];
	    slope = A/B;
	    intercept = -D/B;
	}
	/*
	  Use physical points from previous slice.
	  */
	if (*phys == 2) {
	    stepby = slab_d.stepby;
	    other = slab_d.other;
	    //printf("stepby is %d, other is %d\n",stepby,other);
	    nis = phpts.numx;
	    njs = phpts.numy;
	    if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((cmpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpta = (struct point4a *)cmpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    for (icounter = 0; icounter < nis; icounter++) {
		phpta[icounter].v[locked] = pt1[locked];
	    }
	    iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
	    for (dcounter = 0; dcounter < njs; dcounter++) {
		for (icounter = 0; icounter < nis; icounter++) {
		    phtmp = phpts.pt[icounter];
		    phpta[icounter].v[dfree] = phtmp[dcounter].x;
		    phpta[icounter].v[stepby] = phtmp[dcounter].y;
		    phpta[icounter].v[other] = slope*phpta[icounter].v[stepby]
			+ intercept;
		}
		if (*surface) {
		    for (icounter = 0; icounter < nis; icounter++)
			cmpta[icounter].v[2] = 1;
		    cmpta[0].v[0] = cmpta[0].v[1] = cmpta[0].v[3] = MISSING;
		    phpta[0].v[2] = MISSING;
		    convert(phpt, cmpt, iflag, *dims, coord_dep, nis);
		}
		else
		    phys_2_index_trans_(phpt, cmpt, iflag, dims, &nis);
		for (icounter = 0; icounter < nis; icounter++)
		    slab_dep(icounter,dcounter) = 
			(float)interp_(data,nx,ny,nz,nt,		
				       &cmpt[icounter].x,&cmpt[icounter].y,
				       &cmpt[icounter].z,&cmpt[icounter].t,
				       special);
	    }
	    free(cmpt);
	    free(phpt);
	}
	/*
	  Not *phys == 2.
	  */
	else {
	    /*use pt1 and pt2 as temp storage for min and max*/
	    pt1[dfree]=fullmin[dfree];
	    pt2[dfree]=fullmax[dfree];
	    /*
	      Figure out where the slice intersects the domain limits.
	      
	      "leftmost" point is either in the left, botton or top of the
	      domain.
	      */
	    if ((tester = slope*fullmin[ix] + intercept) >= dfmin[iy] &&
		tester <= dfmax[iy]) {
		/*
		  Left.
		  */
		pt1[iy] = tester;
		pt1[ix] = fullmin[ix];
		i = 0;
	    }
	    else if ((tester = (fullmin[iy]-intercept)/slope) >= dfmin[ix] &&
		     tester <= dfmax[ix]) {
		/*
		  Bottom.
		  */
		pt1[ix] = tester;
		pt1[iy] = fullmin[iy];
		i = 1;
	    }
	    else {
		/*
		  Top.
		  */
		pt1[ix] = (fullmax[iy]-intercept)/slope;
		pt1[iy] = fullmax[iy];
		i = 2;
	    }
	    /*
	      "Rightmost point is either in the right, bottom or top.
	      */
	    if ((tester = slope*fullmax[ix] + intercept) >= dfmin[iy] &&
		tester <= dfmax[iy]) {
		/*
		  Right.
		  */
		pt2[iy] = tester;
		pt2[ix] = fullmax[ix];
	    }
	    else if (i != 1 &&
		     (tester = (fullmin[iy]-intercept)/slope) >= dfmin[ix] &&
		     tester <= dfmax[ix]) {
		/*
		  Bottom.
		  */
		pt2[ix] = tester;
		pt2[iy] = fullmin[iy];
	    }
	    else {
		/*
		  Top.
		  */
		pt2[ix] = (fullmax[iy]-intercept)/slope;
		pt2[iy] = fullmax[iy];
	    }
	    /*
	      Find computational limits of the slice.  Step in the direction
	      of the highest number of computational points.
	      */
	    phys_2_index_trans_(pt1, c1, iflag, dims, &one);
	    phys_2_index_trans_(pt2, c2, iflag, dims, &one);
	    if (fabs(c1[ix]-c2[ix]) > fabs(c1[iy]-c2[iy])) {
		stepby = ix;
		other = iy;
	    }
	    else {
		stepby = iy;
		other = ix;
		slope = 1/slope;
		intercept = - intercept*slope;
	    }
	    //printf("stepby is %d, other is %d\n",stepby,other);
	    if (c1[stepby] > c2[stepby]) {
		float f;
		
		for(i=0; i < 4; i++) {
		    f = c1[i];
		    c1[i] = c2[i];
		    c2[i] = f;
		    f = pt1[i];
		    pt1[i] = pt2[i];
		    pt2[i] = f;
		}
	    }
	    /*
	      See if computational slicing is possible.
	      We have one locked coordinate: locked in phys space
	      We have one free coordinate dfree in comp space
	      We have two coordinates that depend on each other:
	      stepby (in comp space) and other (dep on stepby in phys space).
	      
	      First, can we get stepby to physical coord with the information
	      we currently have.
	      */
	    if (*phys == 0) {
		iflag[stepby] = 1;
		iflag[dfree] = iflag[other] = iflag[locked] = 0;
		ctest[dfree] = cpmin[dfree];
		ctest[stepby] = cpmin[stepby];
		if (*surface) {
		    ctest[other] = ptest[2] = MISSING;
		    ctest[2] = 1;
		}
		else {
		    ctest[other] = ctest[locked] = MISSING;
		    ptest[locked] = dfmin[locked];
		}
		ptest[other] = ptest[stepby] = ptest[dfree] = MISSING;
		good = 0;
		if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
		    /*
		      Okay, now that we have stepby in phys, we can get other
		      in phys.  Now, can we calculate everything else?
		      */
		    ptest[other] = dfmin[other];
		    if (*surface) {
			ctest[stepby] = MISSING;
			ctest[2] = 1;
		    }
		    else ctest[stepby] = ctest[locked] = MISSING;
		    ptest[dfree] = MISSING;
		    iflag[dfree] = iflag[other] = iflag[locked] = 1;
		    good = convert(ptest, ctest, iflag, *dims, coord_dep, 1);
		}
	    }
	    if (*phys == 0 && good) {
		njs = ceil(c2[stepby]) - floor(c1[stepby]) + 1;
		nis = cpmax[dfree] - cpmin[dfree] + 1;
		
		if(nis <2 || njs < 2) {
		    (void)make_help_widget_("slicer: empty slice");
		    return (float *)0;
		}
		
		/*get memory*/
		if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    return((float *) 0);
		}
		phpta = (struct point4a *) phpt;
		if ((cmpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - cmpt");
		    free((char *)phpt);
		    return((float *) 0);
		}
		cmpta = (struct point4a *) cmpt;
		
		if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		if ((phpts.pt =  
		     (struct point **)malloc(nis * sizeof(struct point **))) == 
		    ( struct point **)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpts");
		    free((char *)slab_d.slab);
		    slab_d.slab = 0;
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		
		for(i=0; i<nis; i++)
		    if ((phpts.pt[i] =  
			 (struct point *)malloc(njs * sizeof(struct point))) == 
			( struct point *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - phpts");
			free((char *)slab_d.slab);
			slab_d.slab = 0;
			free((char *)phpt);
			free((char *)cmpt);
			return((float *) 0);
		    }
		phpts.numx = nis;
		phpts.numy = njs;
		
		/*find line*/
		
		for(icounter = 0; icounter < nis; icounter++) {
		    cmpta[icounter].v[dfree] =
			(float) (cpmin[dfree] + icounter);
		    if (*surface) cmpta[icounter].v[2] = 1;
		    else phpta[icounter].v[locked] = pt1[locked];
		}
		
		/* move line through the line */
		
		for(dcounter = 0 ; dcounter < njs; dcounter++){
		    
		    iflag[stepby] = 1;
		    if (dcounter == 0) {
			for (icounter = 0; icounter < nis; icounter++) {
			    phpta[icounter].v[stepby] = pt1[stepby];
			}
			cmpta[0].v[stepby] = cmpta[0].v[other] = MISSING;
			phpta[0].v[dfree] = MISSING;
			if (*surface) phpta[0].v[2] = MISSING;
			else cmpta[0].v[locked] = MISSING;
		    }
		    else if (dcounter == njs-1) {
			for (icounter = 0; icounter < nis; icounter++) {
			    phpta[icounter].v[stepby] = pt2[stepby];
			}
			cmpta[0].v[stepby] = cmpta[0].v[other] = MISSING;
			phpta[0].v[dfree] = MISSING;
			if (*surface) phpta[0].v[2] = MISSING;
			else cmpta[0].v[locked] = MISSING;
		    }
		    else {
			for (icounter = 0; icounter < nis; icounter++)
			    cmpta[icounter].v[stepby] =
				floor(cmpta[icounter].v[stepby]) + 1;
			iflag[dfree] = iflag[other] = iflag[locked] = 0;
			cmpta[0].v[other] = MISSING;
			phpta[0].v[other] = phpta[0].v[stepby] 
			    = phpta[0].v[dfree] = MISSING;
			if (*surface) phpta[0].v[2] = MISSING;
			else cmpta[0].v[locked] = MISSING;
			convert(phpt, cmpt, iflag, *dims, coord_dep, nis);
		    }
		    for (icounter = 0; icounter < nis; icounter++)
			phpta[icounter].v[other] =
			    slope*phpta[icounter].v[stepby] + intercept;
		    iflag[dfree] = iflag[other] = iflag[locked] = 1;
		    convert(phpt, cmpt, iflag, *dims, coord_dep, nis);
		    
		    for(icounter = 0 ; icounter < nis; icounter++){
			slab_dep(icounter,dcounter) = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt[icounter].x,&cmpt[icounter].y,
					   &cmpt[icounter].z,&cmpt[icounter].t,
					   special);
			phtmp = phpts.pt[icounter];
			phtmp[dcounter].x = phpta[icounter].v[dfree];
			phtmp[dcounter].y = phpta[icounter].v[stepby];
		    }
		}
		free(phpt);
		free(cmpt);
	    }
	    /*
	      Call physical slicer or, if not physical and the convert
	      did not work, depend on the
	      user transform to do our work for us.
	      */
	    else {
		struct point4a corner[4];
		struct point4 *compute, *pslicer2d_(), *data_slicer_2d_trans_();
		int ii1 = dfree+1, jj1 = (stepby+1)*10 + other + 1, zero=0;
		
		corner[0].v[locked] = corner[1].v[locked] = 
		    corner[2].v[locked] = corner[3].v[locked] = pt1[locked];
		corner[0].v[dfree] = corner[2].v[dfree] = fullmin[dfree];
		corner[1].v[dfree] = corner[3].v[dfree] = fullmax[dfree];
		corner[0].v[stepby] = corner[1].v[stepby] = pt1[stepby];
		corner[2].v[stepby] = corner[3].v[stepby] = pt2[stepby];
		corner[0].v[other] = corner[1].v[other] = 
		    pt1[stepby]*slope+intercept;
		corner[2].v[other] = corner[3].v[other] = 
		    pt2[stepby]*slope+intercept;
		if (*phys) compute = pslicer2d_(corner, &ii1, &jj1, &nis, &njs,
						&zero, &zero,
						&slab_d.ri, &slab_d.rj);
		else compute = data_slicer_2d_trans_(corner, &ii1, &jj1, &nis,
						     &njs, &zero, &zero,
						     &slab_d.ri, &slab_d.rj);
		if (compute == 0) return((float *)0);
		if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    free((char *)compute);
		    return((float *) 0);
		}
		phpta = (struct point4a *) phpt;
		if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)compute);
		    return((float *) 0);
		}
		if ((phpts.pt =  
		     (struct point **)malloc(nis * sizeof(struct point *))) == 
		    ( struct point **)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpts");
		    free((char *)slab_d.slab);
		    slab_d.slab = 0;
		    free((char *)phpt);
		    free((char *)compute);
		    return((float *) 0);
		}
		for(i=0; i<nis; i++)
		    if ((phpts.pt[i] =  
			 (struct point *)malloc(njs * sizeof(struct point))) == 
			( struct point *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - phpts");
			for(j=0; j < i; j++) free((char *)phpts.pt[j]);
			free((char *)phpts.pt);
			free((char *)slab_d.slab);
			slab_d.slab = 0;
			free((char *)phpt);
			free((char *)compute);
			return((float *) 0);
		    }
		
		phpts.numx = nis;
		phpts.numy = njs;
		iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
		/*find line*/
		/* move line  */
		for(dcounter = 0 ; dcounter < njs; dcounter++){
		    
		    cmpt = compute + nis*dcounter;
		    (void)index_2_phys_trans_(phpt, cmpt, iflag, dims, &nis);
		    
		    for(icounter = 0 ; icounter < nis; icounter++){
			slab_dep(icounter,dcounter) = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt[icounter].x,&cmpt[icounter].y,
					   &cmpt[icounter].z,&cmpt[icounter].t,
					   special);
			phtmp = phpts.pt[icounter];
			phtmp[dcounter].x = phpta[icounter].v[dfree];
			phtmp[dcounter].y = phpta[icounter].v[stepby];
		    }
		}
		free(compute);
		free(phpt);
	    }
	}
	slab_d.xaxis = dfree+1;
	slab_d.yaxis = stepby+1;
	slab_d.dfree = dfree;
	slab_d.stepby = stepby;
	slab_d.other = other;
	slab_d.slope = slope;
	slab_d.intercept = intercept;
	slab_d.locked = locked;
	}
	/*
	   Figure out where in the slice is the window.
	   */
	wp.imin = nis;
	wp.imax = -1;
	wp.jmin = njs;
	wp.jmax = -1;
	wp.x1 = min[slab_d.dfree];
	wp.x2 = max[slab_d.dfree];
	phtmp = phpts.pt[0];
	wp.y1 = phtmp[0].y;
	phtmp = phpts.pt[nis-1];
	wp.y2 = phtmp[njs-1].y;
	if (min[slab_d.stepby] < max[slab_d.stepby]) {
	    if (wp.y1 < min[slab_d.stepby]) wp.y1 = min[slab_d.stepby];
	    if (wp.y2 > max[slab_d.stepby]) wp.y2 = max[slab_d.stepby];
	}
	else {
	    if (wp.y1 > min[slab_d.stepby]) wp.y1 = min[slab_d.stepby];
	    if (wp.y2 < max[slab_d.stepby]) wp.y2 = max[slab_d.stepby];
	}
	pstart = slab_d.slope*wp.y1 + slab_d.intercept;
	pend =  slab_d.slope*wp.y2 + slab_d.intercept;
	if (min[slab_d.other] < max[slab_d.other]) {
	    if (pstart < min[slab_d.other])
		wp.y1 = (min[slab_d.other]-slab_d.intercept)
		    /slab_d.slope;
	    if (pend > max[slab_d.other]) 
		wp.y2 = (max[slab_d.other]-slab_d.intercept)
		    /slab_d.slope;
	}
	else {
	    if (pstart > min[slab_d.other])
		wp.y1 = (min[slab_d.other]-slab_d.intercept)
		    /slab_d.slope;
	    if (pend < max[slab_d.other]) 
		wp.y2 = (max[slab_d.other]-slab_d.intercept)
		    /slab_d.slope;
	}
	for ( icounter = 0; icounter < nis; icounter++ ) {
	    phtmp = phpts.pt[icounter];
	    for ( dcounter = 0; dcounter < njs; dcounter++ ) {
		if (min[slab_d.dfree] < max[slab_d.dfree]) {
		    if (phtmp[dcounter].x > min[slab_d.dfree]
			&& icounter < wp.imin)
			wp.imin = icounter;
		    if (phtmp[dcounter].x < max[slab_d.dfree]
			&& icounter > wp.imax)
			wp.imax = icounter;
		}
		else {
		    if (phtmp[dcounter].x < min[slab_d.dfree]
			&& icounter < wp.imin)
			wp.imin = icounter;
		    if (phtmp[dcounter].x > max[slab_d.dfree]
			&& icounter > wp.imax)
			wp.imax = icounter;
		}
		pother = slab_d.slope*phtmp[dcounter].y + slab_d.intercept;
		if (min[slab_d.stepby] < max[slab_d.stepby]) {
		    if (min[slab_d.other] < max[slab_d.other]) {
			if (phtmp[dcounter].y > min[slab_d.stepby]
			    && pother > min[slab_d.other]
			    && dcounter < wp.jmin)
			    wp.jmin = dcounter;
			if (phtmp[dcounter].y < max[slab_d.stepby]
			    && pother < max[slab_d.other]
			    && dcounter > wp.jmax)
			    wp.jmax = dcounter;
		    }
		    else {
			if (phtmp[dcounter].y > min[slab_d.stepby]
			    && pother < min[slab_d.other]
			    && dcounter < wp.jmin)
			    wp.jmin = dcounter;
			if (phtmp[dcounter].y < max[slab_d.stepby]
			    && pother > max[slab_d.other]
			    && dcounter > wp.jmax)
			    wp.jmax = dcounter;
		    }
		}
		else {
		    if (min[slab_d.other] < max[slab_d.other]) {
			if (phtmp[dcounter].y < min[slab_d.stepby]
			    && pother > min[slab_d.other]
			    && dcounter < wp.jmin)
			    wp.jmin = dcounter;
			if (phtmp[dcounter].y > max[slab_d.stepby] 
			    && pother < max[slab_d.other]
			    && dcounter > wp.jmax)
			wp.jmax = dcounter;
		    }
		    else {
			if (phtmp[dcounter].y < min[slab_d.stepby]
			    && pother < min[slab_d.other]
			    && dcounter < wp.jmin)
			    wp.jmin = dcounter;
			if (phtmp[dcounter].y > max[slab_d.stepby] 
			    && pother > max[slab_d.other]
			    && dcounter > wp.jmax)
			wp.jmax = dcounter;
		    }
		}
	    }
	}
	/*
	   Fortran dimensions.
	   */
	wp.imin = MAX(wp.imin-1, 0);
	wp.imax = MIN(wp.imax+1, nis-1);
	wp.jmin = MAX(wp.jmin-1, 0);
	wp.jmax = MIN(wp.jmax+1, njs-1);
	for (i=0; i<4; i++) {
	    pt1[i] = min[i];
	    pt3[i] = max[i];
	}
	pt3[slab_d.locked] = pt1[slab_d.locked] = pt2[slab_d.locked];
	pt1[slab_d.stepby] = wp.y1;
	pt3[slab_d.stepby] = wp.y2;
	pt1[slab_d.other] = slab_d.slope*wp.y1 + slab_d.intercept;
	pt3[slab_d.other] = slab_d.slope*wp.y2 + slab_d.intercept;
    }
    /*
       If xaxis is Z, we must reverse it.
       */
    if (slab_d.xaxis == 3) {
	float f;

	wp.reverse = 1;
	wp.xaxis = slab_d.yaxis;
	wp.yaxis = slab_d.xaxis;
	f = wp.x1;
	wp.x1 = wp.y1;
	wp.y1 = f;
	f = wp.x2;
	wp.x2 = wp.y2;
	wp.y2 = f;
	*nj = wp.imax - wp.imin + 1;
	*ni = wp.jmax - wp.jmin + 1;
	if ((wslab = (float *)malloc(*ni * *nj * sizeof(float)))
	    == (float *)0)
	{
	    (void)make_help_widget_
		("slicer: can't allocate memory - wslab");
	    return((float *) 0);
	}
	for (j = 0; j < *nj; j++)
	    for (i=0; i < *ni; i++)
		wslab_dep(i, j) = slab_dep(j+wp.imin,
					   i+wp.jmin);
    }
    else {
	wp.reverse = 0;
	wp.xaxis = slab_d.xaxis;
	wp.yaxis = slab_d.yaxis;
	*ni = wp.imax - wp.imin + 1;
	*nj = wp.jmax - wp.jmin + 1;
	if ((wslab = (float *)malloc(*ni * *nj * sizeof(float)))
	    == (float *)0)
	{
	    (void)make_help_widget_
		("slicer: can't allocate memory - wslab");
	    return((float *) 0);
	}
	for (j = 0; j < *nj; j++)
	    for (i=0; i < *ni; i++)
		wslab_dep(i, j) = slab_dep(i+wp.imin,
					   j+wp.jmin);
    }
    phtmp = phpts.pt[0];
    fixmap.miny = MIN(min[1],max[1]);
    fixmap.minx = MIN(min[0],max[0]);
    phtmp = phpts.pt[(nis)-1];
    fixmap.maxy = MAX(min[1],max[1]);
    fixmap.maxx = MAX(min[0],max[0]);
    window_points_ = wp;
    if(!same_slice){
      angle_plot.rec_axis = stepby;
      angle_plot.intercept = intercept;
      angle_plot.slope = slope;
    }
    return(wslab);
}

void reset_same_slice_(){
  force_new_slice=1;
}
