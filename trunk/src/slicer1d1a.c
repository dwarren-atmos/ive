static char rcsid[] = "$Id: slicer1d1a.c,v 1.17 2000/12/13 22:17:22 harry Exp $";
/* $Id: slicer1d1a.c,v 1.17 2000/12/13 22:17:22 harry Exp $
 * Purpose:
 *   This routine is used to compute a line in space through a 
 *   four dimentional data set.  The data values are returned
 *
 *   data:      the data set to slice [*ny][*nx][*nt][*nz]
 *   dims:      number of dimensions of the data
 *   pt?:       the 2 points that define a line
 *		On exit pt1 is the first point, pt2 is the last point
 *		of the first row (used by terrain routines) if the
 *		line is in the x-y plane.
 *   ploc:	location of the points on the line in physical space
 *   ni:	number of points in the line
 *   special:   special value
 *   surface:   if 1, line is along lowest computational surface.
 *
 *   $Log: slicer1d1a.c,v $
 *   Revision 1.17  2000/12/13 22:17:22  harry
 *   Another minor fix.
 *
 *   Revision 1.16  2000/12/12 17:58:24  harry
 *   Fix problems with arbitrary line slices.
 *
 *   Revision 1.15  2000/08/24 19:06:09  harry
 *   Fix out of bound problem with average dimensions.
 *
 *   Revision 1.14  2000/02/11 23:14:39  harry
 *   Allow surface 1d plots.  Fix Z slider for surface plots.
 *
 *   Revision 1.13  1998/09/30 22:28:38  harry
 *   Rewrite makpointplt and add ability for 2 average points, fix errors
 *   in the 1d slicers, remove obsolete yuri routines.
 *
 *   Revision 1.12  1998/09/03 19:19:24  harry
 *   Add in 2d line plots - correct bugs in other averaging.
 *
 *   Revision 1.11  1998/07/16 22:10:59  harryive
 *   Add two dimensional slicing with 2 averaged dimensions.
 *
 *   Revision 1.10  1998/06/12 22:04:02  harry
 *   Fix bugs in 1d averaging.
 *
 *   Revision 1.9  1997/03/07 19:57:20  harry
 *   Use getdvar for data_slope and data_intercept.
 *
 *   Revision 1.8  1996/10/16 18:27:18  harry
 *   Serious error in an arbitrary slice that "comes in the bottom and goes
 *   out the top".
 *
 *   Revision 1.7  1996/07/19 19:50:50  harry
 *   Fix bug when left slider not at beginning.
 *
 *   Revision 1.6  1996/07/12 20:59:10  harry
 *   Change data and domain slopes and intercepts to real*8.
 *
 *   Revision 1.5  1996/06/19 23:24:07  harry
 *   Correctly reset spval, including the local copy.
 *
 *   Revision 1.4  1996/03/27 23:54:07  harry
 *   Slicer should check new variable "field_pointer" to find out name of
 *   field currently being sliced instead of "field" to properly handle
 *   vectors and other future plots (like Skew-T).
 *
 *   Revision 1.3  1996/02/23 22:31:37  harry
 *   Fix for reset_spval feature for new slicer.
 *
 * Revision 1.2  1995/10/18  19:18:31  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.1  1995/09/05  20:34:14  harry
 * Add new slicer.  Good luck!
 *
 *
 */
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <ive_macros.h>
#include <missing.h>
#include <window.h>
#include <ive_slices.h>
extern void make_help_widget_(),getavar_(),getrarr_(),getiarr_(),getlvar_(),
  setrvar_(),setlvar_(),phys_2_index_trans_(),index_2_phys_trans_(),
  getdvar_(),scale_();
extern int convert();

float find_min(), find_max();

#ifndef MAX
#define MAX(x, y) ((x) > (y)? (x):(y))
#endif
#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif

static int nis;

extern struct {
    int dimavg1, dimavg2;
    float dv[4], v[4], avgmin[2], avgmax[2], *slab, *ploc;
    int stepby, other;
    int xaxis, yaxis;
    float slope, intercept;
    char datfil[256], field[256];
} slab_l;

extern double interp_();

struct wpt wp;

float 
  *slicer1d1a_(data, nx, ny, nz, nt,
	   dims, pt1, pt2, da, ploc, ni, special, surface)
float *data, *pt1, *pt2, **ploc, *special, *surface;
int *dims, *nx, *ny, *nz, *nt, *da, *ni;
{
    char datfil[256], field[256];
    double dmax[4],dmin[4],dfmin[4],dfmax[4];
    float *wslab, min[4], max[4], fullmin[4], fullmax[4];
    float ctest[4], ptest[4], c1[4], c2[4];
    double data_slope, data_intercept;
    float tester, slope, intercept, pstart, pend;
    float dv[4];
    float value, sum, dx;
    float cmin, cmax;
    int tt, tnext, tprev, nda, ntda;
    int cpmin[4], cpmax[4], coord_dep[4][4], indep[4];
    int icounter, iflag[4], i, j, zeroes;
    int error, is, ie, dfree, good, stepby, other, locked;
    int same_slice, dimavg, same, ix, iy;
    static int one=1;
    struct point4{
	float x,y,z,t;
    } *cmpt, *phpt;
    struct point4a{
	float v[4];
    } *cmpta, *phpta;

    if( *dims != 4 ) {
	(void)make_help_widget_("slicer: must be 4 dimensional field");
	return((float *)0);
    }
    /* we only do space in 3 or 4 dimensions for now */

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
    dimavg = *da - 1;
    zeroes = 0;
    same = 1;
    for (i=0; i < *dims; i++) {
	if (i == dimavg) dv[i] = 0;
	else dv[i] = pt2[i]-pt1[i];
	if (dv[i] == 0) {
	    zeroes++;
	    if (slab_l.dv[i] || pt1[i] != slab_l.v[i]) same = 0;
	}
	else if (dv[i] != slab_l.dv[i]) same = 0;
    }
    if (strcmp(slab_l.datfil, datfil) == 0 &&
	strcmp(slab_l.field, field) == 0 && slab_l.dimavg1 == dimavg &&
	slab_l.dimavg2 == -1 &&	slab_l.avgmin[0] == min[dimavg] &&
	slab_l.avgmax[0] == max[dimavg] && same)
	same_slice = 1;
    else {
	if (slab_l.slab != 0) {
	    free(slab_l.slab);
	    slab_l.slab = 0;
	}
	if (slab_l.ploc != 0) {
	    free(slab_l.ploc);
	    slab_l.ploc = 0;
	}
	strcpy(slab_l.datfil, datfil);
	strcpy(slab_l.field, field);
	for(i=0; i < *dims; i++) {
	    slab_l.dv[i] = dv[i];
	    if (dv[i] == 0) slab_l.v[i] = pt1[i];
	}
	slab_l.dimavg1 = -1; 
	slab_l.dimavg2 = -1; 
	slab_l.avgmin[0] = min[dimavg];
	slab_l.avgmax[0] = max[dimavg];
	same_slice = 0;
        getlvar_("reset_spval", &i, &error, 11);
        if (i) {
	    int izero = 0;

	    *special = 0.;
            setrvar_("spval", special, &error, 5);
            setlvar_("reset_spval", &izero, &error, 11);
        }
    }
/****************************************************************************
   This is the code for a line orthogonal to 3 of the axes.
****************************************************************************/

    if (zeroes == 3) {

	if (!same_slice) {

	    for (i=0; i < *dims; i++) {
		if (dv[i] && i != dimavg) dfree = i;
	    }
	    /*
	       See if computational slicing is possible.
	       
	       We have two locked coordinates in phys space
	       We have one average coordinate: dimavg in phys or comp space
	       We have one free coordinate: dfree in comp space

	       First, if dfree depends on dimavg, we cannot do it in
	       computational space
	       */
	    good = 0;
	    if (coord_dep[dfree][dimavg] == 0) {
		/*
		   Call convert with dimavg phys to see if it will work.
		   */
		for (icounter=0; icounter < 4; icounter++) {
		    iflag[icounter] = 1;
		    ptest[icounter] = pt1[icounter];
		    ctest[icounter] = MISSING;
		}
		ptest[dfree] = MISSING;
		ctest[dfree] = cpmin[dfree];
		if (*surface) {
		    ptest[2] = MISSING;
		    ctest[2] = 1;
		}		
		if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
		    /*
		       Okay, now try it with dimavg comp.
		       */
		    for (icounter = 0; icounter < 4; icounter++) {
			if (icounter != dfree && icounter != dimavg)
			    ctest[icounter] = MISSING;
		    }
		    ctest[dimavg] = cpmin[dimavg];
		    ptest[dimavg] = MISSING;
		    good = convert(ptest, ctest, iflag, *dims, coord_dep, 1);
		}
	    }
	    if (good) {
		nis = cpmax[dfree] - cpmin[dfree] + 1;

		if(nis <2) {
		    (void)make_help_widget_("slicer1d: empty slice");
		    return (float *)0;
		}
		ntda = cpmax[dimavg] - cpmin[dimavg] + 1;
		/*get memory*/
		if ((phpt=(struct point4 *)malloc(ntda*
						  sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    return((float *) 0);
		}
		phpta = (struct point4a *)phpt;
		if ((cmpt=(struct point4 *)malloc(ntda*
						  sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - cmpt");
		    free((char *)phpt);
		    return((float *) 0);
		}
		cmpta = (struct point4a *)cmpt;
		if ((slab_l.slab = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		if ((slab_l.ploc = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - ploc");
		    free((char *)slab_l.slab);
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		/*find line*/
		for (i=0; i < *dims; i++) {
		    if (i != dfree && i != dimavg) {
			for (icounter=0; icounter < ntda; icounter++)
			    if (*surface && i == 2)
				cmpta[icounter].v[2] = 1;	
			    else phpta[icounter].v[i] = pt1[i];
		    }
		}
		for (icounter=0; icounter < nis; icounter++) {
		    for (i=0; i<ntda; i++)
			cmpta[i].v[dfree] = cpmin[dfree] + icounter;
		    /*
		       Find the first and last point in the average.
		       */
		    for (i=0; i < *dims; i++) {
			if (*surface && i == 2) phpta[0].v[2] = MISSING;
			else if (i == dfree) phpta[0].v[i] = MISSING;
			else cmpta[0].v[i] = MISSING;
		    }
		    phpta[0].v[dimavg] = min[dimavg];
		    phpta[1] = phpta[0];
		    cmpta[1] = cmpta[0];
		    phpta[1].v[dimavg] = max[dimavg];
		    convert(phpta, cmpta, iflag, *dims, coord_dep, 2);
		    if (cmpta[0].v[dimavg] < cpmin[dimavg]) {
			cmpta[0].v[dimavg] = cpmin[dimavg];
			phpta[0].v[dimavg] = MISSING;
			convert(phpta, cmpta, iflag, *dims, coord_dep, 1);
		    }
		    cmin = cmpta[0].v[dimavg];
		    if (cmpta[1].v[dimavg] < 0 ||
			cmpta[1].v[dimavg] > cpmax[dimavg]) {
			cmpta[1].v[dimavg] = cpmax[dimavg];
			phpta[1].v[dimavg] = MISSING;
			convert(phpta+1, cmpta+1, iflag, *dims, coord_dep, 1);
		    }
		    cmax = cmpta[1].v[dimavg];
		    if (cmin == cmax) {
			slab_l.slab[icounter] = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt[0].x,&cmpt[0].y,
					   &cmpt[0].z,&cmpt[0].t,
					   special);
		    }
		    else {
			nda = ceil(cmax) - floor(cmin) + 1;
			if (nda > 2) {
			    cmpta[nda-1] = cmpta[1];
			    phpta[nda-1] = phpta[1];
			    for (i=1; i < nda-1; i++)
				cmpta[i].v[dimavg] = (int) (cmin+i);
			    for (i=0; i < *dims; i++) {
				if (*surface && i == 2)
				    phpta[1].v[2] = MISSING;
				else if (i != dimavg && i != dfree)
				    cmpta[1].v[i] = MISSING;
				else phpta[1].v[i] = MISSING;
			    }
			    convert(phpt+1, cmpt+1, iflag, *dims, coord_dep, 
				    nda-2);
			}
			sum = dx = 0.;
			for(tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    tnext = MIN(tt+1, nda-1);
			    if ((value = (float) interp_(data,nx,ny,nz,nt,
						&cmpt[tt].x,&cmpt[tt].y,
						&cmpt[tt].z,&cmpt[tt].t,
						special)) != *special) {
				sum += value*(phpta[tnext].v[dimavg]
					      -phpta[tprev].v[dimavg])*0.5;
				dx += (phpta[tnext].v[dimavg]
				       -phpta[tprev].v[dimavg])*0.5;
			    }
			}
			if (dx == 0.)
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum/dx;
		    }
		    slab_l.ploc[icounter] = phpta[0].v[dfree];
		}
	    }
	    /*
	       Well, that didn't work, now we have to depend on the
	       user transform to do our work for us.
	       */
	    else {
		struct point4a endpt[2];
		struct point4 *data_slicer_1d_trans_();
		int da1[2];

		da1[0]= dimavg+1;
		da1[1] = 0;
		for (i=0; i < *dims; i++) {
		    if (i == dfree) {
			endpt[0].v[i] = fullmin[i];
			endpt[1].v[i] = fullmax[i];
		    }
		    else if (i == dimavg) {
			endpt[0].v[i] = min[i];
			endpt[1].v[i] = max[i];
		    }
		    else endpt[0].v[i] = endpt[1].v[i] = pt1[i];
		}
		cmpt = data_slicer_1d_trans_(endpt, &nis, da1, &nda);
		if (cmpt == 0) return((float *)0);
		for (i=0; i < *dims; i++) iflag[i] = 1;
		if ((phpt=(struct point4 *)malloc(nda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    free((char *)cmpt);
		    return((float *) 0);
		}
		phpta = (struct point4a *)phpt;
		if ((slab_l.slab = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		if ((slab_l.ploc = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - ploc");
		    free((char *)slab_l.slab);
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		for(icounter = 0 ; icounter < nis; icounter++){
		    struct point4 *cmpt0=cmpt+nda*icounter;

		    (void)index_2_phys_trans_(phpt, cmpt0, iflag, dims, &nda);
		    if (nda == 1) {
			slab_l.slab[icounter] = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt0[0].x,&cmpt0[0].y,
					   &cmpt0[0].z,&cmpt0[0].t,
					   special);
			slab_l.ploc[icounter] = phpta[icounter].v[dfree];
		    }
		    else {
			sum = dx = 0.;
			for (tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    if (phpta[tt].v[dimavg] == max[dimavg]) tnext = tt;
			    else tnext = MIN(tt+1, nda-1);
			    if ((value = (float) interp_(data,nx,ny,nz,nt,
						&cmpt0[tt].x,&cmpt0[tt].y,
						&cmpt0[tt].z,&cmpt0[tt].t,
						special)) != *special) {
				sum += value*(phpta[tnext].v[dimavg]
					      -phpta[tprev].v[dimavg])*0.5;
				dx += (phpta[tnext].v[dimavg]
				       -phpta[tprev].v[dimavg])*0.5;
			    }
			    if(phpta[tt].v[dimavg] == max[dimavg]) break;
			}
			if (dx == 0.)
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum/dx;
		    }
		    slab_l.ploc[icounter] = phpta[0].v[dfree];
		}
	    }
	    free(cmpt);
	    free(phpt);
	    slab_l.xaxis = dfree+1;
	    slab_l.yaxis = 0;
	    slab_l.dimavg1 = dimavg;
	    slice_com_.slope=slope;
	    slice_com_.intercept=intercept;
	    slice_com_.ind=dfree+1;
	    slice_com_.dep=0;
	}
	/*
	   Figure out where in the slice is the window.
	   */
	wp.imin = 0;
	wp.imax = nis-1;
	wp.x1 = min[slab_l.xaxis-1];
	wp.x2 = max[slab_l.xaxis-1];
	if (min[slab_l.xaxis-1] < max[slab_l.xaxis-1]) {
	    for ( icounter = 0; icounter < nis; icounter++ ) {
		if (slab_l.ploc[icounter] <= min[slab_l.xaxis-1])
		    wp.imin = icounter;
		if (slab_l.ploc[icounter] < max[slab_l.xaxis-1])
		    wp.imax = icounter;
	    }
	}
	else {
	    for ( icounter = 0; icounter < nis; icounter++ ) {
		if (slab_l.ploc[icounter] >= min[slab_l.xaxis-1])
		    wp.imin = icounter;
		if (slab_l.ploc[icounter] > max[slab_l.xaxis-1])
		    wp.imax = icounter;
	    }
	}
	wp.imax = MIN(wp.imax+1, nis-1);
	pt1[slab_l.xaxis-1] = min[slab_l.xaxis-1];
	pt2[slab_l.xaxis-1] = max[slab_l.xaxis-1];
    }
/****************************************************************************
   This is the code for a line orthogonal to 2 of the axes.
****************************************************************************/

    else if (zeroes == 2) {

	if (!same_slice) {
	    /*
	       Figure out where the line intersects the domain limits.
	       */
	    ix = -1;
	    for(i=0; i < 4; i++) {
		if (dv[i] == 0 && i != dimavg) locked = i;
		if (dv[i] != 0) {
		    if (ix == -1) ix = i;
		    else iy = i;
		}
	    }
	    slope = (pt2[iy] - pt1[iy])/(pt2[ix] - pt1[ix]);
	    intercept = pt1[iy] - slope*pt1[ix];
	    /*
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
	       Find computational limits of slice.  Step in the direction
	       of the highest number of computational points.
	       */
	    for(i=0; i < 4; i++) iflag[i] = 1;
	    if (*surface) {
		for (i=0; i < 3; i++) c1[i] = c2[i] = MISSING;
		pt1[2] = pt2[2] = MISSING;
		c1[2] = c2[2] = 1;
		if (!convert(pt1, c1, iflag, *dims, coord_dep, 1) ||
		    !convert(pt2, c2, iflag, *dims, coord_dep, 1)) {
		    (void)make_help_widget_
			("slicer: cannot do surface plot with this transform");
		    return((float *) 0);
		}
	    }
	    else {
		phys_2_index_trans_(pt1, c1, iflag, dims, &one);
		phys_2_index_trans_(pt2, c2, iflag, dims, &one);
	    }
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
	       
	       We have one locked coordinate.
	       We have one averaged coordinate (dimavg).
	       We have two coordinates that depend on each other:
	       stepby (in comp space) and other (dep on stepby in phys space).

	       First, all is lost if stepby or other depend on dimavg.
	       */
	    good = 0;
	    if (coord_dep[stepby][dimavg] == 0 && 
		coord_dep[other][dimavg] == 0) { 
		/*
		  Next, can we get stepby to physical coord with the
		  information we currently have.
		*/
		for (i=0; i < 4; i++) {
		    iflag[i] = 0;
		    ctest[i] = MISSING;
		    ptest[i] = dfmin[i];
		};
		iflag[stepby] = 1;
		ctest[stepby] = cpmin[stepby];
		ptest[stepby] = ptest[other] = MISSING;
		if (*surface){
		    ptest[2] = MISSING;
		    ctest[2] = 1;
		}
		if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
		    /*
		      Okay, now that we have stepby in phys, we can get other
		      in phys.  Now, can we calculate everything else?
		    */
		    for (i=0; i < 4; i++) {
			iflag[i] = 1;
			ctest[i] = MISSING;
			ptest[i] = dfmin[i];
		    }
		    if (*surface) {
			ptest[2] = MISSING;
			ctest[2] = 1;
		    }
		    ctest[dimavg] = cpmin[dimavg];
		    ptest[dimavg] = MISSING;
		    good = convert(ptest, ctest, iflag, *dims, coord_dep, 1);
		}
	    }
	    if (good) {
		nis = ceil(c2[stepby]) - floor(c1[stepby]) + 1;
	    	if(nis <2) {
		    (void)make_help_widget_("slicer1d: empty slice");
		    return (float *)0;
		}
		ntda = cpmax[dimavg] - cpmin[dimavg] + 1;
		/*get memory*/
		if ((phpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    return((float *) 0);
		}
		phpta = (struct point4a *)phpt;
		if ((cmpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - cmpt");
		    free((char *)phpt);
		    return((float *) 0);
		}
		cmpta = (struct point4a *)cmpt;
		if ((slab_l.slab = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - slab");
			free((char *)phpt);
			free((char *)cmpt);
			return((float *) 0);
		    }
		if ((slab_l.ploc = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - ploc");
			free((char *)slab_l.slab);
			free((char *)phpt);
			free((char *)cmpt);
			return((float *) 0);
		    }
		/* find line */
		iflag[stepby] = 1;
		for (icounter = 0; icounter < ntda; icounter++) {
		    if (*surface) cmpta[icounter].v[2] = 1;
		    else phpta[icounter].v[locked] = pt1[locked];
		}
		for (icounter = 0; icounter < nis; icounter++) {
		    float first_stepby;

		    phpta[0].v[dimavg] = min[dimavg];

		    if (*surface) phpta[0].v[2] = MISSING;
		    else cmpta[0].v[locked] = MISSING;

		    cmpta[0].v[dimavg] = MISSING;

		    if (icounter == 0) {
			  phpta[0].v[stepby] = pt1[stepby];
			  cmpta[0].v[stepby] = cmpta[0].v[other] = MISSING;
		      first_stepby = floor(cmpta[0].v[stepby]);
		    }
		    else if (icounter == nis-1) {
			  phpta[0].v[stepby] = pt2[stepby];
			  cmpta[0].v[stepby] = cmpta[0].v[other] = MISSING;
		    }
		    else {
			  cmpta[0].v[stepby] = first_stepby + icounter;
			  iflag[locked] = iflag[other] = iflag[dimavg] = 0;
			  cmpta[0].v[other] = MISSING;
			  phpta[0].v[other] = phpta[0].v[stepby] = MISSING;
			  convert(phpt, cmpt, iflag, *dims, coord_dep, 1);
		    }

		    phpta[0].v[other] = slope*phpta[0].v[stepby] + intercept;
		    iflag[locked] = iflag[other] = iflag[dimavg] = 1;
		    cmpta[0].v[dimavg] = MISSING;
		    phpta[1] = phpta[0];
		    cmpta[1] = cmpta[0];
		    phpta[1].v[dimavg] = max[dimavg];
		    convert(phpt, cmpt, iflag, *dims, coord_dep, 2);

		    if (cmpta[0].v[dimavg] < cpmin[dimavg]) {
			cmpta[0].v[dimavg] = cpmin[dimavg];
			phpta[0].v[dimavg] = MISSING;
			convert(phpta, cmpta, iflag, *dims, coord_dep, 1);
		    }
		    cmin = cmpta[0].v[dimavg];
		    if (cmpta[1].v[dimavg] < 0 || cmpta[1].v[dimavg] > cpmax[dimavg]) {
			cmpta[1].v[dimavg] = cpmax[dimavg];
			phpta[1].v[dimavg] = MISSING;
			convert(phpta+1, cmpta+1, iflag, *dims, coord_dep, 1);
		    }
		    cmax = cmpta[1].v[dimavg];
		    if (cmin == cmax) {
			  slab_l.slab[icounter] = 
				(float)interp_(data,nx,ny,nz,nt,		
					   &cmpt[0].x,&cmpt[0].y,
					   &cmpt[0].z,&cmpt[0].t,
					   special);
		    }
		    else {
			nda = ceil(cmax) - floor(cmin) + 1;
			if (nda > 2) {
			    cmpta[nda-1] = cmpta[1];
			    phpta[nda-1] = phpta[1];
			    for (i=1; i < nda-1; i++)
				cmpta[i].v[dimavg] = (int) (cmin+i);
			    for (i=0; i < *dims; i++) {
				if (*surface && i == 2)
				    phpta[1].v[2] = MISSING;
				else if (i != dimavg && i != dfree)
				    cmpta[1].v[i] = MISSING;
				else phpta[1].v[i] = MISSING;
			    }
			    convert(phpt+1, cmpt+1, iflag, *dims, coord_dep, 
				    nda-2);
			}
			sum = dx = 0.;
			for(tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    tnext = MIN(tt+1, nda-1);
			    if ((value = (float) interp_(data,nx,ny,nz,nt,
						&cmpt[tt].x,&cmpt[tt].y,
						&cmpt[tt].z,&cmpt[tt].t,
						special)) != *special) {
				sum += value*(phpta[tnext].v[dimavg]
					      -phpta[tprev].v[dimavg])*0.5;
				dx += (phpta[tnext].v[dimavg]
				       -phpta[tprev].v[dimavg])*0.5;
			    }
			}
			if (dx == 0.)
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum/dx;
		    }
		    slab_l.ploc[icounter] = phpta[0].v[stepby];
		}
		free(cmpt);
		free(phpt);
	    }
	    /*
	       Well, that didn't work, now we have to depend on the
	       user transform to do our work for us.
	       */
	    else {
		struct point4a endpt[2];
		struct point4 *data_slicer_1d_trans_();
		int da1[2];

		da1[0] = dimavg+1;
		da1[1] = 0;
	        for (i=0; i < *dims; i++) {
		    if (i == stepby) {
			endpt[0].v[i] = pt1[i];
			endpt[1].v[i] = pt2[i];
		    }
		    else if (i==other) {
			endpt[0].v[i] = pt1[stepby]*slope+intercept;
			endpt[0].v[i] = pt2[stepby]*slope+intercept;
		    }
		    else if (i == dimavg) {
			endpt[0].v[i] = min[i];
			endpt[1].v[i] = max[i];
		    }
		    else endpt[0].v[i] = endpt[1].v[i] = pt1[i];
		}
		cmpt = data_slicer_1d_trans_(endpt, &nis, da1, &nda);
		if (cmpt == 0) return((float *)0);
		if ((phpt=(struct point4 *)malloc(nda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    free((char *)cmpt);
		    return((float *) 0);
		}
		phpta = (struct point4a *)phpt;
		if ((slab_l.slab = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - slab");
			free((char *)phpt);
			free((char *)cmpt);
			return((float *) 0);
		    }
		if ((slab_l.ploc = (float *)malloc(nis * sizeof(float)))
		== (float *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - ploc");
			free((char *)slab_l.slab);
			free((char *)phpt);
			free((char *)cmpt);
			return((float *) 0);
		    }
		for(icounter = 0 ; icounter < nis; icounter++){
		    struct point4 *cmpt0=cmpt+nda*icounter;

		    (void)index_2_phys_trans_(phpt, cmpt0, iflag, dims, &nda);
		    if (nda == 1) {
			slab_l.slab[icounter] = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt0[0].x,&cmpt0[0].y,
					   &cmpt0[0].z,&cmpt0[0].t,
					   special);
			slab_l.ploc[icounter] = phpta[icounter].v[stepby];
		    }
		    else {
			sum = dx = 0.;
			for (tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    if (phpta[tt].v[dimavg] == max[dimavg]) tnext = tt;
			    else tnext = MIN(tt+1, nda-1);
			    if ((value = (float) interp_(data,nx,ny,nz,nt,
						     &cmpt0[tt].x,&cmpt0[tt].y,
						     &cmpt0[tt].z,&cmpt0[tt].t,
						     special)) != *special) {
				sum += value*(phpta[tnext].v[dimavg]
					      -phpta[tprev].v[dimavg])*0.5;
				dx += (phpta[tnext].v[dimavg]
				       -phpta[tprev].v[dimavg])*0.5;
			    }
			    if(phpta[tt].v[dimavg] == max[dimavg]) break;
			}
			if (dx == 0.)
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum/dx;
		    }
		    slab_l.ploc[icounter] = phpta[0].v[stepby];
		}
		free(cmpt);
		free(phpt);
	    }
	    slab_l.xaxis = stepby+1;
	    slab_l.yaxis = 0;
	    slab_l.slope = slope;
	    slab_l.intercept = intercept;
	    slab_l.stepby = stepby;
	    slab_l.other = other;
	    slab_l.dimavg1 = dimavg;
	    slice_com_.slope=slope;
	    slice_com_.intercept=intercept;
	    slice_com_.ind=stepby+1;
	    slice_com_.dep=other+1;
	}
	/*
	   Figure out where in the slice is the window.
	   */
	wp.imin = 0;
	wp.imax = nis-1;
	wp.x1 = slab_l.ploc[0];
	wp.x2 = slab_l.ploc[nis-1];
	if (min[slab_l.stepby] < max[slab_l.stepby]) {
	    if (wp.x1 < min[slab_l.stepby]) wp.x1 = min[slab_l.stepby];
	    if (wp.x2 > max[slab_l.stepby]) wp.x2 = max[slab_l.stepby];
	}
	else {
	    if (wp.x1 > min[slab_l.stepby]) wp.x1 = min[slab_l.stepby];
	    if (wp.x2 < max[slab_l.stepby]) wp.x2 = max[slab_l.stepby];
	}
	pstart = slab_l.slope*wp.x1 + slab_l.intercept;
	pend = slab_l.slope*wp.x2 + slab_l.intercept;
	if (min[slab_l.other] < max[slab_l.other]) {
	    if (pstart < min[slab_l.other])
		wp.x1 = (min[slab_l.other]-slab_l.intercept)
		    /slab_l.slope;
	    if (pend > max[slab_l.other]) 
		wp.x2 = (max[slab_l.other]-slab_l.intercept)
		    /slab_l.slope;
	}
	else {
	    if (pstart > min[slab_l.other])
		wp.x1 = (min[slab_l.other]-slab_l.intercept)
		    /slab_l.slope;
	    if (pend < max[slab_l.other]) 
		wp.x2 = (max[slab_l.other]-slab_l.intercept)
		    /slab_l.slope;
	}
	if (wp.x1 < wp.x2) {
	    for(icounter=0; icounter < nis; icounter++) {
		if (slab_l.ploc[icounter] <= wp.x1)
		    wp.imin = icounter;
		if (slab_l.ploc[icounter] < wp.x2)
		    wp.imax = icounter;
	    }
	}
	else {
	    for(icounter=0; icounter < nis; icounter++) {
		if (slab_l.ploc[icounter] >= wp.x1)
		    wp.imin = icounter;
		if (slab_l.ploc[icounter] > wp.x2)
		    wp.imax = icounter;
	    }
	}
	wp.imax = MIN(wp.imax+1, nis-1);
	pt1[slab_l.stepby] = wp.x1;
	pt1[slab_l.other] = wp.x1*slab_l.slope + intercept;
	pt2[slab_l.stepby] = wp.x2;
	pt2[slab_l.other] = wp.x2*slab_l.slope + intercept;
    }
    wp.xaxis = slab_l.xaxis;
    wp.yaxis = slab_l.yaxis;
    *ni = wp.imax - wp.imin + 1;
    ix = wp.xaxis%10 - 1;
    if ((wslab = (float *)malloc(*ni * sizeof(float)))
	== (float *)0)
    {
	(void)make_help_widget_
	    ("slicer: can't allocate memory - wslab");
	return((float *) 0);
    }
    if ((*ploc = (float *)malloc(*ni * sizeof(float)))
	== (float *)0)
    {
	free(wslab);
	(void)make_help_widget_
	    ("slicer: can't allocate memory - loc");
	return((float *) 0);
    }
    if (min[ix] < max[ix]) {
	if (wp.x1 <= slab_l.ploc[0]) is = 0;
	else is = 1;
	if (wp.x2 >= slab_l.ploc[nis-1]) ie = *ni;
	else ie = *ni-1;
    }
    else {
	if (wp.x1 >= slab_l.ploc[0]) is = 0;
	else is = 1;
	if (wp.x2 <= slab_l.ploc[nis-1]) ie = *ni;
	else ie = *ni-1;
    }
    if (is == 1) {
	(*ploc)[0] = wp.x1;
	if (slab_l.slab[wp.imin] == *special ||
	    ((wp.x1 != slab_l.ploc[wp.imin]) &&
	     slab_l.slab[wp.imin+1] == *special))
	    wslab[0] = *special;
	else
	    wslab[0] = slab_l.slab[wp.imin] +
		(slab_l.slab[wp.imin+1]-slab_l.slab[wp.imin])*
		    (wp.x1-slab_l.ploc[wp.imin])/
			(slab_l.ploc[wp.imin+1]-slab_l.ploc[wp.imin]);
    }
    for (i=is; i < ie; i++) {
	(*ploc)[i] = slab_l.ploc[i+wp.imin];
	wslab[i] = slab_l.slab[i+wp.imin];
    }
    if (ie == *ni-1) {
	(*ploc)[*ni-1] = wp.x2;
	if (slab_l.slab[*ni-2+wp.imin] == *special ||
	    ((wp.x2 != slab_l.ploc[*ni-1+wp.imin]) &&
	     slab_l.slab[*ni-1+wp.imin] == *special))
	    wslab[*ni-1] = *special;
	else
	    wslab[*ni-1] = slab_l.slab[*ni-2+wp.imin] +
		(slab_l.slab[*ni-1+wp.imin]-slab_l.slab[*ni-2+wp.imin])*
		    (wp.x2-slab_l.ploc[*ni-2+wp.imin])/
		       (slab_l.ploc[*ni-1+wp.imin]-slab_l.ploc[*ni-2+wp.imin]);
    }
    /*
       Scale data array.
       */
    getdvar_("data_slope", &data_slope, &i, 10);
    getdvar_("data_intercept", &data_intercept, &i, 14);
    scale_(wslab, ni, &data_slope, &data_intercept, special);
    wp.y1 = find_min(wslab, *ni, *special);
    wp.y2 = find_max(wslab, *ni, *special);
    /*
       Reverse axis if Z is the X axis
       */
    if (ix == 2) {
        float *temp, tmp;

	ix = wp.xaxis;
        wp.xaxis = wp.yaxis;
        wp.yaxis = ix;
        temp = *ploc;
        *ploc = wslab;
        wslab = temp;
	tmp = wp.y1;
	wp.y1 = wp.x1;
	wp.x1 = tmp;
	tmp = wp.y2;
	wp.y2 = wp.x2;
	wp.x2 = tmp;
    }
    window_points_ = wp;
    return(wslab);
}
