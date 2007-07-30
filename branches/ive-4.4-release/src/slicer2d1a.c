static char rcsid[] = "$Id: slicer2d1a.c,v 1.23 2005/11/03 19:51:57 warren Exp $";
/* $Id: slicer2d1a.c,v 1.23 2005/11/03 19:51:57 warren Exp $
 * Purpose:
 *   This routine is used to compute plane in space through a 
 *   four dimentional data set where one of the dimensions is
 *   reduced by averaging. This code was inspired by Fundamentals
 *   of Interactive Computer Graphics - J. D. Foley and A. Van Dam
 *   revision 1 page 512.
 *
 *   data:      the data set to slice [*ny][*nx][*nt][*nz]
 *   dims:      number of dimensions of the data
 *   pt?:       the 3 points that define a plane (note: if all three are
 *                                              colinear we have a line)
 *		On exit pt1 is the first point, pt2 is the last point
 *		of the first row (used by terrain routines) if the
 *		yaxis is z.
 *   da:	the dimension to average (1-4).
 *   nj:	number of points in first dimension of slice
 *   ni:	number of points in second dimension of slice
 *   special:   special value
 *   phys:      If one - do evenly space slice in physical space
 *              If two - do at physical points of last slice.
 *   surface:   If one - surface plot.
 *
 *   $Log: slicer2d1a.c,v $
 *   Revision 1.23  2005/11/03 19:51:57  warren
 *   Fixed redraw command to really redraw plots
 *
 *   Revision 1.22  2005/09/06 20:22:15  warren
 *   Move updates into current stream
 *
 *   Revision 1.21.2.1  2005/09/06 17:55:24  warren
 *   fixed trajectories to deal with maps. Also first part of bold arrows for
 *   trajectories
 *
 *   Revision 1.21  2002/08/09 19:58:01  warren
 *   Fixed for Linux and tested with DEC sun etc.
 *   most items inside #ifdef's, or were general cleanup/fixes
 *
 *   Revision 1.20  2000/08/24 19:06:11  harry
 *   Fix out of bound problem with average dimensions.
 *
 *   Revision 1.19  2000/08/02 19:59:45  harry
 *   Fix surface vector plotting.
 *
 *   Revision 1.18  2000/04/05 22:38:56  harry
 *   Correct bugs in general surface plotting.
 *
 *   Revision 1.17  1998/09/18 17:34:51  harry
 *   Fix declaration of corner.
 *
 *   Revision 1.16  1998/09/03 19:19:26  harry
 *   Add in 2d line plots - correct bugs in other averaging.
 *
 *   Revision 1.15  1998/07/16 22:11:00  harryive
 *   Add two dimensional slicing with 2 averaged dimensions.
 *
 *   Revision 1.14  1997/12/12 22:59:42  harry
 *   Fix potental problem when there are 2 points in the average.
 *
 *   Revision 1.13  1997/12/12 22:20:00  harry
 *   *** empty log message ***
 *
 *   Revision 1.12  1997/07/25 15:50:41  harry
 *   Make slicer2d1a.c match latest changes in slicer.c for physical slices.
 *
 *   Revision 1.11  1997/03/25 18:43:19  harry
 *   Make sure fixmap structure is properly set (i.e. min < max).
 *
 *   Revision 1.10  1997/01/28 22:02:08  harry
 *   Fix surface plotting problem.
 *
 *   Revision 1.9  1996/10/16 18:27:19  harry
 *   Serious error in an arbitrary slice that "comes in the bottom and goes
 *   out the top".
 *
 *   Revision 1.8  1996/08/09 22:53:15  harry
 *   Fix problem with "reversed" axes and windowing.
 *
 *   Revision 1.7  1996/06/24 19:52:03  harry
 *   Fix bug in arbitrary angle vertical 2d slicer.
 *
 *   Revision 1.6  1996/06/21 16:07:43  harry
 *   Add new option to slicer (phys = 2) to properly get values for
 *   vectors.  By using this option, we get the second component of the
 *   vector on the same grid as the first component.
 *
 *   Revision 1.5  1996/06/19 23:24:09  harry
 *   Correctly reset spval, including the local copy.
 *
 *   Revision 1.4  1996/03/27 23:54:06  harry
 *   Slicer should check new variable "field_pointer" to find out name of
 *   field currently being sliced instead of "field" to properly handle
 *   vectors and other future plots (like Skew-T).
 *
 *   Revision 1.3  1996/02/23 22:31:40  harry
 *   Fix for reset_spval feature for new slicer.
 *
 * Revision 1.2  1995/10/18  19:18:34  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.1  1995/09/05  20:34:15  harry
 * Add new slicer.  Good luck!
 *
 *
 */
#include <stdlib.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#ifdef Linux
#include <tgmath.h>
#else
#include <math.h>
#endif
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

static float *slab=0;
static int nis, njs;

struct point {
    float x,y;
};
struct point2{
    struct point **pt;
    int numx;
    int numy;
} ;

extern struct point2 phpts;

struct minmax {
    float minx;
    float maxx;
    float miny;
    float maxy;
} fixmap; /* used only for horizontal plots on a map */

extern struct {
    int locked, dimavg1, dimavg2, rconst, phys;
    float A, B, C, D, avgmin[2], avgmax[2], vlocked, *slab;
    int dfree, stepby, other;
    int xaxis, yaxis;
    float slope, intercept, *data, ri, rj;
    char datfil[256], field[256];
} slab_d;

extern double interp_();

struct wpt wp;

float 
  *slicer2d1a_(data, nx, ny, nz, nt,
	   dims, pt1, pt2, pt3, da, nj, ni, special, phys, surface)
float *data, *pt1, *pt2, *pt3, *special;
int *dims, *nx, *ny, *nz, *nt, *da, *ni, *nj, *phys, *surface;
{
    char datfil[256], field[256];
    double dmax[4],dmin[4],dfmin[4],dfmax[4];
    float *wslab, min[4], max[4], fullmin[4], fullmax[4];
    float ctest[4], ptest[4], c1[4], c2[4];
    float tester, slope, intercept, pstart, pend;
    float A, B, C, D;/*Ax +By +Cz +D = 0 is a plane*/
    float pother, value, sum, dx;
    float cmin, cmax;
    int tt, tnext, tprev, nda, ntda;
    int cpmin[4], cpmax[4], coord_dep[4][4], indep[4];
    int dcounter, icounter, iflag[4], i, j, v[4], rconst, zeroes;
    int error, stepby, other, dfree, good;
    int ii, jj, same_slice, ix, iy, dimavg;
    static int one=1;
    struct point4{
	float x,y,z,t;
    } *cmpt, *phpt;
    struct point4a{
	float v[4];
    } *cmpta, *phpta, cavg[2], pavg[2], *fcmpta;
    struct point *phtmp;

    if( *dims != 4 ) {
	(void)make_help_widget_("slicer2d1a: must be 4 dimensional field");
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
    dimavg = *da - 1;
    i = 0;
    for (j = 0; j < 4; j++) {
	if (j != dimavg) v[i++] = j;
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
	slab_d.D == D && slab_d.dimavg1 == dimavg &&
	slab_d.dimavg2 == -1 &&
	slab_d.avgmin[0] == min[dimavg] && slab_d.avgmax[0] == max[dimavg])
	same_slice = 1;
    else {
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
	slab_d.dimavg1 = -1;	/* In case something goes wrong below */
	slab_d.dimavg2 = -1;
	slab_d.locked = -1;
	slab_d.avgmin[0] = min[dimavg];
	slab_d.avgmax[0] = max[dimavg];
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
	    (void)make_help_widget_("slicer2d1a: empty slice");
	    return (float *)0;
	}		
	/*
	   See if computational slicing is possible.

	   We have one average coordinate: dimavg in phys or comp space
	   We have one locked coordinates: rconst in phys space
	   We have two free coordinates: ii & jj in comp space

	   First, if ii and/or jj depend on dimavg, we cannot do it in
	   computational space.
	   */
	if (*phys == 0) {
	    good = 0;
	    if (coord_dep[ii][dimavg] == 0 && coord_dep[jj][dimavg] == 0) {
		/*
		  f[dimavg] != ii && f[dimavg] != jj

		  Call convert with dimavg phys to see if it will work.
		*/
		for (icounter=0; icounter < 4; icounter++) iflag[icounter] = 1;
		ctest[ii] = cpmin[ii];
		ctest[jj] = cpmin[jj];
		ctest[dimavg] = MISSING;
		ptest[dimavg] = dfmin[dimavg];
		if (*surface) {
		    ctest[2] = 1;
		    ptest[2] = MISSING;
		}
		else {
		    ctest[rconst] = MISSING;
		    ptest[rconst] = dfmin[rconst];
		}
		ptest[ii] = ptest[jj] = MISSING;
		if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
		    /*
		       Okay, now try it with dimavg comp.
		       */
		    ctest[ii] = cpmin[ii];
		    ctest[jj] = cpmin[jj];
		    ctest[dimavg] = cpmin[dimavg];
		    if (*surface) {
			ctest[2] = 1;
			ptest[2] = MISSING;
		    }
		    else {
			ctest[rconst] = MISSING;
			ptest[rconst] = dfmin[rconst];
		    }
		    ptest[ii] = ptest[jj] = ptest[dimavg] = MISSING;
		    good = convert(ptest, ctest, iflag, *dims, coord_dep, 1);
		}
	    }
	}
	if (*phys == 0 && good) {
	    /*get memory*/
	    ntda = cpmax[dimavg] - cpmin[dimavg] + 1;
	    if ((phpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((cmpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpta = (struct point4a *)cmpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    if ((phpts.pt =  
		 (struct point **)malloc(nis * sizeof(struct point *))) == 
		( struct point **)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - phpts");
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
			("slicer2d1a: can't allocate memory - phpts");
		    for(j=0; j < icounter; j++) free((char *)phpts.pt[j]);
		    free((char *)phpts.pt);
		    free((char *)slab);
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
	
	    phpts.numx = nis;
	    phpts.numy = njs;
	    /*find line*/
	    if (*surface) for (i=0; i < ntda; ++i) cmpta[i].v[2] = 1;
	    else for (i=0; i < ntda; ++i) phpta[i].v[rconst] = slope;
	    /* move line through jj */
	    for(dcounter = 0 ; dcounter < njs; dcounter++){
		for (i=0; i < ntda; ++i) cmpta[i].v[jj] = cpmin[jj] + dcounter;
		for(icounter = 0 ; icounter < nis; icounter++){
		    /*
		       Find the first and last point in the average.
		       */
		    cmpta[0].v[ii] = cpmin[0] + icounter;
		    if (*surface) phpta[0].v[2] = MISSING;
		    else cmpta[0].v[rconst] = MISSING;
		    cmpta[0].v[dimavg] = MISSING;
		    phpta[0].v[ii] = phpta[0].v[jj] = MISSING;
		    cmpta[1] = cmpta[0];
		    phpta[1] = phpta[0];
		    phpta[0].v[dimavg] = min[dimavg];
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
			slab_dep(icounter,dcounter) =
			    (float) interp_(data,nx,ny,nz,nt,
					    &cmpt[0].x,&cmpt[0].y,
					    &cmpt[0].z,&cmpt[0].t,special);
		    }
		    else {
			nda = ceilf(cmax) - floorf(cmin) + 1;
			if (nda > 2) {
			    cmpta[nda-1] = cmpta[1];
			    phpta[nda-1] = phpta[1];
			    for (i=1; i < nda-1; i++) {
				cmpta[i].v[ii] = cmpta[0].v[ii];
				cmpta[i].v[dimavg] = (int) (cmin+i);
			    }
			    if (*surface) phpta[1].v[2] = MISSING;
			    else cmpta[1].v[rconst] = MISSING;
			    phpta[1].v[ii] = phpta[1].v[jj] 
				= phpta[1].v[dimavg] = MISSING;
			    convert(phpta+1, cmpta+1, iflag,
				    *dims, coord_dep, nda-2);
			}
			sum = dx = 0.;
			for (tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    tnext = MIN(tt+1, nda-1);
			    if ((value = (float)interp_(data,nx,ny,nz,nt,
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
			    slab_dep(icounter,dcounter) = *special;
			else
			    slab_dep(icounter,dcounter) = sum/dx;
		    }
		    phtmp = phpts.pt[icounter];
		    phtmp[dcounter].x = phpta[0].v[ii];
		    phtmp[dcounter].y = phpta[0].v[jj];
		}
	    }
	    free(cmpt);
	    free(phpt);
	}
	/*
	  Use physical points from last slice.
	  */
	else if (*phys == 2) {
	    /*
	      See if this slice is possible.
	    */
	    for (icounter = 0; icounter < 4; icounter++) iflag[icounter] = 1;
	    ctest[ii] = ctest[jj] = ctest[rconst] = MISSING;
	    ptest[dimavg] = MISSING;
	    ptest[ii] = dfmin[ii];
	    ptest[jj] = dfmin[jj];
	    ptest[rconst] = dfmin[rconst];
	    ctest[dimavg] = cpmin[dimavg];
	    if (*surface) {
		ptest[2] = MISSING;
		ctest[2] = 1;
	    }
	    if (!convert(ptest, ctest,iflag, *dims, coord_dep, 1)) {
		(void) make_help_widget
		    ("slicer2d1a: cannot do this slice - notify developers");
		return((float *) 0);
	    }		
	    nis = phpts.numx;
	    njs = phpts.numy;
	    /*get memory*/
	    ntda = cpmax[dimavg] - cpmin[dimavg] + 1;
	    if ((phpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((cmpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpta = (struct point4a *)cmpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    for (i=0; i < ntda; i++) phpta[i].v[rconst] = slope;
	    for (dcounter = 0; dcounter < njs; dcounter++) {
		for (icounter = 0; icounter < nis; icounter++) {
		    int two = 2;

		    phtmp = phpts.pt[icounter];
		    /*
		      Find the first and last point in the average.
		      */
		    phpta[0].v[ii] = phtmp[dcounter].x;
		    phpta[0].v[jj] = phtmp[dcounter].y;
		    phpta[1] = phpta[0];
		    phpta[0].v[dimavg] = min[dimavg];
		    phpta[1].v[dimavg] = max[dimavg];
		    iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
		    if (*surface) {
			cmpta[0].v[2] = cmpta[1].v[2] = 1;
			phpta[0].v[2] = MISSING;
			convert(phpta, cmpta, iflag, *dims, coord_dep, 2);
		    }
		    else
			phys_2_index_trans_(phpta, cmpta, iflag, dims, &two);
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
			slab_dep(icounter,dcounter) =
			    (float) interp_(data,nx,ny,nz,nt,
					    &cmpt[0].x,&cmpt[0].y,
					    &cmpt[0].z,&cmpt[0].t,special);
		    }
		    else {
			nda = ceilf(cmax) - floorf(cmin) + 1;
			if (nda > 2) {
			    cmpta[nda-1] = cmpta[1];
			    phpta[nda-1] = phpta[1];
			    for (i=1; i < nda-1; i++) {
				phpta[i].v[ii] = phpta[0].v[ii];
				phpta[i].v[jj] = phpta[0].v[jj];
				cmpta[i].v[dimavg] = (int) (cmin+i);
			    }
			    cmpta[1].v[rconst] = cmpta[1].v[ii]
				= cmpta[1].v[jj] = MISSING;
			    phpta[1].v[dimavg] = MISSING;
			    if (*surface) {
				for (i=1; i < nda-1; i++) cmpta[i].v[2] = 1;
				phpta[1].v[2] = MISSING;
			    }
			    convert(phpta+1, cmpta+1, iflag,
				    *dims, coord_dep, nda-2);
			}
			sum = dx = 0.;
			for (tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    tnext = MIN(tt+1, nda-1);
			    if ((value = (float)interp_(data,nx,ny,nz,nt,
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
			    slab_dep(icounter,dcounter) = *special;
			else
			    slab_dep(icounter,dcounter) = sum/dx;
		    }
		}
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
	    int ii1 = ii+1, jj1 = jj+1, da1[2];

	    da1[0] = dimavg+1;
	    da1[1] = 0;
	    corner[0].v[dimavg] = corner[1].v[dimavg] = min[dimavg];
	    corner[2].v[dimavg] = corner[3].v[dimavg] = max[dimavg];
	    corner[0].v[rconst] = corner[1].v[rconst] = 
		corner[2].v[rconst] = corner[3].v[rconst] = slope;
	    corner[0].v[ii] = corner[2].v[ii] = fullmin[ii];
	    corner[1].v[ii] = corner[3].v[ii] = fullmax[ii];
	    corner[0].v[jj] = corner[1].v[jj] = fullmin[jj];
	    corner[2].v[jj] = corner[3].v[jj] = fullmax[jj];
	    if (*phys) compute = pslicer2d_(corner, &ii1, &jj1, &nis, &njs,
					    da1, &nda, &slab_d.ri,
					    &slab_d.rj);
	    else compute = data_slicer_2d_trans_(corner, &ii1, &jj1, &nis,
						 &njs, da1, &nda,
						 &slab_d.ri, &slab_d.rj);
	    if (compute == 0) return((float *)0);
	    if ((phpt=(struct point4 *)malloc(nda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - phpt");
		free((char *)compute);
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)compute);
		return((float *) 0);
	    }
	    if ((phpts.pt =  
		 (struct point **)malloc(nis * sizeof(struct point *))) == 
		( struct point **)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - phpts");
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
			("slicer2d1a: can't allocate memory - phpts");
		    for(j=0; j < i; j++) free((char *)phpts.pt[j]);
		    free((char *)phpts.pt);
		    free((char *)slab);
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
		for(icounter = 0 ; icounter < nis; icounter++){
		    cmpt = compute + (nis*dcounter + icounter)*nda;
		    (void)index_2_phys_trans_(phpt, cmpt, iflag, dims, &nda);
		    if (nda == 1) {
			slab_dep(icounter,dcounter) = 
			    (float) interp_(data,nx,ny,nz,nt,
					    &cmpt[0].x,&cmpt[0].y,
					    &cmpt[0].z,&cmpt[0].t,special);
		    }
		    else {
			sum = dx = 0.;
			for (tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    if (phpta[tt].v[dimavg] == max[dimavg]) tnext = tt;
			    else tnext = MIN(tt+1, nda-1);
			    if((value = (float)interp_(data,nx,ny,nz,nt,
						&cmpt[tt].x,&cmpt[tt].y,
						&cmpt[tt].z,&cmpt[tt].t,
						special)) != *special) {
				sum += value*(phpta[tnext].v[dimavg]
					      -phpta[tprev].v[dimavg])*0.5;
				dx += (phpta[tnext].v[dimavg]
				       -phpta[tprev].v[dimavg])*0.5;
			    }
			    if (phpta[tt].v[dimavg] == max[dimavg]) break;
			}
			if (dx == 0.)
			    slab_dep(icounter,dcounter) = *special;
			else
			    slab_dep(icounter,dcounter) = sum/dx;
		    }
		    phtmp = phpts.pt[icounter];
		    phtmp[dcounter].x = phpta[0].v[ii];
		    phtmp[dcounter].y = phpta[0].v[jj];
		}
	    }
	    free(compute);
	    free(phpt);
	}
	slab_d.xaxis = ii+1;
	slab_d.yaxis = jj+1;
	slab_d.dimavg1 = dimavg;
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
	if (slab_d.yaxis == 3 || slab_d.xaxis == 3) {
	    for (i=0; i<4; i++) {
		pt1[i] = min[i];
		pt3[i] = max[i];
	    }
	    pt3[slab_d.rconst] = pt1[slab_d.rconst] = pt2[slab_d.rconst];
	    pt3[slab_d.dimavg1] = pt1[slab_d.dimavg1] = pt2[slab_d.dimavg1];
	}
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
	  Use physical points of last slice.
	  */
	if (*phys == 2) {
	    /*
	      See if this slice is possible.
	    */
	    for (icounter = 0; icounter < 4; icounter++) iflag[icounter] = 1;
	    ctest[stepby] = ctest[other] = ctest[rconst] = MISSING;
	    ptest[dimavg] = MISSING;
	    ptest[stepby] = dfmin[stepby];
	    ptest[other] = dfmin[other];
	    ptest[rconst] = dfmin[rconst];
	    ctest[dimavg] = cpmin[dimavg];
	    if (!convert(ptest, ctest,iflag, *dims, coord_dep, 1)) {
		(void) make_help_widget
		    ("slicer2d1a: cannot do this slice - notify developers");
		return((float *) 0);
	    }		
	    stepby = slab_d.stepby;
	    other = slab_d.other;
	    ntda = cpmax[dimavg] - cpmin[dimavg] + 1;
	    nis = phpts.numx;
	    njs = phpts.numy;

	    /*get memory*/
	    if ((phpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpta = (struct point4a *) phpt;
	    if ((cmpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d1a: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpta = (struct point4a *) cmpt;

	    for (dcounter = 0; dcounter < njs; dcounter++) {
		for (icounter = 0; icounter < nis; icounter++) {
		    int two = 2;

		    phtmp = phpts.pt[icounter];
		    /*
		      Find the first and last point in the average.
		      */
		    phpta[0].v[dfree] = phtmp[dcounter].x;
		    phpta[0].v[stepby] = phtmp[dcounter].y;
		    phpta[0].v[other] = slope*phpta[0].v[stepby] + intercept;
		    phpta[1] = phpta[0];
		    phpta[0].v[dimavg] = min[dimavg];
		    phpta[1].v[dimavg] = max[dimavg];
		    iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
		    phys_2_index_trans_(phpt, cmpt, iflag, dims, &two);
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
			slab_dep(icounter,dcounter) =
			    (float) interp_(data,nx,ny,nz,nt,
					    &cmpt[0].x,&cmpt[0].y,
					    &cmpt[0].z,&cmpt[0].t,special);
		    }
		    else {
			nda = ceilf(cmax) - floorf(cmin) + 1;
			if (nda > 2) {
			    cmpta[nda-1] = cmpta[1];
			    phpta[nda-1] = phpta[1];
			    for (i=1; i < nda-1; i++) {
				phpta[i].v[stepby] = phpta[0].v[stepby];
				phpta[i].v[other] = phpta[0].v[other];
				cmpta[i].v[dimavg] = (int) (cmin+i);
			    }
			    cmpta[1].v[rconst] = cmpta[1].v[stepby] 
				= cmpta[1].v[other] = MISSING;
			    phpta[1].v[dimavg] = MISSING;
			    convert(phpta+1, cmpta+1, iflag,
				    *dims, coord_dep, nda-2);
			}
			sum = dx = 0.;
			for (tprev=tt=0; tprev < nda-1; tprev=tt, tt=tnext) {
			    tnext = MIN(tt+1, nda-1);
			    if ((value = (float)interp_(data,nx,ny,nz,nt,
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
			    slab_dep(icounter,dcounter) = *special;
			else
			    slab_dep(icounter,dcounter) = sum/dx;
		    }
		}
	    }
	    free(cmpt);
	    free(phpt);
	}
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
	    if (fabsf(c1[ix]-c2[ix]) > fabsf(c1[iy]-c2[iy])) {
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
	      We have one averaged coordinate: dimavg in phys or comp space
	      We have one free coordinate dfree in comp space
	      We have two coordinates that depend on each other:
	      stepby (in comp space) and other (dep on stepby in phys space).
	      
	      First, all is lost if dfree, stepby or other depend on dimavg.
	      */
	    if (*phys == 0) {
		good = 0;
		if (coord_dep[other][dimavg] == 0 && coord_dep[stepby][dimavg] == 0
		    && coord_dep[dfree][dimavg] == 0) {
		    /*
		      Next, can we get stepby to physical coord with the
		      information we currently have.
		      */	
		    iflag[stepby] = 1;
		    iflag[dfree] = iflag[other] = iflag[dimavg] = 0;
		    ctest[dfree] = cpmin[dfree];
		    ctest[stepby] = cpmin[stepby];
		    ctest[other] = ctest[dimavg] = MISSING;
		    ptest[dimavg] = dfmin[dimavg];
		    ptest[other] = ptest[stepby] = ptest[dfree] = MISSING;
		    if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
			/*
			  Okay, now that we have stepby in phys, we can get other
			  in phys.  Now, can we calculate everything else?
			  */
			ptest[other] = dfmin[other];
			ctest[stepby] = ctest[dimavg] = MISSING;
			ptest[dfree] = MISSING;
			iflag[dfree] = iflag[other] = iflag[dimavg] = 1;
			good = convert(ptest, ctest, iflag, *dims, coord_dep, 1);
		    }
		}
	    }
	    if (*phys == 0 && good) {
		ntda = cpmax[dimavg] - cpmin[dimavg] + 1;
		njs = cpmax[stepby] - cpmin[stepby] + 1;
		nis = cpmax[dfree] - cpmin[dfree] + 1;
		
		if(nis <2 || njs < 2) {
		    (void)make_help_widget_("slicer2d1a: empty slice");
		    return (float *)0;
		}
		
		/*get memory*/
		if ((phpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - phpt");
		    return((float *) 0);
		}
		phpta = (struct point4a *) phpt;
		if ((cmpt=(struct point4 *)malloc(ntda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - cmpt");
		    free((char *)phpt);
		    return((float *) 0);
		}
		cmpta = (struct point4a *) cmpt;
		
		if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		if ((fcmpta=(struct point4a *)malloc(nis
						     *sizeof(struct point4a)))
		    == (struct point4a *)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - fcmpt");
		    free((char *)slab_d.slab);
		    slab_d.slab = 0;
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
		
		if ((phpts.pt =  
		     (struct point **)malloc(nis 
					     * sizeof(struct point **))) == 
		    ( struct point **)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - phpts");
		    free((char *)slab);
		    free((char *)phpt);
		    free((char *)cmpt);
		    free((char *)fcmpta);
		    return((float *) 0);
		}
		
		for(i=0; i<nis; i++)
		    if ((phpts.pt[i] =  
			 (struct point *)malloc(njs
						* sizeof(struct point))) == 
			( struct point *)0)
		    {
			(void)make_help_widget_
			    ("slicer2d1a: can't allocate memory - phpts");
			free((char *)slab);
			free((char *)phpt);
			free((char *)cmpt);
			free((char *)fcmpta);
			return((float *) 0);
		    }
		phpts.numx = nis;
		phpts.numy = njs;
		
		/*find line*/
		/* move line through the line */
		
		for(dcounter = 0 ; dcounter < njs; dcounter++){
		    
		    if (dcounter == 0) phpta[0].v[stepby] = pt1[stepby];
		    else if (dcounter == njs-1) 
			phpta[0].v[stepby] = pt2[stepby];
		    for(icounter = 0 ; icounter < nis; icounter++){
			/*
			  Find the first and last point in the average.
			  */
			cmpta[0].v[dfree] = (float) (cpmin[dfree] + icounter);
			phpta[0].v[dimavg] = min[dimavg];
			cmpta[0].v[dimavg] = MISSING;
			iflag[stepby] = 1;
			if (dcounter == 0 || dcounter == njs-1) {
			    cmpta[0].v[stepby] = cmpta[0].v[other] 
				= phpta[0].v[dfree]= MISSING;
			}
			else {
			    cmpta[0].v[stepby] =
				floorf(fcmpta[icounter].v[stepby]) + dcounter;
			    iflag[dfree] = iflag[other] = iflag[dimavg] = 0;
			    cmpta[0].v[other] = cmpta[0].v[dimavg] = MISSING;
			    phpta[0].v[other] = phpta[0].v[stepby]
				= phpta[0].v[dfree] = MISSING;
			    convert(phpt, cmpt, iflag, *dims, coord_dep, 1);
			}
			phpta[0].v[other] = slope*phpta[0].v[stepby]
			    + intercept;
			iflag[dfree] = iflag[other] = iflag[dimavg] = 1;
			cavg[0] = cmpta[0];
			pavg[0] = phpta[0];
			cavg[1] = cmpta[0];
			pavg[1] = phpta[0];
			pavg[1].v[dimavg] = max[dimavg];
			cavg[0].v[dimavg] = MISSING;
			convert(pavg, cavg, iflag, *dims, coord_dep, 2);
			if (dcounter == 0) fcmpta[icounter] = cavg[0];
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
			    convert(phpta+1, cmpta+1, iflag, *dims,
				    coord_dep, 1);
			}
			cmax = cmpta[1].v[dimavg];
			if (cmin == cmax) {
			    slab_dep(icounter,dcounter) =
				(float) interp_(data,nx,ny,nz,nt,
						&cavg[0].v[0],&cavg[0].v[1],
						&cavg[0].v[2],&cavg[0].v[3],
						special);
			}
			else {
			    nda = ceilf(cmax) - floorf(cmin) + 1;
			    if (nda > 2) { 
				for (i=1; i < nda-1; ++i) {
				    cmpta[i] = cmpta[0];
				    phpta[i] = phpta[0];
				    cmpta[i].v[dimavg] = (int) (cmin+i);
				}
				phpta[1].v[dimavg] = MISSING;
				convert(phpta+1, cmpta+1, iflag, *dims,
					coord_dep, nda-2);
			    }
			    cmpta[0] = cavg[0];
			    phpta[0] = pavg[0];
			    cmpta[nda-1] = cavg[1];
			    phpta[nda-1] = pavg[1];
			    sum = dx = 0.;
			    for (tprev=tt=0; tprev<nda-1; tprev=tt,tt=tnext) {
				tnext = MIN((int) tt+1, nda-1);
				if ((value = (float)interp_(data,nx,ny,nz,nt,
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
				slab_dep(icounter,dcounter) = *special;
			    else
				slab_dep(icounter,dcounter) = sum/dx;
			}
			phtmp = phpts.pt[icounter];
			phtmp[dcounter].x = phpta[0].v[dfree];
			phtmp[dcounter].y = phpta[0].v[stepby];
		    }
		}
		free(fcmpta);
		free(cmpt);
		free(phpt);
	    }
	    /*
	      Well, that didn't work, now we have to depend on the
	      user transform to do our work for us.
	      */
	    else {
		struct point4a corner[4];
		struct point4 *compute, *pslicer2d_(),
		    *data_slicer_2d_trans_();
		int ii1 = dfree+1, jj1 = (stepby+1)*10 + other + 1, da1[2];

		da1[0] = dimavg+1;
		da1[1] = 0;
		corner[0].v[dimavg] = corner[1].v[dimavg] = min[dimavg];
		corner[2].v[dimavg] = corner[3].v[dimavg] = max[dimavg];
		corner[0].v[dfree] = corner[2].v[dfree] = fullmin[dfree];
		corner[1].v[dfree] = corner[3].v[dfree] = fullmax[dfree];
		corner[0].v[stepby] = corner[1].v[stepby] = pt1[stepby];
		corner[2].v[stepby] = corner[3].v[stepby] = pt2[stepby];
		corner[0].v[other] = corner[1].v[other] =
		    pt1[stepby]*slope+intercept;
		corner[2].v[other] = corner[3].v[other] =
		    pt2[stepby]*slope+intercept;
		if (*phys) compute = pslicer2d_(corner, &ii1, &jj1, &nis, &njs,
						da1, &nda, &slab_d.ri,
						&slab_d.rj);
		else compute = data_slicer_2d_trans_(corner, &ii1, &jj1, &nis,
						     &njs, da1, &nda,
						     &slab_d.ri, &slab_d.rj);
		if (compute == 0) return((float *)0);
		if ((phpt=(struct point4 *)malloc(nda*sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - phpt");
		    free((char *)compute);
		    return((float *) 0);
		}
		phpta = (struct point4a *) phpt;
		if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)compute);
		    return((float *) 0);
		}
		if ((phpts.pt =  
		     (struct point **)malloc(nis * sizeof(struct point *))) == 
		    ( struct point **)0)
		{
		    (void)make_help_widget_
			("slicer2d1a: can't allocate memory - phpts");
		    free((char *)slab_d.slab);
		    slab_d.slab = 0;
		    free((char *)phpt);
		    free((char *)compute);
		    return((float *) 0);
		}
		for(i=0; i<nis; i++)
		    if ((phpts.pt[i] =  
			 (struct point *)malloc(njs * sizeof(struct point)))== 
			(struct point *)0)
		    {
			(void)make_help_widget_
			    ("slicer2d1a: can't allocate memory - phpts");
			for(j=0; j < i; j++) free((char *)phpts.pt[j]);
			free((char *)phpts.pt);
			free((char *)slab);
			free((char *)phpt);
			free((char *)compute);
			return((float *) 0);
		    }
		
		phpts.numx = nis;
		phpts.numy = njs;
		iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
		/*find line*/
		/* move line */
		phpts.numx = nis;
		phpts.numy = njs;
		iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;
		/*find line*/
		/* move line through jj */
		for(dcounter = 0 ; dcounter < njs; dcounter++){
		    for(icounter = 0 ; icounter < nis; icounter++){
			cmpt = compute + (nis*dcounter + icounter)*nda;
			(void)index_2_phys_trans_(phpt, cmpt, iflag, 
						  dims, &nda);
			if (nda == 1) {
			    slab_dep(icounter,dcounter) = 
				(float) interp_(data,nx,ny,nz,nt,
						&cmpt[0].x,&cmpt[0].y,
						&cmpt[0].z,&cmpt[0].t,special);
			}
			else {
			    sum = dx = 0.;
			    for (tprev=tt=0; tprev<nda-1; tprev=tt, tt=tnext) {
				if (phpta[tt].v[dimavg] == max[dimavg])
				    tnext = tt;
				else tnext = MIN(tt+1, nda-1);
				if((value = (float)interp_(data,nx,ny,nz,nt,
						   &cmpt[tt].x,&cmpt[tt].y,
						   &cmpt[tt].z,&cmpt[tt].t,
						   special)) != *special) {
				    sum += value*(phpta[tnext].v[dimavg]
						  -phpta[tprev].v[dimavg])*0.5;
				    dx += (phpta[tnext].v[dimavg]
					   -phpta[tprev].v[dimavg])*0.5;
				}
				if (phpta[tt].v[dimavg] == max[dimavg]) break;
			    }
			    if (dx == 0.)
				slab_dep(icounter,dcounter) = *special;
			    else
				slab_dep(icounter,dcounter) = sum/dx;
			}
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
	slab_d.yaxis = stepby+11;
	slab_d.dfree = dfree;
	slab_d.stepby = stepby;
	slab_d.other = other;
	slab_d.slope = slope;
	slab_d.intercept = intercept;
	slab_d.dimavg1 = dimavg;
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
	wp.y1 = min[slab_d.stepby];
	wp.y2 = max[slab_d.stepby];
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
			    && pother > min[other]
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
	if (slab_d.yaxis == 3 || slab_d.xaxis == 3) {
	    for (i=0; i<4; i++) {
		pt1[i] = min[i];
		pt3[i] = max[i];
	    }
	    pt3[slab_d.dimavg1] = pt1[slab_d.dimavg1] = pt2[slab_d.dimavg1];
	    pstart = slab_d.slope*min[stepby] + slab_d.intercept;
	    pend =  slab_d.slope*max[stepby] + slab_d.intercept;
	    if (min[other] < max[other]) {
		if (pstart < min[other])
		    pt1[stepby] = (min[other]-slab_d.intercept)/slab_d.slope;
		if (pend > max[other]) 
		    pt3[stepby] = (max[other]-slab_d.intercept)/slab_d.slope;
	    }
	    else {
		if (pstart > min[other])
		    pt1[stepby] = (min[other]-slab_d.intercept)/slab_d.slope;
		if (pend < max[other]) 
		    pt3[stepby] = (max[other]-slab_d.intercept)/slab_d.slope;
	    }
	}
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
		("slicer2d1a: can't allocate memory - wslab");
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
		("slicer2d1a: can't allocate memory - wslab");
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
    return(wslab);
}
