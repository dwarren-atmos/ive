static char rcsid[] = "$Id: slicer2d2a.c,v 1.3 2000/08/24 19:06:12 harry Exp $";
/* $Id: slicer2d2a.c,v 1.3 2000/08/24 19:06:12 harry Exp $
 * Purpose:
 *   This routine is used to compute plane in space through a 
 *   four dimentional data set where two of the dimensions is
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
 *   da:	the dimensions to average (1-4).
 *   nj:	number of points in first dimension of slice
 *   ni:	number of points in second dimension of slice
 *   special:   special value
 *   phys:      If one - do evenly space slice in physical space
 *              If two - do at physical points of last slice.
 *
 *   $Log: slicer2d2a.c,v $
 *   Revision 1.3  2000/08/24 19:06:12  harry
 *   Fix out of bound problem with average dimensions.
 *
 *   Revision 1.2  1998/09/03 19:19:28  harry
 *   Add in 2d line plots - correct bugs in other averaging.
 *
 *   Revision 1.1  1998/07/16 22:11:22  harryive
 *   Add two dimensional slicing with 2 averaged dimensions.
 *
 *
 */
#include <stdlib.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
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
  *slicer2d2a_(data, nx, ny, nz, nt,
	   dims, pt1, pt2, pt3, da, nj, ni, special, phys)
float *data, *pt1, *pt2, *pt3, *special;
int *dims, *nx, *ny, *nz, *nt, *da, *ni, *nj, *phys;
{
    char datfil[256], field[256];
    double dmax[4],dmin[4],dfmin[4],dfmax[4];
    float *wslab, min[4], max[4], fullmin[4], fullmax[4];
    float ctest[4], ptest[4], c1[4], c2[4];
    float tester, slope, intercept, pstart, pend;
    float A, B, C, D;/*Ax +By +Cz +D = 0 is a plane*/
    float pother, value, sum, sum1, sum2, dx, dx1, dx2;
    float cmin, cmax, cmin1, cmin2, cmax1, cmax2;
    int tt, tnext, tprev, nda, nda1, nda2, ntda1, ntda2;
    int tt1, tt2, tprev1, tnext1, tprev2, tnext2;
    int cpmin[4], cpmax[4], coord_dep[4][4], indep[4];
    int dcounter, icounter, iflag[4], iflag1[4], iflag2[4];
    int i, j, v[4], rconst, zeroes;
    int error, stepby, other, dfree, good;
    int ii, jj, same_slice, ix, iy, dimavg1, dimavg2;
    static int one=1;
    struct point4{
	float x,y,z,t;
    } *cmpt, *phpt, *cmpt1, *cmpt2, *phpt1, *phpt2;
    struct point4a{
	float v[4];
    } *cmpta, *phpta, *cmpta1, *cmpta2, *phpta1, *phpta2, 
	  cavg[2], pavg[2], *fcmpta;
    struct point *phtmp;

    if( *dims != 4 ) {
	(void)make_help_widget_("slicer2d2a: must be 4 dimensional field");
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
       A 2D average MUST have the other 2 dimensions free - thus
       there is no arbitrary slice.
       */

    dimavg1 = da[0] - 1;
    dimavg2 = da[1] - 1;
    i = 0;
    for (j = 0; j < 4; j++) {
	if (j != dimavg1 && j != dimavg2) v[i++] = j;
    }
    if (slab_d.phys == *phys &&
	(slab_d.phys != 1 || (slab_d.ri == pt1[4] && slab_d.rj == pt2[4])) &&
	slab_d.data == data &&
	strcmp(slab_d.datfil, datfil) == 0 &&
	strcmp(slab_d.field, field) == 0 &&
	slab_d.dimavg1 == dimavg1 && slab_d.dimavg2 == dimavg2 &&
	slab_d.avgmin[0] == min[dimavg1] &&
	slab_d.avgmax[0] == max[dimavg1] &&
	slab_d.avgmin[1] == min[dimavg2] &&
	slab_d.avgmax[1] == max[dimavg2])
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
	slab_d.avgmin[0] = min[dimavg1];
	slab_d.avgmax[0] = max[dimavg1];
	slab_d.avgmin[1] = min[dimavg2];
	slab_d.avgmax[1] = max[dimavg2];
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


/****************************************************************************
  This is the code for 2D plots orthogonal to the coordinate axes.

  Cases:

      1. dimavg1 and dimavg2 are not dependent on each other and
         ii and jj are not dependent on dimavg.

	 We can do an area integration.

      2. dimavg1 depends on dimavg2, dimavg2 does not depend
         on dimavg1 and ii and jj are not dependent on dimavg.

	 We can average dimavg1 first at each point in dimavg2,
	 then dimavg2.

      3. All other cases - physical slicing.
****************************************************************************/
    if (!same_slice) {
	ii = v[0];
	jj = v[1];

	njs = cpmax[jj] - cpmin[jj] + 1;
	nis = cpmax[ii] - cpmin[ii] + 1;

	if(nis <2 || njs < 2) {
	    (void)make_help_widget_("slicer2d2a: empty slice");
	    return (float *)0;
	}
	/*
	   See if computational slicing is possible.

	   We have two average coordinate: dimavg in phys or comp space
	   We have two free coordinates: ii & jj in comp space

	   First, if ii and/or jj depend on dimavg, we cannot do it in
	   computational space.
	  */
	if (*phys == 0) {
	    good = 0;
	    if (coord_dep[ii][dimavg1] == 0 && 
		coord_dep[jj][dimavg1] == 0 &&
		coord_dep[ii][dimavg2] == 0 &&
		coord_dep[jj][dimavg2] == 0) {
		/*
		  ii != f(dimavg[0 or 1]) and jj != f(dimavg[0 or 1])
		  */
		if (coord_dep[dimavg2][dimavg1]) {
		    if (coord_dep[dimavg1][dimavg2]) good = 0;
		    else {
			int i=dimavg1;

			dimavg1 = dimavg2;
			dimavg2 = i;
			slab_d.avgmin[0] = min[dimavg1];
			slab_d.avgmax[0] = max[dimavg1];
			slab_d.avgmin[1] = min[dimavg2];
			slab_d.avgmax[1] = max[dimavg2];
			good = 1;
		    }
		}
		else good = 1;
	    }
	    if (good) {
		/*
		   Call convert with dimavg1 phys to see if it will work.
		   */
		for (icounter=0; icounter < 4; icounter++) iflag[icounter] = 1;
		ctest[ii] = cpmin[ii];
		ctest[jj] = cpmin[jj];
		ctest[dimavg2] = cpmin[dimavg2];
		ctest[dimavg1] = MISSING;
		ptest[dimavg1] = dfmin[dimavg1];
		ptest[ii] = ptest[jj] = ptest[dimavg2] = MISSING;
		if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
		    /*
		       Okay, now try it with dimavg1 comp.
		       */
		    ctest[ii] = cpmin[ii];
		    ctest[jj] = cpmin[jj];
		    ctest[dimavg2] = cpmin[dimavg2];
		    ctest[dimavg1] = cpmin[dimavg1];
		    ptest[ii] = ptest[jj] = ptest[dimavg1] = MISSING;
		    if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
			/*
			  Okay, now try dimavg2 phys.
			  */
			iflag[dimavg1] = 0;
			ctest[ii] = cpmin[ii];
			ctest[jj] = cpmin[jj];
			ctest[dimavg1] = ctest[dimavg2] = MISSING;
			ptest[dimavg1] = MISSING;
			ptest[dimavg2] = dfmin[dimavg2];
			ptest[ii] = ptest[jj] = MISSING;
			if (convert(ptest, ctest, iflag,
					  *dims, coord_dep, 1)) {
			    /*
			      Okay, now try it with dimavg2 comp.
			    */
			    ctest[ii] = cpmin[ii];
			    ctest[jj] = cpmin[jj];
			    ctest[dimavg2] = cpmin[dimavg2];
			    ctest[dimavg1] = MISSING;
			    ptest[dimavg1] = MISSING;
			    ptest[ii] = ptest[jj] = ptest[dimavg2] = MISSING;
			    good = convert(ptest, ctest, iflag, *dims,
					   coord_dep, 1);
			}
		    }
		}
	    }
	}
	if (*phys == 0 && good) {
	    /*get memory*/
	    ntda1 = cpmax[dimavg1] - cpmin[dimavg1] + 1;
	    ntda2 = cpmax[dimavg2] - cpmin[dimavg2] + 1;
	    if ((phpt=(struct point4 *)malloc((ntda1+ntda2)
					      *sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpt1 = phpt;
	    phpt2 = phpt + ntda1;
	    phpta1 = (struct point4a *)phpt1;
	    phpta2 = (struct point4a *)phpt2;
	    if ((cmpt=(struct point4 *)malloc((ntda1+ntda2)
					      *sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpt1 = cmpt;
	    cmpt2 = cmpt + ntda1;
	    cmpta1 = (struct point4a *)cmpt1;
	    cmpta2 = (struct point4a *)cmpt2;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }		
	    if ((phpts.pt =  
		 (struct point **)malloc(nis * sizeof(struct point *))) == 
		( struct point **)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - phpts");
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
			("slicer2d2a: can't allocate memory - phpts");
		    for(j=0; j < icounter; j++) free((char *)phpts.pt[j]);
		    free((char *)phpts.pt);
		    free((char *)slab);
		    free((char *)phpt);
		    free((char *)cmpt);
		    return((float *) 0);
		}
	    phpts.numx = nis;
	    phpts.numy = njs;
	    for ( i=0; i < 4; i++) {
		iflag1[i] = 1;
		iflag2[i] = (i != dimavg1);
	    }
	    /* move line through jj */
	    for(dcounter = 0 ; dcounter < njs; dcounter++){
		for (i=0; i < ntda1+ntda2; ++i)
		    cmpta1[i].v[jj] = cpmin[jj] + dcounter;
		for(icounter = 0 ; icounter < nis; icounter++){
		    /*
		       Find the first and last point in dimavg2.
		       */
		    cmpta2[0].v[ii] = cpmin[0] + icounter;
		    cmpta2[0].v[dimavg1] = cpmin[dimavg1];
		    cmpta2[0].v[dimavg2] = MISSING;
		    phpta2[0].v[ii] = phpta2[0].v[jj] = 
			phpta2[0].v[dimavg1] = MISSING;
		    cmpta2[1] = cmpta2[0];
		    phpta2[1] = phpta2[0];
		    phpta2[0].v[dimavg2] = min[dimavg2];
		    phpta2[1].v[dimavg2] = max[dimavg2];
		    convert(phpta2, cmpta2, iflag2, *dims, coord_dep, 2);
		    if (cmpta[0].v[dimavg2] < cpmin[dimavg2]) {
			cmpta[0].v[dimavg2] = cpmin[dimavg2];
			phpta[0].v[dimavg2] = MISSING;
			convert(phpta2, cmpta2, iflag, *dims, coord_dep, 1);
		    }
		    cmin2 = cmpta[0].v[dimavg2];
		    if (cmpta[1].v[dimavg2] < 0 ||
			cmpta[1].v[dimavg2] > cpmax[dimavg2]) {
			cmpta[1].v[dimavg2] = cpmax[dimavg2];
			phpta[1].v[dimavg2] = MISSING;
			convert(phpta2+1, cmpta2+1, iflag, *dims,
				coord_dep, 1);
		    }
		    cmax2 = cmpta[1].v[dimavg2];
		    if (cmin2 == cmax2) nda2 = 2;
		    else {
			nda2 = ceil(cmax2) - floor(cmin2) + 1;
			if (nda2 > 2) {
			    cmpta2[nda2-1] = cmpta2[1];
			    phpta2[nda2-1] = phpta2[1];
			    for (i=1; i < nda2-1; i++) {
				cmpta2[i].v[ii] = cmpta2[0].v[ii];
				cmpta2[i].v[dimavg2] = (int) (cmin2+i);
			    }
			    phpta2[1].v[ii] = phpta2[1].v[jj]
				= phpta2[1].v[dimavg2] = MISSING;
			    convert(phpta2+1, cmpta2+1, iflag2, 
				    *dims, coord_dep, nda2-2);
			}
		    }
		    sum2 = dx2 = 0.;
		    for (tprev2=tt2=0; tprev2 < nda2-1;
			 tprev2=tt2, tt2=tnext2) {
			tnext2 = MIN(tt2+1, nda2-1);
			/*
			  Calculate the dimavg1 average for this
			  dimavg2 point.
			  */
			cmpta1[0] = cmpta2[tt2];
			phpta1[0] = phpta2[tt2];
			cmpta1[0].v[dimavg1] = MISSING;
			cmpta1[1] = cmpta1[0];
			phpta1[1] = phpta1[0];
			phpta1[0].v[dimavg1] = min[dimavg1];
			phpta1[1].v[dimavg1] = max[dimavg1];
			convert(phpta1, cmpta1, iflag1, *dims,
				coord_dep,2);
			if (cmpta[0].v[dimavg1] < cpmin[dimavg1]) {
			    cmpta[0].v[dimavg1] = cpmin[dimavg1];
			    phpta[0].v[dimavg1] = MISSING;
			    convert(phpta1, cmpta1, iflag, *dims,
				    coord_dep, 1);
			}
			cmin1 = cmpta[0].v[dimavg1];
			if (cmpta[1].v[dimavg1] < 0 ||
			    cmpta[1].v[dimavg1] > cpmax[dimavg1]) {
			    cmpta[1].v[dimavg1] = cpmax[dimavg1];
			    phpta[1].v[dimavg1] = MISSING;
			    convert(phpta1+1, cmpta1+1, iflag, *dims,
				    coord_dep, 1);
			}
			cmax1 = cmpta[1].v[dimavg1];
			if (cmin1 == cmax1) {
			    sum1 = (float) interp_(data,nx,ny,nz,nt,
						   &cmpt1[0].x,&cmpt1[0].y,
						   &cmpt1[0].z,&cmpt1[0].t,
						   special);
			    dx1 = 0;
			}
			else {
			    nda1 = ceil(cmax1) - floor(cmin1) + 1;
			    if (nda1 > 2) {
				cmpta1[nda1-1] = cmpta1[1];
				phpta1[nda1-1] = phpta1[1];
				for (i=1; i < nda1-1; i++) {
				    cmpta1[i].v[dimavg2] 
				      = cmpta1[0].v[dimavg2];
				    cmpta1[i].v[ii] = cmpta1[0].v[ii];
				    cmpta1[i].v[dimavg1] = (int) (cmin1+i);
				}
				phpta1[1].v[ii] = phpta1[1].v[jj] 
				    = phpta1[1].v[dimavg1]
				    = phpta1[1].v[dimavg2] = MISSING;
				convert(phpta1+1, cmpta1+1, iflag1,
					*dims, coord_dep, nda1-2);
			    }
			    sum1 = dx1 = 0;
			    for (tprev1 = tt1 = 0; tprev1 < nda1-1;
				 tprev1=tt1,tt1=tnext1) {
				tnext1 = MIN(tt1 + 1, nda1-1);
				if ((value = (float)interp_(data,nx,ny,nz,nt,
						&cmpt1[tt1].x,&cmpt1[tt1].y,
						&cmpt1[tt1].z,&cmpt1[tt1].t,
						special)) != *special) {
				    sum1 +=value*(phpta1[tnext1].v[dimavg1]
					  -phpta1[tprev1].v[dimavg1])*0.5;
				    dx1 += (phpta1[tnext1].v[dimavg1]
					    -phpta1[tprev1].v[dimavg1])*0.5;
				}
			    }
			    if (dx1 == 0.) sum1 = MISSING;
			}
			if (cmin2 == cmax2) {
			    if (dx1 != 0) sum2 = sum1/dx1;
			    else sum2 = sum1;
			    break;
			}
			else if (sum1 != MISSING) {
			    sum2 += sum1*(phpta2[tnext2].v[dimavg2]
					  -phpta2[tprev2].v[dimavg2])*0.5;
			    dx2 += dx1*(phpta2[tnext2].v[dimavg2]
					-phpta2[tprev2].v[dimavg2])*0.5;
			}
		    }
		    if (cmin2 != cmax2) {
			if (dx2 == 0.)
			    slab_dep(icounter, dcounter) = *special;
			else
			    slab_dep(icounter, dcounter) = sum2/dx2;
		    }
		    else
			slab_dep(icounter, dcounter) = sum2;
		    phtmp = phpts.pt[icounter];
		    phtmp[dcounter].x = phpta2[0].v[ii];
		    phtmp[dcounter].y = phpta2[0].v[jj];
		}
	    }
	    free(cmpt);
	    free(phpt);
	}
	/*
	  Use physical points from last slice.
	  */
	else if (*phys == 2) {
	    for (icounter = 0; icounter < 4; icounter++) iflag[icounter] = 1;
	    ctest[ii] = ctest[jj] = MISSING;
	    ptest[dimavg1] = ptest[dimavg2] = MISSING;
	    ptest[ii] = dfmin[ii];
	    ptest[jj] = dfmin[jj];
	    ctest[dimavg1] = cpmin[dimavg1];
	    ctest[dimavg2] = cpmin[dimavg2];
	    if (!convert(ptest, ctest,iflag, *dims, coord_dep, 1)) {
		(void) make_help_widget
		    ("slicer2d2a: cannot do this slice - notify developers");
		return((float *) 0);
	    }		
	    nis = phpts.numx;
	    njs = phpts.numy;
	    /*get memory*/
	    ntda1 = cpmax[dimavg1] - cpmin[dimavg1] + 1;
	    ntda2 = cpmax[dimavg2] - cpmin[dimavg2] + 1;
	    if ((phpt=(struct point4 *)malloc((ntda1+ntda2)
					      *sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpt1 = phpt;
	    phpt2 = phpt + ntda1;
	    phpta1 = (struct point4a *)phpt1;
	    phpta2 = (struct point4a *)phpt2;
	    if ((cmpt=(struct point4 *)malloc((ntda1+ntda2)
					      *sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpt1 = cmpt;
	    cmpt2 = cmpt + ntda1;
	    cmpta1 = (struct point4a *)cmpt1;
	    cmpta2 = (struct point4a *)cmpt2;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    for ( i=0; i < 4; i++) {
		iflag1[i] = 1;
		iflag2[i] = (i != dimavg1);
	    }
	    for (dcounter = 0; dcounter < njs; dcounter++) {
		for (icounter = 0; icounter < nis; icounter++) {
		    int two = 2;

		    phtmp = phpts.pt[icounter];
		    /*
		      Find the first and last point in dimavg2.
		      */
		    phpta2[0].v[ii] = phtmp[dcounter].x;
		    phpta2[0].v[jj] = phtmp[dcounter].y;
		    phpta2[0].v[dimavg1] = min[dimavg1];
		    phpta2[1] = phpta2[0];
		    phpta2[0].v[dimavg2] = min[dimavg2];
		    phpta2[1].v[dimavg2] = max[dimavg2];
		    phys_2_index_trans_(phpta2, cmpta2, iflag2, dims, &two);
		    if (cmpta[0].v[dimavg2] < cpmin[dimavg2]) {
			cmpta[0].v[dimavg2] = cpmin[dimavg2];
			phpta[0].v[dimavg2] = MISSING;
			convert(phpta2, cmpta2, iflag, *dims, coord_dep, 1);
		    }
		    cmin2 = cmpta[0].v[dimavg2];
		    if (cmpta[1].v[dimavg2] < 0 ||
			cmpta[1].v[dimavg2] > cpmax[dimavg2]) {
			cmpta[1].v[dimavg2] = cpmax[dimavg2];
			phpta[1].v[dimavg2] = MISSING;
			convert(phpta2+1, cmpta2+1, iflag, *dims,
				coord_dep, 1);
		    }
		    cmax2 = cmpta[1].v[dimavg2];
		    if (cmin2 == cmax2) nda2 = 2;
		    else {
			nda2 = ceil(cmax2) - floor(cmin2) + 1;
			if (nda2 > 2) {
			    cmpta2[nda2-1] = cmpta2[1];
			    phpta2[nda2-1] = phpta2[1];
			    for (i=1; i < nda2-1; i++) {
				phpta2[i].v[ii] = phpta2[0].v[ii];
				phpta2[i].v[jj] = phpta2[0].v[jj];
				cmpta2[i].v[dimavg2] = (int) (cmin2+i);
			    }
			    cmpta2[1].v[ii] = cmpta2[1].v[jj] = MISSING;
			    phpta2[1].v[dimavg2] = MISSING;
			    convert(phpta2+1, cmpta2+1, iflag2,
				    *dims, coord_dep, nda2-2);
			}
		    }
		    sum2 = dx2 = 0.;
		    for (tprev2=tt2=0; tprev2 < nda2-1;
			 tprev2=tt2, tt2=tnext2) {
			tnext2 = MIN(tt2+1, nda2-1);
			/*
			  Calculate the dimavg1 average for this
			  dimavg2 point.
			*/
			cmpta1[0] = cmpta2[tt2];
			phpta1[0] = phpta2[tt2];
			cmpta1[0].v[dimavg1] = MISSING;
			cmpta1[1] = cmpta1[0];
			phpta1[1] = phpta1[0];
			phpta1[0].v[dimavg1] = min[dimavg1];
			phpta1[1].v[dimavg1] = max[dimavg1];
			convert(phpta1, cmpta1, iflag1, *dims,
				coord_dep,2);
			if (cmpta[0].v[dimavg1] < cpmin[dimavg1]) {
			    cmpta[0].v[dimavg1] = cpmin[dimavg1];
			    phpta[0].v[dimavg1] = MISSING;
			    convert(phpta1, cmpta1, iflag, *dims,
				    coord_dep, 1);
			}
			cmin1 = cmpta[0].v[dimavg1];
			if (cmpta[1].v[dimavg1] < 0 ||
			    cmpta[1].v[dimavg1] > cpmax[dimavg1]) {
			    cmpta[1].v[dimavg1] = cpmax[dimavg1];
			    phpta[1].v[dimavg1] = MISSING;
			    convert(phpta1+1, cmpta1+1, iflag, *dims,
				    coord_dep, 1);
			}
			cmax1 = cmpta[1].v[dimavg1];
			if (cmin1 == cmax1)
			    sum1 = (float) interp_(data,nx,ny,nz,nt,
						   &cmpt1[0].x,&cmpt1[0].y,
						   &cmpt1[0].z,&cmpt1[0].t,
						   special);
			else {
			    nda1 = ceil(cmax1) - floor(cmin1) + 1;
			    if (nda1 > 2) {
				cmpta1[nda1-1] = cmpta1[1];
				phpta1[nda1-1] = phpta1[1];
				for (i=1; i < nda1-1; i++) {
				    phpta1[i].v[ii] = phpta1[0].v[ii];
				    phpta1[i].v[jj] = phpta1[0].v[jj];
				    cmpta1[i].v[dimavg1] = (int) (cmin1+i);
				    cmpta1[i].v[dimavg2] 
				      = cmpta1[0].v[dimavg2];
				}
				cmpta1[1].v[ii] = cmpta1[1].v[jj] = MISSING;
				phpta1[1].v[dimavg1] 
				  = phpta1[1].v[dimavg2]= MISSING;
				convert(phpta1+1, cmpta1+1, iflag1,
					*dims, coord_dep, nda1-2);
			    }
			    sum1 = dx1 = 0;
			    for (tprev1 = tt1 = 0; tprev1 < nda1-1;
				 tprev1=tt1,tt1=tnext1) {
				tnext1 = MIN(tt1 + 1, nda1-1);
				if ((value = (float)interp_(data,nx,ny,nz,nt,
						&cmpt1[tt1].x,&cmpt1[tt1].y,
						&cmpt1[tt1].z,&cmpt1[tt1].t,
						special)) != *special) {
				    sum1 +=value*(phpta1[tnext1].v[dimavg1]
					  -phpta1[tprev1].v[dimavg1])*0.5;
				    dx1 += (phpta1[tnext1].v[dimavg1]
					    -phpta1[tprev1].v[dimavg1])*0.5;
				}
			    }
			    if (dx1 == 0.) sum1 = MISSING;
			}
			if (cmin2 == cmax2) {
			    sum2 = sum1;
			    break;
			}
			else if (sum1 != MISSING) {
			    sum2 += sum1*(phpta2[tnext2].v[dimavg2]
					  -phpta2[tprev2].v[dimavg2])*0.5;
			    dx2 += dx1*(phpta2[tnext2].v[dimavg2]
				    -phpta2[tprev2].v[dimavg2])*0.5;
			}
		    }
		    if (cmin2 != cmax2) {
			if (dx == 0.)
			    slab_dep(icounter, dcounter) = *special;
			else
			    slab_dep(icounter, dcounter) = sum2/dx2;
		    }
		    else
			slab_dep(icounter, dcounter) = sum2;
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
	    int ii1 = ii+1, jj1 = jj+1, da[2], nda[2];

	    da[0] = dimavg1+1;
	    da[1] = dimavg2+1;
	    corner[0].v[dimavg1] = corner[1].v[dimavg1] = min[dimavg1];
	    corner[2].v[dimavg1] = corner[3].v[dimavg1] = max[dimavg1];
	    corner[0].v[dimavg2] = corner[1].v[dimavg2] = min[dimavg2];
	    corner[2].v[dimavg2] = corner[3].v[dimavg2] = max[dimavg2];
	    corner[0].v[ii] = corner[2].v[ii] = fullmin[ii];
	    corner[1].v[ii] = corner[3].v[ii] = fullmax[ii];
	    corner[0].v[jj] = corner[1].v[jj] = fullmin[jj];
	    corner[2].v[jj] = corner[3].v[jj] = fullmax[jj];
	    if (*phys) compute = pslicer2d_(corner, &ii1, &jj1, &nis, &njs,
					    da, nda, &slab_d.ri,
					    &slab_d.rj);
	    else compute = data_slicer_2d_trans_(corner, &ii1, &jj1, &nis,
						 &njs, da, nda,
						 &slab_d.ri, &slab_d.rj);
	    if (compute == 0) return((float *)0);
	    nda1 = nda[0];
	    nda2 = nda[1];
	    if ((cmpt2=(struct point4 *)malloc(nda2
					      *sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - cmpt");
		free((char *)compute);
		return((float *) 0);
	    }
	    if ((phpt=(struct point4 *)malloc((nda1+nda2)
					      *sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - phpt");
		free((char *)cmpt2);
		free((char *)compute);
		return((float *) 0);
	    }
	    phpt1 = phpt;
	    phpt2 = phpt + nda1;
	    phpta1 = (struct point4a *)phpt1;
	    phpta2 = (struct point4a *)phpt2;
	    if ((slab_d.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)compute);
		free((char *)cmpt2);
		return((float *) 0);
	    }
	    if ((phpts.pt =  
		 (struct point **)malloc(nis * sizeof(struct point *))) == 
		( struct point **)0)
	    {
		(void)make_help_widget_
		    ("slicer2d2a: can't allocate memory - phpts");
		free((char *)slab_d.slab);
		slab_d.slab = 0;
		free((char *)phpt);
		free((char *)compute);
		free((char *)cmpt2);
		return((float *) 0);
	    }
	    for(i=0; i<nis; i++)
		if ((phpts.pt[i] =  
		     (struct point *)malloc(njs * sizeof(struct point))) == 
		    ( struct point *)0)
		{
		    (void)make_help_widget_
			("slicer2d2a: can't allocate memory - phpts");
		    for(j=0; j < i; j++) free((char *)phpts.pt[j]);
		    free((char *)phpts.pt);
		    free((char *)slab);
		    free((char *)phpt);
		    free((char *)compute);
		    free((char *)cmpt2);
		    return((float *) 0);
		}
	
	    phpts.numx = nis;
	    phpts.numy = njs;
	    for ( i=0; i < 4; i++) {
		iflag1[i] = 1;
		iflag2[i] = (i != dimavg1);
	    }
	    /*find line*/
	    /* move line through jj */
	    for(dcounter = 0 ; dcounter < njs; dcounter++){
		for(icounter = 0 ; icounter < nis; icounter++){
		    int nda2a = (nda2 == 1)? 2: nda2;

		    for (i=0; i < nda2; i++) {
			cmpt2[i] = *(compute +
			    ((nis*dcounter + icounter)*nda2 + i)*nda1);
		    }
		    (void)index_2_phys_trans_(phpt2, cmpt2, iflag2,
					      dims, &nda2);
		    sum2 = dx2 = 0;
		    for (tprev2=tt2=0; tprev2 < nda2a-1;
			 tprev2=tt2, tt2=tnext2) {
			tnext2 = MIN(tt2+1, nda2-1);
			/*
			  Calculate the dimavg1 average for this
			  dimavg2 point.
			*/
			cmpt1 = compute +
			    ((nis*dcounter + icounter)*nda2 + tt2)*nda1;
			if (nda1 == 1) {
			    sum1 = (float) interp_(data,nx,ny,nz,nt,
						   &cmpt1[0].x,&cmpt1[0].y,
						   &cmpt1[0].z,&cmpt1[0].t,
						   special);
			    dx1 = 0;
			}
			else {
			    (void)index_2_phys_trans_(phpt1, cmpt1, iflag1,
						      dims, &nda1);
			    sum1 = dx1 = 0;
			    for (tprev1 = tt1 = 0; tprev1 < nda1-1;
				 tprev1=tt1,tt1=tnext1) {
				tnext1 = MIN(tt1 + 1, nda1-1);
				if ((value = (float)interp_(data,nx,ny,nz,nt,
						&cmpt1[tt1].x,&cmpt1[tt1].y,
						&cmpt1[tt1].z,&cmpt1[tt1].t,
						special)) != *special) {
				    sum1 +=value*(phpta1[tnext1].v[dimavg1]
					  -phpta1[tprev1].v[dimavg1])*0.5;
				    dx1 += (phpta1[tnext1].v[dimavg1]
					    -phpta1[tprev1].v[dimavg1])*0.5;
				}
			    }
			    if (dx1 == 0.) sum1 = MISSING;
			}
			if (nda2 == 1) {
			    if (dx1 != 0) sum2 = sum1/dx1;
			    else sum2 = sum1;
			    break;
			}
			else if (sum1 != MISSING) {
			    sum2 += sum1*(phpta2[tnext2].v[dimavg2]
					  -phpta2[tprev2].v[dimavg2])*0.5;
			    dx2 += dx1*(phpta2[tnext2].v[dimavg2]
					-phpta2[tprev2].v[dimavg2])*0.5;
			}
		    }
		    if (nda2 > 1) {
			if (dx2 == 0.)
			    slab_dep(icounter, dcounter) = *special;
			else
			    slab_dep(icounter, dcounter) = sum2/dx2;
		    }
		    phtmp = phpts.pt[icounter];
		    phtmp[dcounter].x = phpta2[0].v[ii];
		    phtmp[dcounter].y = phpta2[0].v[jj];
		}
	    }
	    free(compute);
	    free(cmpt2);
	    free(phpt);
	}
	slab_d.xaxis = ii+1;
	slab_d.yaxis = jj+1;
	slab_d.dimavg1 = dimavg1;
	slab_d.dimavg2 = dimavg2;
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
	pt3[slab_d.dimavg1] = pt1[slab_d.dimavg1] = pt2[slab_d.dimavg1];
	pt3[slab_d.dimavg2] = pt1[slab_d.dimavg2] = pt2[slab_d.dimavg2];
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
		("slicer2d2a: can't allocate memory - wslab");
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
		("slicer2d2a: can't allocate memory - wslab");
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
