static char rcsid[] = "$Id: slicer1d2a.c,v 1.5 2001/02/13 00:26:01 harry Exp $";
/* $Id: slicer1d2a.c,v 1.5 2001/02/13 00:26:01 harry Exp $
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
 *   $Log: slicer1d2a.c,v $
 *   Revision 1.5  2001/02/13 00:26:01  harry
 *   Fix problem when using user transform slicer.
 *
 *   Revision 1.4  2000/08/24 19:06:10  harry
 *   Fix out of bound problem with average dimensions.
 *
 *   Revision 1.3  2000/02/11 23:14:40  harry
 *   Allow surface 1d plots.  Fix Z slider for surface plots.
 *
 *   Revision 1.2  1998/09/30 22:28:39  harry
 *   Rewrite makpointplt and add ability for 2 average points, fix errors
 *   in the 1d slicers, remove obsolete yuri routines.
 *
 *   Revision 1.1  1998/09/03 19:19:25  harry
 *   Add in 2d line plots - correct bugs in other averaging.
 *
 *
 */
#include <string.h>
#include <stdlib.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <math.h>

#include <ive_macros.h>
#include <missing.h>
#include <window.h>

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
  *slicer1d2a_(data, nx, ny, nz, nt,
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
    float value, sum, dx, sum1, sum2, dx1, dx2;
    float cmin, cmax, cmin1, cmin2, cmax1, cmax2;
    int tt, tnext, tprev, nda, nda1, nda2, ntda1, ntda2;
    int tt1, tt2, tprev1, tnext1, tprev2, tnext2;
    int cpmin[4], cpmax[4], coord_dep[4][4], indep[4];
    int icounter, iflag[4], iflag1[4], iflag2[4], i, j, zeroes;
    int error, is, ie, dfree, good, stepby, other, locked;
    int same_slice, dimavg1, dimavg2, same, ix, iy;
    static int one=1;
    struct point4{
	float x,y,z,t;
    } *cmpt, *phpt, *cmpt1, *cmpt2, *phpt1, *phpt2;
    struct point4a{
	float v[4];
    } *cmpta, *phpta, *cmpta1, *cmpta2, *phpta1, *phpta2;

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
    dimavg1 = da[0] - 1;
    dimavg2 = da[1] - 1;
    zeroes = 0;
    same = 1;
    for (i=0; i < *dims; i++) {
	if (i == dimavg1 || i == dimavg2) dv[i] = 0;
	else dv[i] = pt2[i]-pt1[i];
	if (dv[i] == 0) {
	    zeroes++;
	    if (slab_l.dv[i] || pt1[i] != slab_l.v[i]) same = 0;
	}
	else if (dv[i] != slab_l.dv[i]) same = 0;
    }
    if (strcmp(slab_l.datfil, datfil) == 0 &&
	strcmp(slab_l.field, field) == 0 && slab_l.dimavg1 == dimavg1 &&
	slab_l.dimavg2 == dimavg2 &&
	slab_l.avgmin[0] == min[dimavg1] &&
	slab_l.avgmax[0] == max[dimavg1] &&
	slab_l.avgmin[1] == min[dimavg2] &&
	slab_l.avgmax[1] == max[dimavg2] &&
	same)
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
	slab_l.dimavg1 = -1;	/* In case something goes wrong below */
	slab_l.dimavg2 = -1;
	slab_l.avgmin[0] = min[dimavg1];
	slab_l.avgmax[0] = max[dimavg1];
	slab_l.avgmin[1] = min[dimavg2];
	slab_l.avgmax[1] = max[dimavg2];
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
		if (dv[i]) dfree = i;
		else if (i != dimavg1 && i != dimavg2) locked = i;
	    }
	    /*
	       See if computational slicing is possible.
	       
	       We have one locked coordinates in phys space
	       We have two average coordinate: dimavg1 & 2 in
	       		phys or comp space
	       We have one free coordinate: dfree in comp space

	       First, if dfree depends on dimavg1 or 2, we cannot do it in
	       computational space.
	       */
	    good = 0;
	    if (coord_dep[dfree][dimavg1] == 0 &&
		coord_dep[dfree][dimavg2] == 0) {
		/*
		  Have to switch the dimensions if dimavg2 depends
		  on dimavg1.
		*/
		if (coord_dep[dimavg2][dimavg1]) {
		    if (coord_dep[dimavg1][dimavg2]) good = 0;
		    else {
			int i=dimavg1;

			dimavg1 = dimavg2;
			dimavg2 = i;
			slab_l.avgmin[0] = min[dimavg1];
			slab_l.avgmax[0] = max[dimavg1];
			slab_l.avgmin[1] = min[dimavg2];
			slab_l.avgmax[1] = max[dimavg2];
			good = 1;
		    }
		}
		else good = 1;
	    }
	    if (good) {
		/*
		   Call convert with dimavg1 phys to see if it will work.
		   */
		for (icounter=0; icounter < 4; icounter++) {
		    iflag[icounter] = 1;
		    ptest[icounter] = pt1[icounter];
		    ctest[icounter] = MISSING;
		}
		ptest[dfree] = ptest[dimavg2] = MISSING;
		ctest[dimavg2] = cpmin[dimavg2];
		ctest[dfree] = cpmin[dfree];
		if (*surface) {
		    ptest[2] = MISSING;
		    ctest[2] = 1;
		}
		if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
		    /*
		       Okay, now try it with dimavg1 comp.
		       */
		    ctest[locked] = MISSING;
		    ctest[dimavg1] = cpmin[dimavg1];
		    ptest[dimavg1] = MISSING;
		    ctest[dimavg2] = cpmin[dimavg2];
		    ptest[dimavg2] = MISSING;
		    if (*surface) {
			ptest[2] = MISSING;
			ctest[2] = 1;
		    }
		    if (convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
			/*
			  Okay, now try it with dimavg2 phys.
			*/
			iflag[dimavg1] = 0;
			for (icounter=0; icounter < 4; icounter++) {
			    ptest[icounter] = pt1[icounter];
			    ctest[icounter] = MISSING;
			}
			ptest[dfree] = ptest[dimavg1] = MISSING;
			ctest[dimavg1] = MISSING;
			if (*surface) {
			    ptest[2] = MISSING;
			    ctest[2] = 1;
			}
			if (convert(ptest, ctest, iflag,
				    *dims, coord_dep, 1)) {
			    /*
			      Okay, now try it with dimavg2 comp.
			    */
			    ctest[locked] = MISSING;
			    ctest[dimavg1] = MISSING;
			    ctest[dimavg2] = cpmin[dimavg2];
			    ptest[dimavg2] = MISSING;
			    if (*surface) {
				ptest[2] = MISSING;
				ctest[2] = 1;
			    }
			    good = convert(ptest, ctest, iflag,
					   *dims, coord_dep, 1);
			}
		    }
		}
	    }
	    if (good) {
		nis = cpmax[dfree] - cpmin[dfree] + 1;

		if(nis <2) {
		    (void)make_help_widget_("slicer1d: empty slice");
		    return (float *)0;
		}
		ntda1 = cpmax[dimavg1] - cpmin[dimavg1] + 1;
		ntda2 = cpmax[dimavg2] - cpmin[dimavg2] + 1;
		/*get memory*/
		if ((phpt=(struct point4 *)malloc((ntda1 + ntda2)*
						  sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
		    return((float *) 0);
		}
		phpt1 = phpt;
		phpt2 = phpt + ntda1;
		phpta1 = (struct point4a *)phpt1;
		phpta2 = (struct point4a *)phpt2;
		if ((cmpt=(struct point4 *)malloc((ntda1+ntda2)*
						  sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - cmpt");
		    free((char *)phpt);
		    return((float *) 0);
		}
		cmpt1 = cmpt;
		cmpt2 = cmpt + ntda1;
		cmpta1 = (struct point4a *)cmpt1;
		cmpta2 = (struct point4a *)cmpt2;
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
		if (*surface)
		    for (icounter=0; icounter < ntda1+ntda2; icounter++)
			cmpta1[icounter].v[2] = 1;
		else
		    for (icounter=0; icounter < ntda1+ntda2; icounter++)
			phpta1[icounter].v[locked] = pt1[locked];
		for (icounter=0; icounter < nis; icounter++) {
		    for (i=0; i<ntda1+ntda2; i++)
			cmpta1[i].v[dfree] = cpmin[dfree] + icounter;
		    /*
		       Find the first and last point in the average2.
		       */
		    for (i=0; i < *dims; i++) {
			iflag1[i] = 1;
			iflag2[i] = (i != dimavg1);
			if (i == dfree) phpta2[0].v[i] = MISSING;
			else cmpta2[0].v[i] = MISSING;
		    }
		    phpta2[0].v[dimavg2] = min[dimavg2];
		    phpta2[0].v[dimavg1] = MISSING;
		    phpta2[1] = phpta2[0];
		    cmpta2[1] = cmpta2[0];
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
			    for (i=1; i < nda2-1; i++)
				cmpta2[i].v[dimavg2] = (int) (cmin2+i);
			    for (i=0; i < *dims; i++) {
				if (i != dimavg2 && i != dfree)
				    cmpta2[1].v[i] = MISSING;
				else phpta2[1].v[i] = MISSING;
			    }
			    cmpta2[1].v[dimavg1] = MISSING;
			    convert(phpt2+1, cmpt2+1, iflag2, *dims,
				    coord_dep, nda2-2);
			}
		    }
		    sum2 = dx2 = 0.;
		    for(tprev2=tt2=0; tprev2 < nda2-1;
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
				    cmpta1[i].v[dfree] = cmpta1[0].v[dfree];
				    cmpta1[i].v[dimavg1] = (int) (cmin1+i);
				}
				phpta1[1].v[dfree] = phpta1[1].v[dimavg1]
				    = phpta1[1].v[dimavg2] = MISSING;
				if (*surface) {
				    cmpta1[1].v[2] = 1;
				    phpta1[1].v[2] = MISSING;
				}
				else     
				    cmpta1[1].v[locked] = MISSING;
				convert(phpta1+1, cmpta1+1, iflag1,
					*dims, coord_dep, nda1-2);
			    }
			    sum1 = dx1 = 0;
			    for (tprev1 = tt1 = 0; tprev1 < nda1-1;
				 tprev1=tt1,tt1=tnext1) {
				tnext1 = MIN(tt1 + 1, nda1-1);
				if ((value=(float)interp_(data,nx,ny,nz,nt,
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
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum2/dx2;
		    }
		    else
			slab_l.slab[icounter] = sum2;
		    slab_l.ploc[icounter] = phpta2[0].v[dfree];
		}
	    }
	    /*
	       Well, that didn't work, now we have to depend on the
	       user transform to do our work for us.
	       */
	    else {
		struct point4a endpt[2];
		struct point4 *data_slicer_1d_trans_();
		int da[2], nda[2]; 
		
		da[0] = dimavg1+1;
		da[1] = dimavg2+1;
		for (i=0; i < *dims; i++) {
		    if (i == dfree) {
			endpt[0].v[i] = fullmin[i];
			endpt[1].v[i] = fullmax[i];
		    }
		    else if (i == dimavg1 || i == dimavg2) {
			endpt[0].v[i] = min[i];
			endpt[1].v[i] = max[i];
		    }
		    else endpt[0].v[i] = endpt[1].v[i] = pt1[i];
		}
		cmpt = data_slicer_1d_trans_(endpt, &nis, da, &nda);
		if (cmpt == 0) return((float *)0);
		nda1 = nda[0];
		nda2 = nda[1];
		if ((cmpt2=(struct point4 *)malloc(nda2
						   *sizeof(struct point4)))
		    == (struct point4 *)0)
		    {
			(void)make_help_widget_
			    ("slicer2d2a: can't allocate memory - cmpt2");
			free((char *)cmpt);
			return((float *) 0);
		    }
		if ((phpt=(struct point4 *)malloc((nda1+nda2)
						  *sizeof(struct point4)))
		    == (struct point4 *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - phpt");
			free((char *)cmpt);
			free((char *)cmpt2);
			return((float *) 0);
		    }
		phpt1 = phpt;
		phpt2 = phpt1 + nda1;
		phpta1 = (struct point4a *) phpt1;
		phpta2 = (struct point4a *) phpt2;
		if ((slab_l.slab = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)cmpt);
		    free((char *)cmpt2);
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
		    free((char *)cmpt2);
		    return((float *) 0);
		}
		for ( i=0; i < 4; i++) {
		    iflag1[i] = 1;
		    iflag2[i] = (i != dimavg1);
		}
		for(icounter = 0 ; icounter < nis; icounter++){
		    int nda2a = (nda2 == 1) ? 2: nda2;

		    for (i=0; i < nda2; i++) {
			cmpt2[i] = *(cmpt + (icounter*nda2 + i)*nda1);
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
			cmpt1 = cmpt + (icounter*nda2 + tt2)*nda1;
			if (nda1 == 1) {
			    sum1 = (float)interp_(data,nx,ny,nz,nt,
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
		    if (nda2 == 1) {
			if (dx2 == 0.)
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum2/dx2;
		    }
		    else
			slab_l.slab[icounter] = sum2;
		    slab_l.ploc[icounter] = phpta2[0].v[dfree];
		}
	    }
	    free(cmpt);
	    free(phpt);
	    free(cmpt2);
	    slab_l.xaxis = dfree+1;
	    slab_l.yaxis = 0;
	    slab_l.dimavg1 = dimavg1;
	    slab_l.dimavg2 = dimavg2;
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
	       
	       We have two averaged coordindates..
	       We have two coordinates that depend on each other:
	       stepby (in comp space) and other (dep on stepby in phys space).

	       Can we get stepby to physical coord with the information
	       we currently have.
	       */
	    good = 0;
	    /*
	      Have to switch the dimensions if dimavg2 depends
	      on dimavg1.
	    */
	    if (coord_dep[dimavg2][dimavg1]) {
		if (coord_dep[dimavg1][dimavg2] == 0) {
		    int i=dimavg1;

		    dimavg1 = dimavg2;
		    dimavg2 = i;
		    slab_l.avgmin[0] = min[dimavg1];
		    slab_l.avgmax[0] = max[dimavg1];
		    slab_l.avgmin[1] = min[dimavg2];
		    slab_l.avgmax[1] = max[dimavg2];
		    good = 1;
		}
	    }
	    else good = 1;
	    if (coord_dep[stepby][dimavg1] == 0 && 
		coord_dep[other][dimavg1] == 0 &&
		coord_dep[stepby][dimavg2] == 0 && 
		coord_dep[other][dimavg2] == 0) { 
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
		    ctest[dimavg1] = cpmin[dimavg1];
		    ptest[dimavg1] = MISSING;
		    ctest[dimavg2] = cpmin[dimavg2];
		    ptest[dimavg2] = MISSING;
		    good = convert(ptest, ctest, iflag, *dims, coord_dep, 1);
		}
	    }
	    if (good) {
		nis = ceil(c2[stepby]) - floor(c1[stepby]) + 1;
	    	if(nis <2) {
		    (void)make_help_widget_("slicer1d: empty slice");
		    return (float *)0;
		}
		ntda1 = cpmax[dimavg1] - cpmin[dimavg1] + 1;
		ntda2 = cpmax[dimavg2] - cpmin[dimavg2] + 1;
		/*get memory*/
		if ((phpt=(struct point4 *)malloc((ntda1+ntda2)
						  *sizeof(struct point4)))
		    == (struct point4 *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - phpt");
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
			("slicer: can't allocate memory - cmpt");
		    free((char *)phpt);
		    return((float *) 0);
		}
		cmpt1 = cmpt;
		cmpt2 = cmpt + ntda1;
		cmpta1 = (struct point4a *)cmpt1;
		cmpta2 = (struct point4a *)cmpt2;
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
		iflag2[stepby] = 1;
		for (icounter = 0; icounter < nis; icounter++) {
		    float first_stepby;

		    phpta2[0].v[dimavg1] = MISSING;
		    phpta2[0].v[dimavg2] = min[dimavg2];
		    cmpta2[0].v[dimavg1] = cmpta2[0].v[dimavg2] = MISSING;
		    if (icounter == 0) {
			phpta2[0].v[stepby] = pt1[stepby];
			cmpta2[0].v[stepby] = cmpta2[0].v[other] = MISSING;
		    }
		    else if (icounter == nis-1) {
			phpta2[0].v[stepby] = pt2[stepby];
			cmpta2[0].v[stepby] = cmpta2[0].v[other] = MISSING;
		    }
		    else {
			cmpta2[0].v[stepby] = first_stepby + icounter;
			iflag2[other] = iflag2[dimavg1] = iflag2[dimavg2] = 0;
			cmpta2[0].v[other] = MISSING;
			phpta2[0].v[other] = phpta2[0].v[stepby] = MISSING;
			convert(phpta2, cmpta2, iflag2, *dims, coord_dep, 1);
		    }
		    /* 
		       Find the first and last point in average2.
		    */
		    phpta2[0].v[other] = slope*phpta2[0].v[stepby] + intercept;
		    iflag2[other] = iflag2[dimavg2] = 1;
		    iflag2[dimavg1] = 0;
		    phpta2[0].v[dimavg1] = MISSING;
		    cmpta2[0].v[dimavg1] = cmpta2[0].v[dimavg2] = MISSING;
		    phpta2[1] = phpta2[0];
		    cmpta2[1] = cmpta2[0];
		    phpta2[1].v[dimavg1] = max[dimavg1];
		    convert(phpta2, cmpta2, iflag2, *dims, coord_dep, 2);
		    if (icounter == 0)
			first_stepby = floor(cmpta2[0].v[stepby]);
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
			    for (i=1; i < nda2-1; i++)
				cmpta2[i].v[dimavg2] = (int) (cmin2+i);
			    for (i=0; i < *dims; i++) {
				if (i != dimavg2 && i != dfree)
				    cmpta2[1].v[i] = MISSING;
				else phpta2[1].v[i] = MISSING;
			    }
			    cmpta2[1].v[dimavg1] = MISSING;
			    convert(phpt2+1, cmpt2+1, iflag2, *dims,
				    coord_dep, nda2-2);
			}
		    }
		    sum2 = dx2 = 0.;
		    for(tprev2=tt2=0; tprev2 < nda2-1;
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
				    cmpta1[i].v[dimavg1] = (int) (cmin1+i);
				}
				phpta1[1].v[dimavg1]
				    = phpta1[1].v[dimavg2] = MISSING;
				cmpta1[1].v[stepby] = cmpta1[1].v[other]
				    = MISSING;
				convert(phpta1+1, cmpta1+1, iflag1,
					*dims, coord_dep, nda1-2);
			    }
			    sum1 = dx1 = 0;
			    for (tprev1 = tt1 = 0; tprev1 < nda1-1;
				 tprev1=tt1,tt1=tnext1) {
				tnext1 = MIN(tt1 + 1, nda1-1);
				if ((value=(float)interp_(data,nx,ny,nz,nt,
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
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum2/dx2;
		    }
		    else
			slab_l.slab[icounter] = sum2;
		    slab_l.ploc[icounter] = phpta2[0].v[stepby];
		}
	    }
	    /*
	       Well, that didn't work, now we have to depend on the
	       user transform to do our work for us.
	       */
	    else {
		struct point4a endpt[2];
		struct point4 *data_slicer_1d_trans_();
		int da[2], nda[2];

		da[0] = dimavg1+1;
		da[1] = dimavg2+1;
	        for (i=0; i < *dims; i++) {
		    if (i == stepby) {
			endpt[0].v[i] = pt1[i];
			endpt[1].v[i] = pt2[i];
		    }
		    else if (i==other) {
			endpt[0].v[i] = pt1[stepby]*slope+intercept;
			endpt[0].v[i] = pt2[stepby]*slope+intercept;
		    }
		    else
			endpt[0].v[i] = min[i];
			endpt[1].v[i] = max[i];
		}
		cmpt = data_slicer_1d_trans_(endpt, &nis, da, &nda);
		nda1 = nda[0];
		nda2 = nda[1];
		if ((cmpt2=(struct point4 *)malloc(nda2
						   *sizeof(struct point4)))
		    == (struct point4 *)0)
		    {
			(void)make_help_widget_
			    ("slicer2d2a: can't allocate memory - cmpt2");
			free((char *)cmpt);
			return((float *) 0);
		    }
		if ((phpt=(struct point4 *)malloc((nda1+nda2)
						  *sizeof(struct point4)))
		    == (struct point4 *)0)
		    {
			(void)make_help_widget_
			    ("slicer: can't allocate memory - phpt");
			free((char *)cmpt);
			free((char *)cmpt2);
			return((float *) 0);
		    }
		phpt1 = phpt;
		phpt2 = phpt1 + nda1;
		phpta1 = (struct point4a *) phpt1;
		phpta2 = (struct point4a *) phpt2;
		if ((slab_l.slab = (float *)malloc(nis * sizeof(float)))
		    == (float *)0)
		{
		    (void)make_help_widget_
			("slicer: can't allocate memory - slab");
		    free((char *)phpt);
		    free((char *)cmpt);
		    free((char *)cmpt2);
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
		    free((char *)cmpt2);
		    return((float *) 0);
		}
		for ( i=0; i < 4; i++) {
		    iflag1[i] = 1;
		    iflag2[i] = (i != dimavg1);
		}
		for(icounter = 0 ; icounter < nis; icounter++){
		    int nda2a = (nda2 == 1) ? 2: nda2;

		    for (i=0; i < nda2; i++) {
			cmpt2[i] = *(cmpt + (icounter*nda2 + i)*nda1);
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
			cmpt1 = cmpt + (icounter*nda2 + tt2)*nda1;
			if (nda1 == 1) {
			    sum1 = (float)interp_(data,nx,ny,nz,nt,
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
			    slab_l.slab[icounter] = *special;
			else
			    slab_l.slab[icounter] = sum2/dx2;
		    }
		    else
			slab_l.slab[icounter] = sum2;
		    slab_l.ploc[icounter] = phpta2[0].v[stepby];
		}
	    }
	    free(cmpt);
	    free(phpt);
	    slab_l.xaxis = stepby+11;
	    slab_l.yaxis = 0;
	    slab_l.slope = slope;
	    slab_l.intercept = intercept;
	    slab_l.stepby = stepby;
	    slab_l.other = other;
	    slab_l.dimavg1 = dimavg1;
	    slab_l.dimavg2 = dimavg2;
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
