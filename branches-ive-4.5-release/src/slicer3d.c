static char rcsid[] = "$Id: slicer3d.c,v 1.2 2001/01/23 22:47:17 davidive Exp $";
/* $Id: slicer3d.c,v 1.2 2001/01/23 22:47:17 davidive Exp $
 * Purpose:
 *   This routine is used to slicer a three dimensional cube from a
 *   four dimentional data set. It returns a pointer to the slice
 *   requested.
 *
 *   data:      the data set to slice [*ny][*nx][*nt][*nz]
 *   dims:      number of dimensions of the data
 *   lock:      the locked dimension
 *   pt:        the value of the locked dimension in physical space.
 *   nk:	number of points in first dimension of slice
 *   nj:	number of points in second dimension of slice
 *   ni:	number of points in third dimension of slice
 *   special:   special value
 *   phys:      If one - do evenly space slice in physical space
 *
 *   $Log: slicer3d.c,v $
 *   Revision 1.2  2001/01/23 22:47:17  davidive
 *   cpline.f: fixed to use zero_color
 *   everything else: First cut at wireframes and isosurfaces. No dimentions yet
 *   no widgets yet.
 *
 *   Revision 1.1  1996/07/18 18:02:39  harry
 *   Initial check-in of 3d slicer.
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

#ifndef MAX
#define MAX(x, y) ((x) > (y)? (x):(y))
#endif
#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif
#define slab_dep(x,y,z) *(slab_3.slab + (x) + (((y) + ((z) * njs)) * nis))
#define wslab_dep(x,y,z) *(wslab + (x) + (((y) + ((z) * *nj)) * *ni))
#define phpts3_dep(x,y,z) (phpts3.pt + (x) + (((y) + ((z) * njs)) * nis))

static int nis, njs, nks;

struct {
    int locked, dimavg, phys;
    float avgmin, avgmax, *slab;
    float slope, intercept, vlocked, *data;
    int xaxis, yaxis, zaxis;
    char datfil[256], field[256];
} slab_3 = {-1, -1, -1, 
	    0., 0., (float *)0, 
	    0., 0., 0., (float *)0, 
	    0, 0, 0, 
	    "",""};

struct point {
    float x,y,z;
};
struct point3{
    struct point *pt;
    int numx;
    int numy;
    int numz;
} ;
struct point3 phpts3 = {(struct point *)0,
			0, 0, 0};
struct wpt3 wp3;

extern double interp_();

float 
  *slicer3d_(data, nx, ny, nz, nt,
	   dims, lock, pt, nk, nj, ni, special, phys)
float *data, *pt, *special;
int *dims, *lock, *nx, *ny, *nz, *nt, *ni, *nj, *nk, *phys;
{
    char datfil[256], field[256];
    double dmax[4],dmin[4],dfmin[4],dfmax[4];
    float *wslab;
    float min[4], max[4], fullmin[4], fullmax[4];
    float ctest[4], ptest[4], c1[4], c2[4];
    float tester, slope, intercept, pstart, pend;
    int cpmin[4], cpmax[4], coord_dep[4][4], indep[4];
    int dcounter, icounter, kcounter, iflag[4], i, j, k, v[4];
    int locked, rconst, zeroes;
    int error, stepby, other, dfree, good;
    int ii, jj, kk, same_slice, ix, iy;
    size_t amount;
    static int one=1;
    struct point4{
	float x,y,z,t;
    } *cmpt, *phpt;
    struct point4a{
	float v[4];
    } *cmpta, *phpta;
    struct point *phtmp;

    if( *dims != 4 ) {
	(void)make_help_widget_("slicer3d: must be 4 dimensional field");
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
    locked = *lock - 1;
    if (slab_3.phys == *phys &&
	slab_3.data == data &&
	strcmp(slab_3.datfil, datfil) == 0 &&
	strcmp(slab_3.field, field) == 0 &&
	slab_3.locked == locked &&
	slab_3.vlocked == *pt)
	same_slice = 1;
    else {
	if(phpts3.numx>0 && phpts3.numy > 0 && phpts3.numz > 0){
	    free(phpts3.pt);
	    phpts3.numx = 0;
	    phpts3.numy = 0;
	    phpts3.numz = 0;
	}
	if (slab_3.slab != 0) {
	    free(slab_3.slab);
	    slab_3.slab = 0;
	}
	slab_3.phys = *phys;
	slab_3.data = data;
	slab_3.locked = -1;	/* In case something goes wrong below */
	slab_3.vlocked = *pt;
	strcpy(slab_3.datfil, datfil);
	strcpy(slab_3.field, field);
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
  This is the code for 3D plots orthogonal to the coordinate axes.
****************************************************************************/

    if (!same_slice) {
	if (locked == 0) {
	    ii = 1;
	    jj = 2;
	    kk = 3;
	}
	else {
	    ii = 0;
	    if (locked == 1) {
		jj = 2;
		kk = 3;
	    }
	    else {
		jj = 1;
		if (locked == 2) kk = 3;
		else kk = 2;
	    }
	}

	nks = cpmax[kk] - cpmin[kk] + 1;
	njs = cpmax[jj] - cpmin[jj] + 1;
	nis = cpmax[ii] - cpmin[ii] + 1;

	if(nis < 2 || njs < 2 || nks < 2) {
	    (void)make_help_widget_("slicer3d: empty slice");
	    return (float *)0;
	}		
	/*
	   See if computational slicing is possible.

	   We have one locked coordinates: locked
	   We have three free coordinates: ii,jj && kk in comp space

	   Call convert with one point to see if it will work.
	   */
	if (*phys == 0) {
	    for (icounter=0; icounter < 4; icounter++) iflag[icounter] = 1;
	    ctest[ii] = cpmin[ii];
	    ctest[jj] = cpmin[jj];
	    ctest[kk] = cpmin[kk];
	    ctest[locked] = MISSING;
	    ptest[locked] = dfmin[locked];
	    ptest[ii] = ptest[jj] = ptest[kk] = MISSING;
	}
	if (*phys == 0 && convert(ptest, ctest, iflag, *dims, coord_dep, 1)) {
	    /*get memory*/
	    if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - phpt");
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((cmpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - cmpt");
		free((char *)phpt);
		return((float *) 0);
	    }
	    cmpta = (struct point4a *)cmpt;
	    if ((slab_3.slab = (float *)malloc(njs * nis * nks *sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    if ((phpts3.pt =  
		 (struct point *)malloc(nis * njs * nks 
					* sizeof(struct point ))) == 
		( struct point *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - phpts3");
		free((char *)slab_3.slab);
		slab_3.slab = 0;
		free((char *)phpt);
		free((char *)cmpt);
		return((float *) 0);
	    }
	    phpts3.numx = nis;
	    phpts3.numy = njs;
	    phpts3.numz = nks;

	    /*find line*/
	    for(icounter = 0 ; icounter < nis; icounter++){
		cmpta[icounter].v[ii] = (float) (cpmin[ii] + icounter);
		phpta[icounter].v[locked] = pt[locked];
	    }

	    for (kcounter = 0; kcounter < nks; kcounter++) {
		for(icounter = 0 ; icounter < nis; icounter++){
		    cmpta[icounter].v[kk] = (float) (cpmin[kk] + kcounter);
		}
		for(dcounter = 0 ; dcounter < njs; dcounter++){
		    for(icounter = 0 ; icounter < nis; icounter++){
			cmpta[icounter].v[jj] = (float) (cpmin[jj] + dcounter);
		    }
		    cmpta[0].v[locked] = MISSING;
		    phpta[0].v[ii] = phpta[0].v[jj] = phpta[0].v[kk] = MISSING;
		    convert(phpt, cmpt, iflag, *dims, coord_dep, nis);
		    for(icounter = 0 ; icounter < nis; icounter++){
			slab_dep(icounter,dcounter, kcounter) = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt[icounter].x,&cmpt[icounter].y,
					   &cmpt[icounter].z,&cmpt[icounter].t,
			    special);
			phpts3_dep(icounter, dcounter, kcounter)->x =
			    phpta[icounter].v[ii];
			phpts3_dep(icounter,dcounter, kcounter)->y = 
			    phpta[icounter].v[jj];
			phpts3_dep(icounter,dcounter, kcounter)->z = 
			    phpta[icounter].v[kk];
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
	    struct point4a corner[2];
	    struct point4 *compute, *pslicer3d_(), *data_slicer_3d_trans_();
	    int ii1 = ii+1, jj1 = jj+1, kk1 = kk+1, zero=0;

	    corner[0].v[locked] = corner[1].v[locked] = pt[locked];
	    corner[0].v[ii] = fullmin[ii];
	    corner[0].v[jj] = fullmin[jj];
	    corner[0].v[kk] = fullmin[kk];
	    corner[1].v[ii] = fullmax[ii];
	    corner[1].v[jj] = fullmax[jj];
	    corner[1].v[kk] = fullmax[kk];
	    if (*phys) compute = pslicer3d_(corner, &ii1, &jj1, &kk1,
					    &nis, &njs, &nks,
					    &zero, &zero);
	    else compute = data_slicer_3d_trans_(corner, &ii1, &jj1, &kk1,
						 &nis, &njs, &nks, 
						 &zero, &zero);
	    if (compute == 0) return((float *)0);
	    if ((phpt=(struct point4 *)malloc(nis*sizeof(struct point4)))
		== (struct point4 *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - phpt");
		free((char *)compute);
		return((float *) 0);
	    }
	    phpta = (struct point4a *)phpt;
	    if ((slab_3.slab = (float *)malloc(njs * nis * sizeof(float)))
		== (float *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - slab");
		free((char *)phpt);
		free((char *)compute);
		return((float *) 0);
	    }
	    if ((phpts3.pt =  
		 (struct point *)malloc(nis * njs * nks 
					* sizeof(struct point ))) == 
		( struct point *)0)
	    {
		(void)make_help_widget_
		    ("slicer3d: can't allocate memory - phpts3");
		free((char *)slab_3.slab);
		slab_3.slab = 0;
		free((char *)phpt);
		free((char *)compute);
		return((float *) 0);
	    }
	    phpts3.numx = nis;
	    phpts3.numy = njs;
	    phpts3.numz = nks;
	    iflag[0] = iflag[1] = iflag[2] = iflag[3] = 1;

	    for(kcounter = 0; kcounter < nks; kcounter++) {
		for(dcounter = 0 ; dcounter < njs; dcounter++){
	    
		    cmpt = compute + (dcounter + nis*kcounter) * njs;
		    (void)index_2_phys_trans_(phpt, cmpt, iflag, dims, &nis);

		    for(icounter = 0 ; icounter < nis; icounter++){
			slab_dep(icounter,dcounter,kcounter) = 
			    (float)interp_(data,nx,ny,nz,nt,		
					   &cmpt[icounter].x,&cmpt[icounter].y,
					   &cmpt[icounter].z,&cmpt[icounter].t,
					   special);
			phpts3_dep(icounter, dcounter, kcounter)->x =
			    phpta[icounter].v[ii];
			phpts3_dep(icounter,dcounter, kcounter)->y = 
			    phpta[icounter].v[jj];
			phpts3_dep(icounter,dcounter, kcounter)->z = 
			    phpta[icounter].v[kk];
		    }
		}
	    }
	    free(compute);
	    free(phpt);
	}
	slab_3.xaxis = ii+1;
	slab_3.yaxis = jj+1;
	slab_3.zaxis = kk+1;
	slab_3.locked = locked;
    }
    /*
      Figure out where in the slice is the window.
      */
    wp3.imin = nis;
    wp3.imax = -1;
    wp3.jmin = njs;
    wp3.jmax = -1;
    wp3.kmin = nks;
    wp3.kmax = -1;
    wp3.x1 = min[slab_3.xaxis-1];
    wp3.x2 = max[slab_3.xaxis-1];
    wp3.y1 = min[slab_3.yaxis-1];
    wp3.y2 = max[slab_3.yaxis-1];
    wp3.z1 = min[slab_3.zaxis-1];
    wp3.z2 = max[slab_3.zaxis-1];
    for ( kcounter = 0; kcounter < nks; kcounter++ )
	for (dcounter = 0; dcounter < njs; dcounter++ )
	    for ( icounter = 0; icounter < nis; icounter++ ) {
		struct point *pt = phpts3_dep(icounter, dcounter, kcounter);
		if (min[slab_3.xaxis-1] < max[slab_3.xaxis-1]) {
		    if (pt->x > min[slab_3.xaxis-1]
			&& icounter < wp3.imin)
			wp3.imin = icounter;
		    if (pt->x < max[slab_3.xaxis-1]
			&& icounter > wp3.imax)
			wp3.imax = icounter;
		}
		else {
		    if (pt->x < min[slab_3.xaxis-1]
			&& icounter < wp3.imin)
			wp3.imin = icounter;
		    if (pt->x > max[slab_3.xaxis-1]
			&& icounter > wp3.imax)
			wp3.imax = icounter;
		}
		if (min[slab_3.yaxis-1] < max[slab_3.yaxis-1]) {
		    if (pt->y > min[slab_3.yaxis-1]
			&& dcounter < wp3.jmin)
			wp3.jmin = dcounter;
		    if (pt->y < max[slab_3.yaxis-1]
			&& dcounter > wp3.jmax)
			wp3.jmax = dcounter;
		}
		else {
		    if (pt->y < min[slab_3.yaxis-1]
			&& dcounter < wp3.jmin)
			wp3.jmin = dcounter;
		    if (pt->y > max[slab_3.yaxis-1]
			&& dcounter > wp3.jmax)
			wp3.jmax = dcounter;
		}
		if (min[slab_3.zaxis-1] < max[slab_3.zaxis-1]) {
		    if (pt->z > min[slab_3.zaxis-1]
			&& dcounter < wp3.kmin)
			wp3.kmin = dcounter;
		    if (pt->z < max[slab_3.zaxis-1]
			&& dcounter > wp3.kmax)
			wp3.kmax = dcounter;
		}
		else {
		    if (pt->z < min[slab_3.zaxis-1]
			&& dcounter < wp3.kmin)
			wp3.kmin = dcounter;
		    if (pt->z > max[slab_3.zaxis-1]
			&& dcounter > wp3.kmax)
			wp3.kmax = dcounter;
		}
	    }
    wp3.imin = MAX(wp3.imin-1, 0);
    wp3.imax = MIN(wp3.imax+1, nis-1);
    wp3.jmin = MAX(wp3.jmin-1, 0);
    wp3.jmax = MIN(wp3.jmax+1, njs-1);
    wp3.kmin = MAX(wp3.kmin-1, 0);
    wp3.kmax = MIN(wp3.kmax+1, nks-1);
/*
 */
    wp3.reverse = 0;
    wp3.xaxis = slab_3.xaxis;
    wp3.yaxis = slab_3.yaxis;
    wp3.zaxis = slab_3.zaxis;
    *ni = wp3.imax - wp3.imin + 1;
    *nj = wp3.jmax - wp3.jmin + 1;
    *nk = wp3.kmax - wp3.kmin + 1;
    amount = *ni * *nj * *nk * sizeof(float);
    if ((wslab = (float *)malloc(amount))
	== (float *)0)
      {
	(void)make_help_widget_
	  ("slicer3d: can't allocate memory - wslab");
	return((float *) 0);
      }
    for (k = 0; k < *nk; k++)
	for (j = 0; j < *nj; j++)
	  for (i=0; i < *ni; i++){
	    wslab_dep(i, j, k) = slab_dep(i+wp3.imin, j+wp3.jmin, k+wp3.kmin);
	  }
    window_points3_ = wp3;
    return(wslab);
}
