#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif
#ifdef __osf__
#define memalign(size1,size2) malloc(size2)
#endif
static char rcsid[] = "$Id: pslicer2d.c,v 1.5 2000/08/02 19:59:43 harry Exp $";
/* $Id: pslicer2d.c,v 1.5 2000/08/02 19:59:43 harry Exp $
 * Purpose:
 *   This routine is used to get the computational points for an evenly
 *   spaced physical slice.
 *
 * Arguments:
 * corner	float *	input	4 corner points of slice in physical space.
 * ii		int *	input	axis coordinate of ni.
 * jj		int *	input	axis coordinate of nj.
 * ni		int *	output	Number of points in the x-direction.
 * nj		int *	output	Number of points in the y-direction.
 * da		int *	input	axis coordinate of average(s)
 * nda		int *	output	Number of points in the da-direction(s).
 *
 * The corners will be:     2    3
 *                      jj
 *                          0    1
 *                            ii
 *
 * $Log: pslicer2d.c,v $
 * Revision 1.5  2000/08/02 19:59:43  harry
 * Fix surface vector plotting.
 *
 * Revision 1.4  1998/07/16 22:10:57  harryive
 * Add two dimensional slicing with 2 averaged dimensions.
 *
 * Revision 1.3  1997/02/21 20:02:10  harry
 * Fix vector interval to be floating point and have values less than one.
 * Handling of vector interval is now in the slicer.
 *
 * Revision 1.2  1995/10/18 19:18:19  warren
 * Stipples from 3.3, First crack at mouse windowing and put window_points_
 * into a header file and out of the mouse stuff and slicers. Touched almost
 * everything it seems.
 *
 * Revision 1.1  1995/09/05  20:34:09  harry
 * Add new slicer.  Good luck!
 *
 */

#include <math.h>
#include <missing.h>
#include <malloc.h>

#ifndef MAX
#define MAX(x, y) ((x) > (y)? (x):(y))
#endif
#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif

extern void getlvar_(),phys_2_index_trans_(),getiarr_(),convert();

float *
pslicer2d_(corner, ii, jj, ni, nj, da, nda, ri, rj)

float *corner, *ri, *rj;
int *ii, *jj, *ni, *nj, *da, *nda;
{
    /*
       Let's do the obvious, just divide the slice evenly, using the
       number of computation points represented by the slice.
       */
    struct point4a {
	float v[4];
    } *pcorn = (struct point4a *) corner, ccorn[4], *pslice, *slice, *p, *s;
    int iflag[4], four=4, i, j, k, l, j1, j2, stepby, other, error;
    int surface;
    float dx, dy, dz, x, y, z, cpmin[4], cmin, cmax;

    (void)getlvar_("surface", &surface, &j, 7);
    for (i=0; i < 4; i++) iflag[i] = 1;
    phys_2_index_trans_(corner, ccorn, iflag, &four, &four);
    *ni = ceil(MAX(ccorn[1].v[*ii-1], ccorn[3].v[*ii-1]))
	- floor(MIN(ccorn[0].v[*ii-1], ccorn[2].v[*ii-1])) + 1;
    if (*ri > 0.) *ni = (*ni-1)*(*ri) + 1;
    dx = (pcorn[1].v[*ii-1] - pcorn[0].v[*ii-1]) / (*ni - 1);
    if (*da == 0) {
	if (*jj < 10) {
	    *nj = ceil(MAX(ccorn[2].v[*jj-1], ccorn[3].v[*jj-1]))
		- floor(MIN(ccorn[0].v[*jj-1], ccorn[1].v[*jj-1])) + 1;
	    if (*rj > 0.) *nj = (*nj-1)*(*rj) + 1;
	    dy = (pcorn[2].v[*jj-1] - pcorn[0].v[*jj-1]) / (*nj - 1);
	    pslice = (struct point4a *) memalign(sizeof(struct point4a),
					   sizeof(struct point4a)*(*ni * *nj));
	    slice = (struct point4a *) memalign(sizeof(struct point4a),
					   sizeof(struct point4a)*(*ni * *nj));
	    p = pslice;
	    for (j=0; j < *nj; j++) {
		y = pcorn[0].v[*jj-1] + j*dy;
		for (i=0; i < *ni; i++, p++) {
		    *p = pcorn[0];
		    p->v[*ii-1] += i*dx;
		    p->v[*jj-1] = y;
		}
	    }
	}
	else {
	    stepby = ((int)(*jj/10))-1;
	    other = j%10-1;
	    j1 = ceil(MAX(ccorn[2].v[stepby], ccorn[3].v[stepby]))
		- floor(MIN(ccorn[0].v[stepby], ccorn[1].v[stepby])) + 1;
	    y = ccorn[0].v[other];
	    z = ccorn[0].v[other];
	    for (i=1; i < 4; ++i) {
		y = MAX(y, ccorn[i].v[other]);
		z = MIN(y, ccorn[i].v[other]);
	    }
	    j2 = ceil(y) - floor(z) + 1;
	    *nj = j1 + j2;
	    if (*rj > 0.) *nj = (*nj-1)*(*rj) + 1;
	    dy = (pcorn[2].v[stepby] - pcorn[0].v[stepby]) / (*nj - 1);
	    dz = (pcorn[2].v[other] - pcorn[0].v[other]) / (*nj - 1);
	    pslice = (struct point4a *) memalign(sizeof(struct point4a),
					   sizeof(struct point4a)*(*ni * *nj));
	    slice = (struct point4a *) memalign(sizeof(struct point4a),
					   sizeof(struct point4a)*(*ni * *nj));
	    p = pslice;
	    for (j=0; j < *nj; j++) {
		y = pcorn[0].v[stepby] + j*dy;
		z = pcorn[0].v[other] + j*dz;
		for (i=0; i < *ni; i++, p++) {
		    *p = pcorn[0];
		    p->v[*ii-1] += i*dx;
		    p->v[stepby] = y;
		    p->v[other] = z;
		}
	    }
	}
	j = *ni * *nj;
    }
    /*
      One dimensional average.
    */
    else if (da[1] == 0) {
	float dda;

	getiarr_("cpmin",cpmin,&four,&error,5,4);
	if (ccorn[0].v[*da-1] < 0 ) cmin = cpmin[*da-1];
	else cmin = ccorn[0].v[*da-1];
	if (ccorn[3].v[*da-1] < 0 ) cmax = cpmin[*da-1];
	else cmax = ccorn[3].v[*da-1];
	if (cmax == cmin) *nda = 1;
	else {
	    *nda = ceil(cmax) - floor(cmin) + 1;
	    dda = (pcorn[3].v[*da-1] - pcorn[0].v[*da-1])/ (*nda - 1);
	}
	if (*jj < 10) {
	    *nj = ceil(MAX(ccorn[2].v[*jj-1], ccorn[3].v[*jj-1]))
		- floor(MIN(ccorn[0].v[*jj-1], ccorn[1].v[*jj-1])) + 1;
	    if (*rj > 0.) *nj = (*nj-1)*(*rj) + 1;
	    dy = (pcorn[2].v[*jj-1] - pcorn[0].v[*jj-1]) / (*nj - 1);
	    pslice = (struct point4a *) memalign(sizeof(struct point4a),
				sizeof(struct point4a)*(*ni * *nj * *nda));
	    slice = (struct point4a *) memalign(sizeof(struct point4a),
				sizeof(struct point4a)*(*ni * *nj * *nda));
	    p = pslice;
	    for (j=0; j < *nj; j++) {
		y = pcorn[0].v[*jj-1] + j*dy;
		for (i=0; i < *ni; i++) {
		    x = pcorn[0].v[*ii-1] + i*dx;
		    if (*nda == 1) {
			*p = pcorn[0];
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p++;
		    }
		    else for (k=0; k < *nda; k++, p++) {
			*p = pcorn[0];
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p->v[*da-1] += k*dda;
		    }
		}
	    }
	}
	else {
	    stepby = ((int)(*jj/10))-1;
	    other = j%10-1;
	    j1 = ceil(MAX(ccorn[2].v[stepby], ccorn[3].v[stepby]))
		- floor(MIN(ccorn[0].v[stepby], ccorn[1].v[stepby])) + 1;
	    y = ccorn[0].v[other];
	    z = ccorn[0].v[other];
	    for (i=1; i < 4; ++i) {
		y = MAX(y, ccorn[i].v[other]);
		z = MIN(y, ccorn[i].v[other]);
	    }
	    j2 = ceil(y) - floor(z) + 1;
	    *nj = j1 + j2;
	    if (*rj > 0.) *nj = (*nj-1)*(*rj) + 1;
	    dy = (pcorn[2].v[stepby] - pcorn[0].v[stepby]) / (*nj - 1);
	    dz = (pcorn[2].v[other] - pcorn[0].v[other]) / (*nj - 1);
	    pslice = (struct point4a *) memalign(sizeof(struct point4a),
				sizeof(struct point4a)*(*ni * *nj * *nda));
	    slice = (struct point4a *) memalign(sizeof(struct point4a),
				sizeof(struct point4a)*(*ni * *nj * *nda));
	    p = pslice;
	    for (j=0; j < *nj; j++) {
		y = pcorn[0].v[stepby] + j*dy;
		z = pcorn[0].v[other] + j*dz;
		for (i=0; i < *ni; i++) {
		    x = pcorn[0].v[*ii-1] + i*dx;
		    if (*nda == 1) {
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p->v[other] = z;
			p->v[*da-1] = pcorn[0].v[*da-1];
			p++;
		    }
		    else for (k=0; k < *nda; k++, p++) {
			p->v[*ii-1] = x;
			p->v[stepby] = y;
			p->v[other] = z;
			p->v[*da-1] = pcorn[0].v[*da-1] + k*dda;
		    }
		}
	    }
	}
	j = *ni * *nj * *nda;
    }
    /*
      Two dimensional average.
    */
    else {
	float da2, dda[2];

	getiarr_("cpmin",cpmin,&four,&error,5,4);
	for (i=0; i < 2; i++) {
	    if (ccorn[0].v[da[i]-1] < 0 ) cmin = cpmin[da[i]-1];
	    else cmin = ccorn[0].v[da[i]-1];
	    if (ccorn[3].v[da[i]-1] < 0 ) cmax = cpmin[da[i]-1];
	    else cmax = ccorn[3].v[da[i]-1];
	    if (cmax == cmin) nda[i] = 1;
	    else {
		nda[i] = ceil(cmax) - floor(cmin) + 1;
		dda[i] = (pcorn[3].v[da[i]-1] - pcorn[0].v[da[i]-1])
		    / (nda[i] - 1);
	    }
	}
	*nj = ceil(MAX(ccorn[2].v[*jj-1], ccorn[3].v[*jj-1]))
	    - floor(MIN(ccorn[0].v[*jj-1], ccorn[1].v[*jj-1])) + 1;
	if (*rj > 0.) *nj = (*nj-1)*(*rj) + 1;
	dy = (pcorn[2].v[*jj-1] - pcorn[0].v[*jj-1]) / (*nj - 1);
	pslice = (struct point4a *) memalign(sizeof(struct point4a),
			sizeof(struct point4a)*(*ni * *nj * nda[0] * nda[1]));
	slice = (struct point4a *) memalign(sizeof(struct point4a),
			sizeof(struct point4a)*(*ni * *nj * nda[0] * nda[1]));
	p = pslice;
	for (j=0; j < *nj; j++) {
	    y = pcorn[0].v[*jj-1] + j*dy;
	    for (i=0; i < *ni; i++) {
		x = pcorn[0].v[*ii-1] + i*dx;
		for (k=0; k < nda[1]; k++) {
		    da2 = pcorn[0].v[da[1]-1] + k*dda[1];
		    if (nda[0] == 1) {
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p->v[da[1]-1] = da2;
			p->v[da[0]-1] = pcorn[0].v[da[0]-1];
			p++;
		    }
		    else for (l=0; l < nda[0]; l++, p++) {
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p->v[da[1]-1] = da2;
			p->v[da[0]-1] = pcorn[0].v[da[0]-1] + l*dda[0];
		    }
		}
	    }
	}
	j = *ni * *nj * nda[0] * nda[1];
    }
    if (surface) {
	int coord_dep[4][4];

	i = 16;
	(void)getiarr_("coord_dep",coord_dep,&i,&error,9,16);
	p = pslice;
	s = slice;
	p->v[2] = s->v[0] = s->v[1] = s->v[3] = MISSING;
	for (i=0; i < j; i++, s++) s->v[2] = 1;
	convert(pslice, slice, iflag, 4, coord_dep, j);
    }
    else
	phys_2_index_trans_(pslice, slice, iflag, &four, &j);
    free(pslice);
    return((float *) slice);
}
