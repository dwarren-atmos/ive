#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif
#ifdef __osf__
#define memalign(size1,size2) malloc(size2)
#endif
static char rcsid[] = "$Id: pslicer3d.c,v 1.1 1996/07/18 18:02:37 harry Exp $";
/* $Id: pslicer3d.c,v 1.1 1996/07/18 18:02:37 harry Exp $
 * Purpose:
 *   This routine is used to get the computational points for an evenly
 *   spaced physical slice.
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
 *
 * $Log: pslicer3d.c,v $
 * Revision 1.1  1996/07/18 18:02:37  harry
 * Initial check-in of 3d slicer.
 *
 *
 */

#include <math.h>

#ifndef MAX
#define MAX(x, y) ((x) > (y)? (x):(y))
#endif
#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif

float *
pslicer3d_(corner, ii, jj, kk, ni, nj, nk, da, nda)

float *corner;
int *ii, *jj, *kk, *ni, *nj, *nk, *da, *nda;
{
    /*
       Let's do the obvious, just divide the slice evenly, using the
       number of computation points represented by the slice.
       */
    struct point4a {
	float v[4];
    } *pcorn = (struct point4a *) corner, ccorn[2], *pslice, *slice, *p;
    int iflag[4], two=2, four=4, i, j, k, n, locked, error;
    float dx, dy, dz, x, y, z, dda, cpmin[4], cmin, cmax;

    for (i=0; i < 4; i++) {
	iflag[i] = 1;
	if (i != *ii-1 && i != *jj-1 && i != *kk-1) locked = i;
    }
    phys_2_index_trans_(corner, ccorn, iflag, &four, &two);
    *ni = ccorn[1].v[*ii-1] - ccorn[0].v[*ii-1] + 1;
    dx = (pcorn[1].v[*ii-1] - pcorn[0].v[*ii-1]) / (*ni - 1);
    *nj = ccorn[1].v[*jj-1] - ccorn[0].v[*jj-1] + 1;
    dy = (pcorn[1].v[*jj-1] - pcorn[0].v[*jj-1]) / (*nj - 1);
    *nk = ccorn[1].v[*kk-1] - ccorn[0].v[*kk-1] + 1;
    dz = (pcorn[1].v[*kk-1] - pcorn[0].v[*kk-1]) / (*nk - 1);
    if (*da == 0) {
	pslice = (struct point4a *) memalign(sizeof(struct point4a),
				  sizeof(struct point4a)*(*ni * *nj * *nk));
	slice = (struct point4a *) memalign(sizeof(struct point4a),
				  sizeof(struct point4a)*(*ni * *nj * *nk));
	p = pslice;
	for (k=0; k < *nk; k++) {
	    z = pcorn[0].v[*kk-1] + k*dz;
	    for (j=0; j < *nj; j++) {
		y = pcorn[0].v[*jj-1] + j*dy;
		for (i=0; i < *ni; i++, p++) {
		    p->v[locked] = pcorn[0].v[locked];
		    p->v[*ii-1] = pcorn[0].v[*ii-1] + i*dx;
		    p->v[*jj-1] = y;
		    p->v[*kk-1] = z;
		}
	    }
	}
	j = *ni * *nj * *nk;
    }
    else {
	getiarr_("cpmin",cpmin,&four,&error,5,4);
	if (ccorn[0].v[*da-1] < 0 ) cmin = cpmin[*da-1];
	else cmin = ccorn[0].v[*da-1];
	if (ccorn[1].v[*da-1] < 0 ) cmax = cpmin[*da-1];
	else cmax = ccorn[1].v[*da-1];
	if (cmax == cmin) *nda = 1;
	else {
	    *nda = ceil(cmax) - floor(cmin) + 1;
	    dda = (pcorn[1].v[*da-1] - pcorn[0].v[*da-1])/ (*nda - 1);
	}
	pslice = (struct point4a *) memalign(sizeof(struct point4a),
			      sizeof(struct point4a)*(*ni * *nj * *nk * *nda));
	slice = (struct point4a *) memalign(sizeof(struct point4a),
			      sizeof(struct point4a)*(*ni * *nj * *nk * *nda));
	p = pslice;
	for (k =0; k < *nk; k++) {
	    z = pcorn[0].v[*kk-1] + k*dz;
	    for (j=0; j < *nj; j++) {
		y = pcorn[0].v[*jj-1] + j*dy;
		for (i=0; i < *ni; i++) {
		    x = pcorn[0].v[*ii-1] + i*dx;
		    if (*nda == 1) {
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p->v[*kk-1] = z;
			p->v[*da-1] = pcorn[0].v[*da-1];
			p++;
		    }
		    else for (n=0; n < *nda; n++, p++) {
			p->v[*ii-1] = x;
			p->v[*jj-1] = y;
			p->v[*kk-1] = z;
			p->v[*da-1] = pcorn[0].v[*da-1] + n*dda;
		    }
		}
	    }
	}
	j = *ni * *nj * *nk * *nda;
    }
    phys_2_index_trans_(pslice, slice, iflag, &four, &j);
    free(pslice);
    return((float *) slice);
}
