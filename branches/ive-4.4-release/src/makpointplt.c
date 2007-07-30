static char rcsid[] = "$Id: makpointplt.c,v 1.5 2007-01-09 22:52:14 warren Exp $";
/* $Id: makpointplt.c,v 1.5 2007-01-09 22:52:14 warren Exp $
 * Purpose:
 *   This routine is used to calculate and print out a point value.
 *
 *   data:	the data set to slice [*nx][*ny][*nz][*nt]
 *
 *   $Log: makpointplt.c,v $
 *   Revision 1.5  2007-01-09 22:52:14  warren
 *   init all parts of iflag
 *
 *   Revision 1.4  2002/05/16 20:29:41  harryive
 *   slicer1d1a expects a Fortran dimension for the average dimension, not C.
 *
 *   Revision 1.3  2000/02/14 21:59:36  harry
 *   Allow point surface plots.
 *
 *   Revision 1.2  2000/02/11 23:14:36  harry
 *   Allow surface 1d plots.  Fix Z slider for surface plots.
 *
 *   Revision 1.1  1998/09/30 22:28:34  harry
 *   Rewrite makpointplt and add ability for 2 average points, fix errors
 *   in the 1d slicers, remove obsolete yuri routines.
 *
 */
#include <window.h>
#include <missing.h>

#ifndef MIN
#define MIN(x, y) ((x) < (y)? (x):(y))
#endif

extern struct wpt wp;

void
makpointplt_(float *data, int *nx, int *ny, int *nz, int *nt)
{
    double interp_();
    float special, pt1[4], pt2[4], cmpt[4], value, sum, dx, *slabloc, *slabad,
	plwmin[4], plwmax[4], *slicer1d1a_(), *slicer1d_();
    int four=4, lock[4], da[2], buttons, tt, tprev, tnext, ni, error, i, na,
	counter, iflag[4], one=1, size, surface;
    char result[64];

    getrvar_("spval", &special, &error, 5);
    getivar_("use_buttons", &buttons, &error, 11);
    if (buttons) {
	getiarr_("lock", lock, &four, &error, 4, 4);
	getrvar_("xloc", pt1, &error, 4);
	getrvar_("yloc", pt1+1, &error, 4);
	getrvar_("zloc", pt1+2, &error, 4);
	getrvar_("tloc", pt1+3, &error, 4);
	na = 0;
	counter = 0;
	for (i=0; i < 4; i++) {
	    if (lock[i] == 1) counter++;
	    else if (lock[i] == 2) da[na++] = i;
	}
    }
    else {
	counter = 4;
	getrarr_("pt1", pt1, &four, &error, 4, 4);
    }
    getlvar_("surface", &surface, &error, 7);
    if (counter == 4) {
	/*
	  No average, just get value.
	*/
	for (i=0; i < 4; ++i) iflag[i] = 1;
	if (surface) {
	    int coord_dep[4][4];

	    i = 16;
	    (void)getiarr_("coord_dep",coord_dep,&i,&error,9,16);
	    pt1[2] = MISSING;
	    cmpt[0] = cmpt[1] = cmpt[3] = MISSING;
	    cmpt[2] = 1;
	    if (!convert(pt1, cmpt, iflag, 4, coord_dep, 1)) {
		(void) make_help_widget_
		    ("makpointplt: cannot get surface point with this transform");
		return;
	    }
	}
	else phys_2_index_trans_(pt1, cmpt, iflag, &four, &one);
	value = interp_(data, nx, ny, nz, nt, cmpt, cmpt+1, 
			cmpt+2, cmpt+3, &special);
	if (value == special) make_value_widget_("No data for that point");
	else {
	    ftoa_c_(&value, result, &size, sizeof(result));
	    result[size] = '\0';
	    write_value_ch_(pt1, result, surface);
	}
    }
    else { 
	/*
	  Get a line and average it.
	*/
	getrarr_("plwmin", plwmin, &four, &error, 6, 4);
	getrarr_("plwmax", plwmax, &four, &error, 6, 4);
	for (i=0; i < 4; i++) pt2[i] = pt1[i];
	pt1[da[0]] = plwmin[da[0]];
	pt2[da[0]] = plwmax[da[0]];
	if (counter == 3) {
	    int zero = 0;
	    slabloc = slicer1d_(data, nx, ny, nz, nt, &four, pt1, pt2,
				  &slabad, &ni, &special, &surface);
	}
	else if (counter == 2) {
	    int davg = da[1]+1;

	    pt1[da[1]] = plwmin[da[1]];
	    pt2[da[1]] = plwmax[da[1]];
	    slabloc = slicer1d1a_(data, nx, ny, nz, nt, &four, pt1, pt2,
				  &davg, &slabad, &ni, &special, &surface);
	}
	if (slabloc == 0) value = special;
	else {
	    if (wp.xaxis == 0) {
		/*
		  Reverse the axis again.
		*/
		float *temp = slabad;
		slabad = slabloc;
		slabloc = temp;
	    }
	    sum = dx = 0;
	    for (tprev=tt=0; tprev < ni-1; tprev=tt, tt=tnext) {
		tnext = MIN(tt+1, ni-1);
		if (slabloc[tt] != special) {
		    sum += slabloc[tt]*(slabad[tnext]-slabad[tprev])*0.5;
		    dx += (slabad[tnext]-slabad[tprev])*0.5;
		}
	    }
	    if (dx == 0) value = special;
	    else value = sum/dx;
	}
	if (value == special) make_value_widget_("No data for that point");
	else {
	    ftoa_c_(&value, result, &size, sizeof(result));
	    result[size] = '\0';
	    write_result_ch_(pt1, pt2, result, surface);
	}
	free(slabloc);
	free(slabad);
    }
}
