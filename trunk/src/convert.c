static char rcsid[] = "$Id: convert.c,v 1.2 1998/07/16 22:10:53 harryive Exp $";

/* $Id: convert.c,v 1.2 1998/07/16 22:10:53 harryive Exp $
 * Purpose
 *   This routine interatively converts from physical to computational
 *   coordinates and vice-versa, until either the needed dimensions
 *   have been converted, or until no more work can be done.
 *
 *   This routine assumes that all members of the phys and comp arrays
 *   have the same missing dimensions as the first point.  It uses the
 *   value "MISSING" to indicate a missing point.
 *
 *   phys:	Array of physical coordinates.
 *   comp:	Array of computational coordinates.
 *   need:	Array specifying the needed dimensions.
 *   dims:	Number of dimensions (assumed to be 4).
 *   coord_dep:	An array specifying the dependencies of a dimension on
 *		the others.
 *   npts:	Number of points to convert.
 *
 * $Log: convert.c,v $
 * Revision 1.2  1998/07/16 22:10:53  harryive
 * Add two dimensional slicing with 2 averaged dimensions.
 *
 * Revision 1.1  1995/09/05 20:33:38  harry
 * Add new slicer.  Good luck!
 *
 */

#include <missing.h>

void phys_2_index_trans_(float *, float *, int *, int *, int*);
void index_2_phys_trans_(float *, float *, int *, int *, int*);

int
convert(float *phys, float *comp, int *need, int dims, int coord_dep[4][4],
	int npts)

{
    int change, compute, okay, i, j, flag[4];

    if (npts <= 0) return(1);
    for(;;) {
	change = 0;
	compute = 0;
	/*
	   Is there anything I can convert from phys to comp?
	   */
	for (j=0; j < 4; j++) flag[j] = 0;
	for (i=0; i < 4; i++) {
	    if (comp[i] == MISSING && phys[i] != MISSING) {
		for (j=0, okay=1; j < 4 && okay; j++)
		    /*
		      i = f(j)
		    */
		    if (coord_dep[i][j]) okay = (phys[j] != MISSING);
		if (okay) {
		    compute = 1;
		    for (j=0; j < 4; j++)
			if (!flag[j]) flag[j] = coord_dep[i][j];
		}
	    }
	}
	if (compute) {
	    change = 1;
	    phys_2_index_trans_(phys, comp, flag, &dims, &npts);
	}
	compute = 0;
	/*
	   Is there anything I can convert from comp to phys?
	   */
	for (j=0; j < 4; j++) flag[j] = 0;
	for (i=0; i < 4; i++) {
	    if (phys[i] == MISSING && comp[i] != MISSING) {
		for (j=0, okay=1; j < 4 && okay; j++)
		    /*
		      i = f(j)
		    */
		    if (coord_dep[i][j]) okay = (comp[j] != MISSING);
		if (okay) {
		    compute = 1;
		    for (j=0; j < 4; j++)
			if (!flag[j]) flag[j] = coord_dep[i][j];
		}
	    }
	}
	if (compute) {
	    change = 1;
	    index_2_phys_trans_(phys, comp, flag, &dims, &npts);
	}
	if (change) {
	    for (i = 0; i < 4; i++) {
		if (need[i] && (phys[i] == MISSING || comp[i] == MISSING)) {
		    change = 0;
		    break;
		}
	    }
	    if (change) return(1);
	}
	else return(0);
    }
}






