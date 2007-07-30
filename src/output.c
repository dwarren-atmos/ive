/*
 * $Id: output.c,v 1.1 1995/05/11 22:15:08 harry Exp $
 * This routines prints out 1D and 2D slices.
 *
 * $Log: output.c,v $
 * Revision 1.1  1995/05/11 22:15:08  harry
 * Add the WRITE_1D command.
 *
 *
 */

static char ident [] = "$Id: output.c,v 1.1 1995/05/11 22:15:08 harry Exp $";

#include <stdio.h>

void
output_1d_(filename, slice, sloc, ni, axes, field, fillen, fieldlen)

char *filename;			/* File to open for writing. */
char *field;			/* Field name */
float *slice, *sloc;		/* Slice to write and locations. */
int *ni, *axes;			/* Number of points and  indep. var */
int fillen, fieldlen;		/* Length of character variables */

{
    FILE *out;
    int i;
    static char *lab[5] = {"X", "Y", "Z", "T", ""};

    out = fopen(filename, "w");

    fprintf(out, "%15.15s %15.15s\n", lab[*axes-1], field);
    if (*axes == 3) {
	for (i=0; i < *ni; i++)
	    fprintf(out, "%15.5g %15.5g\n", sloc[i], slice[i]);
    }
    else {
	for (i=0; i < *ni; i++)
	    fprintf(out, "%15.5g %15.5g\n", slice[i], sloc[i]);
    }
    fclose(out);
}
