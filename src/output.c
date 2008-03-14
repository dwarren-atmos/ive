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
#include <ive_macros.h>

void
output_1d_(filename, x, y, z, t, f,ni,field,netcdf,fillen, fieldlen)

char *filename;		/* File to open for writing. */
char *field;		/* Field name */
float *x,*y,*z,*t,*f;		/* Slice to write and locations. */
int *ni;			/* Number of points*/
int fillen, fieldlen;		/* Length of character variables */
int *netcdf;                                        /*1 if netcdf 0 if not*/

{
  FILE *out;
  int i;
  if(!*netcdf){
    out = fopen(filename, "w");
    
    fprintf(out, "%15.15s %15.15s %15.15s %15.15s %15.15s\n", "X","Y","Z","T", field);
    for (i=0; i < *ni; i++){
      fprintf(out, "%15.5g %15.5g %15.5g %15.5g %15.5g\n", x[i], y[i],z[i],t[i],f[i]);
    }
    
    fclose(out);
  }
  else{
    make_help_widget("We do not yet support 1d netcdf outputs");
  }
}
