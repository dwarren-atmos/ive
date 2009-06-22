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

#include <stdlib.h>
#include <stdio.h>
#include <ive_macros.h>
#include <netcdf.h>
#include <string.h>

void
output_1d_(filename, x, y, z, t, f,ni,field,netcdf,fillen, fieldlen)

char *filename;		/* File to open for writing. */
char *field;		/* Field name */
float *x,*y,*z,*t,*f;		/* Slice to write and locations. */
int *ni;			/* Number of points*/
int fillen, fieldlen;		/* Length of character variables */
int *netcdf;                                        /*1 if netcdf 0 if not*/

{
  char dataunits[80],domainunits[320];
  FILE *out;
  int i,error;
  void getavar_();
  void getaarr_();

  i=4;
  getavar_("data_display_units",dataunits,&error,18,80);
  if(error)strcpy(dataunits,"unknown");
  getaarr_("domain_display_units",domainunits,&i,&error,20,80);
  if(error){
    strcpy(domainunits,"unknown");
    strcpy(domainunits+80,"unknown");
    strcpy(domainunits+160,"unknown");
    strcpy(domainunits+240,"unknown");
  }
  if(!*netcdf){
    out = fopen(filename, "w");
    
    fprintf(out, "%15.15s %15.15s %15.15s %15.15s %15.15s\n", "X","Y","Z","T", field);
    fprintf(out, "%15.15s %15.15s %15.15s %15.15s %15.15s\n", 
	    domainunits, domainunits+80,domainunits+160,domainunits+240,dataunits);
    for (i=0; i < *ni; i++){
      fprintf(out, "%15.5g %15.5g %15.5g %15.5g %15.5g\n", x[i], y[i],z[i],t[i],f[i]);
    }
    
    fclose(out);
  }
  else{
    char  *ncfile;
    int  ncid,varid,dim,xd,yd,zd,td,fd,retval;
    long nilong=(long)(*ni);
    long start[] = {0};    /* start at first value */
    long count[] = {nilong};
    #define ERR(e) {printf("Error: %s\n", nc_strerror(e)); return;}

    if ((retval = nc_create(filename, NC_CLOBBER, &ncid)))
      ERR(retval);
    dim=ncdimdef(ncid, "ni",nilong);
    xd = ncvardef(ncid, "X", NC_FLOAT, 1,&dim);
    yd = ncvardef(ncid, "Y", NC_FLOAT, 1,&dim);
    zd = ncvardef(ncid, "Z", NC_FLOAT, 1,&dim);
    td = ncvardef(ncid, "T", NC_FLOAT, 1,&dim);
    fd = ncvardef(ncid, field, NC_FLOAT, 1,&dim);
    ncattput (ncid, xd, "units", NC_CHAR, strlen(domainunits)+1, domainunits);
    ncattput (ncid, yd, "units", NC_CHAR, strlen(domainunits+80)+1, domainunits+80);
    ncattput (ncid, zd, "units", NC_CHAR, strlen(domainunits+160)+1, domainunits+160);
    ncattput (ncid, td, "units", NC_CHAR, strlen(domainunits+240)+1, domainunits+240);
    ncattput (ncid, fd, "units", NC_CHAR, strlen(dataunits)+1, dataunits);
    nc_enddef(ncid);
    ncvarput(ncid, xd, start, count, x);
    ncvarput(ncid, yd, start, count, y);
    ncvarput(ncid, zd, start, count, z);
    ncvarput(ncid, td, start, count, t);
    ncvarput(ncid, fd, start, count, f);
    ncclose(ncid);
  }
}
