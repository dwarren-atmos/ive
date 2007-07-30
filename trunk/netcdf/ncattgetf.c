/*
   $Id: ncattgetf.c,v 1.2 1996/03/05 18:21:29 harry Exp $

   This routine returns in specified netCDF attribute as a floating point
   number.  The actual attribute may be NC_FLOAT, NC_BYTE, NC_CHAR,
   NC_SHORT, NC_LONG or NC_DOUBLE.

   $Log: ncattgetf.c,v $
   Revision 1.2  1996/03/05 18:21:29  harry
   Isolate the use of "long" for dimension sizes to the netcdf routines.
   Everywhere else we will use "int" or "integer".  Also change use of
   long to nclong as a netCDF type as required by netCDF version 2.4.

   Revision 1.1  1995/11/20 17:54:09  harry
   Add routine ncattgetf to return the value of an attribute as a float no
   matter how it appears in the netCDF file.

*/
static char ident[] = "$Id: ncattgetf.c,v 1.2 1996/03/05 18:21:29 harry Exp $";

#include <netcdf.h>

int ncattgetf(int ncid, int varid, const char* name, float *value)
{
    short s;
    int err;
    nclong l;
    nc_type datatype;
    char c;
    double d;
    
    if ((err=ncattinq(ncid, varid, name, &datatype, (int *) 0)) == -1)
	return(err);
    switch (datatype) {
    case NC_FLOAT:
	err = ncattget(ncid, varid, name, value);
	break;
    case NC_BYTE:
    case NC_CHAR:
	err = ncattget(ncid, varid, name, &c);
	*value = c;
	break;
    case NC_SHORT:
	err = ncattget(ncid, varid, name, &s);
	*value = s;
	break;
    case NC_LONG:
	err = ncattget(ncid, varid, name, &l);
	*value = l;
	break;
    case NC_DOUBLE:
	err = ncattget(ncid, varid, name, &d);
	*value = d;
    }
    return(err);
}
	
