/****************************************************************************
$Log: free_var.c,v $
Revision 1.7  2003/12/22 23:20:43  warren
typo found by alpha

Revision 1.5.4.2  2003/12/22 23:20:23  warren
typo found already

Revision 1.5.4.1  2003/12/22 23:12:57  warren
added vid2name_ to find the list of fields
CVS----------------------------------------------------------------------

Revision 1.5  1997/05/28 17:47:31  harry
More changes for NetCDF 3.3.

 * Revision 1.4  1993/12/09  18:35:41  harry
 * Make sure that vatts.disp_units and display_units are always pointing
 * to the same memory location.
 *
 * Revision 1.3  1993/09/17  18:21:24  harry
 * Fix getvid to return -99 for a null field.
 *
 * Revision 1.2  1993/06/29  16:31:20  harry
 * Remove obsolete routine read_d_var, correct add_field to set
 * data_display_orig, change free_var to copy attributes, change
 * get/set_var_atts to not overflow character variables.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:06  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.8  1993/03/22  18:26:08  warren
 * Christoph's fixes for aliasing
 *
 * Revision 2.7  1993/03/08  16:28:01  warren
 * fixed compare_var to only return true if not temporary
 *
 * Revision 2.6  1993/03/05  18:40:54  warren
 * added compare_var_ so getvar can chek if var is new.
 *
 * Revision 2.5  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 2.4  1993/02/03  23:06:35  warren
 * added call to getmem_cleanmem in garbage_collect_vars_
 *
 * Revision 2.3  1993/01/26  16:57:04  millerp
 * Check for NULL rather than 0 for var_info.values.
 *
 * Revision 2.2  1992/12/30  18:00:17  warren
 * fixed garbage collection to go from top down.
 *
 * Revision 2.1  1992/12/29  20:26:09  warren
 * made additions for garbage collection and for freeing
 *
 * Revision 2.0  1992/07/01  21:12:17  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.1  1992/05/12  20:34:07  harry
 * Initial revision
 *
****************************************************************************/
static char ident[] = "$Id: free_var.c,v 1.7 2003/12/22 23:20:43 warren Exp $" ;

#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif

/*
  This subroutine frees the memory from a previous read_var.
  */

#include "cdfinfo.h"
#include <stdio.h>
void make_help_widget_();
cdf_info var_file;

/*
  The following is an abbreviation for the var_info structure for the 
  variable in question.
  */

#define VAR_INFO var_file.vars[*var_id]
#define VAR_INFO1 var_file.vars[new_var_id]
#define VAR_INFO2 var_file.vars[new_var_id + 1]

int free_var_(var_id)

int *var_id;		/* Id of requested variable */

{
    int new_var_id,i;
    int count_varptrs_();

    /*
      Check for valid variable id.
      */
    if (*var_id < 0 || *var_id >= var_file.nvars) {
	return(1);
    }
    if(VAR_INFO.in_file){
	if (VAR_INFO.values != NULL) {
	    if (count_varptrs_(&VAR_INFO.values) == 1) { 
              free(VAR_INFO.values); 
	    }
	    VAR_INFO.values = NULL;
	}
    }
    else{
	if (VAR_INFO.values != NULL) {
	    if (count_varptrs_(&VAR_INFO.values) == 1) { 
              free(VAR_INFO.values); 
	    }
	    VAR_INFO.values = NULL;
	}
	if (VAR_INFO.dims != 0) {
	    free(VAR_INFO.dims);
	    VAR_INFO.dims = 0;
	}
	var_file.nvars--;
	for (new_var_id=*var_id; new_var_id<var_file.nvars; new_var_id++){
	    strcpy(VAR_INFO1.name,VAR_INFO2.name);
	    strcpy(VAR_INFO1.button_name,VAR_INFO2.button_name);
	    VAR_INFO1.datatype  = VAR_INFO2.datatype; 
	    VAR_INFO1.ndims     = VAR_INFO2.ndims;
	    VAR_INFO1.natts     = VAR_INFO2.natts;
	    VAR_INFO1.misdat    = VAR_INFO2.misdat;
	    VAR_INFO1.values    = VAR_INFO2.values;
	    VAR_INFO1.dims      = VAR_INFO2.dims;
	    VAR_INFO1.temporary = VAR_INFO2.temporary;
	    VAR_INFO1.vatts     = VAR_INFO2.vatts;
	    VAR_INFO1.vatts.disp_units = VAR_INFO1.data_display_units;
	    strcpy(VAR_INFO1.data_units, VAR_INFO2.data_units);      
	    strcpy(VAR_INFO1.data_display_units,VAR_INFO2.data_display_units);
	    strcpy(VAR_INFO1.data_display_units_orig,
		   VAR_INFO2.data_display_units_orig);
	}
	for (i=0; i<MAX_NC_NAME; i++)
	  var_file.vars[var_file.nvars].name[i]= '\0';
	for (i=0; i<MAX_NC_NAME; i++)
	  var_file.vars[var_file.nvars].button_name[i] = '\0';
	var_file.vars[var_file.nvars].ndims                    = 0;
	var_file.vars[var_file.nvars].natts                    = 0;
	var_file.vars[var_file.nvars].misdat                   = 0.0;
	*var_file.vars[var_file.nvars].data_units              = '\0';
	*var_file.vars[var_file.nvars].data_display_units      = '\0';
	*var_file.vars[var_file.nvars].data_display_units_orig = '\0';
	var_file.vars[var_file.nvars].dims                     = 0;
	var_file.vars[var_file.nvars].values                   = 0;
	var_file.vars[var_file.nvars].temporary                = 0;
    }
    
    return(0);
}

getvid_(name,len)
     char *name;
     int len;
{
    int i, test;
    char *tmp;

    if (len == 0) return(-99);
    tmp = (char *)malloc(len+1);
    strncpy(tmp,name,len);
    tmp[len]='\0';

    for (i = 0; i < var_file.nvars; i++){
	if(!strcasecmp(var_file.vars[i].name,tmp) || 
	   !strcasecmp(var_file.vars[i].button_name,tmp)){
	    free(tmp);
	    return(i);
	}
    }
    free(tmp);
    return(-99);
}

vidtoname_(vid, name, len)
     int *vid;
     char *name;
     int len;
{
    int i, test;
    char *tmp;

    if (len == 0){
      strcpy(name,"");
      return(-1);
    }
    if(*vid <1 || *vid > var_file.nvars){
      strcpy(name,"");
      return(-1);
    }
    strncpy(name, var_file.vars[*vid].name,len);
    return(strlen(var_file.vars[*vid].name));
}
 
void make_button_list_(address, len,l)
     char **address;
     int *len,l;
{
    int i;
    if (*len >= var_file.nvars) {
	for (i=0; i<var_file.nvars; i++) {
	    address[i] = var_file.vars[i].button_name;
	}
	address[var_file.nvars] = 0;
    }
    else {
	for (i=0; i<*len; i++)
	  address[i] = var_file.vars[i].button_name;
	address[*len] = 0;
	(void)make_help_widget_("Number of buttons exceeds array size. \n\
See local IVE expert.");
    }
}

void keep_var_(var_id)
     int *var_id;
{
    VAR_INFO.temporary = 0;
}

void garbage_collect_vars_()
{
    int i;

    for (i = var_file.nvars - 1; i >= 0; i--)
      if (var_file.vars[i].temporary){
	  free_var_(&i);
      }
}

int count_varptrs_(var)
/* counts the number of occurences of pointer 'var' in the data structure. */
float **var;
{
    int i, *var_id,count;
    var_id=&i;
    count=0;
    for (i= 0; i<var_file.nvars; i++) {
	if (VAR_INFO.values == *var){
	    count=count+1;
	}
    }
    return(count);
}


int isperm_(name,len)
/* returns 1 if the field 'name' is a permanent one, 0 otherwise */
char *name;
int  len;
{
    int varid;
    varid=getvid_(name,len);
    if (varid >= 0) {
      if (!var_file.vars[varid].temporary) {
        return(1); 
      }
      return(0);
    }
    return(0);
}



