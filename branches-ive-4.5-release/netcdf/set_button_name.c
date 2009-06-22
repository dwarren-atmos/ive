/*
 * $Id: set_button_name.c,v 1.3 1999/12/22 00:32:32 warren Exp $
 *
 * This routine sets the button name for a variable.
 *
 * ierr = set_button_name(variable, button_name)
 *
 * ierr is zero if the variable is not found
 *
 * $Log: set_button_name.c,v $
 * Revision 1.3  1999/12/22 00:32:32  warren
 * Fixed Dales crashing bug when changing from point slicer to normal. Also
 * have some ifdef linuxes in there in case we ever find a fortran compiler.
 *
 * Revision 1.2  1997/12/31 23:31:30  warren
 * Fixed Dales color loxking problem, holton's crashes etc.
 *
 * Revision 1.1  1993/08/18 15:46:51  harry
 * Change to new transform routines.
 *
 */

static char ident[] = "$Id: set_button_name.c,v 1.3 1999/12/22 00:32:32 warren Exp $";

#include "cdfinfo.h"
#include <malloc.h>
#include <string.h>
#include <strings.h>

cdf_info var_file;


int
set_button_name(variable, button_name)
char *variable, *button_name;

{
    int i;

    for (i=0; i < var_file.nvars; ++i) {
	if (strcasecmp(var_file.vars[i].name, variable) == 0) {
	    strcpy(var_file.vars[i].button_name, button_name);
	    return(0);
	}
    }
    return(1);
}

int
get_button_name(variable, button_name)
char *variable, *button_name;

{
    int i;

    for (i=0; i < var_file.nvars; ++i) {
	if (strcasecmp(var_file.vars[i].name, variable) == 0) {
	    if(strlen(var_file.vars[i].button_name) == 0)
		strcpy(button_name,var_file.vars[i].name);
	    else
		strcpy(button_name,var_file.vars[i].button_name);
	    return(0);
	}
    }
    strcpy(button_name,"unknown");
    return(1);
}


/*
 * Fortran jacket
 */

int
set_button_name_(variable, button_name, len1, len2)
char *variable, *button_name;
int len1, len2;

{
    int i;
    char *v, *b;

    for (i=len1-1; i >= 0 && variable[i] == ' '; i--);
    v = (char *) malloc(i+2);
    if (i+1) strncpy(v, variable, i+1);
    v[i+1] = '\0';
    for (i=len2-1; i >= 0 && button_name[i] == ' '; i--);
    b = (char *) malloc(i+2);
    if (i+1) strncpy(b, button_name, i+1);
    b[i+1] = '\0';
    i = set_button_name(v, b);
    free(v);
    free(b);
    return(i);
}

    
