/**********************************************************************
 *                                                                    *
 *     do_system.c                                                    *
 *     make sure fork exec calls are correct                          *
 *                                                                    *
 *    input is fortran string to exec                                 *
 *    $Log: do_system.c,v $
 *    Revision 1.3  2006/05/01 22:08:22  davidive
 *    Changes for gcc 4.x from Orion Poplawski at Colorado Research Associates
 *    (Joan Alexander)
 *
 *    Revision 1.2  1998/12/17 23:40:56  warren
 *    Proper casting once again
 *
 * Revision 1.1  1995/01/19  15:32:41  warren
 * First set of fixes found by the alpha. Mostly making sure that FORTRAN
 * variables yuri declares and then checks are set to something in between.
 *                                                           *
 **********************************************************************/
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static char *id="$id$";


int do_system_(str, len)
     char * str;
     int len;
{
    if(str[len-1] != '\0'){
	char *dummy;
	if((dummy = (char *)malloc(len+1)) == (char *)NULL){
	    (void)make_help_widget_("can't allocate memory in do_system");
	    return(-1);
	}
	
	strncpy(dummy,str,len);
	dummy[len]='\0';
	system(dummy);
	free(dummy);
    }
    else{
	system(str);
    }
    return(1);
}
