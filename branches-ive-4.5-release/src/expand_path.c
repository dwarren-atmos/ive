/****************************************************************************
 *$Log: expand_path.c,v $
 *Revision 1.5  1998/12/17 23:17:18  warren
 *Cleanup of pointer casts to long instead of int, freeing XmStrings when no
 *longer in use, passing all args to sprinf's and making correctly casting NULLs
 *
 * Revision 1.4  1995/02/07  18:27:53  warren
 * update for new dec c compiler
 *
 * Revision 1.3  1994/10/31  22:10:23  warren
 * expand path now puts up an error if the user does not exist
 *
 * Revision 1.2  1994/06/20  16:33:38  harry
 * Change index to strchr.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:35  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.1  1992/08/24  20:39:42  millerp
 * Add function is_there to replace fortran inquire.
 *
 * Revision 2.0  1992/07/01  21:28:10  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.3  1992/06/17  17:46:26  warren
 * took out printf's
 *
 * Revision 1.2  1992/06/17  17:44:08  warren
 * added missing (char *)0 to buff after getting name.
 *
 * Revision 1.1  1992/06/11  22:43:18  warren
 * Initial revision
 *
 * This routin expands ~name/ into a usable file name
 *
 * str is used as both input and output 
 ****************************************************************************/
static char ident[] = "$Id: expand_path.c,v 1.5 1998/12/17 23:17:18 warren Exp $";

#ifdef __osf__
#include <sys/syslimits.h>
#endif
#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#include <pwd.h>
#include <sys/param.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <malloc.h>
expand_path_(str, len)
     char *str;
     int len;
{
    char *where;
    char *buff = (char *)NULL, *buff2, *tmpbuf;
    struct passwd *pptr;
    void make_help_widget_(char *);
    if(str[0] != '~')
      return(1);
    
    if((buff = (char *)malloc(MAXPATHLEN))==(char *)NULL){
	(void)make_help_widget_("memory allocation problem in expand_path");
	return(0);
    }
    if(str[1] == '/'){	
	where = str + 1;
	pptr = (struct passwd *)getpwuid(getuid());
    }
    else{
	where = (char *)strchr(str,'/');
	strncpy(buff,str+1, (long)where - ((long)str +1));
	buff[(long)where - ((long)str +1)] = '\0';
	pptr = (struct passwd *)getpwnam(buff);
    }

    if(pptr == NULL){
	char msg[80];
	sprintf(msg,"unknown user: %s",buff);
	free(buff);
	(void)make_help_widget_(msg);
	return(0);
    }
    
    (long)where++;
    sprintf(buff,"%s/%s",pptr->pw_dir,where);
    strncpy(str,buff,len);
    free(buff);
    return(1);
}

    
int is_there_(file)
     char *file;
{
    struct stat buf;

    return (stat(file, &buf) + 1);
}
