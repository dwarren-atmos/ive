/*
 * $Id: get_args.c,v 1.3 1994/10/08 19:01:27 warren Exp $
 * This routine gets the command arguments from the Fortran common block.
 *
 * $Log: get_args.c,v $
 * Revision 1.3  1994/10/08 19:01:27  warren
 * The annotation widget and updating it. As well as changing tic_bold to
 * tic_labels and adding an off option.
 *
 * Revision 1.2  1993/11/29  19:39:18  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.1.1.1  1993/04/07  19:39:56  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.4  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.3  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.2  1992/11/20  00:03:43  harry
 * Make sure last entry in argv is NULL.
 *
 * Revision 1.1  1992/11/18  18:12:19  harry
 * Initial revision
 *
 */

static char ident[] = "$Id: get_args.c,v 1.3 1994/10/08 19:01:27 warren Exp $";

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <malloc.h>
extern void get_prscmd_();

char **
get_args(argc)

int *argc;

{
    char **argv,	  /* Array of command parameters */
         *c;		  /* Scratch */
    static char *commands; /* Address of Fortran common block */
    int  i,		  /* Scratch */
         beg,		  /* First non-blank character */
         end;		  /* Last non-blank character */
    static int length=0,  /* Number of characters per entry in commands */
    	 number=0;	  /* Number of entries in commands */
	int skip = 0;     /* blank line marker */

    if(length == 0 && number == 0)
       get_prscmd_(&commands, &length, &number);
    argv = (char **) malloc(sizeof(char *) * (number+1));
    *argc = 0;
    for (i=0; i < number; ++i) {
	c = commands + i*length;
	for (beg=0; beg<length && c[beg]==' '; beg++);
	if (beg == length){
	    skip = 1;
	    continue;
	}
	if(skip){
	    argv[(*argc)++] = NULL;
	    skip = 0;
	}
	for (end=length-1; c[end] == ' '; end--);
 	argv[*argc] = (char *) malloc(sizeof(char) * (end-beg+2));
	strncpy(argv[*argc], c+beg, end-beg+1);
	argv[(*argc)++][end-beg+1] = '\0';
    }
    argv[*argc] = NULL;
    return argv;
}
