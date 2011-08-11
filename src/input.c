/*
 * $Id: input.c,v 1.5 1998/09/04 20:51:59 harry Exp $
 * This routine reads ive commands from a file and executes them.
 *
 * $Log: input.c,v $
 * Revision 1.5  1998/09/04 20:51:59  harry
 * Add colorbar button to annotation widget.  Rewrite file widgets to
 * remember previous entries.
 *
 * Revision 1.4  1997/12/31 23:31:17  warren
 * Fixed Dales color loxking problem, holton's crashes etc.
 *
 * Revision 1.3  1993/07/03 01:53:18  yuri
 * Tuning of overlay colors.
 *
 * Revision 1.2  1993/06/28  21:26:27  harry
 * Add "STEP" command and debug argument to "INPUT" command to allow
 * single steps of input files.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:03  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.6  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.5  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.4  1992/12/14  18:33:54  harry
 * Add code for widget interaction.
 *
 * Revision 1.3  1992/11/20  00:03:10  harry
 * User return_args to free argv.
 *
 * Revision 1.2  1992/11/18  18:11:19  harry
 * Changed to be directly called by driver.
 *
 */

static char ident[] = "$Id: input.c,v 1.5 1998/09/04 20:51:59 harry Exp $";

#include <unistd.h>
#include <strings.h>
#include <Xm/XmAll.h>
#include <stdio.h>
#include <ive_macros.h>
#include <file_widgets.h>
#include <malloc.h>

static int not_window=0; /*flag passed to driver; means do update windows*/ 
static int zero=0, one=1;

extern void driver_(),make_help_widget_(),expand_path_();

FILE *input_stream = NULL;


int
step_(flag)

int *flag;

{
    char buf[1024];

    if (input_stream) {
	if (fgets(buf, 1024, input_stream)) {
	    if (*flag) printf(buf);
	    if (buf[0] != '\n' && buf[0] != '#')
		(void) driver_(buf, &not_window, strlen(buf)-1);
	    return(1);
	}
	fclose(input_stream);
	input_stream = NULL;
	return(0);
    }
    (void) make_help_widget_("Cannot STEP - no open input file.");
    return(0);
}



int
input_(iflag)
  
     int *iflag;
     
{
  char buf[1024], buf2[1024], **argv, **get_args();
  int argc;
  
  argv = get_args(&argc);
  if (argc < 2 || argc > 3) {
    sprintf(buf, "Command %s requires 1 or 2 values", argv[0]);
    (void) make_help_widget_(buf);
    return_args(1);
  }
  if (argc == 3 && strcmp(argv[1], "0") == 0 && strcmp(argv[2], "0") == 0) {
    strcpy(buf, "~/.iverc");
    strcpy(buf2, buf);
    expand_path_(buf, 1024);
    if ((input_stream = fopen(buf, "r")) == NULL) {
      strcpy(buf, "~/.uwgaprc");
      strcpy(buf2, buf);
      expand_path_(buf, 1024);
      if ((input_stream = fopen(buf, "r")) == NULL) return_args(0);
    }
  }
  else {
    if(!strncmp(argv[1],"-",1)){
      input_stream = stdin;
    }
    else{
      strcpy(buf, argv[1]);
      strcpy(buf2, buf);
      expand_path_(buf, 1024);
      if ((input_stream = fopen(buf, "r")) == NULL) {
	sprintf(buf, "Command %s cannot access file %s",
		argv[0], argv[1]);
	(void) make_help_widget_(buf);
	return_args(1);
      }
    }
  }
  if (*iflag == 0)
    set_file_box(input_widget.List, buf2, &input_widget.fsb);
  if (argc == 3 && strcasecmp(argv[2], "debug") == 0) step_(&one);
  else while(step_(&zero));
  return_args(0);
}

