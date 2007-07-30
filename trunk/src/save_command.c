/*
 * $Id: save_command.c,v 1.4 2001/01/05 19:34:56 davidive Exp $
 * This Fortran callable routine saves a command in the "cur_commands" list.
 *
 * call save_command(keynum, command)
 *
 *  where "keynum" is the number of the command, "command" is the text
 *  string of the command.
 *
 * $Log: save_command.c,v $
 * Revision 1.4  2001/01/05 19:34:56  davidive
 * fixed typo of " to ' to make index happy.
 *
 * Revision 1.3  1998/12/29 16:40:24  harry
 * Properly loop plots with "draw_line" command.
 *
 * Revision 1.2  1997/06/21 00:15:03  harry
 * Fixes to looping for switches between contour values and contour intervals.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:16  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.4  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.3  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.2  1993/01/05  22:34:55  harry
 * Add Id and Log info.
 *
 */

static char ident[] = "$Id: save_command.c,v 1.4 2001/01/05 19:34:56 davidive Exp $";

#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <stdio.h>
#include <strings.h>
#include "state.h"

char *cur_commands[NVCMND];

save_command_(keynum, command, length)

int *keynum;
char *command;
int length;

{
    if (command[0] == '\0') {
	if (cur_commands[*keynum-1]) {
	    free(cur_commands[*keynum-1]);
	    cur_commands[*keynum-1] = NULL;
	}
    }
    else {
      if (*keynum-1 == P_FIELD) {
	char *stp;
	int len;
	if((stp=index(command,',')) != NULL){
	  stp++;
	  if(stp<(command+length) && *stp != '\0'){
	    len = length - (stp - command);
	    strcpy(command, "FIELD=");
	    strncat(command,stp,len);
	    length=6+len;
	    command[length+1]='\0';
	  }
	}
      }
      if (cur_commands[*keynum-1])
	cur_commands[*keynum-1]
	  = (char *) realloc(cur_commands[*keynum-1], length+1);
      else
	cur_commands[*keynum-1] = (char *) malloc(length+1);
      strncpy(cur_commands[*keynum-1], command, length);
      cur_commands[*keynum-1][length] = '\0';
      if (*keynum-1 == P_CONTOUR_INTERVAL) {
	if (cur_commands[P_CONTOUR_VALUES])
	  free(cur_commands[P_CONTOUR_VALUES]);
	cur_commands[P_CONTOUR_VALUES] = NULL;
      }
      else if (*keynum-1 == P_CONTOUR_VALUES) {
	if (cur_commands[P_CONTOUR_INTERVAL])
	  free(cur_commands[P_CONTOUR_INTERVAL]);
	cur_commands[P_CONTOUR_INTERVAL] = NULL;
      }
    }
}

