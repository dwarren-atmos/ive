/*
 * $Id: dump.c,v 1.7 2000/12/07 19:50:14 harry Exp $
 * This routine dumps the xgks window or the current loop to a file
 * in XWD or GIF format.
 *
 * Command syntax: [LOOP_]DUMP=[file][,type]
 *
 *  file = File to dump, if null - use same name as last time.
 *         If filename contains '##', those characters are replaced with the
 *         two digit frame number (incremented for each time DUMP is 
 *         called with the same file name.
 *  type = xwd or gif
 *
 * $Log: dump.c,v $
 * Revision 1.7  2000/12/07 19:50:14  harry
 * Allow dumping of loops of 100 images or more.
 *
 * Revision 1.6  1998/12/17 23:16:57  warren
 * Cleanup of pointer casts to long instead of int, freeing XmStrings when no
 * longer in use, passing all args to sprinf's and making correctly casting NULLs
 *
 * Revision 1.5  1998/09/04 20:51:39  harry
 * Add colorbar button to annotation widget.  Rewrite file widgets to
 * remember previous entries.
 *
 * Revision 1.4  1996/01/10 17:08:53  harry
 * Fix dumping routines to use proper colormap.
 *
 * Revision 1.3  1994/09/13  17:42:10  harry
 * Add #ifndefs around MAX and MIN.
 *
 * Revision 1.2  1994/05/26  17:32:10  warren
 * change to includes and defines to work with SOL 2
 *
 * Revision 1.1.1.1  1993/04/07  19:41:22  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.6  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.5  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.4  1992/12/14  18:28:18  harry
 * Add code for widget interaction.
 *
 * Revision 1.3  1992/12/04  20:00:28  harry
 * Change to two digit numbers in file names.
 *
 * Revision 1.2  1992/12/02  00:25:31  harry
 * Add LOOP_DUMP command.
 *
 * Revision 1.1  1992/11/20  00:04:12  harry
 * Initial revision
 *
 */

static char ident[] = "$Id: dump.c,v 1.7 2000/12/07 19:50:14 harry Exp $";

#include <strings.h>
#include <stdio.h>
#include <Xm/XmAll.h>
#include <ive_macros.h>
#include <ive.h>
#include <file_widgets.h>
#include <cmndlst.h>
#include <loop.h>
#include <malloc.h>
#ifndef MAX
#define MAX(x, y) (x) > (y)? (x):(y) 
#endif
#ifndef MIN
#define MIN(x, y) (x) < (y)? (x):(y) 
#endif

extern Widget xgks_widget, Toplevel;
extern void expand_path_();
extern int Window_Dump();

static char lastfile[256] = {'\0'};
static int  file_number;

int
dump_(key, iflag)

int *key, *iflag;

{
    char *file, num[4], buf[1024], *num_loc, *p, **argv, **get_args();
    FILE *out;
    int argc, end, i, keynum = *key-1;
    XmString str;
    Colormap cmap;

    argv = get_args(&argc);
    if (argc > 3) {
	sprintf(buf, "Command %s requires 0, 1 or 2 values", argv[0]);
	(void) make_help_widget_(buf);
	return_args(1);
    }
    if (argc == 1) {
	if (lastfile[0] == 0) {
	    sprintf(buf, "Command %s - must specify a file in initial call",
		argv[0]);
	    (void) make_help_widget_(buf);
	    return_args(1);
	}
	file = lastfile;
    }
    else {
	file = argv[1];
	if (argc == 3) {
	    if (strncasecmp(argv[2], "xwd", MIN(3, strlen(argv[2])))==0)
		dump_widget.type = "XWD";
	    else if (strncasecmp(argv[2], "gif", MIN(3, strlen(argv[2])))==0)
		dump_widget.type = "GIF";
	    else if (strncasecmp(argv[2], "png", MIN(3, strlen(argv[2])))==0)
		dump_widget.type = "PNG";
	    else {
		sprintf(buf, "Command %s - invalid dump type %s", 
			argv[0], argv[2]);
		(void) make_help_widget_(buf);
		return_args(1);
	    }
	}
    }
    if ((num_loc=strstr(file, "##")) == NULL) {
	if (keynum == P_LOOP_DUMP) {
	    sprintf(buf, "Command %s - require '##' in name of file %s",
		    argv[0], file);
	    (void) make_help_widget_(buf);
	    return_args(1);
	}
	if (strcmp(lastfile, file) == 0) {
	    sprintf(buf, 
		    "Command %s - File name %s does not have a '##',\nwill write over previous file",
		    argv[0], file);
	    (void) make_help_widget_(buf);
	}
    }
    else num_loc = buf + (num_loc-file);
    if (strcmp(lastfile, file) != 0) {
	strncpy(lastfile, file, 256);
	file_number = 0;
    
    }
    if (keynum == P_LOOP_DUMP) {
	if (loop_form == NULL) {
	    sprintf(buf, "Cannot execute command %s without an active loop",
		    argv[0]);
	    (void) make_help_widget_(buf);
	    return_args(1);
	}
	dump_widget.what = "loop";
	end = num_pixmaps;
    }
    else {
	end = 1;
	dump_widget.what = "plot";
    }
    if (*iflag == 0 && dump_widget.List) {
	str = XmStringCreate(dump_widget.what, XmSTRING_DEFAULT_CHARSET);
	XtVaSetValues(dump_widget.What, XmNlabelString, str, NULL);
	XmStringFree(str);
	str = XmStringCreate(dump_widget.type, XmSTRING_DEFAULT_CHARSET);
	XtVaSetValues(dump_widget.Type, XmNlabelString, str, NULL);
	XmStringFree(str);
    }	
    for (i=0; i < end; ++i) {
	if (end > 99 && num_loc) {
	    strncpy(buf, file, num_loc-buf);
	    strcpy(num_loc+3, file+(num_loc-buf)+2);
	    sprintf(num, "%03.3d", ++file_number);
	    strncpy(num_loc, num, 3);
	}
	else {
	    strcpy(buf, file);
	    if (num_loc != NULL) {
		sprintf(num, "%02.2d", ++file_number);
		strncpy(num_loc, num, 2);
	    }
	}
	expand_path_(buf, 1024);
	if ((out=fopen(buf, "w")) == NULL) {
	    sprintf(buf, "Command %s cannot write to file %s",
		    argv[0], file);
	    (void) make_help_widget_(buf);
	    return_args(1);
	}
	if (*iflag == 0 && i == 0)
	    set_file_box(dump_widget.List, file, &dump_widget.fsb);
	XtVaGetValues(xgks_widget, XmNcolormap, &cmap, NULL);
	if (Window_Dump(XtDisplay(xgks_widget), XtWindow(xgks_widget),
			keynum==P_LOOP_DUMP?pixmap[i]:0,
			0, dump_widget.type, out, buf, cmap)) {
	    make_help_widget_(buf);
	    fclose(out);
	    return_args(1);
	}
	fclose(out);
    }
    return_args(0);
}

    
	


