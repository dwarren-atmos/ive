/*
 * $Id: add_filelist.c,v 1.5 2006/08/22 19:17:06 warren Exp $
 * $Log: add_filelist.c,v $
 * Revision 1.5  2006/08/22 19:17:06  warren
 * Allocate enough memory to no overwrite buffer.
 *
 * Revision 1.4  2001/08/15 22:23:19  davidive
 * Cleaned up code to reduce warnings about type mismatches, and added buttons
 * and callbacks to the 1d form widget to select which access to label against
 * for diagonal plots.
 *
 * Revision 1.3  2001/01/30 18:56:17  harryive
 * Correct the size of memory requested in malloc.
 *
 * Revision 1.2  2000/11/30 18:43:45  harry
 * Fix bug in checking for duplicate names in list.
 * Change format of list in menu.
 *
 * Revision 1.1  2000/11/28 23:35:08  harry
 * Add list of recent files to Input pulldown menu.
 *
 */

#include <strings.h>
#include <Xm/XmAll.h>
#include <ive_for.h>
#include <ive_widgets.h>
#include <malloc.h>
XmString NewString();

static char ident[] = "$Id: add_filelist.c,v 1.5 2006/08/22 19:17:06 warren Exp $";

static char *names[10];
static Widget filelist[10];
static Widget separator = NULL;

void filelist_call(Widget w, char *data, XmAnyCallbackStruct *call)
{
    char *str = (char *)malloc(7+strlen(data));

    sprintf(str, "file=%s", data);
    driver(str);
    free(str);
}

void add_filelist_(char *filename)
{
    int i, j;

    for (i=0; i < 10 && names[i] != NULL; i++) {
	if (strcmp(names[i], filename) == 0) {
	    char *p;

	    p = names[i];
	    for (; i > 0;  i--)
		names[i] = names[i-1];
	    names[0] = p;
	    break;
	}
    }
    if (i == 10 || names[i] == NULL) {
	if (names[9] != NULL) free(names[9]);
	for (i=9; i > 0; i--) names[i] = names[i-1];
	names[0] = (char *) malloc(strlen(filename)+1);
	strcpy(names[0], filename);
    }
    if (separator == NULL) {
	separator = XtVaCreateManagedWidget("Separator",
					    xmSeparatorWidgetClass,
					    main_widget.input_menu,
					    XmNorientation, XmHORIZONTAL,
					    XmNseparatorType, XmSINGLE_LINE,
					    NULL);
	for (i=0; i < 10; i++) filelist[i] = NULL;
    }
    else {
	for (i=0; i < 10 && filelist[i]; i++) {
	    XtDestroyWidget(filelist[i]);
	    filelist[i] = NULL;
	}
    }
    for (i=0; i < 10 && names[i] != NULL; i++) {
	XmString str;
	char *p = (char *)malloc(strlen(names[i])+4), *q;

	if ((q=rindex(names[i], '/'))) {
	    *q = '\0';
	    sprintf(p, "%s (%s)", q+1, names[i]);
	    *q = '/';
	    str = NewString(p);
	    free(p);
	}
	else str = NewString(names[i]);
	filelist[i] = XtVaCreateManagedWidget(names[i],
					      xmPushButtonWidgetClass,
					      main_widget.input_menu,
					      XmNlabelString, str,
					      NULL);
	XmStringFree(str);
	XtAddCallback(filelist[i], XmNactivateCallback, (XtCallbackProc)filelist_call,
		      names[i]);
    }
    return;
}
