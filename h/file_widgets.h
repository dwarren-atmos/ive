/*
 * $Id: file_widgets.h,v 1.4 1998/09/04 20:51:52 harry Exp $
 * Header file for file widgets information.
 *
 * $Log: file_widgets.h,v $
 * Revision 1.4  1998/09/04 20:51:52  harry
 * Add colorbar button to annotation widget.  Rewrite file widgets to
 * remember previous entries.
 *
 * Revision 1.3  1997/03/24 14:45:01  harry
 * Add widgets to file selection window for multiple files.
 *
 * Revision 1.2  1993/07/30  22:47:55  warren
 * add stuff for eps widget.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:11  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.3  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.2  1992/12/23  18:54:23  harry
 * Add log_widget.
 *
 * Revision 1.1  1992/12/14  18:38:02  harry
 * Initial revision
 *
 */
#ifdef MEMDBG
#include <mnemosyne.h>
#endif

struct fsb {
    XmString directory, pattern, dirSpec;
};

struct input_widget {
    Widget List;		/* Input widget */
    struct fsb fsb;
} input_widget;

struct dump_widget {
    char *type;			/* Dump type (XWD or GIF) */
    char *what;			/* What command (DUMP or DUMP_LOOP) */
    Widget List, Type, What;	/* Dump main, type and what widgets */
    struct fsb fsb;
} dump_widget;

struct log_widget {
    Widget List;		/* Log widget */
    struct fsb fsb;
} log_widget;

struct eps_widget {
    char color[4];
    Widget List; 		/* Input widget */
    Widget color_w;
    struct fsb fsb;
} eps_widget;

struct dir_widget {
    int start_val, end_val, coord;
    Widget List, dir_form,
	button[5],
	start, startlab,
	end, endlab;
    struct fsb fsb;
} dir_widget;

struct trans_widget{
    Widget List;
    struct fsb fsb;
} trans_widget;

struct ctb_widget{
    Widget List;
    struct fsb fsb;
} ctb_widget;

struct wctb_widget{
    Widget List;
    struct fsb fsb;
} wctb_widget;

void set_file_box(Widget w, char *s, struct fsb *fsb);
