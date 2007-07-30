/*
 * $Id: loop_setup.h,v 1.2 1996/10/09 18:54:22 harry Exp $
 * Header file for loop setup information.
 *
 * $Log: loop_setup.h,v $
 * Revision 1.2  1996/10/09 18:54:22  harry
 * Add the ability to loop on file names if time value is part of name,
 * and there is only one time per file.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:12  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.2  1992/12/23  18:57:22  harry
 * Add start value and start_widget to structure.
 *
 * Revision 1.1  1992/10/07  22:17:09  harry
 * Initial revision
 *
 *
 */

struct loop_setup {
    int coord;			/* Which coordinate to vary */
    float start,		/* Start value */
          increment,		/* Increment value */
          end_value;		/* End value */
    Widget button[4],		/* Loop coordinate buttons */
           start_widget,	/* Loop start widget */
	   start_t_widget,	/* Loop start widget for time tagged files */
           incr_widget,		/* Loop increment widget */
           endv_widget,		/* Loop end widget */
	   endv_t_widget;	/* Loop end widget for time tagged files */
} loop_setup;
