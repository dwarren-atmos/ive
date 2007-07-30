/* $Id: ive_macros.h,v 1.1.1.1 1993/04/07 19:41:12 harry Exp $
 * Purpose: This file contains macros that can be used by all the C code.
 * $Log: ive_macros.h,v $
 * Revision 1.1.1.1  1993/04/07 19:41:12  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.3  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.2  1992/11/19  22:34:12  harry
 * Add return_args.
 *
 * Revision 1.1  1992/11/17  17:21:29  warren
 * Initial revision
 *
 */
#ifdef MEMDBG
#include <mnemosyne.h>
#endif

#include <sys/param.h>

#define make_help_widget(s) make_help_widget_(s, strlen(s))
#define make_info_widget(s) make_help_widget_(s, strlen(s))
#define return_args(i) {while(argc--) free(argv[argc]); free(argv); return(i);}
