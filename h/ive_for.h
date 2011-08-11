/*
  These two are declared in the same file as main() for lack of anywhere else

  $Id: ive_for.h,v 1.7 2002/08/09 19:57:44 warren Exp $

  $Log: ive_for.h,v $
  Revision 1.7  2002/08/09 19:57:44  warren
  Fixed for Linux and tested with DEC sun etc.
  most items inside #ifdef's, or were general cleanup/fixes

  Revision 1.6  1999/12/22 00:32:25  warren
  Fixed Dales crashing bug when changing from point slicer to normal. Also
  have some ifdef linuxes in there in case we ever find a fortran compiler.

  Revision 1.5  1998/01/07 15:58:07  warren
  fix alpha to ignore underflows correctly

  Revision 1.4  1997/04/10 17:00:01  harry
  Do not nest setjmp calls when driver calls driver.

  Revision 1.3  1996/10/23 15:49:25  harry
  Remove floating point handler for SGI IRIX systems.


*/
#ifndef IVEMAIN
extern int from_window;/*flag passed to driver; means don't update windows*/ 
extern int not_window; /*flag passed to driver; means do update windows*/ 
#endif
/*
  driver definitions for c; driver is the main fortran driving subroutine
  note: changing these macros will effect every callback!!!!!
*/

#include <signal.h>
#include <setjmp.h>
#include <strings.h>
extern void control_c();
#ifdef IRIX
#define SIGNAL_FPE
#else
#ifdef linux
#define SIGNAL_FPE
#else
#ifdef __alpha
#define SIGNAL_FPE  signal(SIGFPE, SIG_IGN);
#else
extern void floating_point_handler();
#define SIGNAL_FPE signal(SIGFPE,floating_point_handler)
#endif
#endif
#endif
extern void quit_query();
extern jmp_buf env;
void driver_(char *__string, int *__update, long __l);
#define driver(s) {\
		     int savemask,ive_dstatus;\
		     void (*sig)();\
		     sig = signal(SIGINT,control_c);\
		     SIGNAL_FPE;\
		     if (sig == quit_query) ive_dstatus=setjmp(env);\
		     else ive_dstatus = 0;\
		     if(!ive_dstatus)\
		       driver_(s,&from_window,strlen(s));		\
		     if (sig == quit_query) signal(SIGINT,quit_query);\
		     signal(SIGFPE,SIG_DFL);\
		     }
#define driver_notwin(s) {\
		     int savemask,ive_dstatus;\
		     void (*sig)();\
		     sig = signal(SIGINT,control_c);\
		     SIGNAL_FPE;\
		     if (sig == quit_query) ive_dstatus=setjmp(env);\
		     else ive_dstatus = 0;\
		     if(!ive_dstatus)\
		       driver_(s,&not_window,strlen(s));		\
		     if (sig == quit_query) signal(SIGINT,quit_query);\
		     signal(SIGFPE,SIG_DFL);\
		   }
