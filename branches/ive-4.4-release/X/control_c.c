/****************************************************************************
 * $Id: control_c.c,v 1.9 1998/01/07 15:55:21 warren Exp $
 *
 * This contains the error handlers for Control-C and Floating Point
 * Exceptions.
 *
 * $Log: control_c.c,v $
 * Revision 1.9  1998/01/07 15:55:21  warren
 * fix alpha to not care about underflows correctly
 *
 * Revision 1.8  1997/04/10 17:27:30  harry
 * Change "Quit" to "Quit IVE?".
 *
 * Revision 1.7  1996/10/25 17:42:31  harry
 * Change "mips" to "ultrix" to differeniate DEC mips from SGI mips.
 *
 * Revision 1.6  1995/06/23 21:11:15  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.5  1994/05/24  21:12:15  harry
 * Upgrade comments in code.
 *
 * Revision 1.4  1994/05/18  17:53:07  harry
 * Clean up of comments, and move color callbacks into one file.
 *
 * Revision 1.3  1993/11/29  19:37:49  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
****************************************************************************/
static char ident[] = "$Id: control_c.c,v 1.9 1998/01/07 15:55:21 warren Exp $";

#include <xgks.h>
#include <sys/signal.h>
#include <setjmp.h>
#include <Xm/XmAll.h>

#include <ive_gks.h>
#include <ive_for.h>
#include <ive_macros.h>

#define app_con XtWidgetToApplicationContext(ive_widget)
extern Widget ive_widget;
  
jmp_buf env;	/* longjmp environment for interruped plots. */
/*
   "ask_quit" puts up the "Quit" widget.
   */
void ask_quit(data)
     XtPointer data;
{
    XmString NewString();
    XmString str;
    Widget w;
    extern void del_callback(), quit_ive();

    str = NewString("Quit IVE?");
    w = XmCreateQuestionDialog(ive_widget,"Quit IVE",NULL,0);
    XtVaSetValues(w, XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON,
		  XmNdialogType, XmDIALOG_QUESTION,
		  XmNmessageString, str, NULL);
    XtAddCallback(w,XmNokCallback,quit_ive,NULL);
    XtAddCallback(w,XmNcancelCallback,del_callback,NULL);
    XtUnmanageChild(XmMessageBoxGetChild(w,XmDIALOG_HELP_BUTTON));    
    XtManageChild(w);
    XmStringFree(str);
}
/*
   "quit query" is the signal handler for SIGINT set up in ive_main.c.
   It calls ask_quit via XtAppAddTimeOut to put up the "Quit" widget.
   */
void quit_query(){
    XtAppAddTimeOut(app_con,0,ask_quit,NULL);
    return;
}
/*
   "control_c" is the signal handler for SIGINT while a plot is
   in progress, set up by the macros for driver and driver_notwin in
   ive.h.  It ends the current plot and returns to driver.
   */
void control_c(){
    int curseg;
    int error;
    static int zero=0;
    static int val=1;
    Gos state;


    ginqopst(&state);
    if (state == GSGOP){
	getivar_("curseg",&curseg,&error,6);
	delseg_(&curseg);
	setlvar_("savflg",&zero,&error,6);
	setlvar_("pltflg",&zero,&error,6);
    }
    (void)melt_dpy_();
    (void)comment_log_(&zero);
    longjmp(env, val);
}
/*
   These are the floating point exception handers.
   */
#ifdef sparc
#include <floatingpoint.h>
#include <sys/ieeefp.h>
void floating_point_handler()
{
    int curseg;
    long error;
    static int zero=0 ;
    static int val=1;
    Gos state;
    char *out;

   ieee_flags("clearall", "exception", "all", &out);
    (void)make_help_widget("IEEE Floating Point Error occured \
aborting operation");

    ginqopst(&state);
    if (state == GSGOP){
	getivar_("curseg",&curseg,&error,6);
	delseg_(&curseg);
	setlvar_("savflg",&zero,&error,6);
	setlvar_("pltflg",&zero,&error,6);
    }
    (void)melt_dpy_();
    longjmp(env, val);
    
}
#endif
#if defined(ultrix) || defined(__alpha)
#ifdef ultrix
#include <mips/fpu.h>
#else
#include <machine/fpu.h>
#endif

void floating_point_handler(sig, code, scp)
     int sig, code;
     struct sigcontext *scp;
{
    int curseg;
    int error;
    static int zero=0 ;
    static int val=1;
    Gos state;
    unsigned long instr;
    unsigned long fp_control,fpcr;
    if(code != 0)return;
    
#ifdef ultrix
    scp->sc_fpc_csr = 0;
    scp->sc_fpc_eir = 0;
#else
/*    scp->sc_fp_control = 0;*/
    ieee_get_state_at_signal(&fp_control,&fpcr);
    if(fp_control&IEEE_STATUS_UNF){
	fp_control=0x000000;
	fpcr=0x000000;
	ieee_set_state_at_signal(fp_control,fpcr);
    ieee_set_fp_control(IEEE_TRAP_ENABLE_INV|IEEE_TRAP_ENABLE_DZE|
			IEEE_TRAP_ENABLE_OVF|IEEE_TRAP_ENABLE_UNF|
			IEEE_TRAP_ENABLE_INE|!IEEE_STATUS_INV|
			!IEEE_STATUS_DZE|!IEEE_STATUS_OVF|!IEEE_STATUS_UNF|
			!IEEE_STATUS_INE);
#if defined(__alpha)
	      sigreturn(scp);
#else
	return;
#endif
    }
    fp_control=0x000000;
    fpcr=0x000000;
    ieee_set_state_at_signal(fp_control,fpcr);
    ieee_set_fp_control(IEEE_TRAP_ENABLE_INV|IEEE_TRAP_ENABLE_DZE|
			IEEE_TRAP_ENABLE_OVF|IEEE_TRAP_ENABLE_UNF|
			IEEE_TRAP_ENABLE_INE|!IEEE_STATUS_INV|
			!IEEE_STATUS_DZE|!IEEE_STATUS_OVF|!IEEE_STATUS_UNF|
			!IEEE_STATUS_INE);
#endif
    (void)make_help_widget("IEEE Floating Point Error occured \
aborting operation");
    
    ginqopst(&state);
    if (state == GSGOP){
	getivar_("curseg",&curseg,&error,6);
	delseg_(&curseg);
	setlvar_("savflg",&zero,&error,6);
	setlvar_("pltflg",&zero,&error,6);
    }
    (void)melt_dpy_();
    longjmp(env, val);
    
}
#endif

#ifdef __alpha

fpe_setup(){
    ieee_set_fp_control(IEEE_TRAP_ENABLE_INV|IEEE_TRAP_ENABLE_DZE|
			IEEE_TRAP_ENABLE_OVF|IEEE_TRAP_ENABLE_UNF|
			IEEE_TRAP_ENABLE_INE);
}

#endif
