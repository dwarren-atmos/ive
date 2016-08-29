/****************************************************************************
 *$Log: ive_main.c,v $
 *Revision 1.34  2002/12/13 23:35:35  harryive
 *Change gets to fgets to make Linux happy.
 *
 *Revision 1.33  2002/11/27 00:30:52  warren
 *24 bit color enhancements are here. Works on Linux and Sun so far - dec to
 *be tested still.
 *
 *Revision 1.32  2002/05/30 16:42:43  warren
 *add a "grayline" command to use when you will be printing in black and white,
 *and added an FPE handler when initializing windows to deal with an FPE out
 *of X.
 *
 *Revision 1.31  2001/10/15 21:53:34  davidive
 *More X cleanup so that True color at least doesn't crash us.
 *
 *Revision 1.30  2001/09/25 17:16:53  davidive
 *Cleaned up X code for opening X, creating a colormap, and making suer we have
 *a pseudocolor visual.
 *
 *Revision 1.29  2001/08/27 16:27:25  davidive
 *Changed call to open X to the newer supported call.
 *
 *Revision 1.28  2001/03/14 20:21:11  warren
 *corrected severa longs to unsigned longs, changed creation of lock button to
 *not set the name, the set the label seperately, change XtFree to XmStringFree
 *where it should be.
 *
 *Revision 1.27  2001/01/23 22:39:10  davidive
 *Start up connection to Open GL Server if available, and change number of
 *colors copied from the default colortable to 10 from 119 (no need).
 *
 *Revision 1.26  1997/03/24 14:44:54  harry
 *Add widgets to file selection window for multiple files.
 *
 *Revision 1.25  1997/01/28 20:34:31  harry
 *Add calls for DEC alpha to set memory, data and stack limits to the maximum.
 *
 *Revision 1.24  1996/12/17 15:36:16  warren
 *look for beta transforms first if compiled with -DBETA and up rev.
 *
 *Revision 1.23  1996/10/16 23:07:45  harry
 *Put IVETRANSFILE_BETA back in.
 *
 *Revision 1.22  1996/08/08 19:06:58  harry
 *Take out Beta transforms env. variable - this is no longer a beta
 *version.
 *
 *Revision 1.21  1996/04/08 18:55:44  warren
 *add -trans and -transform_file to input options.
 *
 *Revision 1.20  1996/02/15 23:17:03  harry
 *Correct check for number of colors.
 *
 *Revision 1.19  1996/02/15 22:56:01  harry
 *Change type of masks and pixels for XAllocColorCells from int to long.
 *
 * Revision 1.18  1996/02/13  20:27:44  warren
 * Always allocate colors when told
 *
 *Revision 1.17  1996/01/12 16:10:58  warren
 *Transfer of new pattern stuff from 3.x and beginning of color fixes
 *(color vectors, correct collors on the pulldowan menus)
 *
 * Revision 1.16  1995/12/18  19:01:48  harry
 * Increase size of pixels array.
 *
 *Revision 1.15  1995/12/15 22:32:38  warren
 *moved patterns into 4.0 and implemented the list of fixes and changes from
 *Curtis.
 * Revision 1.14  1995/09/15  16:46:30  harry
 * Change the default for allocating a new colormap from no to yes.
 *
 * Revision 1.13  1995/09/05  20:49:22  harry
 * Allow the transform file to be specified by IVETRANSFILE_BETA for beta
 * testing.  BE SURE TO REMOVE THIS IN THE FINAL RELEASE!
 *
 * Revision 1.12  1995/08/22  21:45:17  warren
 * make sure we open a log file
 * null outall pointers at the start
 *
 * Revision 1.11  1995/06/23  21:11:20  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.10  1994/06/23  21:35:12  harry
 * Add call to update_all_ so credits do not disappear with the initial
 * Expose event.
 *
 * Revision 1.9  1994/06/20  16:35:37  harry
 * Change index to strchr, and rindex to strrchr.
 *
 * Revision 1.8  1994/03/28  17:13:26  harry
 * Add call to cpinrc_ to make sure CONPACK is initialized when cpnumb is
 * called.  Add call to update_all_ at start of INPUT command line
 * processing so credits are displayed.
 *
 * Revision 1.7  1993/12/22  00:15:17  harry
 * Moved utInit call here from convert_units.
 *
 * Revision 1.6  1993/11/29  19:37:58  warren
 * Changes to 3.1 to make it work on alphas.
 * FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
 * C: Change int to long where necessary.
 * Files: Quite a few .f files are now .F
 *
 * Revision 1.5  1993/11/03  23:33:08  warren
 * General cleanup for beginning of port to alpha.
 *
 * Revision 1.4  1993/06/28  21:25:43  harry
 * Add -debug option to command line.
 *
 * Revision 1.3  1993/06/24  18:45:14  warren
 * Sped up ive start up by fixing loop to remove SHELL environment variable.
 *
 * Revision 1.2  1993/06/22  19:34:38  warren
 * rearanged background and plottype on main widget and fixed associated calls
 * appropriately. Also fixed main to call driver correctly.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:16  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.12  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 2.11  1993/02/03  22:45:07  warren
 * added call to init_getmem
 *
 * Revision 2.10  1992/12/23  18:49:44  harry
 * Add logfile processing to command line and apps default.
 *
 * Revision 2.9  1992/12/03  20:03:24  warren
 * added control_c handler for when you are not in driver
 *
 * Revision 2.8  1992/11/17  17:21:29  warren
 * added ive_macros to includes
 *
 * Revision 2.7  1992/11/16  22:38:11  harry
 * Moved input file processing to input_file and input.
 *
 * Revision 2.6  1992/09/30  16:05:22  harry
 * Add command-line option processing and input file processing.
 *
 * Revision 2.5  1992/09/29  18:45:47  harry
 * Make Toplevel a global variable.
 *
 * Revision 2.4  1992/09/24  23:08:22  harry
 * Remove irritating new line from color map question.
 *
 * Revision 2.3  1992/09/23  23:08:59  harry
 * Change call to add_credits.
 *
 * Revision 2.2  1992/09/11  01:43:35  warren
 * fixed to alloc own colormap if needed
 *
 * Revision 2.1  1992/07/20  19:45:45  warren
 *  did away with warp.
 *
 * Revision 2.0  1992/07/01  21:11:13  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.11  1992/07/01  21:10:11  millerp
 * *** empty log message ***
 *
 * Revision 1.10  1992/06/25  21:32:07  warren
 * fixed fix
 *
 * Revision 1.9  1992/06/18  23:09:15  warren
 * finished the # stuff
 *
 * Revision 1.8  1992/06/18  20:39:36  warren
 * add support for # comments in rc file
 *
 * Revision 1.7  1992/06/11  22:54:21  warren
 * fixed fix.
 *
 * Revision 1.6  1992/06/11  22:42:15  warren
 * allow for ive in name instead of uwgap
 *
 * Revision 1.5  1992/06/05  17:52:07  warren
 * see last message.
 *
 * Revision 1.4  1992/06/05  17:26:16  warren
 * fixed to update widgets if .uwgaprc exists.
 *
 * Revision 1.3  1992/06/05  17:12:42  warren
 * added support for .uwgaprc file
 *
 * Revision 1.2  1992/06/03  19:38:46  harry
 * Add call to add_credits
 *
 * Revision 1.1  1992/04/21  00:08:28  millerp
 * Initial revision
 *
 * Revision 1.103  1992/04/02  20:14:34  warren
 * added cass to init_cursors.
 *
 * Revision 1.102  1992/03/31  19:54:40  warren
 * add XWarp Pointer to startup.
 *
 * Revision 1.101  1992/03/30  17:57:24  warren
 * fixed to link with fortran on suns.
 *
 * Revision 1.100  1992/03/26  22:30:16  warren
 * *** empty log message ***
 *
 ****************************************************************************/
static char ident[] = "@(#)$Id: ive_main.c,v 1.34 2002/12/13 23:35:35 harryive Exp $";

#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <sys/file.h>
#include <udunits.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <malloc.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#ifdef MEMDBG
#include <mcheck.h>
#endif
#define IVEMAIN
#include <ive_macros.h>
#include <ive_for.h>
#include <ive_gks.h>

extern void cpinrc_(),setavar_(),setdef_(),add_credits(),init_cursors(),
  make_help_widget_(),comment_log_(),getavar_();
extern int count_colors();
/*
  Command line options.
  */
static XrmOptionDescRec options[] = {
    {"-file",		"*inputFile",		XrmoptionSepArg, NULL},
    {"-input_file",	"*inputFile",		XrmoptionSepArg, NULL},
    {"-acm",		"*allocateColorMap",	XrmoptionSepArg, NULL},
    {"-allocate_cmap",	"*allocateColorMap",	XrmoptionSepArg, NULL},
    {"-log_file",	"*logFile",		XrmoptionSepArg, NULL},
    {"-log",		"*logFile",		XrmoptionSepArg, NULL},
    {"-trans",		"*transformFile",	XrmoptionSepArg, NULL},
    {"-transform_file",	"*transformFile",	XrmoptionSepArg, NULL},
    {"-debug",		"*Debug",		XrmoptionNoArg,  "True"},
    {"-v",		"*verbose",		XrmoptionNoArg,  "True"},
    {"-stereo",		"*Stereo",		XrmoptionNoArg,  "True"},
};

typedef struct {
  char *input_file;
  char *allocate_cmap;
  char *log_file;
  char *transform_file;
  Boolean debug;
  Boolean verbose;
  Boolean stereo;
} AppData , *AppDataPtr;

AppData app_data;
Boolean Ive_Stereo = False;
	
static XtResource resources[] = {
{   "inputFile", "InputFile", XtRString, sizeof(char*),
    XtOffset(AppDataPtr, input_file), XtRString, NULL,
},
{   "allocateColorMap", "AllocateColorMap", XtRString, sizeof(char*),
    XtOffset(AppDataPtr, allocate_cmap), XtRString, NULL,
},
{   "logFile", "LogFile", XtRString, sizeof(char*),
    XtOffset(AppDataPtr, log_file), XtRString, NULL,
},
{   "debug", "Debug", XtRBoolean, sizeof(Boolean),
    XtOffset(AppDataPtr, debug), XtRImmediate, (XtPointer) False,
},
{   "verbose", "Verbose", XtRBoolean, sizeof(Boolean),
    XtOffset(AppDataPtr, verbose), XtRImmediate, (XtPointer) False,
},
{   "stereo", "Stereo", XtRBoolean, sizeof(Boolean),
    XtOffset(AppDataPtr, stereo), XtRImmediate, (XtPointer) False,
},
{   "transformFile", "InputFile", XtRString, sizeof(char*),
    XtOffset(AppDataPtr, transform_file), XtRString, NULL,
},
};
#ifdef MEMDBG
void ive_mem_handler(int s) {
  muntrace();
  abort();
}
#endif
/*
   these are used in all callback routines except for those that only effect
   the interface
*/
int from_window=1;/*flag passed to driver; means don't update windows*/ 
int not_window=0; /*flag passed to driver; means do update windows*/ 

Widget Toplevel;
Colormap cmap; 
static Pixel colors[6]={(unsigned long)((0x00 <<16)+(0x00 <<8)+(0x00)),
			 (unsigned long)((0xff <<16)+(0x00 <<8)+(0x00)),
			 (unsigned long)((0x00 <<16)+(0xff <<8)+(0x00)),
			 (unsigned long)((0x00 <<16)+(0x00 <<8)+(0xff)),
			(unsigned long)((0xff <<16)+(0xff <<8)+(0xff))};
Widget file_widget, ive_widget, xgks_widget;
void first_loop(app_con)
     XtAppContext app_con;
     
{
  XtInputMask mask;
  Display *dpy;

  dpy=XtDisplay(Toplevel);
  while(mask = XtAppPending(app_con)) {
	XtAppProcessEvent(app_con, mask);
	XSync(dpy,FALSE);
	XSync(dpy,FALSE);
	XSync(dpy,FALSE);
  }
}

int proc_find(const char* name) 
{
    DIR* dir;
    struct dirent* ent;
    char* endptr;
    char buf[512];
    char link[2048],*l;
    int count=0;
    
    if (!(dir = opendir("/proc"))) {
        perror("can't open /proc");
        return -1;
    }

    while((ent = readdir(dir)) != NULL) {
        /* if endptr is not a null character, the directory is not
         * entirely numeric, so ignore it */
        long lpid = strtol(ent->d_name, &endptr, 10);
        if (lpid<0)
	  continue;

        /* try to open the cmdline file */
        snprintf(buf, sizeof(buf), "/proc/%ld/exe", lpid);
	bzero(link,2048);
	readlink(buf,link,2048);
	l=rindex(link,'/');
	if(l){
	  l++;
	  if (!strncmp(l, name,strlen(name))) {
	    count++;
	  }
	}
    }
    return(count%5);
}


main(argc,argv,envp)
int argc;
char **argv, **envp;
{
#define STARTUP_FILE ".iverc"
    Widget init_ive(), init_xgks();
    Arg args[8];
    XtAppContext app_con;
    char *appname, *curdir, *progname, *transenv, *ptr;
    unsigned long att_mask = CWBackingStore|CWSaveUnder;
    XSetWindowAttributes attributes;
    char buff[256],*tmpbuf, **tmpbuf2;
    FILE *stream;
    long error ;
    int i, status, ivecount, me;
    static int one=1, negone=-1;
    extern void quit_query();
    struct rlimit rlp;

    /*
     * Get application name.
     */
#ifdef MEMDBG
    signal(SIGSEGV, ive_mem_handler);
#endif
#ifdef sparc
    (void)f_init();
#endif
#ifdef __alpha
    for_rtl_init_ (&argc, argv);
    fpe_setup();
    i = getrlimit(RLIMIT_AS, &rlp);
    rlp.rlim_cur = rlp.rlim_max;
    i = setrlimit(RLIMIT_AS, &rlp);
    i = getrlimit(RLIMIT_DATA, &rlp);
    rlp.rlim_cur = rlp.rlim_max;
    i = setrlimit(RLIMIT_DATA, &rlp);
    i = getrlimit(RLIMIT_STACK, &rlp);
    rlp.rlim_cur = rlp.rlim_max;
    i = setrlimit(RLIMIT_STACK, &rlp);
#endif
#ifdef ultrix
    if ((appname = (char *)strrchr(*argv, '/')) == NULL){
	appname = *argv;
	sprintf(buff,"which %s",*argv);
	if((progname=(char *)malloc(256)) == NULL)
	    printf("error getting progname\n");
	stream = popen(buff,"r");
	fgets(progname,256,stream);
	pclose(stream);
	if ( progname[strlen(progname)-1] == '\n' )
	  progname[strlen(progname)-1] = NULL ;
    }
    else{
	appname++;
	progname = *argv;
    }
#else
    if ((appname = (char *)strrchr(*argv, '/')) != NULL)
      appname++;
    else
      appname = *argv;
    progname = *argv;
#endif
/*
   Call cpinrc to set up conpack for calls to cpnumb
   */
    me=proc_find(progname);
    cpinrc_();
    setavar_("graphics_path", progname, &error, 13, strlen(progname));
    /*set up X toolkit and open display and shell etc*/

    /*
    Toplevel = XtVaAppInitialize(&app_con,appname,options,XtNumber(options),
				 &argc,argv,NULL,NULL);
    */
    if(me>0)
      me--;
    Toplevel = XtVaOpenApplication(&app_con,appname,options,XtNumber(options),
				   &argc,argv,NULL,applicationShellWidgetClass,
				   XtNallowShellResize, True, NULL);
    if (argc > 1) {
	fprintf(stderr, "Illegal command-line options specified.\n");
	fprintf(stderr, "%s understands all standard \
Xt command-line options.\n", appname);
	fprintf(stderr, "Additional options are:\n");
	fprintf(stderr, "-file           Input file name\n");
	fprintf(stderr, "-input_file     Input file name\n");
	fprintf(stderr, "-acm            Allocate New Colormap (yes/no)\n");
	fprintf(stderr, "-allocate_cmap  Allocate New Colormap (yes/no)\n");
	fprintf(stderr, "-log            Log file name\n");
	fprintf(stderr, "-log_file       Log file name\n");
	fprintf(stderr, "-trans          Transform file name\n");
	fprintf(stderr, "-transform_file Transform file name\n");
    }
    XtVaGetApplicationResources(Toplevel, &app_data, resources,
				XtNumber(resources), NULL);
    if (DisplayCells(XtDisplay(Toplevel),
		     DefaultScreen(XtDisplay(Toplevel))) > 2 ){
	Boolean specified=FALSE, allocate=FALSE;
	XrmValue from, to;
	if (app_data.allocate_cmap != NULL) {
	    from.size = strlen(from.addr=app_data.allocate_cmap);
	    to.size = sizeof(Boolean);
	    to.addr = (XtPointer)&allocate;
	    specified = XtConvertAndStore(Toplevel, XtRString,
					  &from, XtRBoolean, &to);
	}
	cmap = DefaultColormap(XtDisplay(Toplevel),
			       DefaultScreen(XtDisplay(Toplevel)));
	if((i = count_colors(Toplevel,cmap))<NUM_COLORS){
	  if(i==0){
	    /*truecolor or directcolor*/
	    printf("Currently using True Color - I hope that is OK\n");
	    allocate=0;
	  }
	  else{
	    if (specified == FALSE) {
	      printf("IVE: only %d users colors available, \
allocate new color map [y\\n]? ", i-10);
	      fgets(buff,2,stdin);
	      allocate=(buff[0]!='n' && buff[0] != 'N');
	    }
	  }
	}
	if (allocate) {
	    unsigned long mask[1], pixels[119];
	    XColor bundle;
	    Display *dpy;
	    int scrn;
	    int depth=8, vclass=PseudoColor;
	    XVisualInfo vinfo;
	    dpy = XtDisplay(Toplevel);
	    scrn =  DefaultScreen(dpy);

	    if(!XMatchVisualInfo(dpy, scrn, depth, vclass, &vinfo)){
	      printf("Your workstation does not support pseudocolor, and IVE can not use truecolor at this time\n");
	      exit(1);
	    }
	    cmap = XCreateColormap(dpy,
				   RootWindow(dpy,scrn), 
				   vinfo.visual,
				   AllocNone);
	    XAllocColorCells(dpy,cmap,FALSE,mask,0,pixels,10);
	    bundle.flags=DoRed|DoGreen|DoBlue;
	    for(i=0; i<10; i++){
		bundle.pixel=i;
		XQueryColor(XtDisplay(Toplevel), 
			    DefaultColormap(XtDisplay(Toplevel),
				 DefaultScreen(XtDisplay(Toplevel))), &bundle);
		bundle.pixel = pixels[i];
		XStoreColor(XtDisplay(Toplevel), cmap, &bundle);	     
	    }
	}
	else{
	  Display *dpy;
	  int scrn;
	  dpy = XtDisplay(Toplevel);
	  scrn =  DefaultScreen(dpy);
	  cmap = DefaultColormap(dpy, scrn);
	}
    }
    SIGNAL_FPE;
    XtVaSetValues(Toplevel,XtNallowShellResize,TRUE,
		  XmNy,30, XmNcolormap,cmap,NULL);
      
    XtRealizeWidget(Toplevel);
    first_loop(app_con);
    xgks_widget = init_xgks(Toplevel,appname,cmap);
    ive_widget = init_ive(Toplevel,file_widget);
    XtVaSetValues(XtParent(xgks_widget),XmNshadowThickness,2,
		  XmNbottomShadowColor,colors[me],
		  XmNtopShadowColor,colors[me],NULL);
    XtVaSetValues(XtParent(ive_widget),XmNshadowThickness,2,
		  XmNbottomShadowColor,colors[me],
		  XmNtopShadowColor,colors[me],NULL);
    attributes.backing_store = Always;
    attributes.save_under = TRUE;
    XChangeWindowAttributes(XtDisplay(Toplevel),XtWindow(xgks_widget),
			    att_mask,&attributes);
    first_loop(app_con);
    add_credits(XtDisplay(Toplevel), xgks_widget);
/*    XWarpPointer(XtDisplay(Toplevel), None, XtWindow(ive_widget), 0, 0, 0, 0,
		 100, 100);*/
    setdef_();
    (void)init_cursors();
    /* 
       Initialize units conversion package.
       */
    status = utInit("") ;
    if ( status != 0 ) {
	switch ( status ) {
	case UT_ENOFILE : 
	    make_help_widget_ ("Error in ive_main: The units-file doesn't exist.") ;
	    break ;
	case UT_ESYNTAX :  
	    make_help_widget_ ("Error in ive_main: The units-file contains a syntax error.") ;
	    break ;
	case UT_EUNKNOWN :
	    make_help_widget_ ("Error in ive_main: The units-file contains an unknown specification.") ;
	    break ;
	case UT_EIO :
	    make_help_widget_ ("Error in ive_main: An I/O error occurred while accessing the units-file.") ;
	    break ;
	case UT_EALLOC : 
	    make_help_widget_ ("Error in ive_main: A memory allocation failure occurred.") ;
	    break ;
	default :
	    break;
	}
    }
    if (app_data.log_file != NULL) {
	sprintf(buff, "LOG=%s", app_data.log_file);
	driver_notwin(buff);
    }
    else{
	sprintf(buff,"LOG=~/ive.log");
	driver_notwin(buff);
    }
    signal(SIGFPE,SIG_DFL);
    /*
     *load transforms
     */
    if (app_data.transform_file != NULL) {
#ifdef ultrix
	(void)make_info_widget_(
	 "loading transform file\nplease be patient during linking process");
	first_loop(app_con);
#endif
	setavar_("transform_path", app_data.transform_file, &error, 14, 
		 strlen(app_data.transform_file)) ;
	tmpbuf=(char *)malloc(10 + strlen(app_data.transform_file) +1);
	sprintf(tmpbuf,"TRANSFORM=%s",app_data.transform_file);
	driver_notwin(tmpbuf);
	free(tmpbuf);
#ifdef ultrix
	(void)make_info_widget_("");
#endif
    }
    else{
#ifdef BETA
	if(((transenv = (char *)getenv("IVETRANSFILE_BETA")) != NULL ) ||
	   ((transenv = (char *)getenv("IVETRANSFILE")) != NULL ) ||
	   ((transenv = (char *)getenv("ivetransfile")) != NULL ) ||
	   ((transenv = (char *)getenv("UWGAPTRANSFILE")) != NULL ) ||
	   ((transenv = (char *)getenv("uwgaptransfile")) != NULL ) )
	{
#else
	if(((transenv = (char *)getenv("IVETRANSFILE")) != NULL ) ||
	   ((transenv = (char *)getenv("ivetransfile")) != NULL ) ||
	   ((transenv = (char *)getenv("UWGAPTRANSFILE")) != NULL ) ||
	   ((transenv = (char *)getenv("uwgaptransfile")) != NULL )  ||
	   ((transenv = (char *)getenv("IVETRANSFILE_BETA")) != NULL ) )
	{
#endif
#ifdef ultrix
	    (void)make_info_widget_(
	   "loading transform file\nplease be patient during linking process");
	    first_loop(app_con);
#endif
	    setavar_("transform_path", transenv, &error, 14, 
		     strlen(transenv)) ;
	    tmpbuf=(char *)malloc(10 + strlen(transenv) +1);
	    sprintf(tmpbuf,"TRANSFORM=%s",transenv);
	    driver_notwin(tmpbuf);
	    free(tmpbuf);
#ifdef ultrix
	    (void)make_info_widget_("");
#endif
	}
    }
    
    /*
     * check for startup files
     */
    comment_log_(&one);
    driver_notwin("INPUT=0,0");
    comment_log_(&negone);
    if (app_data.input_file != NULL) {
        /*
           Make sure credit window appears.
           */
	first_loop(app_con);
	sprintf(buff, "INPUT=%s", app_data.input_file);
	if (app_data.debug) strcat(buff, ",debug");
	driver_notwin(buff);
    }
    if (app_data.stereo) Ive_Stereo = True;
    (void)getavar_("transform_path",buff,&error,14,256);
    if ( buff[0]=='\0'){
	(void)make_help_widget_(
"You did not set the environment variable IVETRANSFILE.\n\
You will be using the default (identity) transforms\n\
unless you load your own transforms object file.");
	setavar_("transform_path", "default", &error, 14, 7);
	
    }

    if (progname != *argv)free(progname);
    signal(SIGINT,quit_query);

/* unset shell var so system call frrom sun fortran will not go through
   .cshrc.
*/
    i = 0;
    while((tmpbuf = *(envp+i)) != NULL){
	if(!strncmp("SHELL",tmpbuf,7))
	  {	/*found it*/
	      int j;
	      for(j=i, tmpbuf2 = (envp + j); *tmpbuf2!= NULL; j++)
		*(tmpbuf2) = *(tmpbuf2 + 1);
	      free(tmpbuf);
	  }
	i++;
    }

    XtAppMainLoop(app_con);
}
