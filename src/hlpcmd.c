static char rcsid[] = "$Id: hlpcmd.c,v 1.18 2005/09/06 20:22:14 warren Exp $";
/* $Id: hlpcmd.c,v 1.18 2005/09/06 20:22:14 warren Exp $
 *
 * $Log: hlpcmd.c,v $
 * Revision 1.18  2005/09/06 20:22:14  warren
 * Move updates into current stream
 *
 * Revision 1.17.2.1  2005/09/06 17:55:22  warren
 * fixed trajectories to deal with maps. Also first part of bold arrows for
 * trajectories
 *
 * Revision 1.17  2001/08/15 22:25:47  davidive
 * Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
 * Added code to set up which axis to use and to convert the labels in 1d plots.
 *
 * Revision 1.16  1996/10/18 14:54:06  harry
 * Fix declarations.
 *
 */
#ifdef linux
#define vfork fork
#endif

#include <malloc.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <strings.h>
#include <stdio.h>
#include <cmndlst.h>
#include <ive_macros.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <Xm/XmAll.h>
#ifdef mips
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif
#ifndef HELPDIR
//#define HELPDIR "/usr/local/lib/ive/help/"
#define HELPDIR "http://www.atmos.washington.edu/ive/ive_help/"
#endif
#ifndef BROWSER
#define BROWSER "netscape"
#endif

char filename[256], *ive_help_browser=NULL;
void print_help_file ();


void hlpcmd_ (command, len)
     char *command;
     int len;

     /* This function reads and prints out a help file based on the 
	command key number.
	*/
{
    char *libbuf=NULL;
    struct stat statbuf;
    extern char filename[];
    
    /* Get the help file directory.
     * and Construct the help file name.
     */
    if (strcasecmp(command, "help") == 0)
	  sprintf (filename,"%sindex.html",((libbuf=getenv("IVEHELPDIR"))!=NULL? libbuf:HELPDIR));
    else
	  sprintf (filename,"%s%s.html",((libbuf=getenv("IVEHELPDIR"))!=NULL? libbuf:HELPDIR),command);

/*    if(stat(filename, &statbuf) == -1 ){
	  sprintf (filename, "Sorry, IVE cannot locate help for this command.\nPlease see a local IVE power user for assistance.");
	  make_help_widget (filename);
	  return;
    }
	  */
    
    /* Print the help file.
     */
    print_help_file (filename);
    
    return;
}

Window find_netscape()
{
    extern Widget Box;
    int i;
    Display *dpy;
    Window root;
    Window root2, parent, *kids, j;
    unsigned int nkids;
    Window result = 0;
    dpy = XtDisplay(Box);
    root = DefaultRootWindow(dpy);
    if (! XQueryTree (dpy, root, &root2, &parent, &kids, &nkids))
      return (0);
    if (! (kids && nkids))
      return(0);
    for (i = 0; i < nkids; i++){
	char *name;
	XFetchName(dpy, kids[i], &name);
	if(name != NULL && (!strncmp(name,"netscape",8) || !strncmp(name,"Netscape",8))){
	    j=kids[i] ;
	    XFree(kids);
	    return(j);
	}
    }
    return(0);
}

Window find_mozilla()
{
    extern Widget Box;
    int i;
    Display *dpy;
    Window root;
    Window root2, parent, *kids, j;
    unsigned int nkids;
    Window result = 0;
    dpy = XtDisplay(Box);
    root = DefaultRootWindow(dpy);
    if (! XQueryTree (dpy, root, &root2, &parent, &kids, &nkids))
      return (0);
    if (! (kids && nkids))
      return(0);
    for (i = 0; i < nkids; i++){
	char *name;
	XFetchName(dpy, kids[i], &name);
	if(name != NULL && (!strncmp(name+(strlen(name) - 7),"Mozilla",7))) {
	  j=kids[i] ;
	  XFree(kids);
	  return(j);
	}
    }
    return(0);
}

Window find_firefox()
{
    extern Widget Box;
    int i;
    Display *dpy;
    Window root;
    Window root2, parent, *kids, j;
    unsigned int nkids;
    Window result = 0;
    dpy = XtDisplay(Box);
    root = DefaultRootWindow(dpy);
    if (! XQueryTree (dpy, root, &root2, &parent, &kids, &nkids))
      return (0);
    if (! (kids && nkids))
      return(0);
    for (i = 0; i < nkids; i++){
	char *name;
	XFetchName(dpy, kids[i], &name);
	if(name != NULL && (!strncmp(name+(strlen(name) - 7),"Firefox",7))) {
	  j=kids[i] ;
	  XFree(kids);
	  return(j);
	}
    }
    return(0);
}

void print_help_file (filename)
     char *filename;
{
    extern Widget Box;
    char *c;
    int netscape = 0;
    int mozilla = 0;
    int firefox = 0;
    int i;
    char cline[512];
    Window win;
    
    if(ive_help_browser==NULL)
      if((ive_help_browser=getenv("IVEHELPBROWSER")) == NULL)
	ive_help_browser = BROWSER;
    
    if((c = strrchr(ive_help_browser, '/')) != NULL){
	netscape = strcmp("netscape", ++c);
	mozilla = strcmp("mozilla", c);
	firefox = strcmp("firefox", c);
    }
    else{
	netscape = strcmp("netscape", ive_help_browser);
	mozilla = strcmp("mozilla", ive_help_browser);
	firefox = strcmp("firefox", ive_help_browser);
	c = ive_help_browser;
    }

    if(!netscape){
      /* we use netscape */
      if((win = find_netscape()) != 0){
	sprintf(cline,"openFile(%s)",filename);
	i=vfork();
	if(i==0){
	  /*	      close(0);
	    close(1);
	    close(2);*/
	  signal(SIGFPE, SIG_IGN);
	  //printf("netscape -raise -remote %s\n",cline);
	  execlp(ive_help_browser,"netscape","-raise","-remote",cline,(char *)0);
	}
	return;
      }
      else{
	i = fork();
	if(i ==0){
	  close(0);
	  signal(SIGFPE, SIG_IGN);
	  execlp(ive_help_browser, c, filename, NULL);
	  /*	    sprintf(cline,"%s %s &",
	    ive_help_browser,filename);
	    system(cline);*/
	}
      }
    }
    
    if(!mozilla){
      /* we use mozilla */
      if((win = find_netscape()) != 0){
	sprintf(cline,"openFile(%s)",filename);
	i=vfork();
	if(i==0){
	  /*	      close(0);
	    close(1);
	    close(2);*/
	  signal(SIGFPE, SIG_IGN);
	  //printf("mozilla,-raise,-remote %s\n",cline);
	  execlp(ive_help_browser,"mozilla","-raise","-remote",cline,(char *)0);
	}
	return;
      }
      else{
	i = fork();
	if(i ==0){
	  close(0);
	  signal(SIGFPE, SIG_IGN);
	  execlp(ive_help_browser, c, filename, NULL);
	  /*	    sprintf(cline,"%s %s &",
	    ive_help_browser,filename);
	    system(cline);*/
	}
      }
    }
    
    if(!firefox){
      /* we use firefox */
      if((win = find_firefox()) != 0){
	sprintf(cline,"openFile(%s)",filename);
	i=vfork();
	if(i==0){
	  /*	      close(0);
	    close(1);
	    close(2);*/
	  signal(SIGFPE, SIG_IGN);
	  //printf("firefox -raise -remote %s\n",cline);
	  execlp(ive_help_browser,"firefox","-raise","-remote",cline,(char *)0);
	}
	return;
      }
      else{
	i = fork();
	if(i ==0){
	  close(0);
	  signal(SIGFPE, SIG_IGN);
	  execlp(ive_help_browser, c, filename, NULL);
	  /*	    sprintf(cline,"%s %s &",
	    ive_help_browser,filename);
	    system(cline);*/
	}
      }
    }
    
}

    
