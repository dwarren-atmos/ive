#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#ifdef linux
#include <string.h>
#else
#include <strings.h>
#endif
#include <stdio.h>
#include <sys/param.h>
#include <xgks.h>
#include <in_commands.h>
#include <ive_gks.h>
#include <ive_macros.h>
#include <ive_color.h>

/*
-----------------------------------------------------------------------
   $Id: read_color.c,v 1.13 2002/12/09 18:04:45 warren Exp $
   History:
   $Log: read_color.c,v $
   Revision 1.13  2002/12/09 18:04:45  warren
   Fix writecolortable and some of the color init commands

   Revision 1.12  2002/08/09 19:58:00  warren
   Fixed for Linux and tested with DEC sun etc.
   most items inside #ifdef's, or were general cleanup/fixes

   Revision 1.11  2002/02/22 21:10:41  davidive
   Fixed ive to set the traj_entry color and use it correctly, instead of
   zeroing it out every time.

   Revision 1.10  2002/02/11 23:43:09  davidive
   First cut at traj fixes - most of the way there

   Revision 1.9  2001/11/02 18:51:18  davidive
   converted rdctab to c, fixed trajectories so that their independant color
   tables work, including setting center color, center value and interval.

   Revision 1.8  2001/01/31 01:04:55  warren
   Removed print statement

   Revision 1.7  2001/01/31 01:03:33  davidive
   Make zero_color mean the first positive interval, not the last negative.

   Revision 1.6  2001/01/31 00:19:12  davidive
   Read Color now looks for a rgb value bracketed by 888s to set as the zero
   coloe.
   Select old checks for movement from lower to higher dimensions and moves
   the window in that dimension from 0,0 to as big as it goes.

   Revision 1.5  2001/01/23 22:30:11  warren
   took out print statements

   Revision 1.4  2001/01/23 21:42:02  davidive
   First part of new zero centered colors.
   added zero_color to color.icl and an option for setting it to the
   call to read_color.

   Revision 1.3  1999/12/22 00:33:21  warren
   Fixed Dales crashing bug when changing from point slicer to normal. Also
   have some ifdef linuxes in there in case we ever find a fortran compiler.

 * Revision 1.2  1994/03/28  20:00:20  warren
 * added dependencies for read_color to make the color table change the
 * first time.
 *
 * Revision 1.1.1.1  1993/04/07  19:40:46  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 2.1  1992/08/24  20:40:56  millerp
 * Uses make_help_widget.
 *
 * Revision 2.0  1992/07/01  21:16:53  millerp
 * Released version rereving to 2.0
 *
 * Revision 1.2  1992/05/18  22:37:27  millerp
 * Added bogus argument for fortran.
 *
 * Revision 1.1  1992/04/21  00:23:37  millerp
 * Initial revision
 *
-----------------------------------------------------------------------
   these are used in all callback routines except for those that only effect
   the interface
*/
extern int expand_path_(),ginqcolorrep(),gsetcolourrep(),gupdatews();
extern void getivar_(),make_help_widget_(),setivar_(),ive_update_colorbar(),update_all_();

void write_color_table(outfile,min,max,iscontour)
     char *outfile;
     int min,max;
     int iscontour;
{
    FILE *out;
    Gcobundl bundle;
    int c,mark_val,error;
    char buf[80];

    if(iscontour){
      (void)getivar_("zero_color", &mark_val, &error,10);
    }
    else{
      (void)getivar_("traj_entry", &mark_val, &error,10);
    }
    if(error)mark_val=-1;

    if((out = fopen(outfile,"w+")) == 0)
	{
	    sprintf(buf,"can't open file %s\n",outfile);
	    (void)make_help_widget_(buf);
	    return;
	}

    for(c = min; c < max+1; c++){
	    ginqcolorrep(1,c,GREALIZED,&bundle);
	    if(c == mark_val)fprintf(out,"888,888,888\n");
	    fprintf(out,"%d, %d, %d\n",(int) (255*bundle.red),
		    (int) (255*bundle.green),
		    (int) (255*bundle.blue));
	    if(c == mark_val)fprintf(out,"888,888,888\n");
	}	
	    fprintf(out,"999,999,999\n");
    fclose(out);
}


void read_color_table(inname,min,max,iscontour)
     char *inname;
     int min,max,iscontour;
{
    FILE *in;
    int c,center=-1, error;
    int r,g,b;
    Gcobundl bundle;
    char buf[80];
    extern Widget Box;
    
    if((in = fopen(inname,"r")) == 0)
      {
	  sprintf(buf,"can't open file %s\n",inname);
	  (void)make_help_widget_(buf);
	  return;
      }
    c = min;
    while((fscanf(in,"%d,%d,%d,",
		  &r,&g,&b)!= -1)
	  &&c <= max && r != 999){
      if(r > 255 /*should be 888*/){
	fscanf(in,"%d,%d,%d,",
	       &r,&g,&b);
	bundle.red = r/255.0;
	bundle.green = g/255.0;
	bundle.blue = b/255.0;
	gsetcolourrep(1,c,&bundle);
	gupdatews(WS_X,GPERFORM);                /*now!!!*/
	if(iscontour){
	  center = c;
	  (void)setivar_("zero_color", &center, &error,10);
	}	
	else{
	  center = c;
	  (void)setivar_("traj_entry", &center, &error,10);
	}		  
	c++;
	fscanf(in,"%d,%d,%d,",
	       &r,&g,&b); /*should be another 888*/
	if(r<256){
	  bundle.red = r/255.0;
	  bundle.green = g/255.0;
	  bundle.blue = b/255.0;
	  gsetcolourrep(1,c,&bundle);
	  gupdatews(WS_X,GPERFORM);                /*now!!!*/
	  c++;
	}
      }	
      else{
	bundle.red = r/255.0;
	bundle.green = g/255.0;
	bundle.blue = b/255.0;
	gsetcolourrep(1,c,&bundle);
	gupdatews(WS_X,GPERFORM);                /*now!!!*/
	c++;
      }
    }
    if(iscontour && center <min){
	center = (c - 1 - min)/2 + min;
      (void)setivar_("zero_color", &center, &error,10);
    }
    else if(!iscontour && center< min){
      center = (int)((min + max)/2);
      (void)setivar_("traj_entry", &center, &error,10);
    }
      
    if(c < max){
	for(; c < max+1; c++){
	    bundle.red =0.5;
	    bundle.green = 0.5;
	    bundle.blue = 0.5;
	    gsetcolourrep(1,c,&bundle);
	    gupdatews(WS_X,GPERFORM);                /*now!!!*/
	    /*	    ginqcolorrep(1,c,GREALIZED,&bundle);*/
	}
    }
    fclose(in);
    XSync(XtDisplay(Box),FALSE);   /*make sure display is updated now*/
    XSync(XtDisplay(Box),FALSE);   /*make sure display is updated now*/
    XSync(XtDisplay(Box),FALSE);   /*make sure display is updated now*/
    if (IVE_TRUE_COLOR){
      (void)ive_update_colorbar();
    }
    update_all_();
}

read_color_table_(inname,min,max,iscontour)
     char *inname;
     int *min,*max,*iscontour;
{
  /* used by setdef */
  read_color_table(inname,*min,*max,*iscontour);
}


void rdctab_ (error)
     int *error;
{
  char **argv, **get_args(), buff[MAXPATHLEN+1];
  int argc;
  *error = 0;
  argv = get_args(&argc);
  if(argv[1] != NULL){
    strncpy(buff,argv[1],MAXPATHLEN);
    if(expand_path_(buff,MAXPATHLEN)){
      read_color_table(buff, user_colors_.min_user_color,
		       user_colors_.max_user_color, 1);
    }
    else{
      (void)make_help_widget_("Function expand_path failed in subroutine rdctab.");
      *error=1;
    }
  }
  else{
    *error=1;
    (void)make_help_widget_("READ_COLOR_TABLE requires a value");
  }
}

void rdtctab_ (error)
     int *error;
{
  char **argv, **get_args(), buff[MAXPATHLEN+1];
  int argc;
  *error = 0;
  argv = get_args(&argc);
  if(argv[1] != NULL){
    strncpy(buff,argv[1],MAXPATHLEN);
    if(expand_path_(buff,MAXPATHLEN)){
      read_color_table(buff, user_colors_.min_traj_color,
		       user_colors_.max_traj_color, 0);
    }
    else{
      (void)make_help_widget_("Function expand_path failed in subroutine rdtctab.");
      *error=1;
    }
  }
  else{
    *error=1;
    (void)make_help_widget_("READTCOLORTABLE requires a value");
  }
}
    

void wtctab_ (error)
     int *error;
{
  char **argv, **get_args(),  buff[MAXPATHLEN+1];
  int argc;
  *error = 0;
  argv = get_args(&argc);
  if(argv[1] != NULL){
    strncpy(buff,argv[1],MAXPATHLEN);
    if(expand_path_(buff,MAXPATHLEN)){
      write_color_table(buff, user_colors_.min_user_color,
		       user_colors_.max_user_color, 1);
    }
    else{
      (void)make_help_widget_("Function expand_path failed in subroutine wtctab.");
      *error=1;
    }
  }
  else{
    *error=1;
    (void)make_help_widget_("WRITE_COLOR_TABLE requires a value");
  }
}

void wttrajctab_ (error)
     int *error;
{
  char **argv, **get_args(), buff[MAXPATHLEN+1];
  int argc;
  *error = 0;
  argv = get_args(&argc);
  if(argv[1] != NULL){
    strncpy(buff,argv[1],MAXPATHLEN);
    if(expand_path_(buff,MAXPATHLEN)){
      write_color_table(buff, user_colors_.min_traj_color,
		       user_colors_.max_traj_color, 0);
    }
    else{
      (void)make_help_widget_("Function expand_path failed in subroutine wtctab.");
      *error=1;
    }
  }
  else{
    *error=1;
    (void)make_help_widget_("WRITE_TRAJ_COLOR_TABLE requires a value");
  }
}
    
