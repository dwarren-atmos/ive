/****************************************************************************
 *$Log: ive_main_widget.c,v $
 *Revision 1.40  2006-10-05 16:48:22  reinecke
 *Added menu options for grey lines and white to red color table
 *
 *Revision 1.39  2006/04/24 21:36:00  warren
 *Added a scroll bar to the fields area, fixed the background form so that
 *you can see everything with the new motif, fixed the call to getlvar in
 *annotation.c to look for color_bar instead of color.
 *
 *Revision 1.38  2006/03/03 18:17:26  warren
 *fixed newstring to not use strtok due to "NEVER USE THIS FUNCTION" warning in Linux manpage
 *
 *Revision 1.37  2005/11/03 19:50:27  warren
 *Fixed crashing with large strange dataset
 *
 *Revision 1.36  2002/08/09 19:57:27  warren
 *Fixed for Linux and tested with DEC sun etc.
 *most items inside #ifdef's, or were general cleanup/fixes
 *
 *Revision 1.35  2001/03/14 20:21:12  warren
 *corrected severa longs to unsigned longs, changed creation of lock button to
 *not set the name, the set the label seperately, change XtFree to XmStringFree
 *where it should be.
 *
 *Revision 1.34  2001/02/09 00:13:10  davidive
 *look for IVECTB prior to IVECTBLIB
 *
 *Revision 1.33  2000/09/21 19:49:33  harry
 *Fix errors in line color and pattern handling.
 *
 *Revision 1.32  2000/05/09 19:53:33  harry
 *Seperate contour and line colors.  Make patterns work like colors.
 *
 *Revision 1.31  1998/12/17 23:17:00  warren
 *Cleanup of pointer casts to long instead of int, freeing XmStrings when no
 *longer in use, passing all args to sprinf's and making correctly casting NULLs
 *
 *Revision 1.30  1997/10/22 19:34:38  davidive
 *Turned the particle movement part of trajectories into a user transform
 *and added unit checking to the default transform.
 *
 *Revision 1.29  1997/04/30 18:46:10  harry
 *Add widget to write out color table.
 *
 *Revision 1.28  1996/04/25 16:24:23  harry
 *Add Skew-T plots and the beginning of trajectory plots.
 *
 *Revision 1.27  1996/04/01 20:23:10  warren
 *Terrain form and new terrain command (on / off)
 *
 *Revision 1.26  1996/03/29 20:37:02  harry
 *Add new environment variable IVECTBLIB, which specifies the location
 *of the built-in color tables.
 *
 *Revision 1.25  1996/02/21 21:02:04  warren
 *Color menus are now readable and wide enough to select.
 *
 *Revision 1.24  1996/01/12 16:10:59  warren
 *Transfer of new pattern stuff from 3.x and beginning of color fixes
 *(color vectors, correct collors on the pulldowan menus)
 *
 *Revision 1.23  1995/12/15 22:32:39  warren
 *moved patterns into 4.0 and implemented the list of fixes and changes from
 *Curtis.
 *
 *Revision 1.22  1995/10/19 22:13:18  harry
 *Set defaults by clicking right mouse button.  Make all TextWidgets into
 *TextField Widgets, fix bug in loop widget.
 *
 * Revision 1.21  1995/07/10  17:43:36  warren
 * Fixed over active pos label
 *
 * Revision 1.20  1995/07/10  17:16:13  warren
 * Finished renaming widgets so the help key will work. Also fixed the color
 * menus not to contain an _ in the boxes and fixed the second vector
 * componant update proble.
 *
 * Revision 1.19  1995/06/23  21:11:22  warren
 * First part of new ive help
 * Changed " to ' in fortran files
 * Fixed the label spacing in 1d plots
 * added frame_width command
 *
 * Revision 1.18  1995/05/26  15:20:05  harry
 * Add SAVE_WINDOW command and widget.
 *
 * Revision 1.17  1994/10/08  18:59:53  warren
 * The annotation widget and updating it.
 *
 * Revision 1.16  1994/04/26  18:06:03  warren
 * Beginning of mouse slice interface (doesn't really do anything yet except
 * mark up your plot)
 *
 * Revision 1.15  1994/01/20  20:43:35  yuri
 * Propagating of "overlay_patterns" stuff to newer version.
 *
 * Revision 1.14  1993/11/03  23:33:11  warren
 * General cleanup for beginning of port to alpha.
 *
 * Revision 1.13  1993/09/24  16:52:22  yuri
 * Names of colours substituted by colors.
 *
 * Revision 1.12  1993/09/08  23:14:27  warren
 * Print widget code.
 *
 * Revision 1.11  1993/08/17  18:33:55  warren
 * Beginning of printer forms.
 *
 * Revision 1.10  1993/08/02  18:51:49  warren
 * NULL only has 2 Ls
 *
 * Revision 1.9  1993/08/02  16:45:46  warren
 * Last piece to make eps widget work.
 *
 * Revision 1.8  1993/06/28  22:34:49  warren
 * Add missing 0 in create widget call.
 *
 * Revision 1.7  1993/06/25  23:13:36  warren
 * Fixed widget name.
 *
 * Revision 1.6  1993/06/24  23:14:06  warren
 * added call and routine for map form.
 *
 * Revision 1.5  1993/06/22  19:34:39  warren
 * rearanged background and plottype on main widget and fixed associated calls
 * appropriately. Also fixed main to call driver correctly.
 *
 * Revision 1.4  1993/06/10  16:28:27  yuri
 * Vector colors stuff added.
 *
 * Revision 1.3  1993/05/28  18:50:54  warren
 * Moved dump window widget to output from input,
 *
 * Revision 1.2  1993/04/26  19:18:10  warren
 * See previous message
 *
 * Revision 1.1.1.1  1993/04/07  19:41:22  harry
 * Initial CVS checkin of IVE.
 *
 ****************************************************************************/
static char ident[] = "$Id: ive_main_widget.c,v 1.40 2006-10-05 16:48:22 reinecke Exp $";

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/XmAll.h>
#include <stdio.h>
#include <string.h>
#ifndef linux
#include <strings.h>
#endif
#include <locale.h>
#include <stdlib.h>

#include <ive.h>
#include <ive_for.h>
#include <ive_macros.h>
#include <print.h>

#ifndef LIBDIR
#define LIBDIR "/usr/local/lib/ive/"
#endif

/*-----------------------------------------------------
    Create a new XmString from a char*

    This function can deal with embedded 'newline' and
    is equivalent to the obsolete XmStringCreateLtoR,
    except it does not use non AES compliant charset
    XmSTRING_DEFAULT_CHARSET
----------------------------------------------------*/
XmString NewString(s)
     char *s;
{
    XmString xms1;
    XmString xms2;
    XmString line;
    XmString separator;
    char     *p,*q;
    int len;
    len=strlen(s);
    char     *t = XtNewString(s);  /* Make a copy so as not to */
                                    /* damage the original string    */

    separator = XmStringSeparatorCreate();
    /*    p         = strtok(t,"\n");*/
    p=t;
    if((q=index(p,'\n')) != NULL)q='\0';
    xms1      = XmStringCreateLocalized(p);
    p=q+1;

    /*    while ((p = strtok(NULL,"\n")) != (char *)NULL)*/

    while ((((unsigned long)p - (unsigned long)t) < len) &&  ((q=index(p,'\n')) != NULL))
    {
      q='\0';
      line = XmStringCreateLocalized(p);
      xms2 = XmStringConcat(xms1,separator);
      XmStringFree(xms1);
      xms1 = XmStringConcat(xms2,line);
      XmStringFree(xms2);
      XmStringFree(line);
      p=q+1;
    }

    XmStringFree(separator);
    XtFree(t);
    return xms1;
}


#define BOXWIDTH 305
#define BOXHEIGHT 230

struct{
    Widget plot,overlay,lift_overlay;
    Widget input_menu, input,load_file,load_trans,load_input;
    Widget output_menu, output,dump_win,print,eps,pdf;
    Widget print_current, print_setup, print_menu;
    Widget print_twoper, print_fourper, print_sixper;
    Widget set_log, save_win, quit;
    Widget type_menu, type, Scalar, Vector, Trajectory, Skewt, Line;
    Widget color_menu, cotable_sel, color, readtab, fore_to_back;
    Widget Surface;
    Widget mixer, mixerrgb, mixerhsl, mixer_sel,show_color;
    Widget default_table, DL_table, custom_table;
    Widget LD_table, BR_table, GBR_table, MBR_table, RBW_table, WR_table;
    Widget color_opts, write_table;
    Widget loop,help;
    Widget menu,row1,row2;
    Widget field_scroll,field_holder,command;
    Widget plot_menu, plot_att;
    Widget background_menu, background, map, terrain, annotation;
    Widget line_att, line_menu, linecolor_menu;
    Widget linecolorhi_menu, linecolorlo_menu, linecolorbo_menu;
    Widget colorsel, chi, clo, cbo, cdef;
    Widget colorb, red, green, blue, grey; 
    Widget black, white, brown, violet, orange, yellow;
    Widget colorhi,redhi,greenhi,bluehi, greyhi, yellowhi;
    Widget blackhi,whitehi,brownhi,violethi,orangehi;
    Widget colorlo,redlo,greenlo,bluelo,greylo;
    Widget blacklo,whitelo,brownlo,violetlo,orangelo, yellowlo;
    Widget linepattern, patternsel, patternb, patternhi, patternlo, patdef;
    Widget patb, line_solid, dash, dot, dashdot, 
           dashdotdot, longdash_short,longdash;
    Widget pathi, line_solidhi, dashhi, dothi, dashdothi,
           dashdotdothi, longdash_shorthi,longdashhi;
    Widget patlo, line_solidlo, dashlo, dotlo, dashdotlo,
           dashdotdotlo, longdash_shortlo,longdashlo;

    Widget linewidth, widthsel, one, two, three;
} main_widget;

void do_terrain();
void do_map();
void do_annotation();

Widget init_ive(parent,file_widget)
     Widget parent,file_widget;
{
    void init_background(), init_color(),slice_2d_(),init_dir(), init_eps();
    void init_trans(), init_ctb(), init_loop(), init_input(), init_dump();
    void init_log(), init_pdf(), init_printsetup(), do_2up(), do_4up();
    void do_6up(), slice_menu_call(), init_wctb();
    static int z=0,o=1,tw=2,th=3,fo=4,fi=5;
    XmString str; /*temp string holder*/
    static char scalar[]="type=scalar";
    static char vector[]="type=vector";
    static char trajectory[]="type=trajectory";
    static char skewt[]="type=skewt";
    static char line[]="type=line";
    static char surface[]="type=3D";
    char *ivectblib, ctb[1024];
    XFontStruct *font_info;
    XmFontList BigList;
    Pixmap twoper, fourper, sixper;
    Pixel fg,bg,redp,greenp,bluep,violetp,whitep,greyp,blackp,yellowp,brownp;
    XColor color_inout, color_dummy;
    Widget wtemp;
    int i;
    extern Pixel IveGetPixel();
    /*Box etc*/

    Box = XtVaCreateManagedWidget("_IveBox",xmFormWidgetClass,
				  parent, 
				  XmNheight, BOXHEIGHT,
				  XmNwidth, BOXWIDTH,
				  XmNmarginWidth,5,
				  XmNresize, XmRESIZE_ANY,
				  NULL);

    XtAddCallback(Box, XmNhelpCallback,
		  check_help_call,NULL);
/**********************************************************************/
    /* menu bar etc */
    main_widget.menu = XmCreateMenuBar(Box,"menu",NULL,0);

    /*input menu*/
    main_widget.input_menu = XmCreatePulldownMenu(main_widget.menu,"Input",
						 NULL,0);

    str = NewString("Input");
    main_widget.input = XtVaCreateManagedWidget("Input",
					     xmCascadeButtonWidgetClass,
					       main_widget.menu,
					       XmNsubMenuId, 
					       main_widget.input_menu,
					       XmNlabelString,str,
					       NULL);
    XmStringFree(str);
  
    str = NewString("Load Data File");
    main_widget.load_file = XtVaCreateManagedWidget("File",
						    xmPushButtonWidgetClass,
						    main_widget.input_menu,
						    XmNlabelString,str,
						    NULL);
    XmStringFree(str);

    XtAddCallback(main_widget.load_file ,XmNactivateCallback,init_dir,NULL);

    str = NewString("Load Transforms");
    main_widget.load_trans = XtVaCreateManagedWidget("Transform",
						     xmPushButtonWidgetClass,
						     main_widget.input_menu,
						     XmNlabelString,str,
						     NULL);
    XmStringFree(str);

    XtAddCallback(main_widget.load_trans ,XmNactivateCallback,init_trans,NULL);

    str = NewString("Input Commands");
    main_widget.load_input = XtVaCreateManagedWidget("Input",
						     xmPushButtonWidgetClass,
						     main_widget.input_menu,
						     XmNlabelString,str,
						     NULL);
    XmStringFree(str);
    XtAddCallback(main_widget.load_input ,XmNactivateCallback,init_input,NULL);

    /*end of input menue */

    /*output menu*/
    main_widget.output_menu = XmCreatePulldownMenu(main_widget.menu,"Output",
						 NULL,0);

    str = NewString("Output");
    main_widget.output = XtVaCreateManagedWidget("Output",
						 xmCascadeButtonWidgetClass,
						 main_widget.menu,
						 XmNsubMenuId, 
						 main_widget.output_menu,
						 XmNlabelString,str,
						 NULL);
    /* print sub menu */
    main_widget.print_menu = XmCreatePulldownMenu(main_widget.output_menu,
						  "Print",
						  NULL,0);
    XmStringFree(str);

#if  (XmVERSION >1) || (XmREVISION >1)
    XtVaSetValues(main_widget.print_menu, XmNtearOffModel, XmTEAR_OFF_ENABLED,
		  NULL);
#endif
    str = NewString("Print Current");
    main_widget.print_current = 
      XtVaCreateManagedWidget("Print", xmPushButtonWidgetClass,
			      main_widget.print_menu,
			      XmNlabelString,str,
			      NULL);
    
    XmStringFree(str);

    str = NewString("Print Setup");
    main_widget.print_setup = 
      XtVaCreateManagedWidget("Print_Setup", xmPushButtonWidgetClass,
			      main_widget.print_menu,
			      XmNlabelString,str,
			      NULL);
    
    
    i=DefaultDepthOfScreen(XtScreen(main_widget.print_menu));
    XtVaGetValues(main_widget.print_setup,
		  XmNbackground,&bg,XmNforeground,&fg,NULL);

    XmStringFree(str);
    XtAddCallback( main_widget.print_setup, XmNactivateCallback,
		  init_printsetup,NULL);

    twoper = XCreatePixmapFromBitmapData(XtDisplay(main_widget.print_menu),
				 XtWindow(main_widget.print_menu),
				 twoper_bits,twoper_width,twoper_height,
				 fg,bg,i);
    main_widget.print_twoper=
      XtVaCreateManagedWidget("Print_twoper", xmPushButtonWidgetClass,
			      main_widget.print_menu,
			      XmNlabelType,XmPIXMAP,
			      XmNlabelPixmap,twoper,
			      NULL);
    XtAddCallback(main_widget.print_twoper, XmNactivateCallback,
		  do_2up, NULL);

    fourper = XCreatePixmapFromBitmapData(XtDisplay(main_widget.print_menu),
				 XtWindow(main_widget.print_menu),
                                 fourper_bits,fourper_width,fourper_height,
				 fg,bg,i);

    main_widget.print_fourper=
      XtVaCreateManagedWidget("Print_fourper", xmPushButtonWidgetClass,
			      main_widget.print_menu,
			      XmNlabelType,XmPIXMAP,
			      XmNlabelPixmap,fourper,
			      NULL);
    
    XtAddCallback(main_widget.print_fourper, XmNactivateCallback,
		  do_4up, NULL);

    sixper = XCreatePixmapFromBitmapData(XtDisplay(main_widget.print_menu),
				 XtWindow(main_widget.print_menu),
                                 sixper_bits,sixper_width,sixper_height,
				 fg,bg,i);

    main_widget.print_sixper=
      XtVaCreateManagedWidget("Print_sixper", xmPushButtonWidgetClass,
			      main_widget.print_menu,
			      XmNlabelType,XmPIXMAP,
			      XmNlabelPixmap,sixper,
			      NULL);
    
    XtAddCallback(main_widget.print_sixper, XmNactivateCallback,
		  do_6up, NULL);


    
    str = NewString("Print");
    main_widget.print = XtVaCreateManagedWidget("Print",
						xmCascadeButtonWidgetClass,
						main_widget.output_menu,
						XmNsubMenuId, 
						main_widget.print_menu,
						XmNlabelString,str,
						NULL);

    XmStringFree(str);
    XtAddCallback(main_widget.print_current ,XmNactivateCallback,
		  call_driver,"print");
    XtAddCallback(main_widget.print ,XmNactivateCallback,
		  call_driver,"print");


    str = NewString("EPS");
    main_widget.eps = XtVaCreateManagedWidget("EPS",
					      xmPushButtonWidgetClass,
					      main_widget.output_menu,
					      XmNlabelString,str,
					      NULL);

    XmStringFree(str);
    XtAddCallback(main_widget.eps ,XmNactivateCallback,init_eps,
		  NULL);

    str = NewString("PDF");
    main_widget.pdf = XtVaCreateManagedWidget("PDF",
					      xmPushButtonWidgetClass,
					      main_widget.output_menu,
					      XmNlabelString,str,
					      NULL);

    XmStringFree(str);
    XtAddCallback(main_widget.pdf ,XmNactivateCallback,init_pdf,
		  NULL);

    str = NewString("Dump Window");
    main_widget.dump_win = XtVaCreateManagedWidget("Dump",
						 xmPushButtonWidgetClass,
						 main_widget.output_menu,
						   XmNlabelString,str,
						   NULL);
    XmStringFree(str);
    XtAddCallback(main_widget.dump_win ,XmNactivateCallback,init_dump,NULL);


    str = NewString("LogFile");
    main_widget.set_log = XtVaCreateManagedWidget("LogFile",
						  xmPushButtonWidgetClass,
						  main_widget.output_menu,
						  XmNlabelString,str,
						  NULL);
    XmStringFree(str);

    XtAddCallback(main_widget.set_log ,XmNactivateCallback,init_log,NULL);


    /*end of output menu */


    /*background menu*/
    main_widget.background_menu = XmCreatePulldownMenu(main_widget.menu,
						       "Background",
						       NULL,0);

    str = NewString("Plot\nBackground");
    main_widget.background = XtVaCreateManagedWidget("Background",
					       xmCascadeButtonWidgetClass,
					       main_widget.menu,
					       XmNsubMenuId, 
					       main_widget.background_menu,
					       XmNlabelString,str,
					       NULL);
    XmStringFree(str);
  


    str = NewString("Base map");
    main_widget.map = XtVaCreateManagedWidget("BaseMap",
						 xmPushButtonWidgetClass,
						 main_widget.background_menu,
						 XmNlabelString,str,
						 NULL);
    XtAddCallback(main_widget.map ,XmNactivateCallback,do_map,NULL);
    XmStringFree(str);

    str = NewString("Terrain");
    main_widget.terrain = XtVaCreateManagedWidget("Terrain",
						  xmPushButtonWidgetClass,
						  main_widget.background_menu,
						  XmNlabelString,str,
						  NULL);
    XtAddCallback(main_widget.terrain ,XmNactivateCallback,do_terrain,NULL);
    XmStringFree(str);

    str = NewString("Annotation");
    main_widget.annotation = XtVaCreateManagedWidget("Annotation",
						     xmPushButtonWidgetClass,
						   main_widget.background_menu,
						     XmNlabelString,str,
						     NULL);
    XtAddCallback(main_widget.annotation,XmNactivateCallback,do_annotation,
		  NULL);
    XmStringFree(str);

    /*end of background menue */

    /*color menu*/
    main_widget.color_menu = XmCreatePulldownMenu(main_widget.menu,"Color",
						  NULL,0);

    str = NewString("Color\nTable");
    main_widget.color = XtVaCreateManagedWidget("ColorTable",
						xmCascadeButtonWidgetClass,
						main_widget.menu,
						XmNsubMenuId, 
						main_widget.color_menu,
						XmNlabelString,str,
						NULL);
    XmStringFree(str);
  


    str = NewString("Display\nTable");
    main_widget.show_color = XtVaCreateManagedWidget("DisplayTable",
						     xmPushButtonWidgetClass, 
						     main_widget.color_menu,
						     XmNlabelString,str,
						     NULL);
    XtAddCallback(main_widget.show_color ,XmNactivateCallback,
		  color_bar_callback,NULL);
    XmStringFree(str);

/***************/
    main_widget.mixer_sel=XmCreatePulldownMenu(main_widget.color_menu,
					       "Mixer",NULL,0);

    str = NewString("Edit Table");
    main_widget.mixer = XtVaCreateManagedWidget("Edit Table",
						xmCascadeButtonWidgetClass,
						main_widget.color_menu,
						XmNsubMenuId, 
						main_widget.mixer_sel,
						XmNlabelString,str,
						NULL);
    XmStringFree(str);

    str = NewString("Edit RGB");
    main_widget.mixerrgb = XtVaCreateManagedWidget("_MixerRGB",
						 xmPushButtonWidgetClass,
						 main_widget.mixer_sel,
						   XmNlabelString,str,
						   NULL);
    XmStringFree(str);
    XtAddCallback(main_widget.mixerrgb ,XmNactivateCallback,
		  mixer_callback,NULL);

    str = NewString("Edit HSL");
   main_widget.mixerhsl = XtVaCreateManagedWidget("_MixerHSL",
						 xmPushButtonWidgetClass,
						 main_widget.mixer_sel,
						  XmNlabelString,str,
						  NULL);
    XmStringFree(str);
    XtAddCallback(main_widget.mixerhsl ,XmNactivateCallback,mixer_callback,
		  main_widget.mixerhsl);

/*******************/
   main_widget.cotable_sel=XmCreatePulldownMenu(main_widget.color_menu,
						"_pulldown",NULL,0);

    str = NewString("New Table");
    main_widget.readtab = XtVaCreateManagedWidget("ReadColorTable",
						  xmCascadeButtonWidgetClass,
						  main_widget.color_menu,
						  XmNsubMenuId, 
						  main_widget.cotable_sel,
						  XmNlabelString,str,
						  NULL);
    XmStringFree(str);

    ivectblib=(char *)getenv("IVECTB");
    if(!ivectblib)ivectblib=(char *)getenv("IVECTBLIB");
    if(!ivectblib)ivectblib=(char *)getenv("ivectblib");
    if(!ivectblib)ivectblib=LIBDIR;
    
    str = NewString("Default\nTable");
    main_widget.default_table = XtVaCreateManagedWidget("_DefaultTable",
						      xmPushButtonWidgetClass,
						      main_widget.cotable_sel,
						      XmNlabelString,str,
						      NULL);
    sprintf(ctb, DEFCOLORTAB, ivectblib);
    XtAddCallback(main_widget.default_table ,XmNactivateCallback,call_driver,
		  (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("Dark/Light\nTable");
    main_widget.DL_table = XtVaCreateManagedWidget("_DlTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, DLCOLORTAB, ivectblib);
    XtAddCallback(main_widget.DL_table ,XmNactivateCallback,call_driver,
		  (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("Light/Dark\nTable");
    main_widget.LD_table = XtVaCreateManagedWidget("_LdTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, LDCOLORTAB, ivectblib);
    XtAddCallback(main_widget.LD_table ,XmNactivateCallback,call_driver,
		   (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("Blue/Red\nTable");
    main_widget.BR_table = XtVaCreateManagedWidget("_BrTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, BRCOLORTAB, ivectblib);
    XtAddCallback(main_widget.BR_table ,XmNactivateCallback,call_driver,
		  (XtPointer)  strdup(ctb));
    XmStringFree(str);

    str = NewString("Green/Blue/Red\nTable");
    main_widget.GBR_table = XtVaCreateManagedWidget("_GbrTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, GBRCOLORTAB, ivectblib);
    XtAddCallback(main_widget.GBR_table ,XmNactivateCallback,call_driver,
		   (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("Movie Blue/Red\nTable");
    main_widget.MBR_table = XtVaCreateManagedWidget("_MbrTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, MBRCOLORTAB, ivectblib);
    XtAddCallback(main_widget.MBR_table ,XmNactivateCallback,call_driver,
		   (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("Rainbow\nTable");
    main_widget.RBW_table = XtVaCreateManagedWidget("_RbwTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, RBWCOLORTAB, ivectblib);
    XtAddCallback(main_widget.RBW_table ,XmNactivateCallback,call_driver,
		   (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("White/Red\nTable");
    main_widget.WR_table = XtVaCreateManagedWidget("_WRTable",
						   xmPushButtonWidgetClass,
						   main_widget.cotable_sel,
						   XmNlabelString,str,
						   NULL);
    sprintf(ctb, WRCOLORTAB, ivectblib);
    XtAddCallback(main_widget.WR_table ,XmNactivateCallback,call_driver,
		   (XtPointer) strdup(ctb));
    XmStringFree(str);

    str = NewString("Custom\nTable");
    main_widget.custom_table = XtVaCreateManagedWidget("_CustomTable",
						       xmPushButtonWidgetClass,
						       main_widget.cotable_sel,
						       XmNlabelString,str,
						       NULL);
    XtAddCallback(main_widget.custom_table ,XmNactivateCallback,init_ctb,NULL);
    XmStringFree(str);

/*******************/

    str = NewString("Write Table");
    main_widget.write_table = XtVaCreateManagedWidget("Write Table",
						      xmPushButtonWidgetClass,
						      main_widget.color_menu,
						      XmNlabelString,str,
						      NULL);
    XmStringFree(str);

    XtAddCallback(main_widget.write_table ,XmNactivateCallback,init_wctb,NULL);

/*******************/
    str = NewString("Shift Colors");
    main_widget.color_opts = XtVaCreateManagedWidget("SlideLockToggle",
						     xmPushButtonWidgetClass,
						     main_widget.color_menu,
						     XmNlabelString,str,
						     NULL);
    XtAddCallback(main_widget.color_opts ,XmNactivateCallback,init_color,NULL);
    XmStringFree(str);
    /*end of color menue */

/******Line Attribute Menu********/
    main_widget.line_menu=XmCreatePulldownMenu(main_widget.menu,"CustomLines",
					       NULL,0);

    str = NewString("Custom\nLines");
    main_widget.line_att = XtVaCreateManagedWidget("CustomLines",
						   xmCascadeButtonWidgetClass,
						   main_widget.menu,
						   XmNsubMenuId, 
						   main_widget.line_menu,
						   XmNlabelString,str,
						   NULL);
    XmStringFree(str);

/*line color*/
    main_widget.colorsel=XmCreatePulldownMenu(Box,"Linecolor",NULL,0);

    str = XmStringCreateSimple("Color");
    main_widget.linecolor_menu = XtVaCreateManagedWidget("Linecolor",
						    xmCascadeButtonWidgetClass,
							 main_widget.line_menu,
							 XmNsubMenuId, 
							 main_widget.colorsel,
							 XmNlabelString, str,
							 NULL);
    XmStringFree(str);

    main_widget.colorhi=XmCreatePulldownMenu(main_widget.colorsel,
					     "_pulldown",NULL,0);
    main_widget.colorlo=XmCreatePulldownMenu(main_widget.colorsel,
					     "_pulldown",NULL,0);
    main_widget.colorb=XmCreatePulldownMenu(main_widget.colorsel,
					    "_pulldown",NULL,0);

    str = NewString("Both");
    main_widget.cbo = XtVaCreateManagedWidget("_Both",
					      xmCascadeButtonWidgetClass,
					      main_widget.colorsel,
					      XmNsubMenuId, main_widget.colorb,
					      XmNlabelString,str,
					      NULL);
    XmStringFree(str);

    str = NewString("Pos");
    main_widget.chi = XtVaCreateManagedWidget("_Pos",
					      xmCascadeButtonWidgetClass,
					      main_widget.colorsel,
					      XmNsubMenuId,main_widget.colorhi,
					      XmNlabelString,str,
					      NULL);
    XmStringFree(str);

    str = NewString("Neg");
    main_widget.clo = XtVaCreateManagedWidget("_Neg",
					      xmCascadeButtonWidgetClass,
					      main_widget.colorsel,
					      XmNsubMenuId,main_widget.colorlo,
					      XmNlabelString,str,
					      NULL);
    XmStringFree(str);
    str = NewString("Default");
    main_widget.cdef = XtVaCreateManagedWidget("_Default",
					       xmCascadeButtonWidgetClass,
					       main_widget.colorsel,
					       XmNlabelString,str,
					       NULL);
    XmStringFree(str);

/***********/
    redp=IveGetPixel(3);
    bluep=IveGetPixel(7);
    greenp=IveGetPixel(6);
    blackp=IveGetPixel(9);
    whitep=IveGetPixel(2);
    violetp=IveGetPixel(8);
    brownp=IveGetPixel(4);
    yellowp=IveGetPixel(5);
	greyp=IveGetPixel(12);

    str = NewString(" ");
    main_widget.red =  XtVaCreateManagedWidget("Red",xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, redp, NULL);

    main_widget.blue =  XtVaCreateManagedWidget("Blue",xmPushButtonWidgetClass,
					      main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, bluep, NULL);

    main_widget.green =  XtVaCreateManagedWidget("Green",
						 xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, greenp, NULL);

    main_widget.violet =  XtVaCreateManagedWidget("Violet",
						xmPushButtonWidgetClass,
						main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, violetp, NULL);

    main_widget.brown =  XtVaCreateManagedWidget("Brown",
						 xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, brownp, NULL);

    main_widget.black =  XtVaCreateManagedWidget("Black",
						 xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, blackp, NULL);

    main_widget.white =  XtVaCreateManagedWidget("White",
						 xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, whitep, NULL);

    main_widget.grey =  XtVaCreateManagedWidget("Grey",
						 xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, greyp, NULL);

    main_widget.yellow =  XtVaCreateManagedWidget("Yellow",
						 xmPushButtonWidgetClass,
					       main_widget.colorb,
					       /*XmNlabelString,str,*/
				     XmNforeground, yellowp, NULL);
/***********/

    main_widget.redhi =  XtVaCreateManagedWidget("Red",xmPushButtonWidgetClass,
						 main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, redp, NULL);

    main_widget.bluehi =  XtVaCreateManagedWidget("Blue",
						  xmPushButtonWidgetClass,
						  main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, bluep, NULL);

    main_widget.greenhi =  XtVaCreateManagedWidget("Green",
						 xmPushButtonWidgetClass,
						 main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, greenp, NULL);


    main_widget.violethi =  XtVaCreateManagedWidget("Violet",
						  xmPushButtonWidgetClass,
						  main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, violetp, NULL);

    main_widget.brownhi =  XtVaCreateManagedWidget("Brown",
						 xmPushButtonWidgetClass,
						 main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, brownp, NULL);

    main_widget.blackhi =  XtVaCreateManagedWidget("Black",
						 xmPushButtonWidgetClass,
						 main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, blackp, NULL);

    main_widget.whitehi =  XtVaCreateManagedWidget("White",
						 xmPushButtonWidgetClass,
						 main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, whitep, NULL);

    main_widget.greyhi =  XtVaCreateManagedWidget("Grey",
						 xmPushButtonWidgetClass,
					       main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, greyp, NULL);

    main_widget.yellowhi =  XtVaCreateManagedWidget("Yellow",
						 xmPushButtonWidgetClass,
					       main_widget.colorhi,
					       /*XmNlabelString,str,*/
				     XmNforeground, yellowp, NULL);

/***********/

   main_widget. redlo =  XtVaCreateManagedWidget("Red",xmPushButtonWidgetClass,
					       main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, redp, NULL);

  main_widget. bluelo =  XtVaCreateManagedWidget("Blue",
						  xmPushButtonWidgetClass,
						main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, bluep, NULL);

    main_widget.greenlo =  XtVaCreateManagedWidget("Green",
						 xmPushButtonWidgetClass,
						 main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, greenp, NULL);

    main_widget.violetlo =  XtVaCreateManagedWidget("Violet",
						  xmPushButtonWidgetClass,
						  main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, violetp, NULL);

    main_widget.brownlo =  XtVaCreateManagedWidget("Brown",
						 xmPushButtonWidgetClass,
						 main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, brownp, NULL);

    main_widget.blacklo =  XtVaCreateManagedWidget("Black",
						 xmPushButtonWidgetClass,
						 main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, blackp, NULL);

    main_widget.whitelo =  XtVaCreateManagedWidget("white",
						 xmPushButtonWidgetClass,
						 main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, whitep, NULL);

    main_widget.greylo =  XtVaCreateManagedWidget("Grey",
						 xmPushButtonWidgetClass,
					       main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, greyp, NULL);

   main_widget.yellowlo =  XtVaCreateManagedWidget("Yellow",
						 xmPushButtonWidgetClass,
					       main_widget.colorlo,
					       /*XmNlabelString,str,*/
				     XmNforeground, yellowp, NULL);

    XmStringFree(str);
/***********************************/

    XtAddCallback(main_widget.cdef,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=DEFAULT,");

    XtAddCallback(main_widget.white,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=WHITE,WHITE");
    XtAddCallback(main_widget.grey,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=GREY,GREY");
    XtAddCallback(main_widget.red,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=RED,RED");
    XtAddCallback(main_widget.brown,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=BROWN,BROWN");
    XtAddCallback(main_widget.green,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=GREEN,GREEN");
    XtAddCallback(main_widget.blue,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=BLUE,BLUE");
    XtAddCallback(main_widget.violet,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=VIOLET,VIOLET");
    XtAddCallback(main_widget.black,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=BLACK,BLACK");
    XtAddCallback(main_widget.yellow,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=YELLOW,YELLOW");

    
   XtAddCallback(main_widget.whitehi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=WHITE,");
   XtAddCallback(main_widget.greyhi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=GREY,");
   XtAddCallback(main_widget.brownhi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=BROWN,");
   XtAddCallback(main_widget.greenhi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=GREEN,");
   XtAddCallback(main_widget.bluehi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=BLUE,");
   XtAddCallback(main_widget.violethi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=VIOLET,");
   XtAddCallback(main_widget.blackhi,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=BLACK,");
    XtAddCallback(main_widget.yellowhi,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=YELLOW,");

   XtAddCallback(main_widget.whitelo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,WHITE");
   XtAddCallback(main_widget.greylo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,GREY");
   XtAddCallback(main_widget.redlo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,RED");
   XtAddCallback(main_widget.brownlo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,BROWN");
   XtAddCallback(main_widget.greenlo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,GREEN");
   XtAddCallback(main_widget.bluelo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,BLUE");
   XtAddCallback(main_widget.violetlo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,VIOLET");
   XtAddCallback(main_widget.blacklo,XmNactivateCallback,call_driver_scv,
		 "LINECOLOR=,BLACK");
    XtAddCallback(main_widget.yellowlo,XmNactivateCallback,call_driver_scv,
		  "LINECOLOR=,YELLOW");
/********************/


    main_widget.linepattern=XmCreatePulldownMenu(Box,"linepattern",NULL,0);
    
    str = XmStringCreateSimple("Pattern");
    main_widget.patternsel = XtVaCreateManagedWidget("LinePattern",
						    xmCascadeButtonWidgetClass,
						     main_widget.line_menu,
						     XmNsubMenuId, 
						     main_widget.linepattern,
						     XmNlabelString, str,
						     NULL);
    XmStringFree(str);

    main_widget.patternhi=XmCreatePulldownMenu(main_widget.patternsel,
					       "_pulldown",NULL,0);
   main_widget.patternlo=XmCreatePulldownMenu(main_widget.patternsel,
					      "_pulldown",NULL,0);
    main_widget.patternb=XmCreatePulldownMenu(main_widget.patternsel,
					      "_pulldown",NULL,0);

    str=NewString("Both");
    main_widget.patb = XtVaCreateManagedWidget("_Both",
					       xmCascadeButtonWidgetClass,
					       main_widget.linepattern,
					       XmNsubMenuId, 
					       main_widget.patternb,
					       XmNlabelString, str,
					       NULL);
    XmStringFree(str);

    str=NewString("Pos");
    main_widget.pathi = XtVaCreateManagedWidget("_Pos",
						xmCascadeButtonWidgetClass,
						main_widget.linepattern,
						XmNsubMenuId, 
      						main_widget.patternhi,
						XmNlabelString, str,
						NULL);
    XmStringFree(str);

    str=NewString("Neg");
    main_widget.patlo = XtVaCreateManagedWidget("_Neg",
						xmCascadeButtonWidgetClass,
						main_widget.linepattern,
						XmNsubMenuId, 
						main_widget.patternlo,
						XmNlabelString, str,
						NULL);
    XmStringFree(str);

    str=NewString("Default");
    main_widget.patdef = XtVaCreateManagedWidget("_Default",
						 xmCascadeButtonWidgetClass,
						 main_widget.linepattern,
						 XmNlabelString, str,
						 NULL);
    XmStringFree(str);

    str=NewString("Solid");
    main_widget.line_solid=XtVaCreateManagedWidget("_Solid",
						   xmCascadeButtonWidgetClass,
						   main_widget. patternb,
						   XmNlabelString, str,
						   NULL);
    XmStringFree(str);
    
    str=NewString("Dash");
    main_widget.dash = XtVaCreateManagedWidget("_Dash",
					       xmCascadeButtonWidgetClass
					       ,main_widget.patternb,
					       XmNlabelString, str,
					       NULL);
    XmStringFree(str);

    str=NewString("Dot");
    main_widget.dot = XtVaCreateManagedWidget("_Dot",
					      xmCascadeButtonWidgetClass,
					      main_widget.patternb,
					      XmNlabelString, str,
					      NULL);
    XmStringFree(str);

    str=NewString("DashDot");
    main_widget.dashdot = XtVaCreateManagedWidget("_DashDot",
						  xmCascadeButtonWidgetClass,
						  main_widget.patternb,
						  XmNlabelString, str,
						  NULL);
    XmStringFree(str);

    str=NewString("DashDotDot");
    main_widget.dashdotdot = XtVaCreateManagedWidget("_DashDotDot",
						    xmCascadeButtonWidgetClass,
						      main_widget.patternb,
						      XmNlabelString, str,
						      NULL);
    XmStringFree(str);
 
    str=NewString("LongDashShort");
     main_widget.longdash_short = XtVaCreateManagedWidget("_LongDash Short",
						  xmCascadeButtonWidgetClass,
							  main_widget.patternb,
							  XmNlabelString, str,
							  NULL);
    XmStringFree(str);
 
    str=NewString("LongDash");
    main_widget.longdash = XtVaCreateManagedWidget("_Longdash",
                                               xmCascadeButtonWidgetClass,
						   main_widget.patternb,
						   XmNlabelString, str,
						   NULL);
    XmStringFree(str);
 
     /***************** end of both pattern menu ****************/
 
    str=NewString("Solid");
    main_widget.line_solidhi=XtVaCreateManagedWidget("_Solid",
						   xmCascadeButtonWidgetClass,
						   main_widget. patternhi,
						   XmNlabelString,str,
						     NULL);
    XmStringFree(str);
    
    str=NewString("Dash");
    main_widget.dashhi = XtVaCreateManagedWidget("_Dash",
					       xmCascadeButtonWidgetClass,
						 main_widget.patternhi,
						 XmNlabelString,str,
						 NULL);
    XmStringFree(str);
    
    str=NewString("Dot");
    main_widget.dothi = XtVaCreateManagedWidget("_Dot",
						xmCascadeButtonWidgetClass
						,main_widget.patternhi,
						XmNlabelString,str,
						NULL);
    XmStringFree(str);
    
    str=NewString("DashDot");
    main_widget.dashdothi = XtVaCreateManagedWidget("_DashDot",
						    xmCascadeButtonWidgetClass,
						    main_widget.patternhi,
						    XmNlabelString,str,
						    NULL);
    
    XmStringFree(str);
    
    str=NewString("DashDotDot");
    main_widget.dashdotdothi = XtVaCreateManagedWidget("_DashDotDot",
						    xmCascadeButtonWidgetClass,
							main_widget.patternhi,
						       XmNlabelString,str,
							NULL);
    XmStringFree(str);
    
    str=NewString("LongDashShort");
    main_widget.longdash_shorthi = XtVaCreateManagedWidget("_LongDashShort",
						    xmCascadeButtonWidgetClass,
							 main_widget.patternhi,
							   XmNlabelString,str,
							    NULL);
    XmStringFree(str);
    
    str=NewString("LongDash");
    main_widget.longdashhi = XtVaCreateManagedWidget("_Longdash",
                                               xmCascadeButtonWidgetClass,
						      main_widget.patternhi,
						     XmNlabelString,str,
						      NULL);
    XmStringFree(str);
    /****************** end of High patternmenu ****************/
    
    str=NewString("Solid");
    main_widget.line_solidlo=XtVaCreateManagedWidget("_Solid",
						    xmCascadeButtonWidgetClass,
						     main_widget.patternlo,
						     XmNlabelString,str,
						     NULL);
    XmStringFree(str);
    
    str=NewString("Dash");
    main_widget.dashlo = XtVaCreateManagedWidget("_Dash",
						 xmCascadeButtonWidgetClass,
						 main_widget.patternlo,
						 XmNlabelString,str,
						 NULL);
    XmStringFree(str);
    
    str=NewString("dot");
    main_widget.dotlo = XtVaCreateManagedWidget("_Dot",
						xmCascadeButtonWidgetClass,
						main_widget.patternlo,
						XmNlabelString,str,
						NULL);
    XmStringFree(str);
    
    str=NewString("DashDot");
    main_widget.dashdotlo = XtVaCreateManagedWidget("_DashDot",
						    xmCascadeButtonWidgetClass,
						    main_widget.patternlo,
						    XmNlabelString,str,
						    NULL);
    XmStringFree(str);
    
    str=NewString("DashDotDot");
    main_widget.dashdotdotlo = XtVaCreateManagedWidget("_DashDotDot",
						    xmCascadeButtonWidgetClass,
						       main_widget.patternlo,
						       XmNlabelString,str,
						       NULL);
    XmStringFree(str);
    
    str=NewString("LongDashShort");
    main_widget.longdash_shortlo = XtVaCreateManagedWidget("_LongDashShort",
						    xmCascadeButtonWidgetClass,
							 main_widget.patternlo,
							   XmNlabelString,str,
							    NULL);
    XmStringFree(str);
    
    str=NewString("LongDash");
    main_widget.longdashlo = XtVaCreateManagedWidget("_LongDash",
						    xmCascadeButtonWidgetClass,
						      main_widget.patternlo,
						     XmNlabelString,str,
						      NULL);
    XmStringFree(str);
 
 /****************** end of Low pattern menu ****************/

    XtAddCallback(main_widget.patdef,XmNactivateCallback,call_driver,
		  "linepattern=default");

    XtAddCallback(main_widget.line_solid,XmNactivateCallback,call_driver,
		  "linepattern=solid,solid");
    XtAddCallback(main_widget.dashdot,XmNactivateCallback,call_driver,
		  "linepattern=dashdot,dashdot");
    XtAddCallback(main_widget.dash,XmNactivateCallback,call_driver,
		  "linepattern=dash,dash");
    XtAddCallback(main_widget.dot,XmNactivateCallback,call_driver,
		  "linepattern=dot,dot");
     XtAddCallback(main_widget.dashdotdot,XmNactivateCallback,call_driver,
                 "linepattern=dashdotdot,dashdotdot");
     XtAddCallback(main_widget.longdash_short,XmNactivateCallback,call_driver,
                 "linepattern=longdashshort,longdashshort");
     XtAddCallback(main_widget.longdash,XmNactivateCallback,call_driver,
                 "linepattern=longdash,longdash");
    

    XtAddCallback(main_widget.line_solidhi,XmNactivateCallback,call_driver,
		  "linepattern=solid,");
    XtAddCallback(main_widget.dashdothi,XmNactivateCallback,call_driver,
		  "linepattern=dashdot,");
    XtAddCallback(main_widget.dashhi,XmNactivateCallback,call_driver,
		  "linepattern=dash,");
    XtAddCallback(main_widget.dothi,XmNactivateCallback,call_driver,
		  "linepattern=dot,");
     XtAddCallback(main_widget.dashdotdothi,XmNactivateCallback,call_driver,
                 "linepattern=dashdotdot,");
     XtAddCallback(main_widget.longdash_shorthi,XmNactivateCallback,call_driver,
                 "linepattern=longdashshort,");
     XtAddCallback(main_widget.longdashhi,XmNactivateCallback,call_driver,
                 "linepattern=longdash,");
    
    
    XtAddCallback(main_widget.line_solidlo,XmNactivateCallback,call_driver,
		  "linepattern=,solid");
    XtAddCallback(main_widget.dashdotlo,XmNactivateCallback,call_driver,
		  "linepattern=,dashdot");
    XtAddCallback(main_widget.dashlo,XmNactivateCallback,call_driver,
		  "linepattern=,dash");
    XtAddCallback(main_widget.dotlo,XmNactivateCallback,call_driver,
		  "linepattern=,dot");
     XtAddCallback(main_widget.dashdotdotlo,XmNactivateCallback,call_driver,
                 "linepattern=,dashdotdot");
     XtAddCallback(main_widget.longdash_shortlo,XmNactivateCallback,call_driver,
                 "linepattern=,longdashshort");
     XtAddCallback(main_widget.longdashlo,XmNactivateCallback,call_driver,
                 "linepattern=,longdash");

/**********end pattern****************/

    main_widget.linewidth=XmCreatePulldownMenu(Box,"LineWidth",NULL,0);
    
    str = NewString("Width");
    main_widget.widthsel = XtVaCreateManagedWidget("linewidth",
						   xmCascadeButtonWidgetClass,
						   main_widget.line_menu,
						   XmNsubMenuId, 
						   main_widget.linewidth,
						   XmNlabelString, str, 
						   NULL);
    XmStringFree(str);


    str=NewString("Normal");
    main_widget.one =  
      XtVaCreateManagedWidget("_Normal",xmCascadeButtonWidgetClass,
			      main_widget.linewidth, XmNlabelString, str,
			      NULL);
    XmStringFree(str);

    str=NewString("Thick");
    main_widget.two =  
      XtVaCreateManagedWidget("_Thick",xmCascadeButtonWidgetClass,
			      main_widget.linewidth, XmNlabelString, str,
			      NULL);
    XmStringFree(str);

    str=NewString("Thickest");
    main_widget.three =  
      XtVaCreateManagedWidget("_Thickest",xmCascadeButtonWidgetClass,
			      main_widget.linewidth, XmNlabelString, str,
			      NULL);
    XmStringFree(str);

    XtAddCallback(main_widget.one,XmNactivateCallback,
		  form_line_width_call,"1");
    XtAddCallback(main_widget.two,XmNactivateCallback,
		  form_line_width_call,"2");
    XtAddCallback(main_widget.three,XmNactivateCallback,
		  form_line_width_call,"3");

    /* show the rest problem*/

/**********************************************************************/
/*****************************END MENUS********************************/
    /*buttons*/


    /*first do button holder*/
    main_widget.row1 = XtVaCreateManagedWidget("_row1",xmRowColumnWidgetClass,
					       Box,
					       XmNy, 58,
					       XmNorientation,XmHORIZONTAL,
					       XmNpacking,XmPACK_COLUMN,
					       NULL);
    /*row1*/
    str = NewString("\n  New\n Plot\n");
    main_widget.plot = XtVaCreateManagedWidget("Plot",
					       xmPushButtonWidgetClass,
					       main_widget.row1,
					       XmNlabelString,str,
					       XmNmarginHeight,8,
					       XmNmarginWidth,14,
					       XmNx, 5,
					       NULL);
    XtAddCallback(main_widget.plot ,XmNactivateCallback,call_driver,
		  "new_plot");
    XmStringFree(str);
    
    str = NewString("\nOverlay\n Plot\n");
    main_widget.overlay = XtVaCreateManagedWidget("Overlay_Plot",
						  xmPushButtonWidgetClass,
						  main_widget.row1,
						  XmNlabelString,str,
						  XmNmarginHeight,8,
						  XmNmarginWidth,14,
						  NULL);
    XtAddCallback(main_widget.overlay ,XmNactivateCallback,call_driver,
		  "overlay_plot");
    XmStringFree(str);

    str = NewString("\n Lift\nOverlay\n");
    main_widget.lift_overlay = XtVaCreateManagedWidget("Lift_Overlay",
						       xmPushButtonWidgetClass,
						       main_widget.row1,
						       XmNlabelString,str,
						       XmNmarginHeight,8,
						       XmNmarginWidth,14,
						       NULL);
    XtAddCallback(main_widget.lift_overlay ,XmNactivateCallback,call_driver,
		  "lift_overlay");
    XmStringFree(str);



    /*row2*/

    main_widget.row2 = XtVaCreateManagedWidget("_row2",xmRowColumnWidgetClass,
					       Box,
					       XmNy, 120,
					       XmNorientation,XmHORIZONTAL,
					       XmNpacking,XmPACK_TIGHT,
					       XmNmarginHeight,0,
					       XmNtopAttachment,XmATTACH_WIDGET,
					       XmNtopWidget, main_widget.row1,
					       NULL);
    
    /*type menu*/
      
    main_widget.type = XmCreatePulldownMenu(main_widget.row2,
					    "Type",NULL,0);

    main_widget.type_menu = XmCreateOptionMenu(main_widget.row2,"Type",
						 NULL,0);
    XtVaSetValues(main_widget.type_menu,
		  XmNsubMenuId,  main_widget.type,
		  XmNmarginTop,0,
		  XmNmarginBottom,0,
		  XmNmarginHeight,0,NULL);
    XtVaSetValues(main_widget.type,
		  XmNmarginHeight,0,
		  XmNmarginTop,0,
		  XmNmarginBottom,0,
		  NULL);
    XtUnmanageChild(XmOptionLabelGadget(main_widget.type_menu));

    str = NewString("Line\nPlot");
    main_widget.Line = XtVaCreateManagedWidget("Line\nPlot",
					       xmPushButtonWidgetClass,
					       main_widget.type,
					       XmNlabelString,str,
						 XmNmarginTop,0,
						 XmNmarginBottom,0,
					       NULL);
    XtAddCallback(main_widget.Line ,XmNactivateCallback,call_driver,line);
    XmStringFree(str);

    str = NewString("Skew T\nPlot");
    main_widget.Skewt = XtVaCreateManagedWidget("Skew T\nPlot",
						 xmPushButtonWidgetClass,
						 main_widget.type,
						 XmNmarginTop,0,
						 XmNmarginBottom,0,
						 XmNlabelString,str,
						 NULL);
    XtAddCallback(main_widget.Skewt ,XmNactivateCallback,call_driver,skewt);
    XmStringFree(str);

    str = NewString("Scalar\nPlot");
    main_widget.Scalar = XtVaCreateManagedWidget("_ScalarPlot",
						 xmPushButtonWidgetClass,
						 main_widget.type,
						 XmNlabelString,str,
						 XmNmarginTop,0,
						 XmNmarginBottom,0,
						 NULL);
    XtAddCallback(main_widget.Scalar ,XmNactivateCallback,call_driver,scalar);
    XmStringFree(str);

    str = NewString("Vector\nPlot");
    main_widget.Vector = XtVaCreateManagedWidget("_VectorPlot",
						 xmPushButtonWidgetClass,
						 main_widget.type,
						 XmNlabelString,str,
						 XmNmarginTop,0,
						 XmNmarginBottom,0,
						 NULL);
    XtAddCallback(main_widget.Vector ,XmNactivateCallback,call_driver,vector);
    XmStringFree(str);

    str = NewString("Trajectory\nPlot");
    main_widget.Trajectory = XtVaCreateManagedWidget("Trajectory\nPlot",
			 			 xmPushButtonWidgetClass,
				 		 main_widget.type,
					 	 XmNlabelString,str,
						 XmNmarginTop,0,
						 XmNmarginBottom,0,
						     NULL);
    XtAddCallback(main_widget.Trajectory ,XmNactivateCallback,call_driver,trajectory);
    XmStringFree(str); 
    

    str = NewString("3D\nPlot");
    main_widget.Surface = XtVaCreateManagedWidget("_surfacePlot",
						 xmPushButtonWidgetClass,
						 main_widget.type,
						 XmNlabelString,str,
						 XmNmarginTop,0,
						 XmNmarginBottom,0,
						 NULL);
    XtAddCallback(main_widget.Surface ,XmNactivateCallback,call_driver,surface);
    XmStringFree(str);

    XtManageChild(main_widget.type_menu);

    /*end of type menue */
    

    str = NewString("Build\nLoop");
    main_widget.loop = XtVaCreateManagedWidget("Loop",
					       xmPushButtonWidgetClass,
					       main_widget.row2,
					       XmNlabelString,str,
					       NULL);

    XmStringFree(str);
    XtAddCallback(main_widget.loop ,XmNactivateCallback,init_loop,NULL);
    


    main_widget.help = XtCreateManagedWidget("Help",
					     xmPushButtonWidgetClass,
					     main_widget.row2,NULL, 0);
    XtAddCallback(main_widget.help ,XmNactivateCallback,call_driver,"help");

    str = NewString("Save\nWindow");
    main_widget.save_win = XtVaCreateManagedWidget("Save_Window",
						   xmPushButtonWidgetClass,
						   main_widget.row2,
						   XmNlabelString, str, NULL);
    XmStringFree(str);
    XtAddCallback(main_widget.save_win ,XmNactivateCallback,call_driver,
		  "save_window");
    main_widget.quit = XtVaCreateManagedWidget("Quit",
					       xmPushButtonWidgetClass,
					       main_widget.row2,NULL);
    XtAddCallback(main_widget.quit, XmNactivateCallback, quit_ive,NULL);

    /*end of row2*/				     


    /*add the command option at the bottom*/


    main_widget.command = XtVaCreateManagedWidget("Commands",
						  xmTextFieldWidgetClass,
						  Box,
						  /* XmNy,170,*/
					       XmNtopAttachment,XmATTACH_WIDGET,
						  XmNtopWidget, main_widget.row2,
						  XmNwidth,BOXWIDTH - 20,
						  NULL);
    XtAddCallback(main_widget.command,XmNactivateCallback,command_callback,
		  NULL);
    XtAddCallback(main_widget.command,XmNmodifyVerifyCallback,
		  command_callback_check,NULL);

    /* fields*/
    main_widget.field_scroll = XtVaCreateManagedWidget("scrolled",
						       xmScrolledWindowWidgetClass,
						       Box,
						       XmNscrollingPolicy,XmAUTOMATIC,
						       XmNtopAttachment,XmATTACH_WIDGET,
						       XmNtopWidget, 
						       main_widget.command,
						       XmNwidth,BOXWIDTH -15,
						       XmNheight,70,NULL);
    str = NewString("Variables:");
    main_widget.field_holder = XtVaCreateManagedWidget("field",
						       xmRowColumnWidgetClass,
						       main_widget.field_scroll,
						       XmNorientation,
						       XmHORIZONTAL,
						       XmNwidth,BOXWIDTH -25,
						       /*XmNy,200,*/
						       /*XmNtopAttachment,XmATTACH_WIDGET,
						       XmNtopWidget, 
						       main_widget.command,*/
						       XmNresizeWidth,False,
						       XmNresizeHeight,True,
						       XmNcolumns,20,
						       XmNlabelString,str,
						       XmNradioBehavior,TRUE,
						       XmNpacking,XmPACK_TIGHT,
						       NULL);
    XtVaGetValues(main_widget.field_scroll,XmNhorizontalScrollBar, &wtemp,NULL);
    XtUnmanageChild(wtemp);
    XmStringFree(str);
    make_field_buttons(NULL,main_widget.field_holder);
    


    XtManageChild(main_widget.menu);

    XtAddEventHandler(main_widget.command,KeyPressMask|KeyReleaseMask,
		      FALSE,DoComHist,(Opaque)NULL);

    XtAddCallback(main_widget.load_file ,XmNactivateCallback,init_dir,NULL);

    return(Box);
}

