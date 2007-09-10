      subroutine setdef
c-----------------------------------------------------------------------
c     $Id: setdef.F,v 1.51 2007-06-21 20:58:31 reinecke Exp $
c     Purpose:
c        This subroutine sets initial default values for local
c        variables. Default values are assigned in deflts.icl.
c
c     **************NOTE:*************
c        The default value of each command MUST be placed in setdef.f!
c
c     Arguments:
c        None
c     History:
c 	$Log: setdef.F,v $
c 	Revision 1.51  2007-06-21 20:58:31  reinecke
c 	Commiting local modifications for transfer to svn
c
c 	Revision 1.50  2005/09/06 20:22:15  warren
c 	Move updates into current stream
c 	
c 	Revision 1.49.2.1  2005/09/06 17:55:23  warren
c 	fixed trajectories to deal with maps. Also first part of bold arrows for
c 	trajectories
c 	
c 	Revision 1.49  2003/04/02 22:25:24  warren
c 	don't plot iaia(0) as this is under the mountain
c 	
c 	Revision 1.48  2002/12/24 21:30:48  harryive
c 	Must have call to vvumxy so our version of this routine is loaded
c 	instead of the NCAR version.
c 	
c 	Revision 1.47  2002/12/09 18:04:45  warren
c 	Fix writecolortable and some of the color init commands
c 	
c 	Revision 1.46  2002/11/27 00:31:29  warren
c 	Removed old inits that are no longer in NCAR and netcdf.
c 	
c 	Revision 1.45  2002/06/11 17:26:17  harryive
c 	Change aspect ratio to affect the plot, not just printing.
c 	
c 	Add first part of contour value specification.
c 	
c 	Revision 1.44  2002/05/22 17:56:46  davidive
c 	First working version of trajectories colored by alternative fields
c 	There is not a complete interface yet!!!
c 	
c 	Revision 1.43  2001/11/20 23:29:56  davidive
c 	Track if step and mid val are set by ive or a person. Also initialize all
c 	variables for no depth.
c 	
c 	Revision 1.42  2001/11/15 21:38:08  davidive
c 	added control over the step size and color matching for trajectories, as well
c 	as color bar expansion on overlays
c 	
c 	Revision 1.41  2001/08/15 22:25:48  davidive
c 	Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
c 	Added code to set up which axis to use and to convert the labels in 1d plots.
c 	
c 	Revision 1.40  2001/01/23 21:42:47  davidive
c 	added set zero_color flag to read_color calls.
c
c 	Revision 1.39  2001/01/06 00:00:28  harry
c 	The TYPE command should be done first in creating a plot.
c
c 	Revision 1.38  2001/01/05 19:23:42  davidive
c 	Spelled ou default for a few commands to get rid of annoying info message.
c
c 	Revision 1.37  2000/05/09 19:54:00  harry
c 	Seperate contour and line colors.  Make patterns work like colors.
c
c 	Revision 1.36  1999/12/22 00:33:22  warren
c 	Fixed Dales crashing bug when changing from point slicer to normal. Also
c 	have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c 	Revision 1.35  1999/07/23 20:03:39  warren
c 	Background code for the lock all fields option.
c
c 	Revision 1.34  1999/02/04 00:28:30  harry
c 	Add vector scaling.
c
c 	Revision 1.33  1999/01/29 20:46:33  warren
c 	ets the flag for time independence and the plot the trajectories
c
c 	Revision 1.32  1998/12/31 16:01:43  warren
c 	First cut of new trajectory look and feel. Trajectories are no longer
c 	on the popup menu, just plot type.
c
c 	Revision 1.31  1998/06/11 21:53:25  harry
c 	Initialize pat6.
c
c 	Revision 1.30  1998/06/03 17:54:41  harry
c 	Fix tick mark labels.   Add new command COLOR_BAR.
c
c 	Revision 1.29  1998/03/04 19:14:06  davidive
c 	First cut at trajectories and trajectory widget using colors to show depth
c 	and arrows to show direction.
c
c 	Revision 1.28  1997/07/11 14:07:04  davidive
c 	Fix color fill when there are no values or interval set to start from the
c 	middle of the color table again.
c
c 	Revision 1.27  1997/06/03 19:14:23  davidive
c 	The big mouse slicing changeover. We have the new form, only requires you
c 	set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c 	always runs through init_points_ to catch all these things.
c 	note: only actually works when two dimentions have points set. Needs to
c 	be expanded into 1, 3 or 4.
c
c 	Revision 1.26  1997/04/25 20:20:38  warren
c 	Keep a stack of the last 10 windows used for plotting in order to implement
c 	the window=up command.
c
c 	Revision 1.25  1997/03/14 22:26:34  warren
c 	Added new command map_window to allow for mouse windowing on non
c 	equadistant or default maps.
c
c 	Revision 1.24  1997/02/12 17:36:31  warren
c 	Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c 	Revision 1.23  1997/02/07 20:16:59  harry
c 	Yet more MULTIPLE_READ changes.
c
c 	Revision 1.22  1997/02/07 18:39:10  harry
c 	Remove PARTIAL_READ and expand MULTIPLE_READ.
c
c 	Revision 1.21  1997/01/31 18:21:18  warren
c 	Added print_command command for lp or lpr
c
c 	Revision 1.20  1997/01/17 20:38:19  harry
c 	First changes for MULTIPLE_READ and PARTIAL_READ.
c
c 	Revision 1.19  1996/12/23 20:58:14  warren
c 	Landscape printing option
c
c 	Revision 1.18  1996/11/07 16:55:37  warren
c 	Fixed to deal with the case of use_buttons == 0
c
c 	Revision 1.17  1996/10/18 23:21:33  harry
c 	Set color locking variables to defaults.
c
c Revision 1.16  1996/10/11  01:13:05  warren
c To fill or not to fill is no longer the only question. Now there is a seperate
c question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c Labels are not fixed yet.
c
c 	Revision 1.15  1996/06/06 20:56:40  harry
c 	Use IVECTBLIB when looking for the default color table.
c
c 	Revision 1.14  1996/04/25 16:25:26  harry
c 	Add Skew-T plots and the beginning of trajectory plots.
c
c 	Revision 1.13  1996/04/08 18:57:14  warren
c 	first parts of num unlabeled lines for terrain and fixed transform routine
c 	to use last directory if none is given.
c
c 	Revision 1.12  1996/04/01 20:23:44  warren
c 	Terrain form and new terrain command (on / off)
c
c 	Revision 1.11  1995/12/15 22:33:22  warren
c 	moved patterns into 4.0 and implemented the list of fixes and changes from
c 	Curtis.
c
c Revision 1.10  1995/10/18  19:18:21  warren
c Stipples from 3.3, First crack at mouse windowing and put window_points_
c into a header file and out of the mouse stuff and slicers. Touched almost
c everything it seems.
c
c Revision 1.9  1995/09/15  16:48:27  harry
c Add support for timeout of warning messages.
c
c Revision 1.8  1995/06/23  21:16:29  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.7  1995/05/16  16:01:20  harry
c How did IVE ever work with a bad XGKS font value of -101?
c
c Revision 1.6  1995/05/11  22:40:22  harry
c Add new command WRITE_1D.
c
c Revision 1.5  1995/01/09  18:52:43  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.4  1994/11/22  22:48:01  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.3  1994/10/08  19:01:36  warren
c The annotation widget and updating it. As well as changing tic_bold to
c tic_labels and adding an off option.
c
c Revision 1.2  1994/09/14  18:20:05  harry
c Big 1D flip uncommit.
c
c Revision 1.30  1994/06/28  17:43:37  warren
c First cut at fancy tic marks.
c
c Revision 1.29  1994/06/14  05:45:39  harry
c Mods fo Solaris 2.
c
c Revision 1.28  1994/05/06  17:10:12  yuri
c Main vector lock commit + small correction in X/ive_ui_update.c file
c
c Revision 1.27  1994/04/19  23:03:21  yuri
c Big 1D flip commit.
c
c Revision 1.26  1994/04/14  23:29:17  harry
c Make sure vector locking can only be turned on after a vector plot.
c Also fix the user interface updating for the vector_lock command.
c
c Revision 1.25  1994/04/14  01:14:55  warren
c Fixed problem where doing overlays in line plots changes the current line
c type for contours.
c
c Revision 1.24  1994/02/16  17:00:44  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.23  1994/01/29  00:11:41  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.22  1994/01/20  20:44:53  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.21  1993/12/28  21:00:53  harry
c Death to constfil!
c
c Revision 1.20  1993/12/15  18:15:43  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.19  1993/11/15  16:48:56  harry
c Add Fortran netCDF call to force linking of the Fortran netCDF jackets
c into IVE for use by the transforms.
c
c Revision 1.18  1993/11/12  23:31:16  harry
c Change the function code character in PLCHHQ from : to a non-printing
c character.
c
c Revision 1.17  1993/10/13  00:04:23  warren
c Update current version for fixes to 3.0 version related to the meaning of
c default in the property sheet boxes.
c
c Revision 1.16  1993/07/23  20:37:00  yuri
c 	Command "vector_lock" is introduced to lock vector
c scaling in a loop (for example). Also some minor bugs are corrected.
c
c Revision 1.15  1993/07/20  22:44:52  warren
c Added ability to set prints and eps files to draw color lines. Added
c ability to choose printer. Added ability to specify which spot on the
c page to print at.
c
c Revision 1.14  1993/07/19  20:41:23  yuri
c Corrected problem with control-c during contour plot.
c
c Revision 1.13  1993/07/09  16:32:34  harry
c Add defaults for new commands.
c
c Revision 1.12  1993/07/03  01:53:26  yuri
c Tuning of overlay colors.
c
c Revision 1.11  1993/07/01  19:35:02  yuri
c Corrected error in multiple overlay generating with mix
c vector/scalar plots.
c
c Revision 1.10  1993/06/24  22:14:27  yuri
c 	Corrected bugs in overlays scaling + cleaning up.
c
c Revision 1.9  1993/06/22  22:09:55  yuri
c 	New 1D and 2D slicers installed, added command interp_factors
c so user could choose number of interpolation points in slicing.
c Primarily it's for averaging, but user could use it every time
c before call slicers.
c
c Revision 1.6  1993/06/04  17:27:59  yuri
c Linecolor command overwrites current values in overlay colors table.
c It was not.
c
c Revision 1.5  1993/06/01  19:57:14  yuri
c Second try.
c
c Revision 1.4  1993/05/27  16:38:54  warren
c Recover files removed by yuri's commit
c
c Revision 1.2  1993/04/15  22:51:48  warren
c Changed default maplim to cl from df as easymap was complaining df does not
c exist.
c
c Revision 1.1.1.1  1993/04/07  19:40:22  harry
c Initial CVS checkin of IVE.
c
c Revision 2.16  1993/03/18  19:20:06  warren
c set default vector int to 3,3
c
c Revision 2.15  1993/02/01  23:45:02  warren
c added init_dim_atts
c
c Revision 2.14  1992/12/10  21:14:24  millerp
c Delete references to unused variables.
c
c Revision 2.13  1992/11/24  16:32:26  millerp
c Remove hslice, vslice.
c
c Revision 2.12  1992/11/10  16:22:07  harry
c Use parameter names for command numbers.
c
c Revision 2.11  1992/11/05  20:32:13  millerp
c Get key numbers via chkcmd, rather than hardwire them in.
c
c Revision 2.10  1992/10/28  22:40:10  millerp
c Remove references to variable AVERAGE.
c
c Revision 2.9  1992/10/06  18:26:45  warren
c added xtics_line and ytics_line and plot types scaler and vector
c
c Revision 2.8  1992/09/29  21:17:38  warren
c changed default foe topo_scale from 1 to 20 (%)
c
c Revision 2.7  1992/09/29  18:50:46  harry
c Add LOOP_QUIT.
c
c Revision 2.6  1992/09/24  23:15:57  harry
c Add defaults for DATA_UNITS and DOMAIN_UNITS.
c
c Revision 2.5  1992/09/24  17:55:29  harry
c Fix common block conflict between strmln.icl and vector.icl
c
c Revision 2.4  1992/09/24  17:44:04  harry
c Add more defaults.
c
c Revision 2.3  1992/09/23  23:12:47  harry
c Add calls to save_command for defaults (incomplete).
c
c Revision 2.2  1992/08/25  23:29:32  warren
c fixed color allocation problem and added fallback to smaller color table
c
c Revision 2.1  1992/08/24  20:37:36  millerp
c Do not do INQUIRE on color table files.
c
c Revision 2.0  1992/07/01  21:12:59  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/06/11  23:03:02  warren
c allow for ive env vars.
c
c Revision 1.2  1992/06/10  20:14:04  millerp
c Added defaults for slope and intercept variables.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
#ifndef LIBDIR
#define LIBDIR '/usr/local/lib/ive/'
#endif

#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif

#include "slices.icl" 

      include 'cmndlst.icl'
      include 'color.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'deflts.icl'
      include 'flags.icl'
      include 'lineplt.icl'
      include 'printer.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
      include 'segments.icl'
      include 'strmln.icl'
      include 'supma.icl'
      include 'units.icl'
      include 'ovl_props.icl'
      include 'vector.icl'
      include 'message.icl'
      include 'traj.icl'
      include 'attributes.icl'
c
c
c     Local variable declarations.
c
      character *(256) value
      real x
      integer          i, ibeg, iend, status
      integer          asf
      logical          error
      dimension asf(13)
c
      integer vect_color
      common / vector_color/ vect_color
C
C     data blocks
C
      data asf / 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 /
C     ASF(1) set individual to change line type
C     ASF(2) and ASF(3) set to individual in order to be able to vary
C     line width scale factor and polyline color index, respectively.
C     ASF(7) set to individual to set text font
C     ASF(8) set to individual to allow changing text expansion factor.
c
c
c     External function declarations.
c
      integer   chkcolor, strbeg, strend
c
c
      error = .false.
c
c     save state of defaults.
c
      call save_command(P_ABSCISSA,          'ABSCISSA=D')
      call save_command(P_ASPECT_RATIO,      'ASPECT_R=1.0')
      call save_command(P_AXES,              'AXES=LINLI')
      call save_command(P_COLOR_BAR,         'COLOR_BAR=ON')
      call save_command(P_CONTOUR_INTERVAL,  'CONTOUR=0.0')
c     CONTOUR_VALUES not needed, covered by CONTOUR_INTERVAL
      call save_command(P_DATA_UNITS,        'DATA_UNIT=DEFAULT')
      call save_command(P_DEFAULT_MAP,       'DEFAULT_MAP=ON')
      call save_command(P_DOMAIN_UNITS,      'DOMAIN_UNIT=DEFAULT')
      call save_command(P_DRAWMAP,           'DRAWMAP=OFF')
c     DUMP not needed
c     EXIT not needed
c     FIELD, FILE not needed (set by initialization or user)
      call save_command(P_FILLTYPE,          'FILLTYPE=1')
      call save_command(P_FILL_CONTOURS,     'FILL_CONTOURS=1')
      call save_command(P_FRAMEWIDTH,        'FRAMEWIDTH=1')
c     FREE not needed
      call save_command(P_GRIDSPACING,       'GRIDSPAC=10.0')
c     HELP not needed
c     HORIZONTAL scale same as ABSCISSA
c     INPUT not needed
      call save_command(P_LABEL,             'LABEL=ON')
      call save_command(P_LANDSCAPE,             'LANDSCAPE=OFF')
c     LIFT_OVERLAY not needed
      call save_command(P_LINECOLOR,         'LINECOLOR=DEFAULT')
      call save_command(P_LINELABELS,        'LINELABE=ON')
      call save_command(P_LINEPATTERN,       'LINEPATTERN=DEFAULT')
      call save_command(P_LINEWIDTH,         'LINEWIDTH=1.0')
c     LIST_SEGMENTS, LOCKCOLORTABLE, LOGFILE not needed
c     LOOP, LOOP_DUMP, LOOP_QUIT not needed
      call save_command(P_MAPLIMITS,         'MAPLIMIT=CL')
      call save_command(P_MAPOUTLINES,       'MAPOUTLI='//d_outl)
      call save_command(P_MAPROJ,            'MAPROJ='//d_jprj)
      call save_command(P_MAX_PLOT,           'MAX_PLOT=15')
c     NEW_PLOT not needed
      call save_command(P_NOZERO,            'NOZERO=OFF')
      call save_command(P_NULBLL,            'NULBLL=3')
      call save_command(P_ORDINATE,          'ORDINATE=D')
      call save_command(P_OVERLAY_CONTOURCOLORS,
     &     'OVERLAY_CONTOURCOLORD=DEFAULT')
      call save_command(P_OVERLAY_CONTOURPATTERNS,
     &     'OVERLAY_CONTOURPATTERNS=DEFAULT')
      call save_command(P_OVERLAY_LINECOLORS,'OVERLAY_LINECOLORS=D') 
      call save_command(P_OVERLAY_LINEPATTERNS,'OVERLAY_LINEP=DEFAULT')
c     OVERLAY_PLOT not needed
      call save_command(P_PLAT,              'PLAT=0.0')
      call save_command(P_PLON,              'PLON=0.0')
c     PLOT, PRINT, QUIT not needed
c     READCOLORTABLE not needed (it is called during initialization)
c     REDRAW not needed
      call save_command(P_RESIZE,            'RESIZE=D')
      call save_command(P_ROTA,              'ROTA=0.0')
      call save_command(P_SAVE,              'SAVE=OFF')
      call save_command(P_SCALE_VECTOR,      'SCALE_VECTOR=ON')
      call save_command(P_SINGLEVALUE,       'SINGLEVA=OFF')
c     SLICE not needed (set by user if used)
      call save_command(P_STREAMLINE,        'STREAMLI=OFF')
      call save_command(P_TCONTOUR_INTERVAL, 'TCONTOUR=0.0')
c     TCONTOUR_VALUES not needed, covers by TCONTOUR_INTERVAL
      call save_command(P_TIC_SCALE,         'TIC_SCALE=1.0')
      call save_command(P_TIC_WIDTH,         'TIC_WIDTH=THIN,THIN')
      call save_command(P_TIC_LABELS,        'TIC_LABELS=NORM')
      call save_command(P_TIC_DECIMALS,      'TIC_DECIMALS=D')
      call save_command(P_TIME,              'TIME=F')
      call save_command(P_TOPO_SCALE,        'TOPO_SCA=20.0')
c     TRANS not needed (it is called during initialization or set by user)
      call save_command(P_TYPE,             'TYPE=SCALAR')
c     VECTOR_COMPONENTS  not needed (must be set by user before plotting)
      call save_command(P_VECTOR_INTERVAL,   'VECTOR_INT=3,3')
      call save_command(P_VECTOR_POSITION,   'VECTOR_POS=CENTER')
c     VERTICAL SCALE same as ORDINATE
c     VSLICE, WARN_TIMEOUT not needed
      call save_command(P_WINDOW,            'WINDOW')
c     WRITECOLORTABLE, WRITE_FRAME not needed
      call save_command(P_XLOCATION,         'XLOC=F')
      call save_command(P_XTICS,             'XTICS=0.0,0.0')
      call save_command(P_YLOCATION,         'YLOC=F')
      call save_command(P_YTICS,             'YTICS=0.0,0.0')
      call save_command(P_ZLOCATION,         'ZLOC=F')
      call save_command(P_START_TRAJ,        'START_TRAJ=0.,0.,0.,0.')
      call save_command(P_START_TRAJ_MAP,'START_TRAJ_MAP=0.,0.,0.,0.')
      call save_command(P_INT_TRAJ,          'INT_TRAJ=0.,28800')
      call save_command(P_POINT_AXIS,        'POINT_AXIS=Default')
      call driver('POINT_AXIS=Default',1)
c
c     Set misc. variables
c
      aratio = 1.0
      drstrmln = .FALSE.
      plotyp2 = 'SCALAR'
      plotyp1 = 'LINE'
      vect_component(1) = ' '
      vect_component(2) = ' '
      vect_component(3) = ' '
      incx = 3
      incy = 3
      scale_vector = .true.
      vecpos = 0
      vmax = 0.0
      xmajor = 0.0
      xminor = 0.0
      ymajor = 0.0
      yminor = 0.0
      maxplot= 15
      curlist = 1
c
c     Set Abscissa and Ordinate min and max
      abscissa_min=bigone 
      abscissa_max=bigone
      ordinate_min=bigone
      ordinate_max=bigone
c
c
c     Set variables for color options.
c
      clrtab = d_clrtab
      lock_field = ' '
      lock_entry = 0
      lock_set_cint = 0	
      ignore_lock_var = 0
      traj_val = -1e19
      traj_step = 0.
      traj_entry = 0
      traj_depth = 0
      traj_step_set = .false.
      traj_val_set = .false.
      traj_background_field = 'depth'
      old_traj_background = traj_background_field
      old_traj_min_color = 1000
      do i=1,4
         call set_traj_atts(-i, -1e19, 0)
      enddo
      do i=1,160
         traj_levels(i) = 1E38
      enddo
      arrow_bold_interval = 0
c
c
c     Set variables used by contouring routines.
c
      spval  = 0.0
      nulbll = 3
      cllab  = .true.
      nozero = .false.
      cint   = 0.0
      tcint  = 0.0
      tnum_vals = 0
      topo_scale = 20.0
      linlog = 1
      label  = .true.
      color_bar = .true.
      single = .FALSE.
c
c
c     Set trajectory variables
c
      trajend = 0.
      trajbeg = 0.
      num_traj_comp = 0
      time_ind = .FALSE.
      traj_component(1) = ' '
      traj_component(2) = ' '
      traj_component(3) = ' '
c
c
c     Set data variables.
c
      datfil   = d_datfil
      old_file = ' '
c
c
c     Set flags.
c
      movielabs = .false.
      savflg = d_savflg
      terrflg = .true.
      pltflg = .false.
      mapflg = .false.
c
c
c     Set general plotting options.
c
      filtyp  = 1
      fill_frequency = 1
      linwdth = 1
      pltflg = .false.
      do i=1,2
         pat1(i)=1E38
         pat2(i)=1E38
         pat3(i)=1E38
         pat4(i)=1E38
         pat5(i)=1E38
         pat6(i)=1E38
      enddo
      terr_nulbll = 3
c     
c
c     Set variables for calls to supmap.
c
      outlin  = d_outl
      maplim  = 'CL'
      proj    = d_jprj
      plat    = d_plat
      plon    = d_plon
      rota    = d_rota
      grdspa  = 10.0
      defmap  = .true.
      mapwin_u1=0.0
      mapwin_u2=0.0
      mapwin_v1=0.0
      mapwin_v2=0.0
c
c
c     Set initial domain size.
c
      cpmin1 = 1
      cpmin2 = 1
      cpmin3 = 1
      cpmin4 = 1
      cpmax1 = 1
      cpmax2 = 1
      cpmax3 = 1
      cpmax4 = 1
c
c     Initialize dimention attributes
c
      call init_dim_atts()
c
c     Set up GKS stuff.
c
c     Set aspect source flags and text index
      call gsasf (asf)
c
c     Set line type to 1 to get thin solid lines by default
      call gsln (1)
c
c     Set text index to 1 to get normal size black text by default
      call gstxi (1)
c
c     Set polyline index to 1 to get thin black lines by default
      call gspli (1)
c
c     Set text font and precision. font is set to get times-roman font
c     on postscript printer.
      call gstxfp (1, 2)
c
c     Read the default color table.
c
      status = chkcolor()
      if ( status .eq. 1 ) then
         call getenv ('IVECTBFILE', value)
         if ( value  .eq. ' ' ) then
            call getenv ('ivectbfile', value)
         endif
         if ( value  .eq. ' ' ) then
            call getenv ('UWGAPCTBFILE', value)
         endif
         if ( value  .eq. ' ' ) then
            call getenv ('uwgapctbfile', value)
         endif

         if ( value  .ne. ' ' ) then
            call catnull (value, len(value), error)

            call read_color_table (value, MIN_USER_COLOR, 
     &                             MAX_USER_COLOR,1)

            call read_color_table (value, MIN_TRAJ_COLOR, 
     &                             MAX_TRAJ_COLOR,0)
         else
            call getenv ('IVECTB', value)
            if ( value  .eq. ' ' ) then
               call getenv ('ivectb', value)
            endif
            if ( value  .eq. ' ' ) then
               call getenv ('UWGAPCTB', value)
            endif
            if ( value  .eq. ' ' ) then
               call getenv ('uwgapctb', value)
            endif
            if ( value .eq. ' ') then
               call getenv ('IVECTBLIB', value)
            endif
            if ( value  .eq. ' ' ) then
               call getenv ('ivectblib', value)
            endif
            if ( value .ne. ' ' ) then
               ibeg = strbeg (value)
               iend = strend (value)
               value = value(ibeg:iend)//'/default.ctb'
               call catnull (value, len(value), error)
c               write(6,*)MIN_USER_COLOR, MAX_USER_COLOR
               call read_color_table (value, MIN_USER_COLOR, 
     &              MAX_USER_COLOR,1)
               call read_color_table (value, MIN_TRAJ_COLOR, 
     &              MAX_TRAJ_COLOR,0)
            else
               value = LIBDIR//
     &              '/default.ctb'
               call catnull (value, len(value), error)
               call read_color_table (value, MIN_USER_COLOR, 
     &              MAX_USER_COLOR,1)
               call read_color_table (value, MIN_TRAJ_COLOR, 
     &              MAX_TRAJ_COLOR,0)
            endif
         endif
      endif
c
c     Set default high and low contour line colors.
c
      just_set_hicl = .false.
      just_set_locl = .false.
      call  default_overlay_colors
      call  default_overlay_linecolors
c
c     overlay_patterns
c     
      just_set_hipat = .false. 
      just_set_lopat = .false. 
      call default_overlay_patterns
      call default_overlay_linepatterns
c
c     vector magnitude lock = off
c
      call setlvar('veclock',.false. , error)
c
c     warn_timeout is 3
c
      call setivar('warn_timeout', 3, error)
      data_slope = 1.0
      data_intercept = 0.0
      do i = 1, MAX_UNITS
         domain_slope(i) = 1.0
         domain_intercept(i) = 0.0
      enddo
c
c     Set printer defaults
      printer_name = ' '
      print_command = 'lpr'
      color_printer = .false.
      color_file = .false.
      landscape = 0
c
c     Set ticmark preferences
      tic_width_maj = 1
      tic_width_min = 1
      tic_labels = 0
      tic_decimals = -1
      tic_scale = 1.0
c
c     Set frame width
      frmwdth=1.0
c
c	Set slice defaults
c
      ni_1d = 0
      ni_2d = 0
      use_buttons = 1
      multiple_read = -1
c
c     Init window storage stack
c
      call init_window_list()
c
c     NCAR initialization
c
      call pcsetc('FC', char(255))
c
c     Force loading of netcdf Fortran jackets.
c
      call ncgopt(i)
      if(error) then
         print*,' THERE IS ERROR IN SETDEFs'
         write (message, 100)
         call write_message 

c	We should never get here.  This call to vvumxy is to force it
c	to load
c
	call vvumxy(0.,0.,0.,0.,0.,x,x,x,x,i)
      endif
      return
 100  format (1x,'THERE IS ERROR IN SETDEFs')
      end
