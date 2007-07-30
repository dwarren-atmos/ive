c-----------------------------------------------------------------------
c     $Id: plotops.icl,v 1.24 2007-06-21 20:58:31 reinecke Exp $
c     Purpose:
c        This file contains all common blocks which carry global
c        variables related to plotting options.
c
c     History:
c	$Log: plotops.icl,v $
c	Revision 1.24  2007-06-21 20:58:31  reinecke
c	Commiting local modifications for transfer to svn
c
c	Revision 1.23  2006/08/24 22:08:40  warren
c	added vlabel command to get max vectors plotted even if label=off
c	
c	Revision 1.22  2002/08/09 19:57:59  warren
c	Fixed for Linux and tested with DEC sun etc.
c	most items inside #ifdef's, or were general cleanup/fixes
c	
c	Revision 1.21  2002/06/11 17:26:16  harryive
c	Change aspect ratio to affect the plot, not just printing.
c	
c	Add first part of contour value specification.
c	
c	Revision 1.20  2001/01/23 22:47:13  davidive
c	cpline.f: fixed to use zero_color
c	everything else: First cut at wireframes and isosurfaces. No dimentions yet
c	no widgets yet.
c
c	Revision 1.19  2000/05/09 19:53:55  harry
c	Seperate contour and line colors.  Make patterns work like colors.
c
c	Revision 1.18  1998/06/03 17:54:39  harry
c	Fix tick mark labels.   Add new command COLOR_BAR.
c
c	Revision 1.17  1997/07/22 19:50:45  harry
c	Fix multiple reading to allow user to specify coordinate transform.
c
c	Revision 1.16  1997/06/27 21:40:14  davidive
c	Fix arbitrary slice line labeling
c
c	Revision 1.15  1997/06/03 19:14:22  davidive
c	The big mouse slicing changeover. We have the new form, only requires you
c	set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c	always runs through init_points_ to catch all these things.
c	note: only actually works when two dimentions have points set. Needs to
c	be expanded into 1, 3 or 4.
c
c	Revision 1.14  1997/02/07 18:39:09  harry
c	Remove PARTIAL_READ and expand MULTIPLE_READ.
c
c	Revision 1.13  1997/01/17 20:38:18  harry
c	First changes for MULTIPLE_READ and PARTIAL_READ.
c
cRevision 1.12  1996/10/11  01:13:03  warren
cTo fill or not to fill is no longer the only question. Now there is a seperate
cquestion of solid or pattern. Also first part of mouse slicing. 2d -> 1d
cLabels are not fixed yet.
c
c	Revision 1.11  1996/04/25 16:25:25  harry
c	Add Skew-T plots and the beginning of trajectory plots.
c
c	Revision 1.10  1996/04/08 18:57:13  warren
c	first parts of num unlabeled lines for terrain and fixed transform routine
c	to use last directory if none is given.
c
c	Revision 1.9  1996/04/01 20:23:43  warren
c	Terrain form and new terrain command (on / off)
c
c	Revision 1.8  1995/12/15 22:33:21  warren
c	moved patterns into 4.0 and implemented the list of fixes and changes from
c	Curtis.
c
cRevision 1.7  1995/10/18  19:18:17  warren
cStipples from 3.3, First crack at mouse windowing and put window_points_
cinto a header file and out of the mouse stuff and slicers. Touched almost
ceverything it seems.
c
cRevision 1.6  1994/09/14  18:20:02  harry
cBig 1D flip uncommit.
c
cRevision 1.5  1994/04/19  23:03:17  yuri
cBig 1D flip commit.
c
cRevision 1.4  1993/11/16  01:14:11  warren
crest of the rearranging for alignment.
c
cRevision 1.3  1993/07/06  20:33:43  warren
cFixed scaled_loc for DEc compiler.
c
cRevision 1.2  1993/07/06  20:23:10  harry
cAdd the "scaled_loc" variable to retain scaled values for fixed coordinates.
c
cRevision 1.1.1.1  1993/04/07  19:40:12  harry
cInitial CVS checkin of IVE.
c
cRevision 2.9  1993/02/02  00:02:13  warren
cremoved xmajorl, xminorl, ymajorl, yminorl
c
cRevision 2.8  1993/01/13  23:10:30  millerp
cMove char variables to separate common block.
c
cRevision 2.7  1992/12/10  21:13:01  millerp
cDelete unused variables, add new ones.
c
cRevision 2.6  1992/11/24  16:31:44  millerp
cGet rid of hslice, vslice variables.
c
cRevision 2.5  1992/10/30  19:15:50  millerp
cAdded average, deleted sounding.
c
cRevision 2.4  1992/10/28  22:32:48  millerp
cRemove logical variable AVERAGE.
c
cRevision 2.3  1992/10/15  23:01:21  warren
cadded color locking stuff
c
cRevision 2.2  1992/10/06  18:26:45  warren
cadded xtics_line and ytics_line and plot types scaler and vector
c
cRevision 2.1  1992/09/17  15:42:49  warren
cchanged the slicing syntax to use xloc, yloc, zloc and time instead of
chloc and vloc. this include changes to the common blocks and command set.
c
cRevision 2.0  1992/07/01  21:15:48  millerp
cReleased version rereving to 2.0
c
cRevision 1.3  1992/06/15  19:09:12  millerp
cAdded average.
c
cRevision 1.2  1992/05/29  22:37:05  millerp
cAdded tloc.
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c     General plotting options.
c

      character*16 plotyp1, plotyp2, plotyp3
      common / pltopt_char / plotyp1, plotyp2, plotyp3

      integer     filtyp,  fill_frequency
      common / fills / filtyp, fill_frequency

      logical pltflg, surface, average
      real         linwdth
      real         aratio
      common / pltopt / pltflg, surface, average, linwdth, aratio

      real         pat1, pat2, pat3, pat4, pat5, pat6
      dimension    pat1(2), pat2(2), pat3(2), pat4(2), pat5(2),
     &             pat6(2)
      common / plotop_pat_real / pat1, pat2, pat3, pat4, pat5,pat6

      logical terrflg
      integer     terr_nulbll
      common / terplotstuff / terrflg, terr_nulbll



c
c     Options for locations for slices and trajectory points.
c
      integer lock, use_buttons, multiple_read, ive_plock
      integer multiple_read_start, multiple_read_end, multiple_read_flag
      integer multiple_read_real
      real locatn, xloc, yloc, zloc, tloc, point_1, point_2, point_3, 
     &     angle, degang, scaled_loc(4)

      dimension locatn(3), lock(4), point_1(4), point_2(4), point_3(4),
     &          ive_plock(4)
      common / slices / lock, use_buttons, xloc, yloc, zloc, tloc,   
     &			locatn, ive_plock, point_1, point_2, point_3,
     &                  angle, degang, scaled_loc,
     &			multiple_read, multiple_read_start,
     &                  multiple_read_end, multiple_read_flag,
     &			multiple_read_real
c
c     Options for tic marks on plot perimeter.
c   
      logical          ive_set_xmajorl, ive_set_ymajorl
      real             xmajor, xminor, ymajor, yminor
      common  / tics / ive_set_xmajorl, ive_set_ymajorl,
     &                 xmajor, xminor, ymajor, yminor
c
c     Option for plot axis scaling.
c
      integer linlog
      common / axiscal / linlog
c
c     Option for labeling plot.
c
      logical label, color_bar,vlabel
      common / labs / label, color_bar, vlabel

C     Fill defines
      integer SOLID, PATTERN
      integer NEVER, EXECPT_OVERLAY, ALWAYS
c
c     Option for bold arrow interval in trajectories
c
      integer bold_int
	common / bold_traj / bold_int

      parameter(        SOLID   =  1,
     &                  PATTERN = 2)

      parameter(        NEVER          =  1,
     &                  EXECPT_OVERLAY =  2,
     &                  ALWAYS         =  3)
