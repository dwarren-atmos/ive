      subroutine uwperim (xmajor, xminor, ymajor, yminor)
c-----------------------------------------------------------------------
c     $Id: uwperim.F,v 1.1 2002/11/04 18:40:19 warren Exp $
c     Purpose:
c        This subroutine is used in place of the NCAR graphics
c        routine to draw a perimeter for plots. Its main diffenence
c        is that tic intervals are specified in user coordinates, 
c        instead of specifying numbers of evenly-spaced tic marks,
c        when linear axes are drawn. When logarithmic axes are used,
c        this routine works like the NCAR Graphics routine perim. 
c     Arguments:
c        xmajor  real  input  interval, in user coordinates, between 
c                             major tic marks on the x axis.
c        xminor  real  input  interval, in user coordinates, between 
c                             minor tic marks on the x axis.
c        ymajor  real  input  interval, in user coordinates, between 
c                             major tic marks on the y axis.
c        xminor  real  input  interval, in user coordinates, between 
c                             minor tic marks on the y axis.
c     History:
c       $Log: uwperim.F,v $
c       Revision 1.1  2002/11/04 18:40:19  warren
c       Need to use .F for cpp on sun
c
c       Revision 1.30  2002/08/09 19:58:02  warren
c       Fixed for Linux and tested with DEC sun etc.
c       most items inside #ifdef's, or were general cleanup/fixes
c
c       Revision 1.29  1998/06/03 17:54:42  harry
c       Fix tick mark labels.   Add new command COLOR_BAR.
c
c       Revision 1.28  1997/12/23 22:43:53  warren
c       increased MXCRA to 50000 and cleaned up some unused variables and split a
c       couple of large fortran if not blocks, so it wouldn't evaluate it all if
c       unnecessary.
c
c       Revision 1.27  1997/02/27 18:59:03  warren
c       ive starts with an i not a v
c
c       Revision 1.26  1997/02/27 16:22:47  warren
c       put lock_int in common block where it belongs and changed maxval and minval
c       to ive_maxval and ive_minval to not interfere with the f90 intrinsic.
c       Also changed maxval in uwperim to ive_maxval_per to distinguish the
c       variable from the function.
c
c       Revision 1.25  1996/07/26 23:15:13  warren
c       made slope and intercept doubles
c
c       Revision 1.24  1995/09/05 20:34:17  harry
c       Add new slicer.  Good luck!
c
c Revision 1.23  1995/06/23  21:16:31  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.22  1995/03/24  22:57:06  warren
c bring fix up from 3.2 for tic marks.
c
c Revision 1.21  1995/03/20  23:57:13  warren
c Fixed Dales not sticking color changes and louisa's missing tic mark problem.
c
c Revision 1.20  1994/12/29  19:58:36  warren
c Fixed Dales missint Tic bug.
c
c Revision 1.19  1994/12/27  22:37:10  warren
c call white_background for the movielabels=off option as per Dale.
c changed to label the top of the plot box if the value is at least
c half of a tic interval above the last tic mark, not just if it is
c exactly tcint above.
c
c Revision 1.18  1994/10/08  19:01:39  warren
c The annotation widget and updating it. As well as changing tic_bold to
c tic_labels and adding an off option.
c
c Revision 1.17  1994/06/28  17:43:43  warren
c First cut at fancy tic marks.
c
c Revision 1.16  1994/03/30  22:19:46  harry
c Change the way 1D plots handle user specified units.  The data is
c scaled instead of trying to scale the labels.  This is the same way 2D
c plots are handled.
c
c Revision 1.15  1994/03/30  00:43:16  warren
c fixed near 0 problem. Changed it to label vector tics.
c
c Revision 1.14  1994/03/28  17:14:23  harry
c Remove call to cpinrc since it is called in ive_main.
c
c Revision 1.13  1994/03/25  23:12:34  harry
c Fixes to plot labelling again.
c
c Revision 1.12  1994/03/25  22:24:36  warren
c first fix to Dales label problem.
c
c Revision 1.11  1994/02/23  00:15:31  warren
c Final 3.1 label changes.
c
c Revision 1.10  1994/02/16  17:00:55  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.9  1994/02/10  18:11:02  warren
c Fixed missing decimals
c
c Revision 1.8  1994/02/03  22:42:58  warren
c last of the movie labels and made the tic mark labels smarter.
c
c Revision 1.7  1994/01/29  00:11:43  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.6  1993/12/13  23:47:55  warren
c fixed line plots so the box get drawn around them. also added a proof option
c to the makefile.
c
c Revision 1.5  1993/07/09  22:20:09  warren
c Don't scale mins amd maxs twice.
c
c Revision 1.4  1993/07/09  19:09:40  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.3  1993/07/01  16:12:29  warren
c Fixed vertical slice label problem.
c
c Revision 1.2  1993/06/11  20:40:56  yuri
c 	problem with labeling solved.
c
c Revision 1.1.1.1  1993/04/07  19:40:48  harry
c Initial CVS checkin of IVE.
c
c Revision 2.6  1992/11/24  16:29:36  millerp
c Fixed tic scaling.
c
c Revision 2.5  1992/10/30  19:09:32  millerp
c Fixed type mismatch on epsilon.
c
c Revision 2.4  1992/10/27  23:07:29  millerp
c Fix up tic labeling for line plots, and add routine labtic.
c
c Revision 2.3  1992/10/15  23:05:38  warren
c fixed axis scaling to use scaled values.
c
c Revision 2.2  1992/10/02  22:58:59  warren
c changed the commonblock window_points to specify witch axis is what
c and put it in pltwin.icl
c
c Revision 2.1  1992/10/02  16:48:18  warren
c added labeling of major tics in line plots.
c
c Revision 2.0  1992/07/01  21:17:13  millerp
c Released version rereving to 2.0
c
c Revision 1.5  1992/06/15  19:15:32  millerp
c Change argument list for scale.
c ,
c
c Revision 1.4  1992/06/10  17:57:44  millerp
c *** empty log message ***
c
c Revision 1.3  1992/06/02  22:30:45  millerp
c *** empty log message ***
c
c Revision 1.2  1992/05/29  22:39:23  millerp
c Added scaling for tic intervals.
c .\
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'pltwin.icl'
c     Variables
c       xcornr  : world coords of X points used in drawing perimeter
c       ycornr  : world coords of Y points used in drawing perimeter
c       wdwbnd  : world window boundaries
c       vpbnd   : world viewport boundaries
c       *major  : interval in world coords for major tics. Assumes that
c                 tics start at * = 0.  
c       *minor  : interval in world coords for minor tics. 
c       up_rt   : X or Y coord values along the upper or right 
c                 boundaries, depending on which boundary we are drawing
c                 tics
c       lo_lft  : X or Y coord values along the lower or left  
c                 boundaries, depending on which boundary we are drawing
c                 tics
c
c     Argument declarations.
c
      real       xmajor, xminor, ymajor, yminor
c
c
c     Local variable declarations.
c
      integer    cindex, ierror, ixform, lock(4), numfree, i
      integer    xax, yax
      logical    fixx, fixy, error
      real       xcornr, ycornr, wdwbnd, vpbnd, up_rt, lo_lft
      real       wxmin, wxmax, wymin, wymax
      real       TICLEN, xoff_unscaled, yoff_unscaled
      double precision       dslope(4),dinter(4)
      parameter ( TICLEN = 0.015 )
      dimension xcornr(5), ycornr(5), wdwbnd(4), vpbnd(4),
     &          up_rt(2), lo_lft(2)
      character*18  old_font           !to hold the general character
      character*5   dummy_alignment_var
      integer    old_qual              !settings
c
c
c     External function declarations.
c
      real       cfux, cfuy
c
c
c      
c
      call getdarr('domain_slope',dslope,4,error)
      call getdarr('domain_intercept',dinter,4,error)
      xax = xaxis
      yax = yaxis
      xaxis = mod(xaxis, 10)
      yaxis = mod(yaxis, 10)
      call gqplci (ierror, cindex)
      call gsplci (1)
      call getset ( vpbnd(1),  vpbnd(2),  vpbnd(3),  vpbnd(4), 
     &             wdwbnd(1), wdwbnd(2), wdwbnd(3), wdwbnd(4), ixform)
c
c
c     Set up arrays containing the world coordinate points to be 
c     connected for the plot perimeter. Perimeter will be drawn starting
c     at lower left corner, working counterclockwise.
c
c      wxmin = wdwbnd(1)
c      wxmax = wdwbnd(2)
c      wymin = wdwbnd(3)
c      wymax = wdwbnd(4)

      numfree = 0
      call getiarr('lock', lock, 4, ierror)
      do i = 1, 4
         if (lock(i) .eq. 0) then
            numfree = numfree +1
         endif
      enddo
      wxmin=x1
      wxmax=x2
      wymin=y1
      wymax=y2
      
      fixx = .FALSE.
      fixy = .FALSE.
      if(ixform .eq. 3 .or. ixform .eq. 4) then
         if(wxmin .le. 0.0) then
            wxmin = 1.E-37
            fixx = .TRUE.
         else if(wxmax .le. 0.0) then
            wxmax = 1.E-37
            fixx = .TRUE.
         endif
      endif
      if(ixform .eq. 2 .or. ixform .eq. 4) then
         if(wymin .le. 0.0) then
            wymin = 1.E-37
            fixy = .TRUE.
         else if(wymax .le. 0.0) then
            wymax = 1.E-37
            fixy = .TRUE.
         endif
      endif

      call set (vpbnd(1),  vpbnd(2),  vpbnd(3),  vpbnd(4),
     &     wxmin,wxmax,wymin,wymax, ixform)
      
      if(fixx) then
         if(wxmin .eq. 1.E-37) wxmin = 0.0
         if(wxmax .eq. 1.E-37) wxmax = 0.0
      endif
      if(fixy) then
         if(wymin .eq. 1.E-37) wymin = 0.0
         if(wymax .eq. 1.E-37) wymax = 0.0
      endif

      xcornr(1) = wxmin
      xcornr(2) = wxmax
      xcornr(3) = wxmax
      xcornr(4) = wxmin
      xcornr(5) = wxmin
c                 
      ycornr(1) = wymin
      ycornr(2) = wymin
      ycornr(3) = wymax
      ycornr(4) = wymax
      ycornr(5) = wymin
c
c     Set frame line thickness
c
      call  gslwsc (frmwdth)
c
c     Draw the perimeter.
c
      call line ( xcornr(1), ycornr(1), xcornr(2), ycornr(2) )
      call line ( xcornr(2), ycornr(2), xcornr(3), ycornr(3) )
      call line ( xcornr(3), ycornr(3), xcornr(4), ycornr(4) )
      call line ( xcornr(4), ycornr(4), xcornr(1), ycornr(1) )
c     call gpl (5, xcornr, ycornr)
c
c
c
c     Get old font
         call pcgeti ('QU',old_qual)
         call pcgetc ('FN', old_font)
c      
c     Set up font
      if(tic_labels .eq. 1) then
         call pcseti ('QU',0)
         call pcsetc ('FN', 'HELVETICA-BOLD    ')
      else
         call pcseti ('QU',1)
         call pcseti ('CL',1)
      endif
c
c     Draw major tics on X axis.
c
c      call getavar('plotyp', type, lerror)
c
c     Set tic line thickness
      call  gslwsc (tic_width_maj)
c
      xoff_unscaled=xoff
      if(xoff .ne. 0) then
         if(xaxis .gt.0 .and. xaxis .lt.5) then
            call unscale (xoff_unscaled, 1, dslope(xaxis),
     &           dinter(xaxis),0.0)
         else
            xoff=0.
            xoff_unscaled=0.
         endif
      endif
      lo_lft(1) = cfuy (vpbnd(3))
      lo_lft(2) = cfuy (vpbnd(3) + TICLEN)
      up_rt(1)  = cfuy (vpbnd(4))
      up_rt(2)  = cfuy (vpbnd(4) - TICLEN) 
      if ( (ixform .eq. 1) .or. (ixform .eq. 2) ) then 
         if (tic_labels .lt. 2 ) then
            call drxtic (xmajor, lo_lft, up_rt, wxmin+xoff_unscaled, 
     &           wxmax, .true., 0)
         else
            call drxtic (xmajor, lo_lft, up_rt, wxmin+xoff_unscaled,
     &            wxmax,.false.,0)
         endif
      else
c         if (type(1:1) .eq. 'S') then
            call majxtic (nint(xmajor), lo_lft, up_rt, 
     &        wxmin+xoff_unscaled, wxmax, .true.)
c         else
c            call majxtic (nint(xmajor), lo_lft, up_rt, wxmin, wxmax,
c     &                    .false.)
c         endif
      endif
c
c     Draw minor tics on X axis.
c
c     Set tic line thickness
      call  gslwsc (tic_width_min)
      lo_lft(2) = cfuy (vpbnd(3) + 0.5 * TICLEN)
      up_rt(2)  = cfuy (vpbnd(4) - 0.5 * TICLEN) 
      if ( (ixform .eq. 1) .or. (ixform .eq. 2) ) then 
         call drxtic (xminor, lo_lft, up_rt, wxmin+xoff_unscaled, wxmax, 
     &                .false., xmajor)
      else
         call minxtic (nint(xminor), nint(xmajor), lo_lft,
     &        up_rt, wxmin+xoff_unscaled, wxmax)
      endif
c     
c     
c     Draw major tics on Y axis.
c
c     Set tic line thickness
      call  gslwsc (tic_width_maj)
c
            yoff_unscaled=yoff
      if(yoff .ne. 0) then
         if(yaxis .gt.0 .and. yaxis .lt.5) then
            call unscale (yoff_unscaled, 1, dslope(yaxis),
     &           dinter(yaxis),0.0)
         else
            yoff=0.
            yoff_unscaled=0.
         endif
      endif
      lo_lft(1) = cfux (vpbnd(1))
      lo_lft(2) = cfux (vpbnd(1) + TICLEN)
      up_rt(1)  = cfux (vpbnd(2))
      up_rt(2)  = cfux (vpbnd(2) - TICLEN) 
      if ( (ixform .eq. 1) .or. (ixform .eq. 3) ) then
         if (tic_labels .lt. 2) then
            call drytic (ymajor, lo_lft, up_rt, wymin+yoff_unscaled,
     &            wymax, .true.,0)
         else
            call drytic (ymajor, lo_lft, up_rt, wymin+yoff_unscaled,
     &            wymax, .false.,0)
         endif
      else
c         if (type(1:1) .eq. 'S') then
            call majytic (nint(ymajor), lo_lft, up_rt, 
     &        wymin+yoff_unscaled, wymax, .true.)
c         else
c            call majytic (nint(ymajor), lo_lft, up_rt, wymin, wymax, 
c     &                    .false.)
c         endif
      endif
c
c     Draw minor tics on Y axis. 
c
c     Set tic line thickness
      call  gslwsc (tic_width_min)
c     
      lo_lft(2) = cfux (vpbnd(1) + 0.5 * TICLEN)
      up_rt(2)  = cfux (vpbnd(2) - 0.5 * TICLEN) 
      if ( (ixform .eq. 1) .or. (ixform .eq. 3) ) then
         call drytic(yminor, lo_lft, up_rt, wymin+yoff_unscaled, wymax, 
     &               .false., ymajor)
      else
         call minytic (nint(yminor), nint(ymajor), lo_lft,
     &         up_rt, wymin+yoff_unscaled, wymax)
      endif
c     
c
c     Flush pen move buffer and return.
c
c      call plotit (0, 0, 0)
      call gks_flush
      call gsplci (cindex)
      call set( vpbnd(1),  vpbnd(2),  vpbnd(3),  vpbnd(4), 
     &             wdwbnd(1), wdwbnd(2), wdwbnd(3), wdwbnd(4), ixform)

c
c
c      
c     Return font to what it was
         call pcseti ('QU',old_qual)
      if(old_qual .eq. 0) then
         call pcsetc ('FN', old_font)
      else
         call pcseti ('CL',1)
      endif
c     return line width to 1 as expected from this routine.
      call  gslwsc (1.0)
      xaxis = xax
      yaxis = yax
      return
      end
c
c
      subroutine drxtic (ticint, tic1, tic2, ive_minval_per, 
     &     ive_maxval_per, lab, comp)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine is used to draw tic marks around a plot
c        perimeter.  It is called by subroutine uwperim.
c     Arguments:
c        ticint  real  input  The interval in world coordinates between
c                             tic marks.
c        tic1    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the lower or left border of 
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction.  
c        tic2    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the upper or right border of
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction. 
c        ive_minval_per  real  input  The minimum world coordinate value on 
c                             either the lower or left boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        ive_maxval_per  real  input  The maximum world coordinate value on 
c                             either the upper or right boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        lab    logical input Label Tics or not
c        comp   real    input Value to compare against for overwriting
c-----------------------------------------------------------------------
c
      include 'pltwin.icl'
      include 'units.icl'
c
c     Argument derclarations.
c
      logical     lab
      real        ticint, tic1, tic2, ive_minval_per, ive_maxval_per, 
     &     comp
      dimension   tic1(2), tic2(2)
c
c
c     Local variable declarations.
c
      logical   done
      real      EPSLON, hipt, lopt, point, tint
      dimension point(2)
c
c     EXTERNAL FUNCTIONS
c
#ifndef Linux
      double precision epsilon
#endif
c     
c     
c     
c
      EPSLON=epsilon(abs(ive_maxval_per-ive_minval_per))
      if(ive_maxval_per-ive_minval_per >0 .and. 
     &     ive_maxval_per-ive_minval_per< 1)EPSLON=EPSLON/3.
c     If any tic mark interval is zero (or close to zero), return.
c     
      if ( ticint .le. EPSLON ) then
         return
      endif
c     
c     
      point(1) = ive_minval_per
      if(point(1) .le. EPSLON .and. point(1) .ge. 
     &     (-1 * EPSLON)) point(1) = 0.0
      point(2) = point(1)
c     
c     Account for the possibility that the minimum coordinate value
c     is greater than the maximum coordinate value.
c     
      if ( ive_minval_per .lt. ive_maxval_per ) then
         lopt = ive_minval_per
         hipt = ive_maxval_per
         tint = ticint
      else
         lopt = ive_maxval_per
         hipt = ive_minval_per
         tint = -1.0 * ticint
      endif
c     
      done = .false.
c     
c
 5    if ( .not. done ) then
c
c     Tics are drawn at ticint intervals, beginning with ive_minval_per.
c
         point(1) = point(1) + tint
         if(point(1) .le. EPSLON .and. point(1) .ge. 
     &        (-1 * EPSLON)) point(1) = 0.0
         point(2) = point(1)
c     
         if(comp .ne. 0.0) then
            if(abs(mod(point(1)-ive_minval_per,comp)) 
     &           .le. epsilon(0.0) ) then
               go to 5
            endif
         endif
c
         if ( (point(1) .gt. lopt) .and. (point(1) .lt. hipt) ) then
c     
c     Tic location lies between the bounding world coord values,
c     so draw tics.
c
            call line (point(1), tic1(1), point(1), tic1(2))
            call line (point(2), tic2(1), point(2), tic2(2))
            if (lab) then
               call labtic ('x', xaxis, point(1), tic1(1), tic1(2))
            endif
c     call gpl (2, point, tic1)
c     call gpl (2, point, tic2)
c     
         else
            if (lab) then
               call labtic ('x', xaxis, lopt, tic1(1), tic1(2))
               if(hipt .ge. ((point(1) + point(1) - tint)/2)) then
                  call labtic ('x', xaxis, hipt, tic1(1), tic1(2))
               endif
            endif
c     
c     Tic location is outside bounding world coord values,
c     so we are done.
c     
            done = .true.
c     
         endif
c     
         go to 5
c     
      endif
c     
c     
      return
      end
c
      subroutine drytic (ticint, tic1, tic2, ive_minval_per, 
     &     ive_maxval_per, lab, comp)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine is used to draw tic marks around a plot
c        perimeter.  It is called by subroutine uwperim.
c     Arguments:
c        ticint  real  input  The interval in warsd coordinates between
c                             tic marks.
c        tic1    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the lower or left border of 
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction.  
c        tic2    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the upper or right border of
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction. 
c        ive_minval_per  real  input  The minimum world coordinate value on 
c                             either the lower or left boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        ive_maxval_per  real  input  The maximum world coordinate value on 
c                             either the upper or right boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c              
c        lab    logical input Label Tics or not
c        comp   real    input Value to compare against for overwriting
c-----------------------------------------------------------------------
c
c     Argument derclarations.
c
      logical     lab
      real        ticint, tic1, tic2, ive_minval_per, ive_maxval_per, 
     &     comp
      dimension   tic1(2), tic2(2)
c
c
      include 'pltwin.icl'
      include 'units.icl'
c
c     Local variable declarations.
c
      logical   done
      real      EPSLON, hipt, lopt, point, tint
      dimension point(2)
c
c     EXTERNAL FUNCTIONS
#ifndef Linux
      double precision epsilon
#endif
c
c
c
c
      EPSLON=epsilon(abs(ive_maxval_per-ive_minval_per))
      if(ive_maxval_per-ive_minval_per >0 .and. 
     &     ive_maxval_per-ive_minval_per< 1)EPSLON=EPSLON/3.
c     If any tic mark interval is zero (or close to zero), return.
c
      if ( ticint .le. EPSLON ) then
         return
      endif
c
c     
      point(1) = ive_minval_per
      if(point(1) .le. EPSLON .and. point(1) .ge. 
     &                 (-1 * EPSLON)) point(1) = 0.0
      point(2) = point(1)
c
c     Account for the possibility that the minimum coordinate value
c     is greater than the maximum coordinate value.
c
      if ( ive_minval_per .lt. ive_maxval_per ) then
         lopt = ive_minval_per
         hipt = ive_maxval_per
         tint = ticint
      else
         lopt = ive_maxval_per
         hipt = ive_minval_per
         tint = -1.0 * ticint
      endif
c
      done = .false.
c
c
 5    if ( .not. done ) then
c
c        Tics are drawn at ticint intervals, beginning with ive_minval_per.
c
         point(1) = point(1) + tint
         if(point(1) .le. EPSLON .and. point(1) .ge. 
     &                 (-1 * EPSLON)) point(1) = 0.0
         point(2) = point(1)
         if(comp .ne. 0.0) then
            if(abs(mod(point(1)-ive_minval_per,comp)) 
     &           .le. epsilon(0.0) ) then
               go to 5
            endif
         endif
c
         if ( (point(1) .gt. lopt) .and. (point(1) .lt. hipt)) then
c
c           Tic location lies between the bounding world coord values,
c           so draw tics.
c
            call line (tic1(1), point(1), tic1(2), point(1))
            call line (tic2(1), point(2), tic2(2), point(2))
            if ( lab ) then
               call labtic ('y', yaxis, point(1), tic1(1), tic1(2)) 
            endif
c            call gpl (2, tic1, point)
c            call gpl (2, tic2, point)
c
         else
            if ( lab ) then
               call labtic ('y', yaxis, lopt, tic1(1), tic1(2)) 
               if(hipt .ge. ((point(1) + point(1) - tint)/2)) then
                  call labtic ('y', yaxis, hipt, tic1(1), tic1(2)) 
               endif
            endif
c
c           Tic location is outside bounding world coord values,
c           so we are done.
c
            done = .true.
c
         endif
c
         go to 5
c
      endif
c
c
      return
      end
c
c
      subroutine majxtic (ticint, tic1, tic2, ive_minval_per, 
     &     ive_maxval_per, lab)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine is used to draw tic marks around a plot
c        perimeter.  It is called by subroutine uwperim.
c     Arguments:
c        ticint  real  input  The interval in warsd coordinates between
c                             tic marks.
c        tic1    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the lower or left border of 
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction.  
c        tic2    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the upper or right border of
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction. 
c        ive_minval_per  real  input  The minimum world coordinate value on 
c                             either the lower or left boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        ive_maxval_per  real  input  The maximum world coordinate value on 
c                             either the upper or right boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c-----------------------------------------------------------------------
c
c     Argument derclarations.
c
      logical     lab
      integer     ticint
      real        tic1, tic2, ive_minval_per, ive_maxval_per
      dimension   tic1(2), tic2(2)
c
c
      include 'pltwin.icl'
      include 'units.icl'
c
c     Local variable declarations.
c
      logical   done
      real      EPSLON, hipt, lopt, point, tint
      dimension point(2)
c
c     EXTERNAL FUNCTIONS
#ifndef Linux
      double precision epsilon
#endif
c
c
c
      EPSLON=epsilon(abs( ive_maxval_per - ive_minval_per))
      if(ive_maxval_per-ive_minval_per >0 .and. 
     &     ive_maxval_per-ive_minval_per< 1)EPSLON=EPSLON/3.
c
c     If any tic mark interval is less than 1, return.
c
      if ( ticint .lt. 1 ) then
         return
      endif
c
c     
      point(1) = ive_minval_per
      if(point(1) .le. EPSLON .and. point(1) .ge. 
     &                 (-1 * EPSLON)) point(1) = 0.0
      point(2) = point(1)
c
c     Account for the possibility that the minimum coordinate value
c     is greater than the maximum coordinate value.
c
      if ( ive_minval_per .lt. ive_maxval_per ) then
         lopt = ive_minval_per
         hipt = ive_maxval_per
         tint = ticint
      else
         lopt = ive_maxval_per
         hipt = ive_minval_per
         tint = -1.0 * ticint
      endif
c
      done = .false.
c
c
 5    if ( .not. done ) then
c
c        Tics are drawn at ticint intervals, beginning with ive_minval_per.
c
c         point(1) = point(1) * 10.0**(tint)
         point(1) = point(1) +tint
         if(point(1) .le. EPSLON .and. point(1) .ge. 
     &                 (-1 * EPSLON)) point(1) = 0.0
         point(2) = point(1)
c
         if ( (point(1) .gt. lopt) .and. (point(1) .lt. hipt)  ) then
c
c           Tic location lies between the bounding world coord values,
c           so draw tics.
c
            call line (point(1), tic1(1), point(1), tic1(2))
            call line (point(2), tic2(1), point(2), tic2(2))
            if ( lab ) then
               call labtic ('x', xaxis, point(1), tic1(1), tic1(2))
            endif
c            call gpl (2, point, tic1)
c            call gpl (2, point, tic2)
c
         else
            if ( lab ) then
               call labtic ('x', xaxis, lopt, tic1(1), tic1(2))
               if(hipt .ge. ((point(1) + point(1) - tint)/2)) then
                  call labtic ('x', xaxis, hipt, tic1(1), tic1(2))
               endif
            endif
c
c           Tic location is outside bounding world coord values,
c           so we are done.
c
            done = .true.
c
         endif
c
         go to 5
c
      endif
c
c
      return
      end
c
c
      subroutine majytic (ticint, tic1, tic2, ive_minval_per, 
     &     ive_maxval_per, lab)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine is used to draw tic marks around a plot
c        perimeter.  It is called by subroutine uwperim.
c     Arguments:
c        ticint  real  input  The interval in world coordinates between
c                             tic marks.
c        tic1    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the lower or left border of 
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction.  
c        tic2    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the upper or right border of
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction. 
c        ive_minval_per  real  input  The minimum world coordinate value on 
c                             either the lower or left boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        ive_maxval_per  real  input  The maximum world coordinate value on 
c                             either the upper or right boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c-----------------------------------------------------------------------
c
c     Argument derclarations.
c
      logical     lab
      integer     ticint
      real        tic1, tic2, ive_minval_per, ive_maxval_per
      dimension   tic1(2), tic2(2)
c
c
      include 'pltwin.icl'
      include 'units.icl'
c
c     Local variable declarations.
c
      logical   done
      real      EPSLON, hipt, lopt, point, tint
      dimension point(2)
c
c     EXTERNAL FUNCTIONS
#ifndef Linux
      double precision epsilon
#endif
c
c
c
      EPSLON=epsilon(abs(ive_maxval_per-ive_minval_per))
      if(ive_maxval_per-ive_minval_per >0 .and. 
     &     ive_maxval_per-ive_minval_per< 1)EPSLON=EPSLON/3.
c
c     If any tic mark interval is less than 1, return.
c
      if ( ticint .lt. 1 ) then
         return
      endif
c
c     
      if(abs(ive_minval_per) .le. EPSLON) then
         point(1) = 0.0
      else
         point(1) = ive_minval_per
      endif
      point(2) = point(1)
c
c     Account for the possibility that the minimum coordinate value
c     is greater than the maximum coordinate value.
c
      if ( ive_minval_per .lt. ive_maxval_per ) then
         lopt = ive_minval_per
         hipt = ive_maxval_per
         tint = ticint
      else
         lopt = ive_maxval_per
         hipt = ive_minval_per
         tint = -1.0 * ticint
      endif
c
      done = .false.
c
c
      do while( .not. done )
c
c        Tics are drawn at ticint intervals, beginning with ive_minval_per.
c
c         point(1) = point(1) * 10.0**(tint)
c     This doesn't make sense to users, 
c     they think an interval is in height units.
         point(1) = point(1)+tint
        if(point(1) .le. EPSLON .and. point(1) .ge. 
     &                 (-1 * EPSLON)) point(1) = 0.0
         point(2) = point(1)
c
         if ( (point(1) .gt. lopt) .and. (point(1) .lt. hipt)  ) then
c
c           Tic location lies between the bounding world coord values,
c           so draw tics.
c     
               call line (tic1(1), point(1), tic1(2), point(1))
               call line (tic2(1), point(2), tic2(2), point(2))
            if (lab) then
               call labtic ('y', yaxis, point(1), tic1(1), tic1(2))
            endif
c            call gpl (2, tic1, point)
c            call gpl (2, tic2, point)
c
         else
            if (lab) then
               call labtic ('y', yaxis, lopt, tic1(1), tic1(2))
               if(hipt .ge. ((point(1) + point(1) - tint)/2)) then
                  call labtic ('y', yaxis, hipt, tic1(1), tic1(2))
               endif
            endif
c
c           Tic location is outside bounding world coord values,
c           so we are done.
c
            done = .true.
c
         endif
c
      enddo
c     
c
c
      return
      end
c
c
      subroutine minxtic (ticint, majint, tic1, tic2, ive_minval_per,
     &     ive_maxval_per)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine is used to draw minor tic marks along a 
c        logarithmic plot axis. It is called by subroutine uwperim, 
c        and works like the NCAR Graphics routine perim.
c     Arguments:
c        ticint  int   input  the number of tic marks between each
c                             major tic mark.
c        majint  int   input  the value of *major.
c        tic1    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the lower or left border of 
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction.  
c        tic2    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the upper or right border of
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction. 
c        ive_minval_per  real  input  The minimum world coordinate value on 
c                             either the lower or left boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        ive_maxval_per  real  input  The maximum world coordinate value on 
c                             either the upper or right boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c-----------------------------------------------------------------------
c
c     Argument derclarations.
c
      integer     ticint, majint
      real        tic1, tic2, ive_minval_per, ive_maxval_per
      dimension   tic1(2), tic2(2)
c
c
c     Local variable declarations.
c
      integer   i, sign
      logical   done
      real      EPSLON, hipt, interval, lopt, point
      parameter ( EPSLON = 1.0e-10 )
      dimension point(2)
c
c
c     External Functions
c
c
c
c     If any tic mark interval is less than 1 or greater than 10, 
c     return.
c
      if ( (ticint .lt. 1) .or. (ticint .gt. 10) ) then
         return
      endif
c
c     
      point(1) = ive_minval_per
      point(2) = point(1)
c
c     Account for the possibility that the minimum coordinate value
c     is greater than the maximum coordinate value.
c
      if ( ive_minval_per .lt. ive_maxval_per ) then
         lopt = ive_minval_per
         hipt = ive_maxval_per
         sign = 1
      else
         lopt = ive_maxval_per
         hipt = ive_minval_per
         sign = -1
      endif
c
      done = .false.
      majint = sign * majint
c
c
 5    if ( .not. done ) then
c
c        ticint tics are drawn between each major tic mark.
c
         interval = abs ( (point(1) - point(1) * 10.0**(majint)) /
     &                     float (sign * ticint) )
         do 100 i = 1, ticint
            point(1) = point(1) + float(sign) * interval
            point(2) = point(1)
c
            if ( (point(1) .gt. lopt) .and. (point(1) .lt. hipt)  ) then
c
c             Tic location lies between the bounding world coord values,
c              so draw tics.
c
               call line (point(1), tic1(1), point(1), tic1(2))
               call line (point(2), tic2(1), point(2), tic2(2))
c               call gpl (2, point, tic1)
c               call gpl (2, point, tic2)
c
c
            else
c
c              Tic location is outside bounding world coord values,
c              so we are done.
c  
               done = .true.
               go to 5
c
            endif
c
 100     continue
         go to 5
c
      endif
c
c
      return
      end
c
c
      subroutine minytic (ticint, majint, tic1, tic2, ive_minval_per, 
     &     ive_maxval_per)
c-----------------------------------------------------------------------
c     Purpose: 
c        This subroutine is used to draw minor tic marks along a 
c        logarithmic plot axis. It is called by subroutine uwperim, 
c        and works like the NCAR Graphics routine perim.
c     Arguments:
c        ticint  int   input  the number of tic marks between each
c                             major tic mark.
c        majint  int   input  the value of *major.
c        tic1    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the lower or left border of 
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction.  
c        tic2    real  input  The interval in world coordinates in 
c                             which the tic will be drawn. This will 
c                             either be on the upper or right border of
c                             the plot, depending if we are drawing
c                             tics in the X or Y direction. 
c        ive_minval_per  real  input  The minimum world coordinate value on 
c                             either the lower or left boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c        ive_maxval_per  real  input  The maximum world coordinate value on 
c                             either the upper or right boundary of 
c                             the plot, depending if we are
c                             drawing tics in the X or Y direction. 
c-----------------------------------------------------------------------
c
c     Argument derclarations.
c
      integer     ticint, majint
      real        tic1, tic2, ive_minval_per, ive_maxval_per
      dimension   tic1(2), tic2(2)
c
c
c     Local variable declarations.
c
      integer   i, sign
      logical   done
      real      EPSLON, hipt, interval, lopt, point
      parameter ( EPSLON = 1.0e-10 )
      dimension point(2)
c
c
c     External function declarations.
c
#ifndef Linux
      double precision epsilon
#endif
c
c
c
c
c
c     If any tic mark interval is less than 1 or greater than 10, 
c     return.
c
      if ( (ticint .lt. 1) .or. (ticint .gt. 10) ) then
         return
      endif
c
c     
      point(1) = ive_minval_per
      point(2) = point(1)
c
c     Account for the possibility that the minimum coordinate value
c     is greater than the maximum coordinate value.
c
      if ( ive_minval_per .lt. ive_maxval_per ) then
         lopt = ive_minval_per + (ive_minval_per)
         hipt = ive_maxval_per - (ive_maxval_per)
         sign = 1
      else
         lopt = ive_maxval_per + epsilon(ive_maxval_per)
         hipt = ive_minval_per - epsilon(ive_minval_per)
         sign = -1
      endif
c
      done = .false.
      majint = sign * majint
c
c
 5    if ( .not. done ) then
c
c        ticint tics are drawn between each major tic mark.
c
         interval = abs ( (point(1) - point(1) * 10.0**(majint)) /
     &                     float (sign * ticint) )
         do 100 i = 1, ticint
            point(1) = point(1) + float(sign) * interval
            point(2) = point(1)
c
            if ( (point(1) .gt. lopt) .and. (point(1) .lt. hipt)  ) then
c
c             Tic location lies between the bounding world coord values,
c              so draw tics.
c
               call line (tic1(1), point(1), tic1(2), point(1))
               call line (tic2(1), point(2), tic2(2), point(2))
c               call gpl (2, tic1, point)
c               call gpl (2, tic2, point)
c
            else
c
c              Tic location is outside bounding world coord values,
c              so we are done.
c  
               done = .true.
               go to 5
c
            endif
c
 100     continue
         go to 5
c
      endif
c
c
      return
      end
c
c
      subroutine scaletics (xmajor, xminor, ymajor, yminor, xaxis, 
     &                      yaxis)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine is used to put the tick mark intervals into the
c        correct units based on the physical domain units.
c     Arguments:
c        xmajor  real  input  the major tic mark interval along the plot
c                             abscissa.
c        xminor  real  input  the minor tic mark interval along the plot
c                             abscissa.
c        ymajor  real  input  the major tic mark interval along the plot
c                             ordinate.
c        yminor  real  input  the minor tic mark interval along the plot
c                             ordinate.
c        xaxis   int   input  indicates which physical dimension cor-
c                             responds to the plot abscissa.
c                             xaxis = 0  data.
c                             xaxis = 1  x dimension.
c                             xaxis = 2  y dimension.
c                             xaxis = 3  z dimension.
c                             xaxis = 4  t dimension.
c                             xaxis = 5  some combination of x,y,z,t, as
c                                        in an angled slice.
c        yaxis   int   input  indicates which physical dimension cor-
c                             responds to the plot ordinate.
c                             yaxis = 0  data.
c                             yaxis = 1  x dimension.
c                             yaxis = 2  y dimension.
c                             yaxis = 3  z dimension.
c                             yaxis = 4  t dimension.
c                             yaxis = 5  some combination of x,y,z,t, as
c                                        in an angled slice.
c     History:
c-----------------------------------------------------------------------
c
c
      include 'units.icl'
c
c
c     Argument declarations.
c
      integer     xaxis, yaxis 
      real         xmajor, xminor, ymajor, yminor
c
c
c     Local variable declarations.
c
      integer      status, linlog
      logical      error
      real         xmajr, xminr, ymajr, yminr
      real*8       slope, intercept
c
c
c     External function declarations.
c
      integer      convert_units
c
c
c
c
c     Set initial values.
c
      xmajr = xmajor
      xminr = xminor
      ymajr = ymajor
      yminr = yminor
      status = 0
c     
c
      call getivar ('linlog', linlog, error)
c
c     Scale tic interval based on the plot axes, BUT,
c     only scale tics if the axis is linear (since we assume
c     linear unit scaling.)
c     
c      if ( (linlog .eq. 1) .or. (linlog .eq. 2) ) then
      if (xaxis .eq. 0 .or. xaxis .eq. 5) then
         slope = 1.
         intercept = 0.
c     elseif (xaxis .lt. 5) then
c     doing log tics in x or y space so always scale
      else
         status = convert_units (domain_units(xaxis), 
     &        domain_display_units(xaxis), slope, intercept)
      endif
      if ( status .eq. 0 ) then
c     
c     We actually need to unscale the tic interval because
c     the interval was specified in the domain display units, 
c     but the set call is in terms of the domain units.  The 
c     routine unscale is the inverse of the routine scale.
c     
         call unscale (xmajr, 1, slope, intercept, 0.0)
         call unscale (xminr, 1, slope, intercept, 0.0)
      endif
c     endif
c     if ( (linlog .eq. 1) .or. (linlog .eq. 3) ) then
      if (yaxis .eq. 0 .or. yaxis .eq. 5) then
         slope = 1.
         intercept = 0.
c         elseif (yaxis .lt. 5) then
      else
         status = status + convert_units (domain_units(yaxis), 
     &        domain_display_units(yaxis), slope, intercept)
      endif
      if ( status .eq. 0 ) then
         call unscale (ymajr, 1, slope, intercept, 0.0)
         call unscale (yminr, 1, slope, intercept, 0.0)
      endif
c     endif
c     
c     
c     Return scaled tic intervals only if no errors were encountered
c     in this routine.
c     
      if ( status .eq. 0 ) then
         xmajor = xmajr
         xminor = xminr
         ymajor = ymajr
         yminor = yminr
      endif
c
c
      return
      end
c
c
      subroutine labtic (axis, iaxis, position, ticbeg, ticend)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine labels major tic marks.
c     Arguments:
c        axis      char  input  indicates whether we are labeling the
c                               plot abscissa (x) or ordinate (y).
c        iaxis     int   input  the value of xaxis or yaxis.
c        position  real  input  the world coordinate location of the
c                               tic mark along x or y.
c        ticbeg    real  input  the location of one endpoint of the tic
c                               mark.
c        ticend    real  input  the location of the other endpoint of 
c                               the tic mark.
c     History:
c-----------------------------------------------------------------------
c
      include 'units.icl'
      include 'flags.icl'
      include 'pltwin.icl'
c
c     Argument declarations.
c
      character *(*) axis
      integer        iaxis
      real           position, ticbeg, ticend 
c
c     Local variable declarations.
c
      character *(64) numlab
      integer         arr, clip, error, ibeg, iend, linlog,
     &                outnum,outsig,outpow
      real            xpos, ypos, tmp
      real            center, orient, szsf,
     &                vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt
      dimension arr(4)
c
c     External function declarations.
c
      integer          strbeg, strend
c
c     Get clipping flag and current window coordinates.
c
      call gqclip (error, clip, arr)
      call getset (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
c
c
c     Set character quality flag. Use medium quality characters.
c
c
c     Set a scale factor for label sizes.
c
      szsf = (vpr - vpl) * 0.015 * 0.75
      if (movielabs .and. tic_scale .eq. 1) then
         szsf = szsf * 1.7
      else
         szsf = szsf * tic_scale
      endif
      
c
c     Scale label if needed.
c
      tmp = position
      if (iaxis.ne.0 .and. iaxis .ne. 5) then
         call scale (tmp, 1, domain_slope(iaxis), 
     &        domain_intercept(iaxis), 0.0)
      endif
c
c
c     Convert label to character string, and compute its position.
c
      if(axis .eq. 'x')tmp = tmp
      if(axis .eq. 'y')tmp = tmp
      if(tic_decimals .lt. 0) then
         call cpnumb(tmp,4,-10000,4,0,'E',' ',' ',1,0,0,0,
     &               1,1,numlab,outnum,outsig,outpow)
      call clean_zero(numlab)
      else
         call ftoa_fixpt(tmp, tic_decimals, numlab)
      endif

      ibeg = strbeg(numlab)
      iend = strend(numlab)
      if (axis .eq. 'x' ) then
         xpos = position
         if(movielabs .or. tic_scale .gt. 1.2) then
            ypos = ticbeg - (ticend - ticbeg)*1.5
         else
            ypos = ticbeg - (ticend - ticbeg)*1.005
         endif
         center = 0.0
      elseif (axis .eq. 'y') then
         if(movielabs .or. tic_scale .gt. 1.2) then
            xpos = ticbeg - (ticend - ticbeg )/2
         else
            xpos = ticbeg - (ticend - ticbeg )
         endif
         ypos = position 
         center = 1.0
      else
         go to 10
      endif
      orient = 0.0
c
c
c     Turn off clipping and write label.
c
      call gsclip (0)
      call plchhq (xpos, ypos, numlab(ibeg:iend), szsf, orient, center)
c
c
c     Reset quality and clipping.
c
 10   continue
      call gsclip (clip)
c
c      
      return
      end

