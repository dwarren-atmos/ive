      subroutine setlineplt (nhpts, save_flag, lolim, uplim,
     &     set_flag, error)
c-----------------------------------------------------------------------
c     $Id: setlineplt.F,v 1.1 2002/11/04 18:40:17 warren Exp $
c     Purpose:
c        This routine makes the set call for all 1D slices. 
c     Arguments:
c        nhpts   int   input   the number of points along the first
c                              dimension of the slice. Meaningful only
c                              if the slice is vertical.
c        save_flag  logi  input   indicates whether or not the current plot
c                              is saved (used to overlay plots). 
c        lolim   real  output  array containing the plot window
c                              coordinates of the lower left corner.
c        uplim   real  output  array containing the plot window
c                              coordinates of the upper right corner.
c        set_flag int  input   -1 => do not call set
c     History:
c     $Log: setlineplt.F,v $
c     Revision 1.1  2002/11/04 18:40:17  warren
c     Need to use .F for cpp on sun
c
c     Revision 1.33  2002/08/09 19:58:00  warren
c     Fixed for Linux and tested with DEC sun etc.
c     most items inside #ifdef's, or were general cleanup/fixes
c
c     Revision 1.32  2002/06/11 17:26:17  harryive
c     Change aspect ratio to affect the plot, not just printing.
c
c     Add first part of contour value specification.
c
c     Revision 1.31  2000/05/09 19:54:02  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
c     Revision 1.30  1997/05/16 22:15:18  harry
c     More mouse slicing fixes.
c
c     Revision 1.29  1997/05/07 18:43:07  harry
c     Mouse lines are drawn with same colors/patterns as normal lines.
c
c     Revision 1.28  1996/08/15 18:52:54  harry
c     Fix ordinate processing for overlays.
c
c Revision 1.27  1995/08/25  21:50:39  harry
c Add in a call to getset for overlays.
c
c Revision 1.26  1995/01/09  18:52:45  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.25  1994/06/11  21:33:45  yuri
c Added more comments.
c
c Revision 1.24  1994/04/14  01:14:57  warren
c Fixed problem where doing overlays in line plots changes the current line
c type for contours.
c
c Revision 1.23  1994/03/30  22:47:32  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.22  1994/03/30  22:19:43  harry
c Change the way 1D plots handle user specified units.  The data is
c scaled instead of trying to scale the labels.  This is the same way 2D
c plots are handled.
c
c Revision 1.21  1994/01/24  19:34:02  yuri
c Correction in opevrlay patterns routines.
c
c Revision 1.20  1994/01/20  20:44:56  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.19  1993/12/13  23:47:54  warren
c fixed line plots so the box get drawn around them. also added a proof option
c to the makefile.
c
c Revision 1.18  1993/10/27  16:41:02  yuri
c Added stuff for checking log(negative limits) in 1D and 2D cases
c with separate messages for horisontal and vertical axes.
c
c Revision 1.17  1993/10/26  22:16:40  warren
c permit e notation in text boxes and remove duplicate declaration of error.
c
c Revision 1.16  1993/10/26  18:29:13  yuri
c Corrected log(negative number) problem, reported by PABLO.
c
c Revision 1.15  1993/10/18  23:07:30  warren
c updates from 3.0 version.
c
c Revision 1.14  1993/10/13  00:04:25  warren
c Update current version for fixes to 3.0 version related to the meaning of
c default in the property sheet boxes.
c
c Revision 1.13  1993/07/09  19:09:36  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.12  1993/07/06  18:21:17  yuri
c Corrected problem with terrain 1D plot.
c
c Revision 1.11  1993/07/06  14:58:38  yuri
c 1D colors initial set up corrected.
c
c Revision 1.10  1993/07/03  01:53:27  yuri
c Tuning of overlay colors.
c
c Revision 1.9  1993/07/01  19:35:04  yuri
c Corrected error in multiple overlay generating with mix
c vector/scalar plots.
c
c Revision 1.8  1993/06/29  17:35:10  yuri
c Problem with top limit = bottom limit in setlineterr solved.
c (No more weird lines on 1D plots without transform routines).
c
c Revision 1.7  1993/06/24  22:14:31  yuri
c 	Corrected bugs in overlays scaling + cleaning up.
c
c Revision 1.6  1993/06/11  18:21:54  yuri
c New colors for overlay.
c
c Revision 1.5  1993/06/09  22:33:01  yuri
c 	Callback from linecolor command corrected.
c
c Revision 1.4  1993/06/01  19:57:16  yuri
c Second try.
c
c Revision 1.2  1993/05/24  00:12:40  yuri
c Overlay colors + frozen limits added.
c
c Revision 1.1.1.1  1993/04/07  19:40:25  harry
c Initial CVS checkin of IVE.
c
c Revision 2.11  1993/02/02  20:30:56  warren
c changed so overlay warning only occurs if you do not set the range on
c the dependant var.
c
c Revision 2.10  1993/01/05  00:34:31  millerp
c Produce warning if overlays are made and plot limits are not set.
c
c Revision 2.9  1992/11/05  20:30:28  millerp
c Added stuff for averages and time-dependent cross sections.
c
c Revision 2.8  1992/10/27  23:11:01  millerp
c Fix up scaling errors.
c
c Revision 2.7  1992/10/15  23:01:21  warren
c fixed it so when you set the axes limits it assumes they are scaled.
c
c Revision 2.6  1992/10/02  22:58:59  warren
c changed the commonblock window_points to specify witch axis is what
c and put it in pltwin.icl
c
c Revision 2.5  1992/09/30  23:07:57  warren
c fixed gsetwindow()   51 Rectangle definition is invalid
c
c Revision 2.4  1992/09/30  18:23:03  warren
c fixes for labeling and 0 sized plot boxes
c
c Revision 2.3  1992/09/29  22:18:30  warren
c fixed scaling
c
c Revision 2.2  1992/09/29  20:33:16  warren
c forgot to get get topo_scale
c
c Revision 2.1  1992/09/29  20:30:30  warren
c complete rewrite to match new line slicer and added setlineterr for terrain
c 
c
c Revision 2.0  1992/07/01  21:14:28  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/05/05  15:30:28  millerp
c Added stuff for sounding plots.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'data.icl'
      include 'deflts.icl'
      include 'lineplt.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
      include 'units.icl'
      include 'message.icl'
      include 'color.icl'
      include 'incommands.icl'
      include 'segments.icl'
      include 'flags.icl'
      include 'ovl_props.icl'
c
c     Argument declarations.
c
      integer         nhpts, set_flag
      logical         save_flag
      real            lolim, uplim, min, max
      dimension       lolim(MAXDIM), uplim(MAXDIM)
c
c     stuff to save previous state
c
      logical        old_savflg,! was is "overlay" before?
     &     l_was1d,             ! was it 1D plot?
     &     error                ! error flag
c
c     old limits
c
      real  old_wleft, old_wrght, old_wbot, old_wtop
c
c     old axes, and color
c
      integer old_axes, old_color, old_xaxis, old_yaxis
      common /old_lim/  old_savflg,
     & old_wleft, old_wrght, old_wbot, old_wtop, old_axes, old_color,
     &  old_xaxis, old_yaxis
      common / was1d / l_was1d
c     external functions
#ifndef Linux
      double precision epsilon
#endif
c
c     Local variable declarations.
c
      integer         axes
      real            vbot, vtop, vleft, vrght, 
     &                wbot, wtop, wleft, wrght 
c
c
      real                dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      common / voffsets / dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
c
c     External functions
c
c
c
c     is it overlay or new plot?
c
      if ( savflg ) then
c
c----- overlay overlay after overlay -------------
c
         if (set_flag .ne. -1) then
            call getset (vleft, vrght, vbot, vtop, 
     &           wleft, wrght, wbot, wtop, axes)
            if(old_xaxis .eq. xaxis)then
               wleft  =  old_wleft
               wrght  =  old_wrght
            else
               wleft  =  x1
               wrght  =  x2
            endif
c
c     if axes are the same - it's simple
c
            if(old_yaxis  .eq. yaxis)then
               wbot   = old_wbot
               wtop   = old_wtop 
            else
               wbot   = y1
               wtop   = y2
            endif
c
c
            axes   = old_axes
         endif
c
c
c
      else
c
c        "NEW PLOT" case
c
         vleft = VPLEFT_LINE
         vrght = VPRIGHT_LINE
         vbot  = VPBOT_LINE
         vtop  = VPTOP_LINE
         axes  = linlog
         if ( aratio .gt. 1.0) then
            vtop = vbot + (vtop-vbot)/aratio
         else if ( aratio .lt. 1.0) then
            vrght = vleft + (vrght-vleft)*aratio
         endif
c
c     it's all brand new, just starting
c
      endif
      if (set_flag .eq. -1) return
c
c     Set plot limits for set call.
c     
      lolim(1) = xbeg
      lolim(2) = ybeg
      lolim(3) = zbeg
      lolim(4) = tbeg
      uplim(1) = xend
      uplim(2) = yend
      uplim(3) = zend
      uplim(4) = tend
c
c     NO, I did not do this - YY
c
      if( .not. savflg ) then

         call getrvar('abscissa_min', min, error)
         call getrvar('abscissa_max', max, error)
         if(min .ge. bigone - epsilon(bigone)) then
            wleft = x1
         else
            if  (xaxis.ne.0 .and. xaxis .ne. 5) then
               call unscale (min, 1, domain_slope(xaxis), 
     &              domain_intercept(xaxis), 0.0)
            endif
            wleft = min
         endif
         if(max .eq. bigone) then
            wrght = x2
         else
            if  (xaxis.ne.0 .and. xaxis .ne. 5) then
               call unscale (max, 1, domain_slope(xaxis), 
     &              domain_intercept(xaxis), 0.0)
            endif
            wrght = max
         endif
         call getrvar('ordinate_min', min, error)
         call getrvar('ordinate_max', max, error)
         if(min .eq. bigone) then 
            wbot  = y1
         else
            if (yaxis.ne.0 .and. yaxis .ne. 5) then
               call unscale (min, 1, domain_slope(yaxis), 
     &              domain_intercept(yaxis), 0.0)
            endif
            wbot  = min
         endif
         if(max .ge. bigone - epsilon(bigone)) then 
            wtop  = y2
         else
            if (yaxis.ne.0 .and. yaxis .ne. 5) then
               call unscale (max, 1, domain_slope(yaxis), 
     &              domain_intercept(yaxis), 0.0)
            endif
            wtop  = max
         endif
      endif
      if(wleft .eq. wrght) then
         if(wrght .eq. 0.0)then
            wrght =  1
            wleft = -1 
         else 
            wrght = 1.5 * wrght
            wleft = 0.5 * wleft
         endif
      endif
      if(wbot .eq. wtop)then
         if(wtop .eq. 0.0)then
            wtop = 1
            wbot= -1
         else 
            wtop = 1.5 * wtop
            wbot = 0.5 * wbot
         endif
      endif
c      
      x1 = wleft
      x2 = wrght
      y1 = wbot
      y2 = wtop
      if(wbot*wtop .le. 0. .and.(axes .eq. 2 .or. axes .eq. 4))then
         error = .true.
         write (message, 130)
         call write_message
         return
      endif
      if(wleft*wrght.le. 0. .and.(axes .eq. 2 .or. axes .eq. 3))then
         error = .true.
         write (message, 140)
         call write_message
         return
      endif
c
      call set (vleft, vrght, vbot, vtop, 
     &     wleft, wrght, wbot, wtop, axes)
c     new stuff to remember limitis for overlay plot
      if(.not. savflg ) then
         old_savflg =  savflg
         old_wleft  =  wleft
         old_wrght  =  wrght
         old_wbot   =  wbot
         old_wtop   =  wtop
         old_axes   = axes
         old_xaxis = xaxis
         old_yaxis = yaxis
         l_was1d = .true.
       endif
c
 130  format (' You cannot use log. scale with negative vert. limits')
 140  format (' You cannot use log. scale with negative hor. limits')
c
      return
      end
c
c
c
      subroutine setlineterr (xpts, ypts, num)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine makes the set call for terrain when plotting
c        lines
c     Arguments:
c     xpts                     array of values on the horizontal axis
c                              is saved (used to overlay plots). 
c     ypts                     array of values on the vertical axis
c     num                      number of points in the arrays
c-----------------------------------------------------------------------
c
c
c
c     Argument declarations.
c
      integer         num
      real            xpts, ypts, topo_scale
      dimension       xpts(num), ypts(num)
      logical         error
c
c
c     Local variable declarations.
c
      integer         axes, i
      real            vbot, vtop, vleft, vrght, 
     &     wbot, wtop, wleft, wrght, tmp
      
      call getset ( vleft, vrght, vbot, vtop, 
     &     wleft, wrght, wbot, wtop, axes)

      wbot = ypts(1)
      wtop = ypts(1)
      do i = 2, num
         if(wtop .lt. ypts(i)) then
            wtop = ypts(i)
         elseif(wbot .gt. ypts(i)) then
            wbot = ypts(i)
         endif
      enddo
      if ( wtop .ne. wbot ) then
c         return
c      else
         call getrvar('topo_scale', topo_scale, error) 
         if(topo_scale .ne. 0) then
            tmp = wtop - wbot
            tmp = tmp * 100.0/topo_scale
            wtop = wbot + tmp
         else
            tmp = wtop - wbot
            tmp = tmp * 5
            wtop = wbot + tmp
         endif
      endif
      if(wbot .eq. wtop)wtop = wbot + 1
      call set (vleft, vrght, vbot, vtop, 
     &        wleft, wrght, wbot, wtop, axes)
c
      return
      end
C
C
C
      subroutine set_up_old_lim
C     In the case of overlay we need to pop up older parameters
c-----------------------------------------------------------------------
c
c
      include 'data.icl'
      include 'deflts.icl'
      include 'lineplt.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
      include 'units.icl'
      include 'message.icl'
      include 'color.icl'
c
c     stuff to save previous state
c
      logical        old_savflg
      real  old_wleft, old_wrght, old_wbot, old_wtop
      integer old_axes, old_color, old_xaxis, old_yaxis
      common /old_lim/  old_savflg,
     & old_wleft, old_wrght, old_wbot, old_wtop, old_axes, old_color,
     &  old_xaxis, old_yaxis

      include 'incommands.icl'
      include 'ovl_props.icl'
c
c     Local variable declarations.
c
      integer         axes
c      logical         error
c      real            vbot, vtop, vleft, vrght, 
       real wbot, wtop, wleft, wrght 
c
c----- overlay after new or  overlay after overlay -------------
c
         wleft  =  old_wleft
         wrght  =  old_wrght
         wbot   = old_wbot
         wtop   = old_wtop 
         axes   = old_axes
c
         RETURN
         END
