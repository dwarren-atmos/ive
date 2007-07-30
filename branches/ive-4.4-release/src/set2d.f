      subroutine set2d (nhpts, mapflg, savflg, lolim, uplim,error)
c-----------------------------------------------------------------------
c     $Id: set2d.f,v 1.29 2007-07-30 21:59:09 reinecke Exp $
c     Purpose:
c        This routine makes the set call for all 2D slices. It also sets
c        the mapping flag needed in subroutine cpmpxy.
c     Arguments:
c        nhpts   int   input   the number of points along the first
c                              dimension of the slice. Meaningful only
c                              if the slice is vertical.
c        mapflg  logi  input   indicates whether or not the plot is 
c                              projected onto a map background. 
c        savflg  logi  input   indicates whether or not the current plot
c                              is saved (used to overlay plots). 
c        lolim   real  output  array containing the plot window
c                              coordinates of the lower left corner.
c        uplim   real  output  array containing the plot window
c                              coordinates of the upper right corner.
c     History:
c     $Log: set2d.f,v $
c     Revision 1.29  2007-07-30 21:59:09  reinecke
c     Fixed clipping bug instituted by clipping fix.
c
c     Revision 1.28  2007-07-27 19:43:19  warren
c     fix clipping in 2d contour plot. I don't know why it was commented out.
c
c     Revision 1.27  2002/06/11 17:26:17  harryive
c     Change aspect ratio to affect the plot, not just printing.
c
c     Add first part of contour value specification.
c
c     Revision 1.26  2000/06/12 18:38:40  harry
c     Fix crash with no contours and windowing problem.
c
c     Revision 1.25  1997/03/14 22:26:34  warren
c     Added new command map_window to allow for mouse windowing on non
c     equadistant or default maps.
c
c     Revision 1.24  1997/03/06 19:00:01  harry
c     Make changes to properly handle exact_fit case for mouse slicing.
c
c     Revision 1.23  1997/03/06 16:15:21  harry
c     Error in set calls for overlays with maps.
c
c     Revision 1.22  1997/02/21 21:17:09  warren
c     Final fixes for point/mouse slicing and expanding into 2d. It is tested
c     with the z dimention free and seems to work with others, but still should
c     be looked at with known data for this.
c
c Revision 1.21  1995/01/09  18:52:37  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.20  1994/06/11  21:33:40  yuri
c Added more comments.
c
c Revision 1.19  1994/01/29  00:11:40  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.18  1994/01/20  20:44:44  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.17  1993/12/15  18:15:38  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.16  1993/11/19  18:05:25  yuri
c Propagation of corrections from IVE 3.1.
c Tell conpack in "set2d" that set has already been called.
c
c Revision 1.15  1993/10/27  16:40:59  yuri
c Added stuff for checking log(negative limits) in 1D and 2D cases
c with separate messages for horisontal and vertical axes.
c
c Revision 1.14  1993/07/09  19:09:35  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.13  1993/07/06  18:21:15  yuri
c Corrected problem with terrain 1D plot.
c
c Revision 1.12  1993/07/03  01:53:21  yuri
c Tuning of overlay colors.
c
c Revision 1.11  1993/07/01  19:34:58  yuri
c Corrected error in multiple overlay generating with mix
c vector/scalar plots.
c
c Revision 1.10  1993/06/24  22:49:15  yuri
c Correction special case of fun(z) during overlay plot.
c
c Revision 1.9  1993/06/24  22:14:24  yuri
c 	Corrected bugs in overlays scaling + cleaning up.
c
c Revision 1.8  1993/06/18  20:22:55  yuri
c color stuff updated.
c
c Revision 1.7  1993/06/11  18:21:35  yuri
c New colors for overlays.
c
c Revision 1.6  1993/06/09  22:34:02  yuri
c Callback implementation corrected.
c
c Revision 1.5  1993/06/01  19:55:54  yuri
c Second try.
c
c Revision 1.3  1993/05/24  00:43:11  yuri
c Deleted unusable variables.
c
c Revision 1.2  1993/05/24  00:07:06  yuri
c Limits are frozen in overlay case.
c
c Revision 1.1.1.1  1993/04/07  19:40:18  harry
c Initial CVS checkin of IVE.
c
c Revision 2.4  1992/11/05  20:30:28  millerp
c Added stuff for averages and time-dependent cross sections.
c
c Revision 2.3  1992/10/02  22:58:59  warren
c changed the commonblock window_points to specify witch axis is what
c and put it in pltwin.icl
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:53:31  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:56  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'deflts.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
      include 'incommands.icl'
      include 'color.icl'
      include 'message.icl'
      include 'supma.icl'

c     Argument declarations.
c
      integer         nhpts
      logical         mapflg, savflg
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
c
c     Local variable declarations.
c
      integer         axes
      logical         movielabs
      real            lolim, uplim ! min, max limits
c
c     limits to put on plot and in actual window
c
      real            vbot, vtop, vleft, vrght, 
     &                wbot, wtop, wleft, wrght 
c
c
c
      dimension       lolim(MAXDIM), uplim(MAXDIM)
c
      real                dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      common / voffsets / dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
c     
      call getlvar('movielabs',movielabs,error)
      if(error) then
         return
      endif
      axes = linlog
c     quick dirty fix since we only allow linlin with diagnols
      if(xaxis .eq. 5) then
         if(axes .eq. 3)axes=1
         if(axes .eq. 4)axes=2
      endif
      if(yaxis .eq. 5) then
         if(axes .eq. 2)axes=1
         if(axes .eq. 4)axes=3
      endif
      if ( mapflg) then
         if (defmap .and. exact_fit) then
            if (.not. savflg) then
c
c     Make first overlay fits within the map.
c
               if(mapwin_u1 .ne. 0.0 .or. mapwin_u2 .ne. 0.0
     &              .or. mapwin_v1 .ne. 0.0 .or. mapwin_v2 .ne. 0.0)
     &              then
                  call cpseti ('MAP - mapping flag', 2)
               else
                  call getset ( vleft, vrght, vbot, vtop, 
     &                 wleft, wrght, wbot, wtop, axes)
                  call set ( vleft, vrght, vbot, vtop,
     &                 x1, x2, y1, y2, axes)
                  call cpseti ('MAP - mapping flag', 2)
c     
c     This call to mappos forces mapdrw to call mapint again to make
c     the proper set all.
c     
                  call mappos(vleft, vrght, vbot, vtop)
               endif
            endif
         else
c     
c     Set correct mapping flag for cpmpxy.
c     
            call cpseti ('MAP - mapping flag', 1)
         endif
c     
      else
c     
c     if it's overlay - get old limits
c
         if ( savflg ) then
            call getset ( vleft, vrght, vbot, vtop, 
     &           wleft, wrght, wbot, wtop, axes)
            axes = linlog
c
c----- overlay after new or  overlay after overlay -------------
c
            if(old_xaxis .eq. xaxis)then
               wleft  =  old_wleft
               wrght  =  old_wrght
            else
               wleft  =  x1
               wrght  =  x2
            endif
            if(old_yaxis  .eq. yaxis)then
               wbot   = old_wbot
               wtop   = old_wtop 
            else
               wbot   = y1
               wtop   = y2
            endif
            if( l_was1d )then
c     it means that we after 1d case + overlay
               if(old_xaxis .eq. xaxis)then
                  wleft  =  old_wleft
                  wrght  =  old_wrght
               endif
               if(old_yaxis  .eq. yaxis)then
                  wbot   = old_wbot
                  wtop   = old_wtop 
               else
                  wbot   = y1
                  wtop   = y2
               endif
c               wbot   = y1
c               wtop   = y2
            endif
         else
c
c     "new plot" stuff
c
            if(movielabs) then
               vleft = VPLEFT_BIG
               vrght = VPRIGHT_BIG
               vbot  = VPBOT_BIG
               vtop  = VPTOP_BIG
            else if(xmajor .gt. 0 .or. ymajor .gt. 0) then
               vleft = VPLEFT_TIC
               vrght = VPRIGHT_TIC
               vbot  = VPBOT_TIC
               vtop  = VPTOP_TIC
            else
               vleft = VPLEFT
               vrght = VPRIGHT
               vbot  = VPBOT
               vtop  = VPTOP
            endif
            if ( aratio .gt. 1.0) then
               vtop = vbot + (vtop-vbot)/aratio
            else if ( aratio .lt. 1.0) then
               vrght = vleft + (vrght-vleft)*aratio
            endif
c     
c     restore colors in lows, highs, if newplot.
c
         endif
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

         if( .not. savflg ) then
            wleft = x1
            wrght = x2
            wbot  = y1
            wtop  = y2
         endif
         axes = linlog
c
c     may be top limit is smaller then bottom - special case
c
      if(wbot*wtop .le. 0. .and.(axes .eq. 2 .or. axes .eq. 4))then
         error = .true.
         write (message, 130)
         call write_message
         return
      endif
c
c     same about right/left limits
c
      if(wleft*wrght.le. 0. .and.(axes .eq. 3 .or. axes .eq. 4))then
         error = .true.
         write (message, 140)
         call write_message
         return
      endif
c
c     actual set call
c
         call set (vleft, vrght, vbot, vtop, 
     &     wleft, wrght, wbot, wtop, axes)
         window_axes = linlog
c
c     set flag to show, that "set" call is completed.
c
         call cpseti ('SET - do-set-call flag', 0)
c
c     new stuff to remember limitis for overlay plot
c
         if(.not. savflg ) then
            old_savflg =  savflg
            old_wleft  =  wleft
            old_wrght  =  wrght
            old_wbot   = wbot
            old_wtop   = wtop 
            old_axes   = axes
            old_xaxis = xaxis
            old_yaxis = yaxis
         endif
c
c        Set correct mapping flag for cpmpxy.
c     
         call cpseti ('MAP - mapping flag', 2) 
      endif
c     
c     
      return
 130  format (' You cannot use log. scale with negative vert. limits')
 140  format (' You cannot use log. scale with negative hor. limits')
      end
