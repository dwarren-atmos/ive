      subroutine chkwin (flag)
c-----------------------------------------------------------------------
c     $Id: chkwin.f,v 1.8 1997/04/09 21:43:41 warren Exp $
c     Purpose:
c        This routine checks to make sure that the requested plot 
c        window size does not exceed the maximum domain size along
c        any axis. If it does the plot window size is reset to the 
c        maximum domain size along that axis. 
c     Arguments:
c        None.
c     History:
c        $Log: chkwin.f,v $
c        Revision 1.8  1997/04/09 21:43:41  warren
c        don't call init_dim_atts under any circumstances. Whos to say when there are
c        too many tic marks.
c
c        Revision 1.7  1997/03/18 20:45:52  warren
c        Only call init_dim_atts if reset is true and is a new file (so dim atts
c        are wrong)
c
c Revision 1.6  1993/07/09  19:09:23  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.5  1993/07/09  16:33:21  harry
c Add changes to properly handle windowing when changing data files.
c
c Revision 1.4  1993/06/01  19:49:41  yuri
c Second attempt.
c
c Revision 1.2  1993/05/21  17:57:28  yuri
c If user changed files, then message about new limits in slicer shows
c only once.
c
c Revision 1.1.1.1  1993/04/07  19:39:49  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1993/02/01  23:42:56  warren
c added call to init_dims if the window needs to be reset.
c
c Revision 2.0  1992/07/01  21:12:39  millerp
c Released version rereving to 2.0
c
c Revision 1.5  1992/06/26  00:14:24  millerp
c Fixed so that window is not reset if plmin == plmax.
c
c Revision 1.4  1992/06/10  20:04:11  millerp
c Eliminate error message due to rounding.
c
c Revision 1.3  1992/06/02  15:24:34  millerp
c Eliminated debugging print statement.
c
c Revision 1.2  1992/06/01  23:49:15  millerp
c Added stuff for fourth dimension.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'message.icl'
      include 'units.icl'
      include 'ive_widgets.icl'
c
c      Argument declarations
c
      logical flag
c
c
c     Local variable declarations.
c
      integer i
      logical reset
c
      reset = .false.
c
c
      do 10 i = 1, MAXDIM
c
c        Check minimum window settings first.
c
         if ( plmin(i) .lt. plmax(i) ) then
            if ( plwmin(i) .lt. plmin(i) ) then
               reset = .true.
               plwmin(i) = plmin(i)
            endif
            if ( plwmin(i) .gt. plmax(i) ) then
               reset = .true.
               plwmin(i) = plmin(i)
            endif
         elseif  ( plmin(i) .gt. plmax(i) ) then
            if ( plwmin(i) .gt. plmin(i) ) then
               reset = .true.
               plwmin(i) = plmin(i)
            endif 
            if ( plwmin(i) .lt. plmax(i) ) then
               reset = .true.
               plwmin(i) = plmin(i)
            endif 
         endif
c
c        Check maximum window settings.
c
         if ( plmin(i) .lt. plmax(i) ) then
            if ( plwmax(i) .gt. plmax(i) ) then
               reset = .true.
               plwmax(i) = plmax(i)
            endif
            if ( plwmax(i) .lt. plmin(i) ) then
               reset = .true.
               plwmax(i) = plmax(i)
            endif
         elseif ( plmin(i) .gt. plmax(i) ) then
            if ( plwmax(i) .lt. plmax(i) ) then
               reset = .true.
               plwmax(i) = plmax(i)
            endif
            if ( plwmax(i) .gt. plmin(i) ) then
               reset = .true.
               plwmax(i) = plmax(i)
            endif
         endif
         if (reset) then
            plwmax_scaled(i) = plwmax(i)
            call scale(plwmax_scaled(i), 1, domain_slope(i),
     &           domain_intercept(i), 0.0)
            plwmin_scaled(i) = plwmin(i)
            call scale(plwmin_scaled(i), 1, domain_slope(i),
     &           domain_intercept(i), 0.0)
         endif
 10   continue
c
c
c     If plot window request fell outside of the plot domain, then 
c     re-set plot window.
c
            
      if ( reset) then
         if(.not. flag) then
            write (message,100) char(10),
     &           plwmin_scaled(1), plwmax_scaled(1),
     &           plwmin_scaled(2), plwmax_scaled(2),
     &           plwmin_scaled(3), plwmax_scaled(3),
     &           plwmin_scaled(4), plwmax_scaled(4)
            call write_message
         endif
         call ui_update(WINDOW_UI)
      endif
c
c
c     Format statements.
c
 100  format (1x,'Specified plot window exceeds data domain. ',
     &           'Plot window has been re-set to :',a,
     &        1x,7(g12.6,','),g12.6 )
c
c
      return
      end


