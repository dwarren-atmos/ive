      subroutine setwin (error)
c-----------------------------------------------------------------------
c     $Id: setwin.f,v 1.4 1997/04/25 20:20:38 warren Exp $
c     Purpose:
c        This subroutine sets the plot window size in the x, y, and z
c           directions. New values for *wmin and *wmax are found in 
c           comand(fstval) through comand(fstval+5). If none of these 
c           values are present, the window is set to its maximum size.
c           If any of these are present, *wmin or *wmax are reset to 
c           these new values.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setwin.f,v $
c      Revision 1.4  1997/04/25 20:20:38  warren
c      Keep a stack of the last 10 windows used for plotting in order to implement
c      the window=up command.
c
c      Revision 1.3  1997/04/24 19:47:04  harry
c      Make sure min is to the left of max on all windows.
c
c Revision 1.2  1993/07/09  19:09:39  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.1.1.1  1993/04/07  19:40:37  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:15:09  millerp
c Released version rereving to 2.0
c
c Revision 1.5  1992/06/15  19:02:07  millerp
c Change argument list for scale.
c
c Revision 1.4  1992/06/10  20:17:04  millerp
c Calculate scaled and unscaled window quantities.
c
c Revision 1.3  1992/06/02  15:24:34  millerp
c Eliminated debugging print statement.
c
c Revision 1.2  1992/06/01  23:49:37  millerp
c Added stuff for fourth dimension.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'data.icl'
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical       error
c
c
c     Local variable declarations.
c
      integer       i, numval, nxtval
      logical       set_min, set_max, adjust, some_adjust
c
c
c     External function declarlations.
c
      integer       nblank, strbeg, strend
c
c
c 
c
c     Find out how many nonblank values are specified. If there are no
c     nonblank values, set the plot window to its maximum size and 
c     return.
c
      numval = 0
      do 5 i = fstval, maxcom
         if ( nblank(comand(i)) .ne. 0 )  numval = numval + 1
 5    continue
      if ( numval .eq. 0 ) then
         do i = 1, MAXDIM
            plwmin(i) = plmin(i)
            plwmax(i) = plmax(i)
            plwmin_scaled(i) = plmin(i)
            plwmax_scaled(i) = plmax(i)
            call scale (plwmin_scaled(i), 1, domain_slope(i), 
     &                  domain_intercept(i), 0.0)
            call scale (plwmax_scaled(i), 1, domain_slope(i), 
     &                  domain_intercept(i), 0.0)
         enddo
         go to 45
      endif
c
c     Check special case of 'up' first
c
      if ( comand(fstval)(1:2) .eq. 'UP' .or.
     &     comand(fstval)(1:2) .eq. 'up' ) then
         call retreive_window(plwmin,plwmax)
         do i=1,4
            plwmin_scaled(i) = plwmin(i)
            plwmax_scaled(i) = plwmax(i)
            call scale (plwmin_scaled(i), 1, domain_slope(i), 
     &           domain_intercept(i), 0.0)
            call scale (plwmax_scaled(i), 1, domain_slope(i), 
     &           domain_intercept(i), 0.0)
         enddo

         go to 45
      endif
         
      
      nxtval = fstval
      some_adjust = .false.
      do i = 1, 4
         set_min = .false.
         set_max = .true.
c
c     Set plot window min and max along the axis.
c
         if ( comand(nxtval)(1:cmdlen) .ne. ' ' ) then
            call cvatof ( comand(nxtval)(strbeg(comand(nxtval)):
     &           strend(comand(nxtval))),
     &           plwmin_scaled(i), error) 
            if ( error ) then
               write (message, 100) comand(nxtval)
     &              (strbeg(comand(nxtval)):strend(comand(nxtval)))
     &              ,comand(1)(strbeg(comand(1)):strend(comand(1)))
               call write_message
               go to 45
            endif
            set_min = .true.
         endif
         nxtval = nxtval + 1
c
         if ( comand(nxtval)(1:cmdlen) .ne. ' ' ) then
            call cvatof ( comand(nxtval)(strbeg(comand(nxtval)):
     &           strend(comand(nxtval))),
     &           plwmax_scaled(i), error )
            if ( error ) then
               write (message, 100) 
     &              comand(nxtval)(strbeg(comand(nxtval)):
     &              strend(comand(nxtval)))
     &              ,comand(1)(strbeg(comand(1)):strend(comand(1)))
               call write_message
               go to 45
            endif 
            set_max = .true.
         endif
         nxtval = nxtval + 1
c
c     check values to make sure they are in the correct order.
c
         adjust = .false.
         if (set_min .or. set_max) then
            if (plmin_scaled(i) .lt. plmax_scaled(i)) then
               if (plwmin_scaled(i) .gt. plwmax_scaled(i)) then
                  adjust = .true.
               endif
            else if (plwmin_scaled(i) .lt. plwmax_scaled(i)) then
               adjust = .true.
            endif
            if (adjust) then
               some_adjust = .true.
               if (set_min .and. set_max) then
                  plwmin_scaled(i) = plmin_scaled(i)
                  plwmax_scaled(i) = plmax_scaled(i)
               else if (set_min) then
                  plwmax_scaled(i) = plwmin_scaled(i) +
     &                 0.01*(plmax_scaled(i)-plmin_scaled(i))
               else
                  plwmin_scaled(i) = plwmax_scaled(i) -
     &                 0.01*(plmax_scaled(i)-plmin_scaled(i))
               endif
            endif
         endif
      enddo
      if (some_adjust) then
         write(message, 110) comand(1)
     &        (strbeg(comand(1)):strend(comand(1)))
         call write_message
      endif
c
c
c
      do i = 1, MAXDIM
         plwmin(i) = plwmin_scaled(i)
         plwmax(i) = plwmax_scaled(i)
         call unscale (plwmin(i), 1, domain_slope(i), 
     &                 domain_intercept(i), 0.0)
         call unscale (plwmax(i), 1, domain_slope(i), 
     &                 domain_intercept(i), 0.0)
      enddo
c     
c
 45   continue
c 45   if ( .not. error ) then
c
c
c        Make sure window is legitimate.
c
c         call chkwin
c      endif
c
c
c     Format statements.
c
 100  format (1x,a,' is an incorrect value for command ',a,'.')
 110  format (1x, 'Some values out of order for ',a,
     &     ' and have been adjusted')
c
c
      return
      end



