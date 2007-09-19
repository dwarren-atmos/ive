      subroutine map_win (error)
c-----------------------------------------------------------------------
c     $Id: setmwin.f,v 1.1 1997/03/14 22:26:34 warren Exp $
c     Purpose:
c        This subroutine sets u1,u2 v1,v2 for windowing in map space
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setmwin.f,v $
c      Revision 1.1  1997/03/14 22:26:34  warren
c      Added new command map_window to allow for mouse windowing on non
c      equadistant or default maps.
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'units.icl'
      include 'supma.icl'
      include 'ive_widgets.icl'
c
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      real :: mapwin_tmp(4),u1,u2,v1,v2
      integer       i, ii, numval
      character(len=80) :: wincmd
      logical mapflg
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
      error = .false.

      call getlvar('mapflg', mapflg, error)
      if(.not.mapflg.or.error) then
        error=.true.
        write (message,130) trim(comand(1))
        call write_message
        goto 90
      end if
c
c     Get the first value for the current command.
c
      numval = 0
      do 5 i = fstval, 5
         if ( nblank(comand(i)) .ne. 0 )  numval = numval + 1
 5    continue

      if(numval .lt. 4 ) then
         error = .true.
         write (message,'(A,1x,A)')
     &       'map_window requires 4 arguments',
     &       '(map_window=lon_ll,lon_ur,lat_ll,lat_ur)'
         call write_message
         goto 90
      endif

      ! Make sure a values is given
      do i=1,4 
        ii=fstval+i-1
        call cvatof(comand(ii)(strbeg(comand(ii)):strend(comand(ii))+1),
     &                         mapwin_tmp(i),error)
        if(error) then
          write (message, 110) trim(comand(ii)), trim(comand(1))
          call write_message
          goto 90
        end if
      end do

      mapwin_u1=mapwin_tmp(1) ; mapwin_u2=mapwin_tmp(2) 
      mapwin_v1=mapwin_tmp(3) ; mapwin_v2=mapwin_tmp(4) 
      call lonlat_2_phys_trans(u1, v1, mapwin_u1, mapwin_v1, 1)
      call lonlat_2_phys_trans(u2, v2, mapwin_u2, mapwin_v2, 1)

      call scale(u1, 1, domain_slope(1), domain_intercept(1), 0.0)
      call scale(u2, 1, domain_slope(1), domain_intercept(1), 0.0)
      call scale(v1, 1, domain_slope(2), domain_intercept(2), 0.0)
      call scale(v2, 1, domain_slope(2), domain_intercept(2), 0.0)
      write(wincmd,120) u1,u2,v1,v2
      call driver(wincmd,1)

 90   mapwin_u1=0.0 ; mapwin_u2=0.0 ; mapwin_v1=0.0 ; mapwin_v2=0.0


 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format ('window=',3(F14.4,','),F14.4)
 130  format ('You must be on a map to use ',A)

      return
      end
