      subroutine settci (error)
c-----------------------------------------------------------------------
c     $Id: settci.f,v 1.2 2002/06/11 17:26:18 harryive Exp $
c     Purpose:
c        This subroutine sets the value of tcint, which indicates the
c        desired contour interval for terrain to subroutine conpack.
c        tcint > 0  the increment between contour levels is tcint.
c        tcint = 0  a contour interval which produces a "nice" number
c                   of contour levels (between 10 and 30) is chosen by
c                   conpack.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: settci.f,v $
c      Revision 1.2  2002/06/11 17:26:18  harryive
c      Change aspect ratio to affect the plot, not just printing.
c
c      Add first part of contour value specification.
c
c Revision 1.1.1.1  1993/04/07  19:40:33  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:58  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'conre.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(cmdlen) curval
      real                value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      elseif(curval(1:1) .eq. 'd') then
         value = 0d0
      else
c
c        Convert the first value to a real number.
c
         call cvatof (curval(strbeg(curval):strend(curval)), value,
     &                error)
         if ( error ) then
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(strbeg(curval):strend(curval)),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
            call write_message
         endif
      endif
c      
c
      if ( .not. error ) then
c
c        Set the value of tcint.
c
         tcint = value
         if (tnum_vals .gt. 0) tnum_vals = - tnum_vals
c
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end


      subroutine settcv (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine sets the terrain contour levels
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'conre.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(cmdlen) curval
      real                value
      integer i, iv, j
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
c
c     Make sure a value is given.
c
      i = 0
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      elseif(curval(1:1) .eq. 'd') then
         call driver('TCONTOUR_INTERVAL=d', 0)
      else
         
c
c        Convert the first value to a real number.
c
         call cvatof (curval(strbeg(curval):strend(curval)), value,
     &                error)
         if ( error ) then
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(strbeg(curval):strend(curval)),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
         call write_message
         endif
      endif
c      
c
      if ( .not. error ) then
c
c        Set the value of tcont_values
c
         tcont_values(1) = value
         tnum_vals = 1
c
      else
         return
      endif
      
      do 1000 i = 1, MAXCOM - 2
         curval = comand(fstval + i)(1:cmdlen)
         call cvatof (curval(strbeg(curval):strend(curval)), value,
     &                error)
         if ( error ) then
c
c           We must be at the end
c
            error = .false.
            goto 1010
         endif
         tnum_vals = tnum_vals + 1
         tcont_values(tnum_vals) = value
 1000    continue
 1010    continue
c
c     The list of values must be sorted and unique for CONPACK.
c
         do i = 1, tnum_vals-1
            iv = i
            value = tcont_values(i)
            do j = i+1, tnum_vals
               if (tcont_values(j).lt.value) then
                  value = tcont_values(j)
                  iv = j
               endif
            enddo
            if (iv.ne.i) then
               tcont_values(iv) = tcont_values(i)
               tcont_values(i) = value
            endif
         enddo
         do i = 1, tnum_vals-1
            do while (tcont_values(i).eq.tcont_values(i+1) .and.
     &           i .le. tnum_vals-1)
               do j=i, tnum_vals-1
                  tcont_values(j) = tcont_values(j+1)
               enddo
               tnum_vals = tnum_vals - 1
            enddo
         enddo
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
