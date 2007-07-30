      subroutine setint (error, safe_to_lock )
c-----------------------------------------------------------------------
c     $Id: setint.f,v 1.14 1995/12/15 22:33:24 warren Exp $
c     Purpose:
c        This subroutine sets the value of cint, which indicates the
c        desired contour interval to subroutine conpack.
c        cint > 0  the increment between contour levels is cint.
c        cint = 0  a contour interval which produces a "nice" number
c                    of contour levels (between 10 and 30) is chosen by
c                    conpack.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setint.f,v $
c      Revision 1.14  1995/12/15 22:33:24  warren
c      moved patterns into 4.0 and implemented the list of fixes and changes from
c      Curtis.
c
c Revision 1.13  1995/10/19 22:10:02  harry
c Add default as a possibility for contour values => switch to contour
c interval of 0.
c
c Revision 1.12  1995/09/14  17:16:40  harry
c Sort the contour values in setvals to properly do color fills.
c
c Revision 1.11  1994/10/12  21:54:46  warren
c Fixed infinite loop bug in color locking caused by selecting specific
c contour values.
c
c Revision 1.10  1994/09/07  22:07:13  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.9  1994/06/28  17:43:38  warren
c First cut at fancy tic marks.
c
c Revision 1.8  1993/12/10  20:23:04  yuri
c Added "default" condition for contour intervals. (Same as interval= 0.)
c
c Revision 1.7  1993/10/07  15:50:26  harry
c In setint, only reverse the sign of num_vals if it is positive.
c
c Revision 1.6  1993/07/09  16:35:28  harry
c Add number of contour values to attributes and change its meaning so
c that a zero or negative number means use contour interval instead.
c
c Revision 1.5  1993/05/27  16:38:55  warren
c Recover files removed by yuri's commit
c
c Revision 1.2  1993/05/04  18:13:58  warren
c added command contour_values
c
c Revision 1.1.1.1  1993/04/07  19:40:24  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1993/01/05  20:15:11  warren
c set cont_int not cint
c
c Revision 2.0  1992/07/01  21:14:22  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
c      include 'conre.icl'
      include 'attributes.icl'
      include 'message.icl'
      include 'color.icl'
c
c
c     Argument declarations.
c
      logical  error, safe_to_lock
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
      elseif(curval(1:1) .eq. 'd')then
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
c        Set the value of cint.
c
         cont_int = value
         if (num_vals .gt. 0) num_vals = -num_vals
         if(cont_used .ne. cont_int) then
            safe_to_lock = .false.
         endif
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


      subroutine setvals (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine sets the contour levels
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
c      include 'conre.icl'
      include 'attributes.icl'
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
         call driver('CONTOUR_INTERVAL=d', 0)
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
c        Set the value of cint.
c
         cont_values(1) = value
         num_vals = 1
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
         num_vals = num_vals + 1
         cont_values(num_vals) = value
 1000    continue
 1010    continue
c
c     The list of values must be sorted and unique for CONPACK.
c
         do i = 1, num_vals-1
            iv = i
            value = cont_values(i)
            do j = i+1, num_vals
               if (cont_values(j).lt.value) then
                  value = cont_values(j)
                  iv = j
               endif
            enddo
            if (iv.ne.i) then
               cont_values(iv) = cont_values(i)
               cont_values(i) = value
            endif
         enddo
         do i = 1, num_vals-1
            do while (cont_values(i).eq.cont_values(i+1) .and.
     &           i .le. num_vals-1)
               do j=i, num_vals-1
                  cont_values(j) = cont_values(j+1)
               enddo
               num_vals = num_vals - 1
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
