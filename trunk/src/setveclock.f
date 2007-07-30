      subroutine setveclock(error)
c
c     $Id: setveclock.f,v 1.5 1999/02/10 23:13:38 harry Exp $
c     Purpose:
c        This subroutine processes the VECTOR_LOCK command:
c
c     VECTOR_LOCK=on|off
c
c     $Log: setveclock.f,v $
c     Revision 1.5  1999/02/10 23:13:38  harry
c     Get rid of unused command vector_scale.
c
c Revision 1.4  1994/11/22  22:48:06  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c
      include 'incommands.icl'
      include 'ive_widgets.icl'
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
      integer             ibeg, iend
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
         write (message,100) comand(1)(strbeg(comand(1)):
     &                                 strend(comand(1)))
         call write_message
         error = .true.
      else
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:iend), (iend - ibeg + 1))
c
         if (curval(ibeg:iend) .eq. 'ON') then
c
c           Turn lock vect on
c
            call setlvar ('veclock', .true., error) 
c
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn lock vect off
c
            call setlvar ('veclock', .false., error) 
c
         else
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(ibeg:iend),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
            call write_message
            error = .true.
         endif
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
