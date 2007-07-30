      subroutine setvec_pos(error)
c
c     $Id: setvec_pos.f,v 1.1 1994/11/22 22:49:28 harry Exp $
c     Purpose:
c        This subroutine processes the VECTOR_POSITION command.
c
c     VECTOR_POSITION=head|center|tail
c
c     $Log: setvec_pos.f,v $
c     Revision 1.1  1994/11/22 22:49:28  harry
c     Correct vector locking, add support for vector positioning, add
c     support for filled contour overlays.
c
c
      include 'incommands.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      character*(cmdlen) curval
      integer            ibeg, iend
c
c     External function declarations.
c
      integer nblank, strbeg, strend
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
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:iend) .eq. 'HEAD') then
            call setivar('vecpos', -1, error)
         else if (curval(ibeg:iend) .eq. 'CENTER') then
            call setivar('vecpos', 0, error)
         else if (curval(ibeg:iend) .eq. 'TAIL') then
            call setivar('vecpos', 1, error)
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
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')

      return
      end
