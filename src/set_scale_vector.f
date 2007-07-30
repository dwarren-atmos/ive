      subroutine set_scale_vector (error)
c-----------------------------------------------------------------------
c     $Id: set_scale_vector.f,v 1.1 1999/02/04 00:29:08 harry Exp $
c     Purpose:
c        This subroutine sets whether or not to scale vectors.
c        If scale_vector is false the vectors are not scaled.
c        If scale_vector is true the vectors are scaled to the axes.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: set_scale_vector.f,v $
c      Revision 1.1  1999/02/04 00:29:08  harry
c      Add vector scaling.
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'vector.icl'
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
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
c
      else
c
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:iend) .eq. 'ON' .or.
     &        curval(ibeg:iend) .eq. 'YES') then
c
c           Turn vector scaling on
c
            scale_vector = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF' .or.
     &           curval(ibeg:iend) .eq. 'NO') then
c
c           Turn vector scaling off.
c
            scale_vector = .false.
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
         
