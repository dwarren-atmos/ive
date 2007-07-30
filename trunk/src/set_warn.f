      subroutine set_warn (error)
c-----------------------------------------------------------------------
c     $Id: set_warn.f,v 1.1 1995/09/20 16:44:47 harry Exp $
c     Purpose:
c        This subroutine sets the value of the timeout for warnings
c        in seconds.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: set_warn.f,v $
c      Revision 1.1  1995/09/20 16:44:47  harry
c      Add warning timeout command.
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
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
      integer             ibeg, iend, nblank, strbeg, strend, value
c
c
c
c
      error = .false.
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
c
      ibeg = strbeg(curval)
      iend = strend(curval)
c
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         call cvatoi(curval(strbeg(curval):strend(curval)), value,
     &        error)
         if (error) then
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(ibeg:iend),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
            call write_message
         else
            call setivar('warn_timeout', value, error)
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
   




