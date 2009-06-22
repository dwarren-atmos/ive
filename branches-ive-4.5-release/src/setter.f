      subroutine setter (error)
c-----------------------------------------------------------------------
c     $Id: setter.f,v 1.1 1996/04/01 20:23:45 warren Exp $
c     Purpose:
c        This subroutine sets the value of terrflg.
c        If the terr flag is on, terrain will be drawn.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setter.f,v $
c      Revision 1.1  1996/04/01 20:23:45  warren
c      Terrain form and new terrain command (on / off)
c
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'plotops.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
      character *(cmdlen) curval
      integer             ibeg, iend
c
c
c     External function declarations.
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
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:iend) .eq. 'ON') then
c           Turn map flag on.
            terrflg = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c           Turn map flag off.
            terrflg = .false.
         else
c           The value for this command is incorrect.
            write (message,110) curval(ibeg:iend),
     &                          comand(1)(strbeg(comand(1)):
     &                                    strend(comand(1)))
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



