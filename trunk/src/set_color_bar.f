      subroutine set_color_bar (error)
c-----------------------------------------------------------------------
c     $Id: set_color_bar.f,v 1.2 1998/06/03 17:54:40 harry Exp $
c     Purpose:
c        This subroutine sets whether or not to plot the color bar
c        If color_bar is false the color bar is not plotted.
c        If color_bar is true the color bar is plotted.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: set_color_bar.f,v $
c      Revision 1.2  1998/06/03 17:54:40  harry
c      Fix tick mark labels.   Add new command COLOR_BAR.
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
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
         if (curval(ibeg:iend) .eq. 'ON') then
c
c           Turn plot labeling on.
c
            color_bar = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn plot labeling off.
c
            color_bar = .false.
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
         
