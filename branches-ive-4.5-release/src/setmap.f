      subroutine setmap (error)
c-----------------------------------------------------------------------
c     $Id: setmap.f,v 1.1.1.1 1993/04/07 19:40:27 harry Exp $
c     Purpose:
c        This subroutine sets the value of mapflg.
c        If the map flag is on, a map will be drawn so that a contour
c        plot may be overlayed on the map. If the map flag is off, then
c        no map is drawn.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setmap.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:27  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:31  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'flags.icl'
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
            mapflg = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c           Turn map flag off.
            mapflg = .false.
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



