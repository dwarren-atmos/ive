      subroutine setcll (error)
c-----------------------------------------------------------------------
c     $Id: setcll.f,v 1.1.1.1 1993/04/07 19:40:22 harry Exp $
c     Purpose:
c        This subroutine sets the value of cllab, which is used as a 
c        contour line labeling flag . If clllab is false, contour
c        lines are not labeled; if cllab is true, contour lines are 
c        labeled.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setcll.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:22  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:08  millerp
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
      logical error
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
c           Turn contour line labeling on.
c
            cllab = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn contour line labeling off.
c
            cllab = .false.
         else
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(ibeg:iend),
     &                    comand(1)(strbeg(comand(1)):
     &                              strend(comand(1)))
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
         
