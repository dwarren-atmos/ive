      subroutine setnoz (error)
c-----------------------------------------------------------------------
c     $Id: setnoz.f,v 1.1.1.1 1993/04/07 19:40:28 harry Exp $
c     Purpose:
c        This subroutine sets the no zero line flag, which is used  
c        by conpack. If the variable nozero is true, the zero  
c        contour line will not be drawn. Otherwise, the zero line will 
c        be drawn.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setnoz.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:28  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.1  1993/01/05  20:15:11  warren
c set no_zero not nozero
c
c Revision 2.0  1992/07/01  21:14:35  millerp
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
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
c
         if (curval(ibeg:iend) .eq. 'ON') then
c
c           Turn no zero flag on.
c
            no_zero = .true.
c
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn no zero flag off.
c
            no_zero = .false.
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
