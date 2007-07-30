      subroutine setfw (error)
c-----------------------------------------------------------------------
c     $Id: setfw.f,v 1.1 1995/06/23 21:52:00 warren Exp $
c     Purpose:
c        This subroutine sets the value of the line width scale factor
c        used by the GKS line drawing routines. 
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setfw.f,v $
c      Revision 1.1  1995/06/23 21:52:00  warren
c      Opps forgot one
c
c Revision 1.1.1.1  1993/04/07  19:40:27  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:30  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'pltwin.icl'
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
      integer             ibeg, iend, nblank, strbeg, strend
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
         if (curval(ibeg:ibeg) .eq. '1') then
c
c           Set frame width scale factor to 1 (thinnest frame).
c
            frmwdth = 1.0
         elseif (curval(ibeg:ibeg) .eq. '2') then
c
c           Set frame width scale factor to 2 (thicker frame).
c
            frmwdth = 2.0
         elseif (curval(ibeg:ibeg) .eq. '3') then
c
c           Set frame width scale factor to 3 (thick frame).
c
            frmwdth = 3.0
         elseif (curval(ibeg:ibeg) .eq. '4') then
c
c           Set frame width scale factor to 4 (thickest frame).
c
            frmwdth = 4.0
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
   




