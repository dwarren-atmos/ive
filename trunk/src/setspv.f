      subroutine setspv (error)
c-----------------------------------------------------------------------
c     $Id: setspv.f,v 1.1.1.1 1993/04/07 19:40:33 harry Exp $
c     Purpose:
c        This routine sets the conpack special-value value.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: setspv.f,v $
c        Revision 1.1.1.1  1993/04/07 19:40:33  harry
c        Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:53  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'conre.icl'
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
      character *(CMDLEN) curval
      real                value
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
      curval = comand(fstval)(1:CMDLEN)
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
c
c        Convert the first value to a real number.
c
c
         call cvatof (curval(strbeg(curval):strend(curval)), value, 
     &                error)
c
         if ( error ) then
c
c           Log an error message and do not set the value of spval.
c
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
         else
c
c           Set the value of spval.
c
            spval = value
c
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
