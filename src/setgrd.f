      subroutine setgrd (error)
c-----------------------------------------------------------------------
c     $Id: setgrd.f,v 1.1.1.1 1993/04/07 19:40:24 harry Exp $
c     Purpose:
c        This subroutine sets the grid spacing, in degrees, for maps. 
c        A value of 0.0 supresses the grid.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setgrd.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:24  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:19  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'supma.icl'
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
      real                value
c
c
c     External function declarations.
c
      integer            nblank, strbeg, strend 
c
c
c
c
      error = .false.
c
c
c
c     Get the first value for the current command.
      curval = comand(fstval)(1:cmdlen)
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
         call cvatof ( curval(ibeg:iend), value, error )
c
         if ( error ) then
c     
c           The value for this command is incorrect.
c     
            write (message, 110) curval(ibeg:iend),
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
         else
            grdspa = value
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
   




