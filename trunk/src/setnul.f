      subroutine setnul (error)
c-----------------------------------------------------------------------
c     $Id: setnul.f,v 1.2 1996/04/08 18:57:15 warren Exp $
c     Purpose:
c        This subroutine sets the value of nulbll, which sets the number
c        of unlabelled lines in subroutine conpack.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setnul.f,v $
c      Revision 1.2  1996/04/08 18:57:15  warren
c      first parts of num unlabeled lines for terrain and fixed transform routine
c      to use last directory if none is given.
c
c Revision 1.1.1.1  1993/04/07  19:40:28  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:37  millerp
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
      logical  error
c
c
c     Local variable declarations.
c
      character *(cmdlen) curval
      integer             value
c
c
c     External declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.

c     Get the first value for the current command.
      curval = comand(fstval)(1:cmdlen)
c
c     Make sure a value is given.
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
c        Convert the first value to integer.
         call cvatoi (curval(strbeg(curval):strend(curval)), value, 
     &                error)
         if ( error ) then
c           The value for this command is incorrect.
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                     comand(1)(strbeg(comand(1)):
     &                               strend(comand(1)))
            call write_message
         else
c           Set the value of nulbll.
            nulbll = value
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

      subroutine setternul (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine sets the value of terr_nulbll, which sets the number
c        of unlabelled lines in subroutine conpack.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c-----------------------------------------------------------------------
c
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
c
      character *(cmdlen) curval
      integer             value
c
c
c     External declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.

c     Get the first value for the current command.
      curval = comand(fstval)(1:cmdlen)
c
c     Make sure a value is given.
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
c        Convert the first value to integer.
         call cvatoi (curval(strbeg(curval):strend(curval)), value, 
     &                error)
         if ( error ) then
c           The value for this command is incorrect.
            write (message, 110) curval(strbeg(curval):strend(curval)), 
     &                     comand(1)(strbeg(comand(1)):
     &                               strend(comand(1)))
            call write_message
         else
c           Set the value of terr_nulbll.
            terr_nulbll = value
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

