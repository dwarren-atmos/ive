      subroutine setml (error)
c-----------------------------------------------------------------------
c     $Id: movbl.f,v 1.4 2007-05-25 15:48:41 warren Exp $
c     Purpose:
c        This subroutine sets the value of the movielabels flag (movielabels).
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: movbl.f,v $
c      Revision 1.4  2007-05-25 15:48:41  warren
c      Added big_label command. - movie labels on original background.
c
c      Revision 1.3  1994/12/27 22:37:08  warren
c      call white_background for the movielabels=off option as per Dale.
c      changed to label the top of the plot box if the value is at least
c      half of a tic interval above the last tic mark, not just if it is
c      exactly tcint above.
c
c Revision 1.2  1994/04/23  17:49:03  warren
c update from 3.1
c
c Revision 1.1  1994/01/29  00:11:37  warren
c first cut at movie labels. Line plots still need work
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'flags.icl'
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
      else
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:iend) .eq. 'ON') then
c
c           Turn movielabel flag on.
c
            movielabs = .true.
            call black_background()
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn save movielabel off.
c
            movielabs = .false.
            call white_background()
         else
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(ibeg:iend),
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
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


      subroutine setbl (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine sets the value of the movielabels flag (movielabels). to get big labels
c        but it does not change the background.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'flags.icl'
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
      else
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:iend) .eq. 'ON') then
c
c           Turn movielabel flag on.
c
            movielabs = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn save movielabel off.
c
            movielabs = .false.
         else
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(ibeg:iend),
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
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
