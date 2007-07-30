      subroutine setlab (error)
c-----------------------------------------------------------------------
c     $Id: setlab.f,v 1.2 2006/08/24 22:08:40 warren Exp $
c     Purpose:
c        This subroutine sets the value of label, which is used as a 
c        plot labeling flag . If label is false, 
c        the plot is not labeled; if label is true, the plot is 
c        labeled.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setlab.f,v $
c      Revision 1.2  2006/08/24 22:08:40  warren
c      added vlabel command to get max vectors plotted even if label=off
c
c      Revision 1.1.1.1  1993/04/07 19:40:25  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:24  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
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
            label = .true.
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn plot labeling off.
c
            label = .false.
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
         
      subroutine setvlab (error)
c-----------------------------------------------------------------------

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
     &         strend(comand(1)))
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
c           Turn vector labeling on.
c
             vlabel = .true.
          elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn vector labeling off.
c
             vlabel = .false.
          else
c
c           The value for this command is incorrect.
c
             write (message, 110) curval(ibeg:iend),
     &            comand(1)(strbeg(comand(1)):
     &            strend(comand(1)))
             call write_message
             error = .true.
          endif
       endif
c
c
c     Format statements.
c
 100   format (1x,'Command ',a,' requires a value.')
 110   format (1x,a,' is an incorrect value for command ',a,'.')
c
c
       return
       end
         
