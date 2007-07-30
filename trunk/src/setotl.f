      subroutine setotl (error)
c-----------------------------------------------------------------------
c     $Id: setotl.f,v 1.2 1997/03/06 20:25:09 harry Exp $
c     Purpose:
c        This subroutine sets the value of outlin, which is used by the
c        NCAR EZMAP routine mapstc to set the map outline. The value
c        of outlin will be set to a two-character string.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setotl.f,v $
c      Revision 1.2  1997/03/06 20:25:09  harry
c      Add "M3" and "M1" as map outline possibilities.
c
c      Revision 1.1.1.1  1993/04/07 19:40:29  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:39  millerp
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
      character *(CMDLEN) curval
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
      curval = comand(fstval)(1:CMDLEN)
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase ( curval(ibeg:iend), (iend - ibeg) + 1 )
c
c        Check to see that the value given is valid.
c
         if    ( curval(ibeg:iend)  .eq. 'NO' ) then
            outlin = 'NO'
         elseif ( curval(ibeg:iend) .eq. 'CO' ) then
            outlin = 'CO'
         elseif ( curval(ibeg:iend) .eq. 'US' ) then
            outlin = 'US'
         elseif ( curval(ibeg:iend) .eq. 'PS' ) then
            outlin = 'PS'
         elseif ( curval(ibeg:iend) .eq. 'PO' ) then
            outlin = 'PO'
         elseif ( curval(ibeg:iend) .eq. 'M3' ) then
            outlin = 'M3'
         elseif ( curval(ibeg:iend) .eq. 'M1' ) then
            outlin = 'M1'
         else
c           The value for this command is incorrect.
            write (message, 110) curval(strbeg(curval):strend(curval)),
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
