      subroutine help
c-----------------------------------------------------------------------
c     $Id: help.f,v 1.3 1995/06/23 21:16:22 warren Exp $
c     Purpose:
c        This subroutine is the main driver for the online help 
c        facility.
c     Arguments:
c        None
c     History:
c      $Log: help.f,v $
c      Revision 1.3  1995/06/23 21:16:22  warren
c      First part of new ive help
c      Changed " to ' in fortran files
c      Fixed the label spacing in 1d plots
c      added frame_width command
c
c Revision 1.2  1993/06/28  20:15:43  yuri
c Errors in help stuff corrected.
c
c Revision 1.1.1.1  1993/04/07  19:40:00  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/12/21  19:10:32  millerp
c *** empty log message ***
c
c Revision 2.0  1992/07/01  21:13:18  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/06/26  00:17:17  millerp
c *** empty log message ***
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'cmndlst.icl'
      include 'message.icl'
c
c
c     Local variable declarations.
c
      character *(CMDLEN+1) curval
      integer             i, ibeg, iend, keynum
      logical             error
c
c
c     External function declarations.
c
      integer     nblank, strbeg, strend
c
c
c
c
c     Get the first value for the current command.
c
      curval = comand(FSTVAL)(1:CMDLEN)
c
c
c     If no value is given, print out a list of valid commands. This
c     list get passed to C, so remember to null-terminate strings.
c
      if ( nblank(curval) .eq. 0 ) then
         call hlpcmd('help'//char(0)) 
      else
c     
c        Convert curval to uppercase, then get help on that command.
c     
         ibeg = strbeg(curval)
         iend = strend(curval)
         call chkcmd (curval(ibeg:iend), keynum, error)
         if(error) then
           curval = 'ive_commands'//' '
         else
           curval = valcmd(keynum)(1:CMNDLEN)//' '
         end if
         ibeg = strbeg(curval)
         iend = strend(curval)
         call locase (curval(ibeg:ibeg), (iend - ibeg + 1))
         call catnull(curval, CMNDLEN+1, error)
c     Read help file.
         call hlpcmd (curval)
      endif
c
c
c     Format statements.
c
 100  format (1x,'Cannot find help: ',a,' is not a command.')
c
c
      return
      end
