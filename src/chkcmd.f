      integer function chkcmd (cmnd, keynum, error) 
c-----------------------------------------------------------------------
c     $Id: chkcmd.f,v 1.1.1.1 1993/04/07 19:39:47 harry Exp $
c     Purpose:
c        This routine checks for errors in an input command, such as
c           an invalid or ambiguous command. It returns a key number
c           which determines the subsequent action taken by the 
c           calling routine.
c
c     Arguments:
c        cmnd    char  input   user supplied command.
c        keynum  int   output  flag which determines further processing
c                              of the user supplied command.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false  no errors detected
c                              error = true   error detected
c
c     History:
c      $Log: chkcmd.f,v $
c      Revision 1.1.1.1  1993/04/07 19:39:47  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.2  1992/11/10  16:22:42  harry
c Add include file for command name DATA statements.
c
c Revision 2.1  1992/09/29  18:46:32  harry
c Make chkcmd a function.  Change chkcmd to return when a command
c matches exactly.
c
c Revision 2.0  1992/07/01  21:12:29  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'cmndlst.icl'
      include 'cmndlst_data.icl'
      include 'message.icl'
c
c
c     Argument declarations
c
      character *(*) cmnd
      integer        keynum
      logical        error
c
c
c     Local variable declarations
c
      integer ibeg, iend, lencom, mchchk, nmatch, numcmd
c
c
c     External function declarations.
c
      integer strbeg, strend 
c
c
c
c     Initialize some variables
c
      nmatch = 0
      ibeg = strbeg(cmnd)
      iend = strend(cmnd)
      error = .false.
c
c
c     Convert comand to upper case to allow for commands to be entered
c     in either upper or lower case.
c
      lencom = iend - ibeg + 1
      call upcase (cmnd(ibeg:ibeg), lencom)
c
c
c     Run through the list of valid commands, checking cmnd against this
c     list.
c
      do 10 numcmd = 1, nvcmnd
         mchchk = index( valcmd(numcmd)(1:lencom),
     &                   cmnd(ibeg:iend) )
         if (mchchk .gt. 0) then
c
c           Increment the counter recording the number of matches found.
c
            nmatch = nmatch + 1
            keynum = numcmd
            chkcmd = numcmd
            if (iend-ibeg+1 .eq. strend(valcmd(numcmd))) return
         endif
 10   continue
c
c
c
c     Check to see if the command is either invalid or ambiguous by 
c     checking the number of matches found with the valid command list.
c
      if (nmatch .eq. 0) then
c
c        Command is not a valid command.
c
         error = .true.
         write (message, 100) cmnd(ibeg:iend)
         call write_message
c
      elseif (nmatch .gt. 1) then
c
c        Command is ambiguous.
c
         error = .true.
         write (message, 110) cmnd(ibeg:iend)
         call write_message
      endif
c
c
c     Format statements.
c
 100  format (1x,a,' is not a valid command. Enter HELP for a list ',
     &        'of valid commands.')
 110  format (1x,a,' is an ambiguous command. Enter HELP for a ',
     &        'list of valid commands.')
c
c
      return
      end













