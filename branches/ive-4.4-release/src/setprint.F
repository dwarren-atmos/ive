      subroutine setprint
c-----------------------------------------------------------------------
c     $Id: setprint.F,v 1.4 1998/09/03 18:36:29 warren Exp $
c     Purpose:
c        This routine is used to print specified plots. If no 
c        values are specified for the PRINT command, the current 
c        plot is printed. If arguments are specified, then
c        those plots are printed. A maximum of six plots
c        may be printed at a time.
c     Arguments:
c        None.
c     History:
c        $Log: setprint.F,v $
c        Revision 1.4  1998/09/03 18:36:29  warren
c        Commandline interface for printing to a file.
c
c        Revision 1.3  1998/09/03 16:51:14  warren
c        *** empty log message ***
c
c Revision 1.2  1993/06/29  23:36:54  warren
c More includes to defines.
c
c Revision 1.1.1.1  1993/04/07  19:40:31  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1993/01/06  17:17:26  harry
c Change from one segment per plot to one segment per plot.
c
c Revision 2.0  1992/07/01  21:15:35  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c     GKS DEFS
c
c
      INTEGER    GSGOP
      PARAMETER (GSGOP=4)
c
      include 'incommands.icl'
      include 'message.icl'
      include 'segments.icl'
c
c
c     Local variable declarations.
c
      character *(cmdlen) curval, prfilename
      integer             i, istate, MAXNUM, nplts, pltnum, value, j
      logical             error
      parameter ( MAXNUM = 6 )
      dimension pltnum(MAXNUM)
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
c
c     Find out how many values are associated with the command.
c
      prfilename = ' '
      curval = comand(fstval)(1:cmdlen)
c
      if ( nblank(curval) .eq. 0 ) then
c
c        Print the current plot.
c
         call gqops (istate)
         if ( istate .eq. GSGOP ) then
            call gclsg()
         endif
         nplts = 1
         pltnum(1) = curplot
      else
c
c        Convert up to MAXNUM values to integers, then print those
c        plots.
c
         j = 1
         do 10 i = 1, MAXNUM
	    curval = comand(fstval + (i - 1))(1:cmdlen)
	    if (nblank(curval) .eq. 0 ) then
	       go to 20
            endif
            call cvatoi (curval(strbeg(curval):strend(curval)), value,
     &                   error)
            if ( error ) then
               if(curval(strbeg(curval):strend(curval)) .eq. ' ') then
c                 The value for this command is incorrect.
                  write (message, 110) curval(strbeg(curval):
     &                 strend(curval)),
     &                 comand(1)(strbeg(comand(1)):
     &                 strend(comand(1)))
                  call write_message
                  go to 20
               else
                  prfilename=curval(strbeg(curval):strend(curval))
                  error = .false.
               endif
            else
               pltnum(j) = value
               nplts = j
               j = j + 1
            endif
 10      continue
c
 20      continue
c
      endif
c
c
      if ( .not. error ) then
c
c        Print all specified plots.
c
         call printr (nplts, pltnum,
     &        prfilename(strbeg(prfilename):strend(prfilename)))
      endif
c
c
c     Format statements.
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end

