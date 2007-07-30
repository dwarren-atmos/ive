      subroutine write_frame (error)
c-----------------------------------------------------------------------
c     $Id: write_frame.f,v 1.2 2006/08/22 20:26:55 reinecke Exp $
c     Purpose:
c     This subroutine sets up the printing of the current plot to
c     a named file.
c
c     Arguments:
c        error    logi  output  indicates possible errors found in this
c                               routine.
c                               error = false  no errors detected
c                               error = true   error detected
c     History:
c      $Log: write_frame.f,v $
c      Revision 1.2  2006/08/22 20:26:55  reinecke
c      Files name length limit increased from 80 to 512
c
c      Revision 1.1.1.1  1993/04/07 19:40:45  harry
c      Initial CVS checkin of IVE.
c
c Revision 1.2  1993/01/07  17:24:02  harry
c Change name from write_file to write_frame.
c Use plot number instead of segment number.
c
c Revision 1.1  1992/10/21  21:33:12  warren
c Initial revision
c
c-----------------------------------------------------------------------
      include 'incommands.icl'
      include 'message.icl'
      include 'segments.icl'
c
c
c     Argument declarations.
c
      logical error
c
c
c     Local variable declarations.
c
      character *512   file
      character *512    curval
      integer          ibeg, iend
      integer          plot
c
c
c     External function declarations.
c
      integer          nblank, strbeg, strend
c
c
c
c
      error = .false.
      ibeg  = strbeg(comand(FSTVAL))
      iend  = strend(comand(FSTVAL))
c
c
c     Make sure a value is given.
c
      if ( nblank(comand(FSTVAL)) .eq. 0 ) then
         write (message, 100) comand(FSTVAL)(ibeg:iend)
         call write_message
         error = .true.
      else
c
c        Set the file name.
c
         file(1:len(file)) = ' '
         file = comand(FSTVAL)(ibeg:iend)
      endif
c
c     Expand ~'s done in the print_file routine
c
      curval = comand(fstval + 1)(1:cmdlen)
      if (nblank(curval) .ne. 0 ) then
         call cvatoi (curval(strbeg(curval):strend(curval)), plot,
     &        error)
      else
         plot = curplot
      endif
         
      if ( .not. error ) then
c
c        Set the file name.
c
         iend = strend(file)
         call print_file(plot, file(1:iend))
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
c
c
      return
      end


