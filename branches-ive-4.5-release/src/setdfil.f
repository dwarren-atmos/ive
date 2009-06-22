      subroutine setdfil (error)
c-----------------------------------------------------------------------
c     $Id: setdfil.f,v 1.6 1996/10/09 18:54:47 harry Exp $
c     Purpose:
c        This subroutine sets the data file name. Note that a file
c           name consists of the directory + file name.
c     Arguments:
c        error    logi  output  indicates possible errors found in this
c                               routine.
c                               error = false  no errors detected
c                               error = true   error detected
c     History:
c      $Log: setdfil.f,v $
c      Revision 1.6  1996/10/09 18:54:47  harry
c      Add the ability to loop on file names if time value is part of name,
c      and there is only one time per file.
c
c Revision 1.5  1993/07/09  16:33:26  harry
c Add changes to properly handle windowing when changing data files.
c
c Revision 1.4  1993/06/01  19:57:15  yuri
c Second try.
c
c Revision 1.2  1993/05/21  17:56:32  yuri
c If user changed files, then message about new limits in slicer shows only
c once.
c
c Revision 1.1.1.1  1993/04/07  19:40:23  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/08/24  20:36:09  millerp
c Do not do INQUIRE on data file.
c
c Revision 2.0  1992/07/01  21:14:14  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/06/12  17:11:03  warren
c added call to expand_path to take care of ~name/
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'data.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      logical error
c
c
c     Local variable declarations.
c
      character *256   file
      integer          ibeg, iend
c
c
c     External function declarations.
c
      integer          nblank, strbeg, strend, expand_path
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
c
c
c     Expand ~'s
c
      if ( .not. error ) then
         iend = strend(file)
         file(iend+1:iend+1)=char(0)
         error = expand_path(file)
         if(error) then
            error = .false.
         else
            error = .true.
         endif
      endif

      if ( .not. error ) then
c
c     See if the file exists.
c
         inquire(file=file(strbeg(file):strend(file)), exist=error)
         error = .not. error
         if (error) then
            write(message, 101) file(strbeg(file):strend(file))
            call write_message
         else
c
c        Set the data file name.
c
            iend = strend(file)
            datfil(1:len(datfil)) = ' '
            datfil(1:iend) = file(1:iend)
         endif
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 101  format (1x,'File does not exist - ', a)
c
c
      return
      end






