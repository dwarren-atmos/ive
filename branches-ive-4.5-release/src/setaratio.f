      subroutine setaratio (error)
c-----------------------------------------------------------------------
c     $Id: setaratio.f,v 1.2 2002/06/11 17:26:17 harryive Exp $
c     Purpose:
c        This routine sets the aspect ratio for printed plots. The
c        aspect ratio is defined as the ratio of theh length of 
c        horizontal plot axis to the length of the vertical plot axis.
c     Arguments:
c        error  logi  input  indicates errors encountered in this 
c                            routine.
c     History:
c        $Log: setaratio.f,v $
c        Revision 1.2  2002/06/11 17:26:17  harryive
c        Change aspect ratio to affect the plot, not just printing.
c
c        Add first part of contour value specification.
c
c        Revision 1.1.1.1  1993/04/07 19:40:20  harry
c        Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:06  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
      logical             error
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
      integer             nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
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
c
c        Convert the first value to a real number. The value of the
c        aspect ratio must be a positive number.
c
         call cvatof (curval(strbeg(curval):strend(curval)), value, 
     &                error)
         if ( value .lt. 0.0 ) then
            error = .true.
         endif
         if ( error ) then
c
c           Log an error.
c
            write (message, 110) curval(ibeg:iend), 
     &                    comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
         endif
      endif
c
c
      if ( .not. error ) then
c
c        Set the aspect ratio.
c
         aratio = value
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
