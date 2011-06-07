	subroutine setax (error)
c-----------------------------------------------------------------------
c	$Id: setax.f,v 1.2 1993/06/01 19:55:56 yuri Exp $
c     Purpose:
c        This routine sets the flag used by the NCAR graphics routine
c        SET. The value of this flag determines the linear or 
c        logarithmic scaling of plot axes.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: setax.f,v $
c        Revision 1.2  1993/06/01 19:55:56  yuri
c        Second try.
c
c Revision 1.1.1.1  1993/04/07  19:40:21  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:13:59  millerp
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
      include 'pltwin.icl'
      include 'attributes.icl'
c

c
c     Argument declarations.
c
      logical  error
c
c
c     Local valiable declarations.
c
      character *(cmdlen) curval
      integer             ibeg, iend
c
c     External function declarations.
      integer nblank, strbeg, strend
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
      else
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:ibeg + 4) .eq. 'LINLI') then
c
c           Set axis scaling to be x linear, y linear.
c
            linlog = 1
c
         elseif (curval(ibeg:ibeg + 4) .eq. 'LINLO') then
c
c           Set axis scaling to be x linear, y logarithmic.
c
            linlog = 2
c
         elseif (curval(ibeg:ibeg + 4) .eq. 'LOGLI') then
c
c           Set axis scaling to be x logarithmic, y linear.
c
            linlog = 3
c
         elseif (curval(ibeg:ibeg + 4) .eq. 'LOGLO') then
c
c           Set axis scaling to be x logarithmic, y logarithmic.
c
            linlog = 4
c
         elseif (curval(ibeg:ibeg + 4) .eq. '3DLIN') then
c
c           Set axis scaling to be x logarithmic, y logarithmic.
c
            linlog3d = 0
c
         elseif (curval(ibeg:ibeg + 4) .eq. '3DLOG') then
c
c           Set axis scaling to be x logarithmic, y logarithmic.
c
            linlog3d = 1
c
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
         window_axes = linlog
c     
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
