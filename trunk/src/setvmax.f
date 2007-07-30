      subroutine setvmax (error)
c-----------------------------------------------------------------------
c     $Id: setvmax.f,v 1.6 1994/11/22 22:48:08 harry Exp $
c     Purpose:
c        This routine sets the value of vmax. If vmax is non-zero, then
c        a vector whose magnitude is vmax will be drawn so that its  
c        length is (one grid interval * vector_interval).
c     Arguments:
c        error  logi  output  error flag. 
c     History:
c        $Log: setvmax.f,v $
c        Revision 1.6  1994/11/22 22:48:08  harry
c        Correct vector locking, add support for vector positioning, add
c        support for filled contour overlays.  Use new NCAR vector plotting
c        routine instead of modified old velvct.
c
c Revision 1.5  1994/05/06  17:10:17  yuri
c Main vector lock commit + small correction in X/ive_ui_update.c file
c
c Revision 1.4  1993/06/14  21:56:59  warren
c Fixed to recognize the value default
c
c Revision 1.3  1993/05/27  16:38:57  warren
c Recover files removed by yuri's commit
c
c Revision 1.1.1.1  1993/04/07  19:40:36  harry
c Initial CVS checkin of IVE.
c
c Revision 1.1  1992/09/08  19:53:31  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c
      include 'incommands.icl'
      include 'vector.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical             error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
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
         call cvatof (curval(ibeg:iend), value, error)
         if ( .not. error ) then
            vmax = value
            call setlvar('veclock', .true., error)
         else
            if((curval(1:1) .eq. 'D') .or. (curval(1:1) .eq. 'd')) then
               error = .false.
               vmax=0.0
               call setlvar('veclock', .false., error)
            else
               write (message, 110) curval(ibeg:iend),
     &              comand(1)(strbeg(comand(1)):
     &              strend(comand(1)))
               call write_message
               error = .true.
            endif
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
