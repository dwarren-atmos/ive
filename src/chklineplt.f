      subroutine chklineplt (error)
c-----------------------------------------------------------------------
c     $Id: chklineplt.f,v 1.11 1998/09/03 19:19:19 harry Exp $
c     Purpose:
c     This routine checks on consistence in variable parameters:
c     especially during averaging.
c     Arguments:
c        error  logi  output  flag indicating if errors were encountered
c                             in this routine.
c                             false  no errors detected.
c                             true   error detected.
c     History:
c      $Log: chklineplt.f,v $
c      Revision 1.11  1998/09/03 19:19:19  harry
c      Add in 2d line plots - correct bugs in other averaging.
c
c Revision 1.10  1994/09/14  18:19:49  harry
c Big 1D flip uncommit.
c
c Revision 1.9  1994/09/07  22:07:01  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.8  1994/06/28  17:43:23  warren
c First cut at fancy tic marks.
c
c Revision 1.7  1994/06/11  21:33:11  yuri
c Added more comments.
c
c Revision 1.6  1994/04/19  23:02:46  yuri
c Big 1D flip commit.
c
c Revision 1.5  1994/03/30  22:47:05  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.4  1993/06/01  19:48:51  yuri
c Second attempt.
c
c Revision 1.2  1993/05/23  23:37:01  yuri
c You can play with time too now.
c
c Revision 1.1.1.1  1993/04/07  19:39:47  harry
c Initial CVS checkin of IVE.
c
c Revision 2.3  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 2.2  1992/09/29  21:05:16  warren
c *** empty log message ***
c
c Revision 2.1  1992/09/29  20:42:37  warren
c changed to match new slicers.
c
c Revision 2.0  1992/07/01  21:12:33  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'lineplt.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
      logical         error
c
c
c     Local variable declarations.
c
      integer         ave_count, i, lock_count
c
c
c
c
      error = .false.
      average = .false.
c
c
      if (use_buttons .ne. 0) then
c
c        Find out how many buttons are locked, and how many are set on
c        average.
c
         lock_count = 0
         ave_count = 0
         do i = 1, 4
            if (lock(i) .eq. 1) then
               lock_count = lock_count + 1
            elseif (lock(i) .eq. 2) then
               ave_count = ave_count + 1
            endif
         enddo
c
c        Plot location is specified by using widget buttons.
c
         if (ave_count .eq. 0) then
            if (lock_count .lt. 3) then
c              Three buttons must be locked to define a line.
               error = .true.
               write(message, 100)
               call write_message
               go to 10
            endif
         elseif (ave_count .eq. 1) then
            if (lock_count .ne. 2) then
c     If we are doing an average, we must define a slice
c     plane.
               error = .true.
               write(message, 150)
               call write_message
               go to 10
            endif
            average = .true.
         elseif (ave_count .eq. 2) then
            if (lock_count .ne.1) then
               error = .true.
               write(message, 150)
               call write_message
               goto 10
            endif
         else
            error = .true.
            write (message, 140)
            call write_message
            go to 10
         endif
      endif
 10   continue
c
c     Format statements.
c
 100  format (1x, 'You must lock 3 variables to make a line plot')
 140  format (1x, 'It is not possible to average across more than two ',
     &            'dimensions.')
 150  format (1x, 'In order to produce an average plot, you must ',
     &            'first define a slice plane.')

      return
      end
