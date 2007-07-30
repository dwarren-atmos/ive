      subroutine chk2d (error)
c-----------------------------------------------------------------------
c     $Id: chk2d.f,v 1.13 2000/04/05 22:38:52 harry Exp $
c     Purpose:
c        This subroutine checks to make sure that all necessary 
c        parameters are set before drawing a 2d plot. At a minimum, 
c        an existing data set, a field,  and slice plane must be 
c        specified.
c     Arguments:
c        error  logi  output  flag indicating if errors were encountered
c                             in this routine.
c                             false  no errors detected.
c                             true   error detected.
c     History:
c      $Log: chk2d.f,v $
c      Revision 1.13  2000/04/05 22:38:52  harry
c      Correct bugs in general surface plotting.
c
c      Revision 1.12  1997/06/03 19:14:17  davidive
c      The big mouse slicing changeover. We have the new form, only requires you
c      set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c      always runs through init_points_ to catch all these things.
c      note: only actually works when two dimentions have points set. Needs to
c      be expanded into 1, 3 or 4.
c
c Revision 1.11  1994/09/14  18:19:48  harry
c Big 1D flip uncommit.
c
c Revision 1.10  1994/09/07  22:07:00  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.9  1994/06/28  17:43:21  warren
c First cut at fancy tic marks.
c
c Revision 1.8  1994/06/11  21:33:08  yuri
c Added more comments.
c
c Revision 1.7  1994/04/19  23:02:44  yuri
c Big 1D flip commit.
c
c Revision 1.6  1993/10/26  18:29:07  yuri
c Corrected log(negative number) problem, reported by PABLO.
c
c Revision 1.5  1993/10/22  20:36:50  yuri
c Check for negative limits in log-plots added.
c
c Revision 1.4  1993/06/01  19:48:32  yuri
c Second attempt.
c
c Revision 1.2  1993/05/23  23:26:58  yuri
c 	improved version with 0,1,2D averaging.
c
c Revision 1.1.1.1  1993/04/07  19:39:46  harry
c Initial CVS checkin of IVE.
c
c Revision 2.3  1992/11/23  16:53:35  millerp
c Fixed fix.
c
c Revision 2.2  1992/11/23  16:35:06  millerp
c Added stuff for surface plots.
c
c Revision 2.1  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.0  1992/07/01  21:12:24  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/06/10  22:30:16  millerp
c Move setting of xloc, yloc, zloc, angle to sethloc, setvloc.
c
c Revision 1.2  1992/05/05  15:18:26  millerp
c Removed sounding stuff, created new routine for this.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
      logical error
c
c
c     Local variable declarations.
c
      integer lock_count, i
      real    EPSLON 
      parameter ( EPSLON = 1.0e-4 )
c
c
c
c
c
      error = .false.
c
c     Is the slice location set and does the location fall within the
c     data domain?
c
      error = .false.
c
      if (surface) then

      elseif (use_buttons .ne. 0) then
         lock_count = 0
         do i = 1, MAXDIM
            if ( lock(i) .eq. 1 ) lock_count = lock_count + 1
         enddo
         if (lock_count .gt. 3) then
            error = .true.
            write (message, 110)
            call write_message
         endif
      else 
c
c        This means that points were specified
c
         lock_count = 0
c         if((point_1(4) .eq. point_2(4)) .and. 
c     &      (point_1(4) .eq. point_3(4))) then
         do i = 1, 3
            if ((point_1(i) .eq. point_2(i)) .and. 
     &           (point_1(i) .eq. point_3(i))) lock_count=lock_count+1
         enddo
         if (lock_count .eq. 3) then
            error = .true.
            write (message, 110)
            call write_message
         endif
c         else
c            error = .true.
c            write (message, 120)
c            call write_message
c         endif
      endif
c            
c     Format statements.
c
 100  format (1x,'Time must be locked to do a surface plot')
 110  format (1x,'A location must be specified for a 2D slice.')
c 120  format (1x,'Time slices must be along an axis line.')

      return
      end
