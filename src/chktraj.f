      subroutine chktraj (error)
c
c     $Id: chktraj.f,v 1.5 2003/07/02 20:24:41 warren Exp $
c
c     This routine is called by maktraj to check to see if everything
c     is set up properly for a trajectory plot.
c
c     $Log: chktraj.f,v $
c     Revision 1.5  2003/07/02 20:24:41  warren
c     Fix background fill code to fill correctly. Add printout to terminal
c     as requested for traj points. This should really be an option, and show up
c     in a window.
c
c     Revision 1.4  2002/05/22 17:56:43  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.3  1999/02/08 23:20:38  warren
c     Final changes on trajectories. Color depth now works as does line thickness.
c
c     Revision 1.2  1997/10/22 19:35:02  davidive
c     Turned the particle movement part of trajectories into a user transform
c     and added unit checking to the default transform.
c
c     Revision 1.1  1996/04/25 16:25:04  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c                                                                       
      include 'data.icl'
      include 'lineplt.icl'
      include 'message.icl'
      include 'plotops.icl'
      include 'traj.icl'
      include 'flags.icl'
c
c
c     Argument declarations.
c
      logical         error
c
c
c     Local variable declarations.
c
      integer i, count, strbeg, strend
c
c
c
c
      error = .false.
c
c
c      if(savflg) then
c        if(traj_background_field .ne. old_traj_background)then
c           write(6,*)'traj_background_field ',traj_background_field
c           write(6,*)'old_traj_background',old_traj_background
c            write(message, 170)
c            call write_message
c            error = .true.
c            go to 10
c         end if
c      endif
      if (use_buttons .ne. 0) then
c
c     make sure the time button is locked
c
         if (lock(4).ne.1) then 
            write(message, 100)
            call write_message
            error = .true.
            go to 10
         end if
         do i = 1, 4
            if (lock(i) .eq. 2) then
               write(message, 130)
               call write_message
               error = .true.
               go to 10
            end if
         end do

      end if
c
c     make sure it is a 2d plot
c
      count = 0
      do i = 1, 3
         if (lock(i).eq.1) count = count+1
      end do
      if (count.ne.1) then    
           write(message, 160)
           call write_message
           error = .true.
           go to 10
        end if
c
c     check that the start point is in the window
c
      do i = 1,maxdim
         if ( start(i).gt.max(plmax(i),plmin(i))
     &        .or.  start(i).lt.min(plmin(i),plmax(i)))
     &               then
            write(message, 140)
            call write_message
            write(6,*)'start=',start
            write(6,*)'plmin=',plmin
            write(6,*)'plmax=',plmax
            error = .true.
            go to 10
         end if
      end do
c
c     check that the start and end points are within the times of the
c     datafile
c
      if (trajend.gt.max(plmax(4),plmin(4)) .or.  
     &     trajend.lt.min(plmin(4),plmax(4))) then 
            write(message, 150)
            call write_message
            error = .true.
            go to 10
         end if

      if (trajbeg.gt.max(plmax(4),plmin(4)) .or.  
     &     trajbeg.lt.min(plmin(4),plmax(4))) then 
            write(message, 150)
            call write_message
            error = .true.
            go to 10
         end if

         if(trajend .eq. trajbeg )then
            trajbeg = plmin(4)
            trajend = plmax(4)
         endif

c

 10   continue
c
c     Format statements.
c
 100  format (1x, 'The time slider should be fixed when making ',
     &     'trajectory plots')
 130  format (1x, 'Averages do not work in trajectories.')
 140  format (1x, 'The initial point specified lies ouside the data')
 150  format (1x, 'The trajectory time interval is not contained ',
     &            'within the data array.')
 160  format (1x, 'Trajectories should be two dimensional.')
 170  format (1x, 'You can not change the trajectory background ',
     &            'between overlays.')
      return
      end
