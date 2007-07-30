      subroutine int_traj (error)
c
c     $Id: int_traj.f,v 1.5 2002/05/30 21:42:12 warren Exp $
c
c     This routine processes the INT_TRAJ command
c
c     INT_TRAJ=start,end
c
c     where "start" is the start time for the trajectory, and
c     "end" is the end time for the trajectory.
c
c     $Log: int_traj.f,v $
c     Revision 1.5  2002/05/30 21:42:12  warren
c     Sun compiler found some typo's such as ';'s in fortran.
c
c     Revision 1.4  2002/05/22 17:56:45  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.3  1998/12/31 16:01:41  warren
c     First cut of new trajectory look and feel. Trajectories are no longer
c     on the popup menu, just plot type.
c
c     Revision 1.2  1997/10/22 19:35:06  davidive
c     Turned the particle movement part of trajectories into a user transform
c     and added unit checking to the default transform.
c
c     Revision 1.1  1996/04/25 16:25:17  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      include 'incommands.icl'
      include 'data.icl'
      include 'units.icl'
      include 'message.icl'
      include 'traj.icl'
c
c
c     Argument declarations.
c
      logical       error
c
c
c     Local variable declarations.
c
      integer       i, numval
      real value
c     
c
c     External function declarlations.
c
      integer       nblank, strbeg, strend
c
c
c     Find out how many nonblank values are specified. If there are no
c     nonblank values, set int to the endpoints of the dataset and bail
c     
      numval = 0
      do 5 i = fstval, maxcom
         if ( nblank(comand(i)) .ne. 0 )  numval = numval + 1
 5    continue
      if ( numval .eq. 0 ) then
         trajbeg = phmin(4)
         trajend = phmax(4)
         go to 45
      endif
c     
c
c     get the start time
c     
      if ( comand(fstval)(1:cmdlen) .ne. ' ' ) then
         call cvatof ( comand(fstval)(strbeg(comand(fstval)):
     &        strend(comand(fstval))),value, error )
         if ( error ) then
            write (message, 100) 
     &           comand(fstval)(strbeg(comand(fstval)):
     &           strend(comand(fstval)))
     &           ,comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            go to 45
         else
            call unscale (value, 1, domain_slope(4),
     &           domain_intercept(4),0.0)
            trajbeg = value
         endif
      endif
c     find tend

      if ( comand(fstval+1)(1:cmdlen) .ne. ' ' ) then
         call cvatof ( comand(fstval+1)(strbeg(comand(fstval+1)):
     &        strend(comand(fstval+1))),value, error )
         if ( error ) then
            write (message, 100) 
     &           comand(fstval+1)(strbeg(comand(fstval)):
     &           strend(comand(fstval+1)))
     &           ,comand(1)(strbeg(comand(1)):strend(comand(1)))
            call write_message
            go to 45
         else
            call unscale (value, 1, domain_slope(4),
     &           domain_intercept(4),0.0)
            trajend = value
         endif
      endif
c     
c     check to see if trajbeg and trajend lie in the data array
c
c      if (trajbeg.lt.phmin(4)) then
c         write (message, 110) trajbeg 
c         call write_message
c         error = .true.
c      end if

c      if (trajend.gt.phmax(4)) then
c         write (message, 110) trajend
c         call write_message
c         error = .true.
c      end if

c     Make sure trajbeg < trajend
      if(trajbeg .gt. trajend) then
         value = trajend
         trajend = trajbeg
         trajbeg = value
         write (message, *) 'Trajbeg was set to be greater than Trajend'
     &        //' Time must run forward. To do a backwards trajectory '
     &        //'set your starting point at the final time. Your time'
     &        //' has been corrected.'
         call write_message
      endif

 45   continue
c     
c     Format statements.
c
 100  format (1x,a,' is an incorrect value for command ',a,'.')
 110  format (1x, f8.2,' lies outside the times contained ',
     &     'in the data.')
      
c
c
      return
      end



