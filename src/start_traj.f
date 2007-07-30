      subroutine start_traj (error)
c
c     $Id: start_traj.f,v 1.2 1997/10/22 19:35:09 davidive Exp $
c
c     This routine processes the START_TRAJECTORY command
c
c     START_TRAJECTORY=x,y,z,t
c
c     where "x,y,z,t" is the point where the trajectory starts.
c
c     $Log: start_traj.f,v $
c     Revision 1.2  1997/10/22 19:35:09  davidive
c     Turned the particle movement part of trajectories into a user transform
c     and added unit checking to the default transform.
c
c     Revision 1.1  1996/04/25 16:25:34  harry
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
      integer       i, numval, im1
      real value
c     
c
c     External function declarlations.
c
      integer       nblank, strbeg, strend
c
c
c 
c
c     Find out how many nonblank values are specified. If there are no
c     nonblank values, set start to its current value and bail.
c
      numval = 0
      do 5 i = fstval, maxcom
         if ( nblank(comand(i)) .ne. 0 )  numval = numval + 1
 5    continue
      if ( numval .eq. 0 ) then
         do i = 1, MAXDIM
            start(i) = start(i)
         enddo
         go to 45
      endif

c
c     get the start points
c     
      do 20 i = 1,4
         im1=i-1
         if ( comand(fstval+im1)(1:cmdlen) .ne. ' ' ) then
            call cvatof ( comand(fstval+im1)(strbeg(comand(fstval+im1)):
     &           strend(comand(fstval+im1))),value, error )
            if ( error ) then
               write (message, 100) 
     &              comand(fstval+im1)(strbeg(comand(fstval+1)):
     &              strend(comand(fstval+im1)))
     &              ,comand(1)(strbeg(comand(1)):strend(comand(1)))
               call write_message
               go to 45
            else
               call unscale (value, 1, domain_slope(i),
     &              domain_intercept(i),0.0)
               start(i) = value
            endif
         endif
 20   continue

 45   continue
c     
c     Format statements.
c
 100  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end



