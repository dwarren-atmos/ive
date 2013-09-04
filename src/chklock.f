      subroutine chklock(ival)
c-----------------------------------------------------------------------
c     $Id: chk2d.f,v 1.13 2000/04/05 22:38:52 harry Exp $
c     Purpose:
c        This subroutine returns 1,2,3 or 0 for 1,2,3 dimension or no selection
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'message.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
c
c
c     Local variable declarations.
c
      integer ival, lock_count, i, match_count, s12,s23,s13
      real    EPSLON 
      parameter ( EPSLON = 1.0e-4 )
c
c
c
c     
c     
c     
c     Is the slice location set and does the location fall within the
c     data domain?
c     
c     
      if (surface) then
         ival = 2
         return 
      endif
      if (use_buttons .ne. 0) then
         lock_count = 0
         do i = 1, MAXDIM
            if ( lock(i) .gt. 0 ) lock_count = lock_count + 1
         enddo
         if (lock_count .eq. 2) then
            ival = 2
            return
         elseif (lock_count .eq. 3) then
            ival = 1
            return
         elseif (lock_count .eq. 1) then
            ival = 3
            return
         else
            ival = 0
            return
         endif
      else 
c     
c     This means that points were specified
c     
         lock_count = 0
         match_count=0
         s12 = 0
         s13 = 0
         s23 = 0
         do i = 1, MAXDIM
            if ((point_1(i) .eq. point_2(i)) .and. 
     &           (point_1(i) .eq. point_3(i))) then
               lock_count=lock_count+1
            elseif((point_1(i) .eq. point_2(i)) .or.
     &              (point_1(i) .eq. point_3(i)) .or.
     &              ( point_2(i).eq. point_3(i))) then
               match_count = match_count +1
            endif
            if(point_1(i) .eq. point_2(i))s12 = s12 + 1
            if(point_1(i) .eq. point_3(i))s12 = s13 + 1
            if(point_3(i) .eq. point_2(i))s12 = s23 + 1
         enddo
         if (lock_count .eq. 2 .or.
     &        (lock_count .eq. 1 .and. match_count .eq. 3)) then
            ival = 2
            return
         elseif (lock_count .eq. 3 .or.
     &        (s12 == 3 .and. s23 .ne. 3 .and. s13 .ne.3) .or.
     &        (s13 == 3 .and. s23 .ne. 3 .and. s12 .ne.3) .or.
     &        (s23 == 3 .and. s12 .ne. 3 .and. s13 .ne.3))then
            ival = 1
            return
         elseif (lock_count .eq. 1 .and. point_1(4) .eq. 
     &           point_2(4) .and. point_1(4) .eq. point_3(4)) then
            ival = 3
            return
         else
            ival = 0
         endif
         return
      endif
      return
      end
