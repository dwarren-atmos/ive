      function ive_maxval (data, npts, spval)
c-----------------------------------------------------------------------
c     $Id: maxval.f,v 1.2 1997/02/27 16:22:46 warren Exp $
c     Purpose:
c        This function finds the maximum value in the input array.
c     Arguments:
c        data   real  input   the array to be searched for its highest 
c                             value.
c        npts   int   input   the number of points in data.
c        spval  real  input   a special value. If spval is nonzero, then
c                             any value in the array equal to spval
c                             is ignored.
c     History:
c        $Log: maxval.f,v $
c        Revision 1.2  1997/02/27 16:22:46  warren
c        put lock_int in common block where it belongs and changed maxval and minval
c        to ive_maxval and ive_minval to not interfere with the f90 intrinsic.
c        Also changed maxval in uwperim to ive_maxval_per to distinguish the
c        variable from the function.
c
c        Revision 1.1.1.1  1993/04/07 19:39:52  harry
c        Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:05  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/06/15  19:16:58  millerp
c Fixed potential bug.
c
c Revision 1.2  1992/05/04  23:53:05  millerp
c Added checks for special value.
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c     Function declaration.
c
      real        ive_maxval
c
c
c     Argument declarations.
c
      integer     npts
      real        data, spval
      dimension data(npts)
c
c
c     Local variable declarations.
c
      integer     i, start
c
c
c
c
      if ( spval .eq. 0.0 ) then
c
c        Case where special value is not used.
c
         ive_maxval = data(1)
         do 10 i = 2, npts
            if ( data(i) .gt. ive_maxval ) then
               ive_maxval = data(i)
            endif
 10      continue
      else
c
c        Case where special value is used. 
c
c        Find the first non-special value.
c
         start = 0
         do 15 i = 1, npts
            if ( data(i) .ne. spval ) then
               start = i
               go to 18
            endif
 15      continue
c
c        Now find the largest element in the array.
c
 18      if ( start .eq. 0 ) then
c
c           If all array elements == spval, return spval.
c
            ive_maxval = spval
         else
c
c           Search the array for the maximum value.
c
            ive_maxval = data(start)
            do 20 i = start + 1, npts
               if ( data(i) .gt. ive_maxval ) then
                  if ( data(i) .ne. spval ) ive_maxval = data(i)
               endif
 20         continue
         endif
      endif
c     
c
      return
      end
c
c
      function ive_minval (data, npts, spval)
c-----------------------------------------------------------------------
c     Purpose:
c        This function finds the minumum value in the input array.
c     Arguments:
c        data   real  input   the array to be searched for its lowest 
c                             value.
c        npts   int   input   the number of points in data.
c        spval  real  input   a special value. If spval is nonzero, then
c                             any value in the array equal to spval
c                             is ignored.
c     History:
c-----------------------------------------------------------------------
c
c     Function declaration.
c
      real        ive_minval
c
c
c     Argument declarations.
c
      integer     npts
      real        data, spval
      dimension data(npts)
c
c
c     Local variable declarations.
c
      integer     i, start
c
c
c
c
      if ( spval .eq. 0.0 ) then
c
c        Case where special value is not used.
c
         ive_minval = data(1)
         do 10 i = 2, npts
            if ( data(i) .lt. ive_minval ) then
               ive_minval = data(i)
            endif
 10      continue
      else
c
c        Case where special value is used.
c
c        Find the first non-special value.
c
         start = 0
         do 15 i = 1, npts
            if ( data(i) .ne. spval ) then
               start = i
               go to 18
            endif
 15      continue
c
c        Now find the smallest element in the array.
c
 18      if ( start .eq. 0 ) then
c
c           If all array elements == spval, return spval.
c
            ive_minval = spval
         else
c
c           Search the array for the minimum value.
c
            ive_minval = data(start)
            do 20 i = start + 1, npts
               if ( data(i) .lt. ive_minval ) then
                  if ( data(i) .ne. spval ) ive_minval = data(i)
               endif
 20         continue
         endif
      endif
c     
c
      return
      end
