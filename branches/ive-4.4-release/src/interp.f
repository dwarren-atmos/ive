c
c     This file contains both interp and extrap
c
c     $Id: interp.f,v 1.5 1996/02/23 22:31:27 harry Exp $
c
c     History:
c        $Log: interp.f,v $
c        Revision 1.5  1996/02/23 22:31:27  harry
c        Fix for reset_spval feature for new slicer.
c
c        Revision 1.4  1995/11/14 18:31:44  harry
c        Code to properly change variables between two grids (a@b).
c
c Revision 1.3  1995/08/28  17:26:05  harry
c Minimum Fortran subscript is 1.0, not 0.0.
c
c Revision 1.2  1995/05/25  16:08:00  harry
c Change code to speed up interp for integer computational coordinates.
c
c Revision 1.1.1.1  1993/04/07  19:40:05  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/09/08  19:56:01  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:28  millerp
c Released version rereving to 2.0
c
c Revision 1.4  1992/06/26  16:40:35  millerp
c Passing back the value of spval if this routine must set it.
c
c Revision 1.3  1992/06/17  16:28:48  millerp
c Fixed dec compiler errors.
c
c Revision 1.2  1992/06/15  19:06:43  millerp
c *** empty log message ***
c
      function interp (data, nx, ny, nz, nt, 
     &                 reali, realj, realk, realt, spval)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine interpolates the data stored in the array data
c        to the grid point (reali, realj, realk, realt), where this is 
c        some arbitrary point in the array. It is assumed that the data 
c        is floating-point and the data array contains no "holes".
c     Arguments:
c        data  real   input   the data array.
c        nx    int    input   the first dimension of data.
c        ny    int    input   the second dimension of data. 
c        nz    int    input   the third dimension of data. 
c        nt    int    input   the fourth dimension of data. 
c        reali  real  input   the coordinate along the first dimension
c                             to be interpolated to.
c        realj  real  input   the coordinate along the second dimension
c                             to be interpolated to. 
c        realk  real  input   the coordinate along the third dimension
c                             to be interpolated to. 
c        realt  real  input   the coordinate along the fourth dimension
c                             to be interpolated to. 
c        spval  real  input   If the array data contains missing data 
c                             values, set spval equal to the (assumed
c                             nonzero) missing data value. 
c                             If any point used in the interpolation is 
c                             equal to spval, the interpolated value is
c                             set equal to spval. NOTE: This routine is
c                             most efficient when spval = 0.
c                             
c     Output :
c        On output the function value is interpolated value at the
c        point (reali, realj, realk, realt), or spval.
c
c-----------------------------------------------------------------------
c
c
c     Function declaration.
c
      double precision interp
c
c
c     Argument declarations. 
c
      integer    nx, ny, nz, nt
      real       data, reali, realj, realk, realt, spval
      dimension data(nx, ny, nz, nt)
c
c
c     Local variable declarations.
c
      real       SPECIAL
      parameter ( SPECIAL = -10.0**27 )
      double precision  fracti, fractim, fractj, fractjm, fractk, 
     &                  fractkm, fractt, fracttm
      integer    i, ip1, j, jp1, k, kp1, t, tp1
      logical    error
c
c
c
c     Make sure we are not under ground. Recall that the transforms
c     return a negative value for compt if a point in physical space
c     falls below topography. Test all points here for the most general
c     case.
c
      if ( (reali .lt. 1.0) .or. (realj .lt. 1.0) .or. 
     &     (realk .lt. 1.0) .or. (realt .lt. 1.0) .or.
     &     (reali .gt. nx) .or. (realj .gt. ny) .or.
     &     (realk .gt. nz) .or. (realt .gt. nt)) then
c
c        Point is outside computational grid space. In this case, 
c        set interp to the special value so contouring routines will 
c        ignore this point. 
c
         if (spval .eq. 0.0) then
c
c           Set a value for spval for use by the contouring routines.
c           Also, set a flag indicating that spval has been set in
c           the graphics routines, so we can unset spval after
c           the next slice.
c
            call setrvar('spval', SPECIAL, error)
            call setlvar ('reset_spval', .true., error)
            interp = SPECIAL
            spval = SPECIAL
         else
            interp = spval
         endif
         return
      endif
c
c
c     Set up pointers so that we interpolate between points.
c
      i = int (reali)
      j = int (realj) 
      k = int (realk) 
      t = int (realt) 
c
c     special case for a point in the array
c
      if (i.eq.reali .and. j.eq.realj 
     &     .and. k.eq.realk .and .t.eq.realt) then
         interp = data(i,j,k,t)
         return
      endif

      ip1 = i + 1
      if ( ip1 .gt. nx .or. i .eq. reali) ip1 = i
c
      jp1 = j + 1
      if ( jp1 .gt. ny .or. j .eq. realj) jp1 = j
c
      kp1 = k + 1
      if ( kp1 .gt. nz .or. k .eq. realk) kp1 = k
c
      tp1 = t + 1
      if ( tp1 .gt. nt .or. t .eq. realt) tp1 = t
c
c
      if ( spval .ne. 0.0 ) then
c
c        Check for special value.
c
         if ( (data(i  , j  , k  ,t  ) .eq. spval) .or. 
     &        (data(ip1, j  , k  ,t  ) .eq. spval) .or. 
     &        (data(i  , jp1, k  ,t  ) .eq. spval) .or. 
     &        (data(ip1, jp1, k  ,t  ) .eq. spval) .or. 
     &        (data(i  , j  , kp1,t  ) .eq. spval) .or. 
     &        (data(ip1, j  , kp1,t  ) .eq. spval) .or.  
     &        (data(i  , jp1, kp1,t  ) .eq. spval) .or.  
     &        (data(ip1, jp1, kp1,t  ) .eq. spval) .or.  
     &        (data(i  , j  , k  ,tp1) .eq. spval) .or.  
     &        (data(ip1, j  , k  ,tp1) .eq. spval) .or.  
     &        (data(i  , jp1, k  ,tp1) .eq. spval) .or.  
     &        (data(ip1, jp1, k  ,tp1) .eq. spval) .or.   
     &        (data(i  , j  , kp1,tp1) .eq. spval) .or. 
     &        (data(ip1, j  , kp1,tp1) .eq. spval) .or. 
     &        (data(i  , jp1, kp1,tp1) .eq. spval) .or.  
     &        (data(ip1, jp1, kp1,tp1) .eq. spval)     ) then
            interp = spval
            return
         endif
      endif
c
c
c     Do linear interpolation to the point 
c     (reali, realj, realk, realt). Use double precision to increase
c     accuracy.
c
      fracti  = dble(reali) - dble(i)
      fractim = 1.0 - fracti
c
      fractj  = dble(realj) - dble(j)
      fractjm = 1.0 - fractj
c
      fractk  = dble(realk) - dble(k)
      fractkm = 1.0 - fractk
c
      fractt  = dble(realt) - dble(t)
      fracttm = 1.0 - fractt
c
      interp = (  
     & dble(data(i  , j  , k  ,t  ))*fractim*fractjm*fractkm*fracttm +
     & dble(data(ip1, j  , k  ,t  ))*fracti *fractjm*fractkm*fracttm +
     & dble(data(i  , jp1, k  ,t  ))*fractim*fractj *fractkm*fracttm +
     & dble(data(ip1, jp1, k  ,t  ))*fracti *fractj *fractkm*fracttm +
     & dble(data(i  , j  , kp1,t  ))*fractim*fractjm*fractk *fracttm +
     & dble(data(ip1, j  , kp1,t  ))*fracti *fractjm*fractk *fracttm +
     & dble(data(i  , jp1, kp1,t  ))*fractim*fractj *fractk *fracttm +
     & dble(data(ip1, jp1, kp1,t  ))*fracti *fractj *fractk *fracttm +
     & dble(data(i  , j  , k  ,tp1))*fractim*fractjm*fractkm*fractt  +
     & dble(data(ip1, j  , k  ,tp1))*fracti *fractjm*fractkm*fractt  +
     & dble(data(i  , jp1, k  ,tp1))*fractim*fractj *fractkm*fractt  +
     & dble(data(ip1, jp1, k  ,tp1))*fracti *fractj *fractkm*fractt  +
     & dble(data(i  , j  , kp1,tp1))*fractim*fractjm*fractk *fractt  +
     & dble(data(ip1, j  , kp1,tp1))*fracti *fractjm*fractk *fractt  +
     & dble(data(i  , jp1, kp1,tp1))*fractim*fractj *fractk *fractt  +
     & dble(data(ip1, jp1, kp1,tp1))*fracti *fractj *fractk *fractt  )
c
c
      return
      end

c-----------------------------------------------------------------------
 
      function extrap (data, nx, ny, nz, nt, 
     &                 reali, realj, realk, realt, spval)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine interpolates the data stored in the array data
c        to the grid point (reali, realj, realk, realt), where this is 
c        some arbitrary point in the array. It is assumed that the data 
c        is floating-point and the data array contains no "holes".
c        This routine also extrapolates if the requested point is outside
c        the current domain.
c     Arguments:
c        data  real   input   the data array.
c        nx    int    input   the first dimension of data.
c        ny    int    input   the second dimension of data. 
c        nz    int    input   the third dimension of data. 
c        nt    int    input   the fourth dimension of data. 
c        reali  real  input   the coordinate along the first dimension
c                             to be interpolated to.
c        realj  real  input   the coordinate along the second dimension
c                             to be interpolated to. 
c        realk  real  input   the coordinate along the third dimension
c                             to be interpolated to. 
c        realt  real  input   the coordinate along the fourth dimension
c                             to be interpolated to. 
c        spval  real  input   If the array data contains missing data 
c                             values, set spval equal to the (assumed
c                             nonzero) missing data value. 
c                             If any point used in the interpolation is 
c                             equal to spval, the interpolated value is
c                             set equal to spval. NOTE: This routine is
c                             most efficient when spval = 0.
c                             
c     Output :
c        On output the function value is either the interpolated or 
c        extrapolated value at the point (reali, realj, realk, realt),
c        or spval.
c-----------------------------------------------------------------------
c
c
c     Function declaration.
c
      double precision extrap
c
c
c     Argument declarations. 
c
      integer    nx, ny, nz, nt
      real       data, reali, realj, realk, realt, spval
      dimension data(nx, ny, nz, nt)
c
c
c     Local variable declarations.
c
      real       SPECIAL
      parameter ( SPECIAL = -10.0**27 )
      double precision  fracti, fractim, fractj, fractjm, fractk, 
     &                  fractkm, fractt, fracttm
      integer    i, ip1, j, jp1, k, kp1, t, tp1
      logical    error
c
c
c
c     Make sure we are not under ground. Recall that the transforms
c     return a negative value for compt if a point in physical space
c     falls below topography. Test all points here for the most general
c     case.
c
      if ( (reali .lt. 0.0) .or. (realj .lt. 0.0) .or. 
     &     (realk .lt. 0.0) .or. (realt .lt. 0.0)) then
c
c        Point is under ground. In this case, 
c        set interp to the special value so contouring routines will 
c        ignore this point. 
c
         if (spval .eq. 0.0) then
c
c           Set a value for spval for use by the contouring routines.
c           Also, set a flag indicating that spval has been set in
c           the graphics routines, so we can unset spval after
c           the plot is drawn.
c
            call setrvar('spval', SPECIAL, error)
            call setlvar ('reset_spval', .true., error)
            extrap = SPECIAL
            spval = SPECIAL
         else
            extrap = spval
         endif
         return
      endif
c
c
c     Set up pointers so that we either interpolate between points or 
c     extrapolate outside the data domain.
c
      if (reali.lt.1) then
         i = 1
      else if (reali.gt.nx) then
         i = nx - 1
      else
         i = int(reali)
      endif

      if (realj.lt.1) then
         j = 1
      else if (realj.gt.ny) then
         j = ny - 1
      else
         j = int(realj)
      endif

      if (realk.lt.1) then
         k = 1
      else if (realk.gt.nz) then
         k = nz - 1
      else
         k = int(realk)
      endif

      if (realt.lt.1) then
         t = 1
      else if (realt.gt.nt) then
         t = nt - 1
      else
         t = int(realt)
      endif
c
c     special case for a point in the array
c
      if (i.eq.reali .and. j.eq.realj 
     &     .and. k.eq.realk .and .t.eq.realt) then
         extrap = data(i,j,k,t)
         return
      endif

      ip1 = i + 1
      if ( i .eq. reali) ip1 = i
c
      jp1 = j + 1
      if ( j .eq. realj) jp1 = j
c
      kp1 = k + 1
      if ( k .eq. realk) kp1 = k
c
      tp1 = t + 1
      if ( t .eq. realt) tp1 = t
c
c
      if ( spval .ne. 0.0 ) then
c
c        Check for special value.
c
         if ( (data(i  , j  , k  ,t  ) .eq. spval) .or. 
     &        (data(ip1, j  , k  ,t  ) .eq. spval) .or. 
     &        (data(i  , jp1, k  ,t  ) .eq. spval) .or. 
     &        (data(ip1, jp1, k  ,t  ) .eq. spval) .or. 
     &        (data(i  , j  , kp1,t  ) .eq. spval) .or. 
     &        (data(ip1, j  , kp1,t  ) .eq. spval) .or.  
     &        (data(i  , jp1, kp1,t  ) .eq. spval) .or.  
     &        (data(ip1, jp1, kp1,t  ) .eq. spval) .or.  
     &        (data(i  , j  , k  ,tp1) .eq. spval) .or.  
     &        (data(ip1, j  , k  ,tp1) .eq. spval) .or.  
     &        (data(i  , jp1, k  ,tp1) .eq. spval) .or.  
     &        (data(ip1, jp1, k  ,tp1) .eq. spval) .or.   
     &        (data(i  , j  , kp1,tp1) .eq. spval) .or. 
     &        (data(ip1, j  , kp1,tp1) .eq. spval) .or. 
     &        (data(i  , jp1, kp1,tp1) .eq. spval) .or.  
     &        (data(ip1, jp1, kp1,tp1) .eq. spval)     ) then
            extrap = spval
            return
         endif
      endif
c
c
c     Do linear interpolation/extrapolation to the point 
c     (reali, realj, realk, realt). Use double precision to increase
c     accuracy.
c
      fracti  = dble(reali) - dble(i)
      fractim = 1.0 - fracti
c
      fractj  = dble(realj) - dble(j)
      fractjm = 1.0 - fractj
c
      fractk  = dble(realk) - dble(k)
      fractkm = 1.0 - fractk
c
      fractt  = dble(realt) - dble(t)
      fracttm = 1.0 - fractt
c
      extrap = (  
     & dble(data(i  , j  , k  ,t  ))*fractim*fractjm*fractkm*fracttm +
     & dble(data(ip1, j  , k  ,t  ))*fracti *fractjm*fractkm*fracttm +
     & dble(data(i  , jp1, k  ,t  ))*fractim*fractj *fractkm*fracttm +
     & dble(data(ip1, jp1, k  ,t  ))*fracti *fractj *fractkm*fracttm +
     & dble(data(i  , j  , kp1,t  ))*fractim*fractjm*fractk *fracttm +
     & dble(data(ip1, j  , kp1,t  ))*fracti *fractjm*fractk *fracttm +
     & dble(data(i  , jp1, kp1,t  ))*fractim*fractj *fractk *fracttm +
     & dble(data(ip1, jp1, kp1,t  ))*fracti *fractj *fractk *fracttm +
     & dble(data(i  , j  , k  ,tp1))*fractim*fractjm*fractkm*fractt  +
     & dble(data(ip1, j  , k  ,tp1))*fracti *fractjm*fractkm*fractt  +
     & dble(data(i  , jp1, k  ,tp1))*fractim*fractj *fractkm*fractt  +
     & dble(data(ip1, jp1, k  ,tp1))*fracti *fractj *fractkm*fractt  +
     & dble(data(i  , j  , kp1,tp1))*fractim*fractjm*fractk *fractt  +
     & dble(data(ip1, j  , kp1,tp1))*fracti *fractjm*fractk *fractt  +
     & dble(data(i  , jp1, kp1,tp1))*fractim*fractj *fractk *fractt  +
     & dble(data(ip1, jp1, kp1,tp1))*fracti *fractj *fractk *fractt  )
c
c
      return
      end

 
