      subroutine vlimit (nh, nz, dhcos, dhsin, xi, yi, plmin, plmax, 
     &                   plwmin, plwmax,lolim, uplim)
c-----------------------------------------------------------------------
c     $Id: vlimit.f,v 1.2 1993/08/18 15:47:52 harry Exp $
c     Purpose:
c        This routine is used by vcross to determine the limits of a 
c        vertical slab in computational space. Since vertical coordinate
c        transforms may be present, this seemingly cumbersome method 
c        must be used.
c     Arguments:
c        nh      int  input    the number of horizontal points in the 
c                              slab.
c        nz      int  input    the number of vertical points in the 
c                              data.
c        dhcos   real  input   (horizontal distance between points in 
c                              the slab) * cos(slice angle).
c        dhsir   real  input   (horizontal distance between points in 
c                              the slab) * sin(slice angle).
c        xi      real  input   the intersection of the slice plane and 
c                              the x axis in physical space.
c        yi      real  input   the intersection of the slice plane and 
c                              the y axis in physical space.
c        plmin   real  input   the minimum dimensions of the plot.
c        plmax   real  input   the maximum dimensions of the plot.
c        plwmin  real  input   the minimum dimensions of the plot 
c                              window.
c        plwmax  real  input   the maximum dimensions of the plot 
c                              window.
c        lolim   int   output  the lower limit of the plot window in the
c                              computational domain.
c        uplim   int   output  the upper limit of the plot window in the
c                              computational domain.
c     History:
c       $Log: vlimit.f,v $
c       Revision 1.2  1993/08/18 15:47:52  harry
c       Change to new transform routines.
c
c Revision 1.1.1.1  1993/04/07  19:40:44  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:15:25  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer        nh, nz, lolim, uplim
      real           dhcos, dhsin, xi, yi, plmin, plmax, plwmin, plwmax
      dimension      plmin(4), plmax(4), plwmin(4), plwmax(4)
c
c
c     Local variable declarations.
c     
      integer        i, iflag, j
      real           compt, im1, phypt
      dimension iflag(4), compt(4), phypt(4)
c
c
c
c
c
      iflag(1) = 1
      iflag(2) = 1
c
c
      if ( plmin(3) .lt. plmax(3) ) then
         do 20 j = 1, nz
            compt(3) = float(j)
            do 10 i = 1, nh
               im1 = float (i - 1) 
               phypt(1) = xi + im1 * dhsin 
               phypt(2) = yi + im1 * dhcos
               iflag(3) = 0
               call phys_2_index_trans (phypt, compt, iflag, 3, 1)
               iflag(3) = 1
c
c              Check (compt(1), compt(2), j) to see if it falls
c              within the vertical window. 
c
               call index_2_phys_trans (phypt, compt, iflag, 3, 1)
               if ( phypt(3) .ge. plwmin(3) ) then
c
c                 As long as any point in this row falls within the plot
c                 window we will include the entire row in the vertical
c                 slab. If any points in this row fall outside the 
c                 window, the conpack out-of-range feature will be 
c                 used to eliminate those points from the contour plot.
c
                  lolim = j
                  go to 30
               endif
 10         continue
 20      continue
c
 30      continue
c
         do 50 j = nz, 1, -1
            compt(3) = float(j)
            do 40 i = 1, nh
               im1 = float (i - 1) 
               phypt(1) = xi + im1 * dhsin 
               phypt(2) = yi + im1 * dhcos
               iflag(3) = 0
               call phys_2_index_trans (phypt, compt, iflag, 3, 1)
               iflag(3) = 1
               call index_2_phys_trans (phypt, compt, iflag, 3, 1)
               if ( phypt(3) .le. plwmax(3) ) then
                  uplim = j
                  go to 60
               endif
 40         continue
 50      continue
c
 60      continue
c
      else
c
         do 80 j = 1, nz
            compt(3) = float(j)
            do 70 i = 1, nh
               im1 = float (i - 1) 
               phypt(1) = xi + im1 * dhsin 
               phypt(2) = yi + im1 * dhcos
               iflag(3) = 0
               call phys_2_index_trans (phypt, compt, iflag, 3, 1)
               iflag(3) = 1
               call index_2_phys_trans (phypt, compt, iflag, 3, 1)
               if ( phypt(3) .le. plwmin(3) ) then
                  lolim = j
                  go to 90
               endif
 70         continue
 80      continue
c
 90      continue
c
         do 110 j = nz, 1, -1
            compt(3) = float(j)
            do 100 i = 1, nh
               im1 = float (i - 1) 
               phypt(1) = xi + im1 * dhsin 
               phypt(2) = yi + im1 * dhcos
               iflag(3) = 0
               call phys_2_index_trans (phypt, compt, iflag, 3, 1)
               iflag(3) = 1
               call index_2_phys_trans (phypt, compt, iflag, 3, 1)
               if ( phypt(3) .ge. plwmax(3) ) then
                  uplim = j
                  go to 120
               endif
 100        continue
 110      continue
c
 120      continue
c
c
       endif
c
c
       return
       end
