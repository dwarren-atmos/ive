      subroutine vdom (x, y, angle, wxmin, wxmax, wymin, wymax,
     &                 xi, xf, yi, yf)
c-----------------------------------------------------------------------
c     $Id: vdom.f,v 1.1.1.1 1993/04/07 19:40:42 harry Exp $
c     Purpose:
c        This routine calculates the beginning and ending points, and
c        the angle, of a vertical cross-section. This code was 
c        originally developed at the NCAR MMM Division.
c     Arguments:
c        x      real  input   the location of the slice along the first
c                             physical dimension.
c        y      real  input   the location of the slice along the second
c                             physical dimension.
c        angle  real  input   the slice angle, in radians.
c        wxmin  real  input   the minimum window dimension, in physical
c                             space, along the first dimension.
c        wxmax  real  input   the maximum window dimension, in physical
c                             space, along the first dimension.
c        wymin  real  input   the minimum window dimension, in physical
c                             space, along the second dimension.
c        wymax  real  input   the maximum window dimension, in physical
c                             space, along the second dimension.
c        xi     real  output  the coordinate, in physical space, of the
c                             beginning endpoint of the slice plane
c                             along the first dimension.
c        xf     real  output  the coordinate, in physical space, of the
c                             terminal endpoint of the slice plane.
c                             along the first dimension.
c        yi     real  output  the coordinate, in physical space, of the
c                             beginning endpoint of the slice plane
c                             along the second dimension.
c        yf     real  output  the coordinate, in physical space, of the
c                             terminal endpoint of the slice plane.
c                             along the second dimension.
c     History:
c        $Log: vdom.f,v $
c        Revision 1.1.1.1  1993/04/07 19:40:42  harry
c        Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:15:17  millerp
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
      real            angle, x, xf, xi, wxmax, wxmin
      real            y, yf, yi, wymax, wymin
c
c
c     Local variable declarations.
c
      real            ang1, ang2, ang3, ang4, angs, EPSLON, pi
      parameter ( EPSLON = 1.0e-10 )
c
c
c
c
      pi  = acos (-1.0)
c
c
      if ( abs(angle - (0.5 * pi)) .le. EPSLON ) then
c
c        Plot must be parallel to the x-axis.
c
         xi = wxmin
         xf = wxmax
         yi = y
         yf = yi
c
      elseif ( abs(angle) .le. EPSLON ) then
c
c        Plot must be parallel to the y-axis.
c
         xi = x
         xf = xi
         yi = wymin
         yf = wymax
c
      else
c
c
         if ((wymax - y) .lt. 0.0001) then
c
c           ANG1 is the angle associated with the line through 
c           (wxmax, wymax).
c
            ang1 = 0.5 * pi 
         else
            ang1 = atan ((wxmax - x) / (wymax - y))
         end if
c
         if ((y - wymin) .le. 0.0001) then
c
c           ANG2 is the angle associated with the line through 
c           (wxmax, wymin).
c           ANG3 is the angle associated with the line through 
c           (wxmin, wymin)
c
            ang2 = 0.5 * pi
            ang3 = 1.5 * pi
         else
            ang2 = pi - atan ((wxmax - x) / (y - wymin))
            ang3 = pi + atan ((x - wxmin) / (y - wymin))
         end if
c
         if ((x - wxmin) .le. 0.0001) then
c
c           ANG4 is the angle associated with the line through \
c           (wxmin, wymax).
c
            ang4 = 0.0
         else
            ang4 = 1.5 * pi + atan ((wymax - y) / (x - wxmin))
         end if
c
         if (angle .gt. ang1 .and. angle .le. ang2) then
c
c           Compute endpoint along right-side of window.
c
            xf = wxmax
            yf = (xf - x) / tan (angle) + y
        else if (angle .gt. ang2 .and. angle .le. ang3) then
c
c           Compute endpoint along bottom of window.
c
            yf = wymin
            xf = tan (angle) * (yf - y) + x
         else if (angle .gt. ang3 .and. angle .le. ang4) then
c
c           Compute endpoint along left-side of window.
c
            xf = wxmin
            yf = (xf - x) / tan (angle) + y
         else
c
c           Compute endpoint along top of window.
c
            yf = wymax
            xf = tan (angle) * (yf - y) + x
         end if
c
         if (angle .lt. pi) then
            angs = angle + pi
         else
            angs = angle - pi
         end if
c
         if (angs .gt. ang1 .and. angs .le. ang2) then
c
c           Compute beginning point along right-side of window.
c
            xi = wxmax
            yi = (xi - x) / tan (angle) + y
         else if (angs .gt. ang2 .and. angs .le. ang3) then
c
c           Compute endpoint along bottom of window.
c
            yi = wymin
            xi = tan (angle) * (yi - y) + x
         else if (angs .gt. ang3 .and. angs .le. ang4) then
c
c           Compute endpoint along left-side of window.
c
            xi = wxmin
            yi = (xi - x) / tan (angle) + y
         else
c
c           Compute endpoint along top of window.
c
            yi = wymax
            xi = tan (angle) * (yi - y) + x
         end if
c
c        Check values of xi, xf, yi, yf for possible round-off error. 
c
         if ( xi .lt. wxmin ) xi = wxmin
         if ( xi .gt. wxmax ) xi = wxmax
c
         if ( xf .lt. wxmin ) xf = wxmin
         if ( xf .gt. wxmax ) xf = wxmax
c 
         if ( yi .lt. wymin ) yi = wymin
         if ( yi .gt. wymax ) yi = wymax
c
         if ( yf .lt. wymin ) yf = wymin
         if ( yf .gt. wymax ) yf = wymax
      endif
c
c
c
      return
      end

