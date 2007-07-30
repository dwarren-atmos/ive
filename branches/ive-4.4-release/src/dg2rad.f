      function dg2rad (angle)
c-----------------------------------------------------------------------
c     $Id: dg2rad.f,v 1.1.1.1 1993/04/07 19:40:32 harry Exp $
c     Purpose:
c        This function converts an input angle, in degrees, to an angle
c        in radians.
c     Arguments:
c        angle  real  input  an angle in degrees.
c     History:
c      $Log: dg2rad.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:32  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:02  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c     Function declaration.
c
      real dg2rad
c
c
c     Argument declaration.
c
      real angle
c
c
c
c
c     Convert angle to radians.
c
      dg2rad = angle * (acos(-1.0)/180.0)
c
c
      return
      end
c
c
      function rad2dg (angle)
c-----------------------------------------------------------------------
c     Purpose:
c        This function converts an input angle, in radians, to an angle
c        in degrees.
c     Arguments:
c        angle  real  input  an angle in radians.
c-----------------------------------------------------------------------
c
c
c     Function declaration.
c
      real rad2dg
c
c
c     Argument declaration.
c
      real angle
c
c
c
c
c     Convert angle to degrees.
c
      rad2dg = angle * ( 180.0 / acos(-1.0) )
c
c
      return
      end

