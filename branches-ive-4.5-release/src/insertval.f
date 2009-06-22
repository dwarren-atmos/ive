      subroutine insertval (array, npts, loc, value)
c-----------------------------------------------------------------------
c     $Id: insertval.f,v 1.1.1.1 1993/04/07 19:40:04 harry Exp $
c     Purpose:
c        This routine is used to insert value into array. It is needed
c        when arrays are allocated in the calling routine, and are 
c        passed via %val in the argument list.
c     Arguments:
c        array  real  input  the origin of the array.
c        npts   int   input  the number of points in array.
c        loc    int   input  the location in array where the value will
c                            be inserted.
c        value  real  input  the value to be inserted into array.
c     History:
c     $Log: insertval.f,v $
c     Revision 1.1.1.1  1993/04/07 19:40:04  harry
c     Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:13:30  millerp
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
      integer      npts, loc
      real         array, value
      dimension array(npts)
c
c
c
c
      array(loc) = value
c
c
      return
      end
