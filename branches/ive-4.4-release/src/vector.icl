c-----------------------------------------------------------------------
c     $Id: vector.icl,v 1.7 1999/02/04 00:28:31 harry Exp $
c     Purpose:
c       This file carries the velvect common blocks.
c     History:
c     $Log: vector.icl,v $
c     Revision 1.7  1999/02/04 00:28:31  harry
c     Add vector scaling.
c
c     Revision 1.6  1997/02/21 20:02:11  harry
c     Fix vector interval to be floating point and have values less than one.
c     Handling of vector interval is now in the slicer.
c
cRevision 1.5  1994/11/22  22:48:10  harry
cCorrect vector locking, add support for vector positioning, add
csupport for filled contour overlays.  Use new NCAR vector plotting
croutine instead of modified old velvct.
c
cRevision 1.4  1993/11/15  20:15:41  warren
crearranged common blocks for alignment.
c
cRevision 1.3  1993/07/23  20:37:03  yuri
c	Command "vector_lock" is introduced to lock vector
cscaling in a loop (for example). Also some minor bugs are corrected.
c
cRevision 1.2  1993/06/10  16:35:10  yuri
cJust cleaning.
c
cRevision 1.1.1.1  1993/04/07  19:40:43  harry
cInitial CVS checkin of IVE.
c
cRevision 2.2  1993/03/26  09:17:59  davidb
ccom_vect added
c
cRevision 2.1  1992/09/08  19:57:25  millerp
cAdded stuff for new slicers.
c
cRevision 2.0  1992/07/01  21:16:04  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c
      real vmax, incx, incy
      integer vecpos
      logical scale_vector
      COMMON /ive_vect/   vmax, incx, incy, vecpos, scale_vector
c
c     Common block carrying the pointers to the three vector components.
c
      real VECT_SPVAL
      parameter (VECT_SPVAL = -10.0**27)
      character *(80) att_var, vect_component
      dimension vect_component(3)
      common / vector_pointers / att_var, vect_component
c
c     common block carrying the flag, wheter user transform ph2ll 
c     should be used to find the vector-head or not.
c     mapvct is one out of valvct and usercoord is 1, if ph2ll is used.
c
      character *8    valvct,mapvct
      integer         usercoord,MAXVCT
      parameter (MAXVCT = 2)
      dimension valvct(MAXVCT)
      common / com_vect / mapvct, usercoord
c
c valvct added by David Bresch:
      data valvct(1)  / 'PH' /,
     &     valvct(2)  / 'LL' /
c
c	lock max. vectors length 
c
