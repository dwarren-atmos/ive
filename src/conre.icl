c
c-----------------------------------------------------------------------
c     $Id: conre.icl,v 1.3 2002/06/11 17:26:15 harryive Exp $
c     Purpose:
c        This file contains all common block which carry global
c        variables related to the contouring routines.
c
c     History:
c	$Log: conre.icl,v $
c	Revision 1.3  2002/06/11 17:26:15  harryive
c	Change aspect ratio to affect the plot, not just printing.
c	
c	Add first part of contour value specification.
c	
cRevision 1.2  1993/11/16  01:14:10  warren
crest of the rearranging for alignment.
c
cRevision 1.1.1.1  1993/04/07  19:39:50  harry
cInitial CVS checkin of IVE.
c
cRevision 2.0  1992/07/01  21:15:39  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c     Common blocks used by the contouring routines.
c
      integer     nulbll, tnum_vals
      logical     cllab, nozero, single, reset_spval
      real        spval, cint, tcint, topo_scale, tcont_values(40)
c
      common / contcom / nulbll, cllab, nozero, single, reset_spval,
     &		         spval, cint, tcint, topo_scale, tcont_values,
     &			 tnum_vals
c