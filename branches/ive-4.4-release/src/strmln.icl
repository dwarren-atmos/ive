c-----------------------------------------------------------------------
c	$Id: strmln.icl,v 1.2 1993/11/15 20:15:39 warren Exp $
c	$Log: strmln.icl,v $
c	Revision 1.2  1993/11/15 20:15:39  warren
c	rearranged common blocks for alignment.
c	
cRevision 1.1.1.1  1993/04/07  19:40:38  harry
cInitial CVS checkin of IVE.
c
cRevision 2.0  1992/07/01  21:15:57  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c----------------------------------------------------------------------
      real    ext, side, xlt, ybt
      COMMON /STR02/  EXT , SIDE , XLT , YBT
c
c
      integer     inita, initb, iterp, iterc, igflg, imsg, icyc
      real        arowl, uvmsg, displ, dispc, cstop
      COMMON /STR03/  INITA , INITB ,  ITERP , ITERC , IGFLG,
     1             ICYC , IMSG , AROWL ,UVMSG ,  DISPL , DISPC , CSTOP
c
c
      logical drstrmln
      common / streamline / drstrmln
c 
