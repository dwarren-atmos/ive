c-----------------------------------------------------------------------
c     $Id: supma.icl,v 1.4 2002/08/09 19:58:01 warren Exp $
c     Purpose:
c        This file contains all common blocks which carry global
c        variables related used by supmap.
c
c     History:
c	$Log: supma.icl,v $
c	Revision 1.4  2002/08/09 19:58:01  warren
c	Fixed for Linux and tested with DEC sun etc.
c	most items inside #ifdef's, or were general cleanup/fixes
c	
c	Revision 1.3  1997/03/14 22:26:35  warren
c	Added new command map_window to allow for mouse windowing on non
c	equadistant or default maps.
c	
cRevision 1.2  1993/12/15  18:15:54  harry
cAdd support for DEFAULT_MAP command, default_map transform.  Modify map
cclipping to work properly with the map background widget.
c
cRevision 1.1.1.1  1993/04/07  19:40:39  harry
cInitial CVS checkin of IVE.
c
cRevision 2.4  1993/03/12  15:58:36  davidb
cDF,LI,CL added for command maplimits
c
cRevision 2.3  1993/01/13  23:50:05  millerp
c*** empty log message ***
c
cRevision 2.2  1993/01/13  23:48:15  millerp
cFix compiler error.
c
cRevision 2.1  1993/01/13  23:08:47  millerp
cMove char variables to separate common block.
c
cRevision 2.0  1992/07/01  21:15:46  millerp
cReleased version rereving to 2.0
c
cRevision 1.2  1992/05/05  15:32:17  millerp
cAdded projection choices.
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c     Variables for calls to supmap.
c
      character *8   maplim, outlin, proj, vallim, valprj
      integer        MAXLIM, MAXPRJ 
      real           plm1, plm2, plm3, plm4, plat, plon, rota, grdspa,
     &               mapwin_u1, mapwin_u2, mapwin_v1, mapwin_v2
      logical        defmap, exact_fit
      parameter ( MAXLIM = 7 ,
     &            MAXPRJ = 16 )
      dimension plm1(2), plm2(2), plm3(2), plm4(2)
      dimension vallim(MAXLIM), valprj(MAXPRJ)
c
      data valprj( 1) / 'LC'   /,
     &     valprj( 2) / 'ST'   /,
     &     valprj( 3) / 'OR'   /,
     &     valprj( 4) / 'LE'   /,
     &     valprj( 5) / 'GN'   /,
     &     valprj( 6) / 'AE'   /,
     &     valprj( 7) / 'SV'   /,
     &     valprj( 8) / 'CE'   /,
     &     valprj( 9) / 'ME'   /,
     &     valprj(10) / 'MO'   /,
     &     valprj(11) / 'NHST' /,
     &     valprj(12) / 'NHOR' /,
     &     valprj(13) / 'NHSV' /,
     &     valprj(14) / 'SHST' /,
     &     valprj(15) / 'SHOR' /,
     &     valprj(16) / 'SHSV' /
c
      data vallim(1)  / 'MA' /,
     &     vallim(2)  / 'CO' /
     &     vallim(3)  / 'PO' /
     &     vallim(4)  / 'AN' /
     &     vallim(5)  / 'DF' /
     &     vallim(6)  / 'CL' /
     &     vallim(7)  / 'LI' /
c last 3 lines added by David Bresch
c
      common / supmp / plm1, plm2, plm3, plm4, plat, plon, rota,
     &                 grdspa, defmap, exact_fit, mapwin_u1,
     &                 mapwin_u2, mapwin_v1, mapwin_v2
      common / supmp_char / maplim, outlin, proj
