c
c-----------------------------------------------------------------------
c     $Id: work.icl,v 1.6 2002/12/27 18:44:29 warren Exp $
c     Purpose:
c        This file contains all common blocks which carry work
c        arrays needed for conpack and areas.
c
c     History:
c       $Log: work.icl,v $
c       Revision 1.6  2002/12/27 18:44:29  warren
c       First set of changes from zurich. use mapdrw_dl to deal with ever changing
c       borders in Europe. Increase arrays in work.
c
c       Revision 1.5  1995/10/18 19:18:37  warren
c       Stipples from 3.3, First crack at mouse windowing and put window_points_
c       into a header file and out of the mouse stuff and slicers. Touched almost
c       everything it seems.
c
cRevision 1.4  1994/06/28  17:43:45  warren
cFirst cut at fancy tic marks.
c
cRevision 1.3  1994/01/12  19:32:23  yuri
cIncreased space for temp. arrays in ncar to satisfy Joan.
c
cRevision 1.2  1993/11/15  20:15:42  warren
crearranged common blocks for alignment.
c
cRevision 1.1.1.1  1993/04/07  19:40:44  harry
cInitial CVS checkin of IVE.
c
cRevision 2.1  1993/03/12  15:58:36  davidb
cCOM_SPL added
c
cRevision 2.0  1992/07/01  21:16:00  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c
      integer     MXIAMA, MXRWRK, MXIWRK
      parameter (MXIAMA = 8000000,
     &           MXRWRK = 800000, 
     &           MXIWRK = 800000)
c
c     Integer work arrays.
c
      integer            iama, iwrk1
      dimension iama(MXIAMA), iwrk1(MXIWRK)
c 
c     Real work array.
c
      real               rwrk1
      dimension rwrk1(MXRWRK)
      real               spl_t2d
      integer            spl_pic
c
c     Common declarations.
c
      common  / ciwrk /  iama, iwrk1
      common  / crwrk /  rwrk1
c addition by David Bresch
c declaration of common block, containing T2D and PIC
      COMMON / COM_SPL / SPL_PIC, SPL_T2D

