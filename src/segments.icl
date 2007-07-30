c-----------------------------------------------------------------------
c     $Id: segments.icl,v 1.2 2000/04/07 22:23:13 harryive Exp $
c     Purpose:
c        This file contains all common blocks which carry global
c        variables related to the plot segments.
c
c     History:
c       $Log: segments.icl,v $
c       Revision 1.2  2000/04/07 22:23:13  harryive
c       Increase max overlay number to 32.
c
c       Revision 1.1.1.1  1993/04/07 19:40:17  harry
c       Initial CVS checkin of IVE.
c
cRevision 2.3  1993/01/13  18:57:34  millerp
cMove character variables to separate common block.
c
cRevision 2.2  1993/01/06  17:15:38  harry
cAdd MAXOVERLAY, curplot, curlist, segs.  Rearrange common block.
c
cRevision 2.1  1992/09/23  23:14:40  harry
cAdd curoverlay.
c
cRevision 2.0  1992/07/01  21:16:06  millerp
cReleased version rereving to 2.0
c
cRevision 1.3  1992/07/01  18:00:58  millerp
c*** empty log message ***
c
cRevision 1.2  1992/06/30  21:00:24  millerp
cChanged MXLEN to 512 to account for long command lists.
c
cRevision 1.1  1992/04/21  01:15:14  warren
cInitial revision
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
      integer          MAXLIST, MXLEN, MAXOVERLAY
      parameter ( MAXLIST = 15,
     &            MAXOVERLAY = 32,
     &            MXLEN   = 768 )
      character*(MXLEN)  cmdstr, list
      integer curseg, curoverlay(MAXLIST)
      integer segs(MAXOVERLAY, MAXLIST), curplot, curlist, 
     &        maxplot 
      dimension list(MAXLIST)
      common / segments / curseg, curoverlay, curlist, curplot, segs,
     &	                   maxplot
      common / segments_char / cmdstr, list
