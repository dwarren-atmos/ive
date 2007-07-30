c-----------------------------------------------------------------------
c     $Id: flags.icl,v 1.2 1994/01/29 00:11:32 warren Exp $
c     Purpose:
c        This file contains all common blocks which carry global
c        variables related to various flags used by the package.
c
c     History:
c	$Log: flags.icl,v $
c	Revision 1.2  1994/01/29 00:11:32  warren
c	first cut at movie labels. Line plots still need work
c	
cRevision 1.1.1.1  1993/04/07  19:39:55  harry
cInitial CVS checkin of IVE.
c
cRevision 2.0  1992/07/01  21:15:43  millerp
cReleased version rereving to 2.0
c
cRevision 1.1  1992/04/21  00:11:26  millerp
cInitial revision
c
c-----------------------------------------------------------------------
c
c     Flags.
c
      logical mapflg, savflg, movielabs
      common / flags / mapflg, savflg, movielabs
