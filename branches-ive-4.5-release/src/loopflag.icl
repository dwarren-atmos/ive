c-----------------------------------------------------------------------
c     $Id: loopflag.icl,v 1.1.1.1 1993/04/07 19:40:07 harry Exp $
c     Purpose:
c        This file contains a common block that indicates what kind
c	 of loop is currently in progress.  The corresponding "setloc"
c	 subroutine will ignore calls to it when the flag is true.
c
c     History:
c       $Log: loopflag.icl,v $
c       Revision 1.1.1.1  1993/04/07 19:40:07  harry
c       Initial CVS checkin of IVE.
c
c Revision 2.2  1992/10/16  15:14:09  warren
c fixed for DECstation
c
c Revision 2.1  1992/09/29  19:41:45  harry
c A loop flag for every dimension (a chicken in every pot, ...)!
c
c Revision 2.0  1992/07/01  21:16:19  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/05/21  17:45:02  harry
c Initial revision
c
c
c-----------------------------------------------------------------------
c
	logical loop_flag(4)

	common / loopflag / loop_flag

