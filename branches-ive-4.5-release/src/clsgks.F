      subroutine clsgks
c-----------------------------------------------------------------------
c     $Id: clsgks.F,v 1.2 1993/06/29 23:36:53 warren Exp $
c     Purpose:
c        This subroutine closes GKS.
c     Arguments:
c        none
c     History:
c      $Log: clsgks.F,v $
c      Revision 1.2  1993/06/29 23:36:53  warren
c      More includes to defines.
c
c Revision 1.1.1.1  1993/04/07  19:40:00  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:17  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c     GKS DEFS
c
c
      INTEGER    GSGOP, GALWAY
      PARAMETER (GSGOP=4, GALWAY=1)
c
c     Local variable declarations.
c
      integer istate
c
c
c
c
c     Close any open segment. 
c
      call gqops (istate)
      if (istate .eq. GSGOP) call gclsg ()
c
c
c     Clear, deactivate, and close all workstations that were opened in
c     subroutine prog_open.
c
      call gclrwk (1, GALWAY)
      call gclrwk (2, GALWAY)
      call gdawk (1)
      call gdawk (2)
      call gclwk (1)                  
      call gclwk (2)
      call gclks
c
c
      return
      end
