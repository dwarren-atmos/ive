      subroutine clrscr
c-----------------------------------------------------------------------
c     $Id: clrscr.F,v 1.2 1993/06/29 23:32:31 warren Exp $
c     Purpose:
c        This subroutine clears the workstation screen.
c     Arguments:
c        none
c     History:
c      $Log: clrscr.F,v $
c      Revision 1.2  1993/06/29 23:32:31  warren
c      Removed superfluous include and replaces with parameters.
c
c Revision 1.1.1.1  1993/04/07  19:39:59  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:15  millerp
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
      INTEGER    GPOSTP, GSGOP, GALWAY
      PARAMETER (GPOSTP=0, GSGOP=4, GALWAY=1)
c
c
c     Local variable declarations.
c
      integer istate, WS_ID
      parameter ( WS_ID = 1 )
c
c
c
c   
c     Note that the screen workstation is assumed to be number 1.
c
c     Close any open segments.
c
      call gqops (istate)
      if ( istate .eq. GSGOP ) call gclsg()
c
c     Clear workstation.
c
      call guwk (WS_ID, GPOSTP)
      call gclrwk (WS_ID, GALWAY)

c
c
      return
      end        
