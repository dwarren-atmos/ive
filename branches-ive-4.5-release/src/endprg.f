      subroutine endprg
c-----------------------------------------------------------------------
c     $Id: endprg.f,v 1.1.1.1 1993/04/07 19:39:54 harry Exp $
c     Purpose:
c        This subroutine is called to end program execution.
c     Arguments:
c        None
c     History:
c      $Log: endprg.f,v $
c      Revision 1.1.1.1  1993/04/07 19:39:54  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:13:03  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c
c
c     First close GKS.
c
      call clsgks
c
c
c     Terminate program execution.
c
      stop
c
c
      end
