      subroutine setplt (error)
c-----------------------------------------------------------------------
c     $Id: setplt.f,v 1.1.1.1 1993/04/07 19:40:31 harry Exp $
c     Purpose:
c        This subroutine sets the flag that determines whether a plot
c        will be drawn after the current command line is processed.
c     Arguments:
c        error  logi  output  error flag.
c     History:
c      $Log: setplt.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:31  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.1  1992/09/04  21:35:26  millerp
c Fixed up argument list; use call to setlvar.
c
c Revision 2.0  1992/07/01  21:14:46  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      logical    error
c
c
c
c
      call setlvar ('pltflg', .true., error)
c
c
      return
      end

