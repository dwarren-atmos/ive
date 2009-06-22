      subroutine chkvect (field, cpmax4, error)
c-----------------------------------------------------------------------
c     $Id: chkvect.f,v 1.1.1.1 1993/04/07 19:39:48 harry Exp $
c     Purpose:
c        This routine checks to make sure that the current field can
c        be plotted as a vector before the vector processing routines
c        are called.
c     Arguments:
c        field   char  input   the current field.
c        cpmax4  int   input   the fourth dimension of the current 
c                              field. This value must be equal to 3 for
c                              vectors to be drawn.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = true    error detected.
c                              error = false   no error detected.
c     History:
c        $Log: chkvect.f,v $
c        Revision 1.1.1.1  1993/04/07 19:39:48  harry
c        Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:12:37  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'message.icl'
c
c
c     Argument declarations.
c
      character *(*) field
      integer        cpmax4
      logical        error
c
c
c     External function declarations.
c
      integer        strbeg, strend
c
c
c
c
      error = .false.
c
      if ( cpmax4 .ne. 3 ) then
         write (message, 100) field(strbeg(field):strend(field))
         call write_message
         error = .true.
      endif
c
c
c     Format statements. 
c
 100  format (1x, a,' cannot be plotted as a vector field.')
c
c
      return
      end
