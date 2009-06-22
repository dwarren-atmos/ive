      subroutine catnull (string, lenstring, error)
c-----------------------------------------------------------------------
c     $Id: catnull.f,v 1.1.1.1 1993/04/07 19:39:48 harry Exp $
c     Purpose:
c        This routine concatenates a null to the input string.
c     Arguments:
c        string     char  input   the input string.
c        lenstring  int   input   the length of string in the calling
c                                 routine.
c        error      logi  output  error flag.
c     History:
c     $Log: catnull.f,v $
c     Revision 1.1.1.1  1993/04/07 19:39:48  harry
c     Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:28:05  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/05/29  22:38:40  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'message.icl'
c     
c     
c     Argument declarations.
c
      integer        lenstring
      character *(*) string
      logical        error
c
c
c     Local variable declarations.
c
      integer        beg, end
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
c     
      beg = strbeg(string)
      end = strend(string)
c     
c     
      if ( (end - beg + 1) .lt. lenstring ) then
c     
c        Concatenate a null to the string.
c     
         string = string(beg:end)//char(0)
      else
c     
c        The string is not long enough to accept another character,
c        so log an error.
c     
         write (message, 10) string(beg:end), char(10)
         call write_message 
         error = .true.
      endif
c     
c     
c     Format statements.
c
 10   format (1x,'CATNULL: Attempt to concatenate a null to the ',
     &           'string ',a,' failed.',a,
     &           'The string is too short.')
c
c
      return
      end
