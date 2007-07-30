      function nblank (string)
c-----------------------------------------------------------------------
c     $Id: nblank.f,v 1.1.1.1 1993/04/07 19:40:01 harry Exp $
c     Purpose:
c        This function returns the number of nonblank characters in the
c           input string. Returns 0 if the entire string is blank.
c     Arguments:
c        string  char  input  string to be examined.
c     History:
c      $Log: nblank.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:01  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:07  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c     Function declaration.
c
      integer       nblank
c
c
c     Argument declarations.
c
      character*(*) string
c
c
c     Local variable declarations.
c
      integer       i
c
c
c
c
c     Initialize function value.
c
      nblank = 0
c
c
c     Count the total number of nonblank characters in string.
c
      do 10 i = 1, len(string)
         if ( (string(i:i) .ne. ' ') .and. (string(i:i) .ne. char(0)) )
     &   nblank = nblank + 1
 10   continue
c
c
      return
      end

