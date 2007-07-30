      function cntstr (string, substr)
c-----------------------------------------------------------------------
c     $Id: cntstr.f,v 1.1.1.1 1993/04/07 19:40:05 harry Exp $
c     Purpose:
c        This function counts the number of occurences of substr in
c        string.
c     Arguments:
c        string  char  input  string to be examined.
c        substr  char  input  substring to be searched for.
c     History:
c      $Log: cntstr.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:05  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:16:57  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c     Function declaration
c
      integer cntstr
c
c
c     Argument declarations 
c
      character*(*) string, substr
c
c
c     Local variable declarations.
c
      integer i, sealen
c
c
c     External function declarations.
c
      integer len
c
c
c
c
      sealen = len(substr) - 1
      cntstr = 0
c
      do 10 i = 1, len(string)-sealen
         if (string(i:i+sealen) .eq. substr)  cntstr = cntstr + 1
 10   continue
c
c
      return
      end

