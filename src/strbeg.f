      function strbeg (string)
c-----------------------------------------------------------------------
c     $Id: strbeg.f,v 1.2 1994/02/23 00:15:30 warren Exp $
c     Purpose:
c        This function returns the position of the first nonblank 
c           character in the input string.  Returns 1 if the entire 
c           string is blank.
c     Arguments:
c        string  char  input  string to be examined.
c     History:
c      $LogK$: strbeg.f,v $
c-----------------------------------------------------------------------
c
c     Function declaration
c
      integer       strbeg
c
c
c     Argument declarations 
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
      do 10 i = 1, len(string)
         if (string(i:i) .ne. ' ' .and. string(i:i) .ne. char(0)) then
            strbeg = i
            return
         endif
 10   continue
c
c
c     Case when entire string is blank.
c
      strbeg = 1
c
c
      return
      end
c
c
      function strend (string)
c-----------------------------------------------------------------------
c     $Id: strbeg.f,v 1.2 1994/02/23 00:15:30 warren Exp $
c     Purpose:
c        This function returns the position of the last nonblank 
c           character in the input string.  Returns 1 if the entire 
c           string is blank.
c     Arguments:
c        string  char  input  string to be examined.
c     History:
c      $Log: strbeg.f,v $
c      Revision 1.2  1994/02/23 00:15:30  warren
c      Final 3.1 label changes.
c
c Revision 1.1.1.1  1993/04/07  19:40:48  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:11  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c     Function declaration
c
      integer strend
c
c
c     Argument declarations     
c
      character*(*) string
c
c
c     Local variable declarations.
c
      integer i
c
c
c
c
      do 10 i = len(string), 1, -1
         if (string(i:i) .ne. ' ' .and. string(i:i) .ne. char(0)) then
            strend = i
            return
         endif
 10   continue
c
c
c     Case when entire string is blank.
c
      strend = 1
c
c
      return
      end

      subroutine clean_zero (string)
c-----------------------------------------------------------------------
c     Purpose:
c        This function cleans 0s from the input string.
c     Arguments:
c        string  char  input  string to be examined.
c-----------------------------------------------------------------------
c

c
c     Argument declarations 
c
      character*(*) string
c
c
c     Local variable declarations.
c
      integer       i, point, strend
c
c
c
c
      point = index(string, 'E')
      if(point .gt. 0)return
      point = index (string, '.')
      if(point .eq. 0)return
c
      i = strend(string)
      do while (i .ge. point .and. 
     &          (string(i:i) .eq. '0' .or. string(i:i) .eq. '.'))
         string(i:i) = ' '
         i = i - 1
      enddo
c
      return
      end
c
c
