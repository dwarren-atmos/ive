      function cmpstr (strng1, strng2)  
c-----------------------------------------------------------------------
c     $Id: cmpstr.f,v 1.1.1.1 1993/04/07 19:40:02 harry Exp $
c     Purpose:
c        This function compares two strings. If the strings are 
c        identical the function value is set to true, otherwise it is
c        false. Leading and trailing blanks are ignored in the 
c        comparison, but imbedded blanks are taken into account.
c     Arguments:
c        strng1  char  input  one of the strings to be compared.
c        strng2  char  input  the other string.
c     History:
c      $Log: cmpstr.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:02  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:16:55  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c     
c     Function declaration.
c
      logical cmpstr
c
c
c     Argument declarations.
c
      character *(*) strng1, strng2
c
c
c     External function declarations.
c
      integer strbeg, strend
c
c
c
c
      if ( strng1(strbeg(strng1):strend(strng1)).eq. 
     &     strng2(strbeg(strng2):strend(strng2)) ) then
         cmpstr = .true.
      else
         cmpstr = .false.
      endif
c
c
      return
      end
