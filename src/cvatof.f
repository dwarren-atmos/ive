      subroutine cvatof (string, value, error)
c-----------------------------------------------------------------------
c     $Id: cvatof.f,v 1.2 1993/10/26 22:16:38 warren Exp $
c     Purpose:
c        This subroutine is an interface to the c function atof, which
c          converts an alphanumeric string to a floating point number.
c          If the input string is composed entirely of numerals, or an 
c          optional minus sign or decimal point, the  string will be 
c          converted, otherwise an error flag is set and control is 
c          passed back to the calling routine.
c     Arguments:
c        string  char  input   string to be examined and converted to 
c                              floating point, if and only if string is 
c                              composed entirely of valid characters.
c        value   real  output  the result of converting string to 
c                              floating point.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false  no errors detected
c                              error = true   error detected
c     History:
c      $Log: cvatof.f,v $
c      Revision 1.2  1993/10/26 22:16:38  warren
c      permit e notation in text boxes and remove duplicate declaration of error.
c
c Revision 1.1.1.1  1993/04/07  19:40:16  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/12/15  23:52:28  millerp
c Delete unused variables.
c
c Revision 2.0  1992/07/01  21:16:58  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c     Argument declarations
c
      character*(*) string
      real          value
      logical       error
c
c
c     Local variable declarations.
c
      integer      ibeg, iend
c
c
c     External function declarations.
c
      integer      nblank, strbeg, strend
      logical      isfloat
      real         catof
c
c
c
c
c     Set error initially to false.
c
      error = .false.
c
c
c     Find the beginning and end of string.
c
      ibeg = strbeg(string)
      iend = strend(string)
c
c
c     If string is blank, return with an error flagged.
c
      if ( nblank(string(ibeg:iend)) .eq. 0 ) then
         error = .true.
         return
      endif
c
c
c     Now make sure string contains only numerals, a sign, or a decimal
c     point. If it contains other characters, set error flag and return.
c
c      if ( .not. isfloat (string) ) then
c         error = .true.
c         return
c      endif
c
c
c     String is composed entirely of valid characters, so convert 
c     to floating point. 
c
      value = catof (string(ibeg:iend), error)
c
c
      return
      end
c
c
      function isfloat (string)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine tests the input string to see if it is composed
c        entirely of digits or the characters '.', '+', '-'.  
c        If these conditions are met, the function value is set to true,
c        otherwise it is set to false.
c     Argument:
c        string  char  input  the string to be tested.
c-----------------------------------------------------------------------
c
c     Function declaration.
c
      logical        isfloat
c
c
c     Argument declaration.
c
      character *(*) string
c
c
c     Local variable declarations.
c
      integer        i, ibeg, iend, numdec
c
c
c     External function declarations.
c
      integer        strbeg, strend
c
c
c
c
c     Set some initial values.
c
      numdec = 0
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     Search the string for valid characters.
c
c     The characters '-' and '+' are valid only if they are the first
c     nonblank characters in the string.
c
      if ( (string(ibeg:ibeg) .eq. '-') .or. 
     &     (string(ibeg:ibeg) .eq. '+') ) then
         ibeg = ibeg + 1
      endif
c
c
      do 10 i = ibeg, iend
         if ( (string(i:i) .lt. '0') .or. (string(i:i) .gt. '9') ) then
            if ( (string (i:i) .ne. '.') ) then 
c     
c              If the current character is neither a digit nor a decimal
c              point the string cannot represent a number. Set the 
c              function value to false and return.
c
               isfloat = .false.
               return
            else
c
c              Keep track of the number of decimal points found.
c
               numdec = numdec + 1
               if ( numdec .gt. 1 ) then
c
c                 If the number of decimal points in the string > 1, the
c                 string cannot represent a number. Set the function 
c                 value to false and return.
c
                  isfloat = .false.
                  return
               endif
            endif
         endif
 10   continue
c
c
c     If we get this far, the string passed all tests so it does 
c     represent a number.
c
      isfloat = .true.
c
c
      return
      end
