      subroutine cvatoi (string, value, error)
c-----------------------------------------------------------------------
c     $Id: cvatoi.f,v 1.1.1.1 1993/04/07 19:40:21 harry Exp $
c     Purpose:
c        This subroutine is an interface to the c function atoi, which
c          converts an alphanumeric string to an integer number.
c          If the input string is composed entirely of numerals, or an 
c          optional minus sign or decimal point, the  string will be 
c          converted, otherwise an error flag is set and control is 
c          passed back to the calling routine.
c          NOTE: 
c          If the string contains a decimal point, the string will still
c          be converted to integer, but will be truncated at the decimal
c          point.
c     Arguments:
c        string  char  input   string to be examined and converted to an
c                              integer, if and only if string is
c                              composed entirely of valid characters.
c        value   int   output  the result of converting string to 
c                              integer.
c        error   logi  output  indicates possible errors found in this
c                              routine.
c                              error = false  no errors detected
c                              error = true   error detected
c     History:
c      $Log: cvatoi.f,v $
c      Revision 1.1.1.1  1993/04/07 19:40:21  harry
c      Initial CVS checkin of IVE.
c
c Revision 2.1  1992/12/15  23:52:28  millerp
c Delete unused variables.
c
c Revision 2.0  1992/07/01  21:17:00  millerp
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
      integer       value
      logical       error
c
c
c     Local variable declarations.
c
      integer       ibeg, iend
c
c
c     External function declarations.
c
      integer       catoi, nblank, strbeg, strend
      logical       isfloat
c
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
c     Now make sure string contains only numerals or a sign. The string
c     may contain a decimal point, but in this case the resulting number
c     will be truncated at the decimal point.
c     If the string contains other characters, set error flag and 
c     return.
c
      if ( .not. isfloat (string) ) then
         error = .true.
         return
      endif
c
c
c     String is composed entirely of valid characters, so convert string
c     to integer.
c
      value = catoi (string(ibeg:iend))
c
c
      return
      end
