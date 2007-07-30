      function itoa (num)
c-----------------------------------------------------------------------
c     $Id: ftoa.f,v 1.6 1998/09/30 22:28:32 harry Exp $
c     Purpose:
c        This function takes an floating point number as input and 
c           converts it to a character string.
c     Arguments:
c        num  real  input  real number to be converted.
c     History:
c      $Log: ftoa.f,v $
c      Revision 1.6  1998/09/30 22:28:32  harry
c      Rewrite makpointplt and add ability for 2 average points, fix errors
c      in the 1d slicers, remove obsolete yuri routines.
c
c Revision 1.5  1994/05/06  17:27:07  yuri
c Obsolote function deleted.
c
c Revision 1.4  1994/05/06  17:10:05  yuri
c Main vector lock commit + small correction in X/ive_ui_update.c file
c
c Revision 1.3  1994/03/25  23:12:31  harry
c Fixes to plot labelling again.
c
c Revision 1.2  1994/03/25  22:24:35  warren
c first fix to Dales label problem.
c
c Revision 1.1.1.1  1993/04/07  19:40:09  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:17:04  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:23:37  millerp
c Initial revision
c----------------------------------------------------------------------
c
c     Argument declarations
c
      character *(*)  itoa
      integer         num
c
c
c     Local variable declarations.
c
      character *(80) dummy
      integer         i, ibeg, iend
      logical         recalc
c
c
c     External function declarations
c
      integer         strbeg, strend
c     
c
c
c     
      dummy = ' '
      write (dummy, *) num
c
c
c     Strip off leading and trailing zeroes.
c
      ibeg = strbeg(dummy)
      iend = strend(dummy)
      recalc = .false.
c
      do 15 i = ibeg, iend 
         if ( dummy(i:i) .eq. '0' ) then
            dummy(i:i) = ' '
            recalc = .true.
         else
            goto 20
         endif
 15   continue
c
 20   if ( recalc ) then
         ibeg = strbeg (dummy)
         iend = strend (dummy)
      endif
c
c
c     Make sure that the length of the character string does not exceed
c     the length of itoa.
c
      if ( (ibeg - iend + 1) .le. len (itoa) ) then
         itoa = dummy(ibeg:iend)
      endif
c
c
      return
      end
c
      subroutine ftoa_c(num, result, size)
      character*(*) result
      character*(80) ftoa
      integer size, strend
      real num

      result = ftoa(num)
      size = strend(result)
      return
      end

      function ftoa (num)
c
c-----------------------------------------------------------------------
c
c     Function declaration.
c
      character *(*)  ftoa
c
c
c     Argument declarations.
c
      real            num
c
c
c     Local variable declarations.
c
      character *(80) dummy
      integer         i, ibeg, iend
      logical         recalc
      real            EPSLON, rnum
      parameter ( EPSLON = 1.0e-15 )
c
c
c     External function declarations.
c
      integer         strbeg, strend
c     
c
c
c
      rnum = abs(num)
      dummy = ' '
c
      if ( rnum .le. EPSLON ) then
         dummy = '0.0'
      else
         if (     (rnum .ge. 1.0e-1) .and. (rnum .le. 1.0e1) ) then
            write (dummy, 130) num 
         elseif ( (rnum .ge. 1.0e-2) .and. (rnum .le. 1.0e2) ) then 
            write (dummy, 140) num 
         elseif ( (rnum .ge. 1.0e-3) .and. (rnum .le. 1.0e3) ) then 
            write (dummy, 150) num 
         elseif ( (rnum .ge. 1.0e-4) .and. (rnum .le. 1.0e4) ) then 
            write (dummy, 160) num 
         elseif ( (rnum .ge. 1.0e-5) .and. (rnum .le. 1.0e5) ) then 
            write (dummy, 170) num 
         elseif ( (rnum .ge. 1.0e-6) .and. (rnum .le. 1.0e6) ) then 
            write (dummy, 180) num 
         else
            write (dummy, 190) num 
         endif
      endif
c
c
c     Strip off leading and trailing zeroes.
c
      ibeg = strbeg(dummy)
      iend = strend(dummy)
      recalc = .false.
c
      do 15 i = ibeg, iend - 1
         if ( (dummy(i  :i  ) .eq. '0') .and. 
     &        (dummy(i+1:i+1) .ne. '.') ) then
            dummy(i:i) = ' '
            recalc = .true.
         else
            goto 20
         endif
 15   continue
c
 20   if ( recalc ) then
         ibeg = strbeg (dummy)
         iend = strend (dummy)
         recalc = .false.
      endif
c
      do 25 i = iend, ibeg + 1, -1
         if ( (dummy(i  :i  ) .eq. '0' ) .and. 
     &        (dummy(i-1:i-1) .ne. '.') ) then 
            dummy(i:i) = ' '
            recalc = .true.
         else
            goto 30
         endif
 25   continue
c
 30   if ( recalc ) then
         ibeg = strbeg (dummy)
         iend = strend (dummy)
      endif
c
c
c     Make sure that the length of the character string does not exceed
c     the length of ftoa.
c
      if ( (ibeg - iend + 1) .le. len (ftoa) ) then
         ftoa = dummy(ibeg:iend)
      endif
c
c
c     Format statements.
c
 110  format (g 9.1)
 120  format (g10.2)
 130  format (g11.3)
 140  format (g12.4)
 150  format (g13.5)
 160  format (g14.6)
 170  format (g15.7)
 180  format (g16.8)
 190  format (g17.9)
c
c
      return
      end
