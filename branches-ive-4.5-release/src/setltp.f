      subroutine setltp (error)
c-----------------------------------------------------------------------
c     $Id: setltp.f,v 1.8 2000/09/21 19:49:49 harry Exp $
c     Purpose:
c        This routine sets the contour line type. If only one value is
c        given, then either the high and low contour line types are set
c        to that value. If two values are given, then the low contours
c        are set to the first value, and the high contours are set to 
c        the second value.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: setltp.f,v $
c        Revision 1.8  2000/09/21 19:49:49  harry
c        Fix errors in line color and pattern handling.
c
c        Revision 1.7  2000/05/09 19:54:02  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c Revision 1.6  1994/04/14  01:14:58  warren
c Fixed problem where doing overlays in line plots changes the current line
c type for contours.
c
c Revision 1.5  1994/01/20  20:44:59  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.4  1993/06/01  19:57:18  yuri
c Second try.
c
c Revision 1.2  1993/05/24  00:14:08  yuri
c News dased pattern
c
c Revision 1.1.1.1  1993/04/07  19:40:26  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:26  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'color.icl'
      include 'ovl_props.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      integer             ERRFLG, i, ibeg, iend, numblank
      real                val
      parameter ( ERRFLG = -999 )
      dimension           val(2)
c
c
c     External function declarations.
c
      integer             nblank, chkltp, strbeg, strend
c
c
c
c
      error = .false.
      numblank = 0
c
c
      do 10 i = 1, 2
         curval = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg) + 1)
c
         if ( nblank(curval) .eq. 0 ) then
            val(i) = ERRFLG
            numblank = numblank + 1
         else
            val(i) = chkltp (comand(1), curval, ibeg, iend, ERRFLG)
         endif
 10   continue
c
c
      if ( numblank .eq. 2 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
         return
      endif
c
      if ( (val(1) .eq. ERRFLG) .and. (val(2) .eq. ERRFLG) ) then
c
c        None of the values for this command were correct, so set the
c        error flag and return.
c
         error = .true.
         return
      endif
c
      if ( val(1) .ne. ERRFLG ) then
c
c        Set the high contour line pattern.
c
         hipattern = val(1)
         just_set_hipat = .true.
      endif
c
      if ( val(2) .ne. ERRFLG ) then
c
c        Set the low contour line pattern.
c
         lopattern = val(2)
         just_set_lopat = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
c
c
      return
      end
c
c
      function chkltp (comand, chrval, ibeg, iend, errflg)
c-----------------------------------------------------------------------
c     Purpose:
c        This function is used by setltp to check for correct line 
c        patterns. The function returns a value for the line type.
c     Arguments:
c        comand  char  input  the command entered by the user.
c        chrval  char  input  the value assiged to the command.
c        ibeg    int   input  the location in string where the first
c                             nonblank character occurs.
c        iend    int   input  the location in string where the last
c                             nonblank character occurs.
c        errflg  int   input  an error flag.
c     History:
c-----------------------------------------------------------------------
c
      include 'message.icl'
c
c
c     Function declaration.
c
      integer        chkltp
c
c     Argument declarations.
c
      character *(*) comand, chrval
      integer        ibeg, iend, errflg
c
c
c     External function declarations.
c
      integer        strbeg, strend

c
c     Local valiable declarations.
c
      integer     linpat
      dimension linpat(7)
      data 
c               solid line pattern.
     &     linpat(1) / 65535 / ,
c               dash line pattern.
     &     linpat(2) / 52428 / ,
c               dot line pattern.
     &     linpat(3) /  34952 / ,
c               dashdot line pattern.
     &     linpat(4) / 64250 / ,
c               dashdotdot line pattern.
     &     linpat(5) / 60138 / ,
c               longdash shortdash line pattern.
     &     linpat(6) / 63222 / ,
c               longdash pattern.
     &     linpat(7) / 64764 / 
c
c
c
c
c     chrval need only be unique to specify a line pattern.

c
      if ( chrval(1:1) .eq. 'S' ) then
         chkltp = linpat(1)
c      elseif (chrval(1:8) .eq. 'DASHDOTD' ) then
c         chkltp = linpat(5)
      elseif (chrval(1:5) .eq. 'DASHD' ) then
         chkltp = linpat(4)
      elseif (chrval(1:4) .eq. 'DASH' ) then
         chkltp = linpat(2)
      elseif (chrval(1:2) .eq. 'DO' ) then
         chkltp = linpat(3)
      elseif (chrval(1:9) .eq. 'LONGDASHS' ) then
         chkltp = linpat(6)
      elseif (chrval(1:8) .eq. 'LONGDASH' ) then
         chkltp = linpat(7)
      elseif (chrval(1:2) .eq. 'DE' ) then
         chkltp = 0
      else
c
c        An incorrect value was given. Log an error and return the value
c        of errflg.
c
         write (message, 110) chrval(ibeg:iend), 
     &                        comand(strbeg(comand):strend(comand))
         call write_message
         chkltp = errflg
      endif
c
c
c     Format statements.
c
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end


      
