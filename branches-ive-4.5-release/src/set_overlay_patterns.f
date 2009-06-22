      subroutine set_overlay_patterns (error)
c-----------------------------------------------------------------------
c     $Id: set_overlay_patterns.f,v 1.7 2000/09/21 19:49:46 harry Exp $
c     Purpose:
c        This routine sets the pattern in overlay plot.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: set_overlay_patterns.f,v $
c        Revision 1.7  2000/09/21 19:49:46  harry
c        Fix errors in line color and pattern handling.
c
c        Revision 1.6  2000/05/09 19:53:57  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c Revision 1.5  1994/04/14  01:14:53  warren
c Fixed problem where doing overlays in line plots changes the current line
c type for contours.
c
c Revision 1.4  1994/03/30  22:47:30  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.3  1994/01/24  19:33:59  yuri
c Correction in opevrlay patterns routines.
c
c Revision 1.2  1994/01/20  20:44:49  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.1  1994/01/20  17:52:45  yuri
c Overlay_pattern command added and several new patterns added too.
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'color.icl'
      include 'message.icl'
      include 'ovl_props.icl'
C
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      integer              ERRFLG, i, ibeg, iend
      parameter ( ERRFLG = -999 )
c
c
c     External function declarations.
c
      integer nblank, chkltp, strbeg, strend, is_there_slash
c
c
c
c
      error = .true.
      total_contour_patterns = 0
c
c
c
      do 10 i = 1,  MAXCOM 
         curval = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg) + 1)
C        if string is blank, skip it 
         if( nblank( curval ) .eq. 0 ) goto 10 
c
c        there is at least one value 
c
         error = .false.
c
         total_contour_patterns = total_contour_patterns + 1 
c         
         if ( (i .eq. 1) .and. (curval(ibeg:ibeg) .eq. 'D') ) then
c
c           Return patterns to their defaults and return.
c     
            call  default_overlay_patterns
            return
         endif
c
         if ( is_there_slash( curval ) .ne. 0 ) then
C
C           we have both patterns here
C
c           high is first
c
            iend = is_there_slash( curval )
            highpat( total_contour_patterns ) = 
     &           chkltp (comand(1), curval, ibeg, iend, ERRFLG)
            if ( highpat( total_contour_patterns ) .eq.  ERRFLG )
     &           error = .true.
c
c           low is second
c
            ibeg = is_there_slash( curval )
            iend = strend (curval)
            lowpat ( total_contour_patterns ) = 
     &           chkltp (comand(1), curval, ibeg, iend, ERRFLG)
            if ( lowpat( total_contour_patterns ) .eq.  ERRFLG )
     &           error = .true.
         else
c
c            we have one pattern here
c
            highpat( total_contour_patterns ) = 
     &           chkltp (comand(1), curval, ibeg, iend, ERRFLG)
            lowpat ( total_contour_patterns ) =
     &           highpat( total_contour_patterns )
c
         endif
c
c
c
 10   continue
c
c
      if ( error ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                 strend(comand(1)))
         call write_message
      endif

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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine default_overlay_patterns
C
      include 'ovl_props.icl'
c
c

      highpat(1) = 65535
      lowpat (1) = 52428

      highpat(2) = 65535
      lowpat (2) = 65535

      highpat(3) = 65535
      lowpat (3) = 65535

      highpat(4) = 65535
      lowpat (4) = 65535

      highpat(5) = 65535
      lowpat (5) = 65535

      highpat(6) = 65535
      lowpat (6) = 65535

      highpat(7) = 65535
      lowpat (7) = 65535

c         
c      total_contour_patterns of patternss = 7
c
      total_contour_patterns = 7
c
c     Done
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine set_overlay_linepatterns (error)
c
      include 'incommands.icl'
      include 'color.icl'
      include 'message.icl'
      include 'ovl_props.icl'
C
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      integer              ERRFLG, i, ibeg, iend
      parameter ( ERRFLG = -999 )
c
c
c     External function declarations.
c
      integer nblank, chkltp, strbeg, strend, is_there_slash
c
c
c
c
      error = .true.
      total_patterns = 0
c
c
c
      do 10 i = 1,  MAXCOM 
         curval = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg) + 1)
C        if string is blank, skip it 
         if( nblank( curval ) .eq. 0 ) goto 10 
c
c        there is at least one value 
c
         error = .false.
c
         total_line_colors = total_line_colors + 1 
c         
         if ( (i .eq. 1) .and. (curval(ibeg:ibeg) .eq. 'D') ) then
c
c           Return patterns to their defaults and return.
c
            call  default_overlay_linepatterns
            return
         endif
c
         ovl_patterns( total_patterns ) = 
     &        chkltp (comand(1), curval, ibeg, iend, ERRFLG)
c
c
c
 10   continue
c
c
      if ( error ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                 strend(comand(1)))
         call write_message
      endif

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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine default_overlay_linepatterns
C
      include 'ovl_props.icl'
c
c

      ovl_patterns(1) = 65535	! solid

      ovl_patterns(2) = 52428	! dash

      ovl_patterns(3) = 34952	! dot

      ovl_patterns(4) = 64250	! dashdot

      ovl_patterns(5) = 60138	! dashdotdot

      ovl_patterns(6) = 63222	! short dash

      ovl_patterns(7) = 64764	! long dash

c         
c      total_patterns of patterns = 7
c
      total_patterns = 7
c
c     Done
c
      return
      end
