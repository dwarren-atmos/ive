      subroutine set_overlay_colors (error)
c-----------------------------------------------------------------------
c     $Id: set_overlay_colors.f,v 1.11 2000/05/09 19:53:57 harry Exp $
c     Purpose:
c        This routine sets the overlay contour line color.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: set_overlay_colors.f,v $
c        Revision 1.11  2000/05/09 19:53:57  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c        Revision 1.10  1996/02/21 21:02:32  warren
c        Overlay colors are in the correct order and seem to set correctly.
c
c Revision 1.9  1995/01/09  18:52:40  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.8  1994/03/30  22:47:28  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.7  1994/01/20  20:44:47  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.6  1994/01/12  23:06:47  yuri
c Got rid of debugging prints.
c
c Revision 1.5  1993/07/03  01:53:22  yuri
c Tuning of overlay colors.
c
c Revision 1.4  1993/06/18  19:39:57  yuri
c 	Color table updated.
c
c Revision 1.3  1993/06/16  17:09:35  yuri
c By request of D. Durrand I set up new color table.
c
c Revision 1.2  1993/06/11  18:20:11  yuri
c New colors for ovrelay.
c
c Revision 1.1  1993/05/24  15:59:37  yuri
c Main routine to set up overlay colors table.
c
c Revision 1.1.1.1  1993/04/07  19:40:21  harry
c Initial CVS checkin of IVE.
c
c Revision 2.2  1992/08/25  23:29:32  warren
c fixed color allocation problem and added fallback to smaller color table
c
c Revision 2.1  1992/08/22  00:32:36  warren
c changed line color indexes to match new colors
c
c Revision 2.0  1992/07/01  21:14:10  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/06/26  23:09:47  millerp
c Deleted orange, replaced with brown.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
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
      integer nblank, selclr, strbeg, strend, is_there_slash
c
c
c
c
      error = .true.
      total_colors = 0
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
         total_colors = total_colors + 1 
c         
         if ( (i .eq. 1) .and. (curval(ibeg:ibeg) .eq. 'D') ) then
c
c           Return colors to their defaults and return.
c     
C     we define default line colors in this linecolors table
            call  default_overlay_colors
            return
         endif
c
         if ( is_there_slash( curval ) .ne. 0 ) then
C
C           we have both colors here
C
c           high is first
c
            iend = is_there_slash( curval )
            highs( total_colors ) = 
     &       selclr (comand(1), curval, ibeg, iend, ERRFLG)
            if ( highs( total_colors ) .eq.  ERRFLG ) error = .true.
c
c           low is second
c
            ibeg = is_there_slash( curval )
            iend = strend (curval)
            lows ( total_colors ) = 
     &       selclr (comand(1), curval, ibeg, iend, ERRFLG)
            if ( lows( total_colors ) .eq.  ERRFLG ) error = .true.
         else
c
c            we have one color here
c
            highs( total_colors ) = 
     &       selclr (comand(1), curval, ibeg, iend, ERRFLG)
            lows ( total_colors ) =  highs( total_colors )
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
      subroutine default_overlay_colors
C
      include 'ovl_props.icl'
c
c

      highs(1) = 3                                    !red
      lows (1) = 7                                    !blue

      highs(2) = 6                                    !green
      lows (2) = 6                                    !green

      highs(3) = 8                                    !violet 
      lows (3) = 8                                    !violet

      highs(4) = 4                                    !brown
      lows (4) = 4                                    !brown

      highs(5) = 9                                    !black
      lows (5) = 9                                    !black

c         
c      total_colors of colors = 5 = MAXCOM 
c
      total_colors = 5
c
c     Done
c
      return
      end
c
c
c
      integer function  is_there_slash( curval )
      character*(*) curval
      integer i
      do i = 1, 9
         if ( curval(i:i) .eq. '/') then
            is_there_slash = i+1
            return
         endif
         if ( curval(i:i) .eq. ' ' .and. i .gt. 2 ) then
            is_there_slash = 0
            return
         endif

      enddo
      is_there_slash = 0
c
c     Done
c
      return
      end
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine set_overlay_linecolors (error)
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
      integer nblank, selclr, strbeg, strend, is_there_slash
c
c
c
c
      error = .true.
      total_line_colors = 0
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
c           Return colors to their defaults and return.
c     
C     we define default line colors in this linecolors table
            call  default_overlay_linecolors
            return
         endif
c
         line_colors( total_line_colors ) = 
     &        selclr (comand(1), curval, ibeg, iend, ERRFLG)
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
      subroutine default_overlay_linecolors
C
      include 'ovl_props.icl'
c
c

      line_colors(1) = 3                                    !red

      line_colors(2) = 6                                    !green

      line_colors(3) = 8                                    !violet 

      line_colors(4) = 4                                    !brown

      line_colors(5) = 9                                    !black

c         
c      total_line_colors of colors = 5 = MAXCOM 
c
      total_line_colors = 5
c
c     Done
c
      return
      end
