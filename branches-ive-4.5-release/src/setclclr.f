      subroutine setclclr (error)
c-----------------------------------------------------------------------
c     $Id: setclclr.f,v 1.17 2006-10-05 16:45:44 reinecke Exp $
c     Purpose:
c        This routine sets the contour line color.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c        $Log: setclclr.f,v $
c        Revision 1.17  2006-10-05 16:45:44  reinecke
c        Added grey line option
c
c        Revision 1.16  2000/09/21 19:49:48  harry
c        Fix errors in line color and pattern handling.
c
c        Revision 1.15  2000/05/09 19:53:58  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c Revision 1.14  1995/01/09  18:52:41  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.13  1994/01/20  20:44:51  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.12  1993/07/03  01:53:24  yuri
c Tuning of overlay colors.
c
c Revision 1.11  1993/07/01  23:14:19  yuri
c Just cleaning up.
c
c Revision 1.10  1993/07/01  19:35:00  yuri
c Corrected error in multiple overlay generating with mix
c vector/scalar plots.
c
c Revision 1.9  1993/06/18  19:40:23  yuri
c Color table updated.
c
c Revision 1.8  1993/06/16  17:35:39  yuri
c Improved vector color part.
c
c Revision 1.7  1993/06/11  18:22:51  yuri
c New colors for overlays.
c
c Revision 1.6  1993/06/10  16:29:51  yuri
c Vector color stuff added.
c
c Revision 1.5  1993/06/04  17:25:25  yuri
c Linecolors command from now overwrites current pair of colors
c in overlay colors table.
c
c Revision 1.4  1993/06/01  19:57:12  yuri
c Second try.
c
c Revision 1.2  1993/05/24  00:07:39  yuri
c Colors are changing in overlay case.
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
c
c
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      integer             cindex, ERRFLG, i, ibeg, iend, numblank
      parameter ( ERRFLG = -999 )
      dimension cindex(2)
c
c     External function declarations.
c
      integer nblank, selclr, strbeg, strend
c
c
      error = .false.
      numblank = 0
c      just_setup = .true.
c
      do 10 i = 1, 2
         curval = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg) + 1)     
c
C
         if ( nblank(curval) .eq. 0 ) then
            cindex(i) = ERRFLG
            numblank = numblank + 1
         else
c
c           Set the contour line color. 
c
            cindex(i) = selclr (comand(1), curval, ibeg, iend, ERRFLG)
         endif
 10   continue
c      if( cindex(2) .eq. ERRFLG)  cindex(2) = cindex(1)   
c
c
      if ( numblank .eq. 2 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                 strend(comand(1)))
         call write_message
         error = .true.
         return
      endif
c
      if ( (cindex(1) .eq. ERRFLG) .and. (cindex(2) .eq. ERRFLG) ) then
c
c        None of the values for this command were correct, so set the
c        error flag and return.
c
         error = .true.
         return
      endif
c
 11   continue
      if ( cindex(1) .ne. ERRFLG ) then
c
c        Set the high contour line color.
c
         hicolor = cindex(1)
         just_set_hicl = .true.
       endif
c
      if ( cindex(2) .ne. ERRFLG ) then
c
c        Set the low contour line color.
c
         locolor  = cindex(2)
         just_set_locl = .true.
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
      function selclr (comand, string, ibeg, iend, errflg)
c-----------------------------------------------------------------------
c     Purpose:
c        This function is used to select the contour line color. The
c        function returns a color index to be used to set the line
c        colors.
c     Arguments:
c        comand  char  input  the command entered by the user.
c        string  char  input  the value assiged to the command.
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
      integer        selclr
c
c
c     Argument declarations.
c
      character *(*) comand, string
      integer        ibeg, iend, errflg
c
c
c     External function declarations.
c
      integer        strbeg, strend
c
c
c
c
      if     ( string(ibeg:ibeg)   .eq. 'W' ) then
c        Color is white.
         selclr = 2
      elseif ( string(ibeg:ibeg)   .eq. 'R' ) then
c        Color is red.
         selclr = 3
      elseif ( string(ibeg:ibeg+1) .eq. 'BR' ) then
c        Color is brown.
         selclr = 4
      elseif ( string(ibeg:ibeg)   .eq. 'Y' ) then
c        Color is yellow.
         selclr = 5
      elseif ( string(ibeg:ibeg+3)   .eq. 'GREE' ) then
c        Color is green.
         selclr = 6
      elseif ( string(ibeg:ibeg+3)   .eq. 'GREY' ) then
c        Color is gray.
         selclr = 12
      elseif ( string(ibeg:ibeg+2) .eq. 'BLU' ) then
c        Color is blue.
         selclr = 7
      elseif ( string(ibeg:ibeg)   .eq. 'V' ) then
c        Color is violet.
         selclr = 8
      elseif ( string(ibeg:ibeg+2) .eq. 'BLA' ) then
c        Color is black.
         selclr = 9
      elseif (string(ibeg:ibeg+1) .eq. 'DE' ) then
         selclr = 0
      else
c
c        An incorrect value was given. Log an error and return the value
c        of errflg.
c
         write (message, 110) string(ibeg:iend), 
     &                       comand(strbeg(comand):strend(comand))
         call write_message
         selclr = errflg
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
