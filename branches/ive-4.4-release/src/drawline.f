      subroutine drawline (xpts, ypts, npts, error)
c-----------------------------------------------------------------------
c     $Id: drawline.f,v 1.10 2000/05/09 19:53:45 harry Exp $
c     Purpose:
c        This routine makes line plots.
c     Arguments:
c        xpts   real  input   array containing points along the plot
c                             abscissa.
c        ypts   real  input   array containing points along the plot
c                             ordinate.
c        npts   int   input   the number of points in xpts and ypts.
c        error  logi  output  indicates possible errors found in this
c                             routine.
c                             error = false  no errors detected
c                             error = true   error detected
c     History:
c     $Log: drawline.f,v $
c     Revision 1.10  2000/05/09 19:53:45  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
c     Revision 1.9  1999/12/22 00:33:12  warren
c     Fixed Dales crashing bug when changing from point slicer to normal. Also
c     have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c Revision 1.8  1995/03/20  23:56:58  warren
c Fixed Dales not sticking color changes and louisa's missing tic mark problem.
c
c Revision 1.7  1994/09/07  22:07:05  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.6  1993/06/23  21:16:46  yuri
c Just clean-up.
c
c Revision 1.5  1993/06/01  19:52:10  yuri
c Second attempt.
c
c Revision 1.3  1993/05/23  23:48:38  yuri
c Colors are changing if you are using overlay. Also limits are
c frozen in this case.
c
c Revision 1.2  1993/04/09  16:04:06  yuri
c 	If save flag = on, colours are changing every time,
c 	otherwise, color = default one .
c
c Revision 2.3  1992/09/30  22:25:34  warren
c fixed missing lines with x(1) as missing value
c
c Revision 2.2  1992/09/30  18:23:03  warren
c fixes for labeling and 0 sized plot boxes
c
c Revision 2.1  1992/09/30  15:01:10  warren
c new line slicer version
c
c Revision 2.1  1992/09/30  15:01:10  warren
c new line slicer version
c
c Revision 2.0  1992/07/01  21:12:53  millerp
c Released version rereving to 2.0
c
c Revision 2.0  1992/07/01  21:12:53  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/05/05  15:23:09  millerp
c Deleted unused variables.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer   npts
      logical   error,savflg
      real      xpts, ypts
      dimension xpts(npts), ypts(npts)
c
c
c     Local variable declarations.
c
      integer   ierr, oldlw, linecolor, linepat, oldclp
      real      crec, linwdth, spval
      dimension crec(4)
c
      error = .false.
c
c     Turn clipping on.
c
      call gqclip (ierr, oldclp, crec)
      if ( ierr .ne. 0 ) then
         go to 90
      endif
      call gsclip (1)
c
c     Get special value.
c
      call getrvar ('spval', spval, error)
      if ( error ) then
         go to 100
      endif
c
c     Set line pattern.
c
      call getivar ('hipattern', linepat, error)
      if ( error ) then
         go to 100
      endif
      call dashdb (linepat)
c     
c     Set line color.
c
      call getlvar('savflg',savflg,error)

      call getivar ('hicolor', linecolor, error)
      if ( error ) then
         go to 100
      endif
      call gsplci (linecolor)
c
c     Set line width.
c
      call gqlwsc (ierr, oldlw)
      if ( ierr .ne. 0 ) then
         go to 100
      endif
      call getrvar ('linwdth', linwdth, error)
      if ( error ) then
         go to 110
      endif
      call gslwsc (linwdth)
c
c     Draw the curve.
c     
      call drwline (xpts, ypts, npts, spval)
c
c     Normal exit.
c
      call gsclip (oldclp)
      call gslwsc (oldlw)
      return
c
c     Error exits.
c
 90   return
c
 100  call gsclip (oldclp)
      return
c
 110  call gsclip (oldclp)
      call gslwsc (oldlw)
      return
c
c
      end
c
c
      subroutine drwline (xpts, ypts, npts, spval)
c-----------------------------------------------------------------------
c     $Id: drawline.f,v 1.10 2000/05/09 19:53:45 harry Exp $
c     Purpose:
c        This routine serves as an interface to the NCAR Graphics
c        line drawing routines CURVED. It handles cases where 
c        non-plottable values may be present. If this is the case, no 
c        line segment is drawn if one of the endpoints is a 
c        non-plottable value. Note that if this feature is used, the 
c        code executes more slowly.
c     Arguments:
c        xpts   real  input   points in user coordinates along the plot
c                             abscissa.
c        ypts   real  input   points in user coordinates along the plot
c                             ordinate.
c        npts   int   input   the number of points in xpts and ypts.
c                             Each pair of (xpts, ypts) will be 
c                             connected with line segments.
c        spval  real  input   a non-plottable value. If spval is 
c                             non-zero, then a line segment will not
c                             be drawn if one of its endpoints equals
c                             spval. If spval is zero, then it is 
c                             assumed that all points (xpts, ypts) are
c                             to be connected with line segments.
c     History: 
c        $Log: drawline.f,v $
c        Revision 1.10  2000/05/09 19:53:45  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c        Revision 1.9  1999/12/22 00:33:12  warren
c        Fixed Dales crashing bug when changing from point slicer to normal. Also
c        have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c Revision 1.8  1995/03/20  23:56:58  warren
c Fixed Dales not sticking color changes and louisa's missing tic mark problem.
c
c Revision 1.7  1994/09/07  22:07:05  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.6  1993/06/23  21:16:46  yuri
c Just clean-up.
c
c Revision 1.5  1993/06/01  19:52:10  yuri
c Second attempt.
c
c Revision 1.3  1993/05/23  23:48:38  yuri
c Colors are changing if you are using overlay. Also limits are
c frozen in this case.
c
c Revision 1.2  1993/04/09  16:04:06  yuri
c 	If save flag = on, colours are changing every time,
c 	otherwise, color = default one .
c
c Revision 2.3  1992/09/30  22:25:34  warren
c fixed missing lines with x(1) as missing value
c
c Revision 2.2  1992/09/30  18:23:03  warren
c fixes for labeling and 0 sized plot boxes
c
c Revision 2.1  1992/09/30  15:01:10  warren
c new line slicer version
c
c Revision 2.1  1992/09/30  15:01:10  warren
c new line slicer version
c
c Revision 2.0  1992/07/01  21:12:53  millerp
c Released version rereving to 2.0
c
c Revision 2.0  1992/07/01  21:12:53  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/05/05  15:23:09  millerp
c Deleted unused variables.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c     Argument declarations.
c
      integer    npts
      real       xpts, ypts, spval
      dimension xpts(npts), ypts(npts)
c
c     Local variable declarations.
c
      integer    begin, i
c
c
c
      if ( spval .eq. 0.0 ) then
c
c        Case where all points can be connected.
c
         call curved (xpts, ypts, npts)
      else
c
c        Make sure that points whose value equals spval are not 
c        considered in the plot.
c
         begin = 1
         do 10 i = 1, npts
            if ( (xpts(i) .eq. spval) .or. (ypts(i) .eq. spval) ) then
               if(i .ne. 1) then
                  if ( (xpts(i-1) .ne. spval) .and. 
     &                 (ypts(i-1) .ne. spval) ) then
                     call curved (xpts(begin), ypts(begin), i-begin)
                  endif
               endif
               begin = i + 1
            endif
            call plotit (0, 0, 0)
 10      continue
         if(begin .eq. i) then
c            write(6,*)'no points to plot ',begin,' to ',i
            continue
         else
            call curved (xpts(begin), ypts(begin), i - begin)
         endif
      endif
c
c     Empty pen move buffer.
c
      call plotit (0, 0, 0)
c
      return
      end

