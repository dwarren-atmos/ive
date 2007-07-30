	subroutine makseg (lineplot)
c-----------------------------------------------------------------------
c	$Id: makseg.f,v 1.8 2000/09/21 19:49:45 harry Exp $
c	Purpose:
c       This routine is called to create new segments between
c       overlays
c       
c	History:
c       $Log: makseg.f,v $
c       Revision 1.8  2000/09/21 19:49:45  harry
c       Fix errors in line color and pattern handling.
c
c       Revision 1.7  2000/05/09 19:53:54  harry
c       Seperate contour and line colors.  Make patterns work like colors.
c
c       Revision 1.6  2000/04/07 20:08:06  harryive
c       Take out "extra" code that went outside array bounds.
c
c       Revision 1.5  1997/10/29 18:26:51  harry
c       Make sure that plots that do not depend on the loop parameter do not change.
c
c Revision 1.4  1995/05/11  20:21:15  warren
c Bring printer updates forward from 3.2
c
c Revision 1.3  1995/03/20  23:57:09  warren
c Fixed Dales not sticking color changes and louisa's missing tic mark problem.
c
c Revision 1.2  1995/01/09  18:52:34  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c       Revision 1.1.1.1  1993/04/07  19:40:52  harry
c       Initial CVS checkin of IVE.
c       
c       Revision 2.1  1993/01/06  17:16:36  harry
c       Change a segment from one each plot to one each overlay.
c
c       Revision 2.0  1992/07/01  21:15:32  millerp
c       Released version rereving to 2.0
c       
c       Revision 1.1  1992/04/21  00:11:26  millerp
c       Initial revision
c       
c-----------------------------------------------------------------------
c       
c	Arguments
	logical lineplot       ! Is this a line plot?
c
	include 'segments.icl'
	include 'color.icl'
	include 'ovl_props.icl'
	logical savflg, error
	integer i, colnum, patnum, io, ip
c       
c       
c       
	call getlvar ('savflg', savflg, error)
	if (error) return
	if (just_set_hicl .and. hicolor .eq. 0) then
	   just_set_hicl = .false.
	   if (lineplot) then
	      call default_overlay_linecolors
	   else
	      call default_overlay_colors
	   endif
	endif
	if (just_set_hipat .and. hipattern .eq. 0) then
	   just_set_hipat = .false.
	   if (lineplot) then
	      call default_overlay_linepatterns
	   else
	      call default_overlay_patterns
	   endif
	endif
	if(.not.savflg .and. curplot.gt.0) call dump_color_table(curlist)
	if (.not.savflg .or. curplot.eq.0) then
	   if (curplot.eq.0.or.curoverlay(curlist).gt.0) then
	      curlist = mod(curplot, maxplot) + 1
	      curplot = curplot + 1
	      if (curplot .gt. curlist) then
		 do i = 1, curoverlay(curlist)
c
c       make sure this segment is not used elsewhere.
c
		    do ip = 1, maxplot
		       if (ip.ne.curlist) then
			  do io = 1, curoverlay(ip)
			     if (segs(i, curlist) .eq. segs(io, ip))
	1			  goto 10
			  enddo
		       endif
		    enddo
		    call delseg(segs(i, curlist))
 10		 enddo
	      endif
	      call clrscr
	   endif
	   curoverlay(curlist) = 1
	   if (lineplot) then
	      if (just_set_hicl) then
		 line_colors(1) = hicolor
	      else
		 hicolor = line_colors(1)
	      endif
	      if (just_set_hipat) then
		 ovl_patterns(1) = hipattern
	      else
		 hipattern = ovl_patterns(1)
	      endif
	   else
	      if (just_set_hicl) then
		 highs(1) = hicolor
	      else
		 hicolor = highs(1)
	      endif
	      if (just_set_locl) then
		 lows(1) = locolor
	      else
		 locolor = lows(1)
	      endif
	      if (just_set_hipat) then
		 highpat(1) = hipattern
	      else
		 hipattern = highpat(1)
	      endif
	      if (just_set_lopat) then
		 lowpat(1) = lopattern
	      else
		 lopattern = lowpat(1)
	      endif
	   endif
	else
	   curoverlay(curlist) = curoverlay(curlist) + 1
	   if (lineplot) then
	      colnum = mod(curoverlay(curlist), total_line_colors)
	      if(colnum .eq. 0)colnum=total_line_colors
	      patnum = mod(curoverlay(curlist), total_patterns)
	      if(patnum .eq. 0)patnum=total_patterns
	      if (just_set_hicl) then
		 line_colors(colnum) = hicolor
	      else
		 hicolor = line_colors(colnum)
	      endif
	      if (just_set_hipat) then
		 ovl_patterns(patnum) = hipattern
	      else
		 hipattern = ovl_patterns(patnum)
	      endif
	   else
	      colnum = mod(curoverlay(curlist), total_colors)
	      if(colnum .eq. 0)colnum=total_colors
	      patnum = mod(curoverlay(curlist), total_contour_patterns)
	      if(patnum .eq. 0)patnum=total_contour_patterns
	      if (just_set_hicl) then
		 highs(colnum) = hicolor
	      else
		 hicolor = highs(colnum)
	      endif
	      if (just_set_locl) then
		 lows(colnum) = locolor
	      else
		 locolor = lows(colnum)
	      endif
	      if (just_set_hipat) then
		 highpat(patnum) = hipattern
	      else
		 hipattern = highpat(patnum)
	      endif
	      if (just_set_lopat) then
		 lowpat(patnum) = lopattern
	      else
		 lopattern = lowpat(patnum)
	      endif
	   endif
	endif
	curseg = curseg + 1
	call gcrsg (curseg)
	segs(curoverlay(curlist), curlist) = curseg
	just_set_locl = .false.
	just_set_hicl = .false.
	just_set_hipat = .false.
	just_set_lopat = .false.
c       
c       
	return
	end
c       
c
      subroutine delseg (segnum)
c-----------------------------------------------------------------------
c	$Id: makseg.f,v 1.8 2000/09/21 19:49:45 harry Exp $
c	Purpose:
c	   This routine deletes a segment from all workstations.
c          If it is the current segment, it lifts it from the
c          open workstation.
c	Arguments:
c	   segnum  int  input  the segment to be deleted.
c	History:
c	   $Log: makseg.f,v $
c	   Revision 1.8  2000/09/21 19:49:45  harry
c	   Fix errors in line color and pattern handling.
c	
c	   Revision 1.7  2000/05/09 19:53:54  harry
c	   Seperate contour and line colors.  Make patterns work like colors.
c
c	   Revision 1.6  2000/04/07 20:08:06  harryive
c	   Take out "extra" code that went outside array bounds.
c
c	   Revision 1.5  1997/10/29 18:26:51  harry
c	   Make sure that plots that do not depend on the loop parameter do not change.
c
c Revision 1.4  1995/05/11  20:21:15  warren
c Bring printer updates forward from 3.2
c
c Revision 1.3  1995/03/20  23:57:09  warren
c Fixed Dales not sticking color changes and louisa's missing tic mark problem.
c
c Revision 1.2  1995/01/09  18:52:34  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.1.1.1  1993/04/07  19:40:52  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1993/01/06  17:16:36  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.0  1992/07/01  21:15:32  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
c
c     Argument declarations.
c
      integer segnum
c
c
c     Local variable declarations.
c
      include 'segments.icl'
      integer istate,  GSGOP
      PARAMETER (GSGOP=4)
c
c
c
      if ( segnum .eq. curseg ) then
c
c        Delete the current segment and reset curseg.
c
         call gqops (istate)
         if ( istate .eq. GSGOP ) then
            call gclsg()
         endif
c
         call gdsg (segnum)
         curseg = curseg - 1
         if (curoverlay(curlist).eq.1) then
            curoverlay(curlist) = 0
            call clrscr
         else
            curoverlay(curlist) = curoverlay(curlist) - 1
            call guwk(1, 1)
         endif
      else
c
c        Just delete the specified segment.
c
         call gdsg (segnum)
      endif
c
c
      return
      end
