      subroutine maklineplt (datarr, nx, ny, nz, nt)
c-----------------------------------------------------------------------
c     $Id: maklineplt.F,v 1.14 2001/03/05 23:51:38 davidive Exp $
c     Purpose:
c        This subroutine is the top-level driver which produces any 
c        line plot.  It is called by driver.   The routine calls
c        chklineplt to make sure you are ready to plot a line,
c        gets the 1D slice via getaverage or getlineplt, and then sets up
c        a call to drlineplt to draw the plot.
c     Arguments:
c        datarr  real  input  the array containing the data to be
c                             plotted.
c        nx      int   input  the first dimension of datarr.
c        ny      int   input  the second dimension of datarr.
c        nz      int   input  the third dimension of datarr.
c        nt      int   input  the fourth dimension of datarr.
c
c     History:
c      $Log: maklineplt.F,v $
c      Revision 1.14  2001/03/05 23:51:38  davidive
c      Fixed the zero_color code to correctly deal with contour values instead of
c      deviding by zero after discovering that the number of contours assigned
c      was zero. Fixed the line plotting routines to only plot terrain when it
c      exists.
c
c      Revision 1.13  2000/05/09 19:53:53  harry
c      Seperate contour and line colors.  Make patterns work like colors.
c
c      Revision 1.12  1998/09/03 19:19:22  harry
c      Add in 2d line plots - correct bugs in other averaging.
c
c      Revision 1.11  1997/02/12 17:36:18  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.10  1997/01/14 21:05:31  harry
c      Fix problem that could result in incorrect freeing of slabad_1d when
c      no terrain was plotted.
c
c      Revision 1.9  1996/10/16 16:05:28  warren
c      correct non perpendicular 1d plot lables
c
c Revision 1.8  1995/09/05  20:34:01  harry
c Add new slicer.  Good luck!
c
c Revision 1.7  1995/05/11  22:40:25  harry
c Add new command WRITE_1D.
c
c Revision 1.6  1995/01/09  18:52:33  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.5  1994/06/11  21:33:31  yuri
c Added more comments.
c
c Revision 1.4  1994/05/25  20:24:28  harry
c Change calls to cpnumb1 to ftoa to prevent incorrect line labels in
c susequent plots.  Removed cpnumb1 and cpnumb2 from makpointplt.F.
c (from 3.1 fix).
c
c Revision 1.3  1994/04/19  23:03:11  yuri
c Big 1D flip commit.
c
c Revision 1.2  1994/04/14  01:14:50  warren
c Fixed problem where doing overlays in line plots changes the current line
c type for contours.
c
c Revision 1.1  1993/11/29  19:39:33  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.8  1993/07/09  19:09:33  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.7  1993/07/06  18:21:13  yuri
c Corrected problem with terrain 1D plot.
c
c Revision 1.6  1993/06/23  21:04:18  yuri
c Added case for 1D/2D averaging with time in it.
c
c Revision 1.5  1993/06/11  18:19:48  yuri
c New colors for overlays.
c
c Revision 1.4  1993/06/01  19:53:59  yuri
c Second try.
c
c Revision 1.2  1993/05/24  00:02:28  yuri
c 1) Shorter calls way + new plot features.
c
c Revision 1.1.1.1  1993/04/07  19:40:13  harry
c Initial CVS checkin of IVE.
c
c Revision 2.10  1993/02/23  20:44:47  warren
c only free slabloc one time
c
c Revision 2.9  1993/02/22  21:23:08  warren
c check if allocated before freeing arrays.
c
c Revision 2.8  1993/02/17  22:03:34  warren
c added code for memory debugging and cleaned up freeing bugs
c
c Revision 2.7  1993/02/03  22:43:24  warren
c fixed to call freemem_c for stuff allocated in c
c
c Revision 2.6  1993/01/06  17:19:22  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.5  1992/11/06  17:36:55  millerp
c Fix argument list for call to drlineplt.
c
c Revision 2.4  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 2.3  1992/09/30  18:23:03  warren
c fixes for labeling and 0 sized plot boxes
c
c Revision 2.2  1992/09/30  15:10:01  warren
c fixed missing dimensions
c
c Revision 2.1  1992/09/30  15:01:10  warren
c new line slicer version
c
c Revision 2.0  1992/07/01  21:13:47  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/06/15  19:07:46  millerp
c Change argument list for scale.
c
c Revision 1.2  1992/05/29  22:35:42  millerp
c Changed argument list for scale.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'segments.icl'
      include 'message.icl'
      include 'ovl_props.icl'
c
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif

#include "slices.icl"
c
c     Argument declarations.
c
      integer   nx, ny, nz, nt
      real      datarr
      dimension datarr (nx, ny, nz, nt)
c
c     Local variable declarations. 
c
      integer	fix,		! Array of dimensions that are fixed (1)
     &				!    or averaged (2)
     &		buttons,	! 0 => slice specified by "slice command" =>
     &				!    line is not along an axis
     &		aver_counter,	! How many dimensions are set to average
     &          nter,
     &		i
      ive_ptr  	topoad,		! Topography height slice
     &		topopt		! Topography location slice
      real	pt1, pt2,	! Two points specifying slice when buttons == 0
     &		time,		! Time value for two ends of the line
     &		tmp		! For calls to getrarr to get timr.
      logical	average,	! currently not used (for future 3D->1D aver)
     &		error,		! Error flag
     &		savflg		! True => overlay plot

      dimension fix(4), pt1(4), pt2(4), tmp(4),
     &          time(2)
c
c     External function declarations - calucalate first/last non blank
c     character in a string
c
      integer          strbeg, strend
c
c     Initialization
c
      error = .false.
      if (ni_1d .ne. 0) then
         call freemem(slabad_1d)
         call freemem(slabloc_1d)
         ni_1d = 0
      endif
      slabad_1d = 0
      slabloc_1d = 0
      topoad = 0
      topopt = 0
c
c     Make sure all commands necessary to produce a line plot have
c     been given.  Label 900 => clean up and return.
c
      call chklineplt (error)
      if ( error ) then
         go to 900
      endif
c
c     Gather the information necessary to get the slice/average
c     along the line.
c
      call getlvar ('average', average, error)	! ???????
      if (error) then
         go to 900
      endif
      call getivar ('use_buttons', buttons, error)
      if ( error ) go to 900
      call getiarr ('lock', fix, 4, error)
      if ( error ) go to 900
      aver_counter=0
      do i=1,4
         if(fix(i) .eq. 2) aver_counter=aver_counter+1
      enddo
c
c     Perform the slice/average.
c
      if (aver_counter .eq. 3) then
         write (message, 100)
         call write_message
         goto 900
      else
         call getlineplt (datarr, slabad_1d, slabloc_1d, topoad, topopt, 
     &                    nx, ny, nz, nt, ni_1d, nter, error)
      endif
c
c     Prepare to plot the data if the slice/average was successful.
c
      if ( .not. error ) then
         call getlvar ('savflg', savflg, error)
         if ( error ) go to 900
         call makseg(.true.)
         call getivar ('use_buttons', buttons, error)
         if ( error ) go to 900
         if(buttons .eq. 0) then
            call getrarr ('point_1', pt1, 4, error)
            if ( error ) go to 900
            call getrarr ('point_2', pt2, 4, error)
            if ( error ) go to 900
            if(pt1(1) .eq. pt2(1) .and. pt1(2) .eq. pt2(2) .and.
     &         pt1(3) .eq. pt2(3) .and. pt1(4) .eq. pt2(4)) then
               call getrarr('point_3', pt2, 4, error)
               if(error) goto 900
            endif
            time(1) = pt1(4)
            time(2) = pt2(4)
         else         
c
c     Get the time limits.
c
            if(fix(4) .ne. 0) then
               call getrvar('tloc', time(1), error)
               time(2) = time(1)
            else
               call getrarr('plwmin_scaled',tmp,4,error)
               time(1) = tmp(4)
               call getrarr('plwmax_scaled',tmp,4,error)
               time(2) = tmp(4)
            endif
c
c     Figure out which is the independent variable.
c
            if(fix(1) .eq. 0)axes = 1
            if(fix(2) .eq. 0)axes = 2
            if(fix(3) .eq. 0)axes = 3
            if(fix(4) .eq. 0)axes = 4
         endif
c
c     Call drlineplt to do the work.
c
         if ( .not. error ) then
            call drlineplt (%val(slabad_1d), %val(slabloc_1d), ni_1d,
     &           savflg, error, time, axes)
         endif
c     
c     Plot terrain if the first overlay and a horizontal plot.
c
         if ( .not. savflg .and.
     &        fix(3) .ne. 0 .and. fix(4) .ne. 0 .and. nter .gt. 0) then
c
c     Note that we need to rescale the plot.
c     
            call setlineterr(%val(topopt), %val(topoad), nter)
            if ( .not. error ) then
c     
c     Set line type to solid and color to the foreground
c     color.
c     
               call gsplci (1)
               call dashdb (65535)
c     
c     Scale topography and plot.
c     
               call vtopo (%val(topoad), %val(topopt), nter)
            endif
         endif
         call gclsg()
c
c     If the plot worked, add it to the list of plots, otherwise
c     delete the segment.
c
         if ( .not. error ) then
            call add_to_list (cmdstr(strbeg(cmdstr):strend(cmdstr)))
         else
            call delseg (curseg)
         endif
         cmdstr(1:len(cmdstr)) = ' '
      end if
c     
c     Free all allocated memory and exit.
c     
 900  continue

      if(topoad .gt. 0)call freemem (topoad)
      if(topopt .gt. 0 .and. topopt .ne. slabad_1d)
     &	call freemem (topopt)
      if (slabad_1d .eq. 0) ni_1d = 0
      return

 100  format ('3 dimensional averaged line plots not yet implemented')

      end
