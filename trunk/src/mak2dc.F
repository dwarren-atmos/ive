#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine mak2dc (datarr, nx, ny, nz, nt)
c-----------------------------------------------------------------------
c     $Id: mak2dc.F,v 1.24 2002/08/09 19:57:59 warren Exp $
c     Purpose:
c        This subroutine is the top-level driver which produces any 2D
c        contour plot.
c     Arguments:
c        datarr  real  input  the array containing the data to be
c                             plotted.
c        nx      int   input  the first dimension of datarr.
c        ny      int   input  the second dimension of datarr.
c        nz      int   input  the third dimension of datarr.
c        nt      int   input  the fourth dimension of datarr.
c     History:
c      $Log: mak2dc.F,v $
c      Revision 1.24  2002/08/09 19:57:59  warren
c      Fixed for Linux and tested with DEC sun etc.
c      most items inside #ifdef's, or were general cleanup/fixes
c
c      Revision 1.23  1997/08/20 17:25:32  harry
c      Remove more Yuri code.
c
c      Revision 1.22  1997/04/18 22:09:40  harry
c      Fix routines to handle user errors more elegantly.
c
c      Revision 1.21  1997/02/12 17:36:13  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.20  1996/11/07 17:50:07  warren
c      remove multiple declaration that decstation didn't like.
c
c      Revision 1.19  1996/10/11 01:12:59  warren
c      To fill or not to fill is no longer the only question. Now there is a seperate
c      question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c      Labels are not fixed yet.
c
c      Revision 1.18  1996/08/07 18:14:47  harry
c      Fix array indicies for arbitrary slices.
c
c      Revision 1.17  1995/10/18 19:18:13  warren
c      Stipples from 3.3, First crack at mouse windowing and put window_points_
c      into a header file and out of the mouse stuff and slicers. Touched almost
c      everything it seems.
c
c Revision 1.16  1995/09/05  20:33:57  harry
c Add new slicer.  Good luck!
c
c Revision 1.15  1995/08/28  18:11:03  harry
c Take out unnecessary passing of terrain variables in filled contour plots.
c
c Revision 1.14  1995/07/13  18:38:36  warren
c If slabad is null don't pass it into check_nonzero. (FORTRAN if checks
c strike again)
c
c Revision 1.13  1995/03/22  21:05:59  harry
c Fix coordinates to properly overlay different windows.
c
c Revision 1.12  1995/01/09  18:52:29  warren
c Moved the line color rotation selection from everywhere to makseg.
c Note: makseg now has one argument. it should be called with true if
c you only use hicolor and false if you use two colors in the type of plot.
c
c Revision 1.11  1994/11/22  22:47:56  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.10  1994/09/14  18:19:59  harry
c Big 1D flip uncommit.
c
c Revision 1.9  1994/06/11  21:33:26  yuri
c Added more comments.
c
c Revision 1.8  1994/05/25  20:24:25  harry
c Change calls to cpnumb1 to ftoa to prevent incorrect line labels in
c susequent plots.  Removed cpnumb1 and cpnumb2 from makpointplt.F.
c (from 3.1 fix).
c
c Revision 1.7  1994/04/19  23:03:09  yuri
c Big 1D flip commit.
c
c Revision 1.6  1994/02/16  19:19:04  yuri
c Backing up to slicer in physical space.
c
c Revision 1.3  1994/01/20  23:17:11  yuri
c New format of numbers in averaging messages. (After long long testing).
c
c Revision 1.2  1994/01/20  20:44:37  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.1  1993/11/29  19:39:26  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.12  1993/10/21  23:16:00  yuri
c In Z-T case now IVE makes slice in computational space.
c
c Revision 1.11  1993/10/12  21:02:35  yuri
c Changed arithmetic averaging to trapezoidal rule.
c
c Revision 1.10  1993/08/31  18:00:47  yuri
c New comand "VSLICE" (vertical slice from (x1,y1) to (x2,y2)) installed
c 	into version with NEW transforms.
c
c Revision 1.9.2.1  1993/08/30  22:30:22  yuri
c 	New command: vslice=(x1,y1),(x2,y2) has been introduced.
c
c Revision 1.9  1993/07/15  22:13:15  warren
c Fixed scaling for default 1d ticmarks. Also fixed plotting under terrain
c problem.
c
c Revision 1.8  1993/07/09  19:09:31  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.7  1993/06/28  20:23:16  warren
c Removed local variable surface sinc surface also exists in the plotops
c common block which is included into this routine.
c
c Revision 1.6  1993/06/24  22:14:20  yuri
c 	Corrected bugs in overlays scaling + cleaning up.
c
c Revision 1.5  1993/06/23  23:13:29  yuri
c New slicer is used for averaging only.
c
c Revision 1.4  1993/06/01  19:53:56  yuri
c Second try.
c
c Revision 1.2  1993/05/23  23:59:17  yuri
c Shorter way of calls + new plotting features.
c
c Revision 1.1.1.1  1993/04/07  19:40:12  harry
c Initial CVS checkin of IVE.
c
c Revision 2.12  1993/02/17  22:03:34  warren
c added code for memory debugging and cleaned up freeing bugs
c
c Revision 2.11  1993/02/03  22:43:24  warren
c fixed to call freemem_c for stuff allocated in c
c
c Revision 2.10  1993/01/13  22:24:45  warren
c changed || to .or.
c
c Revision 2.9  1993/01/13  21:51:55  harry
c Only allow color fill on first overlay.
c
c Revision 2.8  1993/01/06  17:19:11  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.7  1992/11/23  16:35:06  millerp
c Added stuff for surface plots.
c
c Revision 2.6  1992/11/11  04:00:08  millerp
c Corrected checks for plotting terrain.
c
c Revision 2.5  1992/11/05  20:30:28  millerp
c Added stuff for averages and time-dependent cross sections.
c
c Revision 2.4  1992/09/30  18:18:54  warren
c undid setting line to solid before cptopo
c
c Revision 2.3  1992/09/29  19:46:54  warren
c made the mountain always solid
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:55:08  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:43  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/04/27  16:49:08  millerp
c Got rid of unneeded variables.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'plotops.icl'
      include 'data.icl'
      include 'segments.icl'
      include 'pltwin.icl'
      include 'message.icl'
c
c     Argument declarations.
c
      integer    nx, ny, nz, nt, buttons, fix
      real       datarr, pt1(4), pt2(4), pt3(4)
      dimension  datarr (nx, ny, nz, nt)
      dimension  fix(4)
c
c     Local variable declarations.
c
      integer  ni, nj	! Number of points in slice
                                        !    (slabad, slabloc, topoad, topopt)
      integer nter,  cindex, ierror
      ive_ptr slabad,	! Dependent variable slice
     &     topoad,      ! Topography height slice
     &     topopt       ! Topography location slice
      logical  error, 	! Error flag
     &     mapflg,      ! if true - draw map 
     &     savflg       ! True => overlay plot
c
c     limits for message
      common /ylims / pt1,pt3
c
c     External function declarations.
c
c
c     calucalate first/last non blank
c     character in a string
c
      integer  strbeg, strend
c
      error = .false.
c
      topopt = 0
      topoad = 0
      nter = 0
      slabad = 0
      call chk2d (error)
      if ( error ) then
         go to 900
      endif
c
c     preparations to the slice
c
      fix(1) = 0
      fix(2) = 0
      fix(3) = 0
      fix(4) = 0
c
C
c     May be slice determined by points instead of window?
c
         call getivar ('use_buttons', buttons, error)
         if ( error ) go to 900
         if(buttons .ne. 0) then
            call getiarr ('lock', fix, 4, error)
            if ( error ) go to 900
         else
            call getrarr ('point_1', pt1, 4, error)
            if ( error ) go to 900
            call getrarr ('point_2', pt2, 4, error)
            if ( error ) go to 900
            call getrarr ('point_3', pt3, 4, error)
            if ( error ) go to 900
            if(pt1(1) .eq. pt2(1) .and. pt2(1) .eq. pt3(1)) fix(1) = 1
            if(pt1(2) .eq. pt2(2) .and. pt2(2) .eq. pt3(2)) fix(2) = 1
            if(pt1(3) .eq. pt2(3) .and. pt2(3) .eq. pt3(3)) fix(3) = 1
            if(pt1(4) .eq. pt2(4) .and. pt2(4) .eq. pt3(4)) fix(4) = 1
         endif
         call get2dc (datarr, slabad, topoad, topopt, nx,
     &        ny, nz, nt,ni, nj, nter, error)
c
c     Plot the data.
c
      if ( .not. error ) then
c
c
         call getlvar ('savflg', savflg, error)
         if ( error ) go to 900
c
         call makseg(.false.)
c
c
         call getivar ('use_buttons', buttons, error)
         if ( error ) go to 900
         if(buttons .ne. 0) then
            call getiarr ('lock', fix, 4, error)
            if ( error ) go to 900
         else
            call getrarr ('point_1', pt1, 4, error)
            if ( error ) go to 900
            call getrarr ('point_2', pt2, 4, error)
            if ( error ) go to 900
            call getrarr ('point_3', pt3, 4, error)
            if ( error ) go to 900
            if(pt1(1) .eq. pt2(1) .and. pt2(1) .eq. pt3(1)) fix(1) = 1
            if(pt1(2) .eq. pt2(2) .and. pt2(2) .eq. pt3(2)) fix(2) = 1
            if(pt1(3) .eq. pt2(3) .and. pt2(3) .eq. pt3(3)) fix(3) = 1
            if(pt1(4) .eq. pt2(4) .and. pt2(4) .eq. pt3(4)) fix(4) = 1
         endif
c
         if(fix(3) .ne. 0 .and. fix(4) .ne. 0 ) then
            call getlvar ('mapflg', mapflg, error)
            if ( error ) go to 900
         else
            mapflg = .false.
         endif
c
         if ( mapflg .and. .not.savflg ) then
c     get current line color
            call gqplci (ierror, cindex)
            call drwmap (error)
c     reset current line color
            call gsplci (cindex)
        endif
c
         if ( error ) then
            go to 900
         endif
c
         if (fill_frequency .eq. NEVER .or.
     &        (fill_frequency .ne. ALWAYS .and.
     &        curoverlay(curlist) .gt. 1)) then
            call con2dc (%val(slabad), ni, nj, mapflg, savflg, 
     &           error)
         else
            call con2ds (%val(slabad), ni, nj, mapflg, savflg, 
     &           error)
         endif
c     
         if ((.not.error) .and. (.not.savflg) .and. (topoad.ne.0)) then
c
c     
c           Plot terrain.
c     
            if (fix(3) .ne. 0 .and. fix(4) .ne. 0) then
               call cptopo (%val(topoad), ni, ni, nj, error)
            elseif (surface .and. (fix(4) .ne. 0)) then
               call cptopo (%val(topoad), ni, ni, nj, error)
            else
               if(buttons .ne. 0) then
                  if(fix(4) .ne. 0 .and. 
     &                 (fix(1) .ne. 0 .or. fix(2) .ne. 0)) then
                     if (topopt .ne. 0) then
                        call vtopo (%val(topoad), %val(topopt), nter)
                     endif
                  endif
               elseif(fix(4) .ne. 0 .and. 
     &                 (pt1(1)*(pt2(2)-pt3(2)) + 
     &                 pt2(1)*(pt3(2)-pt1(2)) + 
     &                 pt3(1)*(pt1(2)-pt2(2))) .eq. 0) then
                  if (topopt .ne. 0) then
                     call vtopo (%val(topoad), %val(topopt), nter)
                  endif
               endif
            endif
         endif
c     
c     
c        Add command line to the command list if no errors. 
c     
         call gclsg()
         if ( .not. error ) then
            call add_to_list (cmdstr(strbeg(cmdstr):strend(cmdstr)))
         else
            call delseg (curseg)
         endif
         cmdstr(1:len(cmdstr)) = ' '
      endif
c
 900  continue
      if(error)then
         write (message, 100)
 100     format	(' There was an error in mak2dc')
         call write_message
       endif
c
c     Free all allocated memory.
c
      if ( slabad .gt. 0 ) call freemem (slabad)
      if ( topopt .gt. 0 ) call freemem (topopt)
      if ( topoad .gt. 0 ) call freemem (topoad)
c
 130  format (1x,'In subroutine maklineplt slabloc = 0 .')
c
      return
      end


      logical function check_nonzero(arr, i)
c     chek if any elements are non zero
c
      integer i
      real arr(i)
c
c
c
      integer j
      check_nonzero=.false.
      do j = 1, i
         if(arr(j) .ne. 0.0)check_nonzero= .true.
      enddo
      return
      end
