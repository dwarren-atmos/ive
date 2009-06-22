#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine get2dc (datarr, slabad, topoad, topopt, nx, ny, nz, nt,
     &                   ni, nj, nter, error)
c-----------------------------------------------------------------------
c     $Id: get2dc.F,v 1.17 2000/04/05 22:38:53 harry Exp $
c     Purpose:
c        This routine is the top-level routine which produces 2D contor
c        and 2D color-filled plots.
c     Arguments:
c        datarr  real  input   the data array from which 2D slices will
c                              be extracted.
c        slabad  real  output  the array that contains the 2D slice 
c                              to be contoured.
c        topoad  real  output  the array containing topography heights
c                              under each point in slabad.
c        topopt  real  output  the array containing the location of
c                              each point in topoad along the slice
c                              (vertical slices).
c        nx      int   input   the first dimension of datarr.
c        ny      int   input   the second dimension of datarr.
c        nz      int   input   the third dimension of datarr.
c        nt      int   input   the fourth dimension of datarr.
c        ni      int   output  the number of points in the windowed 
c                              domain along the first slice dimension.
c        nj      int   output  the number of points in the windowed 
c                              domain along the second slice dimension.
c        nter    int   output  the number of points in topoad.
c        error   logi  output  indicates possible errors found in this
c                              routine or the routines further down in
c                              the call sequence.
c     History:
c        $Log: get2dc.F,v $
c        Revision 1.17  2000/04/05 22:38:53  harry
c        Correct bugs in general surface plotting.
c
c        Revision 1.16  1998/07/28 18:22:22  harryive
c        New fixes for trajectories when they are the first overlay.
c        Also, remove old Yuri routine.
c
c        Revision 1.15  1998/07/16 22:10:55  harryive
c        Add two dimensional slicing with 2 averaged dimensions.
c
c        Revision 1.14  1997/08/20 17:25:31  harry
c        Remove more Yuri code.
c
c        Revision 1.13  1997/03/14 19:26:54  harry
c        Fix topography plotting for vertical plots.
c
c        Revision 1.12  1997/02/12 17:35:53  warren
c        Changed pointer type to ive_ptr to avoid interaction with
c	 F90 pointer type.
c
c        Revision 1.11  1996/10/11 01:12:53  warren
c        To fill or not to fill is no longer the only question. Now there is
c	 a seperate question of solid or pattern. Also first part of mouse
c	 slicing. 2d -> 1d
c        Labels are not fixed yet.
c
c Revision 1.10  1995/10/18  19:18:11  warren
c Stipples from 3.3, First crack at mouse windowing and put window_points_
c into a header file and out of the mouse stuff and slicers. Touched almost
c everything it seems.
c
c Revision 1.9  1995/09/05  20:33:46  harry
c Add new slicer.  Good luck!
c
c Revision 1.8  1994/09/14  18:19:54  harry
c Big 1D flip uncommit.
c
c Revision 1.7  1994/06/11  21:33:20  yuri
c Added more comments.
c
c Revision 1.6  1994/04/19  23:02:57  yuri
c Big 1D flip commit.
c
c Revision 1.5  1994/03/30  22:47:12  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.4  1994/02/28  21:47:41  harry
c Correct parameter in a call to get_topo.
c
c Revision 1.3  1994/02/16  17:00:29  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.2  1994/02/09  23:46:28  yuri
c  Make sure, that error value propagates properly
c to make debugging easier.
c
c Revision 1.1  1993/11/29  19:39:14  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.16  1993/10/21  23:15:56  yuri
c In Z-T case now IVE makes slice in computational space.
c
c Revision 1.15  1993/10/07  17:43:07  yuri
c Propagating of tune-up correction into IVE-1-? version.
c
c Revision 1.14  1993/10/06  19:19:18  yuri
c Solurion for interpolation coefficients problem propagated.
c
c Revision 1.13  1993/10/01  22:39:33  yuri
c Fine tune to get IVE (ver. 3.1) ready for official release.
c
c Revision 1.12  1993/08/31  18:00:38  yuri
c New comand "VSLICE" (vertical slice from (x1,y1) to (x2,y2)) installed
c 	into version with NEW transforms.
c
c
c Revision 1.10  1993/06/28  22:58:28  harry
c Give more intelligent error messages in the slicer.  Remove error
c messages from get2dc.
c
c Revision 1.9  1993/06/24  22:14:17  yuri
c 	Corrected bugs in overlays scaling + cleaning up.
c
c Revision 1.8  1993/06/23  23:34:16  yuri
c Cleaning after long working day.
c
c Revision 1.7  1993/06/23  23:25:01  yuri
c New slicer is used for averaging only.
c
c Revision 1.4  1993/06/01  19:52:13  yuri
c Second attempt.
c
c Revision 1.2  1993/05/23  23:55:21  yuri
c Time/coordinates mixed  plots are available now.
c
c Revision 1.1.1.1  1993/04/07  19:39:55  harry
c Initial CVS checkin of IVE.
c
c Revision 2.6  1993/02/17  22:03:34  warren
c added code for memory debugging and cleaned up freeing bugs
c
c Revision 2.5  1992/12/30  19:47:06  warren
c fixed point for surface
c
c Revision 2.4  1992/11/23  16:35:06  millerp
c Added stuff for surface plots.
c
c Revision 2.3  1992/11/05  20:30:28  millerp
c Added stuff for averages and time-dependent cross sections.
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:56:01  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:05  millerp
c Released version rereving to 2.0
c
c Revision 1.3  1992/04/27  16:47:36  millerp
c Got rid of unneeded variables.
c
c Revision 1.2  1992/04/21  18:57:42  millerp
c Fixed to use write_message.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'message.icl'
      include 'plotops.icl'
      include 'topo.icl'
c
c     Argument declarations
c
      integer         ni, nj, nx, ny, nz, nt, nter
      ive_ptr         slabad, topoad, topopt
      logical         error
      real            datarr
      dimension       datarr(nx, ny, nz, nt)
c
c
c     Local valiable declarations.
c
      integer   i, j, num_avg, da(2)
      logical   savflg
      real      pt1(MAXDIM), pt2(MAXDIM), pt3(MAXDIM), EPSILON,
     &          special, pt1t(MAXDIM), pt2t(MAXDIM)
      parameter (EPSILON = 1.0E-5)

c
c
c     External function declarations.
c
      ive_ptr slicer, slicer2d1a, slicer2d2a
c
c
c     Common blocks used by set2d.
c
      real                dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      common / voffsets / dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
c
c     limits for message
      real ptm1(4), ptm3(4)
      common /ylims / ptm1,ptm3
c---------------------------------------------------------------
c
      error = .false.
c
c     Get the value of the save flag.
c
      call getlvar ('savflg', savflg, error)
c
c     Get the value of the spval
c
      call getrvar ('spval', special, error)
      num_avg = 0
      do i = 1, 4
         if (lock(i).eq.2) num_avg = num_avg + 1
      enddo
c 
c     Get cross sections.
c
      if (use_buttons .ne. 0) then
c         
c        Get the cross section. On input to slicer, pt1-pt3
c        define three points on the slice plane. On output from slicer,
c        pt1 is the origin of the slice plane in physical space,
c        pt2 is the origin of the slice plane in array space, and
c        pt3 is the maximum extent of the slice plane in physical space.
c
         if (lock(1).eq.1) then
            pt1(1) = xloc
            pt2(1) = xloc
            pt3(1) = xloc
         else
            pt1(1) = 0.
            pt2(1) = 1.
            pt3(1) = 0.
         endif
         if (lock(2).eq.1) then
            pt1(2) = yloc
            pt2(2) = yloc
            pt3(2) = yloc
         else
            pt1(2) = 0.
            if (lock(1).eq.0) then
               pt2(2) = 0.
               pt3(2) = 1.
            else
               pt2(2) = 1.
               pt3(2) = 0.
            endif
         endif
         if (lock(3).eq.1) then
            pt1(3) = zloc
            pt2(3) = zloc
            pt3(3) = zloc
         else
            pt1(3) = 0.
            if (lock(1).eq.0 .or. lock(2).eq.0) then
               pt2(3) = 0.
               pt3(3) = 1.
            else
               pt2(3) = 1.
               pt3(3) = 0.
            endif
         endif
         if (lock(4).eq.1) then
            pt1(4) = tloc
            pt2(4) = tloc
            pt3(4) = tloc
         else
            pt1(4) = 0.
            pt2(4) = 0.
            pt3(4) = 1.
         endif
c            print *,' dims = ', nx,ny,nz,ni,nj
         if (num_avg .eq. 0) then
            slabad = slicer(datarr, nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, nj, ni, special, 0, surface)
         else if (num_avg .eq. 1) then
            do i = 1, 4
               if (lock(i).eq.2) da(1) = i
            enddo
            slabad = slicer2d1a(datarr, nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special, 0, surface)
         else if (num_avg .eq. 2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da(j) = i
                  j = j + 1
               endif
            enddo
            slabad = slicer2d2a(datarr, nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special, 0)
         endif
            if ( slabad .le. 0 ) then
               error = .true.
               go to 10
            endif
            if (.not. savflg) then
               call get_topo (topoad, topopt, ni, nj, pt1, pt3, MAXDIM, 
     &                        nter, error)
               topo_ni = ni
               topo_nj = nj
               do i = 1, MAXDIM
                  topo_pt1(i) = pt1(i)
                  topo_pt3(i) = pt3(i)
               enddo
            endif

            if(lock(1) .eq. 1) then
               pt1(1) = xloc
               pt3(1) = xloc
            else
               pt1(1) = plwmin(1)
               pt3(1) = plwmax(1)
            endif
            if(lock(2) .eq. 1) then
               pt1(2) = yloc
               pt3(2) = yloc
            else
               pt1(2) = plwmin(2)
               pt3(2) = plwmax(2)
            endif
            if(lock(3) .eq. 1) then
               pt1(3) = zloc
               pt3(3) = zloc
            else
               pt1(3) = plwmin(3)
               pt3(3) = plwmax(3)
            endif
            if(lock(4) .eq. 1) then
               pt1(4) = tloc
               pt3(4) = tloc
            else
               pt1(4) = plwmin(4)
               pt3(4) = plwmax(4)
            endif
c     -------
C     let's remember limits
            do i=1,4
               if( plwmax(i)*plwmax_scaled(i) .ne. 0.) then
                  ptm1(i) = pt1(i)/plwmax(i)*plwmax_scaled(i)
                  ptm3(i) = pt3(i)/plwmax(i)*plwmax_scaled(i)
               else
                  ptm1(i) = pt1(i)
                  ptm3(i) = pt3(i)
               endif
            enddo
C
      else
c
c     stuff for vert slicer
c
c     z should be fixed, and x/y free/fixed
c
c         if (lock(3) .ne. 0) then
c            error = .true.
c            write (message, 130)
c            call write_message
c            return
c         endif
         if (lock(1) .eq. 1) then
c--------------- dims for slicer-------------
            ni = 1 * ny
            nj = 1 * nz 
         elseif (lock(2) .eq. 1) then
c--------------- dims for slicer-------------
            ni = 1 * nx
            nj = 1 * nz 
         endif
         call getrarr ('point_1', pt1, 4, error)
         call getrarr ('point_2', pt2, 4, error)
         call getrarr ('point_3', pt3, 4, error)
         if (num_avg .eq. 0) then
            slabad = slicer(datarr, nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, nj, ni, special, 0, surface)
         else if (num_avg .eq. 1) then
            do i = 1, 4
               if (lock(i).eq.2) da(1) = i
            enddo
            slabad = slicer2d1a(datarr, nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special, 0, surface)
         else if (num_avg .eq. 2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da(j) = i
                  j = j + 1
               endif
            enddo
            slabad = slicer2d2a(datarr, nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special, 0)
         endif
         if ( slabad .eq. 0 ) then
            error = .true.
            write (message, 120)
            call write_message
            return
         endif
C        let's remember limits
            do i=1,4
               if( plwmax(i)*plwmax_scaled(i) .ne. 0.) then
                  ptm1(i) = pt1(i)/plwmax(i)*plwmax_scaled(i)
                  ptm3(i) = pt3(i)/plwmax(i)*plwmax_scaled(i)
               else
                  ptm1(i) = pt1(i)
                  ptm3(i) = pt3(i)
               endif
            enddo
            if (.not. savflg) then
               call get_topo (topoad, topopt, ni, nj, pt1, pt3,
     &              MAXDIM, nter, error)
               topo_ni = ni
               topo_nj = nj
               do i = 1, MAXDIM
                  topo_pt1(i) = pt1(i)
                  topo_pt3(i) = pt3(i)
               enddo
            endif
c
c     end of big if: vslice/window slice
c
         endif
c     
c     Set some factors needed by set2d.
c     
         xbeg = pt1(1)
         ybeg = pt1(2)
         zbeg = pt1(3)
         tbeg = pt1(4)
         xend = pt3(1)
         yend = pt3(2)
         zend = pt3(3)
         tend = pt3(4)
c
c
   10 continue
	if(slabad .le. 0)then
            write (message, 120)
            call write_message
            return
         endif
Cc
c     Format statements.
c
 100  format (1x,'Memory for slabs cannot be allocated in ',
     &           'subroutine get2dc.')
 120  format (1x,'There was an error in calculating the vertical ',
     &           'cross section.')
 130  format (1x,' Z should be free for vslice ')
c
      return
      end
c     
      subroutine get_topo (topoad, topopt, ni, nj, pt1, pt2, ndim, 
     &                     nter, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine gets the terrain underlying a cross-section.
c     Arguments:
c        topoad  int  output   array containing topo locations.
c        topopt  int  output   array containing topo values.
c        ni      int  input    number of points in slab along the 
c                              abscissa.
c        nj      int  input    number of points in slab along the 
c                              ordinate.
c        pt1     real  input   array containing min x, y, z, t values of
c                              the slab.
c        pt2     real  input   array containing max x, y, z, t values of
c                              the slab.
c        ndim    int   input   size of pt1, pt2.
c        nter    int   output  number of points in topoad if vertical
c        error   logi  output  error flag.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'message.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
c
c
c     Argument declarations.
c
      ive_ptr       topoad, topopt
      integer       ni, nj, ndim, nter
      logical       error
      real          pt1, pt2
      dimension pt1(ndim), pt2(ndim)
c
      ive_ptr       xter, yter
c
c
c     External function declarations.
c
      ive_ptr       getmem
c
      error = .false. 
c
      if ((use_buttons .ne. 0 .and. (surface .or. (lock(3) .ne. 0)))
     &	.or. (use_buttons .eq. 0 .and. (yaxis .ne. 3 .or. xaxis.eq.4)))
     &		 then
c     
c        Allocate space for the terrain array.
c     
         topoad = getmem (ni * nj)
c     
c        Make sure memory has been properly allocated.
c     
         if ( topoad .le. 0 ) then
            error = .true.
            write (message, 105)
            call write_message
            go to 10
         endif
c     
c        Fill an array with terrain heights.
c     
         call horiz_ter_trans(%val(topoad), ni, nj, stag(1), stag(2), 
     &               0.0, error)
c     
         if ( error ) then
            write (message, 110) 
            call write_message
            go to 10
         endif
      else
c     
c     Vertical slice
c     Get the terrain height.
c     
c
         call vert_ter_trans(xter, yter, topoad, nter, pt1, pt2,
     &        plwmin(3))
         if (mod(xaxis,10) .eq. 2) then
            call freemem(xter)
            topopt = yter
         else
            call freemem(yter)
            topopt = xter
         endif
      endif
c
 10   if (error) then
         if(topopt .gt. 0) call freemem (topopt)
         if(topoad .gt. 0) call freemem (topoad)
      endif
c
c     Format statements.
c
 105  format (1x,'Memory for terrain cannot be allocated in ',
     &           'subroutine get_topo.')
 110  format (1x,'There was an error when getting terrain ',
     &           'cross section in routine get_topo.')
c     
c
      return
      end
