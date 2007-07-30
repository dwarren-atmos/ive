#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine getlineplt (datarr, slabad, slabloc, topoad, topopt, 
     &                       nx, ny, nz, nt, ni, nter, error)
c-----------------------------------------------------------------------
c     $Id: getlineplt.F,v 1.23 2000/02/11 23:14:34 harry Exp $
c     Purpose:
c        This routine is the top-level routine which produces line 
c        plots.
c     Arguments:
c        datarr  real  input   the data array from which 2D slices will
c                              be extracted.
c        slabad  real  output  the array that contains the 2D slice 
c                              to be contoured.
c        nx      int   input   the first dimension of datarr.
c        ny      int   input   the second dimension of datarr.
c        nz      int   input   the third dimension of datarr.
c        nt      int   input   the fourth dimension of datarr.
c        ni      int   output  the number of points in the windowed 
c                              domain along the first slice dimension.
c        error   logi  output  indicates possible errors found in this
c                              routine or the routines further down in
c                              the call sequence.
c     History:
c        $Log: getlineplt.F,v $
c        Revision 1.23  2000/02/11 23:14:34  harry
c        Allow surface 1d plots.  Fix Z slider for surface plots.
c
c        Revision 1.22  1998/09/17 19:59:09  harry
c        Remove yuri routines "getaverage" and "compute average".
c
c        Revision 1.21  1998/09/03 19:19:21  harry
c        Add in 2d line plots - correct bugs in other averaging.
c
c        Revision 1.20  1997/06/27 21:40:12  davidive
c        Fix arbitrary slice line labeling
c
c        Revision 1.19  1997/06/05 15:55:33  harry
c        Fixes to point slicing.
c
c        Revision 1.18  1997/05/16 22:15:16  harry
c        More mouse slicing fixes.
c
c        Revision 1.17  1997/02/27 16:22:45  warren
c        put lock_int in common block where it belongs and changed maxval and minval
c        to ive_maxval and ive_minval to not interfere with the f90 intrinsic.
c        Also changed maxval in uwperim to ive_maxval_per to distinguish the
c        variable from the function.
c
c        Revision 1.16  1997/02/12 17:35:59  warren
c        Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c        Revision 1.15  1996/11/22 18:49:43  warren
c        fixed crashes related to point slicing.
c
c        Revision 1.14  1996/10/16 15:09:45  warren
c        remove write statement
c
c        Revision 1.13  1996/10/11 01:12:57  warren
c        To fill or not to fill is no longer the only question. Now there is a seperate
c        question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c        Labels are not fixed yet.
c
c        Revision 1.12  1996/02/24 00:30:24  harry
c        No terrain on average plots.
c
c Revision 1.11  1995/09/05  20:33:56  harry
c Add new slicer.  Good luck!
c
c Revision 1.10  1994/11/23  19:49:37  harry
c Fix for plots where Y axis has a higher value at the bottom versus the top.
c
c Revision 1.9  1994/06/11  21:33:23  yuri
c Added more comments.
c
c Revision 1.8  1994/04/19  23:03:04  yuri
c Big 1D flip commit.
c
c Revision 1.7  1994/04/14  23:28:03  harry
c Fix previous mod to allow a horizontal line plot.
c
c Revision 1.6  1994/04/13  23:34:26  harry
c Print error message if no data at selected slice.
c
c Revision 1.5  1994/03/30  22:47:15  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.4  1994/03/30  22:19:40  harry
c Change the way 1D plots handle user specified units.  The data is
c scaled instead of trying to scale the labels.  This is the same way 2D
c plots are handled.
c
c Revision 1.3  1994/02/16  17:00:35  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.2  1993/12/13  23:47:52  warren
c fixed line plots so the box get drawn around them. also added a proof option
c to the makefile.
c
c Revision 1.1  1993/11/29  19:39:23  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.24  1993/10/28  18:14:42  yuri
c Typo correction propagated to IVE-3-1-? version.
c
c Revision 1.23  1993/10/18  23:07:27  warren
c updates from 3.0 version.
c
c Revision 1.22  1993/10/15  19:05:38  yuri
c Accuracy improvements propagated in ive ver 3.1
c
c Revision 1.21  1993/10/13  00:04:16  warren
c Update current version for fixes to 3.0 version related to the meaning of
c default in the property sheet boxes.
c
c Revision 1.20  1993/10/12  21:02:31  yuri
c Changed arithmetic averaging to trapezoidal rule.
c
c Revision 1.19  1993/10/07  17:43:11  yuri
c Propagating of tune-up correction into IVE-1-? version.
c
c Revision 1.18  1993/10/06  19:28:35  yuri
c Little typo correction.
c
c Revision 1.17  1993/10/06  19:19:22  yuri
c Solurion for interpolation coefficients problem propagated.
c
c Revision 1.16  1993/09/14  23:06:08  yuri
c Some typos corrected
c
c Revision 1.15  1993/08/18  15:47:30  harry
c Change to new transform routines.
c
c Revision 1.14  1993/07/15  22:13:13  warren
c Fixed scaling for default 1d ticmarks. Also fixed plotting under terrain
c problem.
c
c Revision 1.13  1993/07/12  21:24:19  warren
c Fixed default ticmark choosing routine to deal with screwy scales.
c
c Revision 1.12  1993/07/06  18:21:10  yuri
c Corrected problem with terrain 1D plot.
c
c Revision 1.11  1993/06/23  21:04:15  yuri
c Added case for 1D/2D averaging with time in it.
c
c Revision 1.10  1993/06/22  22:09:49  yuri
c 	New 1D and 2D slicers installed, added command interp_factors
c so user could choose number of interpolation points in slicing.
c Primarily it's for averaging, but user could use it every time
c before call slicers.
c
c Revision 1.9  1993/06/21  17:10:54  yuri
c interface with new slicer corrected.
c
c Revision 1.8  1993/06/21  04:47:59  yuri
c case with f=f(z) corrected.
c
c Revision 1.7  1993/06/18  22:42:09  yuri
c 	Added case for constant field.
c
c Revision 1.5  1993/06/01  19:52:15  yuri
c Second attempt.
c
c Revision 1.3  1993/05/24  00:44:10  yuri
c Unusable variables deleted.
c
c Revision 1.2  1993/05/23  23:56:51  yuri
c New 1D slicer instead of lslicer.
c
c Revision 1.1.1.1  1993/04/07  19:39:58  harry
c Initial CVS checkin of IVE.
c
c Revision 2.11  1993/02/23  20:44:47  warren
c make sure memory is allocates before freeing
c
c Revision 2.10  1993/02/01  23:45:02  warren
c switched to use the regular tick mark vars as per property sheets.
c
c Revision 2.9  1992/12/10  21:15:11  millerp
c Add code to produce nicer line plots using variable data.
c
c Revision 2.8  1992/12/07  23:55:45  millerp
c Reset xaxis and yaxis correctly after leaving pslicer.
c
c Revision 2.7  1992/11/06  17:53:55  millerp
c Fixed up invalid comment line.
c
c Revision 2.6  1992/11/06  17:42:05  millerp
c Set x1, x2, y1, y2 correctly.
c
c Revision 2.5  1992/11/05  20:30:28  millerp
c Added stuff for averages and time-dependent cross sections.
c
c Revision 2.4  1992/10/30  20:18:26  warren
c fixed multiple declaration of dh
c
c Revision 2.3  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 2.2  1992/09/30  15:23:31  warren
c removed spurious writes
c
c Revision 2.1  1992/09/29  20:52:50  warren
c complete rewrite to work with physical space line slicer
c
c Revision 2.0  1992/07/01  21:13:17  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'plotops.icl'
      include 'data.icl'
      include 'pltwin.icl'
      include 'message.icl'
      include 'units.icl'
      include 'lineplt.icl'
c
c     Argument declarations
c
      integer         ni, ! Number of points in slice
     &	nx, ny, nz, nt, nter
c
      ive_ptr slabad, 	! Dependent variable slices
     &	      slabloc, ! Independent variable slice
     & topoad, topopt   ! Same stuff for topology
c
c
c
      logical         error
      real            datarr
      dimension       datarr(nx, ny, nz, nt)
c
c     Local valiable declarations.
c
      integer         i, j, num_avg, da, da2(2)
      logical         savflg
      real            special, pt1, pt2, ticint,
     &                dmin, dmax, tx1,tx2,ty1,ty2
       dimension       pt1(4), pt2(4)
      real ptm1(4)
c
c     External function declarations.
c
      ive_ptr         getmem, slicer1d, slicer1d1a, slicer1d2a,
     &                xter, yter
c
c     Common blocks used by cpmpxy.
c
      real                xofset, yofset
      common / hoffsets / xofset, yofset
c
      real                dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      common / voffsets / dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
c---------------------------------------------------------
c
c 	 Initialization
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
c     Get the value of sclicer factors
c
c     Get cross sections.
c
      if (use_buttons .ne. 0) then
c     
c        Get the cross section.
c     
         if(lock(1) .eq. 1) then
            pt1(1) = xloc
            pt2(1) = xloc
         else
            pt1(1) = plwmin(1)
            pt2(1) = plwmax(1)
         endif
         if(lock(2) .eq. 1) then
            pt1(2) = yloc
            pt2(2) = yloc
         else
            pt1(2) = plwmin(2)
            pt2(2) = plwmax(2)
         endif
         if(lock(3) .eq. 1) then
            pt1(3) = zloc
            pt2(3) = zloc
         else
            pt1(3) = plwmin(3)
            pt2(3) = plwmax(3)
         endif
         if(lock(4) .eq. 1) then
            pt1(4) = tloc
            pt2(4) = tloc
         else
            pt1(4) = plwmin(4)
            pt2(4) = plwmax(4)
         endif
c
         if (num_avg.eq.0) then
            slabloc = slicer1d(datarr, nx, ny, nz, nt, MAXDIM,
     &           pt1, pt2, slabad, ni, special, surface)
         else if (num_avg.eq.1) then
            do i = 1, 4
               if (lock(i).eq.2) da = i
            enddo
            slabloc = slicer1d1a(datarr, nx, ny, nz, nt, MAXDIM,
     &           pt1, pt2, da, slabad, ni, special, surface)
         else if (num_avg.eq.2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da2(j) = i
                  j = j + 1
               endif
            enddo
            slabloc = slicer1d2a(datarr, nx, ny, nz, nt, MAXDIM,
     &           pt1, pt2, da2, slabad, ni, special, surface)
         endif
c     
c        Set some factors needed by cpmpxy.
c     
         xbeg = pt1(1)
         xend = pt2(1)
         ybeg = pt1(2)
         yend = pt2(2)
         zbeg = pt1(3)
         zend = pt2(3)
         tbeg = pt1(4)
         tend = pt2(4)
c     
c     
      else
c
c     We are using points instead of slicer.
c
         call getrarr('point_1', pt1, 4, error)
         if(error) goto 10
         call getrarr('point_2', pt2, 4, error)
         if(error) goto 10
         if(ive_plock(1) .eq. 1) then
            tx1 = pt1(1)
            tx2 = pt2(1)
            xdim_lab = 1
            if(ive_plock(2) .eq. 1) then
               ty1=pt1(2)
               ty2=pt2(2)
               ydim_lab = 2
            elseif(ive_plock(3) .eq. 1) then
               ty1=pt1(3)
               ty2=pt2(3)
               ydim_lab = 3
            elseif(ive_plock(4) .eq. 1) then
               ty1=pt1(4)
               ty2=pt2(4)
               ydim_lab = 4
            endif
         elseif(ive_plock(2) .eq. 1) then
            tx1 = pt1(2)
            tx2 = pt2(2)
            xdim_lab = 2
            if(ive_plock(3) .eq. 1) then
               ty1=pt1(3)
               ty2=pt2(3)
               ydim_lab = 3
            elseif(ive_plock(4) .eq. 1) then
               ty1=pt1(4)
               ty2=pt2(4)
               ydim_lab = 4
            endif
         elseif(ive_plock(3) .eq. 1) then
            tx1 = pt1(3)
            tx2 = pt2(3)
            xdim_lab = 3
            if(ive_plock(4) .eq. 1) then
               ty1=pt1(4)
               ty2=pt2(4)
               ydim_lab = 4
            endif
         endif
         slice_slope = (ty2 - ty1)/(tx2 -tx1)
         slice_inter=ty1 - (slice_slope * tx1)
         if (num_avg.eq.0) then
            slabloc = slicer1d(datarr, nx, ny, nz, nt, MAXDIM,
     &           pt1, pt2, slabad, ni, special, surface)
         else if (num_avg.eq.1) then
            do i = 1, 4
               if (lock(i).eq.2) da = i
            enddo
            slabloc = slicer1d1a(datarr, nx, ny, nz, nt, MAXDIM,
     &           pt1, pt2, da, slabad, ni, special, surface)
         else if (num_avg.eq.2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da2(j) = i
                  j = j + 1
               endif
            enddo
            slabloc = slicer1d2a(datarr, nx, ny, nz, nt, MAXDIM,
     &           pt1, pt2, da2, slabad, ni, special, surface)
         endif
      endif
      if (slabloc .eq. 0) then
         error = .true.
         return
      endif
c
c     Get terrain
c
      if (num_avg.eq.0 .and. pt1(3) .eq. pt2(3) .and.
     &     (pt1(4) .eq. pt2(4) ) ) then
         call vert_ter_trans (xter, yter, topoad, nter, pt1, pt2,
     &        plwmin(3))
         if (mod(xaxis,10) .eq. 2) then
            call freemem(xter)
            topopt = yter
         else
            call freemem(yter)
            topopt = xter
         endif
      else
	nter = ni
         topoad = getmem (nter)
         topopt = slabad
         call fill_array (%val(topoad), nter, 0.0)  
      endif
c      
c     -------
C     let's remember limits (they got changed in slicer
      if(use_buttons .eq. 1) then 
         do i=1,4		
            if( plwmax(i)*plwmax_scaled(i) .ne. 0.) then
               ptm1(i) = pt1(i)
               pt1(i) = pt1(i)/plwmax(i)*plwmax_scaled(i)
               pt2(i) = pt2(i)/plwmax(i)*plwmax_scaled(i)
            endif
         enddo
      else
         if(ive_plock(1) .eq. 1) then
            if(ive_plock(2) .eq. 1) then
               if(mod(xaxis,10) .eq. 1) then
                  pt1(1) = x1
                  pt1(2) = (slice_slope*x1) + slice_inter
                  pt2(1) = x2
                  pt2(2) = (slice_slope*x2) + slice_inter
               else
                  pt1(2) = x1
                  pt1(1) = (x1 - slice_inter)/slice_slope
                  pt2(2) = x2
                  pt2(1) = (x2 - slice_inter)/slice_slope
               endif
            elseif(ive_plock(3) .eq. 1) then
               if(mod(xaxis,10) .eq. 1) then
                  pt1(1) = x1
                  pt1(3) = (slice_slope*x1) + slice_inter
                  pt2(1) = x2
                  pt2(3) = (slice_slope*x2) + slice_inter
               else
                  pt1(3) = x1
                  pt1(1) = (x1 - slice_inter)/slice_slope
                  pt2(3) = x2
                  pt2(1) = (x2 - slice_inter)/slice_slope
               endif
            elseif(ive_plock(4) .eq. 1) then
               if(mod(xaxis,10) .eq. 1) then
                  pt1(1) = x1
                  pt1(4) = (slice_slope*x1) + slice_inter
                  pt2(1) = x2
                  pt2(4) = (slice_slope*x2) + slice_inter
               else
                  pt1(4) = x1
                  pt1(1) = (x1 - slice_inter)/slice_slope
                  pt2(4) = x2
                  pt2(1) = (x2 - slice_inter)/slice_slope
               endif
            endif
         elseif(ive_plock(2) .eq. 1) then
            if(ive_plock(3) .eq. 1) then
               if(mod(xaxis,10) .eq. 2) then
                  pt1(2) = x1
                  pt1(3) = (slice_slope*x1) + slice_inter
                  pt2(2) = x2
                  pt2(3) = (slice_slope*x2) + slice_inter
               else
                  pt1(3) = x1
                  pt1(2) = (x1 - slice_inter)/slice_slope
                  pt2(3) = x2
                  pt2(2) = (x2 - slice_inter)/slice_slope
               endif
            elseif(ive_plock(4) .eq. 1) then
               if(mod(xaxis,10) .eq. 2) then
                  pt1(2) = x1
                  pt1(4) = (slice_slope*x1) + slice_inter
                  pt2(2) = x2
                  pt2(4) = (slice_slope*x2) + slice_inter
               else
                  pt1(4) = x1
                  pt1(2) = (x1 - slice_inter)/slice_slope
                  pt2(4) = x2
                  pt2(2) = (x2 - slice_inter)/slice_slope
               endif
            endif
         elseif(ive_plock(3) .eq. 1) then
            if(ive_plock(4) .eq. 1) then
               if(mod(xaxis,10) .eq. 3) then
                  pt1(3) = x1
                  pt1(4) = (slice_slope*x1) + slice_inter
                  pt2(3) = x2
                  pt2(4) = (slice_slope*x2) + slice_inter
               else
                  pt1(4) = x1
                  pt1(3) = (x1 - slice_inter)/slice_slope
                  pt2(4) = x2
                  pt2(3) = (x2 - slice_inter)/slice_slope
               endif
            endif
         endif
         xbeg = pt1(1)
         xend = pt2(1)
         ybeg = pt1(2)
         yend = pt2(2)
         zbeg = pt1(3)
         zend = pt2(3)
         tbeg = pt1(4)
         tend = pt2(4)
      endif
c     
c     Set "nice" default variable axis limits and tic mark intervals.
c     If IVE chooses the tic mark interval, set a flag so it can later 
c     be set to its default value.
c
c      if( abs(y1) .gt. 1e20 .or.  abs(y1) .gt. 1e20 .or.
c     &           abs(y1 - y2) .le. 1e-10) then
      if (abs(y1-y2) .lt. (abs(y1)+abs(y2))*1e-10) then
         write (message, 130)
         call write_message
         error = .true.
         return
      endif
c     
c     DW is saying that this could be a problem with DD's plots
c     
      if (xaxis .eq. 0 .and. abs(x1 - x2) .gt. 1e-10 ) then
         if(abscissa_min .eq. bigone) then
            dmin = x1
         else
            dmin = abscissa_min
         endif
         if(abscissa_max .eq. bigone) then
            dmax = x2
         else
            dmax = abscissa_max
         endif
         call limits (dmin, dmax, x1, x2, ticint)
         if (xmajor .eq. 0.0) then
            xmajor = ticint
            ive_set_xmajorl = .true.
            ive_set_ymajorl = .false.
         else
            ive_set_xmajorl = .false.
            ive_set_ymajorl = .false.
         endif
      elseif (yaxis .eq. 0 .and. abs(y1 - y2) .gt. 1e-10 ) then
         if(ordinate_min .eq. bigone) then
            dmin = y1
         else
            dmin = ordinate_min
         endif
         if(ordinate_max .eq. bigone) then
            dmax = y2
         else
            dmax = ordinate_max
         endif
         call limits (dmin, dmax, y1, y2, ticint)
         if (ymajor .eq. 0.0)  then
            ymajor = ticint
            ive_set_ymajorl = .true.
            ive_set_xmajorl = .false.
         else
            ive_set_ymajorl = .false.
            ive_set_xmajorl = .false.
         endif
      endif
c     
 10   continue
c
c     Format statements.
c     
 100  format (1x,'Error allocating memory in subroutine getlineplt.')
 120  format (1x,'There was an error in calculating the vertical ',
     &     'cross section in routine vcross.')
 130  format (1x,'Slice does not exist in data space, ',
     &  'Please check limits and positions of fixed variables.')
c     
c     
      return
      end
c
      subroutine limits (data_min, data_max, display_min, display_max,
     &                   display_range)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine computes nice limits for scaling line
c        plots.
c     Arguments:
c        data_min      real input   minimum coordinate value in data
c        data_max      real input   maximum coordinate value in data
c        display_min   real output  minimum coordinate axis value
c        display_max   real output  maximum coordinate axis value
c        display_range real output  interval for labeled tic marks
c     History:
c        Written by Dale Durran in Noverber, 1992.
c-----------------------------------------------------------------------
c
c     Argument declarations.
c     
      real        data_min, data_max, display_min, display_max,
     &            display_range
c     
c     Local variable declarations.
c     
      real        bracketed_max, bracketed_min, cushion_max, 
     &            cushion_min, range, pwr_ten, scale_factor, scaled_max,
     &            scaled_min, scaled_range
c     
c     choose "nice" max and min
c     
      range=abs(data_max-data_min)
      if(range . ge. 1.-30)then
         pwr_ten=alog10(range)
      else
         pwr_ten=0.
      endif
      if(pwr_ten.lt.0.) pwr_ten=pwr_ten - 1.0
      pwr_ten=aint(pwr_ten)
      scale_factor=10.**int(pwr_ten)
c     
      scaled_max=data_max/scale_factor
      scaled_min=data_min/scale_factor
c 
      cushion_max=0.
      if(data_max.ge.0.) cushion_max=1.
      cushion_min=0.
      if(data_min.le.0.) cushion_min=-1.
c     
      bracketed_max=aint(scaled_max) + cushion_max
      bracketed_min=aint(scaled_min) + cushion_min
c     
      display_max=bracketed_max * scale_factor
      display_min=bracketed_min * scale_factor
c
c     choose interval for major tics
c
      scaled_range=bracketed_max - bracketed_min
c     
c     eleven is an ugly, but possible result which we increase
c     to twelve in such a way that the limits are even
c     
      if(scaled_range.eq.11.) then
c     
         if(amod(bracketed_max,2.).ne.0.) then
            bracketed_max=bracketed_max+1.
            display_max=bracketed_max * scale_factor
         else
            bracketed_min=bracketed_min-1.
            display_min=bracketed_min * scale_factor
         end if
c     
         scaled_range=12.
      end if
c     
c     now proceed to choose the tic interval
c     
      if( (scaled_range.eq.1.) .or.(scaled_range.eq.5.).or.
     &     (scaled_range.eq.10.) ) then
         display_range=scaled_range/5. *scale_factor
c     
      else if((scaled_range.eq.2.) .or.(scaled_range.eq.4.).or.
     &        (scaled_range.eq.8.).or.(scaled_range.eq.12.) ) then
         display_range=scaled_range/4. *scale_factor
c     
      else if((scaled_range.eq.3.) .or.(scaled_range.eq.6.)) then
         display_range=scaled_range/6. *scale_factor
c     
      else if(scaled_range.eq.7.) then
         display_range=scaled_range/7. *scale_factor
c     
      else if(scaled_range.eq.9.) then
         display_range=scaled_range/3. *scale_factor
c     
      end if

c     
      return
      end
