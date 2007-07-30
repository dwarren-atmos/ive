      subroutine get2dv (slab1, slab2, topoad, topopt, ni, nj, 
     &     nter, error)
c-----------------------------------------------------------------------
c     $Id: get2dv.F,v 1.20 2002-08-09 19:57:57 warren Exp $
c     Purpose:
c        This routine is the top-level routine which produces 2D vector
c        plots.
c     Arguments:
c        slab1  real  output   the array that contains the first vector 
c                              component lying in the slice plane.
c        slab2  real  output   the array that contains the second 
c                              vector component lying in the slice
c                              plane.
c        topoad  real  output  the array containing topography heights
c                              under each point in the slab.
c        topopt  real  output  the array containing the location of
c                              each point in topoad along the slice
c                              (vertical slices).
c        ni      int   output  the number of points in the windowed 
c                              domain along the first slice dimension.
c        nj      int   output  the number of points in the windowed 
c                              domain along the second slice dimension.
c        nter    int   output  the number of points in topoad
c        error   logi  output  indicates possible errors found in this
c                              routine or the routines further down in
c                              the call sequence.
c     History:
c        $Log: get2dv.F,v $
c        Revision 1.20  2002-08-09 19:57:57  warren
c        Fixed for Linux and tested with DEC sun etc.
c        most items inside #ifdef's, or were general cleanup/fixes
c
c        Revision 1.19  2002/05/22 17:56:44  davidive
c        First working version of trajectories colored by alternative fields
c        There is not a complete interface yet!!!
c
c        Revision 1.18  2001/04/11 19:11:03  harryive
c        Add new_field_trans and load_atts calls to the end of getvar.  Remove them
c        from other places where they are now no longer needed.
c
c        Revision 1.17  2000/07/20 18:04:16  harry
c        Fix slicer setup for second component when set to surface.
c
c        Revision 1.16  2000/04/05 22:38:54  harry
c        Correct bugs in general surface plotting.
c
c        Revision 1.15  1999/10/26 17:37:02  harry
c        Add call to new_field_trans in get2dv.  Add line width to vector plots.
c
c        Revision 1.14  1999/06/07 18:33:34  warren
c        Fixed vector setup so it happens just before plotting instead of when loading
c        the fields. This should prevent interactions between vectors and tradjectories.
c
c        Revision 1.13  1998/07/28 18:22:24  harryive
c        New fixes for trajectories when they are the first overlay.
c        Also, remove old Yuri routine.
c
c        Revision 1.12  1998/07/16 22:10:56  harryive
c        Add two dimensional slicing with 2 averaged dimensions.
c
c        Revision 1.11  1997/04/15 17:24:39  harry
c        Remove time_tag - use multiple files instead.
c        Various fixes to vector/scalar looping.
c
c        Revision 1.10  1997/02/21 20:02:09  harry
c        Fix vector interval to be floating point and have values less than one.
c        Handling of vector interval is now in the slicer.
c
c        Revision 1.9  1997/02/12 17:35:55  warren
c        Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c        Revision 1.8  1996/06/21 16:07:38  harry
c        Add new option to slicer (phys = 2) to properly get values for
c        vectors.  By using this option, we get the second component of the
c        vector on the same grid as the first component.
c
c        Revision 1.7  1996/03/27 23:53:58  harry
c        Slicer should check new variable "field_pointer" to find out name of
c        field currently being sliced instead of "field" to properly handle
c        vectors and other future plots (like Skew-T).
c
c Revision 1.6  1996/02/13  20:28:21  warren
c Why can't we do horizontal streamlines - per Curtis
c
c        Revision 1.5  1995/12/18 00:22:50  warren
c        Ultrix doesn't like multiple declarations
c
c Revision 1.4  1995/09/05  20:33:49  harry
c Add new slicer.  Good luck!
c
c Revision 1.3  1995/08/24  22:18:58  harry
c Ive_ptrs is as ive_ptrs does.
c
c Revision 1.2  1994/03/11  22:15:19  harry
c Change order of calls to get_pointer and slicer.  It is important that
c get_pointer for a component be called just before slicer so the
c parameters are properly set for the user's transforms.
c
c Revision 1.1  1993/11/29  19:39:17  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.5  1993/11/04  23:27:39  yuri
c Now user can plot velocity fields as function of time too.
c
c Revision 1.1.1.1  1993/04/07  19:39:56  harry
c Initial CVS checkin of IVE.
c
c Revision 2.6  1992/12/03  00:01:18  millerp
c Add surface vectors.
c
c Revision 2.5  1992/11/18  21:45:46  millerp
c Print error message if user tries to draw streamlines
c on vertical slices.
c
c Revision 2.4  1992/11/05  20:31:25  millerp
c Modified common block. Now get terrain via get_topo.
c
c Revision 2.3  1992/10/23  17:23:25  millerp
c Add call to getptr for the current field.
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:54:26  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:07  millerp
c Released version rereving to 2.0
c
c Revision 1.4  1992/06/19  17:49:08  millerp
c Added time dimension to vector calculations.
c
c Revision 1.3  1992/05/05  15:27:11  millerp
c Added special value check in projvec, and got rid of unused variables.
c
c Revision 1.2  1992/04/21  19:01:30  millerp
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
      include 'vector.icl'
      include 'units.icl'
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
c     
c     Argument declarations
c     
      integer         ni, nj
      ive_ptr         topoad, topopt, slab1, slab2
      logical         error
c     
c     
c     Local valiable declarations.
c     
      integer   i, j, nt, nx, ny, nz, nter, num_avg, da(2)
      ive_ptr   varptr1, varptr2, slicer, slicer2d1a, slicer2d2a
      logical   savflg, streamline
      real      pt1, pt2, pt3, special1, special2
      real      pt1t(MAXDIM), pt2t(MAXDIM)
      dimension pt1(MAXDIM+1), pt2(MAXDIM+1), pt3(MAXDIM)
c
c
c     External function declarations.
c
      integer  udim(4)
      integer             dims, ndims, 
     &                    vdims, nvdims, ivars
      ive_ptr   getmem, getvar
      integer   strbeg, strend
      logical   new
      real                datmax, datmin, misdat,  vdatmax, 
     &                    vdatmin, vmisdat, vtstag
      character *(80)     units, dunits, vunits, vdunits,
     &                    dnames, vdnames
      dimension dims(MAXDIM), datmax(MAXDIM), datmin(MAXDIM), 
     &          vdims(MAXDIM), 
     &          vdatmax(MAXDIM), vdatmin(MAXDIM), 
     &          dnames(MAXDIM), vdnames(MAXDIM),
     &          vtstag(MAXDIM)
      dimension ivars(MAXVAR)
 
c
c
c     Common blocks used by set2d.
c
      real                dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      common / voffsets / dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      real ptm1(4), ptm3(4)
      common /ylims / ptm1,ptm3
c
c
c
c     
      error = .false.
      ni = 0
      nj = 0
      slab1 = 0
      slab2 = 0
c     
c     
c     Get the value of the save flag.
c     
      call getlvar ('savflg', savflg, error)
      if ( error )  return
      call getlvar ('drstrmln', streamline, error)
      if ( error )  return
c      if (streamline) then
c         write (message, 140) char(10)
c         call write_message
c         error = .true.
c         go to 10
c      endif
c     
      call getrvar ('spval', special1, error)
      if ( error ) then
         write (message, 160) char(10)
         call write_message
         go to 10
      endif 
c
      call getrvar ('spval', special2, error)
      if ( error ) then
         write (message, 160) char(10)
         call write_message
         go to 10
      endif 
c
c        Get a ive_ptr to the first vector component.
c
c      call get_pointer(vect_component(1)(strbeg(vect_component(1)):
c     &     strend(vect_component(1))),
c     &     varptr1, nx, ny, nz, nt)
      new = .false.
      do j = 1, MAXDIM
         vdims(j) = 1
         vtstag(j) = 0.0
         vdatmin(j) = 0.0
         vdatmax(j) = 0.0
      enddo
       vmisdat = 0.0
      vunits(1:len(vunits)) = ' '
      varptr1 = getvar(vect_component(1)(strbeg(vect_component(1)):
     &        strend(vect_component(1))),nvdims, vdims, 
     &        vtstag, vdatmin, vdatmax, vmisdat, 
     &        vunits, vdunits, vdnames, new)

      if ( varptr1 .lt. 0 ) then
         write (message, 150) char(10)
         call write_message
         error = .true.
         goto 10
      else 
         call garbage_collect_vars()
         if (new) then
            call make_button_list (ivars,MAXVAR) 
            call make_field_buttons (ivars, %val(0))
         endif
         do j = 1, MAXDIM
            dims(j)  = vdims(j)
            stag(j)  = vtstag(j)
            datmin(j) = vdatmin(j)
            datmax(j) = vdatmax(j)
            dnames(j) = vdnames(j)
         enddo
         j = 0
         nx=vdims(1)
         ny=vdims(2)
         nz=vdims(3)
         nt=vdims(4)
         ndims = nvdims
         misdat = vmisdat
         units(1:len(units)) = vunits(strbeg(vunits):
     &        strend(vunits))
         dunits(1:len(dunits)) = vdunits(strbeg(vdunits):
     &        strend(vdunits))
         
         udim(1) = dims(1)
         udim(2) = dims(2)
         udim(3) = dims(3)
         udim(4) = dims(4)
      endif
      num_avg = 0
      do i = 1, 4
         if (lock(i).eq.2) num_avg = num_avg + 1
      enddo
      if (incx .gt. 0) then
         pt1(5) = 1./incx
      else
         pt1(5) = 1.
      endif
      if (incy .gt. 0) then
         pt2(5) = 1./incy
      else
         pt2(5) = 1.
      endif
c     
c     Get cross sections.
c     
      if (use_buttons .ne. 0) then
c           
c         
c        Get the  cross section for the first vector component.
c        On input to slicer, pt1-pt3
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
c     
c           Calculate the cross-section.
c
         if (num_avg .eq. 0) then
            slab1 = slicer(%val(varptr1), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, nj, ni, special1, 1, surface)
         else if (num_avg .eq. 1) then
            do i = 1, 4
               if (lock(i).eq.2) da(1) = i
            enddo
            slab1 = slicer2d1a(%val(varptr1), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special1,1,surface)
         else if (num_avg .eq. 2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da(j) = i
                  j = j + 1
               endif
            enddo
            slab1 = slicer2d2a(%val(varptr1), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special1, 1)
         endif
         if ( slab1 .le. 0 ) then
            error = .true.
            write (message, 110) 
            call write_message
            go to 10
         endif
c     
      else
c
c     stuff for vert slicer
c
c     z should be fixed, and x/y free/fixed
c
         if (lock(3) .ne. 0) then
            error = .true.
            write (message, 130)
            call write_message
            return
         endif
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
            slab1 = slicer(%val(varptr1), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, nj, ni, special1, 1, surface)
         else if (num_avg .eq. 1) then
            do i = 1, 4
               if (lock(i).eq.2) da(1) = i
            enddo
            slab1 = slicer2d1a(%val(varptr1), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special1,1,surface)
         else if (num_avg .eq. 2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da(j) = i
                  j = j + 1
               endif
            enddo
            slab1 = slicer2d2a(%val(varptr1), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special1, 1)
         endif
         if ( slab1 .le. 0 ) then
            error = .true.
            write (message, 110) 
            call write_message
            go to 10
         endif
      endif
c     
c
c        Now get the cross section for the second 
c        vector component.
c
      call get_pointer (vect_component(2)(strbeg(vect_component(2)):
     &     strend(vect_component(2))),
     &     varptr2, nx, ny, nz, nt)
c
      if ( varptr2 .lt. 0 ) then
         write (message, 150) char(10)
         call write_message
         error = .true.
         goto 10
      endif
      if (use_buttons .ne. 0) then
c           
c         
c     Get second component
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
c     
c           Calculate the cross-section.
c
         if (num_avg .eq. 0) then
            slab2 = slicer(%val(varptr2), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, nj, ni, special2, 2, surface)
         else if (num_avg .eq. 1) then
            do i = 1, 4
               if (lock(i).eq.2) da(1) = i
            enddo
            slab2 = slicer2d1a(%val(varptr2), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special2,2,surface)
         else if (num_avg .eq. 2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da(j) = i
                  j = j + 1
               endif
            enddo
            slab2 = slicer2d2a(%val(varptr2), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special2, 2)
         endif
         if ( slab2 .le. 0 ) then
            error = .true.
            write (message, 110) 
            call write_message
            go to 10
         endif
         if (.not. savflg) then
            call get_topo (topoad, topopt, ni, nj, pt1, pt3, MAXDIM, 
     &           nter, error)
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
c     
      else
c
c     stuff for vert slicer
c
c     z should be fixed, and x/y free/fixed
c
         if (lock(3) .ne. 0) then
            error = .true.
            write (message, 130)
            call write_message
            return
         endif
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
            slab2 = slicer(%val(varptr2), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, nj, ni, special2, 2, surface)
         else if (num_avg .eq. 1) then
            do i = 1, 4
               if (lock(i).eq.2) da(1) = i
            enddo
            slab2 = slicer2d1a(%val(varptr2), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special2,2,surface)
         else if (num_avg .eq. 2) then
            j = 1
            do i = 1, 4
               if (lock(i).eq.2) then
                  da(j) = i
                  j = j + 1
               endif
            enddo
            slab2 = slicer2d2a(%val(varptr2), nx, ny, nz, nt,
     &           MAXDIM, pt1, pt2, pt3, da, nj, ni, special2, 2)
         endif
         if ( slab2 .le. 0 ) then
            error = .true.
            write (message, 110) 
            call write_message
            go to 10
         endif
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
               call getrarr ('point_1', pt1t, 4, error)
               call getrarr ('point_2', pt2t, 4, error)
               call get_topo (topoad, topopt, ni, nj, pt1t, pt2t,
     &              MAXDIM, nter, error)
               topo_ni = ni
               topo_nj = nj
               do i = 1, MAXDIM
                  topo_pt1(i) = pt1(i)
                  topo_pt3(i) = pt3(i)
               enddo
            endif
      endif
c     
c
c        If any component does not exist in the data file, fill the 
c        array with zeroes.
c
         if ( (ni .eq. 0) .and. (nj .eq. 0) ) then
            error = .true.
            write (message, 130)
            call write_message
            go to 10
         endif
         if ( varptr1 .le. 0 ) then
            slab1 = getmem (ni*nj)
            if ( slab1 .le. 0 ) then
               error = .true.
               write (message, 110) 
               call write_message
               go to 10
            else
               call fill_array (%val(slab1), ni*nj, 0.0)
            endif
         endif
c     
         if ( varptr2 .le. 0 ) then
            slab2 = getmem (ni*nj)
            if ( slab2 .le. 0 ) then
               error = .true.
               write (message, 110) 
               call write_message
               go to 10
            else
               call fill_array (%val(slab2), ni*nj, 0.0)
            endif
         endif
c
c
c        Check each component array for special values, and substitute
c        the vector special value (note that special values for each 
c        component are not necessarily equivalent).
c
         call check_spval (%val(slab1), ni*nj, special1, VECT_SPVAL)
         call check_spval (%val(slab2), ni*nj, special2, VECT_SPVAL)
         call get_pointer (att_var(strbeg(att_var):strend(att_var)),
     &                     varptr1, nx, ny, nz, nt)
c     
c     
c        Set some factors needed by set2d.
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
c     
c
      if ( .not. error ) then
         call rem_bound_vect (%val(slab1), %val(slab2), ni, nj, 
     &                        VECT_SPVAL)
      endif
c
c
   10 continue
c
c
c     It is necessary to call get_pointer with the current field name. 
c     This must be done so that settrcom is set up with the attributes 
c     for the current field. If this is not done, and the next plot is 
c     not a vector plot, errors in plotting result. 
c
c     call get_pointer (field, varptr1, nx, ny, nz, nt)
c
c
c     Format statements.
c
 100  format (1x,'Memory for slabs cannot be allocated in ',
     &           'subroutine get2dv.')
 105  format (1x,'Memory for terrain cannot be allocated in ',
     &           'subroutine get2dv.')
 110  format (1x,'There was an error in calculating the  ',
     &           'cross section in routine get2dv.')
 120  format (1x,'You must specify vector slices using ',
     &           'XLOCATION, YLOCATION, ZLOCATION and TIME')
 130  format (1x,'Vector components were not specified.')
 140  format (1x,'At this time, streamlines can be drawn for ',a,
     &           'horizontal slices only.')
 150  format (1x,'Can not find vector components.')
 160  format (1x,'Can not get special values in get2dv.')
c
c
      return
      end
c
c
      subroutine projvec (comp1, comp2, npts, angle, spval)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine finds the projection of the horizontal components
c        of a vector field onto the vertical slice plane. It puts 
c        the result into comp1.
c     Arguments:
c        comp1  real  in/out  the first component of the vector. On 
c                             output this array contains the magnitude
c                             of the horizontal vector components 
c                             parallel to the slice plane.
c        comp2  real  input   the second component of the vector.
c        npts   int   input   number of points in comp1, comp2.
c        angle  real  input   the slice angle.
c        spval  real  input   the special value.
c     History:
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer      npts
      real         comp1, comp2, angle, spval
      dimension comp1(npts), comp2(npts)
c
c
c     Local variable declarations.
c
      integer      i
      real         cosang, sinang
c
c
c
c
      sinang = sin(angle)
      cosang = cos(angle)
c
c
c     Calculate projection of horizontal vector components onto the 
c     slice plane.
c
c
      if ( spval .eq. 0.0 ) then
         do 10 i = 1, npts
            comp1(i) = comp1(i) * sinang + comp2(i) * cosang
 10      continue
      else
         do 20 i = 1, npts
            if ( (comp1(i) .eq. spval) .or. (comp2(i) .eq. spval) ) then
               comp1(i) = spval
            else
               comp1(i) = comp1(i) * sinang + comp2(i) * cosang
            endif
 20      continue
      endif
c
c
      return
      end
c
c
      subroutine get_pointer (varname, ptr, nx, ny, nz, nt)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine gets a ive_ptr to a variable, plus its dimensions.
c     Arguments:
c        varname  char input   the variable name.
c        ptr      int  output  the ive_ptr to the variable. If the 
c                              return value of ptr == 0, then the 
c                              varible could not be found.
c        nx       int  output  the first dimension of the variable 
c                              array.
c        ny       int  output  the second dimension of the variable 
c                              array.
c        nz       int  output  the third dimension of the variable 
c                              array.
c        nt       int  output  the fourth dimension of the variable 
c                              array.
c-----------------------------------------------------------------------
c
      include 'data.icl'
c
c
c     Argument declarations.
c
      character *(*) varname
      ive_ptr         ptr
      integer         nx, ny, nz, nt
c
c
c     Local variable declarations.
c
      integer   beg, end
      logical   error
c
c
c     External function declarations.
c
      integer   nblank, strbeg, strend
c
c
c
c     Set default return values.
c
      ptr = 0
c
c
      beg = strbeg(varname)
      end = strend(varname)
c
      if ( nblank(varname(beg:end)) .gt. 0 ) then
c
c        Get a ive_ptr to the variable, and its dimensions.
c
         call readfld (varname(beg:end), ptr, error)
         if ( error ) then
            return
         endif
         nx = cpmax1
         ny = cpmax2
         nz = cpmax3
         nt = cpmax4
         field_pointer = varname(beg:end)
      endif
c
c
      return
      end
c
c
      subroutine fill_array (array, npts, value)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine fills the input array with value. It is used
c        when arrays are allocated in the calling routine using 
c        the function getmem.
c     Arguments:
c        array  real  in/out  the input array. On output, all elements
c                             of array are equal to value.
c        npts   int   input   the number of points in array.
c        value  real  input   the fill value.
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer      npts
      real         array, value
      dimension array(npts)
c
c
c     Local variable declarations.
c
      integer      i
c
c
c
c
      do 10 i = 1, npts
         array(i) = value
 10   continue
c
c
      return
      end
c
c
      subroutine rem_bound_vect (u, v, ni, nj, spval)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine removes vectors that will point through the plot 
c        boundaries.
c     Arguments:
c        u      real  input  array containing the first vector 
c                            component.
c        v      real  input  array containing the second vector 
c                            component.
c        ni     int   input  the first dimension of u, v.
c        nj     int   input  the second dimension of u, v.
c        spval  real  input  the vector special value.
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer     ni, nj
      real        u, v, spval
      dimension u(ni, nj), v(ni, nj)
c
c
c     Local variable declarations.
c
      integer     i
c
c
c
c
c     Remove vectors that would point through the left or right plot
c     border.
c     
      do 10 i = 1, nj
         if ( u(1, i) .lt. 0.0 ) then
            u(1, i) = spval
            v(1, i) = spval
         endif
         if ( u(ni, i) .gt. 0.0 ) then
            u(ni, i) = spval
            v(ni, i) = spval
         endif
 10   continue
c     
c     Remove vectors that would point through the top plot border. 
c     
      do 20 i = 1, ni
         if ( v(i, nj) .gt. 0.0 ) then
            u(i, nj) = spval
            v(i, nj) = spval
         endif
 20   continue
c
c
      return
      end
c
c
      subroutine check_spval (array, npts, spval, vspval)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine checks each vector component for the special 
c        value for that component. If it finds a special value it resets
c        it to the vector special value, so it is ignored when vectors
c        or streamlines are plotted.
c     Arguments:
c        array   real  input  the vector component array.
c        npts    int   input  the number of points in array.
c        spval   real  input  the special value.
c        vspval  real  input  the special value for vectors.
c-----------------------------------------------------------------------
c
c
c     Argument declarations.
c
      integer         npts
      real            array, spval, vspval
      dimension array(npts)
c
c
c     Local variable declarations.
c
      integer         i
c
c
c
c 
      if ( spval .ne. 0.0 ) then
         do i = 1, npts
            if ( array(i) .eq. spval) then
               array(i) = vspval
            endif
         enddo
      endif
c
c
      return
      end
