#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine gettraj (slab1, slab2, slab3, slab4, topoad, 
     &     topopt,  subslab, nf, ni, nj, nter, error)
c
c     $Id: gettraj.F,v 1.11 2007-01-09 18:47:12 warren Exp $
c
c     This routine gets the data for calculating trajectories.
c
c     $Log: gettraj.F,v $
c     Revision 1.11  2007-01-09 18:47:12  warren
c     Require all three wind components for traj, and add additional diagnostic print in maptrn
c
c     Revision 1.10  2002/05/22 17:56:45  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.9  1999/12/22 00:33:17  warren
c     Fixed Dales crashing bug when changing from point slicer to normal. Also
c     have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c     Revision 1.8  1999/03/01 21:19:29  warren
c     Fixed trajectory code to set the correct attributes at plot time instead of
c     at traj componant selection time. - now to do vectors.
c
c     Revision 1.7  1999/01/29 20:46:30  warren
c     ets the flag for time independence and the plot the trajectories
c
c     Revision 1.6  1998/07/28 18:22:27  harryive
c     New fixes for trajectories when they are the first overlay.
c     Also, remove old Yuri routine.
c
c     Revision 1.5  1998/03/04 23:47:41  warren
c     Removed multiple definitions per Digital's compiler
c
c     Revision 1.4  1997/10/30 23:07:50  davidive
c     convert traj to c to dynamically allocate memory, also place trajectory
c     inside black lines one each side.
c
c     Revision 1.3  1997/02/21 20:02:09  harry
c     Fix vector interval to be floating point and have values less than one.
c     Handling of vector interval is now in the slicer.
c
c     Revision 1.2  1997/02/12 17:36:03  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:14  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      include 'data.icl'
      include 'message.icl'
      include 'plotops.icl'
      include 'topo.icl'
      include 'traj.icl'
      include 'units.icl'
c     
c     Argument declarations
c     
      integer         nf, ni, nj, nter
      ive_ptr         topoad, topopt, slab1, slab2, slab3, slab4,
     &                subslab
      logical         error
c     
c     
c     Local valiable declarations.
c     
      integer   nt, i, nx, ny, nz, freeit(3)
      integer  udim(4),vdim(4), wdim(4)
      integer             dims, ibeg, iend, j, ndims, 
     &                    vdims, nvdims, ivars
      ive_ptr   ptru, ptrv, ptrw, getvar
      logical   savflg,new
      real      pt1, pt3, special1
      real                datmax, datmin, misdat,  vdatmax, 
     &                    vdatmin, vmisdat, vtstag
      character *(80)     units, dunits, vunits, vdunits,
     &                    dnames, vdnames
      dimension pt1(MAXDIM+1), pt3(MAXDIM)
      dimension dims(MAXDIM), datmax(MAXDIM), datmin(MAXDIM), 
     &          vdims(MAXDIM), 
     &          vdatmax(MAXDIM), vdatmin(MAXDIM), 
     &          dnames(MAXDIM), vdnames(MAXDIM),
     &          vtstag(MAXDIM)
      dimension ivars(MAXVAR)
      
c
c
c     External function declarations.
c
      ive_ptr   getmem
      integer   strbeg, strend, nblank, traj, traj_ind
c
c
c     Common blocks used by set2d.
c
      real                dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
      common / voffsets / dh, xbeg, xend, ybeg, yend, zbeg, zend, 
     &                    tbeg, tend
c
c
c
c     
      error = .false.
      slab1 = 0
      slab2 = 0
      subslab=0
      freeit(1)=0
      freeit(2)=0
      freeit(3)=0
      if(traj_component(1) .eq. ' ') then
         write (message, 180)
         call write_message
         error = .true.
         goto 10
      endif
      if(traj_component(2) .eq. ' ') then
         write (message, 180)
         call write_message
         error = .true.
         goto 10
      endif
      if(traj_component(3) .eq. ' ' .and. .not. traj_2d) then
         write (message, 180)
         call write_message
         error = .true.
         goto 10
      endif
         
c     
c     
c     Get the value of the save flag.
c     
      call getlvar ('savflg', savflg, error)
      if ( error )  return
c     
c     Get a ive_ptr to the three trajectory components, and save the
c     staggering and dimension information, since it aint the same 
c     for each varible.
c
c	write(6,*) 'firster in traj', stag

c      call get_pointer(traj_component(1)(strbeg(traj_component(1)):
c     &     strend(traj_component(1))), ptru, nx, ny, nz, nt)
      
      new = .false.
      do j = 1, MAXDIM
         vdims(j) = 1
         vtstag(j) = 0.0
         vdatmin(j) = 0.0
         vdatmax(j) = 0.0
      enddo
      vmisdat = 0.0
      vunits(1:len(vunits)) = ' '
      vdunits(1:len(vdunits)) = ' '
      
 
      if(nblank(traj_component(1)) .ne. 0) then
         ptru = getvar(traj_component(1)(strbeg(traj_component(1)):
     &        strend(traj_component(1))),nvdims, vdims, 
     &        vtstag, vdatmin, vdatmax, vmisdat, 
     &        vunits, vdunits, vdnames, new)
         if ( ptru .le. 0 ) then
            write (message, 150)
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
            nx=vdims(1)
            ny=vdims(2)
            nz=vdims(3)
            nt=vdims(4)
            ndims = nvdims
            misdat = vmisdat
            units(1:len(units)) = vunits(strbeg(vunits):
     &           strend(vunits))
            dunits(1:len(dunits)) = vdunits(strbeg(vdunits):
     &           strend(vdunits))
            call load_atts (ndims, dims, stag, datmin, datmax, 
     &           misdat, units, dunits, dnames)
            
            udim(1) = dims(1)
            udim(2) = dims(2)
            udim(3) = dims(3)
            udim(4) = dims(4)
         endif
         if(nblank(traj_component(2)) .ne. 0) then
            call get_pointer(traj_component(2)(strbeg(traj_component(2))
     &           :strend(traj_component(2))), ptrv, nx, ny, nz, nt)
            if ( ptrv .le. 0 ) then
               ptrv = getmem (nx*ny*nz*nt)
               if(ptrv .eq. 0) then
                  write (message, 150)
                  call write_message
                  error = .true.
                  goto 10
               endif
               freeit(2)=1
               call fill_array (%val(ptrv),nx*ny*nz*nt, 0.0)
            endif
         else
            ptrv = getmem (nx*ny*nz*nt)
            if(ptrv .eq. 0) then
               write (message, 150)
               call write_message
               error = .true.
               goto 10
            endif
            freeit(2)=1
            call fill_array (%val(ptrv),nx*ny*nz*nt, 0.0)
         endif
      else
      if(nblank(traj_component(2)) .ne. 0) then
         ptrv = getvar(traj_component(2)(strbeg(traj_component(2)):
     &        strend(traj_component(2))),nvdims, vdims, 
     &        vtstag, vdatmin, vdatmax, vmisdat, 
     &        vunits, vdunits, vdnames, new)
         if ( ptrv .le. 0 ) then
            write (message, 150)
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
            nx=vdims(1)
            ny=vdims(2)
            nz=vdims(3)
            nt=vdims(4)

            ndims = nvdims
            misdat = vmisdat
            units(1:len(units)) = vunits(strbeg(vunits):
     &           strend(vunits))
            dunits(1:len(dunits)) = vdunits(strbeg(vdunits):
     &           strend(vdunits))
            call load_atts (ndims, dims, stag, datmin, datmax, 
     &           misdat, units, dunits, dnames)
            
            udim(1) = dims(1)
            udim(2) = dims(2)
            udim(3) = dims(3)
            udim(4) = dims(4)
         end if
         ptru = getmem (nx*ny*nz*nt)
         if(ptrv .eq. 0) then
            write (message, 150)
            call write_message
            error = .true.
            goto 10
         endif
         freeit(1)=1
         call fill_array (%val(ptrv),nx*ny*nz*nt, 0.0)
      endif
      
      endif
      call getrvar ('spval', special1, error)
      if ( error ) then
         write (message, 160) char(10)
         call write_message
         special1=10e31
      endif
      
      vdim(1) = nx
      vdim(2) = ny
      vdim(3) = nz
      vdim(4) = nt
      
      if(.not. traj_2d .and. (nblank(traj_component(3)) .ne. 0)) then
         call get_pointer(traj_component(3)(strbeg(traj_component(3)):
     &        strend(traj_component(3))), ptrw, nx, ny, nz, nt)
         if ( ptrw .le. 0 ) then
            ptrw = getmem (nx*ny*nz*nt)
            if(ptrw .eq. 0) then
               write (message, 150)
               call write_message
               error = .true.
               goto 10
            endif
            freeit(3)=1
            call fill_array (%val(ptrw),nx*ny*nz*nt, 0.0)
         end if
      else
         ptrw = getmem (nx*ny*nz*nt)
         if(ptrw .eq. 0) then
            write (message, 150)
            call write_message
            error = .true.
            goto 10
         endif
         freeit(3)=1
         call fill_array (%val(ptrw),nx*ny*nz*nt, 0.0)
      end if
      
      wdim(1) = nx
      wdim(2) = ny
      wdim(3) = nz
      wdim(4) = nt	      
c     
      
      if (time_ind) then
         nf = traj_ind(%val(ptru),%val(ptrv),%val(ptrw), slab1,
     &        slab2, slab3, slab4, subslab, 
     &        udim(1),udim(2), udim(3), udim(4),
     &        vdim(1),vdim(2), vdim(3), vdim(4),
     &        wdim(1),wdim(2), wdim(3), wdim(4))
      else
         nf = traj(%val(ptru),%val(ptrv),%val(ptrw), slab1,
     &        slab2, slab3, slab4, subslab,
     &        udim(1),udim(2), udim(3), udim(4),
     &        vdim(1),vdim(2), vdim(3), vdim(4),
     &        wdim(1),wdim(2), wdim(3), wdim(4))
      endif
      if(nf .eq. 0) then
         error =	.true.
c     message already sent
         goto 10
      endif
c     
c     used to set attributes, use w since that is what is stored in
c     the attributes in settraj.
c     
      call get_pointer (att_var_traj(strbeg(att_var_traj)
     &     :strend(att_var_traj)),
     &     ptrw, wdim(1), wdim(2), wdim(3), wdim(4))
c     
c     
c     Set some factors needed by set2d.
c     
      xbeg = plwmin(1)
      ybeg = plwmin(2)
      zbeg = plwmin(3)
      tbeg = plwmin(4)
      xend = plwmax(1)
      yend = plwmax(2)
      zend = plwmax(3)
      tend = plwmax(4)

c     
c
c
c
c     It is necessary to call get_pointer with the current field name. 
c     This must be done so that settrcom is set up with the attributes 
c     for the current field. If this is not done, and the next plot is 
c     not a trajor plot, errors in plotting result. 
c
      call get_pointer (field, ptrw, 
     &            wdim(1), wdim(2), wdim(3), wdim(4))
c
c
c
      if ((.not. error) .and. (.not. savflg)) then
         ni = topo_ni
         nj = topo_nj
         do i = 1, MAXDIM
            pt1(i) = topo_pt1(i)
            pt3(i) = topo_pt3(i)
         enddo
         call get_topo (topoad, topopt, ni, nj, pt1, pt3, MAXDIM, 
     &                  nter, error)
      endif
      if ( freeit(1) .eq. 1 ) call freemem(ptru)
      if ( freeit(2) .eq. 1 ) call freemem(ptrv)
      if ( freeit(3) .eq. 1 ) call freemem(ptrw)

   10 continue
c
c     Format statements.
c
 100  format (1x,'Memory for slabs cannot be allocated in ',
     &           'subroutine gettraj.')
 105  format (1x,'Memory for terrain cannot be allocated in ',
     &           'subroutine gettraj.')
 110  format (1x,'There was an error in calculating the  ',
     &           'cross section in routine gettraj.')
 120  format (1x,'You must specify trajectory slices using ',
     &           'XLOCATION, YLOCATION, ZLOCATION and TIME')
 130  format (1x,'Trajectory components were not specified.')
 150  format (1x,'Can not find trajectory components.')
 160  format (1x,'Can not get special values in gettraj.')
 170  format (1x,'Time needs to be fixed for trajectories.')	
 180  format (1x,'Trajectories require all three wind componants.')	
c
c
      return
      end

