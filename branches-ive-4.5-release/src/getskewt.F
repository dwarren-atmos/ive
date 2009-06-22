#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif            
      subroutine getskewt(error)
c
c     $Id: getskewt.F,v 1.4 2000/02/11 23:14:35 harry Exp $
c
c     This routine gets the data for the SKEW T plot and calls
c     makskewt.
c
c     $Log: getskewt.F,v $
c     Revision 1.4  2000/02/11 23:14:35  harry
c     Allow surface 1d plots.  Fix Z slider for surface plots.
c
c     Revision 1.3  1999/02/18 20:50:48  warren
c     declare slope and intercept as double precision
c
c     Revision 1.2  1997/02/12 17:36:01  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:13  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      include 'message.icl'
      include 'data.icl'
      include 'skewt.icl'
      include 'units.icl'
      include 'plotops.icl'


c     local variables
      integer nx,ny,nz,nt, ni, convert_units
      character*(UNIT_LEN) skewt_units(3)
      real special, pt1(MAXDIM), pt2(MAXDIM), dh
      double precision slope, intercept
      ive_ptr ptr_1, ptr_2,ptr_3, slabad1, slabloc1
      ive_ptr slabad2, slabloc2, slabad3, slabloc3
      logical error

c     external functions
      integer strbeg, strend, nblank
      ive_ptr slicer1d, getmem
      
      error = .false.

c     check that things are properly set up to draw a  line plot
      call chklineplt(error)
      if (error) go to 10

c     make sure that the z variable free one
      if (lock(3).ne.0) then
         write(message, 100)
         call write_message
         error = .true.
         go to 10
      endif

c     get the ive_ptrs for each of the fields
      ptr_1 = 0
      ptr_2 = 0
      ptr_3 = 0

c     get the slab ive_ptrs according to the information on the sliders
      pt1(1) = xloc
      pt2(1) = xloc
      pt1(2) = yloc
      pt2(2) = yloc
      pt1(3) = plwmin(3)
      pt2(3) = plwmax(3)
      pt1(4) = tloc
      pt2(4) = tloc
      slabad1 = 0
      slabad2 = 0
      slabad3 = 0
      slabloc1 = 0
      slabloc2 = 0
      slabloc3 = 0

      if (nblank(skewt_component(1)) .ne. 0) then
         call get_pointer(skewt_component(1)(strbeg(skewt_component(1)):
     &        strend(skewt_component(1))),ptr_1,nx,ny,nz,nt)
         call getrvar ('spval', special, error)
         skewt_units(1) = data_display_units
         slabloc1 = slicer1d(%val(ptr_1),
     &        nx, ny, nz, nt, 
     &        MAXDIM, pt1, pt2, slabad1, ni, special, 0)
         if (ptr_1.le.0) then
            write(message, 120)
            call write_message
            error = .true.
            go to 10 
         else   
            if (skewt_component(1).eq.'PM') then 
               pt1(4) = plmin(4)
               pt2(4) = plmin(4)
            else
               pt1(4) = tloc
               pt2(4) = tloc
            end if
            ni = nz
         end if
         pt1(4) = tloc
         pt2(4) = tloc
      else
         skewt_units(1) = domain_units(3)
      endif

c    since the last 2 arrays  may not have a value associated with
c    them check and, if there is no value, fill the array with zeros.

      if(nblank(skewt_component(2)) .gt. 0) then
         call get_pointer(skewt_component(2)(strbeg(skewt_component(2)):
     &        strend(skewt_component(2))),ptr_2,nx,ny,nz,nt)
         skewt_units(2) = data_display_units
         if (ptr_2.le.0) then
            slabad2=getmem(ni)
            call fill_array (%val(slabad2), ni, special)
         else
            call getrvar ('spval', special, error)
            slabloc2 = slicer1d(%val(ptr_2), nx, 
     &           ny, nz, nt, MAXDIM, pt1, pt2, slabad2, ni, special, 0)
            if (nblank(skewt_component(1)) .eq. 0) then
               slabad1 = slabloc2
            endif
         end if
      else
         slabad2=getmem(ni)
         call fill_array (%val(slabad2), ni, special)
         skewt_units(2) = 'degreeC' // char(0)
      endif


      if(nblank(skewt_component(3)) .gt. 0) then
         call get_pointer(skewt_component(3)(strbeg(skewt_component(3)):
     &        strend(skewt_component(3))),ptr_3,nx,ny,nz,nt)
         skewt_units(3) = data_display_units
         if (ptr_3.le.0) then
            slabad3=getmem(ni)
            call fill_array (%val(slabad3), ni, special)
         else
            slabloc3 = slicer1d(%val(ptr_3), nx, 
     &           ny, nz, nt, MAXDIM, pt1, pt2, slabad3, ni, special, 0)
            if (nblank(skewt_component(1)) .eq. 0) then
               slabad1 = slabloc3
            endif
         end if
      else
         slabad3=getmem(ni)
         call fill_array (%val(slabad3), ni, special)
         skewt_units(3) = 'degreeC' // char(0)
      end if
      if (slabad1 .eq. 0) then
         write(message, 120)
         call write_message
         error = .true.
         goto 10
      endif
c
c     Change units to millibars and degrees C
c
      if (convert_units(skewt_units(1), 'millibar'//char(0),
     &     slope, intercept).eq.0) then
         call scale(%val(slabad1), nz, slope, intercept, special)
      else
         write(message, 130)
         call write_message
         error = .true.
         goto 10
      endif
      if (convert_units(skewt_units(2), 'degreeC'//char(0),
     &     slope, intercept).eq.0) then
         call scale(%val(slabad2), nz, slope, intercept, special)
      else
         write(message, 140)
         call write_message
         error = .true.
         goto 10
      endif
      if (convert_units(skewt_units(3), 'degreeC'//char(0),
     &     slope, intercept).eq.0) then
         call scale(%val(slabad3), nz, slope, intercept, special)
      else
         write(message, 140)
         call write_message
         error = .true.
         goto 10
      endif
      call makskewt(%val(slabad1),%val(slabad2),%val(slabad3), nz
     &     , special)

c     clean things up and exit.

 10   continue

      call get_pointer (field, ptr_1, nx, ny, nz, nt)
      if (slabad1.gt.0) call freemem(slabad1)
      if (slabad2.gt.0) call freemem(slabad2)
      if (slabad3.gt.0) call freemem(slabad3)
      if (slabloc1.gt.0) call freemem(slabloc1)
      if (slabloc2.gt.0) call freemem(slabloc2)
      if (slabloc3.gt.0) call freemem(slabloc3)

 100  format (1x, 'Z must be free to make a Skew-T Plot')
 120  format (1x, 'You must specify the first variable as a' 
     &     ' pressure co-ordinate')
 130  format (1x, 'Pressure units incompatible with millibars')
 140  format (1x, 'Temperature units incompatible with degreeC')
      return 
      end
