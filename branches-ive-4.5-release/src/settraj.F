#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine settraj (error)
c
c     $Id: settraj.F,v 1.4 1998/12/11 19:35:17 warren Exp $
c
c     This routine processes the TRAJECTORY_COMPONENTS command
c
c     TRAJECTORY_COMPONENTS=u,v,w
c
c     where "u,v,w" are the wind components to use the the 
c     trajectory computation.
c
c     $Log: settraj.F,v $
c     Revision 1.4  1998/12/11 19:35:17  warren
c     Fixed the tradjectory plots to go forward to the end time as well as
c     backwards to the beginning (Don't check if returned time is in min to max
c     window any more).
c
c     Revision 1.3  1997/10/22 19:35:08  davidive
c     Turned the particle movement part of trajectories into a user transform
c     and added unit checking to the default transform.
c
c     Revision 1.2  1997/02/12 17:36:34  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:29  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      include 'incommands.icl'
      include 'traj.icl'
      include 'message.icl'
      include 'data.icl'

c
c
c     Argument declarations.
c
      logical             error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval
      character *(80)     component, units, dunits, vunits, vdunits,
     &                    dnames, vdnames
      integer             dims, i, ibeg, iend, j, ndims, num_found, 
     &                    vdims, nvdims, ivars
      ive_ptr             temp_ptr
      logical             loaded, new
      real                datmax, datmin, misdat,  vdatmax, 
     &                    vdatmin, vmisdat, vtstag
      dimension dims(MAXDIM), datmax(MAXDIM), datmin(MAXDIM), 
     &          vdims(MAXDIM), 
     &          vdatmax(MAXDIM), vdatmin(MAXDIM), 
     &          dnames(MAXDIM), vdnames(MAXDIM),
     &          vtstag(MAXDIM), component(3)
      dimension ivars(MAXVAR)
c
c
c     External function declarations.
c
      integer             nblank, strbeg, strend
      ive_ptr             getvar
c
c
c
c
      new = .false.
      error = .false.
      loaded = .false.
      num_found = 0
      units(1:len(units)) = ' '
      dunits(1:len(dunits)) = ' '
      traj_component(1) = ' '
      traj_component(2) = ' '
      traj_component(3) = ' '
c
c
      num_traj_comp = 0
      do 10 i = 1, 3
         curval(1:len(curval)) = comand(FSTVAL+(i-1))(1:CMDLEN)
         ibeg = strbeg (curval)
         iend = strend (curval)
         call upcase (curval(ibeg:iend), (iend - ibeg + 1))
         temp_ptr = 0
         component(i)(1:len(component(i))) = ' '
c     
c        Get a ive_ptr to each variable in the users input list. 
c      
         if ( nblank(curval(ibeg:iend)) .ne. 0 ) then
            component(i)(1:len(component(i))) = curval(ibeg:iend) 
            nvdims = 1
            num_traj_comp = num_traj_comp + 1
            do j = 1, MAXDIM
               vdims(j) = 1
               vtstag(j) = 0.0
               vdatmin(j) = 0.0
               vdatmax(j) = 0.0
            enddo
            vmisdat = 0.0
            vunits(1:len(vunits)) = ' '
            vdunits(1:len(vdunits)) = ' '
            if(curval(ibeg:iend) .ne. '-') then
               temp_ptr = getvar (curval(ibeg:iend),nvdims, vdims, 
     &              vtstag, vdatmin, vdatmax, vmisdat, 
     &              vunits, vdunits, vdnames, new)
c     write(6,*) 'after tmp_ptr', curval(ibeg:iend), nvdims,vdims,
c     &                  vdatmin, vdatmax, new, vtstag
c     
c     Save units
c     
               traj_units(i)=vunits(strbeg(vunits):strend(vunits))
               
               if ( temp_ptr .eq. 0 ) then
                  write (message, 100) curval(ibeg:iend)
                  call write_message
                  error = .true.
                  return
               end if
               call garbage_collect_vars()
               if (new) then
                  call make_button_list (ivars,MAXVAR) 
                  call make_field_buttons (ivars, %val(0))
               endif
               num_found = num_found + 1
            else
               traj_component(i) = ' '
               call garbage_collect_vars()
            endif
         endif
 10   continue
c     if ( .not. loaded ) then
c     loaded = .true.
c     
c     The attributes for traj are set equal to the 
c     attributes of the last varible found in the data
c     file, i.e. w.
c     
      att_var_traj(1:len(att_var_traj)) =
     &     curval(ibeg:iend)
      do j = 1, MAXDIM
         dims(j)  = vdims(j)
         stag(j)  = vtstag(j)
         datmin(j) = vdatmin(j)
         datmax(j) = vdatmax(j)
         dnames(j) = vdnames(j)
      enddo
      ndims = nvdims
      misdat = vmisdat
      units(1:len(units)) = vunits(strbeg(vunits):
     &     strend(vunits))
      dunits(1:len(dunits)) = vdunits(strbeg(vdunits):
     &     strend(vdunits))
      call load_atts (ndims, dims, stag, datmin, datmax, 
     &     misdat, units, dunits, dnames)
c     endif
c     
c
      if ( num_found .gt. 0 ) then
c
         if(nblank(component(1)) .ne. 0) then
            traj_component(1) = ' '		
            traj_component(1)(1:len(traj_component(1))) = 
     &           component(1)(strbeg(component(1)):
     &           strend(component(1)))
         endif	
         if(nblank(component(2)) .ne. 0) then
            traj_component(2) = ' '
            traj_component(2)(1:len(traj_component(2))) = 
     &           component(2)(strbeg(component(2)):
     &           strend(component(2)))
         endif
         if(nblank(component(3)) .ne. 0) then
            traj_component(3) = ' '
            traj_component(3)(1:len(traj_component(3))) = 
     &           component(3)(strbeg(component(3)):
     &           strend(component(3)))
         endif

      endif	
c      write (6,*) traj_component(1), traj_component(2), 
c     &     traj_component(3)
c
c     Format statements.
c
 100  format (1x,'SETTRAJ: The traj component ',a,' cannot be ',
     &           'found.')
 110  format (1x,'SETTRAJ: Cannot get any of the specified traj '
     &           'field components.')
c
c
      return
      end
