#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
      subroutine huen(phys, u, v, w,maxdim,
     &            unx,uny,unz,unt,vnx,vny,vnz,vnt,wnx,wny,wnz,wnt,
     &            grounded,delt, error)
c
c     $Id: huen.F,v 1.6 2002/08/09 19:57:58 warren Exp $
c
c     This routine is called by traj to help calculate the
c     trajectories.
c
c     $Log: huen.F,v $
c     Revision 1.6  2002/08/09 19:57:58  warren
c     Fixed for Linux and tested with DEC sun etc.
c     most items inside #ifdef's, or were general cleanup/fixes
c
c     Revision 1.5  2001/11/21 23:06:40  davidive
c     Check to see if we ran into the ground (spval) and stop, also initialize
c     data_depth array if there is only 1 level
c
c     Revision 1.4  1998/03/04 19:14:01  davidive
c     First cut at trajectories and trajectory widget using colors to show depth
c     and arrows to show direction.
c
c     Revision 1.3  1997/10/22 19:35:05  davidive
c     Turned the particle movement part of trajectories into a user transform
c     and added unit checking to the default transform.
c
c     Revision 1.2  1997/02/12 17:36:11  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:16  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      
c      include 'data.icl'
      include 'traj.icl'
      integer maxdim,unx,uny,unz,unt,vnx,vny,vnz,vnt,wnx,wny,wnz,wnt,
     &     grounded
      real phys(maxdim),u(unx,uny,unz,unt), 
     &     v(vnx,vny,vnz,vnt), w(wnx,wny,wnz,wnt)
      real  delt
c     local variables
      integer repeat, iflag(4)
      real phystmp(4), tmpc(4), tmpp(4), uarr(4),varr(4),
     &     warr(4), spval
      real uold,  vold, utmp, vtmp,d1,d2, wold,wtmp, d3
      logical doubled, halved, error
      
      ive_ptr tmp_ptr
      
c     external functions
      double precision  extrap
      integer   strbeg, strend, nblank
      integer lnx, lny, lnz, lnt
      data iflag / 1,1,1,1/
      halved = .false.
      doubled = .false.
      error= .false.
      call getrvar('spval', spval, error)
      if(spval .eq. 0.) spval = -999999.999
      repeat = 0
      error = .false.
      grounded = 0
      
 10   continue
      
      write(6,*)' start point',phys
      if(nblank(traj_component(1)) .ne. 0) then
         call get_pointer(traj_component(1)(strbeg(traj_component(1)):
     &        strend(traj_component(1))),tmp_ptr,lnx,lny,lnz,lnt)
         call phys_2_index_trans(phys, uarr, iflag, maxdim, 1)
         uold = extrap(u,unx,uny,unz,unt,
     &        uarr(1),uarr(2),uarr(3),uarr(4),spval) 
      else
         uold=0.0
      end if
      if(nblank(traj_component(2)) .ne. 0) then
         call get_pointer(traj_component(2)(strbeg(traj_component(2)):
     &        strend(traj_component(2))),tmp_ptr,lnx,lny,lnz,lnt)
         call phys_2_index_trans(phys, varr, iflag, maxdim, 1)
         vold = extrap(v,vnx,vny,vnz,vnt,
     &        varr(1),varr(2),varr(3),varr(4),spval)
      else
         vold=0.0
      endif 
      if(.not. traj_2d .and. nblank(traj_component(3)) .ne. 0) then
         call get_pointer(traj_component(3)(strbeg(traj_component(3)):
     &        strend(traj_component(3))),tmp_ptr,lnx,lny,lnz,lnt)
         call phys_2_index_trans(phys, warr, iflag, maxdim, 1)
         wold = extrap(w,wnx,wny,wnz,wnt,
     &        warr(1),warr(2),warr(3),warr(4),spval)     
      else
         wold = 0.0
      endif
      
c      write(6,*) phys, uold, wold, vold 
      
c     first part of Huen method
      if(uold .eq. spval .or. vold .eq. spval .or. wold .eq. spval) then
         grounded = 1
         return
      endif
      write(6,*)'u v w delt',uold,vold,wold,delt
      call move_parcel_trans(phystmp, phys, uold, vold, wold, delt,
     &     repeat, error)
      if (error) then
         return
      endif
      repeat = 1
      
c     phystmp(1)= phys(1) + uold*delt
c     phystmp(2) = phys(2) + vold*delt
c     phystmp(3) = phys(3) + wold*delt
c     phystmp(4) = phys(4) + delt
      
c     check to see how far the integration has gone, if it is to 
c     far (more then a half grid point) or not far enough 
c     (less then a tenth grid point) redo it.
      call phys_2_index_trans(phystmp, tmpc, iflag, maxdim, 1)
      call phys_2_index_trans(phys, tmpp, iflag, maxdim, 1)
      d1=abs(tmpc(1)-tmpp(1))
      d2=abs(tmpc(2)-tmpp(2))
      d3=abs(tmpc(3)-tmpp(3))
c     adjust the time step
c     if ((.not.doubled).and.(abs(delt).gt.0.5)) then
      if ((.not.doubled).and.(max(d1,d2,d3).gt.0.5) .and.
     &     (.not. halved)) then
         delt = delt/2.
         halved= .true.
         goto 10
      endif
      if ((.not.halved) .and. (.not. doubled) .and.
     &     (max(d1,d2,d3).lt.0.25)) then 
         delt = 2.*delt
         doubled = .true.
         goto 10
      end if
      
c     now find the u and v values at this intermediate time

      if(nblank(traj_component(1)) .ne. 0) then
         call get_pointer(traj_component(1)(strbeg(traj_component(1)):
     &               strend(traj_component(1))),tmp_ptr,lnx,lny,lnz,lnt)
         call phys_2_index_trans(phystmp, uarr, iflag, maxdim, 1)
         utmp = extrap(u,unx,uny,unz,unt,
     &               uarr(1),uarr(2),uarr(3),uarr(4),spval) 
      else
         utmp=0.0
      end if
      if(nblank(traj_component(2)) .ne. 0) then
         call get_pointer(traj_component(2)(strbeg(traj_component(2)):
     &               strend(traj_component(2))),tmp_ptr,lnx,lny,lnz,lnt)
         call phys_2_index_trans(phystmp, varr, iflag, maxdim, 1)
         vtmp = extrap(v,vnx,vny,vnz,vnt,
     &               varr(1),varr(2),varr(3),varr(4),spval)
      else
         vtmp=0.0
      endif 
      if(.not. traj_2d .and. nblank(traj_component(3)) .ne. 0) then
         call get_pointer(traj_component(3)(strbeg(traj_component(3)):
     &               strend(traj_component(3))),tmp_ptr,lnx,lny,lnz,lnt)
         call phys_2_index_trans(phystmp, warr, iflag, maxdim, 1)
         wtmp = extrap(w,wnx,wny,wnz,wnt,
     &               warr(1),warr(2),warr(3),warr(4),spval)     
      else
         wtmp = 0.0
      endif
      if(utmp .eq. spval .or. vtmp .eq. spval .or. wtmp .eq. spval) then
         grounded = 1
         return
      endif
      if(delt .eq. 0.) then
         grounded = 1
         return
      endif

c     second step of huen method
      call move_parcel_trans(phystmp, phys, (uold+utmp)/2., 
     &     (vold+vtmp)/2, (wold+wold)/2, delt, repeat, error)
      if (error) then
         return
      endif
      repeat = 1
      write(6,*)' new point',phystmp
      phys(1) = phystmp(1)
      phys(2) = phystmp(2)
      phys(3) = phystmp(3)
      phys(4) = phystmp(4)
      
c      phys(1) = phys(1) + .5*delt*utmp + .5*delt*uold
c      phys(2) = phys(2) + .5*delt*vtmp + .5*delt*vold
c      phys(3) = phys(3) + .5*delt*wtmp + .5*delt*wold
c      phys(4) = phys(4) + delt
      return 
      end

