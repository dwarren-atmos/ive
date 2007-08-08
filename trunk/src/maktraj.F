#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer*4
#endif
      subroutine maktraj(error)
c
c     $Id: maktraj.F,v 1.10 2006-10-05 16:27:36 reinecke Exp $
c     Purpose:
c        This subroutine is the top-level driver which produces
c        a trajectory plot.
c     Arguments:
c        None
c     History:
c     $Log: maktraj.F,v $
c     Revision 1.10  2006-10-05 16:27:36  reinecke
c     fixed erronous error message
c
c     Revision 1.9  2005/09/06 20:22:14  warren
c     Move updates into current stream
c
c     Revision 1.8.2.1  2005/09/06 17:55:23  warren
c     fixed trajectories to deal with maps. Also first part of bold arrows for
c     trajectories
c
c     Revision 1.8  2003/07/02 20:24:44  warren
c     Fix background fill code to fill correctly. Add printout to terminal
c     as requested for traj points. This should really be an option, and show up
c     in a window.
c
c     Revision 1.7  2002/05/22 17:56:45  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.6  2002/02/04 22:55:05  warren
c     Fortran arrays start at 1 not 0
c
c     Revision 1.5  1998/07/28 18:22:34  harryive
c     New fixes for trajectories when they are the first overlay.
c     Also, remove old Yuri routine.
c
c     Revision 1.4  1998/03/04 19:14:04  davidive
c     First cut at trajectories and trajectory widget using colors to show depth
c     and arrows to show direction.
c
c     Revision 1.3  1997/10/30 23:07:51  davidive
c     convert traj to c to dynamically allocate memory, also place trajectory
c     inside black lines one each side.
c
c     Revision 1.2  1997/02/12 17:36:23  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.1  1996/04/25 16:25:24  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c	

      include 'segments.icl'
      include 'message.icl'
      include 'traj.icl'
      include 'color.icl'
c
c
c     Local variable declarations.
c
      integer  ni, nj,  buttons, fix, nf, nter, depthvar
      integer  vid, getvid
      ive_ptr slab1, slab2, slab3, slab4, topoad, topopt
      ive_ptr subslab, dp1, dp2, dp3, dp4

      logical  error, mapflg, savflg, surface
      real pt1, pt2, pt3
      dimension  fix(4)
      dimension pt1(4), pt2(4), pt3(4)
c
c
c     External function declarations.
c
      integer  strbeg, strend
c
c
c
c
      error = .false.
c
c      call chkwin
c
c     Init vars so we don't free them if chktraj finds a problem
c
      slab1=0
      slab2=0
      slab3=0
      slab4=0
      topoad=0
      topopt=0
c
      call chktraj(error)
      if( error ) then
         go to 900
      endif
c     
c
c     Get vector components at points in the slice plane.
c
      call gettraj (slab1, slab2, slab3, slab4, topoad, topopt, subslab,
     &          nf, ni, nj, nter, error)
c
c
c     Plot the data.
c
      if ( .not. error ) then
c
c
         call getlvar ('savflg', savflg, error)
         if ( error ) go to 900
c
         call makseg(.true.)
c
c
c
c        Draw a map if required.
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

         if ( mapflg ) then
            call drwmap (error)
         endif
c
         if ( error ) then
            go to 900
         endif
c
c        Draw the vectors.
c              
         if(buttons .ne. 0) then
            if(fix(1) .eq. 0 .and. fix(2) .eq. 0 .and. (fix(3) .ne. 0 
     &           .and. fix(4) .ne. 0)) then
               dp1 = slab1
               dp2 = slab2
               dp3 = slab3
               dp4 = slab4
               depthvar=3
               
            elseif(fix(1) .eq. 0 .and. fix(3) .eq. 0 .and. (fix(2) 
     &              .ne. 0 .and. fix(4) .ne. 0)) then
               
               dp1 = slab1
               dp2 = slab3
               dp3 = slab2
               dp4 = slab4
               depthvar=2
               
            elseif(fix(2) .eq. 0 .and. fix(3) .eq. 0 .and. (fix(1) 
     &              .ne. 0 .and. fix(4) .ne. 0)) then
               
               dp1 = slab2
               dp2 = slab3
               dp3 = slab1
               dp4 = slab4
               depthvar=1
            else
               write(message,200)
 200           format('You can''t do trjectories on a time plot')
               call write_message()
               goto 900
            endif
         else

         endif

         if(traj_background_field(strbeg(traj_background_field):
     &        strend(traj_background_field)) .ne. 'depth') then
c            write(6,*)'Uhoh............. background is ',
c     &           traj_background_field
            dp3 = subslab
            depthvar=5
         endif

         call drwtraj (%val(dp1), %val(dp2), %val(dp3), 
     &        %val(dp4), nf, depthvar, mapflg, 
     &        savflg, error)

         if(.not. error) then
            if(depthvar .lt.5) then
               call set_traj_atts(-depthvar, traj_val, traj_step)
            endif
         else
            vid = getvid(traj_background_field(
     &           strbeg(traj_background_field):
     &           strend(traj_background_field))//char(0))
            call set_traj_atts(vid, traj_val, traj_step)
         endif
         
c     free fix(4) doesn't make sense.
         
c
c
         if ((.not. savflg) .and. (topoad .ne. 0)) then
c
            call getlvar ('surface', surface, error)
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
c
c
c
c
c     Free all allocated memory.
c
  900 continue
      if(error) write(6,*)' not plotting traj due to error'

      if ( slab4  .gt. 0 ) then
         call freemem(slab4) 
      endif

      if ( slab1  .gt. 0 ) then
         call freemem(slab1)
      endif

      if ( slab2  .gt. 0 )  then
         call freemem(slab2) 
      endif

      if ( slab3  .gt. 0 ) then
         call freemem(slab3)
      endif
      if ( subslab.gt. 0 ) call freemem(subslab) 
      if ( topopt .gt. 0 ) call freemem (topopt)
      if ( topoad .gt. 0 ) call freemem (topoad)
c
c
c      if(error)then
c         write (message, 100)
c  100    format(1x, ' There was an error in maktraj')
c         call write_message
c      endif
      
      return
      end
