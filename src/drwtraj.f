      subroutine drwtraj (data_x,data_y, data_depth, time,
     &     nf, depth_field, mapflg, savflg, error)
c-----------------------------------------------------------------------
c     $Id: drwtraj.f,v 1.36 2007-06-21 20:58:30 reinecke Exp $
c
c     This routine draws the trajectory plot.
c     
c     Arguments:
c     data_x             X points of trajectory (Plot orientation)
c     data_y             Y points of trajectory (Plot orientation)
c     data_depth         Z points of trajectory (Plot orientation)
c     time               Array of time points at which x,y and z are given
c     nf                 number of X, Y, Z and T points
c     depth_field        Integer number of the depth dimention (1 - 4: X=1)
c     mapflg             Logical - are we on a map?
c     savflg             Logical - is this an overlay?
c     error              Logical - return true if error.
c
c     $Log: drwtraj.f,v $
c     Revision 1.36  2007-06-21 20:58:30  reinecke
c     Commiting local modifications for transfer to svn
c
c     Revision 1.35  2007/05/01 17:57:10  warren
c     fixes for mouse slicing and line drawing on default map projections.
c
c     Revision 1.34  2006-10-05 16:26:32  reinecke
c     phys_to_latlon is called independent of the state of drwmap flag
c     mapflg alone determines if we are on a map.
c
c     Revision 1.33  2005/09/06 20:48:56  reinecke
c     fix dimension of colorband from 150 to 160
c
c     Revision 1.32  2005/09/06 20:22:13  warren
c     Move updates into current stream
c
c     Revision 1.30.2.2  2005/09/06 17:55:22  warren
c     fixed trajectories to deal with maps. Also first part of bold arrows for
c     trajectories
c
c     Revision 1.30.2.1  2005/08/12 17:28:01  warren
c     Fix crash when we have a 0 length trajectory interval (calm)
c
c     Revision 1.30  2003/07/02 20:24:42  warren
c     Fix background fill code to fill correctly. Add printout to terminal
c     as requested for traj points. This should really be an option, and show up
c     in a window.
c
c     Revision 1.29  2002/12/27 18:44:29  warren
c     First set of changes from zurich. use mapdrw_dl to deal with ever changing
c     borders in Europe. Increase arrays in work.
c
c     Revision 1.28  2002/08/09 19:57:57  warren
c     Fixed for Linux and tested with DEC sun etc.
c     most items inside #ifdef's, or were general cleanup/fixes
c
c     Revision 1.27  2002/03/05 01:19:07  harryive
c     Remove rogue semicolon - after all this IS Fortran, not C.
c
c     Revision 1.26  2002/02/22 21:10:40  davidive
c     Fixed ive to set the traj_entry color and use it correctly, instead of
c     zeroing it out every time.
c
c     Revision 1.25  2002/02/21 22:47:57  davidive
c     Use foreground color for labeling colorbar instead of black.
c
c     Revision 1.24  2002/02/21 21:40:08  davidive
c     Was using traj_depth where I should have been using num_depth (the rounded one)
c
c     Revision 1.23  2002/02/11 23:43:09  davidive
c     First cut at traj fixes - most of the way there
c
c     Revision 1.22  2001/11/26 20:35:58  davidive
c     Fixed it to remember traj_step and only add to the bottom or top, even when
c     dropping down to a single level trajectory from multilevel ones.
c
c     Revision 1.21  2001/11/21 23:06:40  davidive
c     Check to see if we ran into the ground (spval) and stop, also initialize
c     data_depth array if there is only 1 level
c
c     Revision 1.20  2001/11/20 23:29:55  davidive
c     Track if step and mid val are set by ive or a person. Also initialize all
c     variables for no depth.
c
c     Revision 1.19  2001/11/17 00:18:19  warren
c     Uncommented part of the window cleanups for the new traj typins.
c
c     Revision 1.18  2001/11/15 21:41:41  davidive
c     Reset all traj values on new plot
c
c     Revision 1.17  2001/11/15 21:38:08  davidive
c     added control over the step size and color matching for trajectories, as well
c     as color bar expansion on overlays
c
c     Revision 1.16  2001/11/02 18:51:17  davidive
c     converted rdctab to c, fixed trajectories so that their independant color
c     tables work, including setting center color, center value and interval.
c
c     Revision 1.15  2000/05/09 19:53:49  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
c     Revision 1.14  1999/02/08 23:20:39  warren
c     Final changes on trajectories. Color depth now works as does line thickness.
c
c     Revision 1.13  1999/01/29 20:46:27  warren
c     ets the flag for time independence and the plot the trajectories
c
c     Revision 1.12  1998/12/11 19:35:16  warren
c     Fixed the tradjectory plots to go forward to the end time as well as
c     backwards to the beginning (Don't check if returned time is in min to max
c     window any more).
c
c     Revision 1.11  1998/11/04 21:24:59  warren
c     Fixed trajectories to actually use the trajectory color table.
c
c     Revision 1.10  1998/10/30 23:25:08  harryive
c     Do not draw color bar if only one level is represented in trajectory.
c
c     Revision 1.9  1998/07/28 18:22:20  harryive
c     New fixes for trajectories when they are the first overlay.
c     Also, remove old Yuri routine.
c
c     Revision 1.8  1998/07/27 22:05:28  warren
c     First set of fixes for Gene's Tradjectory problems -- include corrected arrows
c     corected color lists and plotting of 0 depth tradjectories.
c
c     Revision 1.7  1998/03/04 23:47:40  warren
c     Removed multiple definitions per Digital's compiler
c
c     Revision 1.6  1998/03/04 19:14:00  davidive
c     First cut at trajectories and trajectory widget using colors to show depth
c     and arrows to show direction.
c
c     Revision 1.5  1997/12/23 22:43:52  warren
c     increased MXCRA to 50000 and cleaned up some unused variables and split a
c     couple of large fortran if not blocks, so it wouldn't evaluate it all if
c     unnecessary.
c
c     Revision 1.4  1997/10/30 23:07:49  davidive
c     convert traj to c to dynamically allocate memory, also place trajectory
c     inside black lines one each side.
c
c     Revision 1.3  1997/03/07 15:39:59  warren
c     Fixed multiple declarations to make Alpha happy.
c
c     Revision 1.2  1997/03/06 19:00:00  harry
c     Make changes to properly handle exact_fit case for mouse slicing.
c
c     Revision 1.1  1996/04/25 16:25:10  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'traj.icl'
      include 'supma.icl'
      include 'pltwin.icl'
      include 'color.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      integer     nf, depth_field
      real        data_x, data_y, data_depth, time
      real        xmajr, xminr, ymajr, yminr
      logical      error, mapflg, savflg
      dimension data_x(nf),data_y(nf), data_depth(nf), time(nf)
c
c
c     Local variable declarations.
c
      real             ORVAL, line_width, arrow_fact
      parameter ( ORVAL = 1.0e12 )
      integer, parameter :: max_levs=160
      integer         imap, linepat,axes,i,j,k,coval,colband(max_levs)
      integer         flag(4), line_color, num_depth, mid_num,tmiddiff
      integer         colband_bot
      logical          movielabs
      real            lolim, uplim, depth_levels, tmplevs,PPI,min_depth
      real        data_min, data_max, step, r, tmidval
      real        d, delx, dely,  arowl, d_x(2), d_y(2), x(6),  y(6)
      real        oldtime, disttime, in(4), out(4), inold(4), outold(4) 
      real        boldtime, arrow_width, rlon, rlat
      dimension       lolim(MAXDIM), uplim(MAXDIM),
     &     depth_levels(max_levs), tmplevs(max_levs)
      real            vbot, vtop, vleft, vrght, 
     &                wbot, wtop, wleft, wrght 
c
c
      error = .false.
      mid_num = 0
      call getlvar('movielabs',movielabs,error)
      if ( error ) then
         return
      endif
c
c     Get constants set
c
      call getset ( vleft, vrght, vbot, vtop, 
     &     wleft, wrght, wbot, wtop, axes)
      flag(1)=1
      flag(2)=1
      flag(3)=1
      flag(4)=1
      PPI=3.*(4*(atan2(1.,1.))/4.)
      delx=abs(wrght - wleft)/180
      dely=abs(wtop - wbot)/180
      arowl = sqrt(delx**2 + dely**2)
      in(1)=data_x(nf)
      in(2)=data_y(nf)
      in(3)=data_depth(nf)
      in(4)=time(nf)
      call phys_2_index_trans(in(1), out(1), flag(1), 4, 1)
      in(1)=int(out(1))
      in(2)=int(out(2))
      in(3)=int(out(3))
      in(4)=int(out(4))
      inold(1)=int(out(1))-1
      inold(2)=int(out(2))-1
      inold(3)=int(out(3))-1
      inold(4)=int(out(4))-1
      call index_2_phys_trans(out(1), in(1), flag(1), 4, 1)
      call index_2_phys_trans(outold(1), inold(1), flag(1), 4, 1)
      disttime = out(4) - outold(4)
      oldtime = time(1)
      boldtime = time(1)

      if(mapflg .and. .not.(defmap .or. exact_fit)) then
            flag(3)=0
            flag(4)=0
         do i=1,nf
            call phys_2_lonlat_trans(data_x(i),data_y(i),rlon,rlat,1)
            call MAPTRN(rlat,rlon,data_x(i),data_y(i))
         end do
      endif
      
c     make min depth change for color change 1/2 gridpoint
c      min_depth=abs(out(depth_field) - outold(depth_field))
c      num_depth = 2

c
c     Use conpack out-of-range feature to avoid unwanted vectors.
c     Note that this particular value is used because the EZMAP routines
c     use it by default.
c
      call cpsetr ('ORV - out-of-range value', ORVAL)
c
c     Make appropriate set call.
c
      call set2d (0, mapflg, savflg, lolim, uplim, error)
c
c     Draw the trajectory.
c
      call cpgeti ('MAP - mapping flag', imap) 

      data_min = data_depth(1) ; data_max = data_depth(1)
      do i=1,nf
         if(data_depth(i) .gt. data_max) data_max = data_depth(i)
         if(data_depth(i) .lt. data_min) data_min = data_depth(i)
      enddo
      
c     get the stride
      if(traj_step .ne. 0.) then ! get the step size
         step = traj_step
      else
         step = (data_max - data_min)/20
      endif

      if(step .eq. 0.) then
         if(traj_val .gt. -1e19) then
            step = abs(traj_val - data_min)
         else
            step = 1
         endif
      endif

c     Warn if there are to many steps
      if(step .gt. 0 .and. (data_max - data_min)/step.gt.20) then
         write(message,100) step,(data_max - data_min)/step
         call write_message
      endif

      num_depth=nint(abs((data_max - data_min)/step)) + 2

      if (num_depth .lt. 2) num_depth = 2

c     get mid value
      if(traj_val_set) then
         tmidval = traj_val
      else
         tmidval = data_min +  (step * (num_depth/2))
         if((tmidval + .5) .le. data_max) then 
            tmidval = int((.5 + tmidval)/step) * step
         endif

         traj_val = tmidval
      endif


c     get the mid color entry
      if (abs(traj_entry) .ge. min_traj_color .and. 
     &        traj_entry  .le. max_traj_color) then
         coval = abs(traj_entry)
      else
         coval =(max_traj_color - min_traj_color)/2 + 
     &        min_traj_color
      endif

c     Get the intervals
      if(step .ne. 0. .and. data_max .ne. data_min) then
         j = 1
         depth_levels(1) = tmidval

         ! does this if tmidval < data_min
         do while (depth_levels(1) .lt. data_min)
            depth_levels(1) = depth_levels(1) + step
         enddo

         ! does this if tmidval < data_min
         do while (depth_levels(1) .gt. data_min)
            depth_levels(1) = depth_levels(1) - step
         enddo

c     Go 1 below data_min
         depth_levels(1) = depth_levels(1) - step
         depth_levels(2) = depth_levels(1) + step
         
         if(num_depth .gt. 2) then
            do i = 3, num_depth
               depth_levels(i)= depth_levels(i-1) + step
            enddo
         endif

      endif
      do i=1,max_levs
         tmplevs(i) = 1E38
      enddo
         
      k=1
      if(traj_depth .gt. 0) then

         ! see if the new levels are less than the old levels
         do i=1,num_depth

            if(depth_levels(i) .lt. traj_levels(1)) then
               tmplevs(k) = depth_levels(i)
               k = k + 1
            endif
         enddo

         ! fill in any missing levels in the middle
         if(k.gt.1) then
         do while (tmplevs(k-1) .lt. traj_levels(1)-2.0*step)
           if(k.lt.max_levs) then
             tmplevs(k) = tmplevs(k-1) + step ; k=k+1
           else
             write(message,400) max_levs ; call write_message
           endif
         end do
         end if

         ! if the levels are monotonic just put them in tmplevs
         do i=1,traj_depth

            if(k.lt.max_levs) then
               tmplevs(k) = traj_levels(i)
               k = k + 1
            else
               write(message,400) max_levs
               call write_message
            endif

         enddo

         ! fill in any missing levels in the middle
         do while (tmplevs(k-1) .lt. depth_levels(1)-step)
           if(k.lt.max_levs) then
             tmplevs(k) = tmplevs(k-1) + step ; k=k+1
           else
             write(message,400) max_levs ; call write_message
           endif
         end do

         do i=1,num_depth

            if(depth_levels(i) .gt. traj_levels(traj_depth)) then
               if(k .lt.max_levs) then
                  tmplevs(k) = depth_levels(i)
                  k = k + 1
               else
                 write(message,400) max_levs
                  call write_message
               endif
            endif

         enddo

      else

         do i=1,num_depth
            tmplevs(k) = depth_levels(i)
            k = k + 1
         enddo

      endif

400   format('You have exceded ',I3,' trajectory levels.',2x,
     &       'Please advise the IVE development team of this error.')
         
c     Thanks to FORTRASH Arrays, we have gone one k beyond
      k= k -1 
      if (k .gt. max_levs) k = max_levs
      
      !print *,'tmidval,data_max,data_min',tmidval,data_max,data_min
      if(tmidval.gt.data_max) then
        !tmiddiff = -int((tmidval-data_max)/step+0.5)
        tmiddiff = -int((tmidval-data_max)/step)-1
        mid_num = num_depth - tmiddiff
      elseif(tmidval.lt.data_min) then
        !tmiddiff = int((data_min-tmidval)/step + 0.5)
        !mid_num = 1 - tmiddiff 
        tmiddiff = int((data_min-tmidval)/step)-1
        mid_num = 1-tmiddiff 
      else
        tmiddiff=0
      endif

      !print *,'mid_num,tmidiff,coval: ',mid_num,tmiddiff,coval

      do i=1,max_levs
         depth_levels(i) = tmplevs(i)
         if(abs(tmplevs(i)).le.epsilon(0.0)) depth_levels(i) = 0.
         if((abs(depth_levels(i) - tmidval).le.epsilon(tmidval)))
     &        mid_num = i 
      enddo

      num_depth = k
      if(num_depth .gt. 0) then
         write(6,430) data_min,data_max,step
      else
         write(6,*)'Numdepth is 0 !!!!!!!!!!!'
      endif

430   format('Min/Max traj values:',1x,F10.4,1x,'/',F10.4,'.',1x,
     &       'Using a colorbar step of:',1x,F10.4)

      if(num_depth .gt. 0) then
      !print *,'mid_num,coval: ',mid_num,coval
            
c     Set the first band then itterate for the rest.
         if(step .ne. 0) then
            colband_bot = coval - mid_num + 1

            if(old_traj_min_color.lt.colband_bot) then
              colband(1) = old_traj_min_color
            else
              colband(1) = colband_bot
            end if

            !print *,'colband(1): ',colband(1),min_traj_color

            do while (colband(1) .lt. min_traj_color) 
               colband(1) = max_traj_color - 
     &                      abs(min_traj_color-colband(1))
            enddo

            do while (colband(1) .gt. max_traj_color) 
               colband(1) = min_traj_color + 
     &                      abs(colband(1)-max_traj_color)
            enddo

         else
            colband(1) = coval
         endif
            
         i = 2
         r = data_min + step
         do i=2,max_levs
            colband(i) = colband(i-1)+1

            do while (colband(i) .lt. min_traj_color) 
               colband(i) = max_traj_color - abs(min_traj_color -1 -
     &              colband(i)) 
            enddo

            do while (colband(i) .gt. max_traj_color) 
               colband(i) = min_traj_color + abs(colband(i) -
     &              max_traj_color -1)
            enddo

 950        continue
         enddo

c         write(6,*)'mid: ',mid_num,' val: ',tmidval,' color: ',
c     &        colband(mid_num)

      else
         
         if (traj_entry .gt. min_traj_color .and. traj_entry .le. 
     &        max_traj_color) then
            coval = traj_entry
            colband(1) = coval
            colband(2) = coval + 1
            colband(3) = coval + 2
         else
            coval =(max_traj_color - min_traj_color)/2 + 
     &           min_traj_color
            colband(1) = coval
            colband(2) = coval + 1
            colband(3) = coval + 2
         endif

         if(traj_val_set) then
            tmidval = traj_val
         else
            tmidval = data_min
         endif
      endif
      old_traj_min_color = colband(1) 
         
         
         
c     wide colored line      
      call getivar ('hipattern', linepat, error)
      call getrvar ('linwdth', line_width, error)
c     use solid line not linepat
      call dashdb (65535)  
      call gslwsc (line_width)
      call gsplci(colband(1))
c     Skip this mess if not needed
      
      
      if(num_depth .eq. 1)then
         depth_levels(1) = data_min
         colband(1) = coval
      endif

      do i=1, nf-1
         if(num_traj_comp .gt. 2 .and. num_depth .gt. 1) then
            j=1

            do j=1, num_depth
               if(data_depth(i) .ge. depth_levels(j) .and.
     &           (j .eq. num_depth .or.
     &            data_depth(i) .lt.  depth_levels(j+1) )) then
                  call gsplci(colband(j))
                  goto 1010
               endif
            enddo

 1010       continue
            call gslwsc (line_width * 2)

         else
            call gsplci(colband(1))   
            call gslwsc (line_width*2)
         endif

         call dashdb (65535)       
c         call curved (data_x(i), data_y(i), 2)
         call gpl(2,data_x(i),data_y(i))

         ! Draw Arrows
c         if(time(i+1) - oldtime .ge. disttime) then
c            arrow_fact = max(line_width*0.6, 1.1)

c            if(arrow_bold_interval .eq. 0)
c     &        arrow_bold_interval=(time(nf)-time(1))/3

c            arrow_width = 1.

c            if(time(i+1)-boldtime.ge.arrow_bold_interval*disttime
c     &           .and.arrow_bold_interval.ne.0) then
c               arrow_width = 2.
c               boldtime = time(i+1)
c               arrow_fact = max(line_width*2.0, 1.1)
c            endif
            
c            oldtime=time(i+1)
c            if(d_x(1) .ne. d_x(2) .and. 
c     &           d_y(1) .ne. data_y(2)) then
c               x(2) = d_x(2)
c               y(2) = d_y(2)
c               D = ATAN2((d_y(2)-d_y(1))/(wtop-wbot),
c     &              (d_x(2)-d_x(1))/(wrght-wleft))
c               x(1) = x(2) + (cos(D + PPI))*delx*1.1*arrow_fact
c               x(3) = x(2) + (cos(D - PPI))*delx*1.1*arrow_fact
c               y(1) = y(2) + (sin(D + PPI))*dely*1.1*arrow_fact
c               y(3) = y(2) + (sin(D - PPI))*dely*1.1*arrow_fact
            
            
c               call dashdb (65535)
c               call gsplci(9)   !black
c               call gslwsc (1.) 
c               call curved (x(1), y(1), 3)
c            endif
c         endif
      enddo
      
 180  continue
c     
c     If these routines encountered any errors, then return.
c     
      if ( error ) then
         return
      endif
c     
c     
c     Draw either a map foreground or perimeter, whichever is 
c     appropriate.
c     
      if ( .not. savflg ) then

         if ( mapflg ) then
            call mapdrw_dl
            if (defmap .and. exact_fit) then
               call getset ( vleft, vrght, vbot, vtop, 
     &              wleft, wrght, wbot, wtop, axes)
               call set ( vleft, vrght, vbot, vtop,
     &              x1, x2, y1, y2, axes)
            endif
         endif

         if ( .not. mapflg .or. (defmap .and. exact_fit)) then
            xmajr = xmajor ; xminr = xminor
            ymajr = ymajor ; yminr = yminor

            call getivar ('xaxis', xaxis, error)
            call getivar ('yaxis', yaxis, error)

            call scaletics (xmajr, xminr, ymajr, yminr,
     &           mod(xaxis,10), mod(yaxis,10))

            if (movielabs) then
               call pcseti ('QU',0)
               call pcsetc ('FN', 'HELVETICA-BOLD    ')
            else
               call pcseti ('QU',1)
               call pcseti ('CL',1)
            endif

            call uwperim (xmajr, xminr, ymajr, yminr)
         endif
      endif

      ! label plot.
      if ( label ) then
         !Use labeling routine for solid fill to label vector plots.
         call labtraj (datfil, lolim, uplim, savflg, mapflg)
      endif
c     
c     Color bar
c     
      if (movielabs) then
         call pcseti ('QU',0)
         call pcsetc ('FN', 'HELVETICA-BOLD    ')
      else
         call pcseti ('QU',1)
         call pcseti ('CL',1)
      endif

      call traj_color_bar(depth_levels(1), colband(1), num_depth, 
     &                    mapflg)
      traj_depth = num_depth

      if(.not. traj_val_set)  traj_val = tmidval
      if(.not. traj_step_set) traj_step = step
      if (.not. (traj_entry .gt. 0))traj_entry = -coval

      do i=1, num_depth
         traj_levels(i) = depth_levels(i)
      enddo

      open(10,file='./traj_dump.dat',status='replace',action='write')
      do i=1,nf
         !write(6 ,700) data_x(i),data_y(i),data_depth(i),time(i)
         write(10,700) data_x(i),data_y(i),data_depth(i),time(i)
      enddo
      close(10)
700   format(4(F14.6,4x))
c     
c     Release all output.
c     
      call plotit (0, 0, 0)
c     
c     Reset out-of-range value to its default.
c     
      call cpsetr ('ORV -out-of-range value', 0.0)
c     
c     
 100  format (1x 'The colorbar step of ',F7.2,' gives ',F7.2,
     &      ' colors. Only 20 will be displayed') 
      return
      end
      
      subroutine traj_color_bar(levels, colband, count, mapflg)
      include 'color.icl'
      include 'units.icl'
      
      real levels
      integer colband, count
      dimension levels(count), colband(count)
      logical :: mapflg
      
      integer       i, linlog, outnum,outsig,outpow, numcontrol
      real          vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt
      
      real          length, x(5), y(5)
      real :: fontscale, cent, rotate, xpos, xtxt
      
      if(count .eq. 0) return

      call gslwsc (5.)
      call dashdb(65535)
      call getset (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
      call scale (levels(1), count, data_slope, data_intercept, 0.0)
      length = (wdt - wdb)/(count)
      call set(0.,  1.,  vpb,  vpt, 0., 1., wdb, wdt, 1)
      call gsclip (0)

      if(mapflg) then
        rotate=0.0
        fontscale=0.0125
        cent=1.25
        xpos=vpl-0.01
        xtxt=xpos
      else
        rotate=90.0
        fontscale=0.0084
        cent=-1.0
        xpos=0.018
        xtxt=0.007
      end if

C     Blank out the old one
      x(1)=0.0  ; y(1)=wdb - 0.005
      x(2)=xpos ; y(2)=wdb - 0.005
      x(3)=xpos ; y(3)=wdt + 0.005
      x(4)=0.0  ; y(4)=wdt + 0.005
      call gsfais(1) ; call gsfaci(0) ; call gfa (4, x, y)

C     Do the new one
      x(1)=xpos ; x(2)=xpos

      if(count .eq. 1) then
         y(1)=wdb
         y(2) = y(1) + length
         call gsplci(colband(1))
         call curved (x(1), y(1), 2)
         call cpnumb(levels(1),4,-10000,4,-1,'E',' ',' ',1,0,0,0,
     &        1,1,barlab(1),outnum,outsig,outpow)

         call clean_zero(barlab(1))
         call gsplci(9)         !black

         call plchhq(x(1),y(1),trim(barlab(1)), 
     &               fontscale, rotate, cent)
      else
         numcontrol=1
         do i=1, count
            if(levels(i) .lt. 1E38) then

               y(1)=wdb+(i-1)*length ; y(2) = y(1) + length
               call gsplci(colband(i))
               call curved (x(1), y(1), 2)
               if(mod(i,numcontrol) .eq. 0) then
                  call cpnumb(levels(i),4,-10000,4,-1,'E',' ',
     &                 ' ',1,0,0,0,
     &                 1,1,barlab(i),outnum,outsig,outpow)
                  call clean_zero(barlab(i))
                  call gsplci(1) !foreground
                  if(i.gt.1) then
                    call plchhq(xtxt,y(1),trim(barlab(i)), 
     &                          fontscale, rotate, cent)
                  end if
               endif

            endif
         enddo
      endif

      call set (vpl, vpr, vpb, vpt, wdl, wdr, wdb, wdt, linlog)
      call gslwsc(1.)
      return
      end
      
      subroutine reset_traj_colors(error)
      include 'traj.icl'
      include 'color.icl'
      
      logical error
      integer i 
      integer, parameter :: max_levs=160

      error = .false.
      old_traj_min_color = max_traj_color
      if(.not. traj_val_set)  traj_val = -1e19
      if(.not. traj_step_set) traj_step = 0.
      if(traj_entry .lt. 0) traj_entry = 0
      traj_depth = 0
      do i=1,max_levs
         traj_levels(i) = 1E38
      enddo
      return
      end
      
