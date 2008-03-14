      subroutine settime (error)
c-----------------------------------------------------------------------
c     $Id: setloc.f,v 1.20 2007-06-21 20:58:31 reinecke Exp $
c     Purpose:
c        This routine sets the time dimension of the data file.
c     Arguments:
c        error  logi  output  error flag.
c     History:
c     $Log: setloc.f,v $
c     Revision 1.20  2007-06-21 20:58:31  reinecke
c     Commiting local modifications for transfer to svn
c
c     Revision 1.19  2002/08/09 19:58:01  warren
c     Fixed for Linux and tested with DEC sun etc.
c     most items inside #ifdef's, or were general cleanup/fixes
c
c     Revision 1.18  2002/05/30 21:42:13  warren
c     Sun compiler found some typo's such as ';'s in fortran.
c
c     Revision 1.17  2002/05/22 17:56:47  davidive
c     First working version of trajectories colored by alternative fields
c     There is not a complete interface yet!!!
c
c     Revision 1.16  2000/12/12 17:58:23  harry
c     Fix problems with arbitrary line slices.
c
c     Revision 1.15  1999/12/22 00:33:23  warren
c     Fixed Dales crashing bug when changing from point slicer to normal. Also
c     have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c     Revision 1.14  1997/06/05 15:55:34  harry
c     Fixes to point slicing.
c
c     Revision 1.13  1997/06/03 19:14:24  davidive
c     The big mouse slicing changeover. We have the new form, only requires you
c     set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c     always runs through init_points_ to catch all these things.
c     note: only actually works when two dimentions have points set. Needs to
c     be expanded into 1, 3 or 4.
c
c     Revision 1.12  1997/03/14 22:50:02  warren
c     Take out extra settings of the scaled_loc array
c
c     Revision 1.11  1996/11/22 18:49:46  warren
c     fixed crashes related to point slicing.
c
c     Revision 1.10  1996/11/07 16:55:39  warren
c     Fixed to deal with the case of use_buttons == 0
c
c     Revision 1.9  1996/10/11 01:13:08  warren
c     To fill or not to fill is no longer the only question. Now there is a seperate
c     question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c     Labels are not fixed yet.
c
c Revision 1.8  1994/03/31  00:22:28  warren
c Fixed it so we now actually use the and store the tic mark and limit values
c in the filed's property sheet.
c
c Revision 1.7  1994/03/30  22:47:35  yuri
c Added comments and more RIGHT comments.
c More stuff will follow.
c
c Revision 1.6  1993/11/30  17:10:42  harry
c Save of scaled locked y value in scaled_loc array was missing.
c
c Revision 1.5  1993/09/23  22:37:03  yuri
c Minor bugs corrected in new version too.
c
c Revision 1.4  1993/09/03  19:44:50  yuri
c 	"set" routimes now make sure variable is inside limits in new
c version too.
c
c Revision 1.3  1993/07/06  20:23:11  harry
c Add the "scaled_loc" variable to retain scaled values for fixed coordinates.
c
c Revision 1.2  1993/06/29  21:02:04  warren
c Fixed Rajul's problem of crashing when reading a file with no mins or maxs.
c No longer look at widgets that don't exist. Also do not free a direction
c if it is fixed by the variable limits
c
c Revision 1.1.1.1  1993/04/07  19:40:26  harry
c Initial CVS checkin of IVE.
c
c Revision 1.5  1992/12/03  00:00:31  millerp
c set lock(3)=1 for surface plots.
c
c Revision 1.4  1992/11/23  16:35:06  millerp
c Added stuff for surface plots.
c
c Revision 1.3  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 1.2  1992/09/29  19:43:15  harry
c Add loop_flag processing for every direction.
c
c Revision 1.1  1992/09/16  15:47:39  warren
c Initial revision
c
c Revision 2.0  1992/07/01  21:16:26  millerp
c Released version rereving to 2.0
c
c Revision 1.4  1992/06/15  19:02:07  millerp
c Change argument list for scale.
c
c Revision 1.3  1992/06/10  20:16:08  millerp
c Unscale entered values for use in graphics routines.
c
c Revision 1.2  1992/06/01  20:16:03  harry
c Add loopflag common block and check for tloop flag.
c
c Revision 1.1  1992/05/29  22:34:37  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
      real                  value
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
      if (loop_flag(4)) then
         error = .true.
         return
      endif
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c     If a value is not given, do not set tloc.
c     Otherwise set tloc to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      else
         if(phmin(4) .ne. phmax(4)) then 
            if (curval(beg:beg) .eq. 'F' .or. 
     &           curval(beg:beg) .eq. 'f') then
               lock(4) = 0
               return
            elseif (curval(beg:beg) .eq. 'A' .or. 
     &              curval(beg:beg) .eq. 'a') then
               lock(4) = 2
               return
            endif
         endif
c     
         call cvatof (curval(beg:end), value, error)
         if ( error ) then
c     
c     The value for this command is incorrect.
c     
            write (message, 110) curval(beg:end),
     &           comand(1)(strbeg(comand(1)):
     &           strend(comand(1)))
            call write_message
         else
            if(value . gt. 
     &       amax1(plmin_scaled(4),plmax_scaled(4)))then
               write (message, 120)
     &          value, amax1(plmin_scaled(4),plmax_scaled(4))
               call write_message
               value =  amax1(plmin_scaled(4),plmax_scaled(4))
            endif
            if(value . lt.
     &           amin1(plmin_scaled(4),plmax_scaled(4)))then
               write (message, 120) value,
     &              amin1(plmin_scaled(4),plmax_scaled(4))
               call write_message
               value =  amin1(plmin(4),plmax(4))
            endif
            scaled_loc(4) = value
            call unscale (value, 1, domain_slope(4), 
     &           domain_intercept(4), 0.0)
            tloc = value
            lock(4) = 1
            call ive_traj_depth_check ()
         endif
      endif
c     
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
c
      subroutine setxloc (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the x dimension of the data file.
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
      real                  value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      if (loop_flag(1)) then
         error = .true.
         return
      endif
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c     If a value is not given, do not set xloc.
c     Otherwise set xloc to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         if(phmin(1) .ne. phmax(1)) then
            if (curval(beg:beg) .eq. 'F' .or. 
     &           curval(beg:beg) .eq. 'f') then
               lock(1) = 0
               return
            elseif (curval(beg:beg) .eq. 'A' .or. 
     &              curval(beg:beg) .eq. 'a') then
               lock(1) = 2
               return
            endif
         endif
c     
         call cvatof (curval(beg:end), value, error)
         if ( error ) then
c     
c     The value for this command is incorrect.
c     
            write (message, 110) curval(beg:end),
     &           comand(1)(strbeg(comand(1)):
     &           strend(comand(1)))
            call write_message
         else
            if(value . gt. 
     &       amax1(plmin_scaled(1),plmax_scaled(1)))then
               write (message, 120) value,
     &          amax1(plmin_scaled(1),plmax_scaled(1))
               call write_message
               value =  amax1(plmin_scaled(1),plmax_scaled(1))
            endif
            if(value . lt. 
     &     amin1(plmin_scaled(1),plmax_scaled(1)))then
               write (message, 120) value,
     &          amin1(plmin_scaled(1),plmax_scaled(1))
               call write_message
               value =  amin1(plmin_scaled(1),plmax_scaled(1))
            endif
            scaled_loc(1) = value
            call unscale (value, 1, domain_slope(1), 
     &           domain_intercept(1), 0.0)
            xloc = value
            lock(1) = 1
            call ive_traj_depth_check ()
         endif
      endif
c     
c     Format statements.
c     
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
c
      subroutine setyloc (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the y dimension of the data file.
c     Arguments:
c        error  logi  output  error flag.
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c     Argument declarations.
c
      logical  error
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
      real                  value
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
      if (loop_flag(2)) then
         error = .true.
         return
      endif
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c     If a value is not given, do not set yloc.
c     Otherwise set yloc to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         if(phmin(2) .ne. phmax(2)) then
            if (curval(beg:beg) .eq. 'F' .or. 
     &           curval(beg:beg) .eq. 'f') then
               lock(2) = 0
               return
            elseif (curval(beg:beg) .eq. 'A' .or. 
     &              curval(beg:beg) .eq. 'a') then
               lock(2) = 2
               return
            endif
         endif
c     
         call cvatof (curval(beg:end), value, error)
         if ( error ) then
c     
c     The value for this command is incorrect.
c     
            write (message, 110) curval(beg:end),
     &           comand(1)(strbeg(comand(1)):
     &           strend(comand(1)))
            call write_message
         else
            if(value . gt.
     &       amax1(plmin_scaled(2),plmax_scaled(2)))then
               write (message, 120)
     &          value, amax1(plmin_scaled(2),plmax_scaled(2))
               call write_message
               value =  amax1(plmin_scaled(2),plmax_scaled(2))
            endif
            if(value . lt.
     &       amin1(plmin_scaled(2),plmax_scaled(2)))then
               write (message, 120)
     &          value, amin1(plmin_scaled(2),plmax_scaled(2))
               call write_message
               value =  amin1(plmin_scaled(2),plmax_scaled(2))
            endif
            scaled_loc(2) = value
            call unscale (value, 1, domain_slope(2),
     &           domain_intercept(2), 0.0)
            yloc = value
            lock(2) = 1
            call ive_traj_depth_check ()
         endif
      endif
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
c
      subroutine setzloc (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the z dimension of the data file.
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
      real                  value
c
c     External function declarations.
c
      integer nblank, strbeg, strend, strcasecmp
c
c
c
      if (loop_flag(3)) then
         error = .true.
         return
      endif
      error = .false.
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c     If a value is not given, do not set zloc.
c     Otherwise set zloc to the specified value.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         if(phmin(3) .ne. phmax(3)) then
            if (curval(beg:beg) .eq. 'F' .or.
     &           curval(beg:beg) .eq. 'f') then
               surface = .false.
               lock(3) = 0
               return
            elseif (curval(beg:beg) .eq. 'A' .or.
     &              curval(beg:beg) .eq. 'a') then
               surface = .false.
               lock(3) = 2
               return
            endif
         endif
         if (strcasecmp(curval(beg:beg)//char(0), 's'//char(0)) 
     &        .eq. 0) then
            surface = .true.
            lock(3) = 1
            call ive_traj_depth_check ()
         else
c     
            call cvatof (curval(beg:end), value, error)
            if ( error ) then
c     
c     The value for this command is incorrect.
c     
               write (message, 110) curval(beg:end),
     &              comand(1)(strbeg(comand(1)):
     &              strend(comand(1)))
               call write_message
            else

               if(value . gt. 
     &              amax1(plmin_scaled(3),plmax_scaled(3)))then
                  write (message, 120)
     &                 value, amax1(plmin_scaled(3),plmax_scaled(3))
                  call write_message
                  value =  amax1(plmin_scaled(3),plmax_scaled(3))
               endif

               if(value . lt.
     &          amin1(plmin_scaled(3),plmax_scaled(3)))then
                  write (message, 120)
     &             value,amin1(plmin_scaled(3),plmax_scaled(3))
                  call write_message
                  value =  amin1(plmin_scaled(3),plmax_scaled(3))
               endif

               scaled_loc(3) = value
               call unscale (value, 1, domain_slope(3), 
     &              domain_intercept(3), 0.0)
               surface = .false.
               zloc = value
               lock(3) = 1
               call ive_traj_depth_check ()
            endif
         endif
      endif
c     
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end

      subroutine line_tic_check()
c---------------------------------------------------------------------------
c     This subroutine checks to see if we have created a line plot and if so
c     sets up the ticmarks and limits appropriately
c---------------------------------------------------------------------------
      include 'plotops.icl'
      include 'attributes.icl'
      include 'lineplt.icl'

      integer freecount, freevar, i

      freecount=0
      freevar=0
      
      do i=1,4
         if (lock(i) .eq. 0) then
            freecount = freecount + 1
            freevar = i
         endif
      enddo


      if (freecount .eq. 1) then
         if (freevar .eq. 3) then
c           We have Z so var is the X axis
            xmajor = major_tic
            xminor = minor_tic
            abscissa_min = lmin
            abscissa_max = lmax
         else
c           We have var on the Y axis
            ymajor = major_tic
            yminor = minor_tic
            ordinate_min = lmin
            ordinate_max = lmax
         endif
      endif
      return 
      end


      subroutine point_setup (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets up the points based on currently locked vars
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      integer               i
c
c
c     Get the first value for the current command.
c
      do i=1,4
         if(lock(i) .eq. 1) then
            if(i .eq. 1) then
               if(lock(1) .eq. 1) then
                  point_1(i) = xloc
                  point_2(i) = xloc
                  point_3(i) = xloc
               endif
            endif
            if(i .eq. 2) then
               if(lock(2) .eq. 1) then
                  point_1(i) = yloc
                  point_2(i) = yloc
                  point_3(i) = yloc
               endif
            endif
            if(i .eq. 3) then
               if(lock(3) .eq. 1) then
                  point_1(i) = zloc
                  point_2(i) = zloc
                  point_3(i) = zloc
               endif
            endif
            if(i .eq. 4) then
               if(lock(4) .eq. 1) then
                  point_1(i) = tloc
                  point_2(i) = tloc
                  point_3(i) = tloc
               endif
            endif
         endif
      enddo
      error = .false.
      return
      end
      
      subroutine setxpoint (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the x point values when using points
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end, i, im1, numerr
      real                  value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
      error = .false.
c
c     cycle through all 3 points
      numerr=0
      do 10 i = 1, 3 
         im1 = i - 1
         curval = comand(fstval+im1)(1:CMDLEN)
         beg = strbeg(curval)
         end = strend(curval)
c     
c     If a value is not given, do not set x.
c     Otherwise set x to the specified values.
c     
         if ( nblank(curval) .ne. 0 ) then
c     
            call cvatof (curval(beg:end), value, error)
            if ( error ) then
c     
c     The value for this command is incorrect.
c     
               write (message, 110) curval(beg:end),
     &              comand(1)(strbeg(comand(1)):
     &              strend(comand(1)))
               call write_message
               numerr = numerr + 1
            else
               if(value . gt. 
     &              amax1(plmin_scaled(1),plmax_scaled(1)))then
                  write (message, 120) value,
     &                 amax1(plmin_scaled(1),plmax_scaled(1))
                  call write_message
                  value =  amax1(plmin_scaled(1),plmax_scaled(1))
               endif
               if(value . lt. 
     &              amin1(plmin_scaled(1),plmax_scaled(1)))then
                  write (message, 120) value,
     &                 amin1(plmin_scaled(1),plmax_scaled(1))
                  call write_message
                  value =  amin1(plmin_scaled(1),plmax_scaled(1))
               endif

               call unscale (value, 1, domain_slope(1), 
     &              domain_intercept(1), 0.0)
               if(i .eq. 1) point_1(1) = value
               if(i .eq. 2) point_2(1) = value
               if(i .eq. 3) point_3(1) = value
            endif
         elseif(i.eq.3)then
            point_3(1) = point_2(1)
         endif
 10   continue
      if ( numerr .ne. 0 ) then
         error = .true.
      else
         ive_plock(1)=1
         call init_points()
      endif

         
c     
c     Format statements.
c     
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
      subroutine setypoint (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the y point values when using points
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end, i, im1, numerr
      real                  value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
      error = .false.
c
c     cycle through all 3 points
      numerr=0
      do 10 i = 1, 3 
         im1 = i - 1
         curval = comand(fstval+im1)(1:CMDLEN)
         beg = strbeg(curval)
         end = strend(curval)
c     
c     If a value is not given, do not set y
c     Otherwise set y to the specified values.
c     
         if ( nblank(curval) .ne. 0 ) then
c     
            call cvatof (curval(beg:end), value, error)
            if ( error ) then
c     
c     The value for this command is incorrect.
c     
               write (message, 110) curval(beg:end),
     &              comand(1)(strbeg(comand(1)):
     &              strend(comand(1)))
               call write_message
               numerr = numerr + 1
            else
               if(value . gt. 
     &              amax1(plmin_scaled(2),plmax_scaled(2)))then
                  write (message, 120) value,
     &                 amax1(plmin_scaled(2),plmax_scaled(2))
                  call write_message
                  value =  amax1(plmin_scaled(2),plmax_scaled(2))
               endif
               if(value . lt. 
     &              amin1(plmin_scaled(2),plmax_scaled(2)))then
                  write (message, 120) value,
     &                 amin1(plmin_scaled(2),plmax_scaled(2))
                  call write_message
                  value =  amin1(plmin_scaled(2),plmax_scaled(2))
               endif

               call unscale (value, 1, domain_slope(2), 
     &              domain_intercept(2), 0.0)
               if(i .eq. 1) point_1(2) = value
               if(i .eq. 2) point_2(2) = value
               if(i .eq. 3) point_3(2) = value
            endif
         elseif(i.eq.3)then
            point_3(2) = point_2(2)
         endif
 10   continue
      if ( numerr .ne. 0 ) then
         error = .true.
      else
         ive_plock(2)=1
         call init_points()
      endif

         
c     
c     Format statements.
c     
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
      subroutine setzpoint (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the z point values when using points
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end, i, im1, numerr
      real                  value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
      error = .false.
c
c     cycle through all 3 points
      numerr=0
      do 10 i = 1, 3 
         im1 = i - 1
         curval = comand(fstval+im1)(1:CMDLEN)
         beg = strbeg(curval)
         end = strend(curval)
c     
c     If a value is not given, do not set z.
c     Otherwise set z to the specified values.
c     
         if ( nblank(curval) .ne. 0 ) then
c     
            call cvatof (curval(beg:end), value, error)
            if ( error ) then
c     
c     The value for this command is incorrect.
c     
               write (message, 110) curval(beg:end),
     &              comand(1)(strbeg(comand(1)):
     &              strend(comand(1)))
               call write_message
               numerr = numerr + 1
            else
               if(value . gt. 
     &              amax1(plmin_scaled(3),plmax_scaled(3)))then
                  write (message, 120) value,
     &                 amax1(plmin_scaled(3),plmax_scaled(3))
                  call write_message
                  value =  amax1(plmin_scaled(3),plmax_scaled(3))
               endif
               if(value . lt. 
     &              amin1(plmin_scaled(3),plmax_scaled(3)))then
                  write (message, 120) value,
     &                 amin1(plmin_scaled(3),plmax_scaled(3))
                  call write_message
                  value =  amin1(plmin_scaled(3),plmax_scaled(3))
               endif
               call unscale (value, 1, domain_slope(3), 
     &              domain_intercept(3), 0.0)
               
               call unscale (value, 1, domain_slope(3), 
     &              domain_intercept(3), 0.0)
               if(i .eq. 1) point_1(3) = value
               if(i .eq. 2) point_2(3) = value
               if(i .eq. 3) point_3(3) = value
            endif
         elseif(i.eq.3)then
            point_3(3) = point_2(3)
         endif
 10   continue
      if ( numerr .ne. 0 ) then
         error = .true.
      else
         ive_plock(3)=1
         call init_points()
      endif

         
c     
c     Format statements.
c     
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
      subroutine settpoint (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the t point values when using points
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'plotops.icl'
      include 'units.icl'
      include 'message.icl'
      include 'data.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end, i, im1, numerr
      real                  value
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
      error = .false.
c
c     cycle through all 3 points
      numerr=0
      do 10 i = 1, 3 
         im1 = i - 1
         curval = comand(fstval+im1)(1:CMDLEN)
         beg = strbeg(curval)
         end = strend(curval)
c     
c     If a value is not given, do not set t.
c     Otherwise set t to the specified values.
c     
         if ( nblank(curval) .ne. 0 ) then
c     
            call cvatof (curval(beg:end), value, error)
            if ( error ) then
c     
c     The value for this command is incorrect.
c     
               write (message, 110) curval(beg:end),
     &              comand(1)(strbeg(comand(1)):
     &              strend(comand(1)))
               call write_message
               numerr = numerr + 1
            else
               if(value . gt. 
     &              amax1(plmin_scaled(4),plmax_scaled(4)))then
                  write (message, 120) value,
     &                 amax1(plmin_scaled(4),plmax_scaled(4))
                  call write_message
                  value =  amax1(plmin_scaled(4),plmax_scaled(4))
               endif
               if(value . lt. 
     &              amin1(plmin_scaled(4),plmax_scaled(4)))then
                  write (message, 120) value,
     &                 amin1(plmin_scaled(4),plmax_scaled(4))
                  call write_message
                  value =  amin1(plmin_scaled(4),plmax_scaled(4))
               endif
               
               call unscale (value, 1, domain_slope(4), 
     &              domain_intercept(4), 0.0)
               if(i .eq. 1) point_1(4) = value
               if(i .eq. 2) point_2(4) = value
               if(i .eq. 3) point_3(4) = value
            endif
         elseif(i.eq.3)then
            point_3(4) = point_2(4)
         endif
 10   continue
      if ( numerr .ne. 0 ) then
         error = .true.
      else
         ive_plock(4)=1
         call init_points()
      endif

         
c     
c     Format statements.
c     
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
 120  format (1x,f,' is outside the limits, new  value is',f,'.')
c
c
      return
      end
c
c
      function check_points_line()
c-----------------------------------------------------------------------
c     Purpose:
c        This routine checks if a point plot is a line
c-----------------------------------------------------------------------
c
c
      integer check_points_line
      include 'plotops.icl'
      logical same
      integer num_zero, count_same, i
      real A, B, C, D, p1, p2, p3, point1, point2, point3
      dimension p1(3), p2(3), p3(3), point1(4), point2(4), point3(4)

c      character buff*256

      point1=point_1
      point2=point_2
      point3=point_3
      do i = 1, 4
         if (lock(i) .eq. 2) then
            point2(i) = point1(i)
            point3(i) = point1(i)
         endif
      enddo
      count_same = 0
      same = .false.
      if(point_1(1) .eq. point_2(1) .and.
     &   point_3(1) .eq. point_2(1)) then
         count_same = count_same + 1
         same = .true.
         p1(1) = point1(2)
         p1(2) = point1(3)
         p1(3) = point1(4)
         p2(1) = point2(2)
         p2(2) = point2(3)
         p2(3) = point2(4)
         p3(1) = point3(2)
         p3(2) = point3(3)
         p3(3) = point3(4)
      endif
      if(point1(2) .eq. point2(2) .and.
     &   point3(2) .eq. point2(2)) then
         count_same = count_same + 1
         same = .true.
         p1(1) = point1(1)
         p1(2) = point1(3)
         p1(3) = point1(4)
         p2(1) = point2(1)
         p2(2) = point2(3)
         p2(3) = point2(4)
         p3(1) = point3(1)
         p3(2) = point3(3)
         p3(3) = point3(4)
      endif
      if(point1(3) .eq. point2(3) .and.
     &   point3(3) .eq. point2(3)) then 
         count_same = count_same + 1
         same = .true.
         p1(1) = point1(1)
         p1(2) = point1(2)
         p1(3) = point1(4)
         p2(1) = point2(1)
         p2(2) = point2(2)
         p2(3) = point2(4)
         p3(1) = point3(1)
         p3(2) = point3(2)
         p3(3) = point3(4)
      endif
      if(point1(4) .eq. point2(4) .and.
     &   point3(4) .eq. point2(4)) then
         count_same = count_same + 1
         same = .true.
         p1(1) = point1(1)
         p1(2) = point1(2)
         p1(3) = point1(3)
         p2(1) = point2(1)
         p2(2) = point2(2)
         p2(3) = point2(3)
         p3(1) = point3(1)
         p3(2) = point3(2)
         p3(3) = point3(3)
      endif
      
      if (same .eq. .false.) then
          check_points_line = -1
          return
       endif
      
       if (count_same .eq. 4) then 
          check_points_line = 0
          return
       endif
      
      A =  p1(2)*(p2(3)-p3(3)) + 
     &     p2(2)*(p3(3)-p1(3)) + 
     &     p3(2)*(p1(3)-p2(3))

      B =  p1(1)*(p2(3)-p3(3)) + 
     &     p2(1)*(p3(3)-p1(3)) + 
     &     p3(1)*(p1(3)-p2(3))

      C =  p1(1)*(p2(2)-p3(2)) + 
     &     p2(1)*(p3(2)-p1(2)) + 
     &     p3(1)*(p1(2)-p2(2))

      D =  p1(1)*((p2(2)*p3(3)) -
     &     (p3(2)*p2(3))) - 
     &     p2(1)*((p1(2)*p3(3)) - 
     &     (p3(2)*p1(3))) +
     &     p3(1)*((p1(2)*p2(3)) - 
     &     (p2(2)*p1(3)))

c     this yields Ax - By + Cz -D = 0 
c     Find out how many of A, B and C are zero. 

      write(6,*)'p1',p1
      write(6,*)'p2',p2
      write(6,*)'p3',p3
      num_zero = 0
      if(A .eq. 0) num_zero = num_zero + 1
      if(B .eq. 0) num_zero = num_zero + 1
      if(C .eq. 0) num_zero = num_zero + 1

      if( num_zero .eq. 2 .or. num_zero .eq. 1) then
c     We have a plane
         check_points_line = 2
        return
      elseif (num_zero .eq. 3) then
c     We have a line as we already caught the point up top
C     a,b,c ==0 -> d==0 so a,b or c != 0
         check_points_line = 1
         return
      else
c     we don't do bigger than planes right now and they must be perpendicular
c     to at least one dimention.
         check_points_line = -1
         return
      endif
      end

      subroutine switch_slicer (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine switch slicer window types
c     Arguments:
c        error  logi  output  error flag.
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'plotops.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN)   curval
      integer               beg, end
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
      error = .false.
c
c     cycle through all 3 points
         curval = comand(fstval)(1:CMDLEN)
         beg = strbeg(curval)
         end = strend(curval)
c     
c     
         if ( nblank(curval) .ne. 0 ) then
c     
            if(curval(beg:beg) .eq. "P" .or. 
     &          curval (beg:beg) .eq. "p") then
               use_buttons=0
               call switch_slicers(0)
            else
               use_buttons=1
               call switch_slicers(1)
            endif
         else
            if(use_buttons .eq. 1) then
               use_buttons=0
               call switch_slicers(0)
            else
               use_buttons=1
               call switch_slicers(1)
            endif
         endif
         return
         end
               


      subroutine ive_traj_depth_check ()
      include 'plotops.icl'
      include 'traj.icl'
      include 'color.icl'
      
      integer strbeg, strend, vid
      
      if(plotyp2 .eq. 'TRAJECTORY') then
         if(traj_background_field(strbeg(traj_background_field):
     &        strend(traj_background_field)) .eq. 'depth') then
            if(lock(1) .eq. 0 .and. lock(2) .eq. 0 .and. 
     &           (lock(3) .ne. 0 .and. lock(4) .ne. 0)) then
               vid = -3
            elseif(lock(1) .eq. 0 .and. lock(3) .eq. 0 .and. 
     &              (lock(2) .ne. 0 .and. lock(4) .ne. 0)) then
               vid = -2
            elseif(lock(2) .eq. 0 .and. lock(3) .eq. 0 .and. 
     &              (lock(1) .ne. 0 .and. lock(4) .ne. 0)) then
               vid = -1
            else 
               return
            endif
c            call get_traj_atts(vid, traj_val, traj_step)
            if (traj_val .gt. -1e19) then
               traj_val_set = .true.
            else
               traj_val_set = .false.
            endif
            if (traj_step .ne. 0) then
               traj_step_set = .true.
            else
               traj_step_set = .false.
            endif
         endif
      endif
      return
      end
