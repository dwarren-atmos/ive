      subroutine lock_color(error)
c---------------------------------------------------------------------------
c     $Id: lock_color.f,v 1.11 1999/08/25 17:50:17 warren Exp $
c     Purpose:
c        This routine locks and frees the color table and alongg with it
c        it locks the contour value.
c     History:
c     $Log: lock_color.f,v $
c     Revision 1.11  1999/08/25 17:50:17  warren
c     swapped from dr2dv.f to dr2dv.F due to #ifdef sun needed as sun does not
c     support the floor function, but has floor_r instead.
c
c     Revision 1.10  1999/07/23 20:03:38  warren
c     Background code for the lock all fields option.
c
c     Revision 1.9  1998/03/04 19:14:02  davidive
c     First cut at trajectories and trajectory widget using colors to show depth
c     and arrows to show direction.
c
c     Revision 1.8  1997/06/06 20:17:55  harry
c     Fix color locking - distinguish value from contour interval.
c
c     Revision 1.7  1997/01/29 23:10:00  warren
c     Cleaned up and fixed Dales Color problem
c
c Revision 1.6  1995/02/14  22:21:53  warren
c Fixed color lock loop and did proper cast to int of XPointer for Alphas.
c
c Revision 1.5  1994/10/12  21:54:44  warren
c Fixed infinite loop bug in color locking caused by selecting specific
c contour values.
c
c Revision 1.4  1994/09/20  17:11:12  warren
c all lock of coor to value: lock=contour val, color table entry.
c Also fixe widget to update correctly
c
c Revision 1.3  1994/09/07  22:07:09  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.2  1994/06/28  17:43:33  warren
c First cut at fancy tic marks.
c
c Revision 1.1.1.1  1993/04/07  19:40:06  harry
c Initial CVS checkin of IVE.
c
c Revision 1.2  1992/12/10  21:32:25  millerp
c Remove unused variables.
c
c Revision 1.1  1992/10/15  23:01:21  warren
c Initial revision
c
c
c---------------------------------------------------------------------------
      include 'incommands.icl'
      include 'plotops.icl'
      include 'message.icl'
      include 'color.icl'
      include 'data.icl'
      include 'attributes.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(80)   curval
      integer           beg, end,ival
      real              rval
c
c
c     External function declarations.
c
      integer strbeg, strend, nblank
c
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c
c
      if(num_vals .le. 0) then
         if(curval(1:1) .eq. 'l' .or. curval(1:1) .eq. 'L') then
            if(cont_int .eq. 0) then
               write(message, 100)
               call write_message
               call driver('lockcolortable=free', 0)
               error = .true.
               return
            endif
c
c           midval is set in cpline.f based on what was used in the last plot.
c
            lock_field = field
            lock_entry = entry_used
            lock_int = cont_int
            cont_int_lock_var = cont_int
            lock_set_cint = num_vals
         elseif(curval(1:1) .eq. 'f' .or. curval(1:1) .eq. 'F') then
            lock_field = ' '
            lock_entry = 0
            lock_int = 0.
         else
c     
c           Get the contour level to lock.
c     
            call cvatof (curval(strbeg(curval):strend(curval)), 
     &                   rval, error)
            if ( error ) then
               midval = 0
               lock_field = ' '
               lock_entry = 0
               lock_int = 0.
               write (message, 102) 
     &              curval(strbeg(curval):strend(curval)), 
     &              comand(1)(strbeg(comand(1)):strend(comand(1)))
               call write_message
               return
            endif
            midval = rval
c
c          Get the color table entry to lock to it.
c     
            curval = comand(fstval+1)(1:CMDLEN)
            if ( nblank(curval) .ne. 0 ) then
               call cvatoi (curval(strbeg(curval):strend(curval)), 
     &                      ival, error)
               if ( error ) then
                  midval = 0
                  lock_field = ' '
                  lock_entry = 0
                  write (message, 103) 
     &                 curval(strbeg(curval):strend(curval)), 
     &                 comand(1)(strbeg(comand(1)):strend(comand(1)))
                  call write_message
                  return
               endif
               lock_entry = ival
               lock_field = field
               cont_int_lock_var = cont_int
               lock_int = -midval
               lock_set_cint = num_vals
            endif                                
         endif                                   !curval =l,f or num
      else                                       !num_vals le 0
c         if(curval(1:1) .eq. 'l' .or. curval(1:1) .eq. 'L') then
c            write(message, 101)
c            call write_message
c         endif
c         call driver('lockcolortable=free', 0)
c         error = .true.
c         return
      endif
      error = .false.
c
c----------FORMAT Statements________________
c
 100  format (1x, 'You must fix the contour interval or values '
     &             'before locking the color table')
 101  format (1x, 'When using specified contour values the color table'
     &            ' is automatically locked since the contours do not'
     &            ' change')
 102  format (1x,a,' is an incorrect value for the contour level'
     &               ' for command ',a,'. The colors are now free.')
 103  format (1x,a,' is an incorrect value for the color table entry'
     &               ' for command ',a,'. The colors are now free.')
      return 
      end



      subroutine lock_trajectory_color(error)
c---------------------------------------------------------------------------
      include 'incommands.icl'
      include 'plotops.icl'
      include 'message.icl'
      include 'color.icl'
      include 'data.icl'
      include 'attributes.icl'
c
c
c     Argument declarations.
c
      logical error
c
c
c     Local variable declarations.
c
      character *(80)   curval
      integer           beg, end,ival
      real              rval
c
c
c     External function declarations.
c
      integer strbeg, strend, nblank
c
c
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:CMDLEN)
      beg = strbeg(curval)
      end = strend(curval)
c
c
c
c
      if(num_vals .le. 0) then
         if(curval(1:1) .eq. 'l' .or. curval(1:1) .eq. 'L') then
            if(cont_int .eq. 0) then
               write(message, 100)
               call write_message
               call driver('lockcolortable=free', 0)
               error = .true.
               return
            endif
c
c           midval is set in cpline.f based on what was used in the last plot.
c
            lock_field = field
            lock_entry = entry_used
            lock_int = cont_int
            lock_set_cint = num_vals
         elseif(curval(1:1) .eq. 'f' .or. curval(1:1) .eq. 'F') then
            lock_field = ' '
            lock_entry = 0
            lock_int = 0.
         else
c     
c           Get the contour level to lock.
c     
            call cvatof (curval(strbeg(curval):strend(curval)), 
     &                   rval, error)
            if ( error ) then
               midval = 0
               lock_field = ' '
               lock_entry = 0
               lock_int = 0.
               write (message, 102) 
     &              curval(strbeg(curval):strend(curval)), 
     &              comand(1)(strbeg(comand(1)):strend(comand(1)))
               call write_message
               return
            endif
            midval = rval
c
c          Get the color table entry to lock to it.
c     
            curval = comand(fstval+1)(1:CMDLEN)
            if ( nblank(curval) .ne. 0 ) then
               call cvatoi (curval(strbeg(curval):strend(curval)), 
     &                      ival, error)
               if ( error ) then
                  midval = 0
                  lock_field = ' '
                  lock_entry = 0
                  write (message, 103) 
     &                 curval(strbeg(curval):strend(curval)), 
     &                 comand(1)(strbeg(comand(1)):strend(comand(1)))
                  call write_message
                  return
               endif
               lock_entry = ival
               lock_field = field
               lock_int = -midval
               lock_set_cint = num_vals
            endif                                
         endif                                   !curval =l,f or num
      else                                       !num_vals le 0
c         if(curval(1:1) .eq. 'l' .or. curval(1:1) .eq. 'L') then
c            write(message, 101)
c            call write_message
c         endif
c         call driver('lockcolortable=free', 0)
c         error = .true.
c         return
      endif
      error = .false.
c
c----------FORMAT Statements________________
c
 100  format (1x, 'You must fix the contour interval or values '
     &             'before locking the color table')
 101  format (1x, 'When using specified contour values the color table'
     &            ' is automatically locked since the contours do not'
     &            ' change')
 102  format (1x,a,' is an incorrect value for the contour level'
     &               ' for command ',a,'. The colors are now free.')
 103  format (1x,a,' is an incorrect value for the color table entry'
     &               ' for command ',a,'. The colors are now free.')
      return 
      end


      subroutine ignore_field_color(error)
c---------------------------------------------------------------------------
      include 'incommands.icl'
      include 'plotops.icl'
      include 'message.icl'
      include 'color.icl'
      include 'data.icl'
      include 'attributes.icl'
c
c
c     Argument declarations.
c
      logical error
c
c
c     Local variable declarations.
c
      character *(80)   curval
      integer           beg, end,ival
      real              rval
c
c
c     External function declarations.
c
      integer strbeg, strend, nblank
c
c
c
c     Get the first value for the current command.
c
      if(num_vals .le. 0) then
         if(ignore_lock_var .gt. 0) then
            ignore_lock_var = 0
         else 
            ignore_lock_var=1
         endif
      else
         curval = comand(fstval)(1:CMDLEN)
         beg = strbeg(curval)
         end = strend(curval)
         call upcase (curval(beg:beg), (end - beg + 1))
         if (curval(beg:end) .eq. 'ON') then
            ignore_lock_var=1
         else
            ignore_lock_var=0
         endif
      endif
      end

