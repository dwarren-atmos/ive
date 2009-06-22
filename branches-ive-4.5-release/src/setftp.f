      subroutine setftp (error)
c-----------------------------------------------------------------------
c     $Id: setftp.f,v 1.7 2002/08/09 19:58:00 warren Exp $
c     Purpose:
c        This subroutine sets the desIred fill type for contour plots. 
c           Valid fill types are LINES (draws contour lines), SOLID 
c           (use color fill to produce color contour plot).
c           It is only necessary to specify either L, or S, since these
c           are unambiguous.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setftp.f,v $
c      Revision 1.7  2002/08/09 19:58:00  warren
c      Fixed for Linux and tested with DEC sun etc.
c      most items inside #ifdef's, or were general cleanup/fixes
c
c      Revision 1.6  1996/10/11 01:13:07  warren
c      To fill or not to fill is no longer the only question. Now there is a seperate
c      question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c      Labels are not fixed yet.
c
c      Revision 1.5  1996/01/12 16:11:59  warren
c      Transfer of new pattern stuff from 3.x and beginning of color fixes
c      (color vectors, correct collors on the pulldowan menus)
c
c      Revision 1.4  1995/12/15 22:33:23  warren
c      moved patterns into 4.0 and implemented the list of fixes and changes from
c      Curtis.
c
c Revision 1.3  1995/10/18  19:18:24  warren
c Stipples from 3.3, First crack at mouse windowing and put window_points_
c into a header file and out of the mouse stuff and slicers. Touched almost
c everything it seems.
c
c Revision 1.2  1994/11/22  22:48:04  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.1.1.1  1993/04/07  19:40:23  harry
c Initial CVS checkin of IVE.
c
c Revision 2.0  1992/07/01  21:14:17  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
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
      character *(cmdlen) curval
      integer             ibeg, iend, i, oldtype,oldfreq
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         oldtype=filtyp
         oldfreq=fill_frequency
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:iend), iend-ibeg+1)
         if ( curval(ibeg:ibeg) .eq. 'L' ) then
c           Produce a line plot.
            fill_frequency=NEVER
         elseif ( curval(ibeg:iend) .eq. 'SOLIDALL') then
c           Produce a color filled plot for all overlays
            filtyp = SOLID
            fill_frequency=ALWAYS
         elseif ( curval(ibeg:ibeg) .eq. 'S' ) then
c           Produce a color filled plot for first overlay only.
            filtyp = SOLID
            fill_frequency=EXECPT_OVERLAY
         elseif ( curval(ibeg:ibeg) .eq. 'P') then
c           Produce a color filled plot for all overlays
            filtyp = PATTERN
            fill_frequency=EXECPT_OVERLAY
         else
c           The value for this command is incorrect.
            write (message, 110) curval(ibeg:strend(curval)),
     &                     comand(1)(strbeg(comand(1)):
     &                     strend(comand(1)))
            call write_message
            error = .true.
         endif
      endif
      write (message,*)'filltype is obsolete, please use fill_type'
     &     //' and fill_contours instead'
      call write_message
      if(filtyp .eq. PATTERN .and. fill_frequency .ne. NEVER) then
         cont_int_bck=cont_int
         num_vals_bck=num_vals
         do i=1,40
            cont_values_bck(i)=cont_values(i)
         enddo
         call do_stipple(1)
         call check_pattern()
      elseif(oldtype .eq. PATTERN .and. oldfreq .ne. NEVER) then
         cont_int=cont_int_bck
         num_vals=num_vals_bck
         do i=1,40
            cont_values(i)=cont_values_bck(i)
         enddo
         call do_stipple(0)
      endif
      
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
      subroutine setfill_ptn (error)
c-----------------------------------------------------------------------
c     Purpose:
c      This subroutine sets the desIred fill pattern for contour plots. 
c      Valid fill types are 1,2,3,4 and 5
c      increasing numbers increase dot size
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
      include 'attributes.icl'
c
c
c
c     Argument declarations.
c
      logical       error
c
c
c     Local variable declarations.
c
      integer       i, numval
c
c
c     External function declarlations.
c
      integer       nblank, strbeg, strend
c
c
c 
c
c     Find out how many nonblank values are specified. If there are no
c     nonblank values, set the plot window to its maximum size and 
c     return.
c
      numval = 0
      do 5 i = fstval, maxcom
         if ( nblank(comand(i)) .ne. 0 )  numval = numval + 1
5     continue
      if ( numval .ne. 3 ) then
         write (message, 200) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      endif
      call upcase(comand(fstval)(1:strend(comand(fstval))), 
     &     strend(comand(fstval)) - strbeg(comand(fstval)) +1)
      if ( comand(fstval)(1:1) .ne. 'P') then
         write (message, 200) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      endif
      if ( comand(fstval)(4:4) .eq. '1') then
         do i=1,numval-1
            if ( comand(fstval+i)(1:cmdlen) .ne. ' ' ) then
               if(comand(fstval+i)(1:1) .eq. 'D') then
                  pat1(i) = 1E38
               else
                  call cvatof (comand(fstval+i)(1:cmdlen),
     &                 pat1(i), error) 
                  if(error) pat1(i) = 1E38
               endif
            endif
         enddo
      elseif ( comand(fstval)(4:4) .eq. '2') then
         do i=1,numval-1
            if ( comand(fstval+i)(1:cmdlen) .ne. ' ' ) then
               if(comand(fstval+i)(1:1) .eq. 'D') then
                  pat2(i) = 1E38
               else
                  call cvatof (comand(fstval+i)(1:cmdlen),
     &                        pat2(i), error) 
                  if(error) pat2(i) = 1E38
               endif
            endif
         enddo
      elseif ( comand(fstval)(4:4) .eq. '3') then
         do i=1,numval-1
            if ( comand(fstval+i)(1:cmdlen) .ne. ' ' ) then
               if(comand(fstval+i)(1:1) .eq. 'D') then
                  pat3(i) = 1E38
               else
                  call cvatof (comand(fstval+i)(1:cmdlen),
     &                 pat3(i), error) 
                  if(error) pat3(i) = 1E38
               endif
            endif
         enddo
      elseif ( comand(fstval)(4:4) .eq. '4') then
         do i=1,numval-1
            if ( comand(fstval+i)(1:cmdlen) .ne. ' ' ) then
               if(comand(fstval+i)(1:1) .eq. 'D') then
                  pat4(i) = 1E38
               else
                  call cvatof (comand(fstval+i)(1:cmdlen),
     &                 pat4(i), error) 
                  if(error) pat4(i) = 1E38
               endif
            endif
         enddo
      elseif ( comand(fstval)(4:4) .eq. '5') then
         do i=1,numval-1
            if ( comand(fstval+i)(1:cmdlen) .ne. ' ' ) then
               if(comand(fstval+i)(1:1) .eq. 'D') then
                  pat5(i) = 1E38
               else
                  call cvatof (comand(fstval+i)(1:cmdlen),
     &                 pat5(i), error) 
                  if(error) pat5(i) = 1E38
               endif
            endif
         enddo
      elseif ( comand(fstval)(4:4) .eq. '6') then
         do i=1,numval-1
            if ( comand(fstval+i)(1:cmdlen) .ne. ' ' ) then
               if(comand(fstval+i)(1:1) .eq. 'D') then
                  pat6(i) = 1E38
               else
                  call cvatof (comand(fstval+i)(1:cmdlen),
     &                 pat6(i), error) 
                  if(error) pat6(i) = 1E38
               endif
            endif
         enddo
      else
         write (message, 200) comand(1)(strbeg(comand(1)):
     &        strend(comand(1)))
         call write_message
         error = .true.
      endif
      call check_pattern()
      return
 200  format (1x,'Command ',a,' requires a pattern [pat1 - pat5]',
     &     ',startval,endval')
 210  format (1x,a,' is an incorrect value for command ',a,'.')
      
      end
      
      
      subroutine check_pattern
c-----------------------------------------------------------------------
c     Purpose:
c     This routine checks if we are doing patterns and if so sets up
c     the plotting environment.
c
c-----------------------------------------------------------------------
c
      include 'plotops.icl'
      character *256 buff
      character *64 buff2
      integer  strbeg, strend
      integer start
      
      if(filtyp .ne. PATTERN) return
      start = 0
      buff2=' '
      buff='contour_values='
      
      if(pat1(1) .ne. 1E38 .and. pat1(2) .ne. 1E38) then
         write(buff2,*)pat1(1), ',', pat1(2)
         start=1
         buff=buff(strbeg(buff):strend(buff))//
     &        buff2(strbeg(buff2):strend(buff2))
      endif
      
      if(pat2(1) .ne. 1E38 .and. pat2(2) .ne. 1E38) then
         if(start .eq. 0) then
            write(buff2,*)pat2(1), ',', pat2(2)
            start=1
         else
            write(buff2,*)',', pat2(1), ',', pat2(2)
         endif
         buff=buff(strbeg(buff):strend(buff))//
     &        buff2(strbeg(buff2):strend(buff2))
      endif
      
      if(pat3(1) .ne. 1E38 .and. pat3(2) .ne. 1E38) then
         if(start .eq. 0) then
            write(buff2,*)pat3(1), ',', pat3(2)
            start=1
         else
            write(buff2,*) ',', pat3(1), ',', pat3(2)
         endif
         buff=buff(strbeg(buff):strend(buff))//
     &        buff2(strbeg(buff2):strend(buff2))
      endif
      if(pat4(1) .ne. 1E38 .and. pat4(2) .ne. 1E38) then
         if(start .eq. 0) then
            write(buff2,*)pat4(1), ',', pat4(2)
            start=1
         else
            write(buff2,*)',', pat4(1), ',', pat4(2)
         endif
         buff=buff(strbeg(buff):strend(buff))//
     &        buff2(strbeg(buff2):strend(buff2))
      endif
      if(pat5(1) .ne. 1E38 .and. pat5(2) .ne. 1E38) then
         if(start .eq. 0) then
            write(buff2,*)pat5(1), ',', pat5(2)
            start=1
         else
            write(buff2,*)',', pat5(1), ',', pat5(2)
         endif
         buff=buff(strbeg(buff):strend(buff))//
     &        buff2(strbeg(buff2):strend(buff2))
      endif
      if(pat6(1) .ne. 1E38 .and. pat6(2) .ne. 1E38) then
         if(start .eq. 0) then
            write(buff2,*)pat6(1), ',',
     &           pat6(2)
            start=1
         else
            write(buff2,*)',', pat6(1), ',', pat6(2)
         endif
         buff=buff(strbeg(buff):strend(buff))//
     &        buff2(strbeg(buff2):strend(buff2))
      endif
      
      if (start .gt. 0) then
         call driver (buff(strbeg(buff):strend(buff))//char(0), 1)
      endif
      
      return
      end

      subroutine set_ftp (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine sets the desIred fill type for contour plots. 
c           Valid SOLID (use color fill to produce color contour plot),
c           or PATTERN (use pattern to fill areas)
c           It is only necessary to specify either S, or P, since these
c           are unambiguous.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
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
      character *(cmdlen) curval
      integer             ibeg, iend, i, oldtype, oldfreq
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.

c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         oldtype=filtyp
         oldfreq=fill_frequency
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:iend), iend-ibeg+1)
         if ( curval(ibeg:ibeg) .eq. 'S' ) then
c           Produce a color filled plot for first overlay only.
            filtyp = SOLID
         elseif ( curval(ibeg:ibeg) .eq. 'P') then
c           Produce a color filled plot for all overlays
            filtyp = PATTERN
         else
c           The value for this command is incorrect.
            write (message, 110) curval(ibeg:strend(curval)),
     &                     comand(1)(strbeg(comand(1)):
     &                     strend(comand(1)))
            call write_message
            error = .true.
         endif
      endif
      if(filtyp .eq. PATTERN .and. fill_frequency .ne. NEVER) then
         cont_int_bck=cont_int
         num_vals_bck=num_vals
         do i=1,40
            cont_values_bck(i)=cont_values(i)
         enddo
         call do_stipple(1)
         call check_pattern() 
      elseif(oldtype .eq. PATTERN .and. oldfreq .ne. NEVER) then
         cont_int=cont_int_bck
         num_vals=num_vals_bck
         do i=1,40
            cont_values(i)=cont_values_bck(i)
         enddo
         call do_stipple(0)
      endif
      
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end

      subroutine set_fct (error)
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine sets the desIred fill frequency.
c        Always, Except on overlay, Never.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'message.icl'
      include 'plotops.icl'
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
      character *(cmdlen) curval
      integer             ibeg, iend, i, oldtype, oldfreq
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
c
c
      error = .false.
c
c     Get the first value for the current command.
c
      curval = comand(fstval)(1:cmdlen)
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
         oldtype=filtyp
         oldfreq=fill_frequency
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:iend), iend-ibeg+1)
         if ( curval(ibeg:ibeg) .eq. 'N' ) then
c           Produce a line plot.
            fill_frequency=NEVER
         elseif ( curval(ibeg:ibeg) .eq. 'A') then
c           Produce a color filled plot for all overlays
            fill_frequency=ALWAYS
         elseif ( curval(ibeg:ibeg) .eq. 'E' ) then
c           Produce a color filled plot for first overlay only.
            fill_frequency=EXECPT_OVERLAY
         else
c           The value for this command is incorrect.
            write (message, 110) curval(ibeg:strend(curval)),
     &                     comand(1)(strbeg(comand(1)):
     &                     strend(comand(1)))
            call write_message
            error = .true.
         endif
      endif
      if(filtyp .eq. PATTERN .and. fill_frequency .ne. NEVER) then
         cont_int_bck=cont_int
         num_vals_bck=num_vals
         do i=1,40
            cont_values_bck(i)=cont_values(i)
         enddo
         call do_stipple(1)
         call check_pattern()
      elseif(oldtype .eq. PATTERN .and. oldfreq .ne. NEVER) then
         cont_int=cont_int_bck
         num_vals=num_vals_bck
         do i=1,40
            cont_values(i)=cont_values_bck(i)
         enddo
         call do_stipple(0)
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,a,' is an incorrect value for command ',a,'.')
c
c
      return
      end
