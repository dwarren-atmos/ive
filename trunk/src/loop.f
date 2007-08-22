      subroutine loop(iflag, error)
c-----------------------------------------------------------------------
c     $Id: loop.f,v 1.14 2001/03/23 19:35:34 harryive Exp $
c     Purpose:
c        This subroutine computes and runs loops.
c
c     Syntax of command:
c
c     LOOP=type,start,inc,end[,batch]
c
c     type = XLOC, YLOC, ZLOC or TIME
c
c     Algorithm:
c
c     1. save current image to pixmap if current = start
c     2. generate new images, saving the pixmaps
c     3. loop the pixmaps
c     4. when done, destroy the pixmaps
c
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: loop.f,v $
c      Revision 1.14  2001/03/23 19:35:34  harryive
c      Add "batch" mode to looping for use with Xvfb - in this mode their is no
c      loop window, but the pixmaps are still created.
c
c      Revision 1.13  2000/06/12 21:20:35  warren
c      Fixed Dale's end of loop time stuck bug.
c
c      Revision 1.12  1998/12/29 16:40:22  harry
c      Properly loop plots with "draw_line" command.
c
c      Revision 1.11  1997/10/29 18:26:50  harry
c      Make sure that plots that do not depend on the loop parameter do not change.
c
c      Revision 1.10  1997/06/03 19:14:20  davidive
c      The big mouse slicing changeover. We have the new form, only requires you
c      set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c      always runs through init_points_ to catch all these things.
c      note: only actually works when two dimentions have points set. Needs to
c      be expanded into 1, 3 or 4.
c
c      Revision 1.9  1997/04/18 22:09:40  harry
c      Fix routines to handle user errors more elegantly.
c
c      Revision 1.8  1997/04/15 17:24:40  harry
c      Remove time_tag - use multiple files instead.
c      Various fixes to vector/scalar looping.
c
c Revision 1.7  1996/10/10  14:03:07  warren
c removed duplicat declaration.
c
c      Revision 1.6  1996/10/09 18:54:41  harry
c      Add the ability to loop on file names if time value is part of name,
c      and there is only one time per file.
c
c Revision 1.5  1995/06/23  21:16:25  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.4  1993/07/07  21:59:51  harry
c Fix error messages.
c
c Revision 1.3  1993/06/18  17:19:50  harry
c Make long loops work by adding save_state.c and changing loop.f and
c restore_state.c.
c
c Revision 1.2  1993/06/11  20:02:06  harry
c Change loop to use NEW_PLOT & OVERLAY_PLOT and check # frames.
c Add NEW_PLOT and OVERLAY_PLOT to list of commands to ignore in
c restore_state.
c
c Revision 1.1.1.1  1993/04/07  19:40:06  harry
c Initial CVS checkin of IVE.
c
c Revision 2.15  1993/01/06  17:18:58  harry
c Change a segment from one each plot to one each overlay.
c
c Revision 2.14  1992/12/23  21:15:24  harry
c Fix start value processing.
c
c Revision 2.13  1992/12/23  19:05:04  harry
c Add start value processing.
c
c Revision 2.12  1992/12/17  22:58:41  harry
c Add iflag to call.
c
c Revision 2.11  1992/11/17  18:40:37  harry
c Documentation and error message corrections.
c
c Revision 2.10  1992/11/10  16:22:17  harry
c Use parameter names for command numbers.
c
c Revision 2.9  1992/10/09  18:18:21  harry
c Add scaling.
c
c Revision 2.8  1992/10/08  23:18:15  harry
c Use scaled coordinate values rather that original ones.
c
c Revision 2.7  1992/10/08  18:21:45  harry
c Correct error messages.
c
c Revision 2.6  1992/10/07  22:19:12  harry
c Add check for locked dimension and call to loop_setup_update.
c
c Revision 2.5  1992/09/29  19:41:16  harry
c Correct zloc, add xloc and yloc loops.
c
c Revision 2.4  1992/09/24  16:00:22  harry
c Put tloop/vloop back in.
c
c Revision 2.3  1992/09/23  23:12:14  harry
c Change VLOC to ZLOC.  Use restore_state instead of segment list.
c
c Revision 2.2  1992/08/26  19:33:29  harry
c Fix format for writing out loop variable.
c
c Revision 2.1  1992/08/11  16:17:13  harry
c Change to use widgets.
c
c Revision 2.0  1992/07/01  21:15:55  millerp
c Released version rereving to 2.0
c
c Revision 1.6  1992/06/18  22:33:19  warren
c changed PARAMETER statement for TIME and VLOC to call  chkcmd
c
c Revision 1.5  1992/06/09  21:07:52  harry
c Add initial call to save_pixmap
c
c Revision 1.4  1992/06/01  20:16:20  harry
c Correct code for time loops.
c
c Revision 1.3  1992/05/29  22:33:46  millerp
c *** empty log message ***
c
c Revision 1.2  1992/05/22  16:02:22  millerp
c Added missing end statement.
c
c Revision 1.1  1992/05/21  17:46:46  harry
c Initial revision
c
c
c-----------------------------------------------------------------------
c
c
c
      include 'cmndlst.icl'
      include 'data.icl'
      include 'incommands.icl'
      include 'loopflag.icl'
      include 'message.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'units.icl'

      data loop_flag/4*.FALSE./

      integer iflag, flag
      logical error, batch

      integer i, nblank, numval, strbeg, strend, keynum, dim, loop_plot
      real f, inc, endnum, limit, startnum
      character*(CMDLEN) com, type, increment, endval, string,
     &     start
      logical err

C
C     error is always true, since we do NOT want this command added to
C     the command list.
C
      error = .TRUE.
C
C     Check number of parameters
C
      numval = 0
      do i = FSTVAL, MAXCOM
         if (nblank(comand(i)) .ne. 0) numval = numval + 1
      enddo
      com = comand(1)(strbeg(comand(1)):strend(comand(1)))
      if (numval.lt.2.or.numval.gt.5) then
         write (message,100) com(strbeg(com):strend(com))
         call write_message
         return
      endif
      type = comand(FSTVAL)(strbeg(comand(FSTVAL)):
     &     strend(comand(FSTVAL)))
      call chkcmd(type, keynum, err)
      if (err.or.(keynum.ne.P_XLOCATION.and.keynum.ne.P_YLOCATION.and.
     &     keynum.ne.P_ZLOCATION.and.keynum.ne.P_TIME)) then
         write (message,110) type(strbeg(type):strend(type)),
     &        com(strbeg(com):strend(com))
         call write_message
         return
      endif
      increment = comand(FSTVAL+2)(strbeg(comand(FSTVAL+2)):
     &     strend(comand(FSTVAL+2)))
      call cvatof(increment, inc, err)
      if (err.or.inc.eq.0.) then
         write (message, 110)
     &        increment(strbeg(increment):strend(increment)),
     &        com(strbeg(com):strend(com))
         call write_message
         return
      endif
      if (keynum.eq.P_XLOCATION) then
         f = xloc
         dim = 1
      else if (keynum.eq.P_YLOCATION) then
         f = yloc
         dim = 2
      else if (keynum.eq.P_ZLOCATION) then
         f = zloc
         dim = 3
      else
         f = tloc
         dim = 4
      endif
      call scale(f, 1, domain_slope(dim), domain_intercept(dim), 0.0)
      if (inc.gt.0) then
         limit = max(plmin_scaled(dim),plmax_scaled(dim))
      else
         limit = min(plmin_scaled(dim),plmax_scaled(dim))
      endif
      if (nblank(comand(FSTVAL+3)) .eq. 0) then
         endnum = limit
      else
         endval = comand(FSTVAL+3)(strbeg(comand(FSTVAL+3)):
     &        strend(comand(FSTVAL+3)))
         call cvatof(endval, endnum, err)
         if (err) then
            write(message, 110) endval(strbeg(endval):strend(endval)),
     &           com(strbeg(com):strend(com))
            call write_message
            return
         endif
         if ((inc.gt.0..and.endnum.gt.limit).or.
     &        inc.lt.0..and.endnum.lt.limit) then
            write(message, 140) endnum, limit
            call write_message
            endnum = limit
         endif
      endif
      if (nblank(comand(FSTVAL+1)) .eq. 0) then
         startnum = f
      else
         start = comand(FSTVAL+1)(strbeg(comand(FSTVAL+1)):
     &        strend(comand(FSTVAL+1)))
         call cvatof(start, startnum, err)
         if (err) then
            write(message, 110) start(strbeg(start):strend(start)),
     &           com(strbeg(com):strend(com))
            call write_message
            return
         endif
      endif
      if (nblank(comand(FSTVAL+4)) .eq. 0) then
         batch = 0
      else
         batch = 1
      endif
      if (iflag.eq.0) call loop_setup_update(dim, startnum, inc, endnum)
c
c     Is this a meaningful loop?
c
      if ((inc.gt.0..and.startnum.gt.endnum) .or.
     &     (inc.lt.0..and.startnum.lt.endnum)) then
         message = 'Zero length loop - check start, end, and increment'
         call write_message
         return
      endif
c
c     make sure previous loop is destroyed.
c
      call loop_quit

      if (startnum.eq.f) then
         call save_pixmap(batch)
         f = f + inc
      else
         f = startnum
      endif

      loop_plot = curlist
      call save_state(loop_plot-1, curoverlay(loop_plot))
      do while ((inc.gt.0..and.f.le.endnum).or.
     &     (inc.lt.0..and.f.ge.endnum))
         write(string, 200) type(strbeg(type):strend(type)), f

         do i=1, curoverlay(loop_plot)
            loop_flag(dim) = .TRUE.
            call restore_state(i-1, flag)

c
c     flag is one for DRAW_LINE, DRAW_LINE_MAP, DRAW_POINT, 
c     or DRAW_POINT_MAP command only
c
            if (flag .eq. 0) then
               if (lock(dim) .eq. 1) then
                  loop_flag(dim) = .FALSE.
                  call driver(string,0)
               endif

               if (i .EQ. 1) then
                  call driver('NEW_PLOT', 0)
               else
                  if(use_buttons .eq. 0) call init_points()
                  call driver('OVERLAY_PLOT', 0)
               endif
            endif
         enddo

         call save_pixmap(batch)
         f = f + inc

      enddo
      call do_loop
      return

 100  format(1x, 'Command ',a,' requires 2-4 values.')
 110  format(1x, a, ' is an incorrect value for command ',a,'.')
 120  format(1x, a, ' is not fixed for command ', a, '.')
 140  format(1x, f20.5, ' is outside the window, ', f20.5,
     &     ' is used instead.')
 200  format(a,'=',f20.5)
      end


      subroutine clear_loop_flags()
      include 'loopflag.icl'
      loop_flag(1)=.FALSE.
      loop_flag(2)=.FALSE.
      loop_flag(3)=.FALSE.
      loop_flag(4)=.FALSE.
      return
      end
