      subroutine settyp (error)
c-----------------------------------------------------------------------
c     $Id: settyp.f,v 1.7 2001/01/23 22:47:16 davidive Exp $
c     Purpose:
c        This subroutine sets the voriable which determines which type
c        of plot (2d, 3d, etc) will be made.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: settyp.f,v $
c      Revision 1.7  2001/01/23 22:47:16  davidive
c      cpline.f: fixed to use zero_color
c      everything else: First cut at wireframes and isosurfaces. No dimentions yet
c      no widgets yet.
c
c      Revision 1.6  1999/01/26 16:41:33  harry
c      Semicolons?  Fortran don't need no stinking semicolons!
c
c      Revision 1.5  1998/12/31 16:01:44  warren
c      First cut of new trajectory look and feel. Trajectories are no longer
c      on the popup menu, just plot type.
c
c      Revision 1.4  1997/04/09 19:37:07  harry
c      Add scalar_name so when you go from vector to scalar you end up with
c      the last scalar field.
c
c      Revision 1.3  1996/04/25 16:25:30  harry
c      Add Skew-T plots and the beginning of trajectory plots.
c
c Revision 1.2  1994/03/22  21:32:06  warren
c fixed bad vectors at the bottom of Dale's plot and made vector labels
c behave better (they no longer overwrite the solid fill labels)
c
c Revision 1.1.1.1  1993/04/07  19:40:34  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/10/06  18:26:45  warren
c added xtics_line and ytics_line and plot types scaler and vector
c
c Revision 2.0  1992/07/01  21:14:59  millerp
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
c
c
c     Argument declarations.
c
      logical error
c
c
c     Local variable declarations.
c
      character *(cmdlen) curval
      character *256 vcomp1, vcomp2
      character *512 buffer
      integer             ibeg, iend
      logical             winopn
      data winopn / .false. /
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend
c
c
      save winopn
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
         error = .false.
      else
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg+1), (iend - ibeg + 2))
         if ( curval(ibeg:ibeg+1) .eq. 'SC' ) then
            plotyp2 = 'SCALAR'
            call stop_traj_mouse
            call getavar('scalar_name', vcomp1, error)
            if (nblank(vcomp1) .ne. 0) then
               buffer = 'field='//vcomp1
               call driver(buffer,0)
            endif
         elseif ( curval(ibeg:ibeg+1) .eq. 'SK' ) then
            plotyp1 = 'SKEWT'
            call stop_traj_mouse
         elseif ( curval(ibeg:ibeg) .eq. 'V' ) then
            plotyp2 = 'VECTOR'
            call stop_traj_mouse
            call getavar('vcomp_1', vcomp1, error)
            if (nblank(vcomp1) .ne. 0) then
               buffer='vector_component='//vcomp1(1:strend(vcomp1))
               call getavar('vcomp_2', vcomp2, error)
               if (nblank(vcomp2) .ne. 0) then
                  buffer=buffer(1:strend(buffer))//','//vcomp2
               endif
               call driver(buffer,0)
            else
               call getavar('vcomp_2', vcomp2, error)
               if (nblank(vcomp2) .ne. 0) then
                  buffer='vector_component=,'//vcomp2
                  call driver(buffer,0)
               endif
            endif
         elseif ( curval(ibeg:ibeg+1) .eq. 'SU') then
            call stop_traj_mouse
            plotyp3='ISOSURF'
         elseif ( curval(ibeg:ibeg+1) .eq. 'PO') then
            call stop_traj_mouse
            plotyp3='SCATTER'
         elseif ( curval(ibeg:ibeg) .eq. 'W') then
            call stop_traj_mouse
            plotyp3='WIREFRAME'
         elseif ( curval(ibeg:ibeg) .eq. 'B') then
            call stop_traj_mouse
            plotyp3='BOX'
         elseif ( curval(ibeg:ibeg) .eq. 'L' ) then
            plotyp1 = 'LINE'
            call stop_traj_mouse
         elseif ( curval(ibeg:ibeg) .eq. 'T' ) then
            plotyp2 = 'TRAJECTORY'
            call start_traj_mouse
            call getavar('vcomp_1', vcomp1, error)
            if (nblank(vcomp1) .ne. 0) then
               buffer='vector_component='//vcomp1(1:strend(vcomp1))
               call getavar('vcomp_2', vcomp2, error)
               if (nblank(vcomp2) .ne. 0) then
                  buffer=buffer(1:strend(buffer))//','//vcomp2
               endif
               call driver(buffer,0)
            else
               call getavar('vcomp_2', vcomp2, error)
               if (nblank(vcomp2) .ne. 0) then
                  buffer='vector_component=,'//vcomp2
                  call driver(buffer,0)
               endif
            endif
          else
c
c           The value for this command is incorrect.
c
            write (message, 110) curval(ibeg:iend),
     &                           comand(1)(strbeg(comand(1)):
     &                                     strend(comand(1)))
            call write_message
            error = .true.
         endif
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
