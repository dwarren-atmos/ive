      subroutine set_defmap(error)
c-----------------------------------------------------------------------
c     $Id: set_defmap.f,v 1.1 1993/12/15 18:15:40 harry Exp $
c     Purpose:
c        This subroutine sets default_map flag and, if true, calls the
c        default_map transform via call_defmap
c
c     Syntax of command:
c
c     DEFAULT_MAP=ON|OFF
c
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: set_defmap.f,v $
c      Revision 1.1  1993/12/15 18:15:40  harry
c      Add support for DEFAULT_MAP command, default_map transform.  Modify map
c      clipping to work properly with the map background widget.
c
c
c-----------------------------------------------------------------------

      include 'incommands.icl'
      include 'message.icl'
      include 'supma.icl'
      include 'ive_widgets.icl'

      logical error
      
c
c
c     Local variable declarations.
      character *(cmdlen) curval
      integer             ibeg, iend
c
c
c     External function declarations.
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
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:ibeg), (iend - ibeg + 1))
         if (curval(ibeg:iend) .eq. 'ON') then
c
c           Turn default map flag on.
c
            defmap = .true.
            call call_defmap
            call ui_update(MAPROJ_UI)
            call ui_update(LON_UI)
            call ui_update(LAT_UI)
            call ui_update(ROT_UI)
            call ui_update(MAPCLIP_UI)
         elseif (curval(ibeg:iend) .eq. 'OFF') then
c
c           Turn map flag off.
c
            defmap = .false.
            if (exact_fit) then
               maplim = 'CL'
               call ui_update(MAPCLIP_UI)
            endif
         else
c
c           The value for this command is incorrect.
c
            write (message,110) curval(ibeg:iend),
     &                          comand(1)(strbeg(comand(1)):
     &                                    strend(comand(1)))
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

      subroutine call_defmap
c-----------------------------------------------------------------------
c     Purpose:
c        This subroutine calls the default_map transform and stores the
c        results in the common block
c-----------------------------------------------------------------------


      include 'supma.icl'

      integer strbeg, strend

      call default_map_trans(proj, plon, plat, rota, maplim,
     &     plm1, plm2, plm3, plm4, exact_fit)
      proj = proj(strbeg(proj):strend(proj))
      maplim = maplim(strbeg(maplim):strend(maplim))
      return

      end
