      subroutine freefld (error)
c-----------------------------------------------------------------------
c     $Id: freefld.f,v 1.2 1993/05/20 17:34:11 warren Exp $
c     Purpose:
c        This subroutine frees the memory associated with a field. The
c           field name must correspond with the name of a variable that
c           appears in the netCDF data file.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: freefld.f,v $
c      Revision 1.2  1993/05/20 17:34:11  warren
c      Additions and changes to make the new forms active.
c
c Revision 1.1.1.1  1993/04/07  19:39:55  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/12/31  18:49:44  millerp
c Make changes for new getvar.F.
c
c Revision 2.0  1992/07/01  21:16:15  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/05/14  21:30:24  harry
c Initial revision
c
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'data.icl'
      include 'message.icl'
      include 'ive_widgets.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(MAX_FIELD_LENGTH) free_field
      character *(CMDLEN) curval
      integer             ibeg, iend, varloc, ivars
      dimension           ivars(MAXVAR)
c
c
c     External function declarations.
c
      integer nblank, strbeg, strend, getvid, free_var
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
         return
      endif
c
c
c     Set the name of the field to be freed.
      ibeg = strbeg(curval)
      iend = strend(curval)
      call upcase (curval(ibeg:iend), (iend - ibeg + 1))
      free_field = ' '
      free_field(1:iend - ibeg + 1) = curval(ibeg:iend)
      varloc = getvid(free_field(1:iend - ibeg + 1))
c
      if (varloc.lt.0) then
         write (message, 110) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1))),
     &        free_field
         call write_message
         error = .true.
         return
      endif
      if ( free_var(varloc) .ne. 0) then
         write (message, 120) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1))),
     &        free_field
         call write_message
         error = .true.
         return
      endif
      call make_button_list (ivars,MAXVAR) 
      call make_field_buttons (ivars, %val(0))
      
      if ((free_field .eq. field).or.(free_field .eq. button_name)) then
         field(1:len(field)) = ' '
      endif
      call ui_update (BUTTONS_UI)
c     
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,'Command ',a,' was given an non-existent field ', a)
 120  format (1x,'INTERNAL ERROR - Command ',a,' field ',a)
c
c
      return
      end










