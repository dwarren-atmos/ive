      subroutine setfld (error)
c-----------------------------------------------------------------------
c     $Id: setfld.f,v 1.3 1997/04/09 19:37:06 harry Exp $
c     Purpose:
c        This subroutine sets the name of the field to be plotted. The
c           field name must correspond with the name of a variable that
c           appears in the netCDF data file.
c     Arguments:
c        error  logi  output  indicates error encountered in this
c                             routine.
c     History:
c      $Log: setfld.f,v $
c      Revision 1.3  1997/04/09 19:37:06  harry
c      Add scalar_name so when you go from vector to scalar you end up with
c      the last scalar field.
c
c      Revision 1.2  1996/03/27 23:54:01  harry
c      Slicer should check new variable "field_pointer" to find out name of
c      field currently being sliced instead of "field" to properly handle
c      vectors and other future plots (like Skew-T).
c
c Revision 1.1.1.1  1993/04/07  19:40:23  harry
c Initial CVS checkin of IVE.
c
c Revision 2.1  1992/12/31  18:49:44  millerp
c Make changes for new getvar.F.
c
c Revision 2.0  1992/07/01  21:14:15  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'incommands.icl'
      include 'data.icl'
      include 'plotops.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical  error
c
c
c     Local variable declarations.
c
      character *(CMDLEN) curval, val1, val2
      integer             ibeg, iend
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
      val1(1:len(val1)) = ' '
      val2(1:len(val2)) = ' '
c
c     Get the first value for the current command.
c
      curval = comand(FSTVAL)(1:cmdlen)
c
c     Make sure a value is given.
c
      if ( nblank(curval) .eq. 0 ) then
         write (message, 100) comand(1)(strbeg(comand(1)):
     &                                  strend(comand(1)))
         call write_message
         error = .true.
      else
c        Set the field name.
         ibeg = strbeg(curval)
         iend = strend(curval)
         call upcase (curval(ibeg:iend), (iend - ibeg + 1))
         val1 = curval(ibeg:iend)
      endif
c
c
      if (.not. error) then
c
c        Check for a different name for the field button.
c
         curval = comand(FSTVAL+1)(1:cmdlen)
         if ( nblank(curval) .eq. 0 ) then
c
c           Button name is set equal to field name.
c
            val2 = val1
         else
c
c           Button name is different than field name.
c
            ibeg = strbeg(curval)
            iend = strend(curval)
            call upcase (curval(ibeg:iend), (iend - ibeg + 1))
            val2 = curval(ibeg:iend)
         endif
c
c        Set new field and button names.
c
         field(1:len(field)) = ' '
         button_name(1:len(button_name)) = ' '
         field = val1(strbeg(val1):strend(val1))
         field_pointer = val1(strbeg(val1):strend(val1))
         button_name = val2(strbeg(val2):strend(val2))
         if (plotyp2 .eq. 'SCALAR') then
            scalar_name = field
         endif
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
c
c
      return
      end

