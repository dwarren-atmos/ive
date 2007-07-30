      subroutine settrfil (error)
c-----------------------------------------------------------------------
c     $Id: settrfil.f,v 1.6 1997/03/24 14:45:20 harry Exp $
c     Purpose:
c        This routine sets the user-written transform file name.
c     Arguments:
c        error  logi  output  indicates possible errors found in this
c                             routine.
c                             error = false  no errors detected
c                             error = true   error detected
c     History:
c     $Log: settrfil.f,v $
c     Revision 1.6  1997/03/24 14:45:20  harry
c     Add support for widgets to file selection window for multiple files.
c
c     Revision 1.5  1997/02/07 20:17:00  harry
c     Yet more MULTIPLE_READ changes.
c
c     Revision 1.4  1996/04/12 18:32:36  harry
c     Add Fortran routine rindex, since Digital Unix does not have one.
c
c     Revision 1.3  1996/04/08 18:57:16  warren
c     first parts of num unlabeled lines for terrain and fixed transform routine
c     to use last directory if none is given.
c
c Revision 1.2  1993/08/18  15:47:45  harry
c Change to new transform routines.
c
c Revision 1.1.1.1  1993/04/07  19:40:34  harry
c Initial CVS checkin of IVE.
c
c Revision 2.4  1992/08/24  20:38:46  millerp
c Replace INQUIRE with IS_THERE.
c
c Revision 2.3  1992/08/11  16:20:08  harry
c Do nothing if name of transform file is the same as the currently
c installed file.
c
c Revision 2.2  1992/08/07  22:29:22  warren
c Checked in by Harry so I can fiddle with it.
c
c Revision 2.1  1992/07/22  23:02:14  millerp
c Fixed call to open_var.
c
c Revision 2.0  1992/07/01  21:15:02  millerp
c Released version rereving to 2.0
c
c Revision 1.2  1992/06/12  17:11:03  warren
c added call to expand_path to take care of ~name/
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
c
      include 'incommands.icl'
      include 'data.icl'
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c
      logical          error
c
c
c     Local variable declarations
c
      character *(256) file, ter_old_file, tmp
      integer          ibeg, iend
      real             dplmin, dplmax
c
c
c     External function declarations.
c
      integer          nblank, open_var, strbeg, strend, status
      integer          expand_path, is_there, rindex

      data ter_old_file/' '/
c
c
c
c
      error = .false.
      ibeg  = strbeg(comand(FSTVAL))
      iend  = strend(comand(FSTVAL))
c
c
c     Make sure a value is given.
c
      if ( nblank(comand(FSTVAL)) .eq. 0 ) then
         write (message, 100) comand(FSTVAL)(ibeg:iend)
         call write_message
         error = .true.
      else
c
c        Set the file name.
c
         file(1:len(file)) = ' '
         file = comand(FSTVAL)(ibeg:iend)
      endif
      if (file.eq.ter_old_file) return
      status = rindex(file, '/')
      if (status .eq. 0) then
         status = rindex(ter_old_file, '/')
         if (status .gt. 0) then
            tmp=ter_old_file(1:status)//file
            file=tmp
         endif
      endif
         
      ter_old_file = file
c
c
      if ( .not. error ) then
c
c        Expand ~'s
c
         call catnull (file, len(file), error)
         if (expand_path(file) .eq. 1) then
            error = .false.
         else
            error = .true.
            write (message, 130) 
            call write_message
         endif
      endif

      if ( .not. error ) then
c
c        Make sure the specified file exists.
c
         iend = strend(file)
         call catnull (file, len(file), error)
         status = is_there(file)
c
         if ( status .gt. 0 ) then
c
c
c           Release memory currently held for field buttons.
c
            call make_field_buttons (%val(0), %val(0))
c
c
c           Release memory currently held for data.
c
            call catnull (data_units, len(data_units), error)
            call catnull (data_display_units, len(data_display_units), 
     &                    error)
            status = open_var (%val(0), dplmin,
     &                         dplmax, data_units, data_display_units,
     &                         0, 0, 0)
c
c
c           Set the transform file name.
c
            transform_path = file(1:iend)
c
c
c           Load the transform file.
c
            file(1:len(file)) = ' '
            call getavar ('graphics_path', file, error)
            call catnull (file, len(file), error)
            call catnull (transform_path, len(transform_path), error)
            call load_func (file, transform_path)
c            write (message, 120) 
c     &              transform_path(1:strend(transform_path))
c            call write_message
         else
            write (message, 110) file(1:strend(file))
            call write_message
            error = .true.
         endif
      endif
c
c
c     Format statements.
c
 100  format (1x,'Command ',a,' requires a value.')
 110  format (1x,'Specified file ',a,' does not exist.')
 120  format (1x,'Current transform file name is : ' a)
 130  format (1x, 'Function expand_path failed in subroutine settrfil.')
c
c
      return
      end
