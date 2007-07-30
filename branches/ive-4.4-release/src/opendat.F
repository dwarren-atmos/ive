      subroutine opendat (error,reload)
c-----------------------------------------------------------------------
c     $Id: opendat.F,v 1.12 2000/11/28 23:35:13 harry Exp $
c     Purpose:
c        This routine opens a netCDF data file, and gets a list of
c        variables in that file.
c     Arguments:
c        error    logi  output  indicates possible errors found in this
c                               routine.
c                               error = false    no errors detected.
c                               error = true     error detected.
c     History:
c      $Log: opendat.F,v $
c      Revision 1.12  2000/11/28 23:35:13  harry
c      Add list of recent files to Input pulldown menu.
c
c      Revision 1.11  2000/03/28 23:43:31  warren
c      Added reload_file function
c
c      Revision 1.10  1998/07/16 22:51:32  harryive
c      ivars should be ive_prt, not integer.
c
c      Revision 1.9  1997/12/18 20:25:24  harry
c      Add field_delta, which is needed for derivative.
c
c      Revision 1.8  1997/03/24 14:45:20  harry
c      Add support for widgets to file selection window for multiple files.
c
c      Revision 1.7  1997/02/12 17:36:25  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.6  1997/02/07 20:16:58  harry
c      Yet more MULTIPLE_READ changes.
c
c      Revision 1.5  1996/11/06 20:55:03  harry
c      Maintain domain units across different data files.
c
c      Revision 1.4  1996/10/02 15:27:51  harry
c      Fix the window limits for single valued dimensions.
c
c Revision 1.3  1995/10/04  22:09:58  harry
c Add new netCDF attributes and specification of staggering.  Add
c changes to slicer and plot labels to reflect user specified labels.
c
c Revision 1.2  1995/09/05  20:34:02  harry
c Add new slicer.  Good luck!
c
c Revision 1.1  1994/09/12  20:58:11  warren
c Make times a ive_ptr since it is.
c
c Revision 1.6  1994/06/02  23:32:25  harry
c Do NOT change the window when opening up a new file (unless it is the first).
c
c Revision 1.5  1993/08/18  15:47:37  harry
c Change to new transform routines.
c
c Revision 1.4  1993/07/09  21:44:52  harry
c Set the initial variable physical limits to be the domain limits.
c
c Revision 1.3  1993/07/09  16:33:22  harry
c Add changes to properly handle windowing when changing data files.
c
c Revision 1.2  1993/06/05  00:24:10  warren
c fixed to make sure plwmin and plwmax are set when a file is opened.
c cleaned up more unnecessary ui_update calls
c
c Revision 1.1.1.1  1993/04/07  19:40:11  harry
c Initial CVS checkin of IVE.
c
c Revision 2.5  1992/12/31  18:48:28  millerp
c Make changes for new getvar.F.
c
c Revision 2.4  1992/09/24  23:15:00  harry
c Keep track of original domain display units.
c
c Revision 2.3  1992/08/11  16:18:39  harry
c Do nothing if name of file to open is the same as the currently
c opened file.
c
c Revision 2.2  1992/07/13  16:45:56  millerp
c *** empty log message ***
c
c Revision 2.1  1992/07/07  19:23:29  millerp
c No longer blanking out field after a new data file is opened.
c
c Revision 2.0  1992/07/01  21:12:57  millerp
c Released version rereving to 2.0
c
c Revision 1.6  1992/06/26  00:05:41  warren
c fixed plwmin and max _scaled
c
c Revision 1.5  1992/06/17  16:29:29  millerp
c Fixed dec compiler errors.
c
c Revision 1.4  1992/06/15  19:00:58  millerp
c Changed argument list for scale.
c
c Revision 1.3  1992/06/10  20:10:47  millerp
c Add calculations for slope and intercept variables in common block.d
c
c Revision 1.2  1992/05/29  22:34:02  millerp
c Added units.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'message.icl'
      include 'units.icl'
#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
c
c
c     Argument declarations.
c
      logical error, first, reload
c
c
c     Local variable declarations.
c
      character *(UNIT_LEN) vdom_units, vdom_display_units
      character *(MAX_FIELD_LENGTH) vlabel
      integer               i, ierr, status, n, ncoord
      ive_ptr               exact_coord, ivars
      real                  dplmin, dplmax, vdelta
      dimension ivars(MAXVAR), dplmin(MAXDIM), dplmax(MAXDIM),
     &          vdom_units(MAX_UNITS), vdom_display_units(MAX_UNITS),
     &          ncoord(MAXDIM), exact_coord(MAXDIM),
     &          vdelta(MAXDIM), vlabel(MAXDIM)

c
c     External function declarations.
c
      integer               convert_units, open_var, strbeg, strend
      data first/.TRUE./
c
c
c
c
      error = .false.
      
      if (.not. reload .and.
     &     datfil(strbeg(datfil):strend(datfil)).eq.old_file) return
      old_file = datfil(strbeg(datfil):strend(datfil))
c
c     Open the data file. The data file to be read is datfil, and it 
c     must be null terminated since open_var is a c routine.
c
      do 10 i = 1, MAXDIM
         dplmin(i) = 0.0
         dplmax(i) = 0.0
         vdelta(i) = 0.0
         vlabel(i) = char(0)
 10   continue
c    
      do 20 i = 1, MAX_UNITS
         vdom_units(i)(1:len(vdom_units(i))) = char(0)
         vdom_display_units(i)(1:len(vdom_display_units(i))) = char(0)
 20   continue
c
      ierr = open_var (datfil(strbeg(datfil):strend(datfil))//char(0),
     &                 dplmin, dplmax, vdom_units, vdom_display_units,
     &	               vdelta, vlabel, 0)
      if ( ierr .eq. -1 ) then
c
c        There is an error in function open_var.
c
         write (message, 100) datfil(strbeg(datfil):strend(datfil))
         call write_message
         error = .true.
      else
c
c        Set the new values for the number of variables (numvar) and the
c        data dimensions. Also set domain units and domain display 
c        units. Cat nulls since these values will be read by a C 
c        routine.
c
         do 30 i = 1, MAXDIM
            plmin(i) = dplmin(i)
            plmax(i) = dplmax(i)
            phmin(i) = dplmin(i)
            phmax(i) = dplmax(i)
            grid_delta(i) = vdelta(i)
            field_delta(i) = vdelta(i)
            domain_units(i)(1:UNIT_LEN) = ' '
            domain_display_units(i)(1:UNIT_LEN) = ' '
            dim_display_names(i)(1:MAX_FIELD_LENGTH) = ' '
            domain_units(i) = vdom_units(i)
            domain_display_units(i) = vdom_display_units(i)
            dim_display_names(i) = vlabel(i)
            call catnull (domain_units(i), UNIT_LEN, error)
            call catnull (domain_display_units(i), UNIT_LEN, error)
            call catnull (dim_display_names(i), MAX_FIELD_LENGTH, error)
 30      continue
c
c        Call the new_file transform routine.
c
         do i = 1, MAXDIM
            exact_coord(i) = 0
            ncoord(i) = 0
         enddo
         call new_file_trans(ierr, exact_coord, ncoord, coord_dep)
         call set_slicer_values(exact_coord, ncoord)
c
         do 35 i = 1, MAXDIM
            plmin_scaled(i) = plmin(i)
            plmax_scaled(i) = plmax(i)

            domain_slope(i) = 1.0
            domain_intercept(i) = 0.0
            status = convert_units (domain_units(i), 
     &                              domain_display_units(i), 
     &                             domain_slope(i), domain_intercept(i))
            if ( status .eq. 0 ) then
               call scale (plmin_scaled(i), 1, domain_slope(i), 
     &                     domain_intercept(i), 0.0)
               call scale (plmax_scaled(i), 1, domain_slope(i), 
     &                     domain_intercept(i), 0.0)
            endif
 35      continue
c
c     If this is the first time thru, set window limits to domain limits.
c     Otherwise, just change the limits for single valued dimensions.
c
         do i = 1, MAXDIM
            if (first .or. (dplmin(i) .eq. dplmax(i))) then
               plwmin(i) = dplmin(i)
               plwmax(i) = dplmax(i)
               plwmin_scaled(i) = plmin_scaled(i)
               plwmax_scaled(i) = plmax_scaled(i)
            endif
         enddo
         first = .false.
c
c        Create field buttons.
c
         call make_button_list (ivars, MAXVAR)
         call make_field_buttons (ivars, %val(0))
         call add_filelist(datfil(strbeg(datfil):strend(datfil))
     &        //char(0))
      endif
c
c
c     Format statements.
c
 100  format (1x,'The data file ',a,' cannot be opened.')
c
c
      return
      end
