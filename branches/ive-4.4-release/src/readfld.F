      subroutine readfld (varname, varptr, error) 
c-----------------------------------------------------------------------
c     $Id: readfld.F,v 1.11 2002/08/09 19:58:00 warren Exp $
c     Purpose:
c        This routine reads a netCDF variable from a file. If that 
c        variable is found the data is passed on to be plotted.
c     Arguments:
c        varname  char  input  the variable name.
c        varptr   int   output  a ive_ptr to the memory location of the
c                               beginning of the data.
c        error    logi  output  indicates possible errors found in this
c                               routine.
c                               error = true   no error detected.
c                               error = false  error detected.
c     History:
c      $Log: readfld.F,v $
c      Revision 1.11  2002/08/09 19:58:00  warren
c      Fixed for Linux and tested with DEC sun etc.
c      most items inside #ifdef's, or were general cleanup/fixes
c
c      Revision 1.10  2001/04/11 19:11:03  harryive
c      Add new_field_trans and load_atts calls to the end of getvar.  Remove them
c      from other places where they are now no longer needed.
c
c      Revision 1.9  2001/03/01 00:52:24  harry
c      Change type of ivars to ive_ptr.
c
c      Revision 1.8  2000/12/08 00:20:30  warren
c      fixed bug where you select field, plot, select new field, select old field
c      and plot -> gives wrong contours.
c
c      Revision 1.7  1997/02/19 19:06:23  harry
c      Get phmin and phmax when ndims < 4.
c
c      Revision 1.6  1997/02/12 17:36:28  warren
c      Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c      Revision 1.5  1996/03/05 18:22:01  harry
c      Isolate the use of "long" for dimension sizes to the netcdf routines.
c      Everywhere else we will use "int" or "integer".  Also change use of
c      long to nclong as a netCDF type as required by netCDF version 2.4.
c
c      Revision 1.4  1996/02/23 22:31:29  harry
c      Fix for reset_spval feature for new slicer.
c
c Revision 1.3  1994/03/10  18:45:02  harry
c Correctly print out messages if the user asks for a non-existent field.
c
c Revision 1.2  1994/02/17  21:36:23  harry
c Add dim_names to getvar.  Also include modified definition of the flag.
c
c Revision 1.1  1993/11/29  19:39:41  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.1.1.1  1993/04/07  19:40:15  harry
c Initial CVS checkin of IVE.
c
c Revision 2.4  1993/03/22  18:26:08  warren
c Christoph's fixes for aliasing
c
c Revision 2.3  1993/01/07  16:57:10  millerp
c Fix argument list for readfld; must pass in variable name for vectors.
c
c Revision 2.2  1992/12/31  18:49:44  millerp
c Make changes for new getvar.F.
c
c Revision 2.1  1992/10/02  01:04:26  warren
c fixed to only show the asked for field.
c
c Revision 2.0  1992/07/01  21:13:50  millerp
c Released version rereving to 2.0
c
c Revision 1.8  1992/06/23  17:16:50  millerp
c *** empty log message ***
c
c Revision 1.7  1992/06/15  19:00:58  millerp
c Changed argument list for scale.
c
c Revision 1.6  1992/06/11  17:23:48  harry
c Fix bug in load_atts for missing global attributes.
c
c Revision 1.5  1992/06/10  20:12:10  millerp
c Add calculations for slope and intercept variables in common blocks.
c
c Revision 1.4  1992/05/29  22:34:11  millerp
c Added units.
c
c Revision 1.3  1992/05/07  20:16:25  millerp
c Add a check for round-off error before setting spval.
c
c Revision 1.2  1992/04/28  18:58:29  millerp
c Changed to call getvar rather than read_var and read_d_var. This allows
c recursive calls to getvar. Also created routines add_field_button
c and load_atts.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'message.icl'
      include 'data.icl'
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
      character *(MAX_FIELD_LENGTH) varname
      ive_ptr        varptr
      logical        error
c
c
c     Local variable declarations.
c
      character *(UNIT_LEN) vdata_units, vdata_display_units,
     &	                    vdim_names(MAXDIM)
      integer        dimlst
      ive_ptr        ivars
      integer        ndims
      logical        new
      real           misdat, vstag, vphmin, vphmax
      dimension ivars(MAXVAR)
      dimension  dimlst(MAXDIM), vstag(MAXDIM), 
     &           vphmin(MAXDIM), vphmax(MAXDIM)
c
c
c     External function declarations.
c
      ive_ptr        addvar
c
c
c
c
      error  = .false.

c
c     Get a ive_ptr to the requested variable, plus all of its 
c     attributes. 
c     
      new = .true.
      varptr = addvar (varname, button_name, ndims, dimlst, vstag, 
     &     vphmin, vphmax, misdat, vdata_units, vdata_display_units, 
     &     vdim_names, new)
c
c     Get rid of intermediate results in the data structure. These 
c     fields are a result of calculating diagnostic fields.
c
      call garbage_collect_vars()
c
c     If getvar returns zero, the variable could not be found in the 
c     data file and the user has not written the code to compute it. 
c

      if ( varptr .eq. 0 ) then
         error = .true.
      else
         if (new) then
c
c           This is a new field, so we need to put up a button.
c
            call make_button_list (ivars,MAXVAR) 
            call make_field_buttons (ivars, %val(0))
         endif
         call reset_same_slice()
      endif

      end
c
c
      subroutine load_atts (ndims, dimlst, vstag, vphmin, vphmax,
     &                      misdat, units, dunits, vdnames)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine loads the graphics common blocks with the 
c        attributes of the current variable.
c     Arguments:
c        ndims   int   input  the number of dimensions.
c        dimlst  int   input  the array containing the variable 
c                             dimensions.
c        vstag   real  input  the array containing the variable 
c                             staggering.
c        vphmin  real  input  the array containing the minimum 
c                             coordinates of the variable in user
c                             space.
c        vphmax  real  input  the array containing the maximum 
c                             coordinates of the variable in user
c                             space.
c        misdat  real  input  the variable missing data value.
c        units   char  input  the variable units.
c        dunits  char  input  the variable display units.
c        vdnames char  input  the variable dimension names.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'units.icl'
c
c
c     Argument declarations.
c
      character *(UNIT_LEN) units, dunits, vdnames(MAXDIM)
      integer         dimlst
      integer         ndims
      real            vstag, vphmin, vphmax, misdat
      dimension dimlst(MAXDIM), vstag(MAXDIM), 
     &          vphmin(MAXDIM), vphmax(MAXDIM)
c
c
c     Local variable declarations.
c
      integer       i, status
      logical       error
c
c
c     External function declarations.
c
      integer       convert_units, strbeg, strend
#ifndef Linux
      double precision epsilon
#endif
c
c
c
c
      error = .false.
c
c     Set missing data value. Since we do several checks later to see
c     if spval = 0, make sure that possible round-off error in the 
c     netCDF file does not result in a small value for misdat.
c
#ifndef Linux
      if ( abs(misdat) .le. sngl(epsilon(0.0)) ) then
#else
      if ( abs(misdat) .le. epsilon(0.0) ) then
#endif
         call setrvar ('spval', 0.0, error)
      else
         call setrvar ('spval', misdat, error)
      endif
      call setlvar('reset_spval', .false., error)
c
c
c     Set data units and data display units. Note that character strings
c     must be null-terminated since they are passed to C routines later
c     on.
c
      data_units = units(strbeg(units):strend(units))
      call catnull (data_units, len(data_units), error)
      data_display_units = dunits(strbeg(dunits):strend(dunits))
      call catnull (data_display_units, len(data_display_units), error)
      status = convert_units (data_units, data_display_units, 
     &                        data_slope, data_intercept)
      if ( status .ne. 0 ) then
         data_slope = 1.0
         data_intercept = 0.0
      endif
c
c
c     Set variable dimensions. Note that getvar has already put 
c     dimlst in the correct order.
c
      num_dims = ndims
      cpmax1 = 1
      cpmax2 = 1
      cpmax3 = 1
      cpmax4 = 1
      if ( dimlst(1) .gt. 0 ) cpmax1 = dimlst(1)
      if ( dimlst(2) .gt. 0 ) cpmax2 = dimlst(2)
      if ( dimlst(3) .gt. 0 ) cpmax3 = dimlst(3)
      if ( dimlst(4) .gt. 0 ) cpmax4 = dimlst(4)
c
c     
c     Set staggering and physical domain size.
c     
      do 30 i = 1, ndims
         stag(i)  = vstag(i)
         phmin(i) = vphmin(i)
         phmax(i) = vphmax(i)
	 data_dim_names(i) = vdnames(i)
c
c
c        The following if block takes care of the case of 
c        the missing global attribute. Rather than find it 
c        we make one up.
c     
         if ( (plmin(i) .eq. 0.0).and.(plmax(i) .eq. 0.0) ) then
            plmin(i) = vphmin(i)
            plmax(i) = vphmax(i)
         endif
         plmin_scaled(i) = plmin(i)
         plmax_scaled(i) = plmax(i)
         call scale (plmin_scaled(i), 1, domain_slope(i), 
     &               domain_intercept(i), 0.0)
         call scale (plmax_scaled(i), 1, domain_slope(i), 
     &               domain_intercept(i), 0.0)
 30   continue
      if (ndims .lt. 4) then
	 do i = ndims+1, 4
            phmin(i) = plmin(i)
            phmax(i) = plmin(i)
            data_dim_names(i) = ' '
         enddo
      endif
c
c
      return
      end
