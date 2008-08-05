      subroutine getavar (string, value, error)
c-----------------------------------------------------------------------
c     $Id: getavar.f,v 1.65 2006/08/24 22:08:40 warren Exp $
c     Purpose:
c        This routine returns the value of an alphanumeric variable 
c        stored in a common block.
c     Arguments:
c        string  char  input   the requested variable name.
c        value   char  output  the value of the requested variable.
c                              CAUTION: The length of value must be at 
c                                       least one byte
c                                       longer than the length of the
c                                       requested variable. Since value
c                                       may be sent to a C routine, we
c                                       must null-terminate it.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c        $Log: getavar.f,v $
c        Revision 1.65  2006/08/24 22:08:40  warren
c        added vlabel command to get max vectors plotted even if label=off
c
c        Revision 1.64  2002/08/09 19:57:58  warren
c        Fixed for Linux and tested with DEC sun etc.
c        most items inside #ifdef's, or were general cleanup/fixes
c
c        Revision 1.63  2002/06/14 19:44:05  harryive
c        Allow specification of terrain contours by value.
c
c        Revision 1.62  2002/05/22 17:56:44  davidive
c        First working version of trajectories colored by alternative fields
c        There is not a complete interface yet!!!
c
c        Revision 1.61  2002/03/11 20:14:16  davidive
c        Fixed ui_pudate to update trajectory step and value.
c
c        Revision 1.60  2002/02/22 21:10:40  davidive
c        Fixed ive to set the traj_entry color and use it correctly, instead of
c        zeroing it out every time.
c
c        Revision 1.59  2001/11/20 23:29:56  davidive
c        Track if step and mid val are set by ive or a person. Also initialize all
c        variables for no depth.
c
c        Revision 1.58  2001/11/02 18:51:17  davidive
c        converted rdctab to c, fixed trajectories so that their independant color
c        tables work, including setting center color, center value and interval.
c
c        Revision 1.57  2001/08/15 22:25:47  davidive
c        Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
c        Added code to set up which axis to use and to convert the labels in 1d plots.
c
c        Revision 1.56  2001/04/11 23:24:03  warren
c        fixed typoed lenvar to lenval
c
c        Revision 1.55  2001/01/23 22:47:11  davidive
c        cpline.f: fixed to use zero_color
c        everything else: First cut at wireframes and isosurfaces. No dimentions yet
c        no widgets yet.
c
c        Revision 1.54  2000/05/09 19:53:50  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c        Revision 1.53  2000/01/27 21:19:57  warren
c        return cur plot to show plot number in save window
c
c        Revision 1.52  1999/12/22 00:33:15  warren
c        Fixed Dales crashing bug when changing from point slicer to normal. Also
c        have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c        Revision 1.51  1999/02/10 23:13:37  harry
c        Get rid of unused command vector_scale.
c
c        Revision 1.50  1999/02/04 00:28:27  harry
c        Add vector scaling.
c
c        Revision 1.49  1999/01/29 20:46:28  warren
c        ets the flag for time independence and the plot the trajectories
c
c        Revision 1.48  1998/12/31 16:01:40  warren
c        First cut of new trajectory look and feel. Trajectories are no longer
c        on the popup menu, just plot type.
c
c        Revision 1.47  1998/09/04 20:51:58  harry
c        Add colorbar button to annotation widget.  Rewrite file widgets to
c        remember previous entries.
c
c        Revision 1.46  1997/12/18 20:25:23  harry
c        Add field_delta, which is needed for derivative.
c
c        Revision 1.45  1997/10/22 19:35:03  davidive
c        Turned the particle movement part of trajectories into a user transform
c        and added unit checking to the default transform.
c
c        Revision 1.44  1997/07/22 19:50:44  harry
c        Fix multiple reading to allow user to specify coordinate transform.
c
c        Revision 1.43  1997/06/03 19:14:19  davidive
c        The big mouse slicing changeover. We have the new form, only requires you
c        set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c        always runs through init_points_ to catch all these things.
c        note: only actually works when two dimentions have points set. Needs to
c        be expanded into 1, 3 or 4.
c
c        Revision 1.42  1997/05/07 18:43:05  harry
c        Mouse lines are drawn with same colors/patterns as normal lines.
c
c        Revision 1.41  1997/04/15 19:36:13  warren
c        Put in pieces to fix color lock indicator in the future.
c
c        Revision 1.40  1997/04/15 17:24:39  harry
c        Remove time_tag - use multiple files instead.
c        Various fixes to vector/scalar looping.
c
c        Revision 1.39  1997/04/09 19:37:06  harry
c        Add scalar_name so when you go from vector to scalar you end up with
c        the last scalar field.
c
c        Revision 1.38  1997/03/14 22:26:33  warren
c        Added new command map_window to allow for mouse windowing on non
c        equadistant or default maps.
c
c        Revision 1.37  1997/02/21 20:02:09  harry
c        Fix vector interval to be floating point and have values less than one.
c        Handling of vector interval is now in the slicer.
c
c        Revision 1.36  1997/02/07 18:39:09  harry
c        Remove PARTIAL_READ and expand MULTIPLE_READ.
c
c        Revision 1.35  1997/02/07 00:07:27  warren
c        Fixed lack of typin bug for sliders and printer widget not remembering
c        to do color.
c
c        Revision 1.34  1997/01/17 20:38:17  harry
c        First changes for MULTIPLE_READ and PARTIAL_READ.
c
c        Revision 1.33  1996/12/23 20:58:13  warren
c        Landscape printing option
c
c        Revision 1.32  1996/11/06 20:55:01  harry
c        Maintain domain units across different data files.
c
c Revision 1.31  1996/10/11  01:12:55  warren
c To fill or not to fill is no longer the only question. Now there is a seperate
c question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c Labels are not fixed yet.
c
c        Revision 1.30  1996/10/09 18:54:40  harry
c        Add the ability to loop on file names if time value is part of name,
c        and there is only one time per file.
c
c        Revision 1.29  1996/07/12 20:59:02  harry
c        Change data and domain slopes and intercepts to real*8.
c
c        Revision 1.28  1996/04/25 16:25:11  harry
c        Add Skew-T plots and the beginning of trajectory plots.
c
c        Revision 1.27  1996/04/23 18:17:26  warren
c        Changes for terrain
c
c        Revision 1.26  1996/04/01 20:23:40  warren
c        Terrain form and new terrain command (on / off)
c
c        Revision 1.25  1996/03/27 23:54:00  harry
c        Slicer should check new variable "field_pointer" to find out name of
c        field currently being sliced instead of "field" to properly handle
c        vectors and other future plots (like Skew-T).
c
c        Revision 1.24  1995/12/15 22:33:17  warren
c        moved patterns into 4.0 and implemented the list of fixes and changes from
c        Curtis.
c
c Revision 1.23  1995/10/04  22:09:55  harry
c Add new netCDF attributes and specification of staggering.  Add
c changes to slicer and plot labels to reflect user specified labels.
c
c Revision 1.22  1995/09/15  16:48:24  harry
c Add support for timeout of warning messages.
c
c Revision 1.21  1995/09/05  20:33:52  harry
c Add new slicer.  Good luck!
c
c Revision 1.20  1994/11/22  22:47:51  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.19  1994/10/08  19:01:28  warren
c The annotation widget and updating it. As well as changing tic_bold to
c tic_labels and adding an off option.
c
c Revision 1.18  1994/09/20  17:11:10  warren
c all lock of coor to value: lock=contour val, color table entry.
c Also fixe widget to update correctly
c
c Revision 1.17  1994/09/14  18:19:56  harry
c Big 1D flip uncommit.
c
c Revision 1.16  1994/05/06  17:10:08  yuri
c Main vector lock commit + small correction in X/ive_ui_update.c file
c
c Revision 1.15  1994/04/19  23:03:00  yuri
c Big 1D flip commit.
c
c Revision 1.14  1994/02/17  21:35:07  harry
c Add in data_dim_names and num_dims, remove unused maxvar.
c
c Revision 1.13  1994/02/16  17:00:32  yuri
c        Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.12  1994/01/29  00:11:35  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.11  1993/12/28  21:00:50  harry
c Death to constfil!
c
c Revision 1.10  1993/12/15  18:15:33  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.9  1993/10/28  22:01:50  yuri
c Added line for retrieving of time if one is locked.
c
c Revision 1.8  1993/08/31  18:00:42  yuri
c New comand "VSLICE" (vertical slice from (x1,y1) to (x2,y2)) installed
c        into version with NEW transforms.
c
c Revision 1.7  1993/08/18  15:47:25  harry
c Change to new transform routines.
c
c Revision 1.6  1993/07/23  20:36:57  yuri
c        Command "vector_lock" is introduced to lock vector
c scaling in a loop (for example). Also some minor bugs are corrected.
c
c Revision 1.5  1993/07/09  16:35:27  harry
c Add number of contour values to attributes and change its meaning so
c that a zero or negative number means use contour interval instead.
c
c Revision 1.4  1993/07/06  20:23:09  harry
c Add the "scaled_loc" variable to retain scaled values for fixed coordinates.
c
c Revision 1.3  1993/06/22  22:09:46  yuri
c        New 1D and 2D slicers installed, added command interp_factors
c so user could choose number of interpolation points in slicing.
c Primarily it's for averaging, but user could use it every time
c before call slicers.
c
c Revision 1.2  1993/05/20  17:34:52  warren
c Additions and changes to make the new forms active.
c
c Revision 1.1.1.1  1993/04/07  19:39:57  harry
c Initial CVS checkin of IVE.
c
c Revision 2.11  1993/02/01  23:45:02  warren
c removed xmajorl and ymajorl
c
c Revision 2.10  1993/01/05  20:15:11  warren
c added no_zero and cont_int removed nozero
c
c Revision 2.9  1992/12/31  18:49:44  millerp
c Make changes for new getvar.F.
c
c Revision 2.8  1992/12/10  21:14:24  millerp
c Delete references to unused variables.
c
c Revision 2.7  1992/11/24  16:33:03  millerp
c Make changes related to removing hslice, vsice.
c
c Revision 2.6  1992/10/30  19:45:46  millerp
c Added stuff for averaged line plots.
c
c Revision 2.5  1992/10/28  22:32:48  millerp
c Remove logical variable AVERAGE.
c
c Revision 2.4  1992/10/15  23:01:21  warren
c added color table locking
c
c Revision 2.3  1992/10/06  18:26:45  warren
c added xtics_line and ytics_line and plot types scaler and vector
c
c Revision 2.2  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.1  1992/09/08  19:53:31  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:13:09  millerp
c Released version rereving to 2.0
c
c Revision 1.6  1992/06/17  16:28:48  millerp
c Fixed dec compiler errors.
c
c Revision 1.5  1992/06/15  18:59:56  millerp
c Added new variables.
c
c Revision 1.4  1992/06/10  20:08:43  millerp
c Add new variables.
c
c Revision 1.3  1992/05/29  22:32:35  millerp
c Added more variables.
c
c Revision 1.2  1992/04/27  16:48:18  millerp
c Added variables to getivar and setivar.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'color.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'flags.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'supma.icl'
      include 'message.icl'
      include 'units.icl'
      include 'attributes.icl'
      include 'vector.icl'
      include 'skewt.icl'
      include 'traj.icl'
c
c
c     Argument declarations.
c
      character *(*)      string, value
      logical             error
c
c
c     Local variable declarations.
c
      integer         begstr, endstr, ibeg, iend, lenval
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg   = strbeg (string)
      iend   = strend (string)
      lenval = len (value)
c
c     color variables.
c
      if ( string(ibeg:iend) .eq. 'clrtab' ) then
         begstr = strbeg (clrtab)
         endstr = strend (clrtab)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = clrtab(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'lock_field' ) then
         begstr = strbeg (lock_field)
         endstr = strend (lock_field)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = lock_field(begstr:endstr)//char(0)
         endif
c
c     data variables.
c
      elseif ( string(ibeg:iend) .eq. 'datfil' ) then
         begstr = strbeg (datfil)
         endstr = strend (datfil)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = datfil(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'field' ) then
         begstr = strbeg (field)
         endstr = strend (field)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = field(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'button_name' ) then
         begstr = strbeg (button_name)
         endstr = strend (button_name)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = button_name(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'field_pointer' ) then
         begstr = strbeg (field_pointer)
         endstr = strend (field_pointer)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = field_pointer(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'graphics_path' ) then
         begstr = strbeg (graphics_path)
         endstr = strend (graphics_path)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = graphics_path(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'transform_path' ) then
         begstr = strbeg (transform_path)
         endstr = strend (transform_path)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = transform_path(begstr:endstr)//char(0)
         endif
c
c     plotops variables.
c
      elseif ( string(ibeg:iend) .eq. 'plotyp1' ) then
         begstr = strbeg (plotyp1)
         endstr = strend (plotyp1)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = plotyp1(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'plotyp2' ) then
         begstr = strbeg (plotyp2)
         endstr = strend (plotyp2)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = plotyp2(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'plotyp3' ) then
         begstr = strbeg (plotyp3)
         endstr = strend (plotyp3)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = plotyp3(begstr:endstr)//char(0)
         endif
c
c     supma variables.
c
      elseif ( string(ibeg:iend) .eq. 'maplim' ) then
         begstr = strbeg (maplim)
         endstr = strend (maplim)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = maplim(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'outlin' ) then
         begstr = strbeg (outlin)
         endstr = strend (outlin)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = outlin(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'proj' ) then
         begstr = strbeg (proj)
         endstr = strend (proj)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = proj(begstr:endstr)//char(0)
         endif
c
c     units variables.
c
      elseif ( string(ibeg:iend) .eq. 'data_units' ) then
         begstr = strbeg (data_units)
         endstr = strend (data_units)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = data_units(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'data_display_units' ) then
         begstr = strbeg (data_display_units)
         endstr = strend (data_display_units)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = data_display_units(begstr:endstr)//char(0)
         endif
c
c     scale variables
c
      elseif ( string(ibeg:iend) .eq. 'scalar_name' ) then   
         begstr = strbeg (scalar_name)
         endstr = strend (scalar_name)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = scalar_name(begstr:endstr)//char(0)
         endif
c
c     vector variables
c
      elseif ( string(ibeg:iend) .eq. 'vcomp_1' ) then   
         begstr = strbeg (vect_component(1))
         endstr = strend (vect_component(1))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = vect_component(1)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'vcomp_2' ) then   
         begstr = strbeg (vect_component(2))
         endstr = strend (vect_component(2))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = vect_component(2)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'vcomp_3' ) then   
         begstr = strbeg (vect_component(3))
         endstr = strend (vect_component(3))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = vect_component(3)(begstr:endstr)//char(0)
         endif
c
c     skewt variables
c
      elseif ( string(ibeg:iend) .eq. 'scomp_1' ) then   
         begstr = strbeg (skewt_component(1))
         endstr = strend (skewt_component(1))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = skewt_component(1)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'scomp_2' ) then   
         begstr = strbeg (skewt_component(2))
         endstr = strend (skewt_component(2))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = skewt_component(2)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'scomp_3' ) then   
         begstr = strbeg (skewt_component(3))
         endstr = strend (skewt_component(3))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = skewt_component(3)(begstr:endstr)//char(0)
         endif
c
c     traj variables
c
      elseif ( string(ibeg:iend) .eq. 'trajcomp_1' ) then   
         begstr = strbeg (traj_component(1))
         endstr = strend (traj_component(1))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_component(1)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'trajcomp_2' ) then   
         begstr = strbeg (traj_component(2))
         endstr = strend (traj_component(2))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_component(2)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'trajcomp_3' ) then   
         begstr = strbeg (traj_component(3))
         endstr = strend (traj_component(3))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_component(3)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'traj_units1' ) then   
         begstr = strbeg (traj_units(1))
         endstr = strend (traj_units(1))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_units(1)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'traj_units2' ) then   
         begstr = strbeg (traj_units(2))
         endstr = strend (traj_units(2))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_units(2)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'traj_units3' ) then   
         begstr = strbeg (traj_units(3))
         endstr = strend (traj_units(3))
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_units(3)(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'traj_background_field' ) then   
         begstr = strbeg (traj_background_field)
         endstr = strend (traj_background_field)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = traj_background_field(begstr:endstr)//char(0)
         endif
      elseif ( string(ibeg:iend) .eq. 'old_traj_background' ) then   
         begstr = strbeg (old_traj_background)
         endstr = strend (old_traj_background)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = old_traj_background(begstr:endstr)//char(0)
         endif
c
c     segment variables
c
      elseif (string(ibeg:iend) .eq. cmdstr) then
         begstr = strbeg (cmdstr)
         endstr = strend (cmdstr)
         if ( (endstr - begstr + 2) .gt. lenval ) then
            write (message, 110) (endstr - begstr + 2)
            call write_message  
            error = .true.
         else
            value = cmdstr(begstr:endstr)//char(0)
         endif
      else
         write (message, 100) string(ibeg:iend)
         call write_message  
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'getavar.')
 110  format ('At least ',i3,' bytes must be allocated for the ',
     &           'string argument in calls to subroutine getavar.')
c
c
      return
      end
c
c
      subroutine getaarr (string, value, dim, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of an alphanumeric variable 
c        array stored in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   char  output  the array of requested values.
c                              CAUTION: The length of value must be at 
c                                       least one byte
c                                       longer than the length of the
c                                       requested variable. Since value
c                                       may be sent to a C routine, we
c                                       must null-terminate it.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'units.icl'
      include 'message.icl'
c
c     Argument declarations.
c     
      character *(*)  string, value
      integer         dim
      logical         error
      dimension value(dim)
c
c
c     Local variable declarations.
c
      integer         begstr, endstr, i, ibeg, iend, lenval
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
      lenval = len (value(dim))
c
c
c     data arrays.
c
      if     ( string(ibeg:iend) .eq. 'domain_units' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do 10 i = 1, MAXDIM
               begstr = strbeg (domain_units(i))
               endstr = strend (domain_units(i))
               if ( (endstr - begstr + 2) .gt. lenval ) then
                  write (message, 210) endstr - begstr + 2
                  call write_message
                  error = .true.
               else
                  value(i) = domain_units(i)(begstr:endstr)//char(0)
               endif
 10         continue
         endif
      elseif ( string(ibeg:iend) .eq. 'domain_display_units' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do 20 i = 1, MAXDIM
               begstr = strbeg (domain_display_units(i))
               endstr = strend (domain_display_units(i))
               if ( (endstr - begstr + 2) .gt. lenval ) then
                  write (message, 210) endstr - begstr + 2
                  call write_message
                  error = .true.
               else
                  value(i) = domain_display_units(i)(begstr:endstr)//
     &                       char(0)
               endif
 20         continue
         endif
      elseif ( string(ibeg:iend) .eq. 'data_dim_names' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               begstr = strbeg (data_dim_names(i))
               endstr = strend (data_dim_names(i))
               if ( (endstr - begstr + 2) .gt. lenval ) then
                  write (message, 210) endstr - begstr + 2
                  call write_message
                  error = .true.
               else
                  value(i) = data_dim_names(i)(begstr:endstr)//
     &                       char(0)
               endif
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'dim_display_names' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               begstr = strbeg (dim_display_names(i))
               endstr = strend (dim_display_names(i))
               if ( (endstr - begstr + 2) .gt. lenval ) then
                  write (message, 210) lenval+2
                  call write_message
                  error = .true.
               else
                  value(i) = dim_display_names(i)(begstr:endstr)//
     &                       char(0)
               endif
            enddo
         endif
      else
         write (message, 220) string(ibeg:iend)
         call write_message  
         error = .true.
      endif

c
c     Format statements.
c
 200  format (1x, 'When calling getaarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format ('At least ',i3,' bytes must be allocated for the ',
     &           'string argument in calls to subroutine getaarr.')
 220  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'getaarr.')
c
c
      return
      end
c
c
      subroutine getiarr (string, value, dim, error) 
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of aninteger variable array 
c        stored in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   int   output  the array of requested values.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'message.icl'
c
c     Argument declarations.
c     
      character *(*)  string
      integer         dim
      integer         value
      logical         error
      dimension value(dim)
c
c     Only in netcdf/open_var.c and newfile trans
      real data(4)
      integer size(4)
      common /cdf_dimvar/data,size
c
c
c     Local variable declarations.
c
      integer         i, j, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     data arrays.
c
      if ( dim .lt. MAXDIM ) then
         write (message, 200) string(ibeg:iend), MAXDIM
         call write_message 
         error = .true.
         return
      endif
      if ( string(ibeg:iend) .eq. 'lock' ) then
         do i = 1, MAXDIM
            value(i) = lock(i)
         enddo
      elseif ( string(ibeg:iend) .eq. 'ive_plock' ) then
         do i = 1, MAXDIM
            value(i) = ive_plock(i)
         enddo
      else if ( string(ibeg:iend) .eq. 'cpmin' ) then
         value(1) = cpmin1
         value(2) = cpmin2
         value(3) = cpmin3
         value(4) = cpmin4
      else if ( string(ibeg:iend) .eq. 'cpmax' ) then
         value(1) = cpmax1
         value(2) = cpmax2
         value(3) = cpmax3
         value(4) = cpmax4
      else if ( string(ibeg:iend) .eq. 'coord_dep' ) then
         if ( dim .lt. MAXDIM*MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM*MAXDIM
            call write_message 
            error = .true.
            return
         endif
         do j = 1, MAXDIM
            do i = 1, MAXDIM
               value(MAXDIM*(j-1)+i) = coord_dep(i,j)
            enddo
         enddo
c     CDFINFO
      elseif ( string(ibeg:iend) .eq. 'dimvar_size' ) then
         do i = 1, MAXDIM
            value(i) = size(i)
         enddo
      else
         write (message, 210) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling getiarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'getiarr.')
c
c
      return
      end
c
c
      subroutine getivar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of an integer variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     int   output  the value of the requested variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'color.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
      include 'segments.icl'
      include 'supma.icl'
      include 'vector.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'printer.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string
      integer         value
      logical         error
c
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c     attribute variables
c
      if     ( string(ibeg:iend) .eq. 'num_vals' ) then
         value = num_vals     
      elseif ( string(ibeg:iend) .eq. 'tnum_vals' ) then
         value = tnum_vals
c
c     color variables.
c
      elseif ( string(ibeg:iend) .eq. 'hicolor' ) then
         value = hicolor
      elseif ( string(ibeg:iend) .eq. 'locolor' ) then
         value = locolor
      elseif ( string(ibeg:iend) .eq. 'hipattern' ) then
         value = hipattern
      elseif ( string(ibeg:iend) .eq. 'lopattern' ) then
         value = lopattern
      elseif ( string(ibeg:iend) .eq. 'lock_entry' ) then
         value = lock_entry
      elseif ( string(ibeg:iend) .eq. 'zero_color' ) then
         value = zero_color
      elseif ( string(ibeg:iend) .eq. 'traj_entry' ) then
         value = traj_entry
c
c     conre variables.
c
      elseif ( string(ibeg:iend) .eq. 'nulbll' ) then
         value = nulbll
      elseif ( string(ibeg:iend) .eq. 'terr_nulbll' ) then
         value = terr_nulbll
c
c     plotops variables
c
      elseif ( string(ibeg:iend) .eq. 'linlog' ) then
         value = linlog
      elseif ( string(ibeg:iend) .eq. 'use_buttons' ) then
         value = use_buttons
      elseif ( string(ibeg:iend) .eq. 'filtyp' ) then
         value = filtyp
      elseif ( string(ibeg:iend) .eq. 'fill_frequency' ) then
         value = fill_frequency
      elseif ( string(ibeg:iend) .eq. 'multiple_read' ) then
         value = multiple_read
      elseif ( string(ibeg:iend) .eq. 'multiple_read_real' ) then
         value = multiple_read_real
      elseif ( string(ibeg:iend) .eq. 'multiple_read_start' ) then
         value = multiple_read_start
      elseif ( string(ibeg:iend) .eq. 'multiple_read_end' ) then
         value = multiple_read_end
      elseif ( string(ibeg:iend) .eq. 'multiple_read_flag' ) then
         value = multiple_read_flag
c
c     pltwin variables.
c
      elseif ( string(ibeg:iend) .eq. 'xaxis' ) then
         value = xaxis
      elseif ( string(ibeg:iend) .eq. 'yaxis' ) then
         value = yaxis
c
c     segments variables.
c
      elseif ( string(ibeg:iend) .eq. 'curseg' ) then
         value = curseg
      elseif ( string(ibeg:iend) .eq. 'curplot' ) then
         value = curplot
c
c     data variables.
c
      elseif ( string(ibeg:iend) .eq. 'cpmin1' ) then
         value = cpmin1
      elseif ( string(ibeg:iend) .eq. 'cpmin2' ) then
         value = cpmin2
      elseif ( string(ibeg:iend) .eq. 'cpmin3' ) then
         value = cpmin3
      elseif ( string(ibeg:iend) .eq. 'cpmin4' ) then
         value = cpmin4
      elseif ( string(ibeg:iend) .eq. 'cpmax1' ) then
         value = cpmax1
      elseif ( string(ibeg:iend) .eq. 'cpmax2' ) then
         value = cpmax2
      elseif ( string(ibeg:iend) .eq. 'cpmax3' ) then
         value = cpmax3
      elseif ( string(ibeg:iend) .eq. 'cpmax4' ) then
         value = cpmax4
      elseif ( string(ibeg:iend) .eq. 'maxdim' ) then
         value = maxdim
      elseif (string(ibeg:iend) .eq. 'num_dims') then
         value = num_dims
c
c     pltwin variables
c
         elseif ( string(ibeg:iend) .eq. 'tic_labels' ) then
            value =tic_labels
         elseif ( string(ibeg:iend) .eq. 'tic_decimals' ) then
            value =tic_decimals
c
c     vector variables.
c
      elseif ( string(ibeg:iend) .eq. 'vecpos' ) then
         value = vecpos
      elseif ( string(ibeg:iend) .eq. 'incx' ) then
         value = incx
      elseif ( string(ibeg:iend) .eq. 'incy' ) then
         value = incy
c
c     Warning timeout
c
      elseif ( string(ibeg:iend) .eq. 'warn_timeout' ) then
         value = warn_timeout
c
c     Printer
c
      elseif ( string(ibeg:iend) .eq. 'landscape' ) then
         value = landscape
c         
c
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'getivar.')
c
c
      return
      end
c
      subroutine getlvar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of a logical variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     logi  output  the value of the requested variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'conre.icl'
      include 'data.icl'
      include 'flags.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'strmln.icl'
      include 'supma.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'printer.icl'
      include 'traj.icl'
      include 'vector.icl'
      include 'color.icl'
c
c     vectors stuff
c
       logical veclock
       real vvmax
       common/com_vecmax/ veclock, vvmax
c
c
c     Argument declarations.
c     
      character *(*)  string
      logical         value
      logical         error
c     
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     attribute variables.
c
      if     ( string(ibeg:iend) .eq. 'no_zero' ) then
         value = no_zero
c
c
c     conre variables.
c
      elseif     ( string(ibeg:iend) .eq. 'cllab' ) then
         value = cllab
c      elseif ( string(ibeg:iend) .eq. 'nozero' ) then
c         value = nozero
      elseif ( string(ibeg:iend) .eq. 'single' ) then
         value = single
      elseif ( string(ibeg:iend) .eq. 'reset_spval' ) then
         value = reset_spval
c
c     flags variables.
c
      elseif ( string(ibeg:iend) .eq. 'mapflg' ) then
         value = mapflg
      elseif ( string(ibeg:iend) .eq. 'savflg' ) then
         value = savflg
      elseif ( string(ibeg:iend) .eq. 'movielabs' ) then
         value = movielabs
c
c     plotops variables
c
      elseif ( string(ibeg:iend) .eq. 'pltflg' ) then
         value = pltflg
      elseif ( string(ibeg:iend) .eq. 'terrflg' ) then
         value = terrflg
      elseif ( string(ibeg:iend) .eq. 'surface' ) then
         value = surface
      elseif ( string(ibeg:iend) .eq. 'average' ) then
         value = average
      elseif ( string(ibeg:iend) .eq. 'label' ) then
         value = label
      elseif ( string(ibeg:iend) .eq. 'vlabel' ) then
         value = vlabel
      elseif ( string(ibeg:iend) .eq. 'color_bar' ) then
         value = color_bar
c
c     supmap variables
c
      elseif ( string(ibeg:iend) .eq. 'defmap' ) then
         value = defmap
      elseif (string(ibeg:iend) .eq. 'exact_fit') then
         value = exact_fit   
c
c     strmln variables.
c
      elseif ( string(ibeg:iend) .eq. 'drstrmln' ) then
         value = drstrmln
c
c     read_var variable
c
      elseif (string(ibeg:iend) .eq. 'read_from_nc' ) then
         value = read_from_nc
c
c     vector variables
c
      elseif ( string(ibeg:iend) .eq. 'veclock' ) then
         value =  veclock
c
      elseif ( string(ibeg:iend) .eq. 'scale_vector' ) then
         value =  scale_vector
c
c     printer variables
c
      elseif ( string(ibeg:iend) .eq. 'color_printer' ) then
         value = color_printer
c
c     Trajectory variables
c
      elseif ( string(ibeg:iend) .eq.'time_ind' ) then
         value = time_ind
c
c
c     color variables
c
      elseif ( string(ibeg:iend) .eq.'traj_step_set' ) then
         value = traj_step_set
      elseif ( string(ibeg:iend) .eq.'traj_val_set' ) then
         value = traj_val_set
c
c     Nothing left
c
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'getlvar.')
c
c
      return
      end
c
      subroutine getrarr (string, value, dim, error) 
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of a real variable array stored
c        in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   real  output  the array of requested values.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'supma.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'conre.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string
      integer         dim
      real            value
      logical         error
      dimension value(dim)

c
c     Only in netcdf/open_var.c and newfile trans
      real data(4)
      integer size(4)
      common /cdf_dimvar/data,size
c
c
c     Local variable declarations.
c
      integer         i, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c     attribute array
c
      if     ( string(ibeg:iend) .eq. 'cont_values' ) then
         if ( dim .lt.  abs(num_vals) ) then
            write (message, 200) string(ibeg:iend), num_vals
            call write_message 
            error = .true.
         else
            do i = 1, abs(num_vals)
               value(i) = cont_values(i)
            enddo
         endif
c
c     conre array
c
      elseif ( string(ibeg:iend) .eq. 'tcont_values' ) then
         if ( dim .lt.  abs(tnum_vals) ) then
            write (message, 200) string(ibeg:iend), tnum_vals
            call write_message 
            error = .true.
         else
            do i = 1, abs(tnum_vals)
               value(i) = tcont_values(i)
            enddo
         endif
c
c
c     data arrays.
c
      elseif ( string(ibeg:iend) .eq. 'phmin' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = phmin(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'phmax' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = phmax(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmin' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plwmin(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmax' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plwmax(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmin_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plwmin_scaled(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmax_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plwmax_scaled(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmin' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plmin(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmax' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plmax(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmin_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plmin_scaled(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmax_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = plmax_scaled(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'stag' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = stag(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'grid_delta' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = grid_delta(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'field_delta' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = field_delta(i)
            enddo
         endif
c
c     plotops variables.
c
      elseif ( string(ibeg:iend) .eq. 'locatn' ) then
         if ( dim .ne. 3 ) then
            write (message, 200) string(ibeg:iend), 3
            call write_message 
            error = .true.
         else
            do i = 1, 3
               value(i) = locatn(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'point_1' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = point_1(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'point_2' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = point_2(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'point_3' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = point_3(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'pat1') then
         do i=1, 2
            value(i) = pat1(i)
         enddo
      elseif ( string(ibeg:iend) .eq. 'pat2') then
         do i=1, 2
            value(i) = pat2(i)
         enddo
      elseif ( string(ibeg:iend) .eq. 'pat3') then
         do i=1, 2
            value(i) = pat3(i)
         enddo
      elseif ( string(ibeg:iend) .eq. 'pat4') then
         do i=1, 2
            value(i) = pat4(i)
         enddo
      elseif ( string(ibeg:iend) .eq. 'pat5') then
         do i=1, 2
            value(i) = pat5(i)
         enddo
      elseif ( string(ibeg:iend) .eq. 'pat6') then
         do i=1, 2
            value(i) = pat6(i)
         enddo
c
c     supma arrays.
c
      elseif ( string(ibeg:iend) .eq. 'plm1' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               value(i) = plm1(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plm2' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               value(i) = plm2(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plm3' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               value(i) = plm3(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plm4' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               value(i) = plm4(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'scaled_loc' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = scaled_loc(i)
            enddo
         endif
c     CDFINFO
      elseif ( string(ibeg:iend) .eq. 'dimvar_data' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               value(i) = data(i)
            enddo
         endif
      else
         write (message, 210) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling getrarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'getrarr.')
c
c
      return
      end
c
c
      subroutine getrvar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of a real variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     real  output  the value of the requested variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'supma.icl'
      include 'conre.icl'
      include 'lineplt.icl'
      include 'vector.icl'
      include 'message.icl'
      include 'color.icl'
      include 'attributes.icl'
      include 'pltwin.icl'
      include 'traj.icl'
c
c     vectors stuff
c
       logical veclock
       real vvmax
       common/com_vecmax/ veclock, vvmax
c
c
c     Argument declarations.
c     
      character *(*)  string
      real            value
      logical         error
c
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     attribute values
c
      if ( string(ibeg:iend) .eq. 'cont_int' ) then
         value = cont_int
      elseif ( string(ibeg:iend) .eq. 'sint' ) then
         value = sint
c
c
c     conre variables.
c
      elseif ( string(ibeg:iend) .eq. 'cint' ) then
         value = cint
      elseif     ( string(ibeg:iend) .eq. 'spval' ) then 
         value = spval
      elseif ( string(ibeg:iend) .eq. 'tcint' ) then
         value = tcint
      elseif ( string(ibeg:iend) .eq. 'topo_scale' ) then
         value = topo_scale
c
c     plotops variables.
c
      elseif ( string(ibeg:iend) .eq. 'linwdth' ) then
         value = linwdth
      elseif ( string(ibeg:iend) .eq. 'xloc' ) then
         value = xloc
      elseif ( string(ibeg:iend) .eq. 'yloc' ) then
         value = yloc
      elseif ( string(ibeg:iend) .eq. 'zloc' ) then
         value = zloc
      elseif ( string(ibeg:iend) .eq. 'xmajor' ) then
         value = xmajor
      elseif ( string(ibeg:iend) .eq. 'xminor' ) then
         value = xminor
c      elseif ( string(ibeg:iend) .eq. 'xmajorl' ) then
c         value = xmajorl
c      elseif ( string(ibeg:iend) .eq. 'xminorl' ) then
c         value = xminorl
      elseif ( string(ibeg:iend) .eq. 'ymajor' ) then
         value = ymajor
      elseif ( string(ibeg:iend) .eq. 'yminor' ) then
         value = yminor
c      elseif ( string(ibeg:iend) .eq. 'ymajorl' ) then
c         value = ymajorl
c      elseif ( string(ibeg:iend) .eq. 'yminorl' ) then
c         value = yminorl
      elseif ( string(ibeg:iend) .eq. 'tloc' ) then
         value = tloc
      elseif ( string(ibeg:iend) .eq. 'time' ) then
         value = tloc
c
c     supma variables.
c
      elseif ( string(ibeg:iend) .eq. 'plat' ) then
         value = plat
      elseif ( string(ibeg:iend) .eq. 'plon' ) then
         value = plon
      elseif ( string(ibeg:iend) .eq. 'rota' ) then
         value = rota
      elseif ( string(ibeg:iend) .eq. 'grdspa' ) then
         value = grdspa
      elseif ( string(ibeg:iend) .eq. 'mapwin_u1' ) then
         value = mapwin_u1
      elseif ( string(ibeg:iend) .eq. 'mapwin_u2' ) then
         value = mapwin_u2
      elseif ( string(ibeg:iend) .eq. 'mapwin_v1' ) then
         value = mapwin_v1
      elseif ( string(ibeg:iend) .eq. 'mapwin_v2' ) then
         value = mapwin_v2
c
c     Lineplt variables.
c
      elseif ( string(ibeg:iend) .eq. 'abscissa_min' ) then
         value = abscissa_min
      elseif ( string(ibeg:iend) .eq. 'abscissa_max' ) then
         value = abscissa_max
      elseif ( string(ibeg:iend) .eq. 'ordinate_min' ) then
         value = ordinate_min
      elseif ( string(ibeg:iend) .eq. 'ordinate_max' ) then
         value = ordinate_max
c
c     Vector variables.
c
      elseif ( string(ibeg:iend) .eq. 'vect_spval' ) then
         value = vect_spval
c
c     actually, it's multiplicative coeff. for vectors
c
      elseif ( string(ibeg:iend) .eq. 'vmax' ) then
         value = vmax
      elseif ( string(ibeg:iend) .eq. 'veclockmax' ) then
          value = vvmax
c
c     Traj variables.
c
      elseif ( string(ibeg:iend) .eq. 'trajbeg' ) then
         value = trajbeg
      elseif ( string(ibeg:iend) .eq. 'trajend' ) then
         value = trajend
      elseif ( string(ibeg:iend) .eq. 'traj_val' ) then
         value = traj_val
      elseif ( string(ibeg:iend) .eq. 'traj_step' ) then
         value = traj_step

c
c     pltwin variables
c
         elseif ( string(ibeg:iend) .eq. 'tic_width_maj' ) then
            value =tic_width_maj
         elseif ( string(ibeg:iend) .eq. 'tic_width_min' ) then
            value =tic_width_min
         elseif ( string(ibeg:iend) .eq. 'tic_scale' ) then
            value =tic_scale
cc
c     Color Variables
c
      elseif ( string(ibeg:iend) .eq. 'midval' ) then
         value = midval
      elseif ( string(ibeg:iend) .eq. 'lock_int' ) then
         value = lock_int
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'getrvar.')
c
c
      return
      end
c
c
      subroutine getdarr (string, value, dim, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of a real*8 variable array stored
c        in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   real*8  output  the array of requested values.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c-----------------------------------------------------------------------
c
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c     
      integer         dim
      character *(*)  string
      real*8          value
      logical         error
      dimension       value(dim)
c
c
c     Local variable declarations.
c
      integer         i, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c     Units array.
c
c
c     Units arrays.
c
      if ( string(ibeg:iend) .eq. 'domain_slope' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               value(i) = domain_slope(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'domain_intercept' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               value(i) = domain_intercept(i)
            enddo
         endif

      else
         write (message, 210) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling getdarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'getdarr.')
c
c
      return
      end
c
c
c
c
      subroutine getdvar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of a real*8 variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     real*8 output  the value of the requested variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string
      real*8          value
      logical         error
c
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c     Units variables.
c
      if ( string(ibeg:iend) .eq. 'data_slope' ) then
         value = data_slope
      elseif ( string(ibeg:iend) .eq. 'data_intercept' ) then
         value = data_intercept
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'getdvar.')
c
c
      return
      end
c
c
      subroutine setaarr (string, value, dim, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of an alphanumeric variable 
c        array stored in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   char  input   the array of values.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string, value
      integer         dim
      logical         error
      dimension value(dim)
c
c
c     Local variable declarations.
c
      integer         begval, endval, i, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     data arrays.
c
      if     ( string(ibeg:iend) .eq. 'domain_units' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               begval = strbeg (value(i))
               endval = strend (value(i)) 
               if (endval.lt.len(value(i)).and.
     &              value(i)(endval+1:endval+1).eq.char(0))
     &              endval = endval+1
               domain_units(i) = value(i)(begval:endval)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'domain_display_units' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               begval = strbeg (value(i))
               endval = strend (value(i)) 
               if (endval.lt.len(value(i)).and.
     &              value(i)(endval+1:endval+1).eq.char(0))
     &              endval = endval+1
               domain_display_units(i) = value(i)(begval:endval)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'data_dim_names' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               begval = strbeg (value(i))
               endval = strend (value(i)) 
               if (endval.lt.len(value(i)).and.
     &              value(i)(endval+1:endval+1).eq.char(0))
     &              endval = endval+1
               data_dim_names(i) = value(i)(begval:endval)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'dim_display_names' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               begval = strbeg (value(i))
               endval = strend (value(i)) 
               if (endval.lt.len(value(i)).and.
     &              value(i)(endval+1:endval+1).eq.char(0))
     &              endval = endval+1
               dim_display_names(i) = value(i)(begval:endval)
            enddo
         endif
      else
         write (message, 220) string(ibeg:iend)
         call write_message  
         error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling setaarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 220  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'setaarr.')
c
c
      return
      end
c
c
      subroutine setavar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of an alphanumeric variable 
c        stored in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     char  output  the value of the variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'color.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'flags.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'supma.icl'
      include 'units.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'traj.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string, value
      logical         error
c
c
c     Local variable declarations.
c
      integer         begval, endval, ibeg, iend, lenvar
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg   = strbeg (string)
      iend   = strend (string)
      begval = strbeg (value)
      endval = strend (value)
c
c
      if ( string(ibeg:iend) .eq. 'clrtab' ) then
         lenvar = len (clrtab)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            clrtab = value(begval:endval)
         endif
c
c     data variables.
c
      elseif ( string(ibeg:iend) .eq. 'datfil' ) then
         lenvar = len (datfil)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            datfil = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'field' ) then
         lenvar = len (field)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            field = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'button_name' ) then
         lenvar = len (button_name)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            button_name = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'field_pointer' ) then
         lenvar = len (field_pointer)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            field_pointer = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'graphics_path' ) then
         lenvar = len (graphics_path)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            graphics_path = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'transform_path' ) then
         lenvar = len (transform_path)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
        else
            transform_path = value(begval:endval)
         endif
c
c     plotops variables.
c
      elseif ( string(ibeg:iend) .eq. 'plotyp1' ) then
         lenvar = len (plotyp1)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            plotyp1 = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'plotyp2' ) then
         lenvar = len (plotyp2)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            plotyp2 = value(begval:endval)
         endif
c
c     supma variables.
c
      elseif ( string(ibeg:iend) .eq. 'maplim' ) then
         lenvar = len (maplim)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            maplim = value(begval:endval)
         endif
      elseif ( string(ibeg:iend) .eq. 'outlin' ) then
         lenvar = len (outlin)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            outlin = value(begval:endval)
         endif
       elseif ( string(ibeg:iend) .eq. 'proj' ) then
         lenvar = len (proj)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message 
            error = .true.
         else
            proj = value(begval:endval)
         endif
c
c     units variables.
c
      elseif ( string(ibeg:iend) .eq. 'data_units' ) then
         lenvar = len (data_units)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message  
            error = .true.
         else
            data_units = value(begval:endval)
            call catnull(data_units, len(data_units), error)
         endif
      elseif ( string(ibeg:iend) .eq. 'data_display_units' ) then
         lenvar = len (data_display_units)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110)  value(begval:endval)
            call write_message  
            error = .true.
         else
            data_display_units = value(begval:endval)
            call catnull(data_display_units, len(data_display_units),
     &           error)
         endif
c
c
c     traj variables.
c
      elseif ( string(ibeg:iend) .eq. 'traj_background_field' ) then
         lenvar = len (traj_background_field)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message  
            error = .true.
         else
            traj_background_field = value(begval:endval)
            call catnull(traj_background_field, 
     &                   len(traj_background_field), error)
         endif
      elseif ( string(ibeg:iend) .eq. 'old_traj_background' ) then
         lenvar = len (old_traj_background)
         if ( (endval - begval + 1) .gt. lenvar ) then
            write (message, 110) value(begval:endval)
            call write_message  
            error = .true.
         else
            old_traj_background = value(begval:endval)
            call catnull(old_traj_background, 
     &                   len(old_traj_background), error)
            write(6,*)'old_traj_background ',old_traj_background
         endif
      else
         write (message, 100) string(ibeg:iend)
         call write_message  
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'setavar.')
 110  format (1x,'The length of ',a,' is too long to be inserted into ',
     &           'the specified variable in subroutine setavar.')
c
c
      return
      end
c
c
      subroutine setiarr (string, value, dim, error) 
c-----------------------------------------------------------------------
c     Purpose:
c        This routine returns the value of aninteger variable array 
c        stored in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   int   input   the array of requested values.
c        dim     int   output  the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'plotops.icl'
      include 'message.icl'
c
c     Argument declarations.
c     
      character *(*)  string
      integer         dim
      integer         value
      logical         error
      dimension value(dim)
c
c
c
c     Only in netcdf/open_var.c and newfile trans
      real data(4)
      integer size(4)
      common /cdf_dimvar/data,size
c
c     Local variable declarations.
c
      integer         i, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend, MAXDIM
      parameter       (MAXDIM = 4)
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     data arrays.
c
      if ( dim .ne. MAXDIM ) then
         write (message, 200) string(ibeg:iend), MAXDIM
         call write_message 
         error = .true.
      endif
      if     ( string(ibeg:iend) .eq. 'lock' ) then
         do i = 1, MAXDIM
            lock(i) = value(i)
         enddo
      elseif     ( string(ibeg:iend) .eq. 'ive_plock' ) then
         do i = 1, MAXDIM
            ive_plock(i) = value(i)
         enddo
      elseif     ( string(ibeg:iend) .eq. 'dimvar_size' ) then
         do i = 1, MAXDIM
            size(i) = value(i)
         enddo
       else
         write (message, 210) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling setrirr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'setiarr.')
c
c
      return
      end
c
c
      subroutine setivar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of an integer variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     int   output  the value of the variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'color.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'plotops.icl'
      include 'pltwin.icl'
      include 'segments.icl'
      include 'supma.icl'
      include 'vector.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'printer.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string
      integer         value
      logical         error
c
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c
c     color variables.
c
      if ( string(ibeg:iend) .eq. 'hicolor' ) then
         hicolor = value
      elseif ( string(ibeg:iend) .eq. 'locolor' ) then
         locolor = value
      elseif ( string(ibeg:iend) .eq. 'hipattern' ) then
         hipattern = value
      elseif ( string(ibeg:iend) .eq. 'lopattern' ) then
         lopattern = value
      elseif ( string(ibeg:iend) .eq. 'zero_color' ) then
         zero_color = value
      elseif ( string(ibeg:iend) .eq. 'traj_entry' ) then
         traj_entry = value
c     
c     conre variables.
c
      elseif ( string(ibeg:iend) .eq. 'nulbll' ) then
         nulbll = value
      elseif ( string(ibeg:iend) .eq. 'tnum_vals' ) then
         tnum_vals = value
c
c     plotops variables
c
      elseif ( string(ibeg:iend) .eq. 'linlog' ) then
         linlog = value
      elseif ( string(ibeg:iend) .eq. 'use_buttons' ) then
         use_buttons = value
      elseif ( string(ibeg:iend) .eq. 'filtyp' ) then
         filtyp = value
      elseif ( string(ibeg:iend) .eq. 'fill_frequency' ) then
         fill_frequency = value
      elseif ( string(ibeg:iend) .eq. 'multiple_read' ) then
         multiple_read = value
      elseif ( string(ibeg:iend) .eq. 'multiple_read_real' ) then
         multiple_read_real = value
      elseif ( string(ibeg:iend) .eq. 'multiple_read_start' ) then
         multiple_read_start = value
      elseif ( string(ibeg:iend) .eq. 'multiple_read_end' ) then
         multiple_read_end = value
      elseif ( string(ibeg:iend) .eq. 'multiple_read_flag' ) then
         multiple_read_flag = value
c
c     pltwin variables.
c
      elseif ( string(ibeg:iend) .eq. 'xaxis' ) then
         xaxis = value
      elseif ( string(ibeg:iend) .eq. 'yaxis' ) then
         yaxis = value
      elseif ( string(ibeg:iend) .eq. 'tic_labels' ) then
         tic_labels = value
      elseif ( string(ibeg:iend) .eq. 'tic_decimals' ) then
         tic_decimals = value
c
c     segments variables.
c
      elseif ( string(ibeg:iend) .eq. 'curseg' ) then
         curseg = value
c
c     data variables.
c
      elseif ( string(ibeg:iend) .eq. 'cpmin1' ) then
         cpmin1 = value
      elseif ( string(ibeg:iend) .eq. 'cpmin2' ) then
         cpmin2 = value
      elseif ( string(ibeg:iend) .eq. 'cpmin3' ) then
         cpmin3 = value
      elseif ( string(ibeg:iend) .eq. 'cpmin4' ) then
         cpmin4 = value
      elseif ( string(ibeg:iend) .eq. 'cpmax1' ) then
         cpmax1 = value
      elseif ( string(ibeg:iend) .eq. 'cpmax2' ) then
         cpmax2 = value
      elseif ( string(ibeg:iend) .eq. 'cpmax3' ) then
         cpmax3 = value
      elseif ( string(ibeg:iend) .eq. 'cpmax4' ) then
         cpmax4 = value
      elseif (string(ibeg:iend) .eq. 'num_dims') then
         num_dims = value
c
c     vector variables.
c
      elseif (string(ibeg:iend) .eq. 'vecpos' ) then
         vecpos = value
      elseif ( string(ibeg:iend) .eq. 'incx' ) then
         incx = value
      elseif ( string(ibeg:iend) .eq. 'incy' ) then
         incy = value
c
c     Warning timeout
c
      elseif ( string(ibeg:iend) .eq. 'warn_timeout' ) then
         warn_timeout = value
c
c
c     Printer
c
      elseif ( string(ibeg:iend) .eq. 'landscape' ) then
         value = landscape
c
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'setivar.')
c
c
      return
      end
c
      subroutine setlvar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of a logical variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     logi  output  the value of the variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'conre.icl'
      include 'data.icl'
      include 'flags.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'strmln.icl'
      include 'supma.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'traj.icl'
      include 'vector.icl'
      include 'color.icl'
c
c     vectors stuff
c
       logical veclock
       real vvmax
       common/com_vecmax/ veclock, vvmax
c
c
c     Argument declarations.
c     
      character *(*)  string
      logical         value
      logical         error
c     
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c     conre variables.
c
      if     ( string(ibeg:iend) .eq. 'cllab' ) then
         cllab = value
      elseif ( string(ibeg:iend) .eq. 'nozero' ) then
         nozero = value
      elseif ( string(ibeg:iend) .eq. 'single' ) then
         single = value
      elseif ( string(ibeg:iend) .eq. 'reset_spval' ) then
         reset_spval = value
c
c     flags variables.
c
      elseif ( string(ibeg:iend) .eq. 'mapflg' ) then
         mapflg = value
      elseif ( string(ibeg:iend) .eq. 'savflg' ) then
         savflg = value
      elseif ( string(ibeg:iend) .eq. 'movielabs' ) then
         movielabs = value
c
c     plotops variables
c
      elseif ( string(ibeg:iend) .eq. 'pltflg' ) then
         pltflg = value
      elseif ( string(ibeg:iend) .eq. 'terrflg' ) then
         terrflg = value
      elseif ( string(ibeg:iend) .eq. 'surface' ) then
         surface = value
      elseif ( string(ibeg:iend) .eq. 'average' ) then
         average = value
      elseif ( string(ibeg:iend) .eq. 'label' ) then
         label = value
      elseif ( string(ibeg:iend) .eq. 'vlabel' ) then
         vlabel = value
      elseif ( string(ibeg:iend) .eq. 'color_bar' ) then
         color_bar = value
c
c     supmap variables
c
      elseif ( string(ibeg:iend) .eq. 'defmap' ) then
         defmap = value
      elseif (string(ibeg:iend) .eq. 'exact_fit') then
         exact_fit = value
c
c     strmln variables.
c
      elseif ( string(ibeg:iend) .eq. 'drstrmln' ) then
         drstrmln = value
c
c     read_var variable
c
      elseif (string(ibeg:iend) .eq. 'read_from_nc' ) then
         read_from_nc = value
c
c     vector variables.
c
      elseif ( string(ibeg:iend) .eq. 'veclock' ) then
         veclock = value
c
      elseif ( string(ibeg:iend) .eq. 'scale_vector' ) then
         scale_vector = value
c
c
c     Trajectory variables
c
      elseif ( string(ibeg:iend) .eq.'time_ind' ) then
         time_ind = value
c
c
c     color variables
c
      elseif ( string(ibeg:iend) .eq.'traj_step_set' ) then
         traj_step_set = value
      elseif ( string(ibeg:iend) .eq.'traj_val_set' ) then
         traj_val_set = value
c
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'setlvar.')
c
c
      return
      end
c
c
      subroutine setrarr (string, value, dim, error) 
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of a real variable array stored
c        in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   real  output  the array of requested values.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c     History:
c-----------------------------------------------------------------------
c
      include 'data.icl'
      include 'plotops.icl'
      include 'supma.icl'
      include 'units.icl'
      include 'message.icl'
      include 'attributes.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string
      integer         dim
      real            value
      logical         error
      dimension value(dim)
c
c     Only in netcdf/open_var.c and newfile trans
      real data(4)
      integer size(4)
      common /cdf_dimvar/data,size
c
c     Local variable declarations.
c
      integer         i, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c
c
c     data arrays.
c
      if     ( string(ibeg:iend) .eq. 'phmin' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               phmin(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'phmax' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               phmax(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmin' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plwmin(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmax' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plwmax(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmin_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plwmin_scaled(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plwmax_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plwmax_scaled(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmin' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plmin(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmax' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plmax(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmin_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plmin_scaled(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plmax_scaled' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               plmax_scaled(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'stag' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               stag(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'grid_delta' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               grid_delta(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'field_delta' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
               field_delta(i) = value(i)
            enddo
         endif
c
c     plotops variables.
c
      elseif ( string(ibeg:iend) .eq. 'locatn' ) then
         if ( dim .ne. 3 ) then
            write (message, 200) string(ibeg:iend), 3
            call write_message 
            error = .true.
         else
            do i = 1, 3
               locatn(i) = value(i)
            enddo
         endif
c points cases
      elseif ( string(ibeg:iend) .eq. 'point_1' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
                point_1(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'point_2' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
                point_2(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'point_3' ) then
         if ( dim .ne. MAXDIM ) then
            write (message, 200) string(ibeg:iend), MAXDIM
            call write_message 
            error = .true.
         else
            do i = 1, MAXDIM
                point_3(i) = value(i)
            enddo
         endif
c
c     supma arrays.
c
      elseif ( string(ibeg:iend) .eq. 'plm1' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               plm1(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plm2' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               plm2(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plm3' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               plm3(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'plm4' ) then
         if ( dim .ne. 2 ) then
            write (message, 200) string(ibeg:iend), 2
            call write_message 
            error = .true.
         else
            do i = 1, 2
               plm4(i) = value(i)
            enddo
         endif
c
c     Units arrays.
c
      elseif ( string(ibeg:iend) .eq. 'domain_slope' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               domain_slope(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'domain_intercept' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               domain_intercept(i) = value(i)
            enddo
         endif
C     CDFINFO
      elseif ( string(ibeg:iend) .eq. 'dimvar_data' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               data(i) = value(i)
            enddo
         endif
      else
            write (message, 210) string(ibeg:iend)
            call write_message 
            error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling setrarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'setrarr.')
c
c
      return
      end
c
c
      subroutine setrvar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of a real variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     real  output  the value of the variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'conre.icl'
      include 'data.icl'
      include 'flags.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'supma.icl'
      include 'units.icl'
      include 'lineplt.icl'
      include 'message.icl'
      include 'attributes.icl'
      include 'pltwin.icl'
      include 'vector.icl'
      include 'traj.icl'
      include 'color.icl'
c
c     vectors stuff
c
       logical veclock
       real vvmax
       common/com_vecmax/ veclock, vvmax
c
c     Argument declarations.
c     
      character *(*)  string
      real            value
      logical         error
c
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c
c
c     conre variables.
c
      if     ( string(ibeg:iend) .eq. 'spval' ) then 
         spval = value
      elseif ( string(ibeg:iend) .eq. 'cint' ) then
         cint = value
      elseif ( string(ibeg:iend) .eq. 'tcint' ) then
         tcint = value
      elseif ( string(ibeg:iend) .eq. 'topo_scale' ) then
         topo_scale = value
c
c     plotops variables.
c
      elseif ( string(ibeg:iend) .eq. 'linwdth' ) then
         linwdth = value
      elseif ( string(ibeg:iend) .eq. 'xloc' ) then
         xloc = value
      elseif ( string(ibeg:iend) .eq. 'yloc' ) then
         yloc = value
      elseif ( string(ibeg:iend) .eq. 'zloc' ) then
         zloc = value
      elseif ( string(ibeg:iend) .eq. 'angle' ) then
         angle = value
      elseif ( string(ibeg:iend) .eq. 'degang' ) then
         degang = value
      elseif ( string(ibeg:iend) .eq. 'xmajor' ) then
         xmajor = value
      elseif ( string(ibeg:iend) .eq. 'xminor' ) then
         xminor = value
      elseif ( string(ibeg:iend) .eq. 'ymajor' ) then
         ymajor = value
      elseif ( string(ibeg:iend) .eq. 'yminor' ) then
         yminor = value
      elseif ( string(ibeg:iend) .eq. 'tloc' ) then
         tloc = value
c
c     supma variables.
c
      elseif ( string(ibeg:iend) .eq. 'plat' ) then
         plat = value
      elseif ( string(ibeg:iend) .eq. 'plon' ) then
         plon = value
      elseif ( string(ibeg:iend) .eq. 'rota' ) then
         rota = value
      elseif ( string(ibeg:iend) .eq. 'grdspa' ) then
         grdspa = value
c
c     Units variables.
c
      elseif ( string(ibeg:iend) .eq. 'data_slope' ) then
         data_slope = value
      elseif ( string(ibeg:iend) .eq. 'data_intercept' ) then
         data_intercept = value
c
c     Lineplt variables.
c
      elseif ( string(ibeg:iend) .eq. 'abscissa_min' ) then
         abscissa_min = value
      elseif ( string(ibeg:iend) .eq. 'abscissa_max' ) then
         abscissa_max = value
      elseif ( string(ibeg:iend) .eq. 'ordinate_min' ) then
         ordinate_min = value
      elseif ( string(ibeg:iend) .eq. 'ordinate_max' ) then
         ordinate_max = value
c     pltwin variables
c
      elseif ( string(ibeg:iend) .eq. 'tic_width_maj' ) then
         tic_width_maj = value
      elseif ( string(ibeg:iend) .eq. 'tic_width_min' ) then
         tic_width_min = value
      elseif ( string(ibeg:iend) .eq. 'tic_scale' ) then
         tic_scale = value
c
c     traj vars
c
      elseif ( string(ibeg:iend) .eq. 'traj_val' ) then
         traj_val = value
      elseif ( string(ibeg:iend) .eq. 'traj_step' ) then
         traj_step = value

c     
c     vector vars
c
      elseif ( string(ibeg:iend) .eq. 'vmax' ) then
         vmax = value
      elseif ( string(ibeg:iend) .eq. 'veclockmax' ) then
          vvmax = value
c
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'setrvar.')
c
c
      return
      end
c
c
      subroutine setdarr (string, value, dim, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of a real*8 variable array stored
c        in a common block.
c     Arguments:
c        string  char  input   the array name.
c        value   real*8  input  the array of requested values.
c        dim     int   input   the dimension of value in the calling
c                              routine. If dim != the dimension of the
c                              requested variable, the error flag is 
c                              set to true and control is returned to
c                              the calling routine.
c        error   logi  output  indicates errors found in this routine.
c                              error = .false.  no error detected.
c                              error = .true.   error detected.
c-----------------------------------------------------------------------
c
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c     
      integer         dim
      character *(*)  string
      real*8          value
      logical         error
      dimension       value(dim)
c
c
c     Local variable declarations.
c
      integer         i, ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c     Units array.
c
c
c     Units arrays.
c
      if ( string(ibeg:iend) .eq. 'domain_slope' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               domain_slope(i) = value(i)
            enddo
         endif
      elseif ( string(ibeg:iend) .eq. 'domain_intercept' ) then
         if ( dim .ne. MAX_UNITS ) then
            write (message, 200) string(ibeg:iend), MAX_UNITS
            call write_message 
            error = .true.
         else
            do i = 1, MAX_UNITS
               domain_intercept(i) = value(i)
            enddo
         endif

      else
         write (message, 210) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 200  format (1x, 'When calling setdarr, the dimension of ',a,' must ',
     &            'be equal to ',i2, '.')
 210  format (1x, 'The array ',a,' is not available to subroutine ',
     &            'setdarr.')
c
c
      return
      end
c
c
c
c
      subroutine setdvar (string, value, error)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine sets the value of a real*8 variable stored
c        in a common block.
c     Arguments:
c        string  char  input   the variable name.
c        var     real*8 input  the value of the requested variable.
c        error   logi  output  indicates whether the requested variable
c                              was found in the common blocks.
c                              error = .false.  variable was found.
c                              error = .true.   variable was not found.
c     History:
c-----------------------------------------------------------------------
c
      include 'units.icl'
      include 'message.icl'
c
c
c     Argument declarations.
c     
      character *(*)  string
      real*8          value
      logical         error
c
c
c     Local variable declarations.
c
      integer         ibeg, iend
c
c
c     External function declarations.
c
      integer         strbeg, strend
c
c
c
c
      error = .false.
c
c
      ibeg = strbeg (string)
      iend = strend (string)
c
c     Units variables.
c
      if ( string(ibeg:iend) .eq. 'data_slope' ) then
         data_slope = value
      elseif ( string(ibeg:iend) .eq. 'data_intercept' ) then
         data_intercept= value
      else
         write (message, 100) string(ibeg:iend)
         call write_message 
         error = .true.
      endif
c
c
c     Format statements.
c
 100  format (1x,'The variable ',a,' is not available to subroutine ',
     &           'setdvar.')
c
c
      return
      end
