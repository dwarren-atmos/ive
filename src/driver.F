#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
#ifdef Linux
      recursive subroutine driver (comlin, iflag)
#else
      subroutine driver (comlin, iflag)
#endif
c-----------------------------------------------------------------------
c     $Id: driver.F,v 1.92 2007-05-25 15:48:41 warren Exp $
c     Purpose:
c        This routine is the main driver for the UARS routines.
c
c     Arguments:
c        comlin  char  input  user supplied command line.
c        iflag   int   input  flag passed from the X-windows routines.
c                             iflag = 0  driver is called from the 
c                                        command line interface.
c                             iflag = 1  driver is called from a window
c                                        interface.
c     History:
c        $Log: driver.F,v $
c        Revision 1.92  2007-05-25 15:48:41  warren
c        Added big_label command. - movie labels on original background.
c
c        Revision 1.91  2006/08/24 22:08:40  warren
c        added vlabel command to get max vectors plotted even if label=off
c
c        Revision 1.90  2006/08/22 23:58:13  warren
c        Added commands draw_point and draw_point_map
c
c        Revision 1.89  2006/08/22 21:31:29  warren
c        added draw_line_map command for entering line end points in long,lat
c
c        Revision 1.88  2002/08/09 19:57:57  warren
c        Fixed for Linux and tested with DEC sun etc.
c        most items inside #ifdef's, or were general cleanup/fixes
c
c        Revision 1.87  2002/06/11 17:26:16  harryive
c        Change aspect ratio to affect the plot, not just printing.
c
c        Add first part of contour value specification.
c
c        Revision 1.86  2002/05/30 16:43:05  warren
c        add a "grayline" command
c
c        Revision 1.85  2002/05/22 17:56:44  davidive
c        First working version of trajectories colored by alternative fields
c        There is not a complete interface yet!!!
c
c        Revision 1.84  2002/03/11 20:14:16  davidive
c        Fixed ui_pudate to update trajectory step and value.
c
c        Revision 1.83  2001/11/15 21:41:40  davidive
c        Reset all traj values on new plot
c
c        Revision 1.82  2001/11/02 18:51:17  davidive
c        converted rdctab to c, fixed trajectories so that their independant color
c        tables work, including setting center color, center value and interval.
c
c        Revision 1.81  2001/08/15 22:25:46  davidive
c        Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
c        Added code to set up which axis to use and to convert the labels in 1d plots.
c
c        Revision 1.80  2001/04/11 19:11:02  harryive
c        Add new_field_trans and load_atts calls to the end of getvar.  Remove them
c        from other places where they are now no longer needed.
c
c        Revision 1.79  2001/03/14 20:18:53  warren
c        replaced superfluous ui_update (COLORTAB_UI)
c
c        Revision 1.78  2001/02/26 21:20:24  harry
c        Fix point plotting error.
c
c        Revision 1.77  2001/01/23 22:47:10  davidive
c        cpline.f: fixed to use zero_color
c        everything else: First cut at wireframes and isosurfaces. No dimentions yet
c        no widgets yet.
c
c        Revision 1.76  2000/12/12 17:58:22  harry
c        Fix problems with arbitrary line slices.
c
c        Revision 1.75  2000/05/09 19:53:47  harry
c        Seperate contour and line colors.  Make patterns work like colors.
c
c        Revision 1.74  2000/03/28 23:43:30  warren
c        Added reload_file function
c
c        Revision 1.73  2000/03/09 17:53:03  warren
c        Rest of stuff to pop up color widget when doing a filled plot
c
c        Revision 1.72  2000/02/11 23:14:31  harry
c        Allow surface 1d plots.  Fix Z slider for surface plots.
c
c        Revision 1.71  1999/12/22 00:33:13  warren
c        Fixed Dales crashing bug when changing from point slicer to normal. Also
c        have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c        Revision 1.70  1999/03/23 19:10:27  warren
c        Added command to set units on any field reguardless of current units.
c        This is primarily for fields derived by IVE math.
c
c        Revision 1.69  1999/02/10 23:13:35  harry
c        Get rid of unused command vector_scale.
c
c        Revision 1.68  1999/02/04 00:28:26  harry
c        Add vector scaling.
c
c        Revision 1.67  1999/01/29 20:46:25  warren
c        ets the flag for time independence and the plot the trajectories
c
c        Revision 1.66  1998/12/31 16:01:39  warren
c        First cut of new trajectory look and feel. Trajectories are no longer
c        on the popup menu, just plot type.
c
c        Revision 1.65  1998/12/29 16:40:20  harry
c        Properly loop plots with "draw_line" command.
c
c        Revision 1.64  1998/09/30 22:28:31  harry
c        Rewrite makpointplt and add ability for 2 average points, fix errors
c        in the 1d slicers, remove obsolete yuri routines.
c
c        Revision 1.63  1998/09/03 19:19:20  harry
c        Add in 2d line plots - correct bugs in other averaging.
c
c        Revision 1.62  1998/07/16 22:10:54  harryive
c        Add two dimensional slicing with 2 averaged dimensions.
c
c        Revision 1.61  1998/06/11 21:53:12  harry
c        Add FILLPATTERN ui_update.
c
c        Revision 1.60  1998/06/03 17:54:39  harry
c        Fix tick mark labels.   Add new command COLOR_BAR.
c
c        Revision 1.59  1998/03/04 19:13:59  davidive
c        First cut at trajectories and trajectory widget using colors to show depth
c        and arrows to show direction.
c
c        Revision 1.58  1997/08/18 23:39:21  harry
c        Do not allow more than one dimension to be averaged.
c
c        Revision 1.57  1997/06/27 21:06:51  harry
c        Designed multiplot printing.
c
c        Revision 1.56  1997/06/12 15:28:17  davidive
c        Fix multidimentional 1 D plots to pay attention to min, max and tic mark
c        settings, and remember them.
c
c        Revision 1.55  1997/06/03 19:14:18  davidive
c        The big mouse slicing changeover. We have the new form, only requires you
c        set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
c        always runs through init_points_ to catch all these things.
c        note: only actually works when two dimentions have points set. Needs to
c        be expanded into 1, 3 or 4.
c
c        Revision 1.54  1997/05/30 18:04:40  harry
c        Fix mouse slicing to auto-cancel when plot is changed.
c
c        Revision 1.53  1997/04/30 18:41:58  harry
c        Change save_window to save_plot_window to avoid conflict with existing
c        routine.
c
c        Revision 1.52  1997/04/25 20:20:37  warren
c        Keep a stack of the last 10 windows used for plotting in order to implement
c        the window=up command.
c
c        Revision 1.51  1997/04/16 17:27:31  harry
c        When you set the field, change the 2d plot type to SCALAR.
c
c        Revision 1.50  1997/04/15 17:29:59  harry
c        Change trajectories and skewt to match vectors with attributes.
c
c        Revision 1.49  1997/04/15 17:24:39  harry
c        Remove time_tag - use multiple files instead.
c        Various fixes to vector/scalar looping.
c
c        Revision 1.48  1997/04/11 18:48:19  warren
c        Fixed for sgi and also to update color widget more frequently.
c
c        Revision 1.47  1997/04/09 17:32:42  warren
c        Fixed color freeing when cint changed problem. Widget now shows real state.
c
c        Revision 1.46  1997/03/14 22:26:32  warren
c        Added new command map_window to allow for mouse windowing on non
c        equadistant or default maps.
c
c        Revision 1.45  1997/02/21 21:17:08  warren
c        Final fixes for point/mouse slicing and expanding into 2d. It is tested
c        with the z dimention free and seems to work with others, but still should
c        be looked at with known data for this.
c
c        Revision 1.44  1997/02/20 22:03:57  harry
c        Minor fix in comment for PRINT_COMMAND.
c
c        Revision 1.43  1997/02/20 18:42:59  harry
c        Do NOT call save_plot_commands_ when the plot failed.
c
c        Revision 1.42  1997/02/12 17:35:51  warren
c        Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c        Revision 1.41  1997/02/07 18:39:08  harry
c        Remove PARTIAL_READ and expand MULTIPLE_READ.
c
c        Revision 1.40  1997/01/31 18:21:16  warren
c        Added print_command command for lp or lpr
c
c        Revision 1.39  1997/01/17 20:38:17  harry
c        First changes for MULTIPLE_READ and PARTIAL_READ.
c
c        Revision 1.38  1996/12/23 20:58:13  warren
c        Landscape printing option
c
c        Revision 1.37  1996/12/12 19:37:21  warren
c        First crack at line on plot. Known to work on contour and line plots.
c
c        Revision 1.36  1996/11/07 16:55:32  warren
c        Fixed to deal with the case of use_buttons == 0
c
c        Revision 1.35  1996/10/11 01:12:51  warren
c        To fill or not to fill is no longer the only question. Now there is a seperate
c        question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
c        Labels are not fixed yet.
c
c        Revision 1.34  1996/10/09 18:54:36  harry
c        Add the ability to loop on file names if time value is part of name,
c        and there is only one time per file.
c
c        Revision 1.33  1996/05/02 23:10:45  harry
c        Properly handle LINE plots again.
c
c        Revision 1.32  1996/04/25 16:25:08  harry
c        Add Skew-T plots and the beginning of trajectory plots.
c
c        Revision 1.31  1996/04/23 18:17:24  warren
c        Changes for terrain
c
c        Revision 1.30  1996/04/08 18:57:11  warren
c        first parts of num unlabeled lines for terrain and fixed transform routine
c        to use last directory if none is given.
c
c        Revision 1.29  1996/04/01 20:23:38  warren
c        Terrain form and new terrain command (on / off)
c
c        Revision 1.28  1996/01/12 16:11:58  warren
c        Transfer of new pattern stuff from 3.x and beginning of color fixes
c        (color vectors, correct collors on the pulldowan menus)
c
c Revision 1.27  1995/10/18  19:18:08  warren
c Stipples from 3.3, First crack at mouse windowing and put window_points_
c into a header file and out of the mouse stuff and slicers. Touched almost
c everything it seems.
c
c Revision 1.26  1995/09/15  16:48:21  harry
c Add support for timeout of warning messages.
c
c Revision 1.25  1995/09/05  20:33:43  harry
c Add new slicer.  Good luck!
c
c Revision 1.24  1995/08/22  22:03:30  warren
c do something when free is called
c
c Revision 1.23  1995/06/23  21:16:14  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.22  1995/05/26  15:21:16  harry
c Add SAVE_WINDOW command and widget.
c
c Revision 1.21  1995/05/11  22:40:18  harry
c Add new command WRITE_1D.
c
c Revision 1.20  1995/02/28  19:29:35  warren
c Fixed problem on alpha's where the popup type in boxes didn't move the
c sliders for the window.
c
c Revision 1.19  1995/01/30  15:26:24  warren
c Fixed crash problem where get_dim_atts was being called with no file.
c
c Revision 1.18  1994/11/22  22:47:48  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.17  1994/10/08  19:01:25  warren
c The annotation widget and updating it. As well as changing tic_bold to
c tic_labels and adding an off option.
c
c Revision 1.16  1994/09/20  18:41:57  warren
c added help for lockcolortable.
c
c Revision 1.15  1994/09/14  18:19:51  harry
c Big 1D flip uncommit.
c
c Revision 1.14  1994/09/07  22:07:06  warren
c removed the extra plot from the color table locking sequence.
c
c Revision 1.13  1994/06/28  17:43:28  warren
c First cut at fancy tic marks.
c
c Revision 1.12  1994/05/06  17:10:02  yuri
c Main vector lock commit + small correction in X/ive_ui_update.c file
c
c Revision 1.11  1994/04/19  23:02:50  yuri
c Big 1D flip commit.
c
c Revision 1.10  1994/04/14  23:29:15  harry
c Make sure vector locking can only be turned on after a vector plot.
c Also fix the user interface updating for the vector_lock command.
c
c Revision 1.9  1994/03/28  17:15:22  harry
c Add calls to update_all when a plot command is executed to make sure
c the plot window (and all the widgets) are updated.
c
c Revision 1.8  1994/03/08  19:59:06  warren
c print error message when trying to plot with not enought things locked.
c
c Revision 1.7  1994/02/16  17:00:26  yuri
c 	Just cleaning and getting rid of "interp_factors" stuff.
c
c Revision 1.6  1994/02/09  01:24:49  warren
c make sure data_display_units is saved and increase MKS in areas calls.
c
c Revision 1.5  1994/01/29  00:11:29  warren
c first cut at movie labels. Line plots still need work
c
c Revision 1.4  1994/01/20  20:44:31  yuri
c Propagating of "overlay_patterns" stuff to newer version.
c
c Revision 1.3  1993/12/30  00:50:10  warren
c Update screen before returning from driver, and make sure char ive_ptrs in
c ui_update are bigger than 0.
c
c Revision 1.2  1993/12/15  18:15:27  harry
c Add support for DEFAULT_MAP command, default_map transform.  Modify map
c clipping to work properly with the map background widget.
c
c Revision 1.1  1993/11/29  19:39:10  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.36  1993/10/18  23:07:25  warren
c updates from 3.0 version.
c
c Revision 1.35  1993/10/04  22:20:43  warren
c added number of unlabeled lines to attributes.
c
c Revision 1.34  1993/08/31  19:14:59  yuri
c 	3D->2D averaging feature added.
c
c Revision 1.33  1993/08/31  18:00:35  yuri
c New comand "VSLICE" (vertical slice from (x1,y1) to (x2,y2)) installed
c 	into version with NEW transforms.
c
c Revision 1.32  1993/08/18  15:47:15  harry
c Change to new transform routines.
c
c Revision 1.31  1993/07/23  20:36:54  yuri
c 	Command "vector_lock" is introduced to lock vector
c scaling in a loop (for example). Also some minor bugs are corrected.
c
c Revision 1.30  1993/07/20  22:44:42  warren
c Added ability to set prints and eps files to draw color lines. Added
c ability to choose printer. Added ability to specify which spot on the
c page to print at.
c
c Revision 1.29  1993/07/09  21:44:10  harry
c Move select_old call to before info_2d.
c
c Revision 1.28  1993/07/09  19:15:42  warren
c make the window change messgae always come up for new file or field if
c appropriate.
c
c Revision 1.27  1993/07/09  19:09:29  warren
c Pretty labels to go with the 2d ticmarks. Also I removed superfluous calls
c to chkwin and moved all the others to driver.f.
c
c Revision 1.26  1993/07/09  16:34:21  harry
c Changes to add number of contour values to attributes and properly
c handle windowing when changing data files.
c
c Revision 1.25  1993/07/07  23:02:08  warren
c Update the dependant var when changing fields in 1d. Also use g floating point.
c
c Revision 1.24  1993/07/06  17:36:16  warren
c Added resize command
c
c Revision 1.23  1993/07/02  23:05:18  harry
c Make more variables automatic due to recursive calls to driver.
c
c Revision 1.22  1993/06/28  21:26:26  harry
c Add "STEP" command and debug argument to "INPUT" command to allow
c single steps of input files.
c
c Revision 1.21  1993/06/25  23:48:32  warren
c removed spurious calls to ui_update.
c
c Revision 1.20  1993/06/25  23:11:15  warren
c Replaced chenges between 1.11 and 1.15 and cleaned up.
c
c Revision 1.19  1993/06/25  22:27:39  yuri
c vector colors are back.
c
c Revision 1.18  1993/06/25  20:55:28  warren
c Fixed crashing when reading a bad file problem.
c
c Revision 1.17  1993/06/25  20:07:26  warren
c Call ui_update for the map stuff when needed.
c
c Revision 1.16  1993/06/22  22:09:40  yuri
c 	New 1D and 2D slicers installed, added command interp_factors
c so user could choose number of interpolation points in slicing.
c Primarily it's for averaging, but user could use it every time
c before call slicers.
c Revision 1.15  1993/06/18  16:27:02  warren
c only call info_2d after file if there really was a file.
c
c Revision 1.14  1993/06/10  16:29:29  yuri
c Vector color stuff added.
c
c Revision 1.13  1993/06/05  00:24:06  warren
c fixed to make sure plwmin and plwmax are set when a file is opened.
c cleaned up more unnecessary ui_update calls
c
c Revision 1.12  1993/06/04  19:11:00  warren
c only call updae when really needed from x,y,zloc and time
c
c Revision 1.11  1993/06/04  18:55:37  warren
c removes suriuos call to ui_update in field.
c
c Revision 1.10  1993/06/03  00:31:53  warren
c Fixed New_plot and Overlay_plot to work directly to eleviate the double
c plot problem in the log files.
c
c Revision 1.9  1993/06/01  19:52:12  yuri
c Second attempt.
c
c Revision 1.8  1993/05/27  16:38:45  warren
c Recover files removed by yuri's commit
c
c Revision 1.5  1993/05/20  17:33:38  warren
c Additions and changes to make the new forms active.
c
c Revision 1.4  1993/05/04  18:13:25  warren
c added command contour_values
c
c Revision 1.3  1993/04/30  18:06:31  warren
c Made the slicer wiget appear after reading the data file.
c
c Revision 1.2  1993/04/26  19:19:09  warren
c See previous message
c
c Revision 1.1.1.1  1993/04/07  19:39:53  harry
c Initial CVS checkin of IVE.
c
c Revision 2.38  1993/02/11  20:39:33  warren
c reset pltflg at beginning of plot not end.
c
c Revision 2.37  1993/02/01  23:42:56  warren
c added calls to get andset dimention properyties
c
c Revision 2.36  1993/01/12  23:54:05  millerp
c Do not do catnull on disp_units; put null in first position instead.
c
c Revision 2.35  1993/01/07  16:57:10  millerp
c Fix argument list for readfld; must pass in variable name for vectors.
c
c Revision 2.34  1993/01/07  00:25:14  millerp
c Cat a null to strings sent to get_ and set_var_atts.
c
c Revision 2.33  1993/01/06  17:18:40  harry
c Change a segment from one each plot to one each overlay.
c Add LIFT_OVERLAY command.
c
c Revision 2.32  1993/01/05  20:15:11  warren
c fixed property save
c
c Revision 2.31  1993/01/04  17:10:13  warren
c added memory to readfld and plot.
c
c Revision 2.30  1992/12/31  18:48:28  millerp
c Change argument list to readfld
c
c Revision 2.29  1992/12/23  19:02:33  harry
c Add log files.
c
c Revision 2.28  1992/12/17  22:58:27  harry
c Add iflag to call to loop.
c
c Revision 2.27  1992/12/14  18:27:43  harry
c Change calls to dump and input.
c
c Revision 2.26  1992/12/07  23:54:45  millerp
c Do not set firstfile when changing data sets.
c ,
c
c Revision 2.25  1992/12/03  23:54:53  warren
c patched patch to bring up slicer widget
c
c Revision 2.24  1992/12/03  20:57:59  millerp
c Sef firstfile when new file is opened.
c
c Revision 2.23  1992/12/03  00:02:00  millerp
c Call driver with field=att_var when vector components are specified.
c
c Revision 2.22  1992/11/23  16:41:23  millerp
c Added stuff for surface plots; Add DUMP command handling (harry).
c
c Revision 2.20  1992/11/18  18:11:07  harry
c Change call from input_file to input.
c
c Revision 2.19  1992/11/16  22:40:17  harry
c Add INPUT command.
c
c Revision 2.18  1992/11/10  16:21:53  harry
c Use parameter names for command numbers.
c
c Revision 2.17  1992/11/07  00:52:30  warren
c fixed HORIZONTAL_SCALE to be 15 instead of 16 to match cmndlst.icl
c
c Revision 2.16  1992/10/28  22:21:38  millerp
c Removed AVERAGE command.
c
c Revision 2.15  1992/10/21  21:33:12  warren
c added write_file
c
c Revision 2.14  1992/10/15  23:05:04  warren
c added lock color table
c
c Revision 2.12  1992/10/06  18:26:45  warren
c added xtics_line and ytics_line and plot types scaler and vector
c
c Revision 2.11  1992/09/30  15:24:20  warren
c commented out avg
c
c Revision 2.10  1992/09/29  18:47:15  harry
c Add LOOP_QUIT.
c
c Revision 2.9  1992/09/23  23:11:26  harry
c Add calls to save_command and save_plot_command.
c Correct bug in logic for LINEPATTERN.
c Removed SOUNDING and VINDEX.
c
c Revision 2.8  1992/09/22  17:49:37  warren
c add slice command
c
c Revision 2.7  1992/09/17  15:42:49  warren
c changed the slicing syntax to use xloc, yloc, zloc and time instead of
c hloc and vloc. this include changes to the common blocks and command set.
c
c Revision 2.6  1992/09/08  19:56:01  millerp
c Added stuff for new slicers.
c
c Revision 2.5  1992/08/26  19:33:52  harry
c Move plot processing to be within the command loop.
c
c Revision 2.4  1992/08/24  23:05:22  warren
c fixed time=bug
c
c Revision 2.3  1992/08/24  20:38:46  millerp
c Replace INQUIRE with IS_THERE.
c
c Revision 2.2  1992/08/11  16:14:41  harry
c Change in call to cmdpar to properly handle recursion.
c Change in call to inscmd to insert a command at a time instead
c of the whole command line.  Do not add list or loop calls
c to segment lists.  Must use new version of cmdpar.f with this change.
c
c Revision 2.1  1992/07/06  21:46:05  millerp
c Added call to select_old.
c
c Revision 2.0  1992/07/01  21:12:51  millerp
c Released version rereving to 2.0
c
c Revision 1.13  1992/06/19  17:52:41  millerp
c Deleted call to chkvct.
c
c Revision 1.12  1992/06/15  19:02:02  warren
c added ui_update call to average.,
c
c Revision 1.11  1992/06/15  18:56:08  millerp
c Added AVERAGE command.
c
c Revision 1.10  1992/06/11  17:23:36  millerp
c Added error checks before calling ui_update.
c
c Revision 1.9  1992/06/10  23:18:52  harry
c Call ui_update after settime.
c
c Revision 1.8  1992/06/10  23:07:56  warren
c added update for vloc and hloc
c
c Revision 1.7  1992/06/10  21:17:07  warren
c add update for loc
c
c Revision 1.6  1992/06/10  17:37:45  millerp
c *** empty log message ***
c
c Revision 1.5  1992/05/29  22:32:09  millerp
c Added units and time commands.
c
c Revision 1.4  1992/05/14  21:30:54  harry
c Added FREE and LOOP commands.
c
c Revision 1.3  1992/05/06  23:16:17  warren
c new map form finished.
c
c Revision 1.2  1992/05/05  15:23:39  millerp
c Added SOUNDING command.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
      include 'attributes.icl'
      include 'cmndlst.icl'
      include 'color.icl'
      include 'conre.icl'
      include 'data.icl'
      include 'deflts.icl'
      include 'flags.icl'
      include 'incommands.icl'
      include 'ive_widgets.icl'
      include 'message.icl'
      include 'ph2ll.icl'
      include 'plotops.icl'
      include 'segments.icl'
      include 'skewt.icl'
      include 'supma.icl'
      include 'traj.icl'
      include 'vector.icl'
c
c     Data block for segments
c
      data curseg, curplot/0, 0/
c
c
c     Argument declarations
c
      character *(*) comlin
      integer   iflag
c
c
c     Local variable declarations
c
c      automatic beggrp, keynum, error, last_beggrp
      integer    keynum, beggrp, last_beggrp,
     &           ibeg, iend, i, lock_count, ave_count,
     &           keepit, free_count
      ive_ptr    varpt1
      logical    error, firstfile
      logical safe_to_lock
      integer att_var_id
      save att_var_id
      character  disp_units*80, plotyp*16
c
c
      data firstfile / .true. / 
      data keepit / 0 / 
      data safe_to_lock / .false. /
c
c
c     External function declarations.
c
      integer  strbeg, strend, getvid, check_points_line,
     &     chkcmd
c
      save    firstfile, varpt1, safe_to_lock
c
c
c
c     Set some initial values.
c
      call write_log(comlin(strbeg(comlin):strend(comlin)))
      beggrp = 1
c
c     Freeze cursor when graphics is busy.
c
      call freeze_dpy
c
c
 5    continue
      if (beggrp.ne.0) then
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c set ph2ll_flag to 0, indicates no use of ph2ll generally
         ph2ll_flag=0
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c 
c        Parse command line.
c
         last_beggrp = beggrp
         call cmdpar (comlin, beggrp)
c
c        Process each command=value group.
c        Set initial error flag to false (no errors).
c
         error = .false.
c
c        Check for errors in the command. Also return a value (keynum) 
c        indicating the command position in the valid command list.
c
         i = chkcmd (comand(1), keynum, error)
         if ( error ) go to 10
c
c        Call the appropriate routine for each command based on the 
c        value of keynum.
c
         if (    keynum .eq.  P_ABSCISSA .or. 
     &        keynum .eq. P_HORIZONTAL_SCALE) then
c           Command ABSCISSA
            call setabscissa (error)
            if ( (iflag .eq. 0) .and. .not.error ) then 
               call ui_update (ABSCISSA)
            endif
         elseif (keynum .eq.  P_ASPECT_RATIO ) then
c           Command ASPECT_RATIO
            call setaratio (error)
         elseif (keynum .eq.  P_AXES) then
c           Command AXES.
            call setax (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (AXES_UI)
         elseif (keynum .eq.  P_BIG_LABELS) then
c           COMMAND BIG_LABELS
            call setbl(error)
         elseif (keynum .eq.  P_COLOR_BAR) then
            call set_color_bar (error)
         elseif (keynum .eq.  P_CONTOUR_INTERVAL) then
c           Command CONTOUR_INTERVAL.
            call setint (error, safe_to_lock)
            if((iflag .eq. 0) .and. .not.error) call ui_update (CINT_UI)
         elseif (keynum .eq.  P_CONTOUR_VALUES) then
c           Command CONTOUR_VALUES.
            call setvals (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (CINT_UI)
         elseif (keynum .eq.  P_DATA_UNITS) then
c           Command DATA_UNITS.
            call set_units (error)
         elseif (keynum .eq.  P_DEFAULT_MAP) then
c           Command DEFAULT_MAP.
            call set_defmap(error)
            if ((iflag .eq.0) .and. .not.error) then
               call ui_update(DEFAULT_MAP_UI)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq.  P_DOMAIN_UNITS) then
c           Command DOMAIN_UNITS.
            call set_domain_units (error)
            if(use_buttons .eq. 1) then
               call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
            else
               call init_points()
               call do_props (1)
            endif
            call ui_update (WINDOW_UI)
         elseif (keynum .eq.  P_DRAW_LINE) then ! Command DRAW_LINE
            call draw_line (error)
         elseif (keynum .eq.  P_DRAW_LINE_MAP) then ! Command DRAW_LINE_MAP
            call draw_line_map (error)
         elseif (keynum .eq.  P_DRAWMAP) then ! Command DRAWMAP
            call setmap (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (DRMAP_UI)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq.  P_DRAW_POINT) then ! Command DRAW_POINT
            call draw_point (error)
         elseif (keynum .eq.  P_DRAW_POINT_MAP) then ! Command DRAW_POINT_MAP
            call draw_point_map (error)
         elseif (keynum .eq. P_DUMP .or. keynum .eq. P_LOOP_DUMP) then
            call dump(keynum, iflag)
         elseif (keynum .eq.  P_EXIT) then
c           Command EXIT.
            call endprg
         elseif (keynum .eq. P_EPS_COLOR) then
c           Command EPS_COLOR
            call set_fi_color(error)
         elseif (keynum .eq.  P_FIELD) then
c           Command FIELD.
            call setfld (error)
c           Read the data for the requested field.
            call readfld (field, varpt1, error)
            if ( .not. error ) then
c
c	set plot type 2 to scalar
c
               call setavar('plotyp2', 'SCALAR', error)
               call ui_update(PLOTTYPE)
               if (cpmax1.eq.1) xloc = plmin(1)
               if (cpmax2.eq.1) yloc = plmin(2)
               if (cpmax3.eq.1) zloc = plmin(3)
               if (cpmax4.eq.1) tloc = plmin(4)
               call setlvar('read_from_nc', .false., error)
c     
c              Re-set the window the first time a data field is 
c              specified for a new data set.
c     
               if ( firstfile ) then
                  call driver ('window', 1)
                  firstfile = .false.
               endif
               call chkwin(.FALSE.)
               att_var_id = getvid(field(strbeg(field):strend(field)))
               call get_var_atts(att_var_id, lin, cont_int, cont_values, 
     &              no_zero, disp_units, major_tic,
     &              minor_tic, lmin, lmax, num_vals,
     &              nulbll, cllab)
               call setavar('data_display_units', disp_units, error)
               call ui_update (CINT_UI)
               call ui_update (NOZERO_UI)
            if(use_buttons .eq. 1) then
               call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
            else
               call init_points()
               call do_props (1)
            endif
               call ui_update(LABELLINE_UI)
               call do_props (1)
               safe_to_lock = .false.
            else
               field = ' '
               field_pointer = ' '
            endif
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (BUTTONS_UI)
            endif
         elseif (keynum .eq. P_FILE) then
c           Command FILE.
            call setdfil (error)
            if ( .not. error ) then
c
c              Open the specified data file.
c
               call opendat (error,.false.)
               if ( .not. error ) then
                  call select_old (field)
            if(use_buttons .eq. 1) then
               call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
            else
               call init_points()
               call do_props (1)
            endif
                  call chkwin(.FALSE.)
c                 call ui_update (WINDOW_UI)
c	            safe_to_lock = .false.
               endif
            endif
            if((iflag .eq. 0) .and. .not.error) call ui_update (DIR_UI)
         elseif (keynum .eq. P_FILLPATTERN) then
c     Command FILLPATTERN.
            call setfill_ptn (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (FILLPATTERN)
            endif
         elseif (keynum .eq. P_FILLTYPE) then
c           Command FILLTYPE.
            call setftp (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (FILLTYPE_UI)
            endif
         elseif (keynum .eq. P_FILL_TYPE) then
c           Command FILL_TYPE.
            call set_ftp (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (FILLTYPE_UI)
            endif
         elseif (keynum .eq. P_FILL_CONTOURS) then
c           Command FILL_CONTOURS.
            call set_fct (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (FILLTYPE_UI)
            endif
         elseif (keynum .eq. P_FRAMEWIDTH) then
c           Command FRAMEWIDTH. 
            call setfw (error)
         elseif (keynum .eq. P_FREE) then
c           Command FREE
            call freefld(error)
         elseif (keynum .eq. P_GRAYLINES .or. 
     &           keynum .eq. P_GREYLINES) then
            call graylines()
         elseif (keynum .eq. P_GRIDSPACING) then
c           Command GRIDSPACING.
            call setgrd (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (GRID_UI)
         elseif (keynum .eq. P_HELP) then
c           Command HELP. 
            call help
         elseif (keynum .eq. P_INPUT) then
c           Command INPUT.
            call comment_log(1)
            call input(iflag)
            call comment_log(-1)
         elseif (keynum .eq. P_LABEL) then
c           Command LABEL.
            call setlab (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (LABELPLOT_UI)
            endif
         elseif (keynum .eq. P_LANDSCAPE) then
c           Command LANDSCAPE
            call set_landscape(error)
            if((iflag .eq. 0) .and.  .not.error) then
               call ui_update (LANDSCAPE_UI)
            endif
         elseif (keynum .eq. P_LIFT_OVERLAY) then
c           Command LIFT_OVERLAY
            call delseg (curseg)
            call set_up_old_lim
            call update_all
         elseif (keynum .eq. P_LINECOLOR) then
c           Command LINECOLOR. 
            call setclclr (error)
         elseif (keynum .eq. P_OVERLAY_CONTOURCOLORS) then
c           Command 
            call set_overlay_colors (error)
         elseif (keynum .eq. P_OVERLAY_LINECOLORS) then
c           Command 
            call set_overlay_linecolors (error)
         elseif (keynum .eq. P_OVERLAY_CONTOURPATTERNS) then
c           Command 
             call set_overlay_patterns (error)
         elseif (keynum .eq. P_OVERLAY_LINEPATTERNS) then
c           Command 
             call set_overlay_linepatterns (error)
         elseif (keynum .eq. P_LINELABELS) then
c           Command LINELABELS. 
            call setcll (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (LABELLINE_UI)
            endif
         elseif (keynum .eq. P_LINEPATTERN) then
c           Command LINEPATTERN. 
            call setltp (error)
         elseif (keynum .eq. P_LINEWIDTH) then
c           Command LINEWIDTH. 
            call setlw (error)
         elseif (keynum .eq. P_LISTSEGMENTS) then
c           Command LIST_SEGMENTS.
            call print_list 
            error = .true.
         elseif (keynum .eq. P_LOCKCOLORTABLE) then
c           Command LOCKCOLORTABLE.
            if (safe_to_lock) then
               call lock_color (error)
c               if((iflag .eq. 0) .and. .not.error) then
c                  call ui_update (COLORTAB_UI)
c               endif
            else
               write(message,120)
               call write_message
            endif
         elseif (keynum .eq. P_LOGFILE) then
            call open_log(iflag)
         elseif (keynum .eq. P_LOOP) then
c           Command LOOP
            call comment_log(1)
            call loop(iflag, error)
            call comment_log(-1)
            error = .true.
         elseif (keynum .eq. P_LOOP_QUIT) then
c           Command LOOP_QUIT
            call loop_quit
            error = .true.
         elseif (keynum .eq. P_MAKE_UNITS) then
c           Command MAKE_UNITS
            call force_set_units (error)
         elseif (keynum .eq. P_MAPLIMITS) then
c           Command MAPLIMITS.
            call setmpl (error)
            if((iflag .eq. 0) .and. .not.error) then 
               call ui_update (MAPCLIP_UI)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_MAPOUTLINES) then
c           Command MAPOUTLINES.
            call setotl (error)
            if((iflag .eq. 0) .and. .not.error) then 
               call ui_update (MAPOUT_UI)
            endif
         elseif (keynum .eq. P_MAPROJ) then
c           Command MAPROJ.
            call setprj (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (MAPROJ_UI)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_MAP_WINDOW) then
c           Command MAP_WINDOW.
            call map_win (error)
         elseif (keynum .eq.  P_MOVIE_LABELS) then
c           COMMAND MOVIE_LABELS
            call setml(error)	
         elseif (keynum .eq. P_MULTIPLE_READ) then
c           Command MULTIPLE_READ.
            call set_multiple_read(error)
         elseif (keynum .eq. P_NEW_PLOT) then
c           Command NEW_PLOT.
            savflg=.false.
            call setplt (error)
            call reset_traj_colors(error)
         elseif (keynum .eq. P_MAX_PLOT) then
c           Command MAX_PLOT.
            call set_max_plot ()
         elseif (keynum .eq. P_NOZERO) then
c           Command NOZERO.
            call setnoz (error)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (NOZERO_UI)
            endif
         elseif (keynum .eq. P_NULBLL) then
c           Command NULBLL.
            call setnul (error)
            if((iflag .eq. 0) .and. .not.error) then 
               call ui_update (NUNLAB_UI)
            endif
         elseif (keynum .eq. P_ORDINATE .or.
     &           keynum .eq. P_VERTICAL_SCALE) then
c           Command ORDINATE.
            call setordinate (error)
            if ( (iflag .eq. 0) .and. .not.error ) then
               call ui_update (ORDINATE)
            endif
         elseif (keynum .eq. P_OVERLAY_PLOT) then
c           Command OVERLAY_PLOT.
            savflg=.true.
            call setplt (error)
         elseif (keynum .eq. P_PLAT) then
c           Command PLAT.
            call setpla (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (LAT_UI)
         elseif (keynum .eq. P_PLON) then
c           Command PLON.
            call setplo (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (LON_UI)
         elseif (keynum .eq. P_PLOT) then
c           Command PLOT.
            call setplt (error)
         elseif  (keynum .eq. P_POINT_SETUP) then
c           Command Point_Setup
            call point_setup (error)
         elseif  (keynum .eq. P_POINT_AXIS) then
c           Command Point_Axis
            call point_axis (error)
         elseif (keynum .eq. P_PRINT) then
c           Command PRINT.
            call setprint 
         elseif (keynum .eq. P_PRINT_COLOR) then
c           Command PRINT_COLOR 
            call set_pr_color(error)
         elseif (keynum .eq. P_PRINT_COMMAND) then
c           Command PRINT_COMMAND 
            call set_pr_com(error)
         elseif (keynum .eq. P_PRINTER_NAME) then
c           Command PRINTER_NAME 
            call set_pr_name(error)
         elseif (keynum .eq. P_QUIT) then
c           Command QUIT.
            call endprg
         elseif (keynum .eq. P_READCOLORTABLE) then
c           Command READCOLORTABLE.
            call rdctab (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (CTB_UI)
         elseif (keynum .eq. P_READTCOLORTABLE) then
c           Command READTCOLORTABLE.
            call rdtctab (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (CTB_UI)
         elseif (keynum .eq. P_REDRAW) then
c           Command REDRAW.
            call redraw
         elseif (keynum .eq. P_RELOAD_FILE) then
c           Command RELOAD_FILE.
c
c              Open the specified data file.
c
            call opendat (error,.true.)
            if ( .not. error ) then
               call select_old (field)
               if(use_buttons .eq. 1) then
                  call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
               else
                  call init_points()
                  call do_props (1)
               endif
               call chkwin(.FALSE.)
            endif
            if((iflag .eq. 0) .and. .not.error) call ui_update (DIR_UI)
         elseif (keynum .eq. P_RESIZE) then
c           Command resize
            call resize_gks
         elseif (keynum .eq. P_ROTA) then
c           Command ROTA.
            call setrot (error)
            if((iflag .eq. 0) .and. .not.error) call ui_update (ROT_UI)
         elseif (keynum .eq. P_ROTATE3D) then
c           Command ROTATE3D.
            call rot3d (error)
         elseif (keynum .eq. P_RUN_SUB) then
c           Command RUN_SUB.
            call run_sub_trans (error)
            if((iflag .eq. 0) .and. .not.error) 
     &           call ui_update (WINDOW_UI)
         elseif (keynum .eq. P_SAVE) then
c           Command SAVE.
            call setsav (error) 
         elseif (keynum .eq. P_SAVE_WINDOW) then
c           Command SAVE_WINDOW
            call save_window
         elseif (keynum .eq. P_SCALE_VECTOR) then
c           Command SCALE_VECTOR
            call set_scale_vector (error)
            if ((iflag .eq. 0) .and. .not.error) then
               call ui_update(VSCALE_UI)
            endif
         elseif  (keynum .eq. P_SINGLEVALUE) then
c           Command SINGLEVALUE.
            call setsvl (error)
            if((iflag .eq. 0) .and. .not.error) then 
               call ui_update (SINGLEVAL_UI)
            endif
         elseif (keynum .eq. P_STEP) then
c           Command STEP.
            call comment_log(1)
            call step(1)
            call comment_log(-1)
         elseif (keynum .eq. P_STREAMLINE) then
c           Command STREAMLINE.
            call setstrmln (error)
            if( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (STREAMLINE)
            endif
         elseif (keynum .eq.  P_SURFACE_VALUE) then
c           Command SURFACE_VALUE.
            call setsurf (error)
c            if((iflag .eq. 0) .and. .not.error) call ui_update (SINT_UI)
         elseif	(keynum .eq. P_SWITCH_SLICERS) then
            call switch_slicer (error)
            call do_props (1)
         elseif (keynum .eq. P_TCONTOUR_INTERVAL) then
c           Command TCONTOUR_INTERVAL.
            call settci (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (TCINT_UI)
            endif
         elseif (keynum .eq. P_TCONTOUR_VALUES) then
c           Command TCONTOUR_VALUES.
            call settcv (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (TCINT_UI)
            endif
         elseif (keynum .eq. P_TERRAIN) then
c           Command TERRAIN.
            call setter (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (TERR_UI)
            endif
         elseif (keynum .eq. P_TERR_NULBLL) then
c           Command TERR_NULBLL.
            call setternul (error)
            if((iflag .eq. 0) .and. .not.error) then 
               call ui_update (TERR_NUNLAB_UI)
            endif
         elseif (keynum .eq. P_TIC_SCALE) then
c           Command TIC_SCALE
            call tic_scale(error)
            if( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (TICSCALE)
            endif
         elseif (keynum .eq. P_TIC_WIDTH) then
c           Command TIC_WIDTH
            call tic_width(error)
            if( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (TICWIDTH)
            endif
         elseif (keynum .eq. P_TIC_LABELS) then
c           Command TIC_LABELS
            call tic_labels(error)
            if( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (TICLABELS)
            endif
         elseif (keynum .eq. P_TIC_DECIMALS) then
c           Command TIC_DECIMALS
            call tic_decimals(error)
            if( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (TICDECIMALS)
            endif
         elseif (keynum .eq. P_TIC_OFFSET) then
c           Command TIC_OFFSET
            call tic_offset(error)
            if( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (TICOFFSET)
            endif
         elseif (keynum .eq. P_TIME) then
c           Command TIME.
            keepit = lock(4)
            call settime (error)
            if (keepit .ne. lock(4)) call get_dim_atts()
            if ( (iflag .eq. 0) .and. .not.error ) call ui_update (LOC)
            call do_props (0)
            if(.not. error) then
               if (keepit .ne. lock(4)) call get_dim_atts()
               if(use_buttons .eq. 1) then
                  if(iflag .eq. 0) then
                     call ui_update (LOC)
                  endif
               else
                  call ui_update (POINTS_UI)
               endif
               call do_props (0)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_TPOINTS) then
c           Command TPOINTS.
            call settpoint (error)
            if(.not.error) then
               call ui_update (POINTS_UI)
            endif
            call get_dim_atts()
            call do_props (1)
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_TOPO_SCALE) then
c           Command TOPO_SCALE.
            call set_topo_scale (error)
            if ( (iflag .eq. 0) .and. .not.error )  then 
               call ui_update (TOPOSCALE_UI)
            endif
         elseif (keynum .eq. P_TRANSLATE3D) then
c           Command TRANSLATE3D.
            call trans3d (error)
         elseif (keynum .eq. P_TRANSFORM_FILE) then
c           Command TRANSFORM_FILE.
            call settrfil (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (TRANS_UI)
            endif
         elseif (keynum .eq. P_TYPE) then
c           Command TYPE.
            call settyp (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (PLOTTYPE)
            endif
            call do_props (0)
         elseif (keynum .eq. P_SKEWT_COMPONENTS) then
c           Command SKEWT_COMPONENTS.
            call setskewt (error) 
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (SKEWT_COMP_UI)
            endif
            if (.not. error) then
c     
c     Re-set the window the first time a data field is 
c     specified for a new data set.
c     
               if ( firstfile ) then
                  call driver ('window', 1)
                  firstfile = .false.
               endif
               call chkwin(iflag)
            if(use_buttons .eq. 1) then
               call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
            else
               call init_points()
               call do_props (1)
            endif
               call ui_update (WINDOW_UI)
C	       att_var_id = getvid(att_var(strbeg(att_var_skewt):
C     &              strend(att_var)))
C                call get_var_atts(att_var_id, lin, cont_int,
C     &              cont_values, no_zero, disp_units, major_tic,
C     &              minor_tic, lmin, lmax, num_vals,
C     &              nulbll, cllab)
C               call setavar('data_display_units', disp_units, error)
            end if
         elseif (keynum .eq. P_TRAJ_2D) then
            call set_traj_2d (error)
            if (.not. error) then
               call ui_update(TRAJ_2D_UI)
            endif
         elseif (keynum .eq. P_TRAJ_IND .or.
     &           keynum .eq. P_TRAJECTORY_IND) then
            call set_traj_time_ind (error)
            if (.not. error) then
               call ui_update(TRAJ_IND_UI)
            endif
         elseif (keynum .eq. P_TRAJ_VALUE .or.
     &           keynum .eq. P_TRAJECTORY_VALUE) then
            call set_traj_val (error)
            if (.not. error) then
               call ui_update(TRAJ_STEP_VAL_UI)
            endif
         elseif (keynum .eq. P_TRAJ_BACKGROUND .or.
     &           keynum .eq. P_TRAJECTORY_BACKGROUND) then
            call set_traj_background (error)
            if (.not. error) then
               call ui_update(TRAJ_BACKGROUND_UI)
            endif
         elseif (keynum .eq. P_TRAJ_STEP .or. 
     &           keynum .eq. P_TRAJ_INTERVAL .or.
     &           keynum .eq. P_TRAJECTORY_STEP .or. 
     &           keynum .eq. P_TRAJECTORY_INTERVAL) then
            call set_traj_step (error)
            if (.not. error) then
               call ui_update(TRAJ_STEP_VAL_UI)
            endif
         elseif (keynum .eq. P_TRAJECTORY_COMPONENTS .or.
     &           keynum .eq. P_TRAJ_COMPONENTS ) then
c     Command TRAJECTORY_COMPONENTS.
            call settraj (error) 
            if (.not. error) then
c     
c     Re-set the window the first time a data field is 
c     specified for a new data set.
c     
              if ( firstfile ) then
                call driver ('window', 1)
                firstfile = .false.
              endif
              call chkwin(iflag)
              if(use_buttons .eq. 1) then
                call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
              else
                call init_points()
                call do_props (1)
              endif
              call ui_update (WINDOW_UI)
              if ( (iflag .eq. 0) .and. .not.error )  then 
                call  ui_update (UI_TRAJ_COMP)
              endif
              att_var_id = getvid(att_var(strbeg(att_var_traj):
     &                            strend(att_var)))
              call get_var_atts(att_var_id, lin, cont_int,
     &             cont_values, no_zero, disp_units, major_tic,
     &             minor_tic, lmin, lmax, num_vals,
     &             nulbll, cllab)
              call setavar('data_display_units', disp_units, error)
           end if
         elseif (keynum .eq. P_START_TRAJ) then !Command start_traj
           call start_traj (error)
c          if(.not.error)  then 
c            call ui_update (START_TRAJ_UI)
c          endif
         elseif (keynum .eq. P_START_TRAJ_MAP) then !Command start_traj_map
           call start_traj_map (error)
         elseif (keynum .eq. P_INT_TRAJ) then ! Command inttraj
            call int_traj (error)
            !if(.not.error)  then 
c               call ui_update (UI_TRAJ_ENDS)
            !endif
         elseif (keynum .eq. P_VECTOR_COMPONENTS) then
c           Command VECTOR_COMPONENTS.
            call setvector (error)
            if (.not. error) then
c     
c                   Re-set the window the first time a data field is 
c                    specified for a new data set.
c     
               if ( firstfile ) then
                  call driver ('window', 1)
                  firstfile = .false.
               endif
               call chkwin(iflag)
            if(use_buttons .eq. 1) then
               call info_2d (plmin_scaled, plmax_scaled, MAXDIM)
            else
               call init_points()
               call do_props (1)
            endif
               call ui_update (WINDOW_UI)
               if ( (iflag .eq. 0) .and. .not.error )  then 
                  call  ui_update (UI_VECT_COMP)
               endif
               att_var_id = getvid(att_var(strbeg(att_var):
     &              strend(att_var)))
                call get_var_atts(att_var_id, lin, cont_int,
     &              cont_values, no_zero, disp_units, major_tic,
     &              minor_tic, lmin, lmax, num_vals,
     &              nulbll, cllab)
               call setavar('data_display_units', disp_units, error)
            endif
         elseif (keynum .eq. P_VECTOR_INTERVAL) then
c        Command VECTOR_INTERVAL.
            call setvin (error)
            if ( (iflag .eq. 0) .and. .not.error ) call ui_update (VINT)
         elseif (keynum .eq. P_VECTOR_MAX) then
c           Command VECTOR_MAX
            call setvmax (error)
            if ( .not. error ) then
               if (iflag .eq. 0) then
                  call ui_update (UI_VMAX)
               endif
               call ui_update (VLOCK_UI)
               call update_all
            endif
         elseif (keynum .eq. P_VECTOR_LOCK) then
c           Command VECTOR_LOCK
            call   setveclock(error)
            if ((iflag .eq. 0) .and. .not.error) then
               call ui_update(VLOCK_UI)
            endif
            call update_all
         elseif (keynum .eq. P_VECTOR_POSITION) then
            call setvec_pos(error)
            if ((iflag .eq. 0) .and. .not.error) then
               call ui_update(VPOS)
            endif
            call update_all
         elseif (keynum .eq. P_VLABEL) then
c           Command VLABEL.
            call setvlab (error)
         elseif  (keynum .eq. P_VSLICE) then
c           Command VSLICE.
            call set_points(error)
            call ui_update (LOC)
         elseif	(keynum .eq. P_WARN_TIMEOUT) then
c           Command WARN_TIMEOUT
            call set_warn(error)
         elseif (keynum .eq. P_WINDOW) then
c           Command WINDOW.
            call setwin (error)
            if (.not. error)call chkwin(iflag)
            if((iflag .eq. 0) .and. .not.error) then
               call ui_update (WINDOW_UI)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_WRITECOLORTABLE) then
c           Command WRITECOLORTABLE.
            call wtctab (error)
         elseif (keynum .eq. P_WRITE_FRAME) then
c           Command WRITE_FRAME.
            call write_frame (error)
         elseif (keynum .eq. P_WRITE_PDF) then
c           Command WRITE_FRAME.
            call write_pdf (error)
         elseif (keynum .eq. P_WRITE_1D) then
c	    Command WRITE_1D.
            call write_1d(error)
         elseif (keynum .eq. P_XLOCATION) then
c           Command XLOCATION.
            keepit = lock(1)
            call setxloc (error)
            if (keepit .ne. lock(1)) call get_dim_atts()
            if(use_buttons .eq. 1) then
               if(iflag .eq. 0) then 
                  call ui_update (LOC)
               endif
            else
               call ui_update (POINTS_UI)
            endif
            call do_props (0)
            if(.not. error) then
               if (keepit .ne. lock(1)) call get_dim_atts()
               if(iflag .eq. 0) call ui_update (LOC)
               call do_props (0)
            endif
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_XPOINTS) then
c           Command XPOINTS.
            call setxpoint (error)
            if(.not.error) then
               call ui_update (POINTS_UI)
            endif
            call get_dim_atts()
            call do_props (1)
            mapwin_u1=0.0 ; mapwin_u2=0.0 ; mapwin_v1=0.0 ; mapwin_v2=0.0
         elseif (keynum .eq. P_XTICS) then
c           Command XTICS
            call setxti (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (XTICS_UI)
            endif
         elseif (keynum .eq. P_YLOCATION) then
c           Command YLOCATION.
            keepit = lock(2)
            call setyloc (error)
            if (keepit .ne. lock(2))call get_dim_atts()
            call do_props (0)
            if(.not. error) then
               if (keepit .ne. lock(2))call get_dim_atts()
               if(use_buttons .eq. 1) then
                  if(iflag .eq. 0) then
                     call ui_update (LOC)
                  endif
               else
                  call ui_update (POINTS_UI)
               endif
               call do_props (0)
            endif
         elseif (keynum .eq. P_YPOINTS) then
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
c           Command YPOINTS.
            call setypoint (error)
            if(.not.error) then
               call ui_update (POINTS_UI)
            endif
            call get_dim_atts()
            call do_props (1)
         elseif (keynum .eq. P_YTICS) then
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
c           Command YTICS
            call setyti (error)
            if((iflag .eq. 0) .and. .not.error)  then 
               call ui_update (YTICS_UI)
               if(iflag .eq. 0) call ui_update (LOC)
            endif
         elseif (keynum .eq. P_ZLOCATION) then
c           Command ZLOCATION.
            keepit = lock(3)
            call setzloc (error)
            if (keepit .ne. lock(3)) call get_dim_atts()
            call do_props (0)
            if(.not. error) then
               if (keepit .ne. lock(3)) call get_dim_atts()
               if(use_buttons .eq. 1) then
                  if(iflag .eq. 0) then 
                     call ui_update (LOC)
                  endif
               else
                  call ui_update (POINTS_UI)
               endif
               call do_props (0)
            endif
         elseif (keynum .eq. P_ZPOINTS) then
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
c           Command ZPOINTS.
            call setzpoint (error)
            if(.not.error) then
               call ui_update (POINTS_UI)
            endif
            call get_dim_atts()
            call do_props (1)
            mapwin_u1=0.0
            mapwin_u2=0.0
            mapwin_v1=0.0
            mapwin_v2=0.0
         elseif (keynum .eq. P_ZOOM3D) then
c           Command XTICS
            call zoom3d (error)
         endif
c
c
         if ( .not. error) then
c
c           There was not an error in the command given, so enter 
c           the command into the segment command list.
c
            if (beggrp .eq. 0) then
               call save_command(keynum,
     &              comlin(last_beggrp:strend(comlin)))
               call inscmd(comlin(last_beggrp: strend(comlin)))
            else
               call save_command(keynum,
     &              comlin(last_beggrp:beggrp-2))
               call inscmd (comlin(last_beggrp:beggrp-2))
            endif
c
c           DRAW_LINE, DRAW_LINE_MAP, DRAW_POINT, and DRAW_POINT_MAP
c           are special cases - they are self-contained overlays!
c
            if (     keynum .eq. P_DRAW_LINE 
     &          .or. keynum .eq. P_DRAW_POINT 
     &          .or. keynum .eq. P_DRAW_LINE_MAP 
     &          .or. keynum .eq. P_DRAW_POINT_MAP) then
               call save_plot_command(curoverlay(curlist)-1, curlist-1)
               call save_command(keynum, '')
            endif
         endif
c
         if (pltflg) then
            call cancel_mouse_slicing
            pltflg = .false.
            lock_count = 0
            ave_count = 0
            free_count = 0
            do i = 1, 4
               if (lock(i) .eq. 0) then
                  free_count = free_count + 1
               elseif (lock(i) .eq. 1) then
                  lock_count = lock_count + 1 
               elseif (lock(i) .eq. 2) then
                  ave_count = ave_count + 1
               endif
            enddo
            if (use_buttons .ne. 0) then
               if (free_count .eq. 0) then
                  plotyp = 'POINT'
               elseif (free_count .eq. 1) then
                  plotyp = plotyp1
               elseif(free_count .eq. 2) then
                  plotyp = plotyp2
               elseif(free_count .eq. 3) then
                  plotyp = plotyp3
               endif
            else
               i = check_points_line ()
c               call get_dim_atts()
               if (i .eq. 1) then
                  plotyp = plotyp1
               elseif (i .eq. 2) then
                  plotyp = plotyp2
               elseif (i .eq. 3) then
                  plotyp = plotyp3
               elseif (i .eq. 0) then
                  plotyp = 'POINT'
               else
                  write (message, 100)
                  call write_message
                  return
               endif
            endif
c
c           Make sure required parameters have been set.
c
            call chkplt (datfil, transform_path, field, plotyp, plwmin, 
     &           plwmax, MAXDIM, error)
c
c
            if ( .not. error ) then
               ibeg = strbeg(plotyp)
               iend = strend(plotyp)
               if (ave_count .ge. 3) then
                  error = .true.
                  write (message, 140)
                  call write_message
               elseif ( plotyp(ibeg:iend) .eq. 'VECTOR' ) then
                  if ( .not. error ) then
                     call mak2dv 
                  endif
               elseif ( plotyp(ibeg:iend) .eq. 'SKEWT' ) then
                  call getskewt(error)
               elseif ( plotyp(ibeg:iend) .eq. 'ISOSURF' ) then
                  call make3d(%val(varpt1), cpmax1, cpmax2, cpmax3,
     &              cpmax4)
c               elseif ( plotyp(ibeg:iend) .eq. 'BOX' ) then
c                  call make3d(%val(varpt1), cpmax1, cpmax2, cpmax3,
c     &              cpmax4)
               elseif ( plotyp(ibeg:iend) .eq. 'SCATTER' ) then
                  call make3d(%val(varpt1), cpmax1, cpmax2, cpmax3,
     &              cpmax4)
               elseif ( plotyp(ibeg:iend) .eq. 'WIREFRAME' ) then
                  call make3d(%val(varpt1), cpmax1, cpmax2, cpmax3,
     &              cpmax4)
               elseif ( plotyp(ibeg:iend) .eq. 'TRAJECTORY' ) then
                  call clear_traj_mouse()
                  call maktraj(error)
                  call start_traj_mouse()
               elseif ( plotyp(ibeg:iend) .eq. 'POINT' ) then
                  call makpointplt(%val(varpt1), cpmax1, cpmax2, 
     &                 cpmax3, cpmax4)
               elseif ( plotyp(ibeg:iend) .eq. 'SCALAR' .or. 
     &                  plotyp(ibeg:iend) .eq. 'LINE') then
c
c	Normal 2D plot
c
                  if(use_buttons .eq. 1 .and.
     &              lock_count .eq. 2 .and. ave_count .eq. 0 .or.
     &               (use_buttons .eq. 0 .and. plotyp .eq. 'SCALAR'))
     &                 then
                     
                     call mak2dc (%val(varpt1), cpmax1, cpmax2, 
     &                    cpmax3, cpmax4)
                     
c     
c	2D surface plot
c
c                  elseif (surface .and. 
c     &                    (lock(3) .eq. 1)) then
c                     call mak2dc (%val(varpt1), cpmax1, cpmax2, 
c     &                    cpmax3, cpmax4)
c
c	Line plots - 0, 1 or 2 average
c
                  elseif ((use_buttons .eq. 1 .and.
     &                (lock_count .eq. 3 .and. ave_count .eq. 0)
     &                  .or. (lock_count .eq. 2 .and. ave_count .eq. 1)
     &                  .or. (lock_count .eq. 1 .and. ave_count .eq. 2))
     &                  .or. (use_buttons .eq. 0 .and. 
     &                        plotyp .eq. 'LINE')) then 
                     call maklineplt (%val(varpt1), cpmax1, cpmax2, 
     &                    cpmax3, cpmax4)
c
c	Point plot - no average or 1 average
c
                  elseif( lock_count .eq. 2 .and. ave_count .eq. 2 .or.
     &                    lock_count .eq. 3 .and. ave_count .eq. 1 .or.
     &                    lock_count .eq. 4 ) then
                     call makpointplt(%val(varpt1), cpmax1, cpmax2, 
     &                    cpmax3, cpmax4)
c
c	2D plot - 1 or 2 average
c
                  elseif(free_count .eq. 2 .and.
     &                (ave_count .eq. 1 .or. ave_count .eq. 2))
     &                    then 
                     call mak2dc(%val(varpt1), cpmax1, cpmax2, 
     &                    cpmax3, cpmax4)
c pop up color
                  else
                     error = .true.
                     write (message, 100)
                     call write_message
                  endif
               endif
               if (.not.error) then
                  call save_plot_command(curoverlay(curlist)-1,
     &                 curlist-1)
c
c     Save plot window to the stack
c
                  call save_plot_window(plwmin,plwmax)
               endif
            endif
c     Make sure GKS really dumps buffers
            call ive_update_gks
            call update_all
            call getavar('data_display_units',disp_units, error)
            call set_var_atts(att_var_id, lin, cont_int, cont_values, 
     &           no_zero, disp_units(strbeg(disp_units):
     &                               strend(disp_units))
     &           //char(0), major_tic, 
     &           minor_tic, lmin, lmax, num_vals,
     &           nulbll,cllab)
            call set_dim_atts()
            safe_to_lock = .true.
         endif
c
 10      go to 5
c
      endif
c
c
c     "Melt" cursor when graphics is not working.
c
      call ui_update (COLORTAB_UI)
      call melt_dpy
c
c
c     Format statements.
c
 100  format (1x, 'You must limit the plot dimentions to 2 or less')
 110  format (1x, 'Error reading constants file.')
 120  format (1x, 'You can not lock the color table if you change',
     &     ' the contour interval or field or have not set these')
 130  format  (1x, 'Darnit')
 140  format (1x, 'Currently, you can only average in one direction')
c
c
      return
      end
