# $Id: Makefile,v 1.101 2006/08/22 23:58:13 warren Exp $
# $Log: Makefile,v $
# Revision 1.101  2006/08/22 23:58:13  warren
# Added commands draw_point and draw_point_map
#
# Revision 1.100  2002/12/27 18:44:29  warren
# First set of changes from zurich. use mapdrw_dl to deal with ever changing
# borders in Europe. Increase arrays in work.
#
# Revision 1.99  2002/12/26 22:49:49  warren
# Switched to new gif encoder
#
# Revision 1.98  2002/08/09 19:57:55  warren
# Fixed for Linux and tested with DEC sun etc.
# most items inside #ifdef's, or were general cleanup/fixes
#
# Revision 1.97  2002/06/11 17:26:14  harryive
# Change aspect ratio to affect the plot, not just printing.
#
# Add first part of contour value specification.
#
# Revision 1.96  2001/11/02 18:51:16  davidive
# converted rdctab to c, fixed trajectories so that their independant color
# tables work, including setting center color, center value and interval.
#
# Revision 1.95  2001/10/15 23:11:44  davidive
# write_message is now in C instead of FORTRAN
#
# Revision 1.94  2001/08/15 22:25:45  davidive
# Added point_axis.c to deal with which axis to plot agains in diagonal 1d plots.
# Added code to set up which axis to use and to convert the labels in 1d plots.
#
# Revision 1.93  2001/01/23 22:47:08  davidive
# cpline.f: fixed to use zero_color
# everything else: First cut at wireframes and isosurfaces. No dimentions yet
# no widgets yet.
#
# Revision 1.92  2000/12/07 23:09:54  harry
# Add fixdata.
#
# Revision 1.91  2000/09/21 19:49:43  harry
# Fix errors in line color and pattern handling.
#
# Revision 1.90  2000/08/02 19:59:42  harry
# Fix surface vector plotting.
#
# Revision 1.89  2000/05/09 19:53:40  harry
# Seperate contour and line colors.  Make patterns work like colors.
#
# Revision 1.88  2000/02/14 21:59:35  harry
# Allow point surface plots.
#
# Revision 1.87  1999/12/22 00:33:04  warren
# Fixed Dales crashing bug when changing from point slicer to normal. Also
# have some ifdef linuxes in there in case we ever find a fortran compiler.
#
# Revision 1.86  1999/10/26 17:37:00  harry
# Add call to new_field_trans in get2dv.  Add line width to vector plots.
#
# Revision 1.85  1999/02/04 00:28:23  harry
# Add vector scaling.
#
# Revision 1.84  1999/01/29 21:00:48  harry
# Fix dependencies.
#
# Revision 1.83  1999/01/29 20:46:23  warren
# ets the flag for time independence and the plot the trajectories
#
# Revision 1.82  1998/12/31 16:01:38  warren
# First cut of new trajectory look and feel. Trajectories are no longer
# on the popup menu, just plot type.
#
# Revision 1.81  1998/12/17 23:56:12  harry
# Remove cony_solid.
#
# Revision 1.80  1998/09/30 22:28:30  harry
# Rewrite makpointplt and add ability for 2 average points, fix errors
# in the 1d slicers, remove obsolete yuri routines.
#
# Revision 1.79  1998/09/03 19:19:18  harry
# Add in 2d line plots - correct bugs in other averaging.
#
# Revision 1.78  1998/07/28 18:22:19  harryive
# New fixes for trajectories when they are the first overlay.
# Also, remove old Yuri routine.
#
# Revision 1.77  1998/07/16 22:10:53  harryive
# Add two dimensional slicing with 2 averaged dimensions.
#
# Revision 1.76  1998/06/03 18:01:46  harry
# Fix typo with last change.
#
# Revision 1.75  1998/06/03 17:54:36  harry
# Fix tick mark labels.   Add new command COLOR_BAR.
#
# Revision 1.74  1998/03/04 19:13:55  davidive
# First cut at trajectories and trajectory widget using colors to show depth
# and arrows to show direction.
#
# Revision 1.73  1998/01/16 20:53:53  harry
# Add color.icl to drwtraj.
#
# Revision 1.72  1997/10/22 20:43:18  harry
# Add dependency on traj.icl to driver.o
#
# Revision 1.71  1997/07/02 16:09:33  harry
# winstack should use acc.
#
# Revision 1.70  1997/04/25 20:20:36  warren
# Keep a stack of the last 10 windows used for plotting in order to implement
# the window=up command.
#
# Revision 1.69  1997/04/15 17:24:37  harry
# Remove time_tag - use multiple files instead.
# Various fixes to vector/scalar looping.
#
# Revision 1.68  1997/04/09 19:37:05  harry
# Add scalar_name so when you go from vector to scalar you end up with
# the last scalar field.
#
# Revision 1.67  1997/03/20 22:22:58  harry
# Remove more meaningless yuri stuff.
#
# Revision 1.66  1997/03/14 22:26:31  warren
# Added new command map_window to allow for mouse windowing on non
# equadistant or default maps.
#
# Revision 1.65  1997/03/06 19:00:00  harry
# Make changes to properly handle exact_fit case for mouse slicing.
#
# Revision 1.64  1997/02/28 17:47:28  harry
# Fix dependencies for color.icl.
#
# Revision 1.63  1997/02/07 20:16:58  harry
# Yet more MULTIPLE_READ changes.
#
# Revision 1.62  1997/02/07 18:39:07  harry
# Remove PARTIAL_READ and expand MULTIPLE_READ.
#
# Revision 1.61  1997/01/31 18:39:19  harry
# This is FORTRAN, not C.
#
# Revision 1.60  1997/01/17 20:38:16  harry
# First changes for MULTIPLE_READ and PARTIAL_READ.
#
# Revision 1.59  1996/12/12 19:37:21  warren
# First crack at line on plot. Known to work on contour and line plots.
#
# Revision 1.58  1996/10/11 16:42:13  harry
# Add dependency to traj.o.
#
# Revision 1.57  1996/10/09 18:54:31  harry
# Add the ability to loop on file names if time value is part of name,
# and there is only one time per file.
#
# Revision 1.56  1996/07/18 18:02:35  harry
# Initial check-in of 3d slicer.
#
# Revision 1.55  1996/07/12 20:57:45  harry
# Fix dependencies for units.icl.
#
# Revision 1.54  1996/04/25 16:25:01  harry
# Add Skew-T plots and the beginning of trajectory plots.
#
# Revision 1.53  1996/04/01 20:23:33  warren
# Terrain form and new terrain command (on / off)
#
# Revision 1.52  1996/02/13  00:07:52  harry
# Remove maptrn.
#
# Revision 1.51  1995/12/18 17:09:13  harry
# Fix dependencies for chklinplt.f.
#
# Revision 1.50  1995/12/18 00:22:49  warren
# Ultrix doesn't like multiple declarations
#
# Revision 1.49  1995/12/15 22:33:12  warren
# moved patterns into 4.0 and implemented the list of fixes and changes from
# Curtis.
#
# Revision 1.48  1995/10/18  19:18:01  warren
# Stipples from 3.3, First crack at mouse windowing and put window_points_
# into a header file and out of the mouse stuff and slicers. Touched almost
# everything it seems.
#
# Revision 1.47  1995/09/15  16:48:17  harry
# Add support for timeout of warning messages.
#
# Revision 1.46  1995/09/05  20:33:35  harry
# Add new slicer.  Good luck!
#
# Revision 1.45  1995/08/28  18:17:57  warren
# ifeq(`arch`,mips) then ifeq better be in column 1.
#
# Revision 1.44  1995/08/28  15:17:42  warren
# removed superflous ifeq so the makefile will run on ultrix
#
# Revision 1.43  1995/08/25  23:33:24  harry
# Make sure there are NO explicit compilations of .F programs in the
# dependencies.
#
# Revision 1.42  1995/08/22  21:46:29  warren
# Build .F files with correct pointer size on alpha's, don't do strncasecmp
# without an n.
#
# Revision 1.41  1995/06/23  21:16:08  warren
# First part of new ive help
# Changed " to ' in fortran files
# Fixed the label spacing in 1d plots
# added frame_width command
#
# Revision 1.40  1995/05/11  22:40:15  harry
# Add new command WRITE_1D.
#
# Revision 1.39  1995/03/01  20:22:03  harry
# Fix dependency for uwperim.
#
# Revision 1.38  1995/01/19  15:32:39  warren
# First set of fixes found by the alpha. Mostly making sure that FORTRAN
# variables yuri declares and then checks are set to something in between.
#
# Revision 1.37  1994/11/22  22:47:41  harry
# Correct vector locking, add support for vector positioning, add
# support for filled contour overlays.  Use new NCAR vector plotting
# routine instead of modified old velvct.
#
# Revision 1.36  1994/09/14  18:19:47  harry
# Big 1D flip uncommit.
#
# Revision 1.35  1994/09/13  18:01:36  harry
# Delete obsolete files and fix the Makefile file:
#     lstseg.f new_ncar.f opengks.F plt2dc.F plt2dv.F pltline.F
#     set_abs_ord.f sethloc.f soundcalc.f strip.c
#
# Revision 1.34  1994/09/07  22:06:59  warren
# removed the extra plot from the color table locking sequence.
#
# Revision 1.33  1994/08/11  20:09:54  harry
# Remove cpline.F from dependencies for cpline.o.
#
# Revision 1.32  1994/06/28  17:43:20  warren
# First cut at fancy tic marks.
#
# Revision 1.31  1994/04/19  23:02:41  yuri
# Big 1D flip commit.
#
# Revision 1.30  1994/04/14  23:29:10  harry
# Make sure vector locking can only be turned on after a vector plot.
# Also fix the user interface updating for the vector_lock command.
#
# Revision 1.29  1994/03/31  00:22:25  warren
# Fixed it so we now actually use the and store the tic mark and limit values
# in the filed's property sheet.
#
# Revision 1.28  1994/03/28  20:00:19  warren
# added dependencies for read_color to make the color table change the
# first time.
#
# Revision 1.27  1994/03/15  17:23:37  harry
# Compile .F files directly to improve debugging.
#
# Revision 1.26  1994/02/17  21:37:04  harry
# Remove unused subroutines in get_time_var.f.
#
# Revision 1.25  1994/02/16  17:00:21  yuri
# 	Just cleaning and getting rid of "interp_factors" stuff.
#
# Revision 1.24  1994/01/29  00:11:24  warren
# first cut at movie labels. Line plots still need work
#
# Revision 1.23  1994/01/20  23:17:01  yuri
# New format of numbers in averaging messages. (After long long testing).
#
# Revision 1.22  1994/01/20  20:44:21  yuri
# Propagating of "overlay_patterns" stuff to newer version.
#
# Revision 1.21  1993/12/27  21:23:38  warren
# fixed make clean to get rid of spurious .f files when .F files exist.
#
# Revision 1.20  1993/12/15  18:15:24  harry
# Add support for DEFAULT_MAP command, default_map transform.  Modify map
# clipping to work properly with the map background widget.
#
# Revision 1.19  1993/11/29  19:38:57  warren
# Changes to 3.1 to make it work on alphas.
# FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
# C: Change int to long where necessary.
# Files: Quite a few .f files are now .F
#
# Revision 1.18  1993/11/02  17:42:36  warren
# Simplified Makefiles using config directory and passing everything down from
# the top.
#
# Revision 1.17  1993/10/29  18:17:39  yuri
# Propagating of new getdom file to IVE-3-1-? version.
#
# Revision 1.16  1993/08/31  19:14:56  yuri
# 	3D->2D averaging feature added.
#
# Revision 1.15  1993/07/23  20:36:48  yuri
# 	Command "vector_lock" is introduced to lock vector
# scaling in a loop (for example). Also some minor bugs are corrected.
#
# Revision 1.14  1993/07/20  22:44:40  warren
# Added ability to set prints and eps files to draw color lines. Added
# ability to choose printer. Added ability to specify which spot on the
# page to print at.
#
# Revision 1.13  1993/07/19  22:19:20  yuri
# 	Corrected typo.
#
# Revision 1.12  1993/07/16  17:19:45  yuri
# 	Updated ncar stuff added.
#
# Revision 1.11  1993/06/28  20:08:10  warren
# Changed acc to variable ACC to make compiling on SOLARIS 2 easier
#
# Revision 1.10  1993/06/24  22:14:06  yuri
# 	Corrected bugs in overlays scaling + cleaning up.
#
# Revision 1.9  1993/06/22  22:09:31  yuri
# 	New 1D and 2D slicers installed, added command interp_factors
# so user could choose number of interpolation points in slicing.
# Primarily it's for averaging, but user could use it every time
# before call slicers.
#
# Revision 1.8  1993/06/18  17:19:48  harry
# Make long loops work by adding save_state.c and changing loop.f and
# restore_state.c.
#
# Revision 1.7  1993/06/01  19:48:01  yuri
# 	Second attempt.
#
# Revision 1.6  1993/05/27  16:38:38  warren
# Recover files removed by yuri's commit
#
# Revision 1.2  1993/05/20  17:32:16  warren
# Additions and changes to make the new forms active.
#
# Revision 1.1.1.1  1993/04/07  19:39:55  harry
# Initial CVS checkin of IVE.
#
#

CPP = /usr/lib/cpp ${ALPHA}

.c.o:
	${CC}  -c ${CFLAGS} $*.c -o $*.o

.f.o:
	${F77} -c ${FFLAGS}  -o $*.o $*.f

.F.o:  
	${F77} -c  ${FCPP} ${FFLAGS} $*.F -o $*.o


#
#
OBJS = 	add_to_list.o average.o catnull.o catof.o  \
	catoi.o chk2d.o  chkcmd.o  chkcolor.o  \
	chklineplt.o chkplt.o chkvect.o  chkwin.o  \
	clrscr.o clsgks.o cmdpar.o cmpstr.o  \
	cntstr.o con2dc.o convert.o convert_units.o \
	cpline.o  \
	cvatof.o cvatoi.o  dg2rad.o do_system.o dr2dstrmln.o \
	dr2dv.o  drawline.o driver.o  drlineplt.o  draw_line.o draw_point.o\
	drwmap.o endprg.o expand_path.o fixdata.o freefld.o  \
	freemem.o ftoa.o  ftoa_fixpt.o genran.o get2dc.o  \
	get2dv.o  get_args.o  get_prscmd.o  \
	getavar.o  getdom.o getdom_2.o getlineplt.o  getmem.o \
	getvar.o help.o hlpcmd.o  input.o \
	inscmd.o  insertval.o  interp.o lock_color.o \
	loop.o mak2dc.o  mak2dv.o make3d.o \
	maklineplt.o makpointplt.o makseg.o mapdrw_dl.o maxval.o \
	mouse_slice.o movbl.o nblank.o  open_log.o opendat.o  output.o \
	point_axis.o printr.o pslicer2d.o pslicer3d.o \
	read_color.o readfld.o  redraw.o  run_sub_trans.o \
	restore_state.o  save_command.o save_plot_command.o save_state.o \
	scale.o \
	select_old.o  set2d.o \
	set_color_bar.o set_defmap.o set_points.o set_printer.o \
	set_scale_vector.o set_topo_scale.o  \
	set_units.o setabscissa.o setaratio.o  setax.o set_warn.o\
	setclclr.o  set_multiple_read.o \
	set_overlay_colors.o set_overlay_patterns.o \
	setcll.o setdef.o  setdfil.o  \
	setfld.o setftp.o setfw.o setgrd.o setint.o  \
	setlab.o  setlineplt.o setloc.o  setltp.o  \
	setlw.o setmap.o  set_max_plot.o setmpl.o  setmvct.o setmwin.o \
	setnoz.o  set_traj_ind.o \
	setnul.o setordinate.o  setotl.o setpla.o  \
	setplo.o  setplt.o  setprint.o  setprj.o \
	setrot.o setsav.o  setsurf.o setspv.o setstrmln.o \
	setsvl.o  settci.o setter.o set_traj_stuff.o settrfil.o settyp.o  \
	setvector.o setveclock.o setvec_pos.o setvin.o setvmax.o  setwin.o  \
	setxti.o slicer.o slicer1d.o slicer1d1a.o slicer1d2a.o \
	slicer2d1a.o slicer2d2a.o slicer3d.o \
	strbeg.o strmln1.o \
	tic_setup.o upcase.o  uwperim.o vdom.o  \
	vect2d.o  vlimit.o winstack.o write_frame.o \
	write_log.o write_message.o write_1d.o ybar.o \
	makskewt.o skewt.o setskewt.o getskewt.o labskewt.o\
	gettraj.o settraj.o maktraj.o drwtraj.o traj.o chktraj.o\
	huen.o start_traj.o labtraj.o int_traj.o traj_ind.o \
	volume_functions.o 
#
#
all: libIVE.a
#
libIVE.a: ${OBJS}
	@echo ------------------------------------
	@echo Comparing objects with library
	@echo ------------------------------------
	@ar rcvu ../src/libIVE.a ${OBJS}>>../need_ranlib
	${RM} *.out
#
#
clean:
	${RM} *.o
	${RM} libIVE.a
#
#
#
# dependencies

add_to_list.o : segments.icl
average.o : average.c
	${ACC} -c ${CFLAGS} $*.c -o $*.o
catnull.o : message.icl
chk2d.o   :  data.icl message.icl plotops.icl
chkcmd.o  :  cmndlst.icl cmndlst_data.icl message.icl
chklineplt.o : data.icl lineplt.icl message.icl plotops.icl
chkplt.o  :  message.icl units.icl
chktraj.o : data.icl lineplt.icl message.icl plotops.icl traj.icl 
chkvect.o :  message.icl
chkwin.o  :  data.icl message.icl units.icl
clrscr.o : clrscr.F
clsgks.o : clsgks.F
cmdpar.o  :  incommands.icl
../h/cmndlst.h cmndlst.icl cmndlst_data.icl: command_list \
                  command_list.perl
	perl command_list.perl
con2dc.o  :  attributes.icl conre.icl data.icl \
		  plotops.icl units.icl color.icl pltwin.icl supma.icl\
		  message.icl segments.icl ovl_props.icl con2dc.F
convert.o  :  convert.c ../h/missing.h
ifeq (${machine},sun4)
	${ACC} -c ${CFLAGS} $*.c -o $*.o
endif
cpline.o  :  work.icl ph2ll.icl plotops.icl \
		conre.icl color.icl message.icl \
		units.icl attributes.icl data.icl  supma.icl pltwin.icl
dr2dstrmln.o : strmln.icl dr2dstrmln.F color.icl
#	${F77} -c ${FFLAGS} ${FCPP} -assume recursive $*.f -o $*.o
dr2dv.o   :  color.icl vector.icl ive_widgets.icl plotops.icl
draw_line.o:	../h/window.h ../h/in_commands.h
driver.o  :  attributes.icl cmndlst.icl color.icl \
		  conre.icl data.icl \
		  deflts.icl driver.F flags.icl \
                  incommands.icl message.icl \
                  ph2ll.icl plotops.icl \
	          segments.icl supma.icl traj.icl \
	          ive_widgets.icl vector.icl
ifeq (${machine},sun4)
	${REALF77} -c  ${FCPP} ${FFLAGS} $*.F -o $*.o
endif
drlineplt.o : color.icl data.icl plotops.icl units.icl \
		   pltwin.icl  ovl_props.icl
drwmap.o  :  data.icl deflts.icl ive_widgets.icl message.icl \
                  ph2ll.icl supma.icl
drwtraj.o : traj.icl data.icl plotops.icl pltwin.icl supma.icl \
		color.icl message.icl
dump.o    :  ../h/file_widgets.h ../h/ive_macros.h ive.h loop.h \
		  ../h/cmndlst.h
fixdata.o : fixdata.c
ifeq (${machine},Linux)
	${CC}  -c ${CFLAGS} $*.c -o $*.o
else
ifeq (${machine},sun4)
	${CC}  -c ${CFLAGS} $*.c -o $*.o
else
	${CC}  -c ${CFLAGS} -ieee $*.c -o $*.o
endif
endif
freefld.o :  incommands.icl data.icl message.icl \
                  ive_widgets.icl
get2dc.o  :  data.icl message.icl plotops.icl get2dc.F topo.icl
get2dv.o  :  data.icl message.icl plotops.icl \
	          vector.icl get2dv.F units.icl topo.icl
get_prscmd.o: incommands.icl get_prscmd.F
getavar.o :  color.icl conre.icl   data.icl  lineplt.icl \
                  flags.icl message.icl traj.icl\
	          plotops.icl pltwin.icl segments.icl \
                  strmln.icl \
                  supma.icl message.icl units.icl \
	          vector.icl attributes.icl skewt.icl printer.icl
getdom.o  :  data.icl
getlineplt.o : data.icl plotops.icl message.icl getlineplt.F units.icl
getskewt.o:  message.icl data.icl skewt.icl units.icl plotops.icl
gettraj.o : traj.icl data.icl message.icl plotops.icl topo.icl units.icl
getvar.o  :  getvar.F message.icl data.icl units.icl
help.o    :  incommands.icl cmndlst.icl message.icl
huen.o	  :  traj.icl
hlpcmd.o  :  ../h/cmndlst.h ../h/ive_macros.h
input.o   :  ../h/file_widgets.h ../h/ive_macros.h
inscmd.o  :  segments.icl
int_traj.o : incommands.icl data.icl units.icl message.icl traj.icl
labskewt.o:  pltwin.icl units.icl segments.icl data.icl
labtraj.o : units.icl segments.icl pltwin.icl traj.icl
lock_color.o: incommands.icl plotops.icl units.icl\
	           message.icl color.icl data.icl attributes.icl
loop.o    :  cmndlst.icl data.icl incommands.icl \
		  loopflag.icl units.icl \
	          message.icl plotops.icl segments.icl
make3d.o: ../h/volume.h
makpointplt.o:	../h/missing.h ../h/window.h
makseg.o  : color.icl segments.icl
makskewt.o: message.icl data.icl segments.icl plotops.icl
maktraj.o : segments.icl message.icl
mak2dc.o: mak2dc.F  plotops.icl data.icl segments.icl pltwin.icl message.icl
mak2dv.o: mak2dv.F mak2dv.F segments.icl message.icl
maklineplt.o: segments.icl message.icl ovl_props.icl slices.icl plotops.icl
mouse_slice.o:  ../h/window.h ../h/ive_for.h
movbl.o:     incommands.icl flags.icl message.icl
open_log.o:  ../h/file_widgets.h ../h/ive_macros.h ../h/log.h
opendat.o :  data.icl units.icl message.icl
printr.o : message.icl segments.icl printer.icl
pslicer2d.o: ../h/missing.h
read_color.o: ../h/ive_gks.h ../h/ive_macros.h ../h/in_commands.h ../h/ive_color.h
readfld.o :  data.icl message.icl units.icl readfld.F
redraw.o  :  incommands.icl message.icl
restore_state.o : ../h/cmndlst.h ../h/state.h
save_command.o : ../h/cmndlst.h ../h/state.h
save_plot_command.o : ../h/cmndlst.h ../h/state.h 
save_state.o : ../h/cmndlst.h ../h/state.h
select_old.o:
set2d.o   :  data.icl deflts.icl plotops.icl color.icl message.icl \
		  pltwin.icl ovl_props.icl incommands.icl supma.icl\
	          flags.icl
setabscissa.o : incommands.icl lineplt.icl attributes.icl plotops.icl\
                     message.icl
setaratio.o : incommands.icl message.icl plotops.icl
setax.o   :  incommands.icl message.icl plotops.icl
setclclr.o : incommands.icl color.icl message.icl ovl_props.icl
setcll.o  :  incommands.icl conre.icl message.icl
setdef.o  :  cmndlst.icl color.icl conre.icl \
		  data.icl  pltwin.icl traj.icl\
		  deflts.icl flags.icl lineplt.icl \
		  plotops.icl segments.icl strmln.icl ovl_props.icl \
		  slices.icl supma.icl  units.icl vector.icl printer.icl
set_color_bar.o :  incommands.icl message.icl plotops.icl
set_scale_vector.o :  incommands.icl message.icl vector.icl
set_defmap.o  :  incommands.icl message.icl supma.icl ive_widgets.icl
setdfil.o :  data.icl incommands.icl message.icl
setfld.o  :  data.icl incommands.icl message.icl plotops.icl
setftp.o  :  incommands.icl message.icl plotops.icl attributes.icl
setfw.o   :  incommands.icl
setgrd.o  :  incommands.icl message.icl supma.icl
setint.o  :  color.icl incommands.icl message.icl \
		  attributes.icl
setlab.o  :  incommands.icl message.icl plotops.icl
setlineplt.o : color.icl data.icl deflts.icl lineplt.icl \
                    plotops.icl pltwin.icl units.icl ovl_props.icl
setloc.o :  incommands.icl loopflag.icl plotops.icl units.icl \
	message.icl data.icl attributes.icl lineplt.icl
setltp.o  :  incommands.icl message.icl plotops.icl color.icl ovl_props.icl
setlw.o   :  incommands.icl message.icl plotops.icl
setmap.o  :  incommands.icl flags.icl message.icl 
set_max_plot.o   :  ../h/cmndlst.h
setmpl.o  :  incommands.icl message.icl supma.icl ive_widgets.icl
setmvct.o :  incommands.icl message.icl vector.icl
setnoz.o  :  incommands.icl attributes.icl \
		  message.icl 
setnul.o  :  incommands.icl plotops.icl
set_multiple_read.o:	incommands.icl message.icl plotops.icl data.icl
set_overlay_colors.o:incommands.icl color.icl message.icl \
 ovl_props.icl
set_overlay_patterns.o:incommands.icl color.icl message.icl \
	  ovl_props.icl
setnul.o  :  incommands.icl conre.icl message.icl
setordinate.o : incommands.icl lineplt.icl attributes.icl plotops.icl \
                     message.icl
setotl.o  :  incommands.icl message.icl supma.icl
setpla.o  :  incommands.icl message.icl supma.icl ive_widgets.icl
setplo.o  :  incommands.icl message.icl supma.icl ive_widgets.icl
setplt.o  :  plotops.icl
set_points.o: incommands.icl message.icl plotops.icl pltwin.icl \
		data.icl units.icl
set_printer.o:	incommands.icl message.icl printer.icl
setprint.o :  setprint.F incommands.icl \
                   message.icl segments.icl
setprj.o  :  incommands.icl message.icl supma.icl ive_widgets.icl
setrot.o  :  incommands.icl message.icl supma.icl ive_widgets.icl
setsav.o  :  incommands.icl flags.icl message.icl 
setskewt.o:  incommands.icl skewt.icl message.icl data.icl
setspv.o  :  incommands.icl conre.icl message.icl
setstrmln.o : incommands.icl message.icl strmln.icl
set_topo_scale.o : incommands.icl message.icl 
settraj.o : traj.icl incommands.icl message.icl data.icl
set_traj_ind:	incommands.icl message.icl 
setsvl.o  :  incommands.icl conre.icl message.icl
settci.o  :  incommands.icl conre.icl message.icl
setter.o  :  incommands.icl plotops.icl message.icl 
settrfil.o : incommands.icl data.icl message.icl units.icl
settyp.o  :  incommands.icl message.icl plotops.icl
setveclock.o : incommands.icl ive_widgets.icl message.icl
setvector.o :  incommands.icl vector.icl \
                     message.icl  setvector.F
setvec_pos.o:  incommands.icl message.icl
setvin.o  :  incommands.icl message.icl vector.icl
setvmax.o : message.icl vector.icl message.icl incommands.icl
setwin.o  :  incommands.icl data.icl units.icl \
	          message.icl 
setxti.o  :  incommands.icl message.icl plotops.icl attributes.icl
set_units.o : incommands.icl message.icl units.icl data.icl
slicer.o  :  ../h/ive_macros.h ../h/missing.h ../h/window.h
slicer1d.o  :  ../h/ive_macros.h ../h/missing.h  ../h/window.h
slicer1d1a.o  :  ../h/ive_macros.h ../h/missing.h  ../h/window.h
slicer1d2a.o  :  ../h/ive_macros.h ../h/missing.h  ../h/window.h
slicer2d1a.o  :  ../h/ive_macros.h ../h/missing.h  ../h/window.h
slicer2d2a.o  :  ../h/ive_macros.h ../h/missing.h  ../h/window.h
slicer3d.o  :  ../h/ive_macros.h ../h/missing.h  ../h/window.h
set_warn.o: incommands.icl message.icl
start_traj.o: incommands.icl data.icl units.icl message.icl traj.icl
strmln1.o :  strmln1.f incommands.icl strmln.icl
	${F77} -c ${FOPT} $*.f -o $*.o
tic_setup.o:  ../h/cmndlst.h
traj.o : data.icl message.icl traj.icl plotops.icl
uwperim.o : data.icl units.icl pltwin.icl
vect2d.o  :  data.icl plotops.icl vector.icl pltwin.icl supma.icl
volume_functions.o: ../h/volume.h
winstack.o : winstack.c
	${ACC} -c ${CFLAGS} $*.c -o $*.o
write_frame.o : incommands.icl message.icl \
		    segments.icl
write_message.o : ../h/message.h write_message.c
write_log.o: ../h/log.h
write_1d.o:  incommands.icl message.icl segments.icl slices.icl

#

# DO NOT DELETE THIS LINE -- make depend depends on it.
