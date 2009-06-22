# $Log: Makefile,v $
# Revision 1.32  2002/12/26 22:48:10  warren
# New gif encoder (faster and easier to use), also fixed gif encoding
# to work from truecolor screens.
#
# Revision 1.31  2002/12/09 18:03:07  warren
# Instead of checking number of plane every time, just check once and record
# if it is true color in bool.
#
# Revision 1.30  2000/12/07 19:50:13  harry
# Allow dumping of loops of 100 images or more.
#
# Revision 1.29  2000/11/28 23:35:07  harry
# Add list of recent files to Input pulldown menu.
#
# Revision 1.28  1999/12/22 00:32:03  warren
# Fixed Dales crashing bug when changing from point slicer to normal. Also
# have some ifdef linuxes in there in case we ever find a fortran compiler.
#
# Revision 1.27  1998/09/04 20:51:37  harry
# Add colorbar button to annotation widget.  Rewrite file widgets to
# remember previous entries.
#
# Revision 1.26  1998/06/11 21:52:11  harry
# Fix loop sliders to act like window sliders.
#
# Revision 1.25  1998/01/28 15:28:17  harry
# Round off slider values to the nearest integer rather than truncating.
#
# Revision 1.24  1996/11/07 16:53:54  warren
# First set of activated point slicing and widgets to go with it. Only 1d plots
# are known to be correct. Use at your own risk for 2d. There is still a known
# bug in the connection between the tic mark entry boxes and the plot when doing
# point or mouse slicing.
#
# Revision 1.23  1996/04/25 16:24:14  harry
# Add Skew-T plots and the beginning of trajectory plots.
#
# Revision 1.22  1996/04/01 20:23:07  warren
# Terrain form and new terrain command (on / off)
#
# Revision 1.21  1996/02/15 23:17:00  harry
# Correct check for number of colors.
#
# Revision 1.20  1995/12/15 22:32:33  warren
# moved patterns into 4.0 and implemented the list of fixes and changes from
# Curtis.
#
# Revision 1.19  1995/10/19 22:13:03  harry
# Set defaults by clicking right mouse button.  Make all TextWidgets into
# TextField Widgets, fix bug in loop widget.
#
# Revision 1.18  1995/08/28  18:17:41  warren
# ifeq(`arch`,mips) then ifeq better be in column 1.
#
# Revision 1.17  1995/06/23  21:11:13  warren
# First part of new ive help
# Changed " to ' in fortran files
# Fixed the label spacing in 1d plots
# added frame_width command
#
# Revision 1.16  1995/05/26  15:20:00  harry
# Add SAVE_WINDOW command and widget.
#
# Revision 1.15  1995/02/07  19:47:38  warren
# Last change for DEC C.
#
# Revision 1.14  1994/11/22  22:46:28  harry
# Correct vector locking, add support for vector positioning, add
# support for filled contour overlays.
#
# Revision 1.13  1994/11/18  20:43:38  harry
# Add ive_widgets.h to the dependencies of forms_callbacks.o.
#
# Revision 1.12  1994/10/08  18:59:46  warren
# The annotation widget and updating it.
#
# Revision 1.11  1994/05/18  17:52:54  harry
# Clean up of comments, and move color callbacks into one file.
#
# Revision 1.10  1993/11/02  17:42:47  warren
# Simplified Makefiles using config directory and passing everything down from
# the top.
#
# Revision 1.9  1993/09/08  23:14:25  warren
# Print widget code.
#
# Revision 1.8  1993/08/11  21:43:04  warren
# Fixed broken toggle buttons.
#
# Revision 1.7  1993/07/30  22:48:44  warren
# Added widget to eps button to save EPS files.
#
# Revision 1.6  1993/06/28  23:27:20  harry
# Change name of ive_slicer_widget.c to slicer_widget.c.
#
# Revision 1.5  1993/06/24  23:14:05  warren
# added call and routine for map form.
#
# Revision 1.4  1993/05/27  19:12:22  warren
# Fixed from merged version
#
# Revision 1.3  1993/05/27  16:38:15  warren
# Recover files removed by yuri's commit
#
# Revision 1.2  1993/05/20  16:58:17  warren
# Additions and changes to make the new forms active. Includes new ui_update
# code.
#
# Revision 1.1.1.1  1993/04/07  19:41:16  harry
# Initial CVS checkin of IVE.
#
# Revision 2.67  1993/03/02  18:35:01  warren
# updated to use the new xgks
#
# Revision 2.66  1993/02/19  17:30:12  warren
# fixed getting contour int from property sheet for color fill
#
# Revision 2.65  1993/02/17  22:48:58  warren
# fixed errors in RCSCLEAN and REALCLEAN
#
# Revision 2.64  1993/02/17  22:41:34  warren
# added carrage return to end of file
#
# Revision 2.63  1993/02/17  22:07:27  warren
# added MEMDBG option for memory allocation debugging.
#
# Revision 2.62  1993/01/12  20:45:55  millerp
# Fix some dependencies.
#
# Revision 2.61  1993/01/08  16:43:42  harry
# Add segments.icl to con2dc.o.
#
# Revision 2.60  1993/01/07  17:23:24  harry
# Change write_file to write_frame.
#
# Revision 2.59  1993/01/06  18:16:12  harry
# Fix dependencies for con2dc.o.
#
# Revision 2.58  1993/01/06  17:20:19  harry
# Add segments.icl to dependencies of makseg.o.
#
# Revision 2.57  1993/01/04  16:09:11  millerp
# remove maklst
#
# Revision 2.56  1993/01/01  00:47:58  warren
# added  netcdf/property_sheets. removed uars/maklst.o.
# added dependency for driver.o on uars/attributes.icl
#
# Revision 2.55  1992/12/23  18:49:20  harry
# Add open_log and write_log.
#
# Revision 2.54  1992/12/21  19:21:08  harry
# Use static version of udunits, add udport.
#
# Revision 2.53  1992/12/21  19:11:14  millerp
# Changed help routines.
#
# Revision 2.52  1992/12/14  18:42:01  harry
# Change depends for file_callbacks, file_windows, dump, input
# and loop_calls.
#
# Revision 2.51  1992/12/10  22:20:00  millerp
# Fix some dependencies.
#
# Revision 2.50  1992/12/03  00:04:52  millerp
# Add dependency for driver
#
# Revision 2.49  1992/12/02  00:42:53  harry
# Fix dependencies for uars/dump.o
#
# Revision 2.48  1992/11/26  18:07:29  millerp
# Remove rule for set_abs_ord, an obsolete function.
#
# Revision 2.47  1992/11/21  00:23:32  harry
# Fix cmndlst_data.icl dependencies.
#
# Revision 2.46  1992/11/21  00:09:25  millerp
# Fix up some dependencies.
#
# Revision 2.45  1992/11/20  00:27:54  harry
# Fix dependencies for dump and makgif.
#
# Revision 2.44  1992/11/20  00:04:57  harry
# Add Window_Dump, dump and makgif.
#
# Revision 2.43  1992/11/18  19:50:01  warren
# added dependency for ive_text_enum.h to loop_setup.o
#
# Revision 2.42  1992/11/18  18:10:35  harry
# Remove input_file, add get_args and get_prscmd.
#
# Revision 2.41  1992/11/17  22:58:51  warren
# fixed last fix
#
# Revision 2.40  1992/11/17  22:25:35  warren
# made dec not use acc
#
# Revision 2.39  1992/11/17  20:11:12  millerp
# add CLEAN_SOURCE.
#
# Revision 2.38  1992/11/17  17:40:59  millerp
# Fix dependencies for forms_callbacks and forms_widgets.
#
# Revision 2.37  1992/11/17  17:35:10  warren
# added dependencies for c code calling make_help_widget - ive_macros.h
#
# Revision 2.36  1992/11/16  22:37:22  harry
# Add input.c and input_file.f
#
# Revision 2.35  1992/11/12  22:19:05  harry
# Check for command_list and command_list.perl.
# Delete derived cmndlst* files if command_list and command_list.perl
# are cleaned.
#
# Revision 2.34  1992/11/10  16:23:11  harry
# Add new method to generate command include files.
#
# Revision 2.33  1992/11/06  18:33:59  warren
# increased dec Olimit
#
# Revision 2.32  1992/11/05  20:34:37  millerp
# Added tslicer.
#
# Revision 2.29  1992/10/28  22:30:59  millerp
# Remove references to SETAVERAGE
#
# Revision 2.28  1992/10/28  22:26:02  warren
# added average.o
#
# Revision 2.27  1992/10/21  21:33:12  warren
# added write_file
#
# Revision 2.26  1992/10/16  16:37:54  warren
# *** empty log message ***
#
# Revision 2.25  1992/10/16  16:36:35  warren
# added to RAELCLEAN
#
# Revision 2.24  1992/10/16  16:16:56  warren
# added REALCLEAN option
#
# Revision 2.23  1992/10/16  15:06:50  warren
# got rid of blank line that messed up con2dc.o
#
# Revision 2.22  1992/10/15  23:16:05  warren
# *** empty log message ***
#
# Revision 2.21  1992/10/15  23:12:46  warren
# add dependency for con2dc
#
# Revision 2.20  1992/10/15  23:01:21  warren
# added color locking stuff
#
# Revision 2.19  1992/10/08  23:15:27  harry
# Update dependencies on loop_setup.o
#
# Revision 2.18  1992/10/07  22:16:13  harry
# Add loop_setup.c, loop_setup.h, loop_setup_calls.c
#
# Revision 2.17  1992/10/02  22:58:59  warren
# changed the commonblock window_points to specify witch axis is what
# and put it in pltwin.icl
#
# Revision 2.16  1992/09/30  15:23:42  warren
# new makefile additions and deletions for physical line slicer.
#
# Revision 2.15  1992/09/29  18:45:21  harry
# Rename free_pixmaps to loop_quit.
#
# Revision 2.14  1992/09/24  23:13:51  harry
# Remove strip.
#
# Revision 2.13  1992/09/24  17:56:12  harry
# Fix depends for setdef.
#
# Revision 2.12  1992/09/24  17:46:25  harry
# Add dependencies to setdef.
#
# Revision 2.11  1992/09/23  23:09:53  harry
# Add restore_state, save_command, save_plot_command, cmndlst.h.
# Removed setsounding and setvindex.
#
# Revision 2.10  1992/09/22  21:35:23  harry
# Add *.c and *.h files in UARS to RCSCLEAN.,
#
# Revision 2.9  1992/09/22  17:49:37  warren
# add slice command
#
# Revision 2.8  1992/09/16  15:49:51  warren
# added setloc.o and removed settime.o and setvloc.o
#
# Revision 2.7  1992/09/08  21:57:32  millerp
# Fix up dependencies.
#
# Revision 2.6  1992/09/08  21:35:32  millerp
# Added stuff for new slicers.
#
# Revision 2.4  1992/08/11  16:09:21  harry
# Add loop_control, loop_calls.  Remove loopem.
#
# Revision 2.3  1992/07/13  22:26:57  warren
# fxgks -> fxgks_uw
#
# Revision 2.2  1992/07/06  21:46:19  millerp
# *** empty log message ***
#
# Revision 2.1  1992/07/06  19:54:34  millerp
# *** empty log message ***
#
# Revision 2.0  1992/07/01  21:11:08  millerp
# Released version rereving to 2.0
#
# Revision 1.15  1992/06/19  21:40:12  warren
# added uars/util/get_time_var.o
#
# Revision 1.14  1992/06/17  18:31:27  warren
# added udunits for DECstations
#
# Revision 1.13  1992/06/17  16:17:45  warren
# *** empty log message ***
#
# Revision 1.12  1992/06/15  20:08:48  millerp
# New routines.
#
# Revision 1.8  1992/06/01  16:20:58  millerp
# Added new routines.
#
# Revision 1.7  1992/05/29  22:29:10  millerp
# Added units stuff and time dimension in slicers, plus other
# minor fixes and additions.
#
# Revision 1.4  1992/05/06  15:35:07  millerp
# Added routines for sounding plots.
#
# Revision 1.3  1992/04/29  00:04:57  millerp
# *** empty log message ***
#
# Revision 1.2  1992/04/21  18:57:07  millerp
# Fixed some dependencies.
#
# Revision 1.1  1992/04/21  00:08:28  millerp
# Initial revision
#

machine := ${shell /usr/bin/arch}
CPP = /usr/lib/cpp ${ALPHA}
RM = /bin/rm -f

#
OBJS = 	Window_Dump.o add_credits.o add_filelist.o \
	annotation.o background.o color_cb.o \
	colormix.o control_c.o dump.o \
	file_callbacks.o file_windows.o forms_callbacks.o \
	forms_setup.o \
	forms_widgets.o gifencod.o gks_widgets.o ive_callbacks.o \
	ive_main.o ive_main_widget.o slicer_widget.o \
	ive_ui_update.o loop_calls.o loop_control.o \
	loop_quit.o loop_setup.o loop_setup_calls.o \
	loopem.o pattern_form.o points_widget.o points_callbacks.o \
        nup_forms.o \
	save_pixmap.o save_window.o \
	terrain_form.o
#
#
all: ../src/libIVE.a
#
../src/libIVE.a : ${OBJS}
	@echo ------------------------------------
	@echo Comparing objects with library
	@echo ------------------------------------
	@ar rcvu ../src/libIVE.a ${OBJS}>>../need_ranlib
#
#
clean:
	${RM} *.o
#
#
#
# dependencies
add_filelist.o		: ../h/ive_for.h ../h/ive_widgets.h
annotation.o		: ../h/ive_macros.h ../h/ive_text_enum.h
color_cb.o		: ../gks/gks_implem.h ../h/ive_gks.h ../h/ive_for.h \
			  ../h/ive_macros.h ../h/ive_color.h
colormix.o		: ../h/ive_gks.h  ../h/ive_macros.h ../h/ive_color.h
control_c.o		: ../h/ive_gks.h  ../h/ive_macros.h ../h/ive_for.h
ifeq (${machine}, alpha)
	cc -migrate -D__alpha -I/usr/include -I/usr/local/include -I${PWD}/h -I/usr/local/include/udposix -c control_c.c -o control_c.o
endif
dump.o			: ../h/ive.h ../h/ive_macros.h ../h/file_widgets.h \
			  ../h/cmndlst.h ../h/loop.h
file_callbacks.o	: ../h/ive_macros.h ../h/ive_for.h \
			  ../h/file_widgets.h ../h/loop.h
file_windows.o		: ../h/file_widgets.h
forms_callbacks.o	: ../h/ive_macros.h ../h/file_widgets.h ../h/ive.h \
			  ../h/ive_text_enum.h ../h/ive_widgets.h \
			  ../h/properties.h
forms_setup.o		: ../h/ive_for.h ../h/ive_text_enum.h forms_setup.c \
			  ../h/ive.h ../h/ive_macros.h ../h/ive_widgets.h \
			  ../h/properties.h
	${ACC}  -c ${CFLAGS} forms_setup.c -o forms_setup.o
forms_widgets.o		: ../h/ive_macros.h ../h/file_widgets.h \
			  ../h/ive_text_enum.h ../h/ive_widgets.h
gks_widgets.o		: ../h/ive_gks.h 
ive_callbacks.o		: ../h/ive_for.h ../h/ive_gks.h ../h/ive_macros.h \
			  ../h/ive.h
ive_main.o		: ../h/ive_macros.h ../h/ive_for.h ../h/ive_gks.h
ive_main_widget.o	: ../h/ive.h  ../h/ive_for.h ../h/ive_macros.h \
			  ../h/print.h
slicer_widget.o		: ../h/ive.h  ../h/ive_for.h ../h/ive_macros.h
ive_ui_update.o		: ../h/ive_widgets.h  ../h/loop_setup.h \
			  ../h/properties.h ../h/loop_setup.h \
			  ../h/ive_text_enum.h ../h/file_widgets.h
loop_calls.o		: ../h/ive_for.h ../h/file_widgets.h ../h/loop.h
loop_control.o		: ../h/ive.h ../h/ive_macros.h ../h/loop.h
loop_quit.o		: ../h/loop.h
loop_setup.o		: ../h/ive.h ../h/ive_widgets.h ../h/loop_setup.h \
			  ../h/ive_text_enum.h
loop_setup_calls.o	: ../h/ive_for.h ../h/loop_setup.h ../h/ive.h
nup_forms.o		: ../h/ive_macros.h ../h/file_widgets.h \
			  ../h/ive_text_enum.h ../h/ive_for.h
points_callbacks.0	: ../h/ivs_for.h ../h/ive.h ./h/ive_widgets.h
save_pixmap.o		: ../h/ive.h ../h/loop.h
save_window.o		: ../h/ive.h
terrain_form.c		: ../h/ive.h ../h/ive_for.h ../h/ive_macros.h \
			  ../h/ive_text_enum.h
Window_Dump.o		: ../h/ive_gks.h
#.Silent
