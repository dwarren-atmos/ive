# $Id: Makefile,v 1.35 2005/09/06 20:21:52 warren Exp $
# $Log: Makefile,v $
# Revision 1.35  2005/09/06 20:21:52  warren
# Move updates into current stream
#
# Revision 1.34  2002/08/09 19:56:33  warren
# added linuxisms to Makefile
#
# Revision 1.33  1999/12/22 00:31:58  warren
# Fixed Dales crashing bug when changing from point slicer to normal. Also
# have some ifdef linuxes in there in case we ever find a fortran compiler.
#
# Revision 1.32  1998/12/22 17:52:52  warren
# ifdef for insure
#
# Revision 1.31  1998/06/03 17:52:50  harry
# Move NO_PROTO to config.sun.  Fix config.alpha to remove lm_sqrt.
#
# Revision 1.30  1997/09/05 02:59:18  warren
# got rid of depends and mad symbole for udposix area
#
# Revision 1.29  1997/04/11 18:46:07  warren
# Fixed to pass acc everywhere
#
# Revision 1.28  1997/03/20 16:29:02  warren
# added IRIX64 for demo sgi
#
# Revision 1.27  1996/12/17  15:36:09  warren
# look for beta transforms first if compiled with -DBETA and up rev.
#
# Revision 1.26  1996/10/23 15:48:14  harry
# Add SGI IRIX.
#
# Revision 1.25  1996/02/22 23:18:22  harry
# Put TRANSOBJ in front of LOCALLIB.
#
# Revision 1.24  1995/12/18 00:21:34  warren
# move all ifeq and endifs to the left column for gmake on ultrix
#
# Revision 1.23  1995/12/17  21:32:59  warren
# use acc in trans directory so prototypinng works on sunos.
#
# Revision 1.22  1995/10/18  19:16:42  warren
# Stipples from 3.3, First crack at mouse windowing and put window_points_
# into a header file and out of the mouse stuff and slicers. Touched almost
# everything it seems.
#
# Revision 1.21  1995/07/13  17:24:58  warren
# Pass RANLIB to libcci
#
# Revision 1.20  1995/06/23  21:10:58  warren
# First part of new ive help
# Changed " to ' in fortran files
# Fixed the label spacing in 1d plots
# added frame_width command
#
# Revision 1.19  1995/02/24  19:19:50  harry
# Change location of the CenterLine products.
#
# Revision 1.18  1995/02/07  18:25:34  warren
# update for new dec c compiler
#
# Revision 1.17  1994/06/28  17:48:49  warren
# Opps, this wasn't supposed to go. Reverting to 1.15 equiv.
#
# Revision 1.16  1994/06/28  17:42:27  warren
# First cut at fancy tic marks.
#
# Revision 1.15  1993/12/13  23:47:40  warren
# fixed line plots so the box get drawn around them. also added a proof option
# to the makefile.
#
# Revision 1.14  1993/11/29  19:37:39  warren
# Changes to 3.1 to make it work on alphas.
# FORTRAN: new data type pointer ifdefed to either 32 bit or 64 bit.
# C: Change int to long where necessary.
# Files: Quite a few .f files are now .F
#
# Revision 1.13  1993/11/19  17:18:22  harry
# Fix Makefile for DBG=1 options.
#
# Revision 1.12  1993/11/02  17:42:27  warren
# Simplified Makefiles using config directory and passing everything down from
# the top.
#
# Revision 1.11  1993/10/04  22:29:59  warren
# Put tab where I got spaces.
#
# Revision 1.10  1993/09/08  22:34:51  harry
# Use static version of netcdf library.
#
# Revision 1.9  1993/08/09  17:57:19  warren
# changer -fast to -O -cg89
#
# Revision 1.8  1993/07/16  15:50:23  warren
# Fixed recursive part.
#
# Revision 1.7  1993/07/16  15:46:45  warren
# Align common blocks in DEC compiler.
#
# Revision 1.6  1993/06/28  21:20:18  warren
# Fixed passing of acc for X
#
# Revision 1.5  1993/06/28  20:08:03  warren
# Changed acc to variable ACC to make compiling on SOLARIS 2 easier
#
# Revision 1.4  1993/06/22  17:49:23  harry
# Fix Makefile to allow statically linked transforms for debugging purposes.
#
# Revision 1.3  1993/05/27  16:37:15  warren
# Recover files removed by yuri's commit
#
# Revision 1.1.1.1  1993/04/07  19:39:45  harry
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
UDPOSIX_INCLUDE = -I/usr/local/include/udposix
LD = ${CC}
PR =  /usr/local/CenterLine/bin/proof ${LD}
machine := ${shell /usr/bin/uname}
ifndef machine
machine := ${shell /bin/uname}
endif
ifndef machine
machine := ${shell /usr/ucb/uname}
endif
ifndef machine
machine := ${shell /usr/ucb/arch}
endif
ifndef machine
machine := ${shell /usr/bin/arch}
endif
ifndef machine
machine := ${shell /usr/bin/uname}
endif
ifndef machine
machine := ${shell /bin/uname}
endif
PWD := ${shell /usr/bin/pwd}
ifndef PWD
PWD := ${shell /bin/pwd}
endif
COFLAGS = -M
FDEC = 
TRANSOBJ =
RM = /bin/rm -f
RCSRM = -/usr/local/bin/rcsclean
ifeq (${machine},mips)
include configs/config.ult
endif
ifeq (${machine}, alpha)
include configs/config.alpha
endif
ifeq (${machine}, OSF1)
include configs/config.alpha
endif
ifeq (${machine},sun4)
include configs/config.sun
endif
ifeq (${machine},SunOS)
include configs/config.sun
endif
ifeq (${machine}, IRIX)
include configs/config.sgi
endif
ifeq (${machine}, IRIX64)
include configs/config.sgi
endif
ifeq (${machine}, Linux)
include configs/config.linux
endif
ifeq (${machine}, x86_64)
include configs/config.linux
endif

ifdef DBG
FOPT = -g
COPT = -g -DSTATIC
TRANSOBJ = trans.o
else
ifdef MEMDBG 
COPT= -g -O0 -DMEMDBG
FOPT= -g -O0
endif
endif

ifdef BETA
DOBETA = -DBETA
else
BETA =
endif

ifndef BROWSER
BROWSER='-DBROWSER=\"netscape\"'
endif

ifdef INSURE
ACC=insure
CC=insure
LD=insure -Zsl
COPT=-g
endif

CFLAGS = ${COPT} -I${PWD}/h -I${PWD}/FTGL/include ${DEC} ${DOBETA}\
	-I${PWD}/h/haru ${UDPOSIX_INCLUDE} ${BROWSER}
FFLAGS = ${FOPT} ${FDEC} ${ALPHA} ${FCPP}
#
#

#
ifdef MEMDBG 
LOCALLIB = src/libIVE.a gks/libIVEgks.a haru/libhpdf.a src/libIVE.a -lmcheck
else
LOCALLIB = src/libIVE.a gks/libIVEgks.a haru/libhpdf.a src/libIVE.a 
endif
#
#
all: IVE
link: LINK_ONLY
haru:
	${MAKE} -C ./haru CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}" RANLIB="${RANLIB}"
gks:
	${MAKE} -C ./gks CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}" RANLIB="${RANLIB}"
	${MAKE} LDFLAGS="${LDFLAGS}" link

X:
	${MAKE} -C ./X CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${ACC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP} COPT=${COPT} PWD=${PWD}"
	${MAKE} LDFLAGS="${LDFLAGS}" link

src:
	${MAKE} -C ./src CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" KF77="${KF77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}" machine="${machine}"
	${MAKE} LDFLAGS="${LDFLAGS}" link

netcdf:
	${MAKE} -C ./netcdf CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}"
	${MAKE} LDFLAGS="${LDFLAGS}" link

trans:
	${MAKE} -C ./trans CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}"
	${MAKE} LDFLAGS="${LDFLAGS}" link
#
#
clean:
	cd h; ${MAKE} clean; cd ..
	cd src; ${MAKE} clean; cd ..
	cd gks; ${MAKE} clean; cd ..
	cd haru; ${MAKE} clean; cd ..
	cd netcdf; ${MAKE} clean; cd ..
	cd trans; ${MAKE}  clean; cd ..
	cd X; ${MAKE}  clean; cd ..

#
#
#

IVE:  
	${MAKE} -C ./h CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}"
	${MAKE} -C ./src CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" KF77="${KF77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}" machine="${machine}"
	${MAKE} -C ./gks CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}" RANLIB="${RANLIB}"
	${MAKE} -C ./haru CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}" RANLIB="${RANLIB}"
	${MAKE} -C ./netcdf CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${CC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}"
	${MAKE} -C ./trans CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${ACC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}"
	${MAKE} -C ./X CFLAGS="${CFLAGS}" FFLAGS="${FFLAGS}" ACC="${ACC}" ALPHA="${ALPHA}" CC="${ACC}" F77="${F77}" TRANSOBJ=${TRANSOBJ} FCPP="${FCPP}"
	@if test -s need_ranlib; then echo need ranlib; ${RANLIB} src/libIVE.a; fi
	${RM} need_ranlib
#	${LD} -o ive -u main  ${LDSTUFF} ${TRANSOBJ} ${LOCALLIB} ${LIBS}
	${LD} -o ive X/ive_main.o  ${LDSTUFF} ${TRANSOBJ} ${LOCALLIB} ${LIBS} ${PWD}/FTGL/lib/libftgl.a
#
LINK_ONLY:
	@if test -s need_ranlib; then ranlib src/libIVE.a; fi
	${RM} need_ranlib
	${LD} -o ive -u main ${LDSTUFF} ${LOCALLIB} ${TRANSOBJ} {LIBS}
#
PROOF:
	@if test -s need_ranlib; then ranlib src/libIVE.a; fi
	${RM} need_ranlib
	${PR} -o ive -u main ${LDSTUFF} ${LOCALLIB} ${TRANSOBJ} ${LIBS}
#
#
# dependencies
.PHONY:gks haru X src netcdf trans clean all link
#

