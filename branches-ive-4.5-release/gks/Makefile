# $Id: Makefile,v 1.8 2006/03/06 19:47:20 warren Exp $
# $Log: Makefile,v $
# Revision 1.8  2006/03/06 19:47:20  warren
# Need local version of fortran wrappers to keep consistancy between IVE changes and fortran
# calling.
#
# Revision 1.7  1999/12/22 00:32:20  warren
# Fixed Dales crashing bug when changing from point slicer to normal. Also
# have some ifdef linuxes in there in case we ever find a fortran compiler.
#
# Revision 1.6  1996/01/12 16:11:40  warren
# Transfer of new pattern stuff from 3.x and beginning of color fixes
# (color vectors, correct collors on the pulldowan menus) added xcolours.c
#
# Revision 1.5  1995/12/18  22:24:17  harry
# Remove the library on clean.
#
# Revision 1.4  1995/10/18  19:17:21  warren
# Stipples from 3.3, First crack at mouse windowing and put window_points_
# into a header file and out of the mouse stuff and slicers. Touched almost
# everything it seems.
#
# Revision 1.3  1995/05/11  20:20:40  warren
# Bring printer updates forward from 3.2
#
# Revision 1.2  1993/07/06  17:34:49  warren
# Add Mouse control to gks window.
#
# Revision 1.1.1.1  1993/04/07  19:41:03  harry
# Initial CVS checkin of IVE.
#
#

machine := ${shell /usr/bin/arch}
COFLAGS = -M
F77 = f77
CC = cc
CPP = /usr/lib/cpp
ifdef DBG
FOPT = -g
COPT = -g -DSTATIC
TRANSOBJ = user/trans.o
else
FOPT = -O 
ifeq (${machine},mips)
COPT = -O -Olimit 1000
else
COPT = -O
endif
#
ifdef MEMDBG 
COPT= -g -DMEMDBG -I./mnemosyne
FOPT= -g 
endif
#
ifdef DEBUG
COPT= -g 
FOPT= -g 
endif
#
TRANSOBJ =
endif
#DECstation 
ifeq (${machine},mips)
CC = c89
DEC = -common
ifeq(${CC},cc)  DEC = -Wf,-XNl4096
#SPARCstation
else
ifeq (${machine},sun4)
DEC = -I/usr/lang/SC1.0/ansi_include  #gets sun float.h
endif
endif
RM = /bin/rm -f
ifeq (${CC}, gcc)
CFLAGS = ${COPT} ${DEC}  \
        -I../h -I/usr/local/include/udposix -I/usr/include/mit
else
CFLAGS = ${COPT} ${DEC} -D_NO_PROTO \
        -I../h -I/usr/local/include/udposix -I/usr/include/mit
endif
FFLAGS = ${FOPT} -u 

#
OBJS = 	XgksWidget.o open_ws.o xopws.o xevent.o segments.o xfillarea.o \
	xcolours.o fortran_wrap.o
#
#
all: libIVEgks.a
#
libIVEgks.a : ${OBJS}
	@echo ------------------------------------
	@echo Comparing objects with library
	@echo ------------------------------------
	@ar rcvu libIVEgks.a ${OBJS}
	${RANLIB} libIVEgks.a
#
#
clean:
	${RM} *.o libIVEgks.a
#
#
# dependencies
XgksWidget.o: gks_implem.h XgksWidget.h XgksWidgetP.h 
open_ws.o: gks_implem.h
xopws.o: gks_implem.h
gks_implem.h: 	gks_defines.h gkslist.h primitive.h gks_errors.h input.h \
		wdt.h event.h metafile.h wslist.h 

xfillarea.o: gks_implem.h gks_defines.h primitive.h input.h wdt.h wslist.h gkslist.h gks_errors.h event.h metafile.h
xevent.o:	gks_implem.h ../h/ive_for.h
