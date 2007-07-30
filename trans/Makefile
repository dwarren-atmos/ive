# $Id: Makefile,v 1.6 2007-07-05 19:25:02 warren Exp $
# $Log: Makefile,v $
# Revision 1.6  2007-07-05 19:25:02  warren
# Final commit of everything pre svn
#
# Revision 1.5  1997/02/27 16:23:27  warren
# Fixed to accept F77 passed down from main Makefile (config)
#
# Revision 1.4  1995/12/08 17:12:51  harry
# Add derivative computation for default transform.
#
# Revision 1.3  1994/11/22  22:45:23  harry
# Add vector transform routine vvumxy for use with new NCAR vector routine.
#
# Revision 1.2  1993/11/02  17:43:05  warren
# Simplified Makefiles using config directory and passing everything down from
# the top.
#
# Revision 1.1.1.1  1993/04/07  19:41:27  harry
# Initial CVS checkin of IVE.
#
#
machine := ${shell /usr/bin/arch}
CPP = /usr/lib/cpp ${ALPHA}
RM = /bin/rm -f

.c.o:
	${CC}  -c ${CFLAGS} $*.c -o $*.o

.f.o:
	${F77} -c ${FFLAGS} $*.f -o $*.o

.F.o:  
	${F77} -c  ${FCPP} ${FFLAGS} $*.F -o $*.o
#
OBJS = 	cpmpxy.o derivative.o loadfunc.o vvumxy.o
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

.PHONY: ../src/libIVE.a
