# $Id: Makefile,v 1.8 1997/07/02 18:14:54 warren Exp $
# $Log: Makefile,v $
# Revision 1.8  1997/07/02 18:14:54  warren
# Use "ACC" for everything in this directory.
#
# Revision 1.7  1995/12/18 00:22:13  warren
# increase olimit to 1500 for derivitive.c
#
# Revision 1.6  1995/11/20 17:54:06  harry
# Add routine ncattgetf to return the value of an attribute as a float no
# matter how it appears in the netCDF file.
#
# Revision 1.5  1994/02/17  21:33:02  harry
# Add dim_names to the calling sequece of read_var and remove subroutine
# get_dim_names.
#
# Revision 1.4  1993/11/02  17:42:42  warren
# Simplified Makefiles using config directory and passing everything down from
# the top.
#
# Revision 1.3  1993/08/18  15:46:39  harry
# Change to new transform routines.
#
# Revision 1.2  1993/06/29  16:31:18  harry
# Remove obsolete routine read_d_var, correct add_field to set
# data_display_orig, change free_var to copy attributes, change
# get/set_var_atts to not overflow character variables.
#
# Revision 1.1.1.1  1993/04/07  19:41:09  harry
# Initial CVS checkin of IVE.
#
#

machine := ${shell /usr/bin/arch}
#
OBJS = 	add_field.o change_units.o free_var.o ncattgetf.o open_var.o \
	 property_sheets.o read_var.o set_button_name.o
.c.o:
	${ACC} -c ${CFLAGS} $*.c -o $*.o
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
add_field.o      : cdfinfo.h diminfo.h
change_units.o    : cdfinfo.h diminfo.h
free_var.o        : cdfinfo.h diminfo.h
open_var.o        : cdfinfo.h diminfo.h
property_sheets.o : cdfinfo.h diminfo.h
read_var.o        : cdfinfo.h diminfo.h
set_button_name.o : cdfinfo.h
#

