# $Id: Makefile,v 1.3 1994/09/14 18:19:30 harry Exp $
# $Log: Makefile,v $
# Revision 1.3  1994/09/14 18:19:30  harry
# Big 1D flip uncommit.
#
# Revision 1.2  1994/04/19  23:02:22  yuri
# Big 1D flip commit.
#
# Revision 1.1.1.1  1993/04/07  19:41:14  harry
# Initial CVS checkin of IVE.
#
#
RM = /bin/rm -f
HEADERS = 	cmndlst.h  ive_text_enum.h loop_setup.h ive_for.h \
		file_widgets.h  log.h state.h ive_widgets.h \
		ive_macros.h  loop.h ive.h ive_gks.h

all : ${HEADERS}
#
clean:
	${RM} cmndlst.h
#dependencies
cmndlst.h: ../src/command_list ../src/command_list.perl
	perl ../src/command_list.perl
#
