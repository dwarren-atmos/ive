# Makefile written by Paul Pomes, University of Illinois,
# Computing Services Office
#
# Copyright (C) 1990 by Paul Pomes and the University of Illinois Board
# of Trustees
#
# Written by Paul Pomes, University of Illinois, Computing Services Office
# Copyright (C) 1990 by Paul Pomes and the University of Illinois Board
# of Trustees

.SUFFIXES:	.c,v .h,v .y,v

CC     = cc
CO     = co
RM     = /bin/rm -f

# define MALLOC_IS_VOIDSTAR if your system's malloc is declared as a (void *)
# otherwise, it is assumed to be a (char *). a "mall_t" is typedef'ed in
# mnemconf.h and mnemosyne.h to implement this.
DEFS   = -DMALLOC_IS_VOIDSTAR
CFLAGS = ${DEFS} -O
FFLAGS = ${DEFS}
PFLAGS = ${DEFS}
LFLAGS = 
YFLAGS = 
LDFLAGS= 
LIBS   = 

BIN= /usr/local/bin
ETC= /usr/local/etc
LIB= /usr/local/lib
MAN= /usr/local/src/man
DOCDIR = /usr/local/doc

HDRS   = mnemconf.h mnemosyne.h
SRCS   = mnemalyse.c mnemosyne.c mtest.c
OTHERS = Makefile README mnemalyse.1 mnemosyne.3
VERS   = 

.c,v.o:
	${CO} -q $*.c
	${CC} ${CFLAGS} -c $*.c
	${RM} $*.c

.c,v.c:
	${CO} -q $*.c

.h,v.h:
	${CO} -q $*.h

.y,v.y:
	${CO} -q $*.y

all: mnemalyse libmnem.a

mnemalyse: mnemalyse.o
	cc ${CFLAGS} ${LDFLAGS} -o $@ mnemalyse.o

libmnem.a: mnemosyne.o
	ar rcv $@ mnemosyne.o
	ranlib $@

mtest: mtest.o libmnem.a
	cc ${CFLAGS} ${LDFLAGS} -o $@ mtest.o libmnem.a

runmtest: all mtest
	@echo "running memory waster"
	mtest
	@echo "press return for symbol list"; read ff
	@cat mnem.syms
	@echo "press return for waste analysis"; read ff
	mnemalyse

install:	all
	install -s mnemalyse ${BIN}
	install -m 644 libmnem.a ${LIB}
	install -c mnemosyne.h /usr/include
	ranlib ${LIB}/libmnem.a
	install -c -m 444 mnemalyse.1 ${MAN}/man1
	install -c -m 444 mnemosyne.3 ${MAN}/man3

lint:	${HDRS} ${SRCS}
	lint -habx ${DEFS} ${SRCS}

shar:	${HDRS} ${SRCS} ${OTHERS}
	shar ${HDRS} ${SRCS} ${OTHERS} > xbin.shar

tags:	${HDRS} ${SRCS}
	ctags ${HDRS} ${SRCS}

clean:
	@echo "Removing object and junk files."
	rm -f mnemalyse libmnem.a mtest *.o core a.out make.log lint.out \
		mnem.dat mnem.syms Makefile.bak mnemosyne.shar

clobber:
	@echo "Removing read-only source files that have RCS parents."
	@echo "Error code 1 indicates last file in SRCS list was not removed."
	@echo "(Which is OK)"
	make clean
	-if [ `whoami` != root ]; then \
	   for i in ${HDRS} ${SRCS}; do \
	      if [ ! -w $$i ]; then \
	         ( if   [ -f $$i,v ];     then rm -f $$i; \
	           elif [ -f RCS/$$i,v ]; then rm -f $$i; \
	         fi ); \
	      else echo $$i "writeable, not removed"; \
	      fi; \
	   done; \
	else echo "Running \"make clobber\" as root will zap ALL SRCS,\
		RCS'ed or not (not done)."; \
	fi

compress:
	make clean
	@echo "Compressing source and RCS files."
	find . -size +2 \( -name \*.h -o -name \*.c -o -name \*.f \
		-o -name \*.p -o -name \*.l -o -name \*.y -o -name \*,v \) \
		-exec compress {} \;

uncompress:
	uncompressdir .

# RCS stuff

ci:		${HDRS} ${SRCS}
		-ci $?
		@touch ci

coall:
		co -l ${HDRS} ${SRCS}

update:
		ci -sDist -u -f${VERS} ${HDRS} ${SRCS}
		@touch ci

depend:
	mkdep ${DEFS} ${SRCS}

# DO NOT DELETE THIS LINE -- make depend uses it

xbin.o: /usr/include/stdio.h
xbin.o: /usr/include/assert.h
xbin.o: /usr/include/sys/types.h
xbin.o: /usr/include/sys/stat.h
xbin.o: /usr/include/ctype.h
xbin.o: /usr/include/signal.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
