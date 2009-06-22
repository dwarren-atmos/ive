# makefile for Haru Free PDFLibrary II (Libharu)
# Copyright (C) 1999-2006 Takeshi Kanno
# For conditions of distribution and use, see copyright notice in hpdf.h
#
# To compile, type:
#   ./configure; make
# If you wish to build zlib as a shared library, use: ./configure -s
# To install /usr/local/lib/libhpdf.* and /usr/local/include/hpdf.h, type:
#    make install
# To install in $HOME instead of /usr/local, use:
#    make install prefix=$HOME

LIBNAME=libhpdf.a
LIBTARGET=libhpdf.a
CFLAGS_EXE=-o 
LDFLAGS=-L. -lpng -lz -lm
DEFNAME=
RESNAME=

OBJS = \
	hpdf_utils.o \
	hpdf_error.o \
	hpdf_mmgr.o \
	hpdf_list.o \
	hpdf_streams.o \
	hpdf_objects.o \
	hpdf_null.o \
	hpdf_boolean.o \
	hpdf_number.o \
	hpdf_real.o \
	hpdf_name.o \
	hpdf_array.o \
	hpdf_dict.o \
	hpdf_xref.o \
	hpdf_encoder.o \
	hpdf_string.o \
	hpdf_binary.o \
	hpdf_encrypt.o \
	hpdf_encryptdict.o \
	hpdf_fontdef.o \
	hpdf_fontdef_tt.o \
	hpdf_fontdef_type1.o \
	hpdf_fontdef_base14.o \
	hpdf_fontdef_cid.o \
	hpdf_font.o \
	hpdf_font_type1.o \
	hpdf_font_tt.o \
	hpdf_font_cid.o \
	hpdf_doc.o \
	hpdf_info.o \
	hpdf_catalog.o \
	hpdf_page_label.o\
	hpdf_gstate.o \
	hpdf_pages.o \
	hpdf_page_operator.o \
	hpdf_destination.o \
	hpdf_annotation.o \
	hpdf_outline.o \
	hpdf_image.o \
	hpdf_encoder_jp.o \
	hpdf_encoder_kr.o \
	hpdf_encoder_cns.o \
	hpdf_encoder_cnt.o \
	hpdf_fontdef_jp.o \
	hpdf_fontdef_kr.o \
	hpdf_fontdef_cns.o \
	hpdf_fontdef_cnt.o \
	hpdf_image_png.o \
	hpdf_doc_png.o \
	hpdf_ext_gstate.o \

.SUFFIXES:  .c .o

all: $(LIBTARGET)

$(LIBNAME): $(OBJS)
	ar rc $(LIBNAME) $(OBJS)
	ranlib $(LIBNAME)


clean:
	rm -f *.o *.a


.c.o:
	$(CC) -o $@ $(CFLAGS) -c $*.c

