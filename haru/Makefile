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

HEADERS =../h/haru/config.h \
	../h/haru/hpdf.h \
	../h/haru/hpdf_annotation.h \
	../h/haru/hpdf_catalog.h \
	../h/haru/hpdf_conf.h \
	../h/haru/hpdf_config.h \
	../h/haru/hpdf_consts.h \
	../h/haru/hpdf_destination.h \
	../h/haru/hpdf_doc.h \
	../h/haru/hpdf_encoder.h \
	../h/haru/hpdf_encrypt.h \
	../h/haru/hpdf_encryptdict.h \
	../h/haru/hpdf_error.h \
	../h/haru/hpdf_ext_gstate.h \
	../h/haru/hpdf_font.h \
	../h/haru/hpdf_fontdef.h \
	../h/haru/hpdf_gstate.h \
	../h/haru/hpdf_image.h \
	../h/haru/hpdf_info.h \
	../h/haru/hpdf_list.h \
	../h/haru/hpdf_mmgr.h \
	../h/haru/hpdf_namedict.h \
	../h/haru/hpdf_objects.h \
	../h/haru/hpdf_outline.h \
	../h/haru/hpdf_page_label.h \
	../h/haru/hpdf_pages.h \
	../h/haru/hpdf_pdfa.h \
	../h/haru/hpdf_streams.h \
	../h/haru/hpdf_types.h \
	../h/haru/hpdf_u3d.h \
	../h/haru/hpdf_utils.h \
	../h/haru/hpdf_version.h \


OBJS = hpdf_annotation.o \
	hpdf_array.o \
	hpdf_binary.o \
	hpdf_boolean.o \
	hpdf_catalog.o \
	hpdf_destination.o \
	hpdf_dict.o \
	hpdf_doc.o \
	hpdf_doc_png.o \
	hpdf_encoder.o \
	hpdf_encoder_cns.o \
	hpdf_encoder_cnt.o \
	hpdf_encoder_jp.o \
	hpdf_encoder_kr.o \
	hpdf_encrypt.o \
	hpdf_encryptdict.o \
	hpdf_error.o \
	hpdf_ext_gstate.o \
	hpdf_font.o \
	hpdf_font_cid.o \
	hpdf_font_tt.o \
	hpdf_font_type1.o \
	hpdf_fontdef.o \
	hpdf_fontdef_base14.o \
	hpdf_fontdef_cid.o \
	hpdf_fontdef_cns.o \
	hpdf_fontdef_cnt.o \
	hpdf_fontdef_jp.o \
	hpdf_fontdef_kr.o \
	hpdf_fontdef_tt.o \
	hpdf_fontdef_type1.o \
	hpdf_gstate.o \
	hpdf_image.o \
	hpdf_image_png.o \
	hpdf_info.o \
	hpdf_list.o \
	hpdf_mmgr.o \
	hpdf_name.o \
	hpdf_namedict.o \
	hpdf_null.o \
	hpdf_number.o \
	hpdf_objects.o \
	hpdf_outline.o \
	hpdf_page_label.o \
	hpdf_page_operator.o \
	hpdf_pages.o \
	hpdf_pdfa.o \
	hpdf_real.o \
	hpdf_streams.o \
	hpdf_string.o \
	hpdf_u3d.o \
	hpdf_utils.o \
	hpdf_xref.o 


.SUFFIXES:  .o .c

all: $(LIBTARGET) 

$(OBJS): $(HEADERS)

$(LIBNAME): $(OBJS)
	ar rc $(LIBNAME) $(OBJS)
	ranlib $(LIBNAME)


clean:
	rm -f *.o *.a


.o.o:
	$(CC) -o $@ $(CFLAGS) -c $*.c

