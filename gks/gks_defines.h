/*
 *		Copyright IBM Corporation 1989
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of IBM not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 *
 * University of Illinois at Urbana-Champaign
 * Department of Computer Science
 * 1304 W. Springfield Ave.
 * Urbana, IL	61801
 *
 * (C) Copyright 1987, 1988 by The University of Illinois Board of Trustees.
 * All rights reserved.
 *
 * Tool: X 11 Graphical Kernel System
 * Author: Gregory Scott Rogers
 * Author: Sung Hsien Ching Kelvin
 * Author: Yu Pan
 * 
 * Define file for : Implementation dependent constants
 *
 * This header-file depends upon header-file "xgks.h".
 *
 * $Id: gks_defines.h,v 2.6 2000/08/01 16:38:00 steve Exp $
 * $__Header$
 */

#ifndef  LOCAL_H
#define  LOCAL_H

typedef void       *voidp;

/*
 * Macros for insuring the proper compilation of macros which contain
 * conditional statements (the trick is to generate a syntax error if and when
 * the user forgets the final semicolon):
 */
#ifdef lint
    extern int		GKS_ZERO;
#else
#   define GKS_ZERO	0
#endif
#define GKS_STMT(stuff)	do {stuff} while (GKS_ZERO)

/*
 * strlen() was defined as STRLEN so a NULL ptr is allowed AIX port
 */
#define STRLEN(a)       (((a) == NULL) ? 0 : strlen(a))

/*
 * strcmp() was defined as STRCMP so a NULL ptr is allowed AIX port.
 * Note: STRCMP given a NULL pointer will return 1
 */
#define STRCMP(a,b)	(((a) != NULL && (b) != NULL) ? strcmp((a),(b)) : 1)

/*
 * strcpy() was defined as STRCPY so a NULL ptr is allowed AIX port
 */
#define STRCPY(d,s)	GKS_STMT(if ((s) != NULL) (void)strcpy((d),(s));)

#define GKSERROR(c,n,m)	GKS_STMT(if (c) { (void)gerrorhand( (n), (m), \
			    xgks_state.gks_err_file); return( (n) ); })

/*
 *	Manage memory safely (i.e. ignore signals while doing it).
 *
 *	This is a kludge since the sources should have been
 *	written with umalloc(), urealloc(), and ufree() rather than malloc(),
 *	realloc(), and free().
 *	Changing all occurrences, however, would be painful -- especially for
 *	"just a patch".  Hopefully, we'll remember to do this before the next
 *	release.
 */
#ifndef lint
#   define free(ptr)		ufree(ptr)
#   define malloc(size)		umalloc(size)
#   define realloc(ptr, size)	urealloc(ptr, size)
    extern void		ufree		PROTO((voidp ptr));
    extern voidp	umalloc		PROTO((size_t size));
    extern voidp	urealloc	PROTO((voidp ptr, size_t size));
#endif

/*
 *	Free allocated memory.
 */
#define GKS_FREE(p)	free((voidp)p)

#define MAX_SEG		32767		/* Largest segment name for 16-bit */
					/* integers */
#define MAX_OPEN_WS	10		/* Hardwired limits */
#define MAX_ACTIVE_WS	10
#define	MAX_ASSOC_WS	10

#define MAX_BUNDL_TBL	21  		/* Legal bundle table range 1-20 */
					/* entry 0 will be initialized */
					/* however */

/*
 * Added following 4 defines for use w/ predefined representations
 */
#define PDF_FILL_BNDLS	5		/* # of predef fill rep bundles */
#define PDF_LINE_BNDLS	5		/* # of predef line rep bundles */
#define PDF_MARK_BNDLS	5		/* # of predef marker rep bundles */
#define PDF_TEXT_BNDLS	6		/* # of predef text rep bundles */

#define DEFINED_FONT_TYPE 9		/* number of defined font type XGKS */
					/* system */

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

#define INVALID		-1

#define OK		0

/*
 * Metafile error flag values
 */
#define	METAFILE_OK	0
#define	MF_ITEM_ERR	163
#define	MF_DATA_ERR	165
#define	MF_FILE_ERR	1000

/*
 * A separate-palette entry:
 */
typedef struct XcToGKSMap {
    unsigned	    red;		/* red intensity value */
    unsigned	    green;		/* green intensity value */
    unsigned	    blue;		/* blue intensity value */
}		XcRGB;

/*
 * GKS <-> X color-mapping (bidirectional):
 */
typedef struct {
    unsigned	    length;		/* length of pixel/index maps */
    long           *Xpixels;		/* GKS color index to X pixel value */
    Gint           *GKSindexes;		/* X pixel value to GKS color index */
}		XcMap;

/*
 * Colour indexes mandated by the GKS standard:
 */
#define GKS_BACKGROUND_INDEX	0
#define	GKS_FOREGROUND_INDEX	1

/*
 * PID - Primitive identifier
 */
typedef enum {
	PLINE,
	PMARK,
	TEXT,
	FILL_AREA,
	CELL_ARRAY,
	CLIP_REC,
	XGKS_MESG,
	GDP
} PID;

/*
 * ERROR_ST - GKS error state
 */
typedef enum {
	ERR_ON,
	ERR_OFF
} ERROR_ST;


typedef enum {
	VAL_HORIZ,
	VAL_VERT
} VAL_AXIS;

typedef struct seg_struct {		/* set of segments associated ws a ws */
	Gint	seg;			/* segment name */
	struct seg_struct *next;
} WS_SEG_LIST;

typedef struct {
	Gfloat	height;			/* Current character height */
	Gpoint	up;			/* Current character up vector */
	Gfloat	chwidth;		/* Current character width */
	Gpoint	base;			/* Current character base vector */
	Gtxpath	path;			/* Current text path */
	Gtxalign  align;		/* Crrent text alignment */
} CHATTR;				/* Character attributes */

typedef struct {
	Gpoint	widthvec;		/* Current pattern width vector */
	Gpoint	heightvec;		/* Current pattern height vector */
	Gpoint	ptp;			/* Current pattern reference point */
} PTATTR;				/* Pattern attributes */

#endif
