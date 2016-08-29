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
 * segments.c - functions for handling workstation segments.
 *		gcreateseg()
 *		gcloseseg()
 *		grenameseg()
 *		gdelseg()
 *		gdelsegws()
 *		gsetsegattr()
 *		  Setting all segment attributes :
n *		  - transformation, visibility, highlight, prioirty,
 *		    detectability
 *		gsetpickid()
 *		gredrawsegws()
 *		XgksInSeg ()
 *		gassocsegws()
 *		gcopysegws()
 *		ginsertseg()
 *
 *		i_dynamic_mod_seg_attr()
 *		i_name_open_seg()
 *		i_seg_attr()
 *		i_set_assoc_ws()
 *		i_set_seg_names()
 *		i_set_seg_names_ws()
 *		i_num_seg_priorities()
 *
 * utility functions:
 *		XgksInitGksSegments()
 *		XgksAppendSegPrimi ()
 *		XgksAppendSegClip ()
 *		XgksSetHighLight ()
 *  		XgksDelAssocWs ()
 *		XgksClearWs ()
 *		XgksUpdateWsSegList ()
 *		XgksReDrawSeg ()
 *		XgksShowPick ()
 *		XgksFindPickSeg ()
 *		XgksIrgNec ()
 *		XgksDeleteAllSeg ()
 *		XgksReDrawAssocWs ()
 *		XgksReDrawSegWs ()
 *		XgksDrawSegToWs ()
 *		XgksOutputSeg ()
 *		XgksSegPrimiTran ()
 *
 *
 * for segment state list : there's only one segment state list, it's mantained
 * and defined only in this file. manipulating routines are :
 *	      	XgksInstallSeg ();
 *		XgksDeleteSeg (); this list is implemented as a hash table
 *		XgksNewSeg ();
 *		XgksFindSeg ();
 *
 * for workstation segment list : there're one of this lists associated with
 * each ws.  List of segments are linked by the order of their creation.
 * Manipulating routines are :
 *		XgksInsertWsSeg ();
 *		XgksDeleteWsSeg ();
 *		XgksRenameWsSeg ();
 *		XgksNewWsSeg ();
 *
 *
 * NOTE : one should never, NEVER use the segment state list to do redraw !,
 * the ws->seglist is there for this purpose. segment state list is a good
 * and convenient way of storing and retrieving segment attributes.
 */

/*LINTLIBRARY*/

#include "udposix.h"
#include <stddef.h>		/* for "size_t" */
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#include "gks_implem.h"
#include<hpdf.h>

extern void xXgksInqTextExtent(),XgksMiniMax(),
  XgksDeletePrimi(),xXgksClearWs(),XgksUnpendPendingTrans(),
  XgksIDevDisable(), xXgksHighLight(),XgksIDevEnable(),XgksDrawToWs();
extern void xXgksUpdateClip(),gerrorhand(),
  XgksWsWinInterset(),XgksInsertPrimi(),XgksOutputToWs(),
  XgksAppendWsPrimi(),XgksUpdatePrimiBound();


#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: segments.c,v 1.13 2007-02-26 23:48:29 warren Exp $";
#endif

#define MAXNUM 1.0e+20

/* definition for segment state list */
typedef struct seg_st {
    Gint            assoc_ws[MAX_ASSOC_WS];
    /* Set of associated workstations */
    Gsegattr        segattr;		/* Segment attribute including
					 * segment name */
    OUT_PRIMI       primi_list;		/* First primitive is always a
					 * CLIP_REC */
    OUT_PRIMI      *primi_insert_pt;
    /*
     * Primitive list manger need this for efficient insertion
     */
    Glimit          bound;		/* Bounding box for all primitive */
    Gint            text_present;	/* Flag indicating if there's text
					 * primitive in segments */
    struct seg_st  *seg_next;		/* pointer to next */
}              *SEG_STATE_PTR, SEG_STATE_ENTRY;

#define SHSIZE		128
#define SHASH(segname)	(segname & (SHSIZE-1))

/*
 * Macro --- SegTrans (ndcpt, tranpt, matric)
 *
 * Gpoint *ndcpt;		the normalized device coordinate point passed in
 * Gpoint *transpt		the transformed ndc point return
 * Gfloat matrix[2][3]		the transformation matirx
 *
 * WARNING: This macro is not CAPITALIZED!
 */
#define SegTrans(ndcpt, transpt, matrix) { \
    (transpt)->x = (ndcpt)->x*(matrix[0][0]) + (ndcpt)->y*(matrix[1][0]) \
		   + (matrix[0][2]); \
    (transpt)->y = (ndcpt)->x*(matrix[0][1]) + (ndcpt)->y*(matrix[1][1]) \
		   + (matrix[1][2]); \
}

#define MAX(a,b)	((a)>(b) ? (a) : (b))
#define MIN(a,b)	((a)>(b) ? (b) : (a))

#define NOT_WISS(ws_id)	((ws_id) != xgks_state.wiss_id)

/* Hash table where segment state information are stored */
static SEG_STATE_PTR segtable[SHSIZE];

OUT_PRIMI      *XgksDuplicatePrimi();
static void    psHeader();
static FILE *fp; /*file for postscript*/
static void XgksClearWs();

extern struct {
int isopen;
}is_eps_file_open_;

static HPDF_UINT16 PDFDASH[] = {3};
static HPDF_UINT16 PDFDOT[] = {1,1,1,1};
static HPDF_UINT16 PDFDOTDASH[] = {1,1,3,1};

XgksSetLineAttrMo(ws, lnattr)
    WS_STATE_PTR    ws;
    Glnattr        *lnattr;
{
    XgksMoSetGraphicAttrOnWs(ws, 21, lnattr->line);
    XgksMoSetGraphicAttrOnWs(ws, 22, lnattr->bundl.type);
    XgksMoSetGraphicSizeOnWs(ws, 23, lnattr->bundl.width);
    XgksMoSetGraphicAttrOnWs(ws, 24, lnattr->bundl.colour);
}


XgksSetMarkAttrMo(ws, mkattr)
    WS_STATE_PTR    ws;
    Gmkattr        *mkattr;
{
    XgksMoSetGraphicAttrOnWs(ws, 25, mkattr->mark);
    XgksMoSetGraphicAttrOnWs(ws, 26, mkattr->bundl.type);
    XgksMoSetGraphicSizeOnWs(ws, 27, mkattr->bundl.size);
    XgksMoSetGraphicAttrOnWs(ws, 28, mkattr->bundl.colour);
}


XgksSetTextAttrMo(ws, txattr, chattr)
    WS_STATE_PTR    ws;
    Gtxattr        *txattr;
    CHATTR         *chattr;
{
    CHATTR          tmp;

    XgksMoSetGraphicAttrOnWs(ws, 29, txattr->text);
    XgksMoSetTextFPOnWs(ws, &(txattr->bundl.fp));
    XgksMoSetGraphicSizeOnWs(ws, 31, txattr->bundl.ch_exp);
    XgksMoSetGraphicSizeOnWs(ws, 32, txattr->bundl.space);
    XgksMoSetGraphicAttrOnWs(ws, 33, txattr->bundl.colour);

    tmp = xgks_state.gks_chattr;
    xgks_state.gks_chattr = *chattr;
    XgksMoSetTextPathOnWs(ws, chattr->path);
    XgksMoSetTextAlignOnWs(ws, &(chattr->align));
    xgks_state.gks_chattr = tmp;
}


XgksSetFillPatAttrMo(ws, flattr, ptattr)
    WS_STATE_PTR    ws;
    Gflattr        *flattr;
 /* ARGSUSED */
    PTATTR         *ptattr;
{
    XgksMoSetGraphicAttrOnWs(ws, 37, flattr->fill);
    XgksMoSetFillIntStyleOnWs(ws, flattr->bundl.inter);
    XgksMoSetGraphicAttrOnWs(ws, 39, flattr->bundl.style);
    XgksMoSetGraphicAttrOnWs(ws, 40, flattr->bundl.colour);
    /* 41 & 42 not supported (patterns) */
}

/*
 * SEG_STATE_PTR
 * XgksFindSeg (name)
 * Gint name;		 - the target segment name;
 *
 * Tries to find the <name> segment in the segment table, if found return a 
 * pointer to to the segment state, else return NULL
 */
    static SEG_STATE_PTR 
XgksFindSeg(name)
    Gint            name;
{
    SEG_STATE_PTR   next;

    next = segtable[SHASH(name)];
    while (next != NULL) {
	if (next->segattr.seg == name)
	    return next;
	next = next->seg_next;
    }
    return NULL;
}
/*
 * XgksSegPrimiTran (primi, matrix)	build a transformed ndc primitive
 *					structure and return a pointer to the
 *					newly constructed primitive
 */
    static OUT_PRIMI*
XgksSegPrimiTran(primi, matrix)
    OUT_PRIMI      *primi;
    Gfloat          matrix[2][3];
{
    Gint            cnt, num_pts;
    Gpoint         *ndc_pt, up_pt, base_pt;
    static OUT_PRIMI trans_primi;	/* transformed primi */
    static Gpoint  *trans_pt = NULL;
    static Gchar   *string = NULL;
    static Gint    *colour = NULL;

    trans_primi = *primi;

#define ALLOC_CHECK(c)	{\
	if (c) {\
	    (void)gerrorhand(300, errXgksSegPrimiTran, \
			     xgks_state.gks_err_file);\
	    return NULL;\
	}\
    }

    if (trans_pt != NULL) {
	ufree((voidp)trans_pt);
	trans_pt = NULL;
    }
    if (string != NULL) {
	ufree((voidp)string);
	string = NULL;
    }
    if (colour != NULL) {
	ufree((voidp)colour);
	colour = NULL;
    }
    switch (primi->pid) {
    case PLINE:
	num_pts = trans_primi.primi.pline.num_pts = primi->primi.pline.num_pts;
	ALLOC_CHECK((trans_pt = (Gpoint *) malloc((size_t) (num_pts * 
		    sizeof(Gpoint)))) == NULL);
	ndc_pt = primi->primi.pline.pts;
	for (cnt = 0; cnt < num_pts; cnt++) {
	    SegTrans(ndc_pt, &(trans_pt[cnt]), matrix);
	    ndc_pt++;
	}
	trans_primi.primi.pline.pts = trans_pt;
	break;
    case PMARK:
	num_pts = trans_primi.primi.pmark.num_pts = primi->primi.pmark.num_pts;
	ALLOC_CHECK((trans_pt = (Gpoint *) malloc((size_t) (num_pts *
		    sizeof(Gpoint)))) == NULL);
	ndc_pt = primi->primi.pmark.location;
	for (cnt = 0; cnt < num_pts; cnt++) {
	    SegTrans(ndc_pt, &(trans_pt[cnt]), matrix);
	    ndc_pt++;
	}
	trans_primi.primi.pmark.location = trans_pt;
	break;
    case FILL_AREA:
	num_pts = trans_primi.primi.fill_area.num_pts = 
		  primi->primi.fill_area.num_pts;
	ALLOC_CHECK((trans_pt = (Gpoint *) malloc((size_t) (num_pts *
		    sizeof(Gpoint)))) == NULL);
	ndc_pt = primi->primi.fill_area.pts;
	for (cnt = 0; cnt < num_pts; cnt++) {
	    SegTrans(ndc_pt, &(trans_pt[cnt]), matrix);
	    ndc_pt++;
	}
	trans_primi.primi.fill_area.pts = trans_pt;
	break;
    case CLIP_REC:
	trans_primi.primi.clip.rec = primi->primi.clip.rec;
	trans_primi.primi.clip.segment = primi->primi.clip.segment;
	break;
    case TEXT:
	ALLOC_CHECK((trans_pt = (Gpoint *) malloc(sizeof(Gpoint)))
		    == NULL);

	/* SegTran on starting location of string */
	ndc_pt = primi->primi.text.location;
	SegTrans(ndc_pt, trans_pt, matrix);
	trans_primi.primi.text.location = trans_pt;

	up_pt = primi->primi.text.up_vec;
	base_pt = primi->primi.text.base_vec;
	/*
	 * SegTran on up/base vector of string, NO TRANSLATION can be done on
	 * vectors
	 */
	SegTrans(&up_pt, &(trans_primi.primi.text.up_vec), matrix);
	trans_primi.primi.text.up_vec.x -= matrix[0][2];
	trans_primi.primi.text.up_vec.y -= matrix[1][2];
	SegTrans(&base_pt, &(trans_primi.primi.text.base_vec), matrix);
	trans_primi.primi.text.base_vec.x -= matrix[0][2];
	trans_primi.primi.text.base_vec.y -= matrix[1][2];

	ALLOC_CHECK((string = (Gchar *) malloc((size_t)
	    (STRLEN(primi->primi.text.string) + 1))) == NULL);
	STRCPY(string, primi->primi.text.string);
	trans_primi.primi.text.string = string;
	break;

    case CELL_ARRAY:
	cnt = primi->primi.cell_array.rowsize * primi->primi.cell_array.dim.y;
	ALLOC_CHECK((colour = (Gint *) malloc((size_t) (cnt * sizeof(Gint))))
		    == NULL);
	cnt--;
	while (cnt >= 0) {
	    trans_primi.primi.cell_array.colour[cnt] =
		 primi->primi.cell_array.colour[cnt];
	    cnt--;
	}
	SegTrans(&(primi->primi.cell_array.ll),
		 &(trans_primi.primi.cell_array.ll), matrix);
	SegTrans(&(primi->primi.cell_array.lr),
		 &(trans_primi.primi.cell_array.lr), matrix);
	SegTrans(&(primi->primi.cell_array.ur),
		 &(trans_primi.primi.cell_array.ur), matrix);
	SegTrans(&(primi->primi.cell_array.ul),
		 &(trans_primi.primi.cell_array.ul), matrix);
	trans_primi.primi.cell_array.dim = primi->primi.cell_array.dim;
	trans_primi.primi.cell_array.rowsize = primi->primi.cell_array.rowsize;
	break;
    case GDP:
	break;
    default:
	break;
    }
    return &trans_primi;
}
/*
 * XgksProcessLocalBound -- if there's text-primitive in segment than update
 * seg->bound into localbound, else simply return seg->bound
 */
    static void
XgksProcessLocalBound(ws, seg, localbound)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
    Glimit         *localbound;
{
    OUT_PRIMI      *primi;
    Gpoint          ndc_points[5];

    *localbound = seg->bound;

    if (seg->text_present == FALSE)
	return;

    primi = seg->primi_list.next;
    while (primi != NULL) {
	if (primi->pid == TEXT) {
	    (void) xXgksInqTextExtent(ws, &(primi->primi.text), ndc_points);
	    XgksMiniMax(localbound, &(ndc_points[1]));
	    XgksMiniMax(localbound, &(ndc_points[2]));
	    XgksMiniMax(localbound, &(ndc_points[3]));
	    XgksMiniMax(localbound, &(ndc_points[4]));
	}
	primi = primi->next;
    }
}

/*
 * XgksSetHighLight(ws, seg)	setting/unsetting segment highlight on
 *				specified workstations.
 */
    static 
XgksSetHighLight(ws, seg)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
{
    Gint            i;
    Gpoint          ndc[4], trans[5];
    Gfloat          xfact, yfact;
    Glimit          localbound;

    XgksProcessLocalBound(ws, seg, &localbound);

    /* figuring out the stretching factor for the bounding box */
    xfact = 0.01 * (ws->wsti.current.w.xmax - ws->wsti.current.w.xmin);
    yfact = 0.01 * (ws->wsti.current.w.ymax - ws->wsti.current.w.ymin);

    /* now ndc values are stretched bounding box */
    ndc[0].x = localbound.xmin - xfact;
    ndc[0].y = localbound.ymin - yfact;
    ndc[1].x = localbound.xmin - xfact;
    ndc[1].y = localbound.ymax + yfact;
    ndc[2].x = localbound.xmax + xfact;
    ndc[2].y = localbound.ymax + yfact;
    ndc[3].x = localbound.xmax + xfact;
    ndc[3].y = localbound.ymin - yfact;

    /* pass the bounding box through segtran */
    for (i = 0; i < 4; i++)
	SegTrans(&(ndc[i]), &(trans[i]), seg->segattr.segtran);

    trans[4] = trans[0];			/* for xpolyline to do
						 * drawing */

    XgksIDevDisable(ws);
    (void) xXgksHighLight(ws, trans);
    XgksIDevEnable(ws);
}
/*
 * XgksUpdateWsSegList (wsg)  -- re-insert the whole ws->seglist according to
 *				 each segment's priority
 *
 *	Reorder list from low priority to high priority
 *
 *	The way priority works:
 *
 * 	During redraw ... just redraw blindly from the front, BIGGEST will
 *	get drawn last, thus highest prority.
 */
    void
XgksUpdateWsSegList(ws)
    WS_STATE_PTR    ws;
{
    WS_SEG_LIST    *old, *cnt, *pre;
    SEG_STATE_PTR   workSeg, oldSeg;
    void XgksCleanUpWsSegList();
    /* segment list is clean again */
    ws->seg_list_dirty = FALSE;

    /* First clean up the list by deleting all segment with INVALID name */
    XgksCleanUpWsSegList(ws);

    /* Do the rearrangment by construct the list */

    /*
     * if there's only one segment in the list then we are done
     */
    if (ws->seglist == ws->seg_insertpt)
	return;

    old = ws->seglist->next;
    ws->seg_insertpt = ws->seglist;
    ws->seg_insertpt->next = NULL;

    while (old != NULL) {
	oldSeg = XgksFindSeg(old->seg);
	cnt = ws->seglist;
	workSeg = XgksFindSeg(cnt->seg);
	pre = NULL;
	while ((workSeg->segattr.pri <= oldSeg->segattr.pri) &&
	       (cnt != NULL)) {
	    pre = cnt;
	    cnt = cnt->next;
	    if (cnt != NULL)
		workSeg = XgksFindSeg(cnt->seg);
	    else
		break;
	}
	if (cnt == NULL) {			/* At the end of the
						 * ws->seglist */
	    pre->next = old;
	    ws->seg_insertpt = old;
	    old = old->next;
	    ws->seg_insertpt->next = NULL;
	} else if (pre == NULL) {		/* Begingin of the
						 * ws->seglist */
	    ws->seglist = old;
	    old = old->next;
	    ws->seglist->next = cnt;
	} else {
	    pre->next = old;
	    old = old->next;
	    pre->next->next = cnt;
	}
    }
}
/*
 * XgksDrawSegToWs(ws) 		- draw out all ws assoc segments to ws
 *
 * Routine will do UPDATE_SEG_CNT for every segment!!
 * Routine will also Disable and Enable before and after segment redraws
 */
    void
XgksDrawSegToWs(ws)
    WS_STATE_PTR    ws;
{
    SEG_STATE_PTR   seg;
    WS_SEG_LIST    *seglist;
    int XgksReDrawSeg();
    XgksIDevDisable(ws);
    seglist = ws->seglist;
    (void)xXgksUpdateTrans(ws);
    while (seglist != NULL) {
	if (seglist->seg != INVALID) {
	    seg = XgksFindSeg(seglist->seg);
	    UPDATE_SEG_CNT(ws->primi_insert_pt);
	    if (seg->segattr.vis == GVISIBLE)
		XgksReDrawSeg(ws, seg->segattr.seg);
	}
	seglist = seglist->next;
    }
    XgksIDevEnable(ws);
}

/*
 * Do gredrawsegws() - with no error checking and no MO output
 *
 */
XgksReDrawSegWs(ws)
    WS_STATE_PTR    ws;
{
  XgksIDevDisable(ws);

  XgksClearWs(ws);
    /*
     * Now workstaiton had been cleared, update segment list (according to
     * priority) and redraw all segments
     */
    if (ws->seg_list_dirty == TRUE)
	XgksUpdateWsSegList(ws);
    XgksDrawSegToWs(ws);			/* Now redraw all segments */

    XgksIDevEnable(ws);
}

/*
 * XgksReDrawSeg(ws, seg_id)	This is the x-initiated re-draw segment path,
 *				this function should only be called be re-draw
 *				initialted.
 */
XgksReDrawSeg(ws, seg_id)
    WS_STATE_PTR    ws;
    Gint            seg_id;
{
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi;
    Glimit          tmp_clip;
    void XgksReDrawWs();
    if (seg_id != INVALID) {

	seg = XgksFindSeg(seg_id);
	if (seg->segattr.vis == GINVISIBLE)
	    return;
	primi = &(seg->primi_list);
	tmp_clip = ws->clip;			/* save the current clip
						 * region */
	while (primi != NULL) {
	    XgksReDrawWs(ws, XgksSegPrimiTran(primi, seg->segattr.segtran));
	    primi = primi->next;
	}
	ws->clip = tmp_clip;			/* restore clip ws-clip
						 * region */
	xXgksUpdateClip(ws);
	if (seg->segattr.hilight == GHIGHLIGHTED)
	    XgksSetHighLight(ws, seg);
    }
}


    static void
XgksSegAttrMo(ws, seg)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
{
    XgksMoSetSegTransOnWs(ws, seg->segattr.seg, seg->segattr.segtran);
    XgksMoSetSegAttrOnWs(ws, seg->segattr.seg, 92,
			 (seg->segattr.vis == GVISIBLE ? 0 : 1));
    XgksMoSetSegAttrOnWs(ws, seg->segattr.seg, 93,
			 (seg->segattr.hilight == GNORMAL ? 0 : 1));
    XgksMoSetSegPriOnWs(ws, seg->segattr.seg, seg->segattr.pri);
    XgksMoSetSegAttrOnWs(ws, seg->segattr.seg, 95,
			 (seg->segattr.det == GUNDETECTABLE ? 0 : 1));
}


    static void
XgksOutPrimiToMo(ws, primi)
    WS_STATE_PTR    ws;
    OUT_PRIMI      *primi;
{
    switch (primi->pid) {
    case PLINE:
	XgksMoSetGraphicAttrOnWs(ws, 44, primi->pickid);
	XgksSetLineAttrMo(ws, &(primi->primi.pline.plnattr));
	XgksMoGraphicOutputToWs
	    (ws, 11, primi->primi.pline.num_pts, primi->primi.pline.pts);
	break;
    case PMARK:
	XgksMoSetGraphicAttrOnWs(ws, 44, primi->pickid);
	XgksSetMarkAttrMo(ws, &(primi->primi.pmark.mkattr));
	XgksMoGraphicOutputToWs
	    (ws, 12, primi->primi.pmark.num_pts, primi->primi.pmark.location);
	break;
    case TEXT:
	XgksMoSetGraphicAttrOnWs(ws, 44, primi->pickid);
	XgksSetTextAttrMo(ws, &(primi->primi.text.txattr),
			  &(primi->primi.text.chattr));
	XgksMoSetCharUpOnWs(ws, &(primi->primi.text.up_vec),
			    &(primi->primi.text.base_vec));
	XgksMoTextToWs(ws, primi->primi.text.location,
		       primi->primi.text.string);
	break;
    case FILL_AREA:
	XgksMoSetGraphicAttrOnWs(ws, 44, primi->pickid);
	XgksSetFillPatAttrMo(ws,
	&(primi->primi.fill_area.flattr), &(primi->primi.fill_area.ptattr));
	XgksMoGraphicOutputToWs(ws, 14, primi->primi.fill_area.num_pts,
			        primi->primi.fill_area.pts);
	break;
    case CELL_ARRAY:
	XgksMoSetGraphicAttrOnWs(ws, 44, primi->pickid);
	XgksMoCellArrayToWs
	    (ws, &(primi->primi.cell_array.ll), &(primi->primi.cell_array.ur),
	     &(primi->primi.cell_array.lr), primi->primi.cell_array.rowsize,
	     primi->primi.cell_array.colour, &(primi->primi.cell_array.dim));
	break;
    case GDP:					/* gdp */
    case CLIP_REC:
    case XGKS_MESG:
    default:
	break;
    }
}




    static void
XgksSegTransProcessMo(seg, matrix)
    SEG_STATE_PTR   seg;
    Gfloat          matrix[2][3];
{
    Gint            wscnt;
    OUT_PRIMI      *primi;

    for (wscnt = 0; wscnt < MAX_ACTIVE_WS; wscnt++) {
	if ((xgks_state.activews[wscnt].ws_id != INVALID) &&
		(xgks_state.activews[wscnt].ws->ewstype == MO)) {
	    XgksMoSetAsfOnWs(xgks_state.activews[wscnt].ws);	/* c1144 */
	    primi = seg->primi_list.next;
	    while (primi != NULL) {
		XgksOutPrimiToMo(xgks_state.activews[wscnt].ws,
				 XgksSegPrimiTran(primi, matrix));
		primi = primi->next;
	    }
	}
    }
    XgksMoSetClip(&xgks_state.cliprec.rec);
}


    static void
XgksSegCopyMo(ws, seg)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
{
    OUT_PRIMI      *primi = seg->primi_list.next;

    XgksMoSetPatRefOnWs(ws);
    XgksMoSetPatSizeOnWs(ws);
    XgksMoSetAsfOnWs(ws);
    while (primi != NULL) {
	XgksOutPrimiToMo(ws, XgksSegPrimiTran(primi, seg->segattr.segtran));
	primi = primi->next;
    }
    XgksMoSetClipOnWs(ws, &xgks_state.cliprec.rec);
}


    static void
XgksSegProcessMo(ws, seg)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
{
    OUT_PRIMI      *primi = seg->primi_list.next;

    XgksMoSetAsfOnWs(ws);
    while (primi != NULL) {
	XgksOutPrimiToMo(ws, primi);
	primi = primi->next;
    }
    XgksMoSetClipOnWs(ws, &xgks_state.cliprec.rec);
}


    static void
XgksRestoreMoGksStateOnWs(ws)
    WS_STATE_PTR    ws;
{
    if (ws->ws_id != INVALID && ws->ewstype == MO) {
	XgksSetLineAttrMo(ws, &(xgks_state.gks_lnattr));
	XgksSetMarkAttrMo(ws, &(xgks_state.gks_mkattr));
	XgksSetTextAttrMo(ws, &(xgks_state.gks_txattr),
			  &(xgks_state.gks_chattr));
	XgksMoSetCharUpOnWs(ws, (Gpoint *) NULL, (Gpoint *) NULL);
	XgksSetFillPatAttrMo(ws, &(xgks_state.gks_flattr),
			     &(xgks_state.gks_ptattr));
	XgksMoSetPatRefOnWs(ws);
	XgksMoSetPatSizeOnWs(ws);
    }
}


    static void
XgksRestoreMoGksState()
{
    int             cnt;

    for (cnt = 0; cnt < MAX_ACTIVE_WS; cnt++)
        if (xgks_state.activews[cnt].ws_id != INVALID &&
		xgks_state.activews[cnt].ws->ewstype == MO)
	    XgksRestoreMoGksStateOnWs(xgks_state.activews[cnt].ws);
}


    static void
XgksCheckSegAttr(old, new)
    Gsegattr       *old, *new;
{
    Gint            tranDiff, i;

    tranDiff = FALSE;

    for (i = 0; i < 3; i++)
	if ((old->segtran[0][i] != new->segtran[0][i]) ||
		(old->segtran[1][i] != new->segtran[1][i])) {
	    tranDiff = TRUE;
	    i = 3;
	}
    if (tranDiff == TRUE)
	XgksMoSetSegTrans(old->seg, new->segtran);

    if (old->vis != new->vis)
	XgksMoSetSegVis(old->seg, new->vis);

    if (old->hilight != new->hilight)
	XgksMoSetSegHiLight(old->seg, new->hilight);

    if (old->pri != new->pri)
	XgksMoSetSegPri(old->seg, new->pri);

    if (old->det != new->det)
	XgksMoSetSegDet(old->seg, new->det);
}




/*
 * float XgksDistPline(ws, seg, primi, pt) - Find the distance between pick
 *					     input point and a polyline
 *					     primitive
 * WS_STATE_PTR ws;		workstation state list pointer.
 * SEG_STATE_PTR seg;		segment state list pointer.
 * OUT_PRIMI *primi;		output primitive pointer.
 * Gpoint *pt;			pick input point.
 */
    static float
XgksDistPline(ws, seg, primi, pt)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi;
    Gpoint         *pt;
{
    PLINE_ST       *line;
    float           s, s1, s2, x, y;
    float           X, Y, x1, y_1, x2, y2;	/* "y_1" rather than "y1"
						 * because <math.h> might
						 * declare "extern double
						 * y1()". */
    float           DX, DY, DY1, DY2, mini, dist, width;

#ifdef notdef
    float           DD;

#endif
    Gpoint          pe, p;
    Gint            i;

    XPoint          xpe;


    mini = (float) MAXNUM;
    line = &(primi->primi.pline);
    x = pt->x;
    y = pt->y;
    for (i = 0; i < line->num_pts - 1; i++) {
	SegTrans(&(line->pts[i]), &p, seg->segattr.segtran);
	x1 = p.x;
	y_1 = p.y;
	SegTrans(&(line->pts[i + 1]), &p, seg->segattr.segtran);
	x2 = p.x;
	y2 = p.y;
	if (y_1 == y2) {
	    X = x;
	    Y = y_1;
	}
	 /* Horizontal line */ 
	else if (x1 == x2) {
	    X = x1;
	    Y = y;
	}
	 /* Vertical line */ 
	else {
	    s = (y_1 - y2) / (x1 - x2);
	    s1 = 1 / s;
	    s2 = s * s + 1.0;
	    X = (s * x1 - y_1 + y + s1 * x) * s / s2;
	    Y = (y_1 - y - s * x1 - s1 * x) / s2 + y + s1 * x;
	}
	/*
	 * The line (x,y)(X,Y) is perpendicular to (x1,y_1)(x2,y2)
	 * and intersects (x1,y_1)(x2,y2) at (X,Y)
	 * Note: (X,Y) is not necessarily between (x1,y_1) and (x2,y2).
	 * DX = Perpendicular distance
	 */
	DX = (float) sqrt((double) ((X - x) * (X - x) + (Y - y) * (Y - y)));

	/* DY = line length */
	DY = (float) sqrt((double) ((x1 - x2) * (x1 - x2) + (y_1 - y2) *
				    (y_1 - y2)));

	/* DY1 = distance from (X,Y) to (x1,y_1) endpoint */
	DY1 = (float) sqrt((double) ((X - x1) * (X - x1) + (Y - y_1) *
				     (Y - y_1)));

	/* DY2 = distance from (X,Y) to (x2,y2) endpoint */
	DY2 = (float) sqrt((double) ((X - x2) * (X - x2) + (Y - y2) *
				     (Y - y2)));

	if ((dist = DY1 + DY2) > DY)		/* (X,Y) is not between
						 * (x1,y_1)(x2,y2)  AUG */
	    dist = sqrt((DX + dist) * (DX + dist));
	else {
	    pe.x = DX;
	    pe.y = 0.0;
	    NdcToX(ws, &pe, &xpe);
	    if (xpe.x < 0)
		xpe.x = -xpe.x;
	    width = (line->plnattr.width == GINDIVIDUAL) ?
		(Gint) (line->plnattr.bundl.width) :
		(Gint) (ws->lnbundl_table[line->plnattr.line].width);
	    width = width / 2;
	    dist = (width >= xpe.x) ? 0.0 : DX;
	}

#ifdef notdef
	/* minimum distance of intersect to endpoint */
	dist = (DY1 < DY2) ? DY1 : DY2;
	DD = DY1 + DY2;
	dist = (DY >= DD) ? DX : sqrt((DX + dist) * (DX + dist));
	if (DX == dist) {
	    pe.x = DX;
	    pe.y = 0.0;
	    NdcToX(ws, &pe, &xpe);
	    if (xpe.x < 0)
		xpe.x = -xpe.x;
	    width = (line->plnattr.width == GINDIVIDUAL) ?
		(Gint) (line->plnattr.bundl.width) :
		(Gint) (ws->lnbundl_table[line->plnattr.line].width);
	    width = width / 2;
	    mini = (width >= xpe.x) ? 0.0 : mini;
	}
#endif

	mini = (dist < mini) ? dist : mini;
	if (mini == 0.0)
	    return 0.0;
    }
    return mini;
}


/*
 * float XgksInFill(seg, pt, pe, m)	distance between point *pt and fillarea
 *					*pe.
 * SEG_STATE_PTR seg;		segment state list pointer.
 * Gpoint *pe, *pt;		point and fillarea pointers.
 * Gint m;			point number of fillarea *pe.
 */
    static float
XgksInFill(seg, pt, pe, m)
    SEG_STATE_PTR   seg;
    Gpoint         *pe, *pt;
    Gint            m;
{
    float           s, s1, s2, x, y;
    float           X, Y, x1, y_1, x2, y2;	/* "y_1" rather than "y1"
						 * because <math.h> might
						 * declare "extern double
						 * y1()". */
    float           DX, DY, DY1, DY2, DD, DD1, DD2, DD3, DD4, mini, dist;
    Gpoint         *ppe, p;
    Gint            i, num;

    num = 0;
    mini = MAXNUM;
    ppe = pe;
    ppe++;
    x = pt->x;
    y = pt->y;
    for (i = 0; i < m - 1; i++, pe++, ppe++) {
	SegTrans(pe, &p, seg->segattr.segtran);
	x1 = p.x;
	y_1 = p.y;
	SegTrans(ppe, &p, seg->segattr.segtran);
	x2 = p.x;
	y2 = p.y;
	if (y_1 == y2) {
	    X = x;
	    Y = y_1;
	    DD4 = MAXNUM;
	} else if (x1 == x2) {
	    X = x1;
	    Y = y;
	    DD4 = x1;
	} else {
	    s = (y_1 - y2) / (x1 - x2);
	    s1 = 1 / s;
	    s2 = s * s + 1.0;
	    X = (s * x1 - y_1 + y + s1 * x) * s / s2;
	    Y = (y_1 - y - s * x1 - s1 * x) / s2 + y + s1 * x;
	    DD4 = x1 + (y - y_1) * s1;
	}
	DX = (float) sqrt((double) ((X - x) * (X - x) + (Y - y) * (Y - y)));
	DY = (float) sqrt((double) ((x1 - x2) * (x1 - x2) + (y_1 - y2) *
				    (y_1 - y2)));
	DY1 = (float) sqrt((double) ((X - x1) * (X - x1) + (Y - y_1) *
				     (Y - y_1)));
	DY2 = (float) sqrt((double) ((X - x2) * (X - x2) + (Y - y2) *
				     (Y - y2)));
	dist = (DY1 < DY2) ? DY1 : DY2;
	DD = DY1 + DY2;
	dist = (DY >= DD) ? DX : (DX + dist);
	DD1 = fabs(y_1 - y2);
	DD2 = fabs(y - y_1) + fabs(y - y2);
	DD3 = x;
	num = (DD1 >= DD2 && DD3 >= DD4) ? (num + 1) : num;
	mini = (dist < mini) ? dist : mini;
    }
    if (num % 2 != 0)
	return 0.0;
    return mini;
}


/*
 * float XgksDistFillarea(seg, primi, pt) - Find the distance between pick
 *					    input point and fillarea primitive.
 * SEG_STATE_PTR seg;		segment state list pointer.
 * OUT_PRIMI *primi;		output primitive pointer.
 * Gpoint *pt;			pick input point.
 */
    static float
XgksDistFillarea(seg, primi, pt)
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi;
    Gpoint         *pt;
{
    FILL_AREA_ST   *fill;
    float           mini;
    Gpoint         *pe;
    Gint            i, n;


    fill = &(primi->primi.fill_area);
    pe = (Gpoint *) malloc((size_t) (sizeof(fill->pts[0]) *
			   (fill->num_pts + 1)));
    GKSERROR((pe == NULL), 300, errXgksDistFillarea);
    n = fill->num_pts;
    for (i = 0; i < n; i++)
	*(pe + i) = fill->pts[i];
    if (fill->pts[0].x != fill->pts[n - 1].x ||
	    fill->pts[0].y != fill->pts[n - 1].y) {
	*(pe + n) = *pe;
	n += 1;
    }
    mini = XgksInFill(seg, pt, pe, n);
    ufree((voidp)pe);
    return mini;
}


/*
 * float XgksDistPmark(ws, seg, primi, pt) - Find the distance between pick
 *					     input point and polymarker
 *					     primitive
 * WS_STATE_PTR ws;		workstation state list pointer.
 * SEG_STATE_PTR seg;		segment state list pointer.
 * OUT_PRIMI *primi;		output primitive pointer.
 * Gpoint *pt;			pick input point.
 */
    static float
XgksDistPmark(ws, seg, primi, pt)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi;
    Gpoint         *pt;
{
    PMARK_ST       *mark;
    float           x, y;
    float           x1, y_1;		/* "y_1" rather than "y1" because
					 * <math.h> might declare "extern
					 * double y1()". */
    float           mini, dist;
    Gpoint          pe, p;
    Gint            i, size;

    XPoint          xpe;

    mini = MAXNUM;
    mark = &(primi->primi.pmark);
    x = pt->x;
    y = pt->y;
    for (i = 0; i < mark->num_pts; i++) {
	SegTrans(&(mark->location[i]), &p, seg->segattr.segtran);
	x1 = p.x;
	y_1 = p.y;
	dist = (float) sqrt((double) ((x1 - x) * (x1 - x) + (y_1 - y) *
			              (y_1 - y)));
	mini = (dist < mini) ? dist : mini;
	pe.x = dist;
	pe.y = 0.0;
	NdcToX(ws, &pe, &xpe);
	size = (mark->mkattr.size == GINDIVIDUAL) ?
	    (Gint) (mark->mkattr.bundl.size) :
	    (Gint) (ws->mkbundl_table[mark->mkattr.mark].size);
	mini = (size >= xpe.x) ? 0.0 : mini;
	if (mini == 0.0)
	    return 0.0;
    }
    return mini;
}


/*
 * float XgksDistText(ws, seg, primi, pt) - Find the distance between pick
 *					    input point and text primitive
 * WS_STATE_PTR ws;		workstation state list pointer.
 * SEG_STATE_PTR seg;		segment state list pointer.
 * OUT_PRIMI *primi;		output primitive pointer.
 * Gpoint *pt;			pick input point.
 */
    static float
XgksDistText(ws, seg, primi, pt)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi;
    Gpoint         *pt;
{
    Gpoint          points[5], ptn[5];
    Gfloat          mini;

    (void) xXgksInqTextExtent(ws, &(primi->primi.text), points);
    ptn[0] = points[1];
    ptn[1] = points[2];
    ptn[2] = points[3];
    ptn[3] = points[4];
    ptn[4] = points[1];
    mini = XgksInFill(seg, pt, &ptn[0], 5);
    return mini;
}


/*
 * float XgksDistCellarray(seg, primi, pt) - Find the distance between pick
 *					     input point and fillarea primitive
 * SEG_STATE_PTR seg;		segment state list pointer.
 * OUT_PRIMI *primi;		output primitive pointer.
 * Gpoint *pt;			pick input point.
 */
    static float
XgksDistCellarray(seg, primi, pt)
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi;
    Gpoint         *pt;
{
    CELL_ARRAY_ST  *cell;
    float           mini;
    Gpoint         *pe;

    cell = &(primi->primi.cell_array);
    pe = (Gpoint *) malloc((size_t) (sizeof(Gpoint) * 5));
    GKSERROR((pe == NULL), 300, errXgksDistCellarray);
    *(pe) = cell->ll;
    *(pe + 1) = cell->lr;
    *(pe + 2) = cell->ur;
    *(pe + 3) = cell->ul;
    *(pe + 4) = cell->ll;
    mini = XgksInFill(seg, pt, pe, 5);
    ufree((voidp)pe);
    return mini;
}


/*
 * XgksFindDistance(ws, seg, pid, pt) - find the distance between the pick
 *					input point and the segment
 *	WS_STATE_PTR ws;		workstation list pointer.
 *	SEG_STATE_PTR seg;		segment idetify.
 *	Gint *pid;			pick identifier of the picked primitive.
 *	Gpoint *pt;			pick input point.
 */
    static float
XgksFindDistance(ws, seg, pid, pt)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
    Gint           *pid;
    Gpoint         *pt;
{
    OUT_PRIMI      *primi;
    float           dist_primi, mini_primi;


    primi = &(seg->primi_list);

    mini_primi = MAXNUM;
    while (primi != NULL) {
	dist_primi = MAXNUM;
	switch (primi->pid) {
	case PLINE:
	    dist_primi = XgksDistPline(ws, seg, primi, pt);

#ifdef SEGMENTDEBUG
	    printf("seg= %d, line dist= %f\n", seg->segattr.seg, dist_primi);
#endif

	    break;
	case PMARK:
	    dist_primi = XgksDistPmark(ws, seg, primi, pt);

#ifdef SEGMENTDEBUG
	    printf("seg= %d, mark dist= %f\n", seg->segattr.seg, dist_primi);
#endif

	    break;
	case TEXT:
	    dist_primi = XgksDistText(ws, seg, primi, pt);

#ifdef SEGMENTDEBUG
	    printf("seg= %d, text dist= %f\n", seg->segattr.seg, dist_primi);
#endif

	    break;
	case FILL_AREA:
	    dist_primi = XgksDistFillarea(seg, primi, pt);

#ifdef SEGMENTDEBUG
	    printf("seg= %d, fill dist= %f\n", seg->segattr.seg, dist_primi);
#endif

	    break;
	case CELL_ARRAY:
	    dist_primi = XgksDistCellarray(seg, primi, pt);

#ifdef SEGMENTDEBUG
	    printf("seg= %d, cellarray dist= %f\n", seg->segattr.seg,
		   dist_primi);
#endif

	    break;
	case GDP:				/* gdp */
	case CLIP_REC:
	case XGKS_MESG:
	default:
	    break;
	}
	if (dist_primi < mini_primi) {
	    mini_primi = dist_primi;
	    *pid = primi->pickid;
	}
	if (mini_primi == 0.0)
	    return 0.0;
	primi = primi->next;
    }
    return mini_primi;
}


/*
 * int XgksInSeg(seg, pt, pe, m) - is point *pt in segment *pe.
 *	SEG_STATE_PTR seg;           segment state list pointer.
 *	Gpoint *pe, *pt;             point and bound pointers.
 *	Gint m;                      number of points in segment bound
 *
 * Function will return 0 if pt lies outside the rectangle pointed to by pe,
 * and 1 otherwise.
 *
 * NB: It is assumed that the points in "pe" are ordered counterclockwise and
 * that pe[0] = pe[m].
 */
    static int
XgksInSeg(seg, pt, pe, m)
    SEG_STATE_PTR   seg;
    Gpoint         *pe, *pt;
    Gint            m;
{
    Gpoint          trans[5];
    Gint            i;

    /*
     * In the original version, XgksInSeg might not have found the picked
     * segment if the segment-transformation included a rotation.  The array
     * "pe" contains in the first 4 elements the corners of an rectangle
     * which is parallel to the X and Y axes.  The points are sorted
     * counterclockwise and pe[4] = pe[0].  The array "trans" contains the
     * transformed points of "pe".  The so defined rectangle is NOT, in
     * general, parallel to the axes.  But if one follows the border of the
     * rectangle counterclockwise, every inner point will be on the left
     * side.  This condition is now tested for the rectangle "trans" and the
     * point "pt". -- Helmut Schumacher <ZDV145%DJUKFA11.BITNET> 4/26/90
     * (edited by Steve Emmerson 4/26/90)
     */

    /*
     * Initialize the search by transforming the first corner of the
     * rectangle.
     */
    SegTrans(pe, trans, seg->segattr.segtran);	/* WARNING: SegTrans() is a
						 * macro */

    /*
     * Proceed around the rectangle, checking to see if the given point lies
     * to the right of any transformed side.
     */
    for (i = 1, ++pe; i <= m; i++, pe++) {
	SegTrans(pe, &trans[i], seg->segattr.segtran);	/* WARNING: SegTrans()
							 * is a macro */

	/* The following condition is true if "pt" lies to the right. */
	if ((trans[i].x - trans[i - 1].x) * (pt->y - trans[i - 1].y) <
		(pt->x - trans[i - 1].x) * (trans[i].y - trans[i - 1].y))
	    return 0;
    }

    return 1;
}


/*
 *   XgksDeleteSeg (name) -  remove the entry from the table
 *			  returns INVALID if name is undefined. Will return
 *		      	  pointer to the segment as a entry independent
 *			  from the hash table
 */
    static SEG_STATE_PTR 
XgksDeleteSeg(name)
    Gint            name;
{
    SEG_STATE_PTR   pre, cnt;

    if ((pre = segtable[SHASH(name)]) == NULL)
	return NULL;

    if (pre->segattr.seg == name) {
	segtable[SHASH(name)] = pre->seg_next;
	pre->seg_next = NULL;
	return pre;
    }
    while ((cnt = pre->seg_next) != NULL) {
	if (cnt->segattr.seg == name) {
	    pre->seg_next = cnt->seg_next;
	    cnt->seg_next = NULL;
	    return cnt;
	}
	pre = cnt;
    }
    return NULL;
}


/*
 * XgksDelAssocWs(seg, ws_id)	delete ws_id from segment state return OK if
 *				successful, else INVALID, will free the segment
 *				state list if after deletion, there's no more
 *				ws assoc with the segment.
 *
 *  Will redraw the workstation if necessary
 */
    static
XgksDelAssocWs(seg, ws_id)
    SEG_STATE_PTR   seg;
    Gint            ws_id;
{
    Gint            i;

    /* First find the segment in the assoc_ws array */
    for (i = 0; seg->assoc_ws[i] != ws_id && seg->assoc_ws[i] != INVALID; i++)
	continue;
    /* not on this workstation */
    if (seg->assoc_ws[i] == INVALID)
	return INVALID;

    /* Remove this workstation by squeeze the assoc_ws array */
    while (i < MAX_ASSOC_WS) {
	if (i == (MAX_ASSOC_WS - 1)) {
	    seg->assoc_ws[i] = INVALID;
	    break;
	}
	if ((seg->assoc_ws[i] = seg->assoc_ws[i + 1]) == INVALID)
	    break;
	i++;
    }

    /*
     * If no more workstation associated with the segment delete it from data
     * structures.
     */
    if (seg->assoc_ws[0] == INVALID) {
	seg = XgksDeleteSeg(seg->segattr.seg);
	ufree((voidp)seg);
    }
    return 0;
}


    void
XgksCleanUpWsSegList(ws)
    WS_STATE_PTR    ws;
{
    WS_SEG_LIST    *cnt, *pre;

    cnt = pre = ws->seglist;
    while (cnt != NULL) {
	if (cnt->seg == INVALID) {
	    if (cnt == pre) {
		cnt = cnt->next;
		/*
		 * change ws seg ptr if deleting head of list
		 */
		if (pre == ws->seglist) {
		    ws->seglist = cnt;
		}
		/*
		 * if we just deleted the segment pointed to by seg_insertpt,
		 * then, we better reset the insert point.     (DWO)
		 */
		if (pre == ws->seg_insertpt)
		    ws->seg_insertpt = cnt;

		ufree((voidp)pre);
		pre = cnt;
	    } else {
		pre->next = cnt->next;

		/*
		 * if we just deleted the segment pointed to by seg_insertpt,
		 * then, we better reset the insert point.     (DWO)
		 */
		if (cnt == ws->seg_insertpt)
		    ws->seg_insertpt = pre;

		ufree((voidp)cnt);
		cnt = pre->next;
	    }
	} else {
	    pre = cnt;
	    cnt = cnt->next;
	}
    }
}


/*
 * XgksClearWs(ws)	Clear the workstation WITHOUT any checking, and WILL
 *			DELETE all non-segment primitives.  Will also
 *			unpend_pending_transformations.
 */
static void
XgksClearWs(ws)
     WS_STATE_PTR    ws;
{
    /* Free up ALL non-segment primitive associcated with this ws */
    XgksDeletePrimi(&(ws->primi_list), &(ws->primi_insert_pt));

    /* Clear message ptr */
    ws->message_pt = NULL;

    /* Now delete All the ws->seglist with name = INVALID */
    XgksCleanUpWsSegList(ws);

    /* if display surface is not empty, clear display */
    if (ws->wsdus.dspsurf == GNOTEMPTY)
	(void) xXgksClearWs(ws);

    /* Display now empty, and no new frame on display */
    ws->wsdus.dspsurf = GEMPTY;
    ws->wsdus.nframe = GNO;

    /* unpend pending transformations */
    if (ws->wsti.wstus == GPENDING)
	XgksUnpendPendingTrans(ws);

    return;
}



/*
 * XgksOutputSeg (ws, seg)	Output all primitive in the seg->primi_list to 
 *				ws.
 */
    static 
XgksOutputSeg(ws, seg)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
{
    OUT_PRIMI      *primi;
    Glimit          tmp_clip;

    if (seg->segattr.vis == GINVISIBLE)
	return;
    primi = &(seg->primi_list);
    tmp_clip = ws->clip;			/* save the current clip
						 * region */
    while (primi != NULL) {
	XgksDrawToWs(ws, XgksSegPrimiTran(primi, seg->segattr.segtran));
	primi = primi->next;
    }
    ws->clip = tmp_clip;			/* restore clip ws-clip
						 * region */
    xXgksUpdateClip(ws);
}


/*
 * XgksReDrawAssocWs (seg)	- generate redraw signal to all the ws assoc.
 *				  with seg->
 *
 * NOTE: This  function will not redraw on WISS (that is if WISS, skip )
 */
    static 
XgksReDrawAssocWs(seg)
    SEG_STATE_PTR   seg;
{
    Gint            i;
    WS_STATE_PTR    ws;

    for (i = 0; seg->assoc_ws[i] != INVALID && i < MAX_ASSOC_WS; i++) {
	if (NOT_WISS(seg->assoc_ws[i])) {
	    ws = OPEN_WSID(seg->assoc_ws[i]);
	    REDRAWWS(ws);
	}
    }
}


/*
 * XgksIrgNec (old, new)  - decide if implicit regeneration is necessary
 *  Gsegattr *old, *new   depending on the values of:
 *
 *    ->segtran : if different return TRUE
 *    ->vis: if old != new     return TRUE
 *    ->pri: if different      return TRUE
 */
    static 
XgksIrgNec(old, new)
    Gsegattr       *old, *new;
{
    Gint            i;

    if (old->pri != new->pri)
	return TRUE;

    if (old->vis != new->vis)
	return TRUE;

    for (i = 0; i < 3; i++)
	if ((old->segtran[0][i] != new->segtran[0][i]) ||
		(old->segtran[1][i] != new->segtran[1][i]))
	    return TRUE;

    return FALSE;
}


/*
 *   XgksNewSeg() - will return a pointer to an empty segment structure
 */
    static SEG_STATE_PTR 
XgksNewSeg()
{
    SEG_STATE_PTR   new;
    Gint            i;

    if ((new = (SEG_STATE_PTR) malloc(sizeof(SEG_STATE_ENTRY))) != 
	    NULL) {
	new->primi_list.pid = CLIP_REC;
	new->primi_list.primi.clip.segment = TRUE;
	new->primi_list.primi.clip.rec = xgks_state.cliprec.rec;
	new->primi_list.next = NULL;
	new->primi_insert_pt = &(new->primi_list);
	new->segattr.seg = INVALID;
	new->segattr.vis = GVISIBLE;
	new->segattr.hilight = GNORMAL;
	new->segattr.pri = 0.0;
	new->segattr.det = GUNDETECTABLE;
	new->text_present = FALSE;
	new->seg_next = NULL;
	new->bound.xmax = new->bound.ymax = 0.0;
	new->bound.xmin = new->bound.ymin = 1.0;
	for (i = 0; i < 3; i++)
	    new->segattr.segtran[0][i] = new->segattr.segtran[1][i] = 0.0;
	new->segattr.segtran[0][0] = new->segattr.segtran[1][1] = 1.0;
	for (i = 0; i < MAX_ASSOC_WS; i++)
	    new->assoc_ws[i] = INVALID;
    }
    return new;
}


/*
 *   XgksInstallSeg(seg) -
 *       SEG_STATE_PTR  seg  install seg-> into the segment state hash table
 */
    static 
XgksInstallSeg(seg)
    SEG_STATE_PTR   seg;
{
    seg->seg_next = segtable[SHASH(seg->segattr.seg)];
    segtable[SHASH(seg->segattr.seg)] = seg;
}


/*
 * XgksNewWsSeg()	allocate memory for workstation segment list return 
 *			pointer to the entry
 */
    static WS_SEG_LIST *
XgksNewWsSeg()
{
    WS_SEG_LIST    *new;

    if ((new = (WS_SEG_LIST *) malloc(sizeof(WS_SEG_LIST))) != NULL)
	new->next = NULL;
    return new;
}


/*
 * XgksInsertWsSeg(ws, seg_id)	build a ws_seg structure insert it to the end of
 * ws->seglist according to seg_id's priority.
 *
 * The ws->seglist is a priority queue ordered from lowest to highest priority.
 * The lowest possible priority is 0. The highest priority is 1.	c1032
 *
 */
    static 
XgksInsertWsSeg(ws, seg_id)
    WS_STATE_PTR    ws;
    Gint            seg_id;
{
    if (ws->seglist == NULL) {
	ws->seg_insertpt = XgksNewWsSeg();
	ws->seglist = ws->seg_insertpt;
    } else {
	ws->seg_insertpt->next = XgksNewWsSeg();
	ws->seg_insertpt = ws->seg_insertpt->next;
    }
    ws->seg_insertpt->seg = seg_id;

    /* mess up of segment priority */
    ws->seg_list_dirty = TRUE;
}

/*
 *   XgksDeleteWsSeg (ws, seg_id)	delete the entry in the list with
 *					sement name = name
 *
 *	returns:
 *		INVALID if undefined
 *		else return the name of the deleted segment
 */
    static
XgksDeleteWsSeg(ws, seg_id)
    WS_STATE_PTR    ws;
    Gint            seg_id;
{
    WS_SEG_LIST    *cnt;

    cnt = ws->seglist;
    while (cnt != NULL) {
	if (cnt->seg == seg_id) {
	    cnt->seg = INVALID;
	    return seg_id;
	}
	cnt = cnt->next;
    }
    return INVALID;
}


/*
 * XgksRenameWsSeg (ws, old, new) - rename the segment name in ws->seglist
 *
 */
    static 
XgksRenameWsSeg(ws, old, new)
    WS_STATE_PTR    ws;
    Gint            old, new;
{
    WS_SEG_LIST    *ptr;

    if (ws->ewstype == MO)
	return;

    ptr = ws->seglist;
    while (ptr != NULL) {
	if (ptr->seg == old) {
	    ptr->seg = new;
	    return;
	}
	ptr = ptr->next;
    }
}



/*
 * XgksInitGksSegments() - utility function to initialize GKS segment data.
 */
XgksInitGksSegments()
{
    Gint            i;

    for (i = 0; i < SHSIZE; i++)
	segtable[i] = (SEG_STATE_PTR) NULL;
    xgks_state.gks_open_seg = INVALID;
    xgks_state.gks_pick_id = 0;
}


/*
 * gcreateseg(name) - CREATE SEGMENT
 *
 * Gint name;		Name of the new segment. 1..n   c1175
 *
 * returns: 0, 3, 120, 121, 300
 *
 * See Also: ANSI Standard p.111
 */
gcreateseg(name)
    Gint            name;
{
    SEG_STATE_PTR   seg;
    Gint            i, wscnt;

    /* STEP 1: Check for errors */
    /* check for proper state */
    GKSERROR((xgks_state.gks_state != GWSAC), 3, errgcreateseg);

    /* check for valid segment name */
    GKSERROR((name < 1), 120, errgcreateseg);

    /* check for name in use */
    GKSERROR((XgksFindSeg(name) != NULL), 121, errgcreateseg);

    /* STEP 2: Allocate memory for segment state list entry */
    GKSERROR(((seg = XgksNewSeg()) == NULL), 300, errgcreateseg);

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicAttr(81, name);

    xgks_state.gks_open_seg = seg->segattr.seg = name;

    /* install it in segment state list */
    XgksInstallSeg(seg);

    /*
     * Output assocaite this segment with all avtive ws and insert it into
     * ws->seglist
     */
    i = 0;
    for (wscnt = 0; wscnt < MAX_ACTIVE_WS; wscnt++)
	if (xgks_state.activews[wscnt].ws_id != INVALID) {
	    seg->assoc_ws[i++] = xgks_state.activews[wscnt].ws_id;
	    if (xgks_state.activews[wscnt].ws->ewstype != MO) {	/* c1139 */
		XgksInsertWsSeg(xgks_state.activews[wscnt].ws, name);
		UPDATE_SEG_CNT(xgks_state.activews[wscnt].ws->primi_insert_pt);
	    }
	}
    for (; i < MAX_ASSOC_WS; i++)
	seg->assoc_ws[i] = INVALID;

/* STEP 3: Change operating state */
    xgks_state.gks_state = GSGOP;

    return 0;
}


/*
 * gcloseseg() - CLOSE SEGMENT
 *
 * returns: 0, 4
 *
 * See Also: ANSI Standard p.111
 */
gcloseseg()
{
    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state != GSGOP), 4, errgcloseseg);

    /* STEP 2: INVALIDate gks_open_seg */
    xgks_state.gks_open_seg = INVALID;

    /* STEP 3: change state */
    xgks_state.gks_state = GWSAC;

    if (MO_OPENED == TRUE)
	XgksMoCloseSeg();

    return 0;
}


/*
 * grenameseg(old, new) - RENAME SEGMENT
 *
 * Gint old,			name of an existing segment
 *	new;			new name for the segment.
 *
 * returns: 0 = OK, or one of 7, 120, 121, 122
 *
 * See also: ANSI standard p.111
 */
grenameseg(old, new)
    Gint            old, new;
{
    SEG_STATE_PTR   seg;
    Gint            i;

    /* STEP 1: check for errors */
    /* check for proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgrenameseg);

    /* check for invalid name */
    GKSERROR(((old < 1) || (new < 1)), 120, errgrenameseg);

    /* check for new name already in use */
    GKSERROR((XgksFindSeg(new) != NULL), 121, errgrenameseg);

    /*
     * Check for existance of old segment by delete it from segment state list.
     */
    GKSERROR(((seg = XgksDeleteSeg(old)) == NULL), 122, errgrenameseg);

    if (MO_OPENED == TRUE)
	XgksMoRenameSeg(old, new);

    /*
     * STEP 2: change the segments name and re-install it into segment state
     * list.
     */
    seg->segattr.seg = new;

    /*
     * If segment being renamed is the currently open one, update gks state
     * list.
     */
    if (xgks_state.gks_open_seg == old)
	xgks_state.gks_open_seg = new;
    XgksInstallSeg(seg);

    /* Now do the renaming in all associated ws->seglist */
    for (i = 0; seg->assoc_ws[i] != INVALID; i++)
	XgksRenameWsSeg(OPEN_WSID(seg->assoc_ws[i]), old, new);

    return 0;
}


/*
 * gdelseg(name)	- DELETE SEGMENT
 *
 * Gint name;		Name of segment to delete from all workstations.
 *
 * returns 0, 7, 120, 122, 125
 *
 * See Also: ANSI Standard p.112
 */
gdelseg(name)
    Gint            name;
{
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
    Gint            wscnt;

    /* STEP 1: check for errors. */
    /* check for proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgdelseg);

    /* check for invalid name */
    GKSERROR((name < 1), 120, errgdelseg);

    /* check for segment currently open */
    GKSERROR((xgks_state.gks_state == GSGOP && xgks_state.gks_open_seg == 
	name), 125, errgdelseg);

    /* try to delete the segment from segment state list in the mean time
   check for existance of segment */
    GKSERROR(((seg = XgksDeleteSeg(name)) == NULL), 122, errgdelseg);

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicAttr(84, name);

    /*
     * Now delete the segment from ws segment list and redraw all associated 
     * workstations.
     */
    for (wscnt = 0; seg->assoc_ws[wscnt] != INVALID; wscnt++) {
	ws = OPEN_WSID(seg->assoc_ws[wscnt]);
	if (ws->ewstype != MO) {
	    (void) XgksDeleteWsSeg(ws, name);
	    REDRAWWS(ws);
	}
    }

    /* remove ALL assoc primitive */
    XgksDeletePrimi(&(seg->primi_list), &(seg->primi_insert_pt));
    ufree((voidp)seg);				/* Free up the memory */

    return 0;
}


/*
 * gdelsegws(ws_id, name) - DELETE SEGMENT FROM WORKSTATION
 *
 * Gint	ws_id;			workstation to delete segment from.
 * Gint name;			segment to delete.
 *
 * returns 0, 7, 20, 25, 33, 35, 120, 123, 125
 *
 * See Also: ANSI Standard p.112
 */
gdelsegws(ws_id, name)
    Gint            ws_id;
    Gint            name;
{
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;

    /* STEP 1: check for errors. */
    /* check for proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgdelsegws);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgdelsegws);

    /* check for invalid workstation identifier */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgdelsegws);

    /* check workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgdelsegws);

    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgdelsegws);

    /* check for invalid name */
    GKSERROR((name < 1), 120, errgdelsegws);

    /* check for existance of segment */
    GKSERROR(((seg = XgksFindSeg(name)) == NULL), 123, errgdelsegws);

    /* check for segment currently open */
    GKSERROR((xgks_state.gks_state == GSGOP && xgks_state.gks_open_seg == 
	name), 125, errgdelsegws);

    /* delete the segment from the assoc_ws array */
    GKSERROR((XgksDelAssocWs(seg, ws->ws_id) == INVALID), 123, errgdelsegws);

    if (ws->ewstype != MO)
	(void) XgksDeleteWsSeg(ws, name);

    if (ws->ewstype == MO)
	XgksMoSetGraphicAttrOnWs(ws, 84, name);

    REDRAWWS(ws);

    return 0;
}


/*
 * gsetsegattr (name segattr) - SET SEGMENT ATTRIBUTES
 *
 * Gint name;			segment whos attribute is to be changed.
 * Gsegattr	*segattr;	Pointer to segment attribute structure
 *
 * returns 0,
 *      transformation : 7, 120, 122 (ANSI standard p115)
 *	visibility     : .. same ..  (ANSI standard p116)
 *	highlighting   : .. same ..  (ANSI standard p116)
 *	detectability  : .. same ..  (ANSI standard p117)
 *	priority       : 7, 120, 122, 126 (ANSI standard p117)
 */
gsetsegattr(name, segattr)
    Gint            name;
    Gsegattr       *segattr;
{
    SEG_STATE_PTR   seg;
    WS_STATE_PTR    ws;
    Gint            Redraw, Hilighted, wscnt;
    Gsegattr        old;

    /* STEP 1: check for errors. */
    /* check for proper state */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errgsetsegattr);

    /* check for invalid name */
    GKSERROR((name < 1), 120, errgsetsegattr);

    /* check for existance osegment */
    GKSERROR(((seg = XgksFindSeg(name)) == NULL), 122, errgsetsegattr);

    /* check for valid priority */
    GKSERROR((segattr->pri > 1.0 || segattr->pri < 0.0), 126, errgsetsegattr);

    if (MO_OPENED == TRUE)
	XgksCheckSegAttr(&(seg->segattr), segattr);

    /* STEP 2: Go ahead and change the attribute to the segment in state list */
    old = seg->segattr;
    segattr->seg = seg->segattr.seg;		/* ingore input seg name */
    seg->segattr = (*segattr);

    /*
     * See if priority is changed.
     * NOTE : no redraw or anything like that will happen here, just re-
     * arranging the assoc ws-seg_list.
     *
     * MORE IMPORTANT NOTE:	Rearrangement of the ws->seglist cannot occur
     *				until right before the GKS-generated redraw,
     *				else a x-initiated redraw will leave ws in a
     *				inconsistant state.
     */
    if (old.pri != seg->segattr.pri) {
	wscnt = 0;
	while (wscnt < MAX_ASSOC_WS && seg->assoc_ws[wscnt] != INVALID) {
	    ws = OPEN_WSID(seg->assoc_ws[wscnt]);
	    ws->seg_list_dirty = TRUE;
	    wscnt++;
	}
    }

    /* See if redraw is necessary -- Will take care of highlight */
    Redraw = FALSE;
    if (XgksIrgNec(&(old), &(seg->segattr)) == TRUE) {
	XgksReDrawAssocWs(seg);
	Redraw = TRUE;
    }
    if (old.hilight == seg->segattr.hilight)
	Hilighted = FALSE;
    else
	Hilighted = TRUE;

    /* If making VISIBLE, draw it out !! and check if hilighting is necessary */
    if (old.vis == GINVISIBLE && seg->segattr.vis == GVISIBLE) {
	wscnt = 0;
	while (wscnt < MAX_ASSOC_WS && seg->assoc_ws[wscnt] != INVALID) {
	    if (NOT_WISS(seg->assoc_ws[wscnt])) {
		ws = OPEN_WSID(seg->assoc_ws[wscnt]);
		if (ws->wsdus.irgmode != GALLOWED) {
		    XgksOutputSeg(ws, seg);
		    if (seg->segattr.hilight == GHIGHLIGHTED)
			XgksSetHighLight(ws, seg);
		}
	    }
	    wscnt++;
	}
    } else if (Hilighted == TRUE && seg->segattr.vis == GVISIBLE) {
	wscnt = 0;
	while (wscnt < MAX_ASSOC_WS && seg->assoc_ws[wscnt] != INVALID) {
	    if (NOT_WISS(seg->assoc_ws[wscnt])) {
		ws = OPEN_WSID(seg->assoc_ws[wscnt]);
		if ((Redraw == FALSE) ||
			(Redraw == TRUE && ws->wsdus.irgmode != GALLOWED))
		    XgksSetHighLight(ws, seg);
	    }
	    wscnt++;
	}
    }
    return 0;
}


/*
 * gsetpickid(pick_id) - SET PICK IDENTIFER
 *
 * Gint pick_id;		new pick identifier.
 *
 * returns: 0 = OK, or one of 8, 97
 *
 * See Also: ANSI Standard p.99
 */
gsetpickid(pick_id)
    Gint            pick_id;
{
    /* STEP 1: check for errors */
    /* gks in proper state? */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errgsetpickid);

    /* valid pick id? */
    GKSERROR((pick_id < 0), 97, errgsetpickid);

    /* STEP 2: change the current pick id */
    xgks_state.gks_pick_id = pick_id;

    if (MO_OPENED == TRUE)
	XgksMoSetGraphicAttr(44, pick_id);

    return 0;
}


/*
 * gassocsegws(ws_id, seg_id) ASSOCIATE SEGMENT WITH WORKSTATION
 *
 * Gint  ws_id;
 * Gint seg_id;
 *
 * returns : 0 = OK, or one of 6, 20, 25, 27, 33, 35, 120, 124
 *
 * See also: ANSI standard p.113
 */
gassocsegws(ws_id, seg_id)
    Gint            ws_id, seg_id;
{
    WS_STATE_PTR    ws, wis;
    SEG_STATE_PTR   seg;
    WS_SEG_LIST    *WsSeg;
    Gint            i;

    /* check for operating state */
    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state != GWSAC), 
	     6, errgassocsegws);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgassocsegws);

    /* Check if ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgassocsegws);

    /* Check if WISS ws is opened */
    GKSERROR(((xgks_state.wiss_id == INVALID) || 
		((wis = OPEN_WSID(xgks_state.wiss_id)) == NULL)),
	     27, errgassocsegws);

    /* check for valid ws category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgassocsegws);
    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgassocsegws);

    /* check for valid seg_id */
    GKSERROR((seg_id < 1), 120, errgassocsegws);

    /* Check if Segment is on WISS */
    WsSeg = wis->seglist;
    while (WsSeg != NULL) {
	if (WsSeg->seg == seg_id)
	    break;
	WsSeg = WsSeg->next;
    }
    GKSERROR((WsSeg == NULL), 124, errgassocsegws);

    /* Locate the desired segment from segment state list */
    seg = XgksFindSeg(WsSeg->seg);

    /* Insert the new ws_id into seg assoc_ws array if the
   ws_id is not already in there */
    i = 0;
    while (seg->assoc_ws[i] != INVALID) {
	if (seg->assoc_ws[i] == ws_id)
	    return 0;				/* <---- NOTICE the return
						 * here !! */
	i++;
    }
    seg->assoc_ws[i] = ws_id;

    /* Now insert the segment into ws->seglist if ws is not MO */
    if (ws->ewstype != MO) {
	XgksInsertWsSeg(ws, seg_id);
	UPDATE_SEG_CNT(ws->primi_insert_pt);
    }

    /* if destination ws is a MO, do what should be done, and return */
    else {
	XgksMoSetGraphicAttrOnWs(ws, 81, seg_id);
	XgksSegAttrMo(ws, seg);
	XgksRestoreMoGksStateOnWs(ws);
	XgksSegProcessMo(ws, seg);
	XgksMoCloseSegOnWs(ws);
	return OK;
    }

    /* Now if irgmode is not suppressed go ahead and redraw whole ws
   else set ws->wsdu.nframe == GYES as necessary */
    if (ws->wsdus.irgmode == GSUPPRESSED) {
	/* Now output the segment onto the destinatin ws (if it's visible) */
	if (seg->segattr.vis == GVISIBLE) {
	    XgksIDevDisable(ws);
	    XgksReDrawSeg(ws, seg->segattr.seg);
	    XgksIDevEnable(ws);
	    ws->wsdus.nframe = GYES;
	}
    } else
	REDRAWWS(ws);

    return 0;
}


/*
 * gcopysegws(ws_id, seg_id) COPY SEGMENT TO WORKSTATION
 *
 * Gint  ws_id;
 * Gint seg_id;
 *
 * returns : 0 = OK, or one of 6, 20, 25, 27, 33, 35, 36, 120, 124
 *
 * See also: ANSI standard p.113
 */
gcopysegws(ws_id, seg_id)
    Gint            ws_id, seg_id;
{
    WS_STATE_PTR    ws, wis;
    SEG_STATE_PTR   seg;
    WS_SEG_LIST    *WsSeg;
    OUT_PRIMI      *clip, *primi, *tran;
    Glimit          wsclip;
    void XgksReDrawWs();
    /* check for operating state */
    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state != GWSAC), 
	     6, errgcopysegws);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgcopysegws);

    /* Check if ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgcopysegws);

    /* Check if WISS ws is opened */
    GKSERROR(((xgks_state.wiss_id == INVALID) || 
		((wis = OPEN_WSID(xgks_state.wiss_id)) == NULL)),
	     27, errgcopysegws);

    /* check for valid ws category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgcopysegws);
    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgcopysegws);
    GKSERROR((WS_CAT(ws) == GWISS), 36, errgcopysegws);

    /* check for valid seg_id */
    GKSERROR((seg_id < 1), 120, errgcopysegws);

    /* Check if Segment is on WISS */
    WsSeg = wis->seglist;
    while (WsSeg != NULL) {
	if (WsSeg->seg == seg_id)
	    break;
	WsSeg = WsSeg->next;
    }
    GKSERROR((WsSeg == NULL), 124, errgcopysegws);

    /* if ws is of type MO, do what should be done */
    if (ws->ewstype == MO) {
	XgksRestoreMoGksStateOnWs(ws);
	XgksSegCopyMo(ws, XgksFindSeg(seg_id));
    }
    wsclip = ws->clip;				/* Save current workstation
						 * clip */
    XgksIDevDisable(ws);			/* Disable all input devices */

    /* Get ready to output all primitives in segment to ws */
    seg = XgksFindSeg(seg_id);
    primi = &(seg->primi_list);
    while (primi != NULL) {
	if (primi->pid == CLIP_REC) {
	    if (ws->primi_insert_pt->pid == CLIP_REC) {
		XgksWsWinInterset(ws, &(primi->primi.clip.rec),
				  &(ws->primi_insert_pt->primi.clip.rec));
	    } else {
		clip = XgksNewPrimi();
		clip->pid = CLIP_REC;
		XgksWsWinInterset(ws, &(primi->primi.clip.rec),
				  &(clip->primi.clip.rec));
		XgksInsertPrimi(&(ws->primi_insert_pt), clip);
	    }
	} else {
	    tran = XgksSegPrimiTran(primi, seg->segattr.segtran);
	    XgksInsertPrimi(&(ws->primi_insert_pt), tran);
	    XgksReDrawWs(ws, tran);
	}
	primi = primi->next;
    }
    XgksIDevEnable(ws);				/* Disable all input devices */
    ws->clip = wsclip;				/* Restore the old ws->clip */
    xXgksUpdateClip(ws);			/* imform x about the
						 * clip-rec change */

    return 0;
}


/*
 * ginsertseg(seg_id, segtran) - INSERT SEGMENT
 *
 * Gint seg_id;
 * Gfloat segtran[2][3];
 *
 * returns : 0 = OK, or one of 5, 27, 120, 124, 125
 *
 * See also: ANSI standard p.114
 */
ginsertseg(seg_id, segtran)
    Gint            seg_id;
    Gfloat          segtran[2][3];
{
    /*
     * declare global functions
     */
    OUT_PRIMI      *XgksDuplicatePrimi();

    WS_STATE_PTR    wis;
    WS_SEG_LIST    *WsSeg;
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *primi, *tran, *tran2;
    Gint            tmp_pickid = 0;	/* MWW: added to suppress warning */

    /* check for operating state */
    GKSERROR((xgks_state.gks_state != GWSAC && xgks_state.gks_state != GSGOP), 
	     5, errginsertseg);

    /* Check if WISS ws is opened */
    GKSERROR(((xgks_state.wiss_id == INVALID) || 
		((wis = OPEN_WSID(xgks_state.wiss_id)) == NULL)),
	     27, errginsertseg);

    /* check for valid seg_id */
    GKSERROR((seg_id < 1), 120, errginsertseg);

    /* check if seg_id is currently opened */
    GKSERROR((xgks_state.gks_state == GSGOP &&
		xgks_state.gks_open_seg == seg_id),
	     125, errginsertseg);

    /* Check if Segment is on WISS */
    WsSeg = wis->seglist;
    while (WsSeg != NULL) {
	if (WsSeg->seg == seg_id)
	    break;
	WsSeg = WsSeg->next;
    }
    GKSERROR((WsSeg == NULL), 124, errginsertseg);

    /* if MO opened, note what's going on in to MO */
    if (MO_OPENED == TRUE) {
	XgksRestoreMoGksState();
	XgksSegTransProcessMo(XgksFindSeg(seg_id), segtran);
    }
    if (xgks_state.gks_state == GSGOP)
	tmp_pickid = xgks_state.gks_pick_id;

    /* Get ready to output all primitives in segment to ws */
    seg = XgksFindSeg(seg_id);
    primi = &(seg->primi_list);
    while (primi != NULL) {
	if (primi->pid != CLIP_REC) {		/* Ignore all clipings use,
						 * current gkslist clip */
	    if (xgks_state.gks_state == GSGOP) {
		xgks_state.gks_pick_id = primi->pickid;
		tran2 = XgksDuplicatePrimi(XgksSegPrimiTran(primi, segtran));
		if ((tran = XgksAppendSegPrimi(tran2)) != NULL)
		    XgksOutputToWs(tran);
	    } else {
		tran = XgksSegPrimiTran(primi, segtran);
		XgksAppendWsPrimi(tran);
		XgksOutputToWs(tran);
	    }
	}
	primi = primi->next;
    }

    if (xgks_state.gks_state == GSGOP)
	xgks_state.gks_pick_id = tmp_pickid;

    return 0;
}


/*
 * gredrawsegws(ws_id) - REDRAW ALL SEGMENTS ON WORKSTATION
 *
 * Gint ws_id;
 *
 * returns: 0 = OK, or one of 7, 20, 25, 33, 35, 36
 *
 * See also: ANSI standard p.77
 */
gredrawsegws(ws_id)
    Gint            ws_id;
{
    WS_STATE_PTR    ws;

    /* check proper gks operating state */
    GKSERROR((xgks_state.gks_state == GGKOP || xgks_state.gks_state == GGKCL), 
	     7, errgredrawsegws);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errgredrawsegws);

    /* check valid ws_id */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgredrawsegws);

    /* check for valid workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errgredrawsegws);
    GKSERROR((WS_CAT(ws) == GINPUT), 35, errgredrawsegws);
    GKSERROR((WS_CAT(ws) == GWISS), 36, errgredrawsegws);

    /* If MO, output info to file */
    if (ws->ewstype == MO) {
	XgksMoReDrawAllSeg(ws);
    } else {
	XgksReDrawSegWs(ws);
    }

    /* if necessary call user defined redraw notifying function */
    if (ws->redrawfuncp != NULL)
	(*(ws->redrawfuncp)) (ws->ws_id, GRD_GKS);

    return OK;
}


/*
 * XgksAppendSegPrimi (primi)
 *  OUT_PRIMI *primi	append this entry to current open segment, built the
 *			input primitive into transformed ndc world, return
 *			pointer to the transformed primitive
 *
 * After appending, check current segment attribute and decide if this segment
 * should show on output -- pointer
 * or maybe it's invisible -- NULL
 */
    OUT_PRIMI*
XgksAppendSegPrimi(primi)
    OUT_PRIMI      *primi;
{
    SEG_STATE_PTR   seg;

    seg = XgksFindSeg(xgks_state.gks_open_seg);
    primi->pickid = xgks_state.gks_pick_id;
    XgksInsertPrimi(&(seg->primi_insert_pt), primi);
    XgksUpdatePrimiBound(primi, &(seg->bound));
    if (primi->pid == TEXT)
	seg->text_present = TRUE;

    /* No need to calculate transformation if it will no show up */
    if (seg->segattr.vis == GINVISIBLE)
	return NULL;

    return XgksSegPrimiTran(primi, seg->segattr.segtran);
}


/*
 * XgksDeleteAllSeg (ws)	delete all segments from the workstation
 *
 */
XgksDeleteAllSeg(ws)
    WS_STATE_PTR    ws;
{
    Girgmode        tmp;

    tmp = ws->wsdus.irgmode;
    ws->wsdus.irgmode = GSUPPRESSED;		/* Avoid redraw after each
						 * deletion */
    while (ws->seglist != NULL) {
	ws->seg_insertpt = ws->seglist->next;
	if (ws->seglist->seg != INVALID) {
	    (void) XgksDelAssocWs(XgksFindSeg(ws->seglist->seg), ws->ws_id);
	    ufree((voidp)(ws->seglist));
	}
	ws->seglist = ws->seg_insertpt;
    }
    ws->wsdus.irgmode = tmp;			/* restore the old irgmode */
    REDRAWWS(ws);
}


/*
 * XgksDelAllMoSeg( ws )        delete the MO ws from all assoc seg
 *
 * Note that MO ws have no segment list, so we have to search
 * all seg
 */
    void
XgksDelAllMoSeg(ws)
    WS_STATE_PTR    ws;
{
    Gint            i;
    SEG_STATE_PTR   seg;

    for (i = 0; i < SHSIZE; i++) {
	seg = segtable[i];
	while (seg != NULL) {
	    (void) XgksDelAssocWs(seg, ws->ws_id);
	    seg = seg->seg_next;
	}
    }
}



/*
 * XgksAppendSegClip()	check if current gks_open_seg->primi_insertpt->pid 
 *			== CLIP_REC.  If yes, change it's current value to
 *			clip->rec; else build one with value = clip->rec
 *			append to list.
 */
XgksAppendSegClip()
{
    SEG_STATE_PTR   seg;
    OUT_PRIMI      *clip;

    seg = XgksFindSeg(xgks_state.gks_open_seg);
    if (seg->primi_insert_pt->pid == CLIP_REC) {
	seg->primi_insert_pt->primi.clip.rec = xgks_state.cliprec.rec;
	return;
    } else {
	clip = XgksNewPrimi();
	clip->pid = CLIP_REC;
	clip->primi.clip.segment = TRUE;
	clip->primi.clip.rec = xgks_state.cliprec.rec;
	XgksInsertPrimi(&(seg->primi_insert_pt), clip);
	/* dbw - per insure - primi is duplicated in XgksInsertPrimi */
	free(clip);
    }
}


/*
 * XgksSegdump() - ....
 */
#ifdef SEGMENTDEBUG
XgksSegDump(seg)
    SEG_STATE_PTR   seg;
{
    (void) fprintf(stderr, "name=%d, vis=%d, hi=%d, det=%d, pri=%f\n",
		   seg->segattr.seg, seg->segattr.vis,
		   seg->segattr.hilight, seg->segattr.det, seg->segattr.pri);
    (void) fprintf(stderr, "trans= %f %f %f \n", seg->segattr.segtran[0][0],
		   seg->segattr.segtran[0][1], seg->segattr.segtran[0][2]);
    (void) fprintf(stderr, "       %f %f %f \n", seg->segattr.segtran[1][0],
		   seg->segattr.segtran[1][1], seg->segattr.segtran[1][2]);
}
#endif





/*
 * XgksShowPick -- bound or unbound the picked segment for 1 second
 */
XgksShowPick(ws, seg)
    WS_STATE_PTR    ws;
    SEG_STATE_PTR   seg;
{
    XgksSetHighLight(ws, seg);
    (void) sleep((unsigned) 1);
    XgksSetHighLight(ws, seg);
}


/*
 * XgksFindPickSeg - based on the value of ndc-point, locate a segment
 *
 * returns GP_OK     -- for pick_ok
 *	   GP_NOPICK -- for nothing is picked
 *	   GP_NONE   -- for what ?
 *
 *
 *        BIGGER->BIGGER->SAME->SAME->SMALLER->SAME->SMALLEST=
 *
 * 	During redraw ... just redraw blindly from the front, SMALLEST will
 *	get drawn last, thus highest priority.
 *
 *	The way pick works, ...
 *
 *	return_seg_id <- INVALID
 *	While (not end of ws_assoc_seg_list)
 *	  if (within or equal bound box) set return_seg_id
 *
 *	if seg_id == INVALID return GP_NOPICK
 *	else pick_id <- first non_clip primitive pick_id
 *
 *      This function was modified to improve performance.
 *      The findpickid is used as a flag to prevent unnecessary
 * 	calculation of the pickid. If the device is in sample
 * 	mode the pickid is not calculated on every click. It
 *	is calculated only when an actual sample is done.
 */
    Gpstat
XgksFindPickSeg(ws, ndcpt, response, idev, findpickid)
    WS_STATE_PTR    ws;
    Gpoint         *ndcpt;
    Gpick          *response;
    INPUT_DEV      *idev;
    Gint            findpickid;
{
    SEG_STATE_PTR   seg, foundseg = NULL;
    WS_SEG_LIST    *WsSeg = ws->seglist;
    Gpoint          segpt[5];
    Gfloat          highest;
    Glimit          localbound;
    Gint            pickid;

    highest = -1;
    response->seg = INVALID;
    response->pickid = INVALID;
    while (WsSeg != NULL) {
	seg = XgksFindSeg(WsSeg->seg);
	if (seg->segattr.det == GDETECTABLE
		&& seg->segattr.vis == GVISIBLE
		&& seg->segattr.pri >= highest) {

	    /* if text is present up date bound  c1032 */
	    XgksProcessLocalBound(ws, seg, &localbound);
	    segpt[0].x = localbound.xmin;
	    segpt[0].y = localbound.ymin;
	    segpt[1].x = localbound.xmax;
	    segpt[1].y = localbound.ymin;
	    segpt[2].x = localbound.xmax;
	    segpt[2].y = localbound.ymax;
	    segpt[3].x = localbound.xmin;
	    segpt[3].y = localbound.ymax;
	    segpt[4].x = localbound.xmin;
	    segpt[4].y = localbound.ymin;

	    if (XgksInSeg(seg, ndcpt, &segpt[0], 4)) {
		highest = seg->segattr.pri;
		foundseg = seg;
		response->seg = WsSeg->seg;
	    }
	}
	WsSeg = WsSeg->next;
    }						/* End While */

    /* If no segment is satisfied just return NO_PICK */
    if (response->seg == INVALID)
	return (response->status = GP_NOPICK);

    /* prevent another pick from interrupting  */
    idev->active = FALSE;

    /*
     * if findpickid == 2 then the segment has already been highlighted so
     * don't rehighlight just find the pickid.  This only happens in sample
     * mode.
     */
    if ((findpickid != 2) && (idev->data.pic.initst.esw == GECHO))
	XgksShowPick(ws, foundseg);

    /* is it necessary to find the pick id ? c1032 */
    if (findpickid) {
	/* Find Pick Id  c1032 */
	(void) XgksFindDistance(ws, foundseg, &pickid, ndcpt);
	response->pickid = pickid;
    }
    idev->active = TRUE;			/* allow more picks */

    return (response->status = GP_OK);
}






/* Inquiry functions for segment */

/*
 * ginqmodsegattr(ws_type, dyn)
 *	- INQUIRE DYNAMIC MODIFICATION OF SEGMENT ATTRIBUTES
 *
 * Gchar *ws_type;		type of workstation the inquiry is about.
 * Gmodseg *dyn;                OUT dynamic modification of segment attributes
 *
 * errors 0, 8, 22, 23, 39
 *
 * See Also: ANSI Standard p.184
 */
ginqmodsegattr(ws_type, dyn)
    Gchar          *ws_type;
    Gmodseg        *dyn;
{

    EWSTYPE         ewstype;

    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqmodsegattr);

    /* check for valid workstation */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqmodsegattr);

    /* check for proper workstation category */
    GKSERROR((ewstype != X_WIN), 39, errginqmodsegattr);

    /* STEP 2: set up the return values. */
    dyn->transform = GIRG;
    dyn->appear = GIMM;
    dyn->disappear = GIRG;
    dyn->highlight = GIMM;
    dyn->priority = GIRG;
    dyn->addition = GIMM;
    dyn->deletion = GIRG;

    return OK;
}


/*
 * ginqnameopenseg(seg) - INQUIRE NAME OF OPEN SEGMENT
 *
 * Gint *seg;		name of the currently open segment.
 *
 * errors 0, 4
 *
 * See Also: ANSI Standard p.151
 */
ginqnameopenseg(seg)
    Gint           *seg;
{
    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state != GSGOP), 4, errginqnameopenseg);

    /* STEP 2: set up the return values */
    *seg = xgks_state.gks_open_seg;

    return OK;
}


/*
 * ginqsegattr(segattr) - INQUIRE SEGMENT ATTRIBUTES
 *
 * Gsegattr *segattr;              OUT segment attributes
 *
 * errors 0, 7, 120, 122
 *
 * See Also: ANSI Standard p.189
 */
ginqsegattr(segattr)
    Gsegattr       *segattr;
{
    SEG_STATE_PTR   seg;

    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqsegattr);

    /* check for valid segment name */
    GKSERROR((segattr->seg < 1), 120, errginqsegattr);

    /* check for segment existance */
    GKSERROR(((seg = XgksFindSeg(segattr->seg)) == NULL), 122, errginqsegattr);

    *segattr = seg->segattr;

    return OK;
}


/*
 * ginqassocws(seg, asswk) - INQUIRE SET OF ASSOCIATED WORKSTATIONS
 *
 * Gint seg;                segment name
 * Gintlist *asswk;         OUT set of associated workstation identifiers
 *
 *	On input, num contains the maximum size for the returned set.
 *
 * errors 0, 7, 120, 122
 *
 * See Also: ANSI Standard p.189
 */
ginqassocws(seg, asswk)
    Gint            seg;
    Gintlist       *asswk;
{
    SEG_STATE_PTR   segp;
    Gint            i;

    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqassocws);

    /* check for valid segment name */
    GKSERROR((seg < 1), 120, errginqassocws);

    /* check for segment existance */
    GKSERROR(((segp = XgksFindSeg(seg)) == NULL), 122, errginqassocws);

    /* STEP 2: set up the return values */
    for (i = 0; segp->assoc_ws[i] != INVALID; i++)
	;					/* EMPTY */
    asswk->number = i;
    GKSERROR(((asswk->integers = (Gint *) malloc((size_t) (i * sizeof(Gint))))
	      == NULL), 300, errginqassocws);
    for (i = 0; i < asswk->number; i++)
	asswk->integers[i] = segp->assoc_ws[i];

    return OK;
}


/*
 * ginqsegnames(segs) - INQUIRE SET OF SEGMENT NAMES IN USE
 *
 * Gintlist *segs;               OUT set of segment names in use
 *
 *	On input, num contains the maximum number of names that can be returned.
 *	The names are not returned in any particular order.
 *
 * errors 0, 7
 *
 * See Also: ANSI Standard p.152
 */
ginqsegnames(segs)
    Gintlist       *segs;
{
    SEG_STATE_PTR   seg;
    Gint            cnt = 0, num_seg = 0;

    /* STEP 1: check for errors */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqsegnames);

    /* STEP 2: set up the return values. */
    while (cnt < SHSIZE) {
	if ((seg = segtable[cnt]) != NULL) {
	    num_seg++;
	    seg = seg->seg_next;
	    while (seg != NULL) {
		num_seg++;
		seg = seg->seg_next;
	    }
	}
	cnt++;
    }
    segs->number = num_seg;
    GKSERROR(((segs->integers = (Gint *) malloc((size_t) (num_seg * 
						sizeof(Gint)))) == NULL),
	     300, errginqsegnames);
    cnt = num_seg = 0;
    while (cnt < SHSIZE) {
	if ((seg = segtable[cnt]) != NULL) {
	    segs->integers[num_seg] = seg->segattr.seg;
	    num_seg++;
	    seg = seg->seg_next;
	    while (seg != NULL) {
		segs->integers[num_seg] = seg->segattr.seg;
		num_seg++;
		seg = seg->seg_next;
	    }
	}
	cnt++;
    }

    return OK;
}


/*
 * ginqsegnamesws(ws_id, segs) - INQUIRE SET OF SEGMENT NAMES ON WORKSTATION
 *
 * Gint ws_id;			workstation identifier
 * Gintlist *segs;              OUT set of stored segments for this workstation
 *
 * errors 0, 7, 20, 25, 33, 35
 *
 * See Also: ANSI Standard p.166
 */
ginqsegnamesws(ws_id, segs)
    Gint            ws_id;
    Gintlist       *segs;
{
    WS_STATE_PTR    ws;
    WS_SEG_LIST    *seg;
    Gint            i;

    /* STEP 1: check for errors */
    /* gks in proper state? */
    GKSERROR((xgks_state.gks_state == GGKCL || xgks_state.gks_state == GGKOP), 
	     7, errginqsegnamesws);

    /* check for invalid workstation id */
    GKSERROR((!VALID_WSID(ws_id)), 20, errginqsegnamesws);

    /* workstation open? */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errginqsegnamesws);

    /* check the workstation category */
    GKSERROR((WS_CAT(ws) == GMI), 33, errginqsegnamesws);

    /* STEP 2: ask the workstation */
    for (i = 0, seg = ws->seglist; seg != NULL; seg = seg->next, i++)
	;					/* EMPTY */
    segs->number = i;
    GKSERROR(((segs->integers = (Gint *) malloc((size_t) (i * sizeof(int))))
	      == NULL), 300, errginqsegnamesws);
    for (i = 0, seg = ws->seglist; seg != NULL; seg = seg->next, i++)
	segs->integers[i] = seg->seg;
    return OK;
}


/*
 * ginqnumsegpri(ws_type, numpri) - 
 *		INQUIRE NUMBER OF SEGMENT PRIORITIES SUPPORTED
 *
 * Gchar *ws_type;		type of workstation the inquiry is about.
 * Gint *numpri;                OUT number of segment priorities supported
 *
 * errors 0, 8, 22, 23, 39
 *
 * See Also: ANSI Standard p.183
 */
ginqnumsegpri(ws_type, numpri)
    Gchar          *ws_type;
    Gint           *numpri;
{
    EWSTYPE         ewstype;

    /* STEP 1: check for errors */
    /* proper state? */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqnumsegpri);

    /* valid workstation type */
    ewstype = XgksWsTypeToEnum(ws_type);
    GKSERROR((ewstype == WST_INVALID), 22, errginqnumsegpri);

    /* valid workstation category */
    GKSERROR((ewstype != X_WIN && ewstype != MO), 39, errginqnumsegpri);

    /* STEP 2: set up the return values */
    *numpri = 0;
    return OK;
}


/*
 * gincurpickid(pickid) - INQUIRE CURRENT PICK IDENTIFIER VALUE
 *
 * Gint *pickid		 OUT current pick identifier
 *
 * errors 0, 8
 *
 * See Also: ANSI Standard p.148
 */
ginqcurpickid(pickid)
    Gint           *pickid;
{
    /* STEP 1: check for errors */
    /* proper state? */
    GKSERROR((xgks_state.gks_state == GGKCL), 8, errginqcurpickid);

    /* STEP 2: set up the return values */
    *pickid = xgks_state.gks_pick_id;
    return OK;
}
struct ive_color {
    float r;
    float g;
    float b;
};

gps_nprint_(ws_id, seg_id, BWPRINT, as, coltab)
     Gint            *ws_id,*seg_id,*BWPRINT; 
     Gfloat          *as;
     struct ive_color *coltab; /* color table to use */
                   /*note: here ws_id refers to the X one*/
{

        extern gps_print();
        return(gps_print(*ws_id, *seg_id, *BWPRINT, *as, coltab));
}
gpdf_nprint_(ws_id, seg_id,  as, coltab, overlay)
Gint            *ws_id,*seg_id, *overlay; 
     Gfloat          *as;
     struct ive_color *coltab; /* color table to use */
                   /*note: here ws_id refers to the X one*/
{

        extern gpdf_print();
        return(gpdf_print(*ws_id, *seg_id, *as, coltab,*overlay));
}

/*
 * gps_print(wsid, seg_id, BWLINES, as, coltab) - Output a subset of possible segment 
 * stuff in postscript. This is not really GKS, but was added for IVE to 
 * void the need for SUN or DEC GKS to print.
 */

/*
 *=============================================================================
 * Postscript Routines and Structures
 */

float STANDARD_PAGE_W = 8.25;

#define VDPI                    1200.0
#define LDIM                    11.0
#define SDIM                    8.5
#define MICRONS_PER_INCH        2.54E+04
#define POINTS_PER_INCH         72.0
#define INCHES_PER_POINT        1.0/72.0

#define PS_BDR_PAD              0.075
#define PS_AXIS_PAD             0.1
#define PS_LEG_PAD              0.025
#define PS_TICK_LEN             0.125
#define BASE_DASH               (1.0/48.0)

#define BASE_WIDTH              (1.0/8.0)
#define PS_AXIS_WBASE           1
#define PS_ZERO_WBASE           4
#define PS_DATA_WBASE           7
#define PS_PIXEL                4
#define PS_DOT                  12
#define PS_MARK                 12

#define FONT_WIDTH_EST          0.55
#define PS_MAX_SEGS             1000
#define PS_NO_TSTYLE            -1
#define PS_NO_DSTYLE            -1
#define PS_NO_WIDTH             -1
#define PS_NO_LSTYLE            -1

static double PS_scale;         /* devs/micron */
static int PS_clipsave;
static int minx=1200,miny=1200,maxx=0,maxy=0;
static int EPSFILE=0;
static float xoff=0, yoff=0;
static char *PDFFILE=NULL;
static HPDF_Doc pdf;
static HPDF_Page pdfpage;
static jmp_buf myenv;
static float oldxm,oldym,oldxmin,oldymin,oldxmax,oldymax;

int
gps_print(ws_id, seg_id, BWLINES, as, coltab)
     Gint            ws_id,seg_id, BWLINES; 
     Gfloat          as; /* aspect ratio */
     struct ive_color *coltab; /* color table to use */
                   /*note: here ws_id refers to the X one*/
{
    /*
     * declare local functions
     */

    SEG_STATE_PTR XgksFindSeg();
    OUT_PRIMI *XgksSegPrimiTran();

    SEG_STATE_PTR   seg;
    WS_STATE_PTR    ws, wis;
    WS_SEG_LIST    *WsSeg;
    OUT_PRIMI      *clip, *primi, *tran, *sizeprimi;
    Glimit          wsclip;
    Gfloat         xm, ym;
    int PsInsertPrimi();
    /* check for operating state */
    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state != GWSAC),
             6, errgcopysegws);

    /* Check if ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgcopysegws);

    /* Check if WISS ws is opened */
    GKSERROR(((xgks_state.wiss_id == INVALID) ||
                ((wis = OPEN_WSID(xgks_state.wiss_id)) == NULL)),
             27, errgcopysegws);
    /* check for valid seg_id */
    GKSERROR((seg_id < 1), 120, errgcopysegws);

    /* Check if Segment is on WISS */
    WsSeg = wis->seglist;
    while (WsSeg != NULL) {
        if (WsSeg->seg == seg_id)
	  break;
        WsSeg = WsSeg->next;
    }
    GKSERROR((WsSeg == NULL), 124, errgcopysegws);
    
    /* Get ready to output all primitives in segment to file */
    seg = XgksFindSeg(seg_id);
    primi = &(seg->primi_list);
    sizeprimi = &(seg->primi_list);
    if(as <=0 ){
	xm = ym = 1.0;
    }
    else if(as < 1){
	xm = as;
	ym = 1.0;
    }
    else{
	xm = 1.0;
	ym = 1.0/as;
    }
    while (primi != NULL) {
	tran = XgksSegPrimiTran(primi, seg->segattr.segtran);
	PsInsertPrimi(tran, ws_id, BWLINES, xm, ym, coltab);
	primi = primi->next;
    }
    return 0;
}


int
gpdf_print(ws_id, seg_id, as, coltab, overlay)
     Gint            ws_id,seg_id, overlay; 
     Gfloat          as; /* aspect ratio */
     struct ive_color *coltab; /* color table to use */
                   /*note: here ws_id refers to the X one*/
{
    /*
     * declare local functions
     */

    SEG_STATE_PTR XgksFindSeg();
    OUT_PRIMI *XgksSegPrimiTran();

    SEG_STATE_PTR   seg;
    WS_STATE_PTR    ws, wis;
    WS_SEG_LIST    *WsSeg;
    OUT_PRIMI      *clip, *primi, *tran, *sizeprimi;
    Glimit          wsclip;
    Gfloat         xm, ym, xmin, xmax, ymin, ymax;
    int PdfInsertPrimi();
    /* check for operating state */
    GKSERROR((xgks_state.gks_state != GWSOP && xgks_state.gks_state != GWSAC),
             6, errgcopysegws);

    /* Check if ws_id is opened */
    GKSERROR(((ws = OPEN_WSID(ws_id)) == NULL), 25, errgcopysegws);

    /* Check if WISS ws is opened */
    GKSERROR(((xgks_state.wiss_id == INVALID) ||
                ((wis = OPEN_WSID(xgks_state.wiss_id)) == NULL)),
             27, errgcopysegws);
    /* check for valid seg_id */
    GKSERROR((seg_id < 1), 120, errgcopysegws);

    /* Check if Segment is on WISS */
    WsSeg = wis->seglist;
    while (WsSeg != NULL) {
        if (WsSeg->seg == seg_id)
	  break;
        WsSeg = WsSeg->next;
    }
    GKSERROR((WsSeg == NULL), 124, errgcopysegws);
    
    /* Get ready to output all primitives in segment to file */
    seg = XgksFindSeg(seg_id);
    primi = &(seg->primi_list);
    sizeprimi = &(seg->primi_list);
    printf("as %f\n",as);
    if(!overlay){
      if(as <=0 ){
	xm = ym = 1.0;
      }
      else if(as <= 1){
	xm = as;
	ym = 1.0;
      }
      else{
	xm = 1.0;
	ym = 1.0/as;
      }
      xmin=1.;ymin=1.;
      xmax=0.; ymax=0.;
      while (sizeprimi != NULL) {
	tran = XgksSegPrimiTran(sizeprimi, seg->segattr.segtran);
	Pdfgetsize(tran, ws_id, xm, ym, &xmin, &xmax, &ymin, &ymax);
	sizeprimi = sizeprimi->next;
      }
      //printf("before xm ym: %f %f\n",xm,ym);
      xm=xm/(xmax-xmin);
      ym=ym/(ymax-ymin);
      //printf("after xm ym: %f %f\n",xm,ym);
    }
    else{
      xm = oldxm;
      ym = oldym;
      xmin = oldxmin;
      ymin = oldymin;
      xmax = oldxmax;
      ymax = oldymax;
    }
    xoff = -xmin;
    yoff = -ymin;
    while (primi != NULL) {
	tran = XgksSegPrimiTran(primi, seg->segattr.segtran);
	PdfInsertPrimi(tran, ws_id, xm, ym, coltab);
	primi = primi->next;
    }
    oldxm=xm;
    oldym=ym;
    oldxmin=xmin;
    oldymin=ymin;
    oldxmax=xmax;
    oldymax=ymax;
    return 0;
}


static void
psHeader()
/*
 * Prints out a standard greeting to the Postscript file.
 */
{
  if(EPSFILE)
    (void) fprintf(fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
  (void) fprintf(fp, "%%%% Creator: XGKS postscript output\n");
  (void) fprintf(fp, "%%%%+ Pete Miller, David Warren and Harry Edmon\n");
  (void) fprintf(fp, "%%%%+ University of Washington\n");
  (void) fprintf(fp, "%%%%DocumentNeededResources: font Times-Roman\n");
  if(EPSFILE)
    (void) fprintf(fp, "%%%%BoundingBox: (atend)\n");
  (void) fprintf(fp, "%%%%EndComments\n");
  (void) fprintf(fp, "/l {lineto} def\n");
  (void) fprintf(fp, "/m {moveto} def\n");
  (void) fprintf(fp, "/rm {rmoveto} def\n");
  /*
   * Write out pattern fill definitions
   */
/*1*/
  (void) fprintf(fp,"/tile1 { newpath 0 0 .6 0 360 arc  fill} bind def\n");
  /*2*/
  (void) fprintf(fp,"/tile2 { newpath 0 0 1.2 0 360 arc  fill } bind def\n");
  /*.7*/
  (void) fprintf(fp,"/tile3 { newpath 0 0 .42 0 360 arc fill } bind def\n");
  /*.3*/
  (void) fprintf(fp,"/tile4 { newpath 0 0 .18 0 360 arc fill } bind def\n");
  /*1.4*/
  (void) fprintf(fp,"/tile5 { newpath 0 0 .84 0 360 arc  fill} bind def\n");
  
  (void) fprintf(fp,"/pat3 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 2 y2{ /y exch store y y1 sub 2 idiv 2 mod 0 eq { /xx x1 store}{/xx x1 1 add store}ifelse xx 2 x2 { /x exch store gsave x y translate tile1 grestore } for  } for grestore } bind def\n");
  (void) fprintf(fp,"/pat2 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 2 y2{ /y exch store y y1 sub 2 idiv 2 mod 0 eq { /xx x1 store}{/xx x1 1 add store}ifelse xx 2 x2 { /x exch store gsave x y translate tile3 grestore } for  } for grestore } bind def\n");
  
  (void) fprintf(fp,"/pat1 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 2 y2{ /y exch store y y1 sub 2 idiv 2 mod 0 eq { /xx x1 store}{/xx x1 1 add store}ifelse xx 2 x2 { /x exch store gsave x y translate tile4 grestore } for  } for grestore } bind def\n");
  (void) fprintf(fp,"/pat4 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 2 y2{ /y exch store y y1 sub 2 idiv 2 mod 0 eq { /xx x1 store}{/xx x1 1 add store}ifelse xx 2 x2 { /x exch store gsave x y translate tile5 grestore } for  } for grestore } bind def\n");
  
  (void) fprintf(fp,"/pat6 { gsave clippath 1 setgray fill grestore} bind def \n");
  (void) fprintf(fp,"/pat5 { gsave clippath 0 setgray fill grestore} bind def \n");
  
  (void) fprintf(fp,"/pat7 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 3 y2{ /y exch store y y1 sub 2 mod 0 eq { /xx x1 store}{/xx x1 1.5 add store}ifelse xx 3 x2 { /x exch store gsave x y translate tile1 grestore } for } for grestore } bind def\n");
  (void) fprintf(fp,"/pat8 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 3 y2{ /y exch store y y1 sub 2 mod 0 eq { /xx x1 store}{/xx x1 1.5 add store}ifelse xx 3 x2 { /x exch store gsave x y translate tile2 grestore }  for } for grestore } bind def\n");
 
  (void) fprintf(fp,"/pat9 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 5 y2{ /y exch store y y1 sub 2 mod 0 eq { /xx x1 store}{/xx x1 2.5 add store}ifelse xx 5 x2 { /x exch store gsave x y translate tile1 grestore } for } for grestore} bind def\n");
  (void) fprintf(fp,"/pat10 { gsave /y2 exch store /y1 exch store /x2 exch store /x1 exch store y1 5 y2{ /y exch store y y1 sub 2 mod 0 eq { /xx x1 store}{/xx x1 2.5 add store}ifelse xx 5 x2 { /x exch store gsave x y translate tile2 grestore } for } for grestore} bind def\n");  
/*
 * Writes out marker definitions
 */
  (void) fprintf(fp, "%%%% Marker definitions\n");
  (void) fprintf(fp, "/mark0 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "newpath x size sub y size sub m\n");
  (void) fprintf(fp, "size size add 0 rlineto 0 size size add rlineto\n");
  (void) fprintf(fp, "0 size size add sub 0 rlineto closepath fill} def\n");
  
  (void) fprintf(fp, "/mark1 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "newpath x size sub y size sub m\n");
  (void) fprintf(fp, "size size add 0 rlineto 0 size size add rlineto\n");
  (void) fprintf(fp,
		 "0 size size add sub 0 rlineto closepath stroke} def\n");
  
  (void) fprintf(fp, "/mark2 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "newpath x y m x y size 0 360 arc stroke} def\n");
  
  (void) fprintf(fp, "/mark3 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp,
		 "newpath x size sub y size sub m x size add y size add l\n");
  (void) fprintf(fp,
	 "x size sub y size add m x size add y size sub l stroke} def\n");

  (void) fprintf(fp, "/mark4 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "newpath x size sub y m x y size add l\n");
  (void) fprintf(fp, "x size add y l x y size sub l\n");
  (void) fprintf(fp, "closepath stroke} def\n");

  (void) fprintf(fp, "/mark5 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "x y size mark1\n");
  (void) fprintf(fp,
    "newpath x size sub y m size size add 0 rlineto stroke} def\n");

  (void) fprintf(fp, "/mark6 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "newpath x y m x y size 0 360 arc fill} def\n");

  (void) fprintf(fp, "/mark7 {/size exch def /y exch def /x exch def\n");
  (void) fprintf(fp, "newpath x y m x size sub y size sub l\n");
  (void) fprintf(fp, "x size add y size sub l closepath fill\n");
  (void) fprintf(fp, "newpath x y m x size add y size add l\n");
  (void) fprintf(fp, "x size sub y size add l closepath fill} def\n");
}

void
gps_init(file)
char        *file;  /* Output file              */

{
/*
 * The basic coordinate system is points (roughly 1/72 inch).
 * However,  most laser printers can do much better than that.
 * We invent a coordinate system based on VDPI dots per inch.
 * This goes along the long side of the page.  The long side
 * of the page is LDIM inches in length,  the short side
 * SDIM inches in length.  We we call this unit a `dev'.
 * We map `width' and `height' into devs.
 */
    

    if((fp = fopen(file,"w"))==NULL)
      return ;

    /* Postscript file identification */
    (void) fprintf(fp, "%%!PS-Adobe-3.0\n");
    psHeader();
    
    EPSFILE=0;
    PS_clipsave = 0;
}


void
geps_init(file)
char        *file;  /* Output file              */

{
/*
 * The basic coordinate system is points (roughly 1/72 inch).
 * However,  most laser printers can do much better than that.
 * We invent a coordinate system based on VDPI dots per inch.
 * This goes along the long side of the page.  The long side
 * of the page is LDIM inches in length,  the short side
 * SDIM inches in length.  We we call this unit a `dev'.
 * We map `width' and `height' into devs.
 */
    

    if((fp = fopen(file,"w"))==NULL){
	is_eps_file_open_.isopen = 0;
	return ;
    }

    /* Postscript file identification */
    (void) fprintf(fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
    EPSFILE=1;
    psHeader();
    
    
    /* Definitions */
/*    psMarks();*/
    PS_clipsave = 0;
    is_eps_file_open_.isopen = 1;
}

int ive_haru_status_to_errmsg(HPDF_STATUS status, char **msg) /* {{{ */
{
	if (status == HPDF_OK) {
		*msg = strdup("No error");
		return 0;
	}
	switch(status) { 
		case HPDF_ARRAY_COUNT_ERR: 
		case HPDF_ARRAY_ITEM_NOT_FOUND:
		case HPDF_ARRAY_ITEM_UNEXPECTED_TYPE:
		case HPDF_DICT_ITEM_NOT_FOUND:
		case HPDF_DICT_ITEM_UNEXPECTED_TYPE:	
		case HPDF_DICT_STREAM_LENGTH_NOT_FOUND:	
		case HPDF_DOC_INVALID_OBJECT:
		case HPDF_ERR_UNKNOWN_CLASS:
		case HPDF_INVALID_FONTDEF_TYPE:
		case HPDF_INVALID_OBJ_ID:
		case HPDF_INVALID_STREAM:
		case HPDF_ITEM_NOT_FOUND:
		case HPDF_NAME_INVALID_VALUE:
		case HPDF_NAME_OUT_OF_RANGE:
		case HPDF_PAGES_MISSING_KIDS_ENTRY: 
		case HPDF_PAGE_CANNOT_FIND_OBJECT:
		case HPDF_PAGE_CANNOT_GET_ROOT_PAGES:
		case HPDF_PAGE_CANNOT_SET_PARENT: 
		case HPDF_PAGE_INVALID_INDEX: 
		case HPDF_STREAM_READLN_CONTINUE: 
		case HPDF_UNSUPPORTED_FONT_TYPE:
		case HPDF_XREF_COUNT_ERR:
			*msg = strdup("libharu internal error. The consistency of the data was lost");
			break;
		case HPDF_BINARY_LENGTH_ERR:
			*msg = strdup("The length of the data exceeds HPDF_LIMIT_MAX_STRING_LEN");
			break;
		case HPDF_CANNOT_GET_PALLET:
			*msg = strdup("Cannot get a pallet data from PNG image");
			break;
		case HPDF_DICT_COUNT_ERR:
			*msg = strdup("The count of elements of a dictionary exceeds HPDF_LIMIT_MAX_DICT_ELEMENT");
			break;
		case HPDF_DOC_ENCRYPTDICT_NOT_FOUND:
			*msg = strdup("HPDF_SetPermission() OR HPDF_SetEncryptMode() was called before a password is set");
			break;
		case HPDF_DUPLICATE_REGISTRATION:
			*msg = strdup("Tried to register a font that has been registered");
			break;
		case HPDF_EXCEED_JWW_CODE_NUM_LIMIT:
			*msg = strdup("Cannot register a character to the japanese word wrap characters list");
			break;
		case HPDF_ENCRYPT_INVALID_PASSWORD:
			*msg = strdup("Tried to set the owner password to NULL or the owner password and user password are the same");
			break;
		case HPDF_EXCEED_GSTATE_LIMIT:
			*msg = strdup("The depth of the stack exceeded HPDF_LIMIT_MAX_GSTATE");
			break;
		case HPDF_FAILD_TO_ALLOC_MEM:
			*msg = strdup("Memory allocation failed");
			break;
		case HPDF_FILE_IO_ERROR:
			*msg = strdup("File processing failed");
			break;
		case HPDF_FILE_OPEN_ERROR:
			*msg = strdup("Cannot open a file");
			break;
		case HPDF_FONT_EXISTS:
			*msg = strdup("Tried to load a font that has been registered");
			break;
		case HPDF_FONT_INVALID_WIDTHS_TABLE:
			*msg = strdup("The format of a font-file is invalid");
			break;
		case HPDF_INVALID_AFM_HEADER:
			*msg = strdup("Cannot recognize a header of an afm file");
			break;
		case HPDF_INVALID_ANNOTATION:
			*msg = strdup("The specified annotation handle is invalid");
			break;
		case HPDF_INVALID_BIT_PER_COMPONENT:
			*msg = strdup("Bit-per-component of a image which was set as mask-image is invalid");
			break;
		case HPDF_INVALID_CHAR_MATRICS_DATA:
			*msg = strdup("Cannot recognize char-matrics-data  of an afm file");
			break;
		case HPDF_INVALID_COLOR_SPACE:
			*msg = strdup("The color_space parameter of HPDF_LoadRawImage is invalid, or color-space of a image which was set as mask-image is invalid or the function which is invalid in the present color-space was invoked");
			break;
		case HPDF_INVALID_COMPRESSION_MODE:
			*msg = strdup("Invalid value was set when invoking HPDF_SetCommpressionMode()");
			break;
		case HPDF_INVALID_DATE_TIME:
			*msg = strdup("An invalid date-time value was set");
			break;
		case HPDF_INVALID_DESTINATION:
			*msg = strdup("An invalid annotation handle was set");
			break;
		case HPDF_INVALID_DOCUMENT:
			*msg = strdup("An invalid document handle is set");
			break;
		case HPDF_INVALID_DOCUMENT_STATE:
			*msg = strdup("The function which is invalid in the present state was invoked");
			break;
		case HPDF_INVALID_ENCODER:
			*msg = strdup("An invalid encoder handle is set");
			break;
		case HPDF_INVALID_ENCODER_TYPE:
			*msg = strdup("A combination between font and encoder is wrong");
			break;
		case HPDF_INVALID_ENCODING_NAME:
			*msg = strdup("An invalid encoding name is specified");
			break;
		case HPDF_INVALID_ENCRYPT_KEY_LEN:
			*msg = strdup("The lengh of the key of encryption is invalid");
			break;
		case HPDF_INVALID_FONTDEF_DATA:
			*msg = strdup("An invalid font handle was set or the font format is unsupported");
			break;
		case HPDF_INVALID_FONT_NAME:
			*msg = strdup("A font which has the specified name is not found");
			break;
		case HPDF_INVALID_IMAGE:
		case HPDF_INVALID_JPEG_DATA:
			*msg = strdup("Unsupported or invalid image format");
			break;
		case HPDF_INVALID_N_DATA:
			*msg = strdup("Cannot read a postscript-name from an afm file");
			break;
		case HPDF_INVALID_OBJECT:
			*msg = strdup("An invalid object is set");
			break;
		case HPDF_INVALID_OPERATION:
			*msg = strdup("Invoked HPDF_Image_SetColorMask() against the image-object which was set a mask-image");
			break;
		case HPDF_INVALID_OUTLINE:
			*msg = strdup("An invalid outline-handle was specified");
			break;
		case HPDF_INVALID_PAGE:
			*msg = strdup("An invalid page-handle was specified");
			break;
		case HPDF_INVALID_PAGES:
			*msg = strdup("An invalid pages-handle was specified");
			break;
		case HPDF_INVALID_PARAMETER:
			*msg = strdup("An invalid value is set");
			break;
		case HPDF_INVALID_PNG_IMAGE:
			*msg = strdup("Invalid PNG image format");
			break;
		case HPDF_MISSING_FILE_NAME_ENTRY:
			*msg = strdup("libharu internal error. The _FILE_NAME entry for delayed loading is missing");
			break;
		case HPDF_INVALID_TTC_FILE:
			*msg = strdup("Invalid .TTC file format");
			break;
		case HPDF_INVALID_TTC_INDEX:
			*msg = strdup("The index parameter exceeds the number of included fonts");
			break;
		case HPDF_INVALID_WX_DATA:
			*msg = strdup("Cannot read a width-data from an afm file");
			break;
		case HPDF_LIBPNG_ERROR:
			*msg = strdup("An error has returned from PNGLIB while loading an image");
			break;
		case HPDF_PAGE_CANNOT_RESTORE_GSTATE:
			*msg = strdup("There are no graphics-states to be restored");
			break;
		case HPDF_PAGE_FONT_NOT_FOUND:
			*msg = strdup("The current font is not set");
			break;
		case HPDF_PAGE_INVALID_FONT:
			*msg = strdup("An invalid font-handle was specified");
			break;
		case HPDF_PAGE_INVALID_FONT_SIZE:
			*msg = strdup("An invalid font-size was set");
			break;
		case HPDF_PAGE_INVALID_GMODE:
			*msg = strdup("Invalid graphics mode");
			break;
		case HPDF_PAGE_INVALID_ROTATE_VALUE:
			*msg = strdup("The specified value is not a multiple of 90");
			break;
		case HPDF_PAGE_INVALID_SIZE:
			*msg = strdup("An invalid page-size was set");
			break;
		case HPDF_PAGE_INVALID_XOBJECT:
			*msg = strdup("An invalid image-handle was set");
			break;
		case HPDF_PAGE_OUT_OF_RANGE:
			*msg = strdup("The specified value is out of range");
			break;
		case HPDF_REAL_OUT_OF_RANGE:
			*msg = strdup("The specified value is out of range");
			break;
		case HPDF_STREAM_EOF:
			*msg = strdup("Unexpected EOF marker was detected");
			break;
		case HPDF_STRING_OUT_OF_RANGE:
			*msg = strdup("The length of the specified text is too big");
			break;
		case HPDF_THIS_FUNC_WAS_SKIPPED:
			*msg = strdup("The execution of a function was skipped because of other errors");
			break;
		case HPDF_TTF_CANNOT_EMBEDDING_FONT:
			*msg = strdup("This font cannot be embedded (restricted by license)");
			break;
		case HPDF_TTF_INVALID_CMAP:
			*msg = strdup("Unsupported or invalid ttf format (cannot find unicode cmap)");
			break;
		case HPDF_TTF_INVALID_FOMAT:
			*msg = strdup("Unsupported or invalid ttf format");
			break;
		case HPDF_TTF_MISSING_TABLE:
			*msg = strdup("Unsupported or invalid ttf format (cannot find a necessary table)");
			break;
		case HPDF_UNSUPPORTED_FUNC:
			*msg = strdup("The library is not configured to use PNGLIB or internal error occured");
			break;
		case HPDF_UNSUPPORTED_JPEG_FORMAT:
			*msg = strdup("Unsupported or invalid JPEG format");
			break;
		case HPDF_UNSUPPORTED_TYPE1_FONT:
			*msg = strdup("Failed to parse .PFB file");
			break;
		case HPDF_ZLIB_ERROR:
			*msg = strdup("An error has occurred while executing a function of Zlib");
			break;
		case HPDF_INVALID_PAGE_INDEX:
			*msg = strdup("An error returned from Zlib");
			break;
		case HPDF_INVALID_URI:
			*msg = strdup("An invalid URI was set");
			break;
		case HPDF_ANNOT_INVALID_ICON:
			*msg = strdup("An invalid icon was set");
			break;
		case HPDF_ANNOT_INVALID_BORDER_STYLE:
			*msg = strdup("An invalid border-style was set");
			break;
		case HPDF_PAGE_INVALID_DIRECTION:
			*msg = strdup("An invalid page-direction was set");
			break;
		case HPDF_INVALID_FONT:
			*msg = strdup("An invalid font-handle was specified");
			break;
		default:
			*msg = strdup("Unknown error occured, please report");
			break;
	}
	return 1;
}

void gpdferror_handler  (HPDF_STATUS   error_no,
				       HPDF_STATUS   detail_no,
				       void         *user_data)
{
  char *msg;
  printf ("ERROR: error_no=%04X, detail_no=%u\n", (HPDF_UINT)error_no,
	  (HPDF_UINT)detail_no);
  ive_haru_status_to_errmsg(error_no, &msg);
  printf("%s\n",msg);
  longjmp(myenv, 1);
}

void
gpdf_init(file)
char        *file;  /* Output file              */

{
/*
 * The basic coordinate system is points (roughly 1/72 inch).
 * However,  most laser printers can do much better than that.
 * We invent a coordinate system based on VDPI dots per inch.
 * This goes along the long side of the page.  The long side
 * of the page is LDIM inches in length,  the short side
 * SDIM inches in length.  We we call this unit a `dev'.
 * We map `width' and `height' into devs.
 */
    
  
  if (setjmp(myenv)) {
    if(pdf != NULL)
      HPDF_Free (pdf);
    printf ("returned: cannot create PdfDoc object\n");
    return;
  }

  pdf = HPDF_New (gpdferror_handler, NULL);
      if (!pdf) {
          printf ("error: cannot create PdfDoc object\n");
          return;
      }
    
    PDFFILE=file;
    HPDF_SetCompressionMode (pdf, HPDF_COMP_ALL);
    
}

void
gps_init_page(do_landscape, aspect)
    int *do_landscape;
    float *aspect;
{
    if(!EPSFILE)(void) fprintf(fp, "initgraphics\n");
    (void) fprintf(fp, "gsave\n");
    (void) fprintf(fp, "18.75 112.5 translate\n");
    STANDARD_PAGE_W = 8.25;
    if(*do_landscape){
      if((*aspect) >= 1.259){
	  (void) fprintf(fp, 
			   "%%do landscape\n90 rotate -100 -580 translate .95 .98 scale\n");
	    STANDARD_PAGE_W = 11;
	}
	else{
	    (void) fprintf(fp, "%%do landscape\n90 rotate 0 -580 translate\n");
	    STANDARD_PAGE_W = 8.25;
	}
    }
}

void
gpdf_init_page(do_landscape, aspect)
     int *do_landscape;
     float *aspect;
{
  float xm,ym;
  int tf, error, pr=0,pt=0;
  // HPDF_SetPageLayout  (pdf, HPDF_PAGE_LAYOUT_SINGLE);
  pdfpage = HPDF_AddPage(pdf);
  STANDARD_PAGE_W = 8.25;
  pr=0;pt=0;
  if(*aspect <=0.0 ){
    xm = ym = 1.0;
  }
  else if(*aspect <= 1.0){
    xm = *aspect;
    ym = 1.0;
  }
  else{
    xm = 1.0;
    ym = 1.0 / *aspect;
  }
  //printf("wxh %f %f %f \n",xm,ym,*aspect);
  HPDF_Page_SetHeight(pdfpage, STANDARD_PAGE_W*POINTS_PER_INCH*ym+2);
  HPDF_Page_SetWidth(pdfpage, STANDARD_PAGE_W*POINTS_PER_INCH*xm+2);
  //  if(do_landscape)
  //HPDF_Page_SetRotate(pdfpage,90);
  
  
}

PsInsertPrimi(primi, X_id, BWLINES, xm, ym, coltab)
     OUT_PRIMI      *primi;
     Gint X_id, BWLINES;
     Gfloat xm, ym;
     struct ive_color *coltab;
{
    Gint            num_pts, cnt;
    Gpoint         *new_pts, *old_pts;
    OUT_PRIMI      *new_primi;
    Gcobundl        cobundl;
    Gint            cheight;
    Gint gi, i, type;
    WS_STATE_PTR    ws;
    float           size;
    int position;
	    

/*    if ((new_primi = XgksNewPrimi()) == NULL) {
        (void) gerrorhand(300, errXgksDuplicatePrimi,
                          xgks_state.gks_err_file);
        return NULL;
    }
    *new_primi = *primi;*/

    switch (primi->pid) {
      case PLINE:
	{
	    Glnattr        *ptr;
	    Glnbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	    
	    num_pts = primi->primi.fill_area.num_pts;
	    old_pts = primi->primi.fill_area.pts;
	    cobundl.red = cobundl.green = cobundl.blue = 0.0;
	    ws=OPEN_WSID(X_id);
	    
	    ptr = &(primi->primi.pline.plnattr);
	    gi = ptr->line;
	    if (gi < 1 || gi >= MAX_BUNDL_TBL)
	      gi = 1;
	    idv_ptr = &(ptr->bundl);
	    bdl_ptr = &(ws->lnbundl_table[gi]);
	    
	    if (ptr->colour == GBUNDLED) {              /* gc.foreground */
		bundl_ptr = bdl_ptr;
	    } else {
		bundl_ptr = idv_ptr;
	    }
	    
	    if(!BWLINES){
/*		if(WS_AVAIL_COLOUR(OPEN_WSID(X_id), 
				   primi->primi.pline.plnattr.bundl.colour))
		  xXgksInqColourRep(ws,bundl_ptr->colour,
				    GREALIZED,&cobundl);
*/
		cobundl.red = coltab[bundl_ptr->colour].r;
		cobundl.green = coltab[bundl_ptr->colour].g;
		cobundl.blue = coltab[bundl_ptr->colour].b;
	    }
	    fprintf(fp, "%f %f %f setrgbcolor\n",cobundl.red,cobundl.green,
		    cobundl.blue);
	    
	    if (ptr->width == GBUNDLED) {               /* gc.line_width */
		size = bdl_ptr->width;            /* line width    */
	    } else {
		size = idv_ptr->width;
	    }
	    
	    fprintf(fp, "%f setlinewidth\n", 
		    (float)size - .5);
	    
	    if (ptr->type == GBUNDLED) {                /* gc.line_style */
		bundl_ptr = bdl_ptr;
	    } else {                                    /* line type     */
		bundl_ptr = idv_ptr;
	    }
	    
	    switch (bundl_ptr->type) {
	      case GLN_SOLID:
		fprintf(fp, "[] 0 setdash\n");
		break;
	      case GLN_DASH:
		fprintf(fp, "[10 3] 0 setdash\n");
		break;
	      case GLN_DOT:
		fprintf(fp, "[2] 0 setdash\n");
		break;
	      case GLN_DOTDASH:
		fprintf(fp, "[2 2 10 3] 0 setdash\n");
		break;
	      default:
		fprintf(fp, "[] 0 setdash\n");
	    }
	    
	    fprintf(fp, "newpath\n");
	    (void) fprintf(fp," %f %f m\n", 
			   old_pts[0].x * 
			   STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			   (old_pts[0].y) * 
			   STANDARD_PAGE_W * POINTS_PER_INCH * ym );
	    
	    
	    minx = MIN(old_pts[0].x * STANDARD_PAGE_W * 
		       POINTS_PER_INCH * xm, minx);
	    miny = MIN(old_pts[0].y * STANDARD_PAGE_W * 
		       POINTS_PER_INCH * ym, miny);
	    maxx = MAX(old_pts[0].x * STANDARD_PAGE_W * 
		       POINTS_PER_INCH * xm, maxx);
	    maxy = MAX(old_pts[0].y * STANDARD_PAGE_W * 
		       POINTS_PER_INCH * ym, maxy);
	    for (cnt = 1; cnt < num_pts; cnt++){
	      fprintf(fp," %f %f l\n",
		      (old_pts[cnt].x * 
		       STANDARD_PAGE_W * POINTS_PER_INCH * xm),
		      (old_pts[cnt].y) * 
		      STANDARD_PAGE_W * POINTS_PER_INCH * ym);
	      minx = MIN(old_pts[cnt].x * STANDARD_PAGE_W * 
			 POINTS_PER_INCH * xm, minx);
	      miny = MIN(old_pts[cnt].y * STANDARD_PAGE_W * 
			 POINTS_PER_INCH * ym, miny);
	      maxx = MAX(old_pts[cnt].x * STANDARD_PAGE_W * 
			 POINTS_PER_INCH * xm, maxx);
	      maxy = MAX(old_pts[cnt].y * STANDARD_PAGE_W * 
			 POINTS_PER_INCH * ym, maxy);
	    }
	    fprintf(fp, "stroke\n");
        break;
	}
      case PMARK:
	{
	    Gmkattr        *ptr;
	    Gmkbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;

	    ws=OPEN_WSID(X_id);
	    ptr = &primi->primi.pmark.mkattr;
	    gi = ptr->mark;
	    if (gi < 1 || gi >= MAX_BUNDL_TBL)
	      gi = 1;
	    idv_ptr = &(ptr->bundl);
	    bdl_ptr = &(ws->mkbundl_table[gi]);
    
	    if (ptr->colour == GBUNDLED)                /* marker type  */
	      bundl_ptr = bdl_ptr;
	    else
	      bundl_ptr = idv_ptr;
	    
	    cobundl.red = coltab[bundl_ptr->colour].r;
	    cobundl.green = coltab[bundl_ptr->colour].g;
	    cobundl.blue = coltab[bundl_ptr->colour].b;
	    
	    fprintf(fp, "%f %f %f setrgbcolor\n",cobundl.red,cobundl.green,
		    cobundl.blue);

	    if (ptr->size == GBUNDLED)                  /* marker size */
	      size = (float) bdl_ptr->size;
	    else
	      size = (float) idv_ptr->size;
	    printf("Marker Size: %f\n",size);
	    
	    if (ptr->type == GBUNDLED)                  /* marker type  */
	      bundl_ptr = bdl_ptr;
	    else
	      bundl_ptr = idv_ptr;
	    
	    if (WS_MARKER_TYPE(bundl_ptr->type))
	      type = bundl_ptr->type;
	    else
	      type = GMK_POINT;
	    
	    old_pts = primi->primi.pmark.location;
	    switch(type){
	      case GMK_POINT:
		for(i=0; i<primi->primi.pmark.num_pts; i++){
		    fprintf(fp, "newpath %f %f m\n",
			    old_pts[i].x * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			    old_pts[i].y * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * ym);
		    fprintf(fp, "%f %f %f 0 360 arc\n",
			    old_pts[i].x * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			    old_pts[i].y * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * ym,
			    (float)size * 
			    .016 * POINTS_PER_INCH );
		    fprintf(fp, "fill\n");
		    minx = MIN(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, minx);
		    miny = MIN(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, miny);
		    maxx = MAX(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, maxx);
		    maxy = MAX(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, maxy);
		}
		break;
	      case GMK_PLUS:
		for(i=0; i<primi->primi.pmark.num_pts; i++){
		    fprintf(fp, "newpath %f %f m \n/Helvetica findfont %f scalefont\n setfont\n(+) dup stringwidth pop 2 div neg dup rmoveto show\n",
			    old_pts[i].x
			    * STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			    old_pts[i].y * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * ym,
			    (float) size*.07* POINTS_PER_INCH);
		    minx = MIN(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, minx);
		    miny = MIN(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, miny);
		    maxx = MAX(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, maxx);
		    maxy = MAX(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, maxy);
		}
		break;
	      case GMK_STAR:
		for(i=0; i<primi->primi.pmark.num_pts; i++){
		    fprintf(fp, "newpath %f %f m \n/Helvetica findfont %f scalefont\n setfont\n(*) dup stringwidth pop 2 div neg dup 3 mul rmoveto show\n",
			    old_pts[i].x
			    * STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			    old_pts[i].y * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * ym,
			    (float) size*.1* POINTS_PER_INCH);
		    minx = MIN(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, minx);
		    miny = MIN(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, miny);
		    maxx = MAX(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, maxx);
		    maxy = MAX(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, maxy);
		}
		break;
	      case GMK_O:
		for(i=0; i<primi->primi.pmark.num_pts; i++){
		    fprintf(fp, "newpath %f %f m\n",
			    old_pts[i].x * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			    old_pts[i].y * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * ym +
			   (float)size *  .016 * POINTS_PER_INCH);
		    fprintf(fp, "%f %f %f 0 360 arc\n",
			    old_pts[i].x * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			    old_pts[i].y * 
			    STANDARD_PAGE_W * POINTS_PER_INCH * ym,
			    (float)size * 
			    .016 * POINTS_PER_INCH );
		    fprintf(fp, "stroke\n");
		    minx = MIN(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, minx);
		    miny = MIN(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, miny);
		    maxx = MAX(old_pts[i].x * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * xm, maxx);
		    maxy = MAX(old_pts[i].y * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym, maxy);
		}
		break;
		
	      case GMK_X:
		for(i=0; i<primi->primi.pmark.num_pts; i++){
		  fprintf(fp, "  newpath %f %f m\n/Helvetica findfont %f scalefont\n setfont\n(X) dup stringwidth pop 2 div neg dup  rmoveto show\n",
		    old_pts[i].x 
		    * STANDARD_PAGE_W * POINTS_PER_INCH * xm,
		    old_pts[i].y 
		    * STANDARD_PAGE_W * POINTS_PER_INCH * ym,
		    size*.05*POINTS_PER_INCH);
		  minx = MIN(old_pts[i].x * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * xm, minx);
		  miny = MIN(old_pts[i].y * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * ym, miny);
		  maxx = MAX(old_pts[i].x * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * xm, maxx);
		  maxy = MAX(old_pts[i].y * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * ym, maxy);
		}
		break;
	    }
	}
	break;
      case FILL_AREA:
	{
	    Gflattr        *ptr;
	    Gflbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
            Gflinter        fill_inter;
            int             px, py, prev, cur, fill_style;          
 	    
	    
	    ws=OPEN_WSID(X_id);
	    num_pts = primi->primi.fill_area.num_pts;
	    old_pts = primi->primi.fill_area.pts;
	    cobundl.red = cobundl.green = cobundl.blue = 0.0;
	    
	    ptr = &(primi->primi.fill_area.flattr);
	    gi = ptr->fill;
	    if (gi < 1 || gi >= MAX_BUNDL_TBL)
	      gi = 1;
	    idv_ptr = &(ptr->bundl);
	    bdl_ptr = &(ws->flbundl_table[gi]);
	    
	    /* the colour attribute */
	    
	    if (ptr->colour == GBUNDLED)
	      bundl_ptr = bdl_ptr;
	    else
	      bundl_ptr = idv_ptr;
	    
            /* the fill interior attribute */
            
            if (ptr->inter == GBUNDLED)
              bundl_ptr = bdl_ptr;
            else
              bundl_ptr = idv_ptr;
            
            fill_inter = bundl_ptr->inter;
            
            /* the fill style attribute */
            
            if (ptr->style == GBUNDLED)
              fill_style = bdl_ptr->style;
            else
              fill_style = idv_ptr->style;
            
            
 /*	    if(WS_AVAIL_COLOUR(ws,bundl_ptr->colour))
	      xXgksInqColourRep(OPEN_WSID(X_id),
				bundl_ptr->colour,
				GREALIZED,&cobundl);
*/
	    cobundl.red = coltab[bundl_ptr->colour].r;
	    cobundl.green = coltab[bundl_ptr->colour].g;
	    cobundl.blue = coltab[bundl_ptr->colour].b;
	    fprintf(fp, "%f %f %f setrgbcolor\n",cobundl.red,cobundl.green,
		    cobundl.blue);
	    fprintf(fp, "newpath\n");
            if (fill_inter == GPATTERN) {
                
                (void)fprintf(fp,"gsave\n");
                (void) fprintf(fp,"  %f %f m\n", 
                               old_pts[0].x * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * xm,
                               old_pts[0].y * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * ym);
		//                minx = maxx = old_pts[0].x * STANDARD_PAGE_W *
		//             POINTS_PER_INCH * xm;
                //             miny = maxy = old_pts[0].y * STANDARD_PAGE_W *
		//             POINTS_PER_INCH * ym;
		minx = MIN(old_pts[0].x * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * xm, minx);
		miny = MIN(old_pts[0].y * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * ym, miny);
		maxx = MAX(old_pts[0].x * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * xm, maxx);
		maxy = MAX(old_pts[0].y * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * ym, maxy);
                
                for (cnt = 1; cnt < num_pts; cnt++){
                    fprintf(fp,"  %f %f l\n",
                            (old_pts[cnt].x * STANDARD_PAGE_W * 
                             POINTS_PER_INCH * xm),
                            (old_pts[cnt].y) * STANDARD_PAGE_W * 
                            POINTS_PER_INCH * ym);
                    minx = MIN(old_pts[cnt].x * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * xm, minx);
                    miny = MIN(old_pts[cnt].y * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * ym, miny);
                    maxx = MAX(old_pts[cnt].x * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * xm, maxx);
                    maxy = MAX(old_pts[cnt].y * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * ym, maxy);
                }
                fprintf(fp, "clip\n");
                (void)fprintf(fp,"%d %d %d %d pat%d\n",minx,maxx,miny,maxy,
                              fill_style);
                (void)fprintf(fp,"grestore\n");
            }
            else{
                (void) fprintf(fp,"  %f %f m\n", 
                               old_pts[0].x * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * xm,
                               old_pts[0].y * STANDARD_PAGE_W * 
                               POINTS_PER_INCH * ym);
		
		minx = MIN(old_pts[0].x * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * xm, minx);
		miny = MIN(old_pts[0].y * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * ym, miny);
		maxx = MAX(old_pts[0].x * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * xm, maxx);
		maxy = MAX(old_pts[0].y * STANDARD_PAGE_W * 
			   POINTS_PER_INCH * ym, maxy);
                for (cnt = 1; cnt < num_pts; cnt++){
                  fprintf(fp,"  %f %f l\n",
                          (old_pts[cnt].x * STANDARD_PAGE_W * 
                           POINTS_PER_INCH * xm),
                          (old_pts[cnt].y) * STANDARD_PAGE_W * 
                          POINTS_PER_INCH * ym);
		  minx = MIN(old_pts[cnt].x * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * xm, minx);
		  miny = MIN(old_pts[cnt].y * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * ym, miny);
		  maxx = MAX(old_pts[cnt].x * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * xm, maxx);
		  maxy = MAX(old_pts[cnt].y * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * ym, maxy);
		}
                fprintf(fp, "fill\n");
            }
        }
        break;
    case CLIP_REC:
      {
	if(PS_clipsave > 0)
	  fprintf(fp,"grestore\n");
	if(primi->primi.clip.rec.xmin != primi->primi.clip.rec.xmax &&
	       primi->primi.clip.rec.ymin != primi->primi.clip.rec.ymax){
		if(EPSFILE){
		    fprintf(fp,"gsave\n");
		    PS_clipsave = 1;
		}
		else{
		    fprintf(fp,"initclip\n");
		    PS_clipsave=0;
		}
		fprintf(fp,"newpath\n");
		fprintf(fp,"%f %f m\n",
			primi->primi.clip.rec.xmin * 
			STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			primi->primi.clip.rec.ymin * 
			STANDARD_PAGE_W * POINTS_PER_INCH * ym);
		fprintf(fp,"%f %f l\n",
			primi->primi.clip.rec.xmax * 
			STANDARD_PAGE_W * POINTS_PER_INCH * xm ,
			primi->primi.clip.rec.ymin * 
			STANDARD_PAGE_W * POINTS_PER_INCH * ym );
		fprintf(fp,"%f %f l\n",
			primi->primi.clip.rec.xmax * 
			STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			primi->primi.clip.rec.ymax * 
			STANDARD_PAGE_W * POINTS_PER_INCH * ym);
		fprintf(fp,"%f %f l\n",
			primi->primi.clip.rec.xmin * 
			STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			primi->primi.clip.rec.ymax * 
			STANDARD_PAGE_W * POINTS_PER_INCH * ym);
		fprintf(fp, "closepath clip\n");
	    }
	    break;
	}
    case TEXT:
      {	

	float angle;
	int vert=0;
	old_pts = primi->primi.text.location;
	fprintf(fp,"gsave\n");
	  cobundl.red = cobundl.green = cobundl.blue = 0.0;
	    if(!BWLINES){
/*		if(WS_AVAIL_COLOUR(OPEN_WSID(X_id), 
				   primi->primi.text.txattr.bundl.colour))
		  xXgksInqColourRep(OPEN_WSID(X_id),
				    primi->primi.text.txattr.bundl.colour,
				    GREALIZED,&cobundl);
*/
		cobundl.red = 
		  coltab[primi->primi.text.txattr.bundl.colour].r;
		cobundl.green = 
		  coltab[primi->primi.text.txattr.bundl.colour].g;
		cobundl.blue = 
		  coltab[primi->primi.text.txattr.bundl.colour].b;
	    }
	    if(primi->primi.text.txattr.tx_exp == GINDIVIDUAL) 
	      fprintf(fp, "/%s findfont %d scalefont\n",
		      "Times-Roman", 
		      (cheight = 
		       (int)(primi->primi.text.chattr.chwidth * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * 
			     primi->primi.text.txattr.bundl.ch_exp * 1.5 
			     + .5)));
	    else
	      fprintf(fp, "/%s findfont %d scalefont\n",
		      "Times-Roman", 
		      (cheight = (int)
		       (primi->primi.text.chattr.chwidth * STANDARD_PAGE_W * 
			POINTS_PER_INCH * 1.5)));
	    
	    fprintf(fp, "setfont\n");
	    fprintf(fp, "%f %f %f setrgbcolor\n",cobundl.red,cobundl.green,
		    cobundl.blue);
	    fprintf(fp, "newpath\n");
	    
	    fprintf(fp,  "%f %f  m\n",
		    old_pts[0].x  * STANDARD_PAGE_W * POINTS_PER_INCH * xm,
		    old_pts[0].y  * STANDARD_PAGE_W * POINTS_PER_INCH * ym);
	    
	    fprintf(fp,"  %f %f atan rotate\n",
		    primi->primi.text.base_vec.y,
		    primi->primi.text.base_vec.x);
	    angle = atan(primi->primi.text.base_vec.y/
			 primi->primi.text.base_vec.x);
	   if(angle >.75 && angle < 2.4)vert=1;

	    switch(primi->primi.text.chattr.align.hor){
	      case  GTH_CENTRE:
		switch(primi->primi.text.chattr.align.ver){
		  case GTV_HALF:
		    fprintf(fp, "(%s) stringwidth pop -2.0 div %f rm\n",
			    primi->primi.text.string,(float)cheight/-3.0);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)-xm*cheight/2, minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)+xm*cheight/2, maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-ym*cheight/2, miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+ym*cheight/2, maxy);
		    }
		    break;
		  case GTV_BASE:
		  case GTV_BOTTOM:
		    fprintf(fp, "(%s) stringwidth pop -2.0 div 0.0 rm\n",
			    primi->primi.text.string);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)-xm*cheight/2, minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)+xm*cheight/2, maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym), miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+ym*cheight, maxy);
		    }
		    break;
		  case GTV_NORMAL:
		  case GTV_TOP:
		  case GTV_CAP:
		  default:
		    fprintf(fp, "(%s) stringwidth pop -2.0 div %d rm\n",
			    primi->primi.text.string, -cheight);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-xm*cheight/2, minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+xm*cheight/2, maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-ym*cheight,miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym), maxy);
		    }
		    break;
		}
		break;
	      case  GTH_RIGHT:
		switch(primi->primi.text.chattr.align.ver){
		  case GTV_BASE:
		  case GTV_BOTTOM:
		    fprintf(fp, "(%s) stringwidth pop -1.0 mul 0.0 rm\n",
			    primi->primi.text.string);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)-xm*cheight, minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm), maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym), miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+ym*cheight, maxy);
		    }
		    break;
		  case GTV_HALF:
		    fprintf(fp, "(%s) stringwidth pop -1.0 mul %f rm\n",
			    primi->primi.text.string,(float)cheight/-3.0);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)-xm*cheight, minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm), maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-ym*cheight/2, miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+ym*cheight/2, maxy);
		    }
		    break;
		  case GTV_NORMAL:
		  case GTV_TOP:
		  case GTV_CAP:
		  default:
		    fprintf(fp, "(%s) stringwidth pop -1.0 mul %d rm\n",
			    primi->primi.text.string, -cheight);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)-xm*cheight, minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm), maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-ym*cheight, miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym), maxy);
		    }
		    break;
		}
		break;
	      case  GTH_NORMAL:
	      case  GTH_LEFT:
	      default:
		switch(primi->primi.text.chattr.align.ver){
		  case GTV_HALF:
		    fprintf(fp,
			    " 0.0 %f rm\n",(float)cheight/-3.0);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm), minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)+xm*cheight, maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-ym*cheight/2, miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+ym*cheight/2, maxy);
		    }
		    break;
		  case GTV_BASE:
		  case GTV_BOTTOM:
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm), minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)+xm*cheight, maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym), miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)+ym*cheight, maxy);
		    }
		    break;
		  case GTV_NORMAL:
		  case GTV_TOP:
		  case GTV_CAP:
		  default:
		    fprintf(fp,
			    "0.0 %d  rm\n",-cheight);
		    if(vert){
		      minx = MIN((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm), minx);
		      maxx = MAX((old_pts[0].x * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * xm)+xm*cheight, maxx);
		    }
		    else{
		      miny = MIN((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym)-ym*cheight, miny);
		      maxy = MAX((old_pts[0].y * STANDARD_PAGE_W * 
				  POINTS_PER_INCH * ym), maxy);
		    }
		    break;
		}
		break;
	    }
	    fprintf(fp,"(");
	    for(position=0;position<strlen(primi->primi.text.string);
		position++){
		if(primi->primi.text.string[position] == '(' ||
		   primi->primi.text.string[position] == ')' ||
		   primi->primi.text.string[position] == '\\' ){
		    fprintf(fp,"\\%c",
			    primi->primi.text.string[position]);
		}
		else{
		    fprintf(fp,"%c",primi->primi.text.string[position]);
		}
	    }
	    fprintf(fp,") show\n");
	    fprintf(fp,"grestore\n");
	    break;
	}
      case XGKS_MESG:
        break;
      case CELL_ARRAY:
        break;
      case GDP:
        break;
    }
    return 0;
}


PdfInsertPrimi(primi, X_id, xm, ym, coltab)
     OUT_PRIMI      *primi;
     Gint X_id;
     Gfloat xm, ym;
     struct ive_color *coltab;
{
    Gint            num_pts, cnt;
    Gpoint         *new_pts, *old_pts;
    OUT_PRIMI      *new_primi;
    Gcobundl        cobundl;
    Gint            cheight;
    Gint gi, i, type;
    WS_STATE_PTR    ws;
    float           size;
    int position;
    HPDF_Font pdffont;
	    

    HPDF_UINT16 mode;
    /*    
    mode = HPDF_Page_GetGMode  (pdfpage);
    if(mode&HPDF_GMODE_PAGE_DESCRIPTION)
      printf("HPDF_GMODE_PAGE_DESCRIPTION\n");
    if(mode&HPDF_GMODE_PATH_OBJECT)
      printf("HPDF_GMODE_PATH_OBJECT\n");
    if(mode&HPDF_GMODE_TEXT_OBJECT)
      printf("HPDF_GMODE_TEXT_OBJECT\n");
    if(mode&HPDF_GMODE_CLIPPING_PATH)
      printf("HPDF_GMODE_CLIPPING_PATH\n");
    if(mode&HPDF_GMODE_SHADING)
      printf("HPDF_GMODE_SHADING\n");
    if(mode&HPDF_GMODE_INLINE_IMAGE)
      printf("HPDF_GMODE_INLINE_IMAGE\n");
    if(mode&HPDF_GMODE_EXTERNAL_OBJECT)
      printf("HPDF_GMODE_EXTERNAL_OBJECT\n");
    */
    //    printf("xoff %f, yoff %f\n",xoff,yoff);
    switch (primi->pid) {
      case PLINE:
	{
	  Glnattr        *ptr;
	  Glnbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	  
	  //printf("pline\n");
	  num_pts = primi->primi.fill_area.num_pts;
	  old_pts = primi->primi.fill_area.pts;
	  cobundl.red = cobundl.green = cobundl.blue = 0.0;
	  ws=OPEN_WSID(X_id);
	  
	  ptr = &(primi->primi.pline.plnattr);
	  gi = ptr->line;
	  if (gi < 1 || gi >= MAX_BUNDL_TBL)
	    gi = 1;
	  idv_ptr = &(ptr->bundl);
	  bdl_ptr = &(ws->lnbundl_table[gi]);
	  
	  if (ptr->colour == GBUNDLED) {              /* gc.foreground */
	    bundl_ptr = bdl_ptr;
	  } else {
	    bundl_ptr = idv_ptr;
	  }
	  
	  cobundl.red = coltab[bundl_ptr->colour].r;
	  cobundl.green = coltab[bundl_ptr->colour].g;
	  cobundl.blue = coltab[bundl_ptr->colour].b;
	  if(cobundl.red > 1.0 || cobundl.red < 0.0)cobundl.red = 0.0;
	  if(cobundl.green > 1.0 || cobundl.green < 0.0)cobundl.green = 0.0;
	  if(cobundl.blue > 1.0 || cobundl.blue < 0.0)cobundl.blue = 0.0;
	  HPDF_Page_SetRGBStroke(pdfpage,cobundl.red,cobundl.green,
				 cobundl.blue);
	  
	  if (ptr->width == GBUNDLED) {               /* gc.line_width */
	    size = bdl_ptr->width;            /* line width    */
	  } else {
	    size = idv_ptr->width;
	  }
	  HPDF_Page_SetLineWidth(pdfpage, size);
	  HPDF_Page_SetLineCap(pdfpage, HPDF_BUTT_END);
	  HPDF_Page_SetLineJoin(pdfpage,HPDF_MITER_JOIN);
	  
	  if (ptr->type == GBUNDLED) {                /* gc.line_style */
	    bundl_ptr = bdl_ptr;
	  } else {                                    /* line type     */
	    bundl_ptr = idv_ptr;
	  }

	  switch (bundl_ptr->type) {
	  case GLN_SOLID:
	    HPDF_Page_SetDash(pdfpage, NULL,0,0);
	    break;
	  case GLN_DASH:
	    HPDF_Page_SetDash(pdfpage, PDFDASH,1,1);
	    break;
	  case GLN_DOT:
	    HPDF_Page_SetDash(pdfpage, PDFDOT,4,1);
	    break;
	  case GLN_DOTDASH:
	    HPDF_Page_SetDash(pdfpage,PDFDOTDASH,4,1);
	    break;
	  default:
	    HPDF_Page_SetDash(pdfpage, NULL,0,0);
	  }

	  /*	  
	  HPDF_Page_MoveTo(pdfpage,
			   (int)(old_pts[0].x * 
				 STANDARD_PAGE_W * POINTS_PER_INCH * xm),
			   (int)(old_pts[0].y * 
				 STANDARD_PAGE_W * POINTS_PER_INCH * ym) );
	  
	  */
	  HPDF_Page_MoveTo(pdfpage,
			   xm*(old_pts[0].x + xoff)*
			   POINTS_PER_INCH*STANDARD_PAGE_W + 1,
			   ym*(old_pts[0].y+yoff)*
			   POINTS_PER_INCH*STANDARD_PAGE_W + 1);
	  
	  for (cnt = 1; cnt < num_pts; cnt++)
	    HPDF_Page_LineTo(pdfpage,
			     xm*(xoff+old_pts[cnt].x)*
			     POINTS_PER_INCH*STANDARD_PAGE_W + 1,
			     ym*(yoff+old_pts[cnt].y)*
			     POINTS_PER_INCH*STANDARD_PAGE_W + 1);
	  //	    HPDF_Page_LineTo(pdfpage,
	  //	     (old_pts[cnt].x * 
	  //	      STANDARD_PAGE_W * POINTS_PER_INCH * xm),
	  //	     (old_pts[cnt].y) * 
	  //	     STANDARD_PAGE_W * POINTS_PER_INCH * ym);
	  HPDF_Page_Stroke(pdfpage);
	  break;
	}
    case PMARK:
      {
	Gmkattr        *ptr;
	Gmkbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	//printf("pmark\n");
	ws=OPEN_WSID(X_id);
	ptr = &primi->primi.pmark.mkattr;
	gi = ptr->mark;
	if (gi < 1 || gi >= MAX_BUNDL_TBL)
	  gi = 1;
	idv_ptr = &(ptr->bundl);
	bdl_ptr = &(ws->mkbundl_table[gi]);
	
	if (ptr->colour == GBUNDLED)                /* marker type  */
	  bundl_ptr = bdl_ptr;
	else
	  bundl_ptr = idv_ptr;
	
	cobundl.red = coltab[bundl_ptr->colour].r;
	cobundl.green = coltab[bundl_ptr->colour].g;
	cobundl.blue = coltab[bundl_ptr->colour].b;
	if(cobundl.red > 1.0 || cobundl.red < 0.0)cobundl.red = 0.0;
	if(cobundl.green > 1.0 || cobundl.green < 0.0)cobundl.green = 0.0;
	if(cobundl.blue > 1.0 || cobundl.blue < 0.0)cobundl.blue = 0.0;
	

	HPDF_Page_SetRGBStroke(pdfpage,cobundl.red,cobundl.green,
			       cobundl.blue);
	HPDF_Page_SetRGBFill(pdfpage,cobundl.red,cobundl.green,
			     cobundl.blue);
	HPDF_Page_SetLineWidth(pdfpage, 1.0);
	
	pdffont = HPDF_GetFont (pdf, "Helvetica", NULL);

	if (ptr->size == GBUNDLED)                  /* marker size */
	  size = (float) bdl_ptr->size;
	else
	  size = (float) idv_ptr->size;
	printf("Marker Size: %f\n",size);
	
	if (ptr->type == GBUNDLED)                  /* marker type  */
	  bundl_ptr = bdl_ptr;
	else
	  bundl_ptr = idv_ptr;
	
	if (WS_MARKER_TYPE(bundl_ptr->type))
	  type = bundl_ptr->type;
	else
	  type = GMK_POINT;
	
	old_pts = primi->primi.pmark.location;
	switch(type){
	case GMK_POINT:
	  for(i=0; i<primi->primi.pmark.num_pts; i++){
	    HPDF_Page_MoveTo(pdfpage,
			     (old_pts[0].x +xoff)* 
			     STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (old_pts[0].y +yoff) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1);
	    HPDF_Page_Circle(pdfpage,
			     (old_pts[i].x +xoff) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (old_pts[i].y +yoff)* 
			     STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1,
			     (float)size * 
			     .016 * POINTS_PER_INCH );
	    HPDF_Page_Fill(pdfpage);
	  }
	  break;
	case GMK_PLUS:
	  HPDF_Page_BeginText (pdfpage);
	    for(i=0; i<primi->primi.pmark.num_pts; i++){
	    HPDF_Page_MoveTextPos(pdfpage,
				  (xoff+old_pts[0].x) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (yoff+ old_pts[0].y) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1);
	    HPDF_Page_SetFontAndSize(pdfpage, pdffont,
				     (float) size*.07* POINTS_PER_INCH);
	    HPDF_Page_ShowText  (pdfpage,"+");
	  }
	  HPDF_Page_EndText (pdfpage);
	  break;
	case GMK_STAR:
	  HPDF_Page_BeginText (pdfpage);
	  for(i=0; i<primi->primi.pmark.num_pts; i++){
	    HPDF_Page_MoveTextPos(pdfpage,
				  (xoff+ old_pts[0].x) * 
				  STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (old_pts[0].y +yoff) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1);
	    HPDF_Page_SetFontAndSize(pdfpage, pdffont,
				     (float) size*.07* POINTS_PER_INCH);
	    HPDF_Page_ShowText  (pdfpage,"*");
	  }
	  HPDF_Page_EndText (pdfpage);
	  break;
	case GMK_O:
	  for(i=0; i<primi->primi.pmark.num_pts; i++){
	    HPDF_Page_MoveTo(pdfpage,
			     (xoff+ old_pts[0].x) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (old_pts[0].y +yoff) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1);
	    HPDF_Page_Circle(pdfpage,
			     (xoff+ old_pts[i].x) * 
		    STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (yoff+ old_pts[i].y) * 
		    STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1,
		    (float)size * 
		    .016 * POINTS_PER_INCH );
	    HPDF_Page_Stroke(pdfpage);
	  }
	  break;
	  
	case GMK_X:
	  HPDF_Page_BeginText (pdfpage);
	  for(i=0; i<primi->primi.pmark.num_pts; i++){
	    HPDF_Page_MoveTextPos(pdfpage,
				  (xoff+ old_pts[0].x) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1,
			     (old_pts[0].y +yoff) * 
			     STANDARD_PAGE_W * POINTS_PER_INCH * ym + 1);
	    HPDF_Page_SetFontAndSize(pdfpage, pdffont,
				     (float) size*.07* POINTS_PER_INCH);
	    HPDF_Page_ShowText  (pdfpage,"X");
	  }
	  HPDF_Page_EndText (pdfpage);
	  break;
	}
      }
      break;
      case FILL_AREA:
	{
	  Gflattr        *ptr;
	  Gflbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	  Gflinter        fill_inter;
	  int             px, py, prev, cur, fill_style;          
 	  
	  
	  //printf("fill\n");
	  ws=OPEN_WSID(X_id);
	  num_pts = primi->primi.fill_area.num_pts;
	  old_pts = primi->primi.fill_area.pts;
	  cobundl.red = cobundl.green = cobundl.blue = 0.0;
	  
	  ptr = &(primi->primi.fill_area.flattr);
	  gi = ptr->fill;
	  if (gi < 1 || gi >= MAX_BUNDL_TBL)
	    gi = 1;
	  idv_ptr = &(ptr->bundl);
	  bdl_ptr = &(ws->flbundl_table[gi]);
	  
	  /* the colour attribute */
	  
	  if (ptr->colour == GBUNDLED)
	    bundl_ptr = bdl_ptr;
	  else
	    bundl_ptr = idv_ptr;
	  
	  /* the fill interior attribute */
          
	  if (ptr->inter == GBUNDLED)
	    bundl_ptr = bdl_ptr;
	  else
	    bundl_ptr = idv_ptr;
	  
	  fill_inter = bundl_ptr->inter;
          
	  /* the fill style attribute */
          
	  if (ptr->style == GBUNDLED)
	    fill_style = bdl_ptr->style;
	  else
	    fill_style = idv_ptr->style;
	  
          
	  /*	    if(WS_AVAIL_COLOUR(ws,bundl_ptr->colour))
		    xXgksInqColourRep(OPEN_WSID(X_id),
		    bundl_ptr->colour,
		    GREALIZED,&cobundl);
	  */
	  cobundl.red = coltab[bundl_ptr->colour].r;
	  cobundl.green = coltab[bundl_ptr->colour].g;
	  cobundl.blue = coltab[bundl_ptr->colour].b;
	  if(cobundl.red > 1.0 || cobundl.red < 0.0)cobundl.red = 0.0;
	  if(cobundl.green > 1.0 || cobundl.green < 0.0)cobundl.green = 0.0;
	  if(cobundl.blue > 1.0 || cobundl.blue < 0.0)cobundl.blue = 0.0;
	  
	  HPDF_Page_SetRGBStroke(pdfpage,cobundl.red,cobundl.green,
				 cobundl.blue);
	  HPDF_Page_SetRGBFill(pdfpage,cobundl.red,cobundl.green,
			       cobundl.blue);
	  HPDF_Page_SetLineWidth(pdfpage, 1.0);
	
	  if (fill_inter == GPATTERN) {
	    float myx, myy;
	    HPDF_Page_GSave(pdfpage);
	    HPDF_Page_MoveTo(pdfpage,
			     (xoff+ old_pts[0].x) * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * xm + 1,
			     (yoff+ old_pts[0].y) * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * ym + 1);

	    minx = maxx = (xoff+ old_pts[0].x) * STANDARD_PAGE_W *
	      POINTS_PER_INCH * xm  + 1;
	    miny = maxy = (yoff+ old_pts[0].y) * STANDARD_PAGE_W *
	      POINTS_PER_INCH * ym + 1;
                
	    for (cnt = 1; cnt < num_pts; cnt++){
	      HPDF_Page_LineTo(pdfpage,
			       (xoff+ old_pts[cnt].x) * STANDARD_PAGE_W * 
				POINTS_PER_INCH * xm + 1,
			       (old_pts[cnt].y +yoff) * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym + 1);
	    }
	    HPDF_Page_Clip(pdfpage);
	    myx = minx;
	    myy = miny;
	    while (myx < maxx){
	      HPDF_Page_MoveTo(pdfpage,myx,miny);
	      HPDF_Page_LineTo(pdfpage,myx,maxy);
	      HPDF_Page_Stroke(pdfpage);
	    }
	    while (myy < maxy){
	      HPDF_Page_MoveTo(pdfpage,minx,myy);
	      HPDF_Page_LineTo(pdfpage,maxx,myy);
	      HPDF_Page_Stroke(pdfpage);
	    }
	    HPDF_Page_GRestore(pdfpage);
	  }
	  else{
	    HPDF_Page_MoveTo(pdfpage,
			     (xoff+old_pts[0].x) * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * xm + 1 ,
			     (yoff+old_pts[0].y) * STANDARD_PAGE_W * 
			     POINTS_PER_INCH * ym + 1);
	    
	    for (cnt = 1; cnt < num_pts; cnt++)
	      HPDF_Page_LineTo(pdfpage, 
			       (xoff+ old_pts[cnt].x) * STANDARD_PAGE_W * 
				POINTS_PER_INCH * xm + 1,
			       (old_pts[cnt].y+yoff) * STANDARD_PAGE_W * 
			       POINTS_PER_INCH * ym + 1);
	    HPDF_Page_Fill(pdfpage);
	  }
        }
        break;
      case CLIP_REC:
	{
	  break;
	  //printf("clip\n");
	  /*
            if(PS_clipsave > 0)
	      HPDF_Page_GRestore(pdfpage);
	    if(primi->primi.clip.rec.xmin != primi->primi.clip.rec.xmax &&
	       primi->primi.clip.rec.ymin != primi->primi.clip.rec.ymax){
	      HPDF_Page_GSave(pdfpage);
	      PS_clipsave = 1;
	      HPDF_Page_MoveTo(pdfpage,
			       primi->primi.clip.rec.xmin * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			       primi->primi.clip.rec.ymin * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * ym);
	      HPDF_Page_LineTo(pdfpage,
			       primi->primi.clip.rec.xmax * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * xm ,
			       primi->primi.clip.rec.ymin * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * ym );
	      HPDF_Page_LineTo(pdfpage,
			       primi->primi.clip.rec.xmax * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			       primi->primi.clip.rec.ymax * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * ym);
	      HPDF_Page_LineTo(pdfpage,  
			       primi->primi.clip.rec.xmin * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * xm,
			       primi->primi.clip.rec.ymax * 
			       STANDARD_PAGE_W * POINTS_PER_INCH * ym);
	      HPDF_Page_ClosePath(pdfpage);
	      HPDF_Page_Clip(pdfpage);
	    }
	  */
	    break;
	}
    case TEXT:
      {	
	float angle;
	//printf("text\n");
	old_pts = primi->primi.text.location;
	cobundl.red = 
	  coltab[primi->primi.text.txattr.bundl.colour].r;
	cobundl.green = 
	  coltab[primi->primi.text.txattr.bundl.colour].g;
	cobundl.blue = 
	  coltab[primi->primi.text.txattr.bundl.colour].b;
	
	if(cobundl.red > 1.0 || cobundl.red < 0.0)cobundl.red = 0.0;
	if(cobundl.green > 1.0)cobundl.green = 0.0;
	if(cobundl.blue > 1.0)cobundl.blue = 0.0;
	HPDF_Page_SetRGBStroke(pdfpage,cobundl.red,cobundl.green,
			       cobundl.blue);
	HPDF_Page_SetRGBFill(pdfpage,cobundl.red,cobundl.green,
			     cobundl.blue);
	HPDF_Page_SetLineWidth(pdfpage, 1.0);
	
	angle=atan(primi->primi.text.base_vec.y/primi->primi.text.base_vec.x);
	
	HPDF_Page_GSave(pdfpage);
	HPDF_Page_Concat(pdfpage, cos(angle), sin(angle), 
			 -sin(angle), cos(angle),
			 (xoff+ old_pts[0].x) * 
			 STANDARD_PAGE_W * POINTS_PER_INCH * xm + 1 ,
			 (yoff+ old_pts[0].y) * STANDARD_PAGE_W * 
			 POINTS_PER_INCH * ym + 1);
	
	HPDF_Page_BeginText (pdfpage);	
	pdffont = HPDF_GetFont (pdf, "Times-Roman", NULL);
	if(primi->primi.text.txattr.tx_exp == GINDIVIDUAL) {
	  HPDF_Page_SetFontAndSize
	    (pdfpage, pdffont,
	     primi->primi.text.chattr.chwidth * STANDARD_PAGE_W * 
	     POINTS_PER_INCH * 
	     primi->primi.text.txattr.bundl.ch_exp * 1.5 
	     + .5);
	}
	else
	  HPDF_Page_SetFontAndSize
	    (pdfpage, pdffont,
	     primi->primi.text.chattr.chwidth * STANDARD_PAGE_W * 
	     POINTS_PER_INCH * 1.5);
	
	switch(primi->primi.text.chattr.align.hor){
	case  GTH_CENTRE:
	  switch(primi->primi.text.chattr.align.ver){
	  case GTV_HALF:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  -(HPDF_Page_TextWidth
				    (pdfpage,primi->primi.text.string)/2.0),
				  -(HPDF_Page_GetTextRise(pdfpage)/2.0));
	    break;
	  case GTV_BASE:
	  case GTV_BOTTOM:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  -(HPDF_Page_TextWidth
				    (pdfpage,primi->primi.text.string)/2.0),
				  0);
	    break;
	  case GTV_NORMAL:
	  case GTV_TOP:
	  case GTV_CAP:
	  default:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  -(HPDF_Page_TextWidth
				    (pdfpage,primi->primi.text.string)/2.0),
				  -(HPDF_Page_GetTextRise(pdfpage)));
	    break;
	  }
	  break;
	case  GTH_RIGHT:
	  switch(primi->primi.text.chattr.align.ver){
	  case GTV_BASE:
	  case GTV_BOTTOM:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  -(HPDF_Page_TextWidth
				    (pdfpage,primi->primi.text.string)),
				  0);
	    break;
	  case GTV_HALF:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  -(HPDF_Page_TextWidth
				    (pdfpage,primi->primi.text.string)),
				  -(HPDF_Page_GetTextRise(pdfpage)/2.0));
	    break;
	  case GTV_NORMAL:
	  case GTV_TOP:
	  case GTV_CAP:
	  default:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  -(HPDF_Page_TextWidth
				    (pdfpage,primi->primi.text.string)),
				  -(HPDF_Page_GetTextRise(pdfpage)));
	    break;
	  }
	  break;
	case  GTH_NORMAL:
	case  GTH_LEFT:
	default:
	  switch(primi->primi.text.chattr.align.ver){
	  case GTV_HALF:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  0,-(HPDF_Page_GetTextRise(pdfpage)/2.0));
	    break;
	  case GTV_BASE:
	  case GTV_BOTTOM:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  0,0);
	    break;
	  case GTV_NORMAL:
	  case GTV_TOP:
	  case GTV_CAP:
	  default:
	    HPDF_Page_MoveTextPos(pdfpage, 
				  0,-(HPDF_Page_GetTextRise(pdfpage)));
	    break;
	  }
	  break;
	}
	HPDF_Page_ShowText(pdfpage, primi->primi.text.string);
	HPDF_Page_EndText (pdfpage);	
	/*
	  mode = HPDF_Page_GetGMode  (pdfpage);
	  if(mode&HPDF_GMODE_PAGE_DESCRIPTION)
	  printf("HPDF_GMODE_PAGE_DESCRIPTION\n");
	  if(mode&HPDF_GMODE_PATH_OBJECT)
	  printf("HPDF_GMODE_PATH_OBJECT\n");
	  if(mode&HPDF_GMODE_TEXT_OBJECT)
	  printf("HPDF_GMODE_TEXT_OBJECT\n");
	  if(mode&HPDF_GMODE_CLIPPING_PATH)
	  printf("HPDF_GMODE_CLIPPING_PATH\n");
	  if(mode&HPDF_GMODE_SHADING)
	  printf("HPDF_GMODE_SHADING\n");
	  if(mode&HPDF_GMODE_INLINE_IMAGE)
	  printf("HPDF_GMODE_INLINE_IMAGE\n");
	  if(mode&HPDF_GMODE_EXTERNAL_OBJECT)
	  printf("HPDF_GMODE_EXTERNAL_OBJECT\n");
	*/
	//	printf("restore\n");
	HPDF_Page_GRestore(pdfpage);
	break;
      }
    case XGKS_MESG:
      break;
    case CELL_ARRAY:
      break;
    case GDP:
      break;
    }
    return 0;
}

int
Pdfgetsize(primi, X_id, xm, ym, xminp, xmaxp, yminp, ymaxp)
     OUT_PRIMI      *primi;
     Gint X_id;
     Gfloat xm, ym, *xminp, *yminp, *xmaxp, *ymaxp;
{
    Gint            num_pts, cnt;
    Gpoint         *new_pts, *old_pts;
    OUT_PRIMI      *new_primi;
    Gcobundl        cobundl;
    Gint            cheight;
    Gint            gi, i, type;
    Gfloat          xmin,xmax,ymin,ymax;
    WS_STATE_PTR    ws;
    float           size;
    int position;

    xmin = *xminp;
    ymin = *yminp;
    xmax = *xmaxp;
    ymax = *ymaxp;
    switch (primi->pid) {
      case PLINE:
	{
	  Glnattr        *ptr;
	  Glnbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	  
	  //	  printf("pline\n");
	  num_pts = primi->primi.fill_area.num_pts;
	  old_pts = primi->primi.fill_area.pts;
	  ws=OPEN_WSID(X_id);
	  
	  ptr = &(primi->primi.pline.plnattr);
	  gi = ptr->line;
	  if (gi < 1 || gi >= MAX_BUNDL_TBL)
	    gi = 1;
	  idv_ptr = &(ptr->bundl);
	  bdl_ptr = &(ws->lnbundl_table[gi]);
	  
	  if(xmin > old_pts[0].x)xmin=old_pts[0].x;
	  if(xmax < old_pts[0].x)xmax=old_pts[0].x;
	  if(ymin > old_pts[0].y)ymin=old_pts[0].y;
	  if(ymax < old_pts[0].y)ymax=old_pts[0].y;
	  for (cnt = 1; cnt < num_pts; cnt++){
	    if(xmin > old_pts[cnt].x)xmin=old_pts[cnt].x;
	    if(xmax < old_pts[cnt].x)xmax=old_pts[cnt].x;
	    if(ymin > old_pts[cnt].y)ymin=old_pts[cnt].y;
	    if(ymax < old_pts[cnt].y)ymax=old_pts[cnt].y;
	  }
	  break;
	}
    case PMARK:
      {
	Gmkattr        *ptr;
	Gmkbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	//	printf("pmark\n");
	ws=OPEN_WSID(X_id);
	ptr = &primi->primi.pmark.mkattr;
	gi = ptr->mark;
	if (gi < 1 || gi >= MAX_BUNDL_TBL)
	  gi = 1;
	idv_ptr = &(ptr->bundl);
	bdl_ptr = &(ws->mkbundl_table[gi]);
	

	if (ptr->size == GBUNDLED)                  /* marker size */
	  size = (float) bdl_ptr->size;
	else
	  size = (float) idv_ptr->size;
	
	if (ptr->type == GBUNDLED)                  /* marker type  */
	  bundl_ptr = bdl_ptr;
	else
	  bundl_ptr = idv_ptr;
	
	if (WS_MARKER_TYPE(bundl_ptr->type))
	  type = bundl_ptr->type;
	else
	  type = GMK_POINT;
	
	old_pts = primi->primi.pmark.location;
	if(xmin > old_pts[0].x)xmin=old_pts[0].x;
	if(xmax < old_pts[0].x)xmax=old_pts[0].x;
	if(ymin > old_pts[0].y)ymin=old_pts[0].y;
	if(ymax < old_pts[0].y)ymax=old_pts[0].y;
	switch(type){
	case GMK_POINT:
	case GMK_O:
	  for(i=0; i<primi->primi.pmark.num_pts; i++){
	    if(xmin > old_pts[cnt].x)xmin=old_pts[cnt].x;
	    if(xmax < old_pts[cnt].x)xmax=old_pts[cnt].x;
	    if(ymin > old_pts[cnt].y)ymin=old_pts[cnt].y;
	    if(ymax < old_pts[cnt].y)ymax=old_pts[cnt].y;
	  }
	  break;
	case GMK_PLUS:
	case GMK_STAR:
	case GMK_X:
	  if(xmin > old_pts[0].x-(float)size*.07)xmin=old_pts[0].x-
						   (float)size*.07;
	  if(xmax < old_pts[0].x+(float)size*.07)xmax=old_pts[0].x+
						   (float)size*.07;
	  if(ymin > old_pts[0].y-(float)size*.07)ymin=old_pts[0].y-
						   (float)size*.07;
	  if(ymax < old_pts[0].y+(float)size*.07)ymax=old_pts[0].y+
						   (float)size*.07;
	  break;
	}
      }
      break;
    case FILL_AREA:
      {
	Gflattr        *ptr;
	Gflbundl       *idv_ptr, *bdl_ptr, *bundl_ptr;
	Gflinter        fill_inter;
	int             px, py, prev, cur, fill_style;          
 	
	
	//	printf("fill\n");
	ws=OPEN_WSID(X_id);
	num_pts = primi->primi.fill_area.num_pts;
	old_pts = primi->primi.fill_area.pts;
	
	ptr = &(primi->primi.fill_area.flattr);
	gi = ptr->fill;
	if (gi < 1 || gi >= MAX_BUNDL_TBL)
	  gi = 1;
	idv_ptr = &(ptr->bundl);
	bdl_ptr = &(ws->flbundl_table[gi]);
	
	
	
	if(xmin > old_pts[0].x)xmin=old_pts[0].x;
	if(xmax < old_pts[0].x)xmax=old_pts[0].x;
	if(ymin > old_pts[0].y)ymin=old_pts[0].y;
	if(ymax < old_pts[0].y)ymax=old_pts[0].y;
	for (cnt = 1; cnt < num_pts; cnt++){
	  if(xmin > old_pts[cnt].x)xmin=old_pts[cnt].x;
	  if(xmax < old_pts[cnt].x)xmax=old_pts[cnt].x;
	  if(ymin > old_pts[cnt].y)ymin=old_pts[cnt].y;
	  if(ymax < old_pts[cnt].y)ymax=old_pts[cnt].y;
	}
      }
      break;
    case CLIP_REC:
      break;
      
    case TEXT:
      {	
	float angle, cheight, clen, chwidth;
	int vert = 0;
	//	printf("text\n");

	old_pts = primi->primi.text.location;
	
	angle=atan(primi->primi.text.base_vec.y/primi->primi.text.base_vec.x);
	
	if(angle >.75 && angle < 2.4)vert=1;
	
	if(primi->primi.text.txattr.tx_exp == GINDIVIDUAL) {
	  chwidth=primi->primi.text.chattr.chwidth * 
	    primi->primi.text.txattr.bundl.ch_exp;
	  cheight=primi->primi.text.chattr.height * 
	    primi->primi.text.txattr.bundl.ch_exp;
	}
	else{
	  chwidth = primi->primi.text.chattr.chwidth;
	  cheight = primi->primi.text.chattr.height;
	}
	
	switch(primi->primi.text.chattr.align.hor){
	case  GTH_CENTRE:
	  switch(primi->primi.text.chattr.align.ver){
	  case GTV_HALF:
	    if(vert){
	      if(xmin > old_pts[0].x -cheight/2.)
		xmin=old_pts[0].x - cheight/2.;
	      if(xmax < old_pts[0].x + cheight/2)
		xmax=old_pts[0].x + cheight/2.;
	      if(ymin > (old_pts[0].y - 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		ymin=old_pts[0].y - chwidth*strlen(primi->primi.text.string)/2.0;
	      if(ymax < (old_pts[0].y + 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		ymax=old_pts[0].y +chwidth*strlen(primi->primi.text.string)/2.0;
	    }
	    else{
	      if(ymin > old_pts[0].y -cheight/2.)
		ymin=old_pts[0].y - cheight/2.;
	      if(ymax < old_pts[0].y + cheight/2)
		ymax=old_pts[0].y + cheight/2.;
	      if(xmin > (old_pts[0].x - 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		xmin=old_pts[0].x - 
		  chwidth*strlen(primi->primi.text.string)/2.0;
	      if(xmax < (old_pts[0].x + 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		xmax=old_pts[0].x +chwidth*strlen(primi->primi.text.string)/2.0;
	    }
	    break;
	  case GTV_BASE:
	  case GTV_BOTTOM:
	    if(vert){
	      if(xmin > old_pts[0].x)
		xmin=old_pts[0].x;
	      if(xmax < old_pts[0].x + cheight)
		xmax=old_pts[0].x + cheight;
	      if(ymin > (old_pts[0].y - 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		ymin=old_pts[0].y - chwidth*strlen(primi->primi.text.string)/2.0;
	      if(ymax < (old_pts[0].y + 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		ymax=old_pts[0].y +chwidth*strlen(primi->primi.text.string)/2.0;
	    }
	    else{
	      if(ymin > old_pts[0].y)
		ymin=old_pts[0].y;
	      if(ymax < old_pts[0].y + cheight)
		ymax=old_pts[0].y + cheight;
	      if(xmin > (old_pts[0].x - 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		xmin=old_pts[0].x - chwidth*strlen(primi->primi.text.string)/2.0;
	      if(xmax < (old_pts[0].x + 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		xmax=old_pts[0].x +chwidth*strlen(primi->primi.text.string)/2.0;
	    }
	    break;
	  case GTV_NORMAL:
	  case GTV_TOP:
	  case GTV_CAP:
	  default:
	    if(vert){
	      if(xmin > old_pts[0].x -cheight)
		xmin=old_pts[0].x -cheight;
	      if(xmax < old_pts[0].x)
		xmax=old_pts[0].x;
	      if(ymin > (old_pts[0].y - 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		ymin=old_pts[0].y - chwidth*strlen(primi->primi.text.string)/2.0;
	      if(ymax < (old_pts[0].y + 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		ymax=old_pts[0].y +chwidth*strlen(primi->primi.text.string)/2.0;
	    }
	    else{
	      if(ymin > old_pts[0].y -cheight)
		ymin=old_pts[0].y - cheight;
	      if(ymax < old_pts[0].y)
		ymax=old_pts[0].y;
	      if(xmin > (old_pts[0].x - 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		xmin=old_pts[0].x - chwidth*strlen(primi->primi.text.string)/2.0;
	      if(xmax < (old_pts[0].x + 
			 chwidth*strlen(primi->primi.text.string)/2.0))
		xmax=old_pts[0].x +chwidth*strlen(primi->primi.text.string)/2.0;
	    }
	    break;
	  }
	  break;
	case  GTH_RIGHT:
	  switch(primi->primi.text.chattr.align.ver){
	  case GTV_BASE:
	  case GTV_BOTTOM:
	    if(vert){
	      if(xmin > old_pts[0].x)
		xmin=old_pts[0].x;
	      if(xmax < old_pts[0].x + cheight/2)
		xmax=old_pts[0].x + cheight/2.;
	      if(angle > 0){
		if(ymin > (old_pts[0].y - chwidth*strlen(primi->primi.text.string)))
		  ymin=old_pts[0].y - chwidth*strlen(primi->primi.text.string);
		if(ymax < old_pts[0].y)
		  ymax=old_pts[0].y;
	      }
	      else{
		if(ymax < (old_pts[0].y - chwidth*strlen(primi->primi.text.string)))
		  ymax=old_pts[0].y - chwidth*strlen(primi->primi.text.string);
		if(ymin > old_pts[0].y)
		  ymin=old_pts[0].y;
	      }
	    }
	    else{
	      if(ymin > old_pts[0].y)
		ymin=old_pts[0].y;
	      if(ymax < old_pts[0].y + cheight/2)
		ymax=old_pts[0].y + cheight/2.;
	      if(xmin > (old_pts[0].x - chwidth*strlen(primi->primi.text.string)))
		xmin=old_pts[0].x - chwidth*strlen(primi->primi.text.string);
	      if(xmax < old_pts[0].x)
		xmax=old_pts[0].x;
	    }
	    break;
	  case GTV_HALF:
	    if(vert){
	      if(xmin > old_pts[0].x -cheight/2.)
		xmin=old_pts[0].x - cheight/2.;
	      if(xmax < old_pts[0].x + cheight/2)
		xmax=old_pts[0].x + cheight/2.;
	      if(angle > 0){
		if(ymin > (old_pts[0].y - chwidth*strlen(primi->primi.text.string)))
		  ymin=old_pts[0].y - chwidth*strlen(primi->primi.text.string);
		if(ymax < old_pts[0].y)
		  ymax=old_pts[0].y;
	      }
	      else{
		if(ymax < (old_pts[0].y - chwidth*strlen(primi->primi.text.string)))
		  ymax=old_pts[0].y - chwidth*strlen(primi->primi.text.string);
		if(ymin > old_pts[0].y)
		  ymin=old_pts[0].y;
	      }
	    }
	    else{
	      if(ymin > old_pts[0].y -cheight/2.)
		ymin=old_pts[0].y - cheight/2.;
	      if(ymax < old_pts[0].y + cheight/2)
		ymax=old_pts[0].y + cheight/2.;
	      if(xmin > (old_pts[0].x - chwidth*strlen(primi->primi.text.string)))
		xmin=old_pts[0].x - chwidth*strlen(primi->primi.text.string);
	      if(xmax < old_pts[0].x)
		xmax=old_pts[0].x;
	    }
	    
	    break;
	  case GTV_NORMAL:
	  case GTV_TOP:
	  case GTV_CAP:
	  default:
	    if(vert){
	      if(xmin > old_pts[0].x -cheight)
		xmin=old_pts[0].x - cheight;
	      if(xmax < old_pts[0].x)
		xmax=old_pts[0].x;
	      if(angle > 0){
		if(ymin > (old_pts[0].y - chwidth*strlen(primi->primi.text.string)))
		  ymin=old_pts[0].y - chwidth*strlen(primi->primi.text.string);
		if(ymax < old_pts[0].y)
		  ymax=old_pts[0].y;
	      }
	      else{
		if(ymax < (old_pts[0].y - chwidth*strlen(primi->primi.text.string)))
		  ymax=old_pts[0].y - chwidth*strlen(primi->primi.text.string);
		if(ymin > old_pts[0].y)
		  ymin=old_pts[0].y;
	      }
	    }
	    else{
	      if(ymin > old_pts[0].y -cheight)
		ymin=old_pts[0].y - cheight;
	      if(ymax < old_pts[0].y)
		ymax=old_pts[0].y;
	      if(xmin > (old_pts[0].x - chwidth*strlen(primi->primi.text.string)))
		xmin=old_pts[0].x - chwidth*strlen(primi->primi.text.string);
	      if(xmax < old_pts[0].x)
		xmax=old_pts[0].x;
	    }
	    break;
	  }
	  break;
	case  GTH_NORMAL:
	case  GTH_LEFT:
	default:
	  switch(primi->primi.text.chattr.align.ver){
	  case GTV_HALF:
	    if(vert){
	      if(xmin > old_pts[0].x -cheight/2.)
		xmin=old_pts[0].x - cheight/2.;
	      if(xmax < old_pts[0].x + cheight/2)
		xmax=old_pts[0].x + cheight/2.;
	      if(angle>0){
		if(ymin > old_pts[0].y)
		  ymin=old_pts[0].y;
		if(ymax < (old_pts[0].y + chwidth*strlen(primi->primi.text.string)))
		  ymax=old_pts[0].y +chwidth*strlen(primi->primi.text.string);
	      }
	      else{
		if(ymax < old_pts[0].y)
		  ymax=old_pts[0].y;
		if(ymin > (old_pts[0].y + chwidth*strlen(primi->primi.text.string)))
		  ymin=old_pts[0].y +chwidth*strlen(primi->primi.text.string);
	      }
	    }
	    else{
	      if(ymin > old_pts[0].y -cheight/2.)
		ymin=old_pts[0].y - cheight/2.;
	      if(ymax < old_pts[0].y + cheight/2)
		ymax=old_pts[0].y + cheight/2.;
	      if(xmin > (old_pts[0].x))
		xmin=old_pts[0].x;
	      if(xmax < (old_pts[0].x + chwidth*strlen(primi->primi.text.string)))
		xmax=old_pts[0].x +chwidth*strlen(primi->primi.text.string);
	    }
	    break;
	  case GTV_BASE:
	  case GTV_BOTTOM:
	    if(vert){
	      if(xmin > old_pts[0].x)
		xmin=old_pts[0].x;
	      if(xmax < old_pts[0].x + cheight)
		xmax=old_pts[0].x + cheight;
	      if(angle>0){
		if(ymin > old_pts[0].y)
		  ymin=old_pts[0].y;
		if(ymax < (old_pts[0].y + chwidth*strlen(primi->primi.text.string)))
		  ymax=old_pts[0].y +chwidth*strlen(primi->primi.text.string);
	      }
	      else{
		if(ymax < old_pts[0].y)
		  ymax=old_pts[0].y;
		if(ymin > (old_pts[0].y + chwidth*strlen(primi->primi.text.string)))
		  ymin=old_pts[0].y +chwidth*strlen(primi->primi.text.string);
	      }
	    }
	    else{
	      if(ymin > old_pts[0].y)
		ymin=old_pts[0].y;
	      if(ymax < old_pts[0].y + cheight)
		ymax=old_pts[0].y + cheight;
	      if(xmin > (old_pts[0].x))
		xmin=old_pts[0].x;
	      if(xmax < (old_pts[0].x + chwidth*strlen(primi->primi.text.string)))
		xmax=old_pts[0].x +chwidth*strlen(primi->primi.text.string);
	    }
	    break;
	  case GTV_NORMAL:
	  case GTV_TOP:
	  case GTV_CAP:
	  default:
	    if(vert){
	      if(xmin > old_pts[0].x -cheight/2.)
		xmin=old_pts[0].x - cheight/2.;
	      if(xmax < old_pts[0].x)
		xmax=old_pts[0].x;
	      if(angle > 0){
		if(ymin > old_pts[0].y)
		  ymin=old_pts[0].y;
		if(ymax < (old_pts[0].y + chwidth*strlen(primi->primi.text.string)))
		  ymax=old_pts[0].y +chwidth*strlen(primi->primi.text.string);
	      }
	      else{
		if(ymax < old_pts[0].y)
		  ymax=old_pts[0].y;
		if(ymin > (old_pts[0].y + chwidth*strlen(primi->primi.text.string)))
		  ymin=old_pts[0].y +chwidth*strlen(primi->primi.text.string);
	      }
	    }
	    else{
	      if(ymin > old_pts[0].y -cheight/2.)
		ymin=old_pts[0].y - cheight/2.;
	      if(ymax < old_pts[0].y)
		ymax=old_pts[0].y;
	      if(xmin > (old_pts[0].x))
		xmin=old_pts[0].x;
	      if(xmax < (old_pts[0].x + chwidth*strlen(primi->primi.text.string)))
		xmax=old_pts[0].x +chwidth*strlen(primi->primi.text.string);
	    }
	    break;
	  }
	  break;
	}
	break;
      }
      break;
    case XGKS_MESG:
      break;
    case CELL_ARRAY:
      break;
    case GDP:
      break;
    }
    *xminp = xmin;
    *yminp = ymin;
    *xmaxp = xmax;
    *ymaxp = ymax;
    return(0);
}

void
gps_page()
{
  if (fp != NULL){
    if(PS_clipsave > 0)
      fprintf(fp, "grestore\n");
    fprintf(fp,  "showpage\n");
    fprintf(fp, "grestore\n");
  }
}

void
gps_end()
{
  if (fp != NULL){
    fprintf(fp, "%% End of XGKS output\n");
    if(EPSFILE){
      fprintf(fp,"%%%%Trailer\n");
      fprintf(fp, "%%%%BoundingBox: %d %d %d %d\n",
	      minx+18,miny+112,maxx+19,maxy+113);
      fprintf(fp,"%%%%EOF\n");
    }
    fclose(fp);
    fp = NULL;
  }
}
void gpdf_end()
{
  HPDF_STATUS stat;
  HPDF_UINT16 mode;
  HPDF_Destination dst;
  /*
    mode = HPDF_Page_GetGMode  (pdfpage);
    if(mode&HPDF_GMODE_PAGE_DESCRIPTION)
    printf("End: HPDF_GMODE_PAGE_DESCRIPTION\n");
    if(mode&HPDF_GMODE_PATH_OBJECT)
    printf("End: HPDF_GMODE_PATH_OBJECT\n");
    if(mode&HPDF_GMODE_TEXT_OBJECT)
    printf("End: HPDF_GMODE_TEXT_OBJECT\n");
    if(mode&HPDF_GMODE_CLIPPING_PATH)
    printf("End: HPDF_GMODE_CLIPPING_PATH\n");
    if(mode&HPDF_GMODE_SHADING)
    printf("End: HPDF_GMODE_SHADING\n");
    if(mode&HPDF_GMODE_INLINE_IMAGE)
    printf("End: HPDF_GMODE_INLINE_IMAGE\n");
    if(mode&HPDF_GMODE_EXTERNAL_OBJECT)
    printf("End: HPDF_GMODE_EXTERNAL_OBJECT\n");
  */
  stat = HPDF_SaveToFile (pdf, PDFFILE);
  if(stat == HPDF_INVALID_DOCUMENT)printf("HPDF_INVALID_DOCUMENT\n");
  else if(stat == HPDF_FAILD_TO_ALLOC_MEM)printf("HPDF_FAILD_TO_ALLOC_MEM\n");
  else if(stat == HPDF_FILE_IO_ERROR)printf("HPDF_FILE_IO_ERROR\n");
  HPDF_Free(pdf);
}
