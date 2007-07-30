#line 1  "segment.fc"
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
 * FORTRAN to C binding for XGKS
 *
 * GKS Segment Manipulation Functions:
 *
 *	gcrsg_ (sgna)
 *	gclsg_ ()
 *	grensg_ (old,new)
 *	gdsg_ (sgna)
 *	gdsgwk_ (wkid, sgna)
 *	gasgwk_ (wkid,sgna)
 *	gcsgwk_ (wkid, sgna)
 *	ginsg_ (sgna, m)
 *
 * David Berkowitz
 * Bruce Haimowitz
 * TCS Development
 * Cambridge MA
 *
 * August 31 1988
 */

/*LINTLIBRARY*/

#include "udposix.h"
#include <stdlib.h>
#include "xgks.h"
#include "fortxgks.h"


/*
 *  gcrsg - Create Segment
 *
 * Parameters :
 * int *sgna;		segment name
 *
 * Returns: ANSI standard errors for this function
 * See also: ANSI standard p111.
 */
#line 67  "segment.fc"
gcrsg_(sgna)
    int       *sgna; 
{
#line 70  "segment.fc"
    debug(("Create Segment %d \n", *sgna));

    (void) gcreateseg((Gint) *sgna);
}


/*
 *  gclsg - Close Segment
 *
 * Parameters :
 *
 * Returns: ANSI standard errors for this function
 * See also: ANSI standard p111.
 */
#line 85  "segment.fc"
gclsg_() 
{
#line 87  "segment.fc"
    debug(("Close Segment \n"));

    (void) gcloseseg();
}


/*
 *  grensg - Rename Segment
 *
 * Parameters :
 * int *old; 		old segment name
 * int *new;		new segment name
 *
 * Returns: ANSI standard errors for this function
 *
 * See also: ANSI standard p.111
 */
#line 105  "segment.fc"
grensg_(old, new)
    int       *old;
    int       *new; 
{
#line 109  "segment.fc"
    debug(("Rename Segment %d %d \n", *old, *new));

    (void) grenameseg((Gint) *old, (Gint) *new);
}


/*
 *  gdsg - Delete Segment
 *
 * Parameters :
 * int *sgna;		segment name
 *
 * Returns: ANSI standard errors for this function
 *
 * See also: ANSI standard p.112
 */
#line 126  "segment.fc"
gdsg_(sgna)
    int       *sgna; 
{
#line 129  "segment.fc"
    debug(("Delete Segment %d \n", *sgna));

    (void) gdelseg((Gint) *sgna);
}


/*
 *  gdsgwk - Delete Segment from Workstation
 *
 * Parameters :
 * int *wkid;		workstation identifier
 * int *sgna;		segment name
 *
 * Returns: ANSI standard errors for this function
 *
 * See also: ANSI standard p.112
 */
#line 147  "segment.fc"
gdsgwk_(wkid, sgna)
    int       *wkid;
    int       *sgna; 
{
#line 151  "segment.fc"
    debug(("Delete Segment from Workstation %d %d \n", *wkid, *sgna));

    (void) gdelsegws((Gint) *wkid, (Gint) *sgna);
}


/*
 *  gasgwk - Associate Segment with Workstation
 *
 * Parameters :
 * int *wkid; 		workstation identifier
 * int *sgna; 		segment name
 *
 * Returns: ANSI standard errors for this function
 *
 * See also: ANSI standard p.113
 */
#line 169  "segment.fc"
gasgwk_(wkid, sgna)
    int       *wkid;
    int       *sgna; 
{
#line 173  "segment.fc"
    debug(("Associate Segment with Workstation %d %d \n", *wkid, *sgna));

    (void) gassocsegws((Gint) *wkid, (Gint) *sgna);
}


/*
 *  gcsgwk - Copy Segment to Workstation
 *
 * Parameters :
 * int *wkid; 		workstation identifier
 * int *sgna; 		segment name
 *
 * Returns: ANSI standard errors for this function
 *
 * See also: ANSI standard p.113
 */
#line 191  "segment.fc"
gcsgwk_(wkid, sgna)
    int       *wkid;
    int       *sgna; 
{
#line 195  "segment.fc"
    debug(("Copy Segment to Workstation %d %d \n", *wkid, *sgna));

    (void) gcopysegws((Gint) *wkid, (Gint) *sgna);
}


/*
 *  ginsg - Insert Segment
 *
 * Parameters :
 * int   *sgna; 	segment name
 * float *m;	 	transformation matrix
 *
 * Returns: ANSI standard errors for this function
 *
 * See also: ANSI standard p.114
 */
#line 213  "segment.fc"
ginsg_(sgna, m)
    int       *sgna;
    float     *m; 
{
#line 217  "segment.fc"
    Gfloat          input_segtran[2][3];

    debug(("Insert Segment %d  \n", *sgna));

    MOVE_ARRAY_1X6_TO_2X3(m, input_segtran)
	(void) ginsertseg((Gint) *sgna, input_segtran);
}



void
gps_init_(file)
char *file;
{
	(void)gps_init(file);
}

void
geps_init_(file)
char *file;
{
	(void)geps_init(file);
}

/*
 * gps_print(wsid, seg_id, BWPRINT, as) - Output a subset of possible segment 
 * stuff in postscript. This is not really GKS, but was added for IVE to avoid 
 * the need for SUN or DEC GKS to print.
 * FORTRAN
 */
gps_print_(ws_id, seg_id, BWPRINT, as)
    Gint            *ws_id,*seg_id,*BWPRINT; 
    Gfloat	    *as;
	           /*note: here ws_id refers to the X one*/
{

	extern gps_print();
	return(gps_print(*ws_id, *seg_id, *BWPRINT, *as));
}

gps_page_()
{
	(void)gps_page();
}

gps_init_page_(do_landscape, aspect)
    int *do_landscape;
    float *aspect;
{
	(void)gps_init_page(do_landscape, aspect);
}

gps_end_()
{
	(void)gps_end();
}











