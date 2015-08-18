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
 * XGKS colour related functions
 *	xXgksSetColourRep():
 *	xXgksInqColourRep():
 */

/*LINTLIBRARY*/

#include "udposix.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "gks_implem.h"


#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: xcolours.c,v 2.6 2000/08/01 16:38:25 steve Exp $";
#endif

#ifdef DEBUG
#   undef NDEBUG
#endif

#define	COLOR_UNSET	-1

/*
 * Macro for computing the color value (either GKS colour-index or X pixel-
 * value) for visuals with separate RGB palettes:
 */
#define RGB(map,tbl,color) \
    ( \
	(tbl)->rgb[IRED(map,color)].red == COLOR_UNSET \
	    ? COLOR_UNSET \
	    : RED(map, (tbl)->rgb[IRED(map,color)].red) + \
	      GRN(map, (tbl)->rgb[IGRN(map,color)].green) + \
	      BLU(map, (tbl)->rgb[IBLU(map,color)].blue) \
    )

/*
 * Macro for computing the color value (either GKS colour-index or X pixel-
 * value) regardless of the visual class.
 */
#define COLOR(map,tbl,ndx) \
    ( \
	(map)->SeparateRGB \
	    ? RGB(map,tbl,ndx) \
	    : (tbl)->color[ndx] == COLOR_UNSET \
		? COLOR_UNSET \
		: (tbl)->color[ndx] \
    )


extern int		XgksSIGIO_OFF();
extern int		XgksSIGIO_ON();
static int AllocColour=0;
static int AllocNumColours=0;
static XcMap realmap = {0,NULL,NULL};
static Colormap realdclmp = 0; /*note this is global so that an application can
                          allocate its own colormap for xgks to use*/


/*
 * Initializes the memory for a GKS <-> X mapper.
 */
    static void
XcMap_new(map)
    XcMap	   *map;		/* GKS<->X mapper (out) */
{
    assert(map != NULL);
    map->length = 0;
    map->Xpixels = NULL;
    map->GKSindexes = NULL;
}


/*
 * Allocate memory for the GKS color-index and X pixel-value vectors.
 *
 * @return		0 if and only if failure
 */
    static int
XcMap_malloc(map, size)
    XcMap*	map;
    int		size;
{
    int		status = 0;		/* failure */
    assert(map != NULL);
    assert(size > 0);
    {
	unsigned	nbytes = sizeof(long) * size;
	if ((map->Xpixels = (long *) umalloc((size_t)nbytes)) == NULL)
	{
	    (void)fprintf(stderr,
	"XcMap_malloc(): Couldn't allocate %u-bytes for index-to-pixel map.\n",
		nbytes);
	}
	else
	{
	    nbytes = sizeof(Gint) * size;
	    if ((map->GKSindexes = (Gint *) umalloc((size_t)nbytes)) != NULL)
	    {
		map->length = size;
		status = 1;		/* success */
	    }
	    else
	    {
		(void)fprintf(stderr,
       "XcMap_malloc(): Couldn't allocate %u-bytes for pixel-to-index map.\n",
		    nbytes);
		ufree((voidp)map->Xpixels);
	    }
	}
    }
    return status;
}


/*
 * WHAT:   Initialize a GKS colour-index <-> X pixel mapper.
 *
 * HOW:    Allocate storage for the mapping tables and initialize the
 *	   GKS-colour-index/X-pixel-value mapping.  Assumes single index
 *	   for RGB intensities (e.g. PseudoColor).  Doesn't enter any mapping
 *	   for ground colors.
 *
 * RETURNS:
 *	0	Error
 *	1	Success
 */
    static int
XcMap_init(map, size)
    XcMap*	map;		/* GKS<->X mapper (out) */
    int		size;		/* number of colors */
{
    int             ReturnStatus = 0;	/* failure */

    assert(map != NULL);
    //assert(map->length <= 0);
    assert(map->Xpixels == NULL);
    assert(map->GKSindexes == NULL);
    assert(size > 0);

    if (XcMap_malloc(map, size))
    {
	/* Initialize mapping table. */
	register int       i;
	for (i = 0; i < map->length; ++i)
	    map->Xpixels[i] = map->GKSindexes[i] = i;

	ReturnStatus = 1;	/* success */
    }

    return ReturnStatus;
}


/*
 * Copy GKS <-> X mapping.
 *
 * @return		0 if and only if failure
 */
    static int
XcMap_copy(toMap, fromMap)
    XcMap*	toMap;
    XcMap*	fromMap;
{
    int		status = 0;	/* failure */
    assert(toMap != NULL);
    assert(fromMap != NULL);
    {
	if (XcMap_malloc(toMap, fromMap->length))
	{
	    memcpy(
		toMap->Xpixels,
		fromMap->Xpixels,
		fromMap->length * sizeof(fromMap->Xpixels[0]));
	    memcpy(
		toMap->GKSindexes,
		fromMap->GKSindexes,
		fromMap->length * sizeof(fromMap->GKSindexes[0]));
	    status = 1;		/* success */
	}
    }
    return status;
}


/*
 * WHAT:   Set a GKS colour-index <-> X pixel mapping.
 *
 * HOW:    Store the values in the mapping tables.
 */
    static void
XcMap_enter(map, index, pixel)
    XcMap	   *map;		/* GKS<->X index mapper (in/out) */
    Gint            index;		/* GKS color-index (in) */
    unsigned long   pixel;		/* X pixel value (in) */
{
    assert(map != NULL);
    assert(pixel < map->length);
    assert(index >= 0 && index < map->length);

    map->Xpixels[index] = pixel;
    //map->GKSindexes[pixel] = index;
}


/*
 * WHAT:   Return the X pixel corresponding to a GKS colour-index.
 *
 * HOW:	   Use the GKS-to-X color-mapping to determine the pixel-value
 *	   -- either by simple lookup (for non true-color visuals) or
 *	   by computation (for true-color visuals).
 *
 * RETURNS:
 *	COLOR_UNSET	No corresponding X pixel.
 *	else		Corresponding X pixel.
 */
    static long
XcMap_pixel(map, index)
    XcMap	   *map;		/* GKS<->X index mapper (in) */
    Gint            index;		/* GKS color-index (in) */
{
    assert(map != NULL);
    assert(index >= 0 && index < map->length);

    return map->Xpixels[index];
}


/*
 * WHAT:   Return the GKS colour-index corresponding to an X pixel.
 *
 * HOW:	   Use the X-to-GKS color-mapping to determine the pixel-value
 *	   -- either by simple lookup (for non true-color visuals) or
 *	   by computation (for true-color visuals).
 *
 * RERUTNS:
 *	COLOR_UNSET	No corresponding GKS colour-index.
 *	else		Corresponding GKS colour-index.
 */
    static Gint
XcMap_index(map, pixel)
    XcMap	   *map;		/* GKS<->X index mapper (in */
    unsigned long   pixel;		/* X pixel value (in) */
{
  int i;
    assert(map != NULL);
    assert(pixel < map->length);
    for (i=0; i<map->length; i++){
      if(map->Xpixels[i]==pixel)
	return(i);
    }
    return(-1);

    //return map->GKSindexes[pixel];
}


/*
 * Returns the size of the GKS-color-index/X-pixel-value map.
 */
    static unsigned
XcMap_size(map)
    XcMap	   *map;
{
    assert(map != NULL);
    return map->length;
}


/*
 * WHAT:   Destroy a GKS colour-index <-> X pixel mapper.
 *
 * HOW:	   Free allocated storage, if appropriate, and set the relevant
 *	   members to NULL pointers.
 */
    static void
XcMap_destroy(map)
    XcMap	   *map;		/* GKS<->X mapper (in/out) */
{
    assert(map != NULL);
    assert(map->length != 0);
    assert(map->Xpixels != NULL);
    assert(map->GKSindexes != NULL);

    ufree((voidp)map->Xpixels);
    ufree((voidp)map->GKSindexes);

    map->length = 0;
    map->Xpixels = NULL;
    map->GKSindexes = NULL;
}


    int
xXgksSetColourRep(ws, idx, rep)
    WS_STATE_PTR    ws;
    int             idx;
    Gcobundl       *rep;
{
    int             ncolours;
    long            old_pixel_value;
    long            new_pixel_value;

    if (ws->ewstype != X_WIN)
	return OK;

    /* restore the Signal Default Function */

    (void) XgksSIGIO_OFF(ws->dpy);

    /* initialize some values and check the index value */

    ncolours = ws->colorCount;

    if (ncolours < 3) {
	(void) XgksSIGIO_ON(ws->dpy);		/* for black&white screen */
	return 0;
    }
    if ((idx < 0) || (idx >= ncolours)) {
	(void) XgksSIGIO_ON(ws->dpy);		/* index value out of the
						 * size of the colour map */
	return 1;
    }

    /*
     * Save the current corresponding pixel value to see if a refresh
     * will be truely necessary.
     */
    old_pixel_value = XcPixelValue(ws, idx);

     /* Set the color. */
    (void) XcSetColour(ws, idx, rep);

    /*
     * Handle a change to a ground color but only if the color has truely
     * changed (i.e. the X pixel value is different).  --SRE
     */
    new_pixel_value = XcPixelValue(ws, idx);
    if (new_pixel_value != old_pixel_value)
    {
	INPUT_DEV	*id;

	if (idx == GKS_BACKGROUND_INDEX)
	{
	    /* The background color was changed. */

	    (void) XSetWindowBackground(ws->dpy, ws->win, new_pixel_value);
	    (void) XSetBackground(ws->dpy, ws->plinegc, new_pixel_value);
	    (void) XSetBackground(ws->dpy, ws->pmarkgc, new_pixel_value);
	    (void) XSetBackground(ws->dpy, ws->fillareagc, new_pixel_value);
	    (void) XSetBackground(ws->dpy, ws->gc, new_pixel_value);

	    for (id = ws->in_dev_list; id != NULL; id = id->next)
		XSetBackground(ws->dpy, id->gc, new_pixel_value);
	}
	else if (idx == GKS_FOREGROUND_INDEX)
	{
	    /* The foreground color was changed. */

	    for (id = ws->in_dev_list; id != NULL; id = id->next)
		XSetForeground(ws->dpy, id->gc, new_pixel_value);
	}

	/* Redraw the screen (using the new pixel value) if truely necessary. */
	if (old_pixel_value != COLOR_UNSET && 
	    old_pixel_value != new_pixel_value)
	{
	    (void) xXgksXReDrawWs(ws);
	}
    }

    /* Restore the interrupt of I/O signals */

    (void) XgksSIGIO_ON(ws->dpy);

    return 0;
}


/*
 * Returns:
 *	 OK	Success
 *	!OK	Error (invalid GKS colour index: color not set)
 */
    int
xXgksInqColourRep(ws, idx, type, rep)
    WS_STATE_PTR    ws;
    int             idx;
 /* ARGSUSED */
    Gqtype          type;
    Gcobundl       *rep;
{
    int		    retcode = !OK;

    /*****************************************************************/
    /* NOTE: This routine is now only called for the GREALIZED case! */
    /*       When type == GSET, everything is handled in             */
    /*       ginqcolourrep() in colors.c  (DWO)                      */
    /*****************************************************************/

    if (ws->ewstype == X_WIN)
    {
	XColor          colour_ret;

	/* restore the Signal Default Function */
	(void) XgksSIGIO_OFF(ws->dpy);

	colour_ret.pixel = XcPixelValue(ws, idx);

	if (colour_ret.pixel != COLOR_UNSET)
	{
	    /* get the RGB values */
	    XQueryColor(ws->dpy, ws->clmp, &colour_ret);

	    /* set the returned RGB values */
	    rep->red = (Gfloat) colour_ret.red / 65535.0;
	    rep->green = (Gfloat) colour_ret.green / 65535.0;
	    rep->blue = (Gfloat) colour_ret.blue / 65535.0;

	    retcode = OK;
	}

	(void) XgksSIGIO_ON(ws->dpy);
    }

    return retcode;
}


/*
 * Initializes memory for a new GKS <-> X color mapping data structure.
 */
    void
XcNew(ws)
    WS_STATE_PTR    ws;			/* the GKS workstation (in/out) */
{
    assert(ws != NULL);
    XcMap_new(&ws->XcMap);
    &ws->XcMap;
    ws->clmp = 0;
    ws->hasColor = 0;
    ws->colorCount = 0;
    ws->predefinedColorCount = 0;
}


/*
 * Initialize the color-mapping for the given display by setting the components
 * of the workstation entry.
 *
 * @return		0 if and only if failure.
 */
XcInit(ws)
    WS_STATE_PTR	ws;		/* the GKS workstation (in/out) */
{
  int			status = 0;	/* failure */
  int                   i;

    assert(ws != NULL);
    assert(ws->dpy != NULL);

    {
	Display*        dpy = ws->dpy;	/* for convenience */
	int		screenNum = DefaultScreen(dpy);
	Visual*		defaultVisual = DefaultVisual(dpy, screenNum);
	XVisualInfo	vinfo;
	XcMap          *map;
	unsigned long mask[1];
	XColor          Xrep;		/* X color-representation */
	/* Find visual information that matches the default. */
	{
	    int		numMatched;
	    XVisualInfo	visualTemplate;
	    XVisualInfo	*visualList;

	    visualTemplate.screen = screenNum;
	    visualTemplate.visualid = defaultVisual->visualid;;
	    visualList =
		XGetVisualInfo(
		    dpy,
		    VisualScreenMask | VisualIDMask, &visualTemplate,
		    &numMatched);
	    vinfo = visualList[0];
	    XFree(visualList);
	}
	map = &realmap;
        map->length = vinfo.colormap_size;

	if(AllocColour && vinfo.colormap_size >AllocNumColours){
	  map = &realmap;
	  map->length = AllocNumColours;
	}
	else{
        AllocColour = 0;
	}
	if (vinfo.class == TrueColor || vinfo.class == DirectColor) {
	  AllocColour = 0;
	}
	if(AllocColour){

	  mask[0]=0;
	  if(XcMap_init(&realmap, map->length)){
	    ws->XcMap = realmap;
	    if(!XAllocColorCells(ws->dpy,realdclmp,FALSE,mask,0,
				 map->Xpixels,map->length)){
	      perror("error allocating color map\n");
	      AllocColour = 0;
	      ws->clmp = 0;
	      goto oops;
	    }
	    for (i = 0; i < map->length; ++i){
	      map->GKSindexes[i] = i;
	    }
	    /* Set background GKS -> WhitePixel() */
	    Xrep.red = 1;
	    Xrep.green = 1;
	    Xrep.blue = 1;
	    Xrep.flags=DoRed|DoGreen|DoBlue;
	    Xrep.pixel = map->Xpixels[0];
	    XStoreColor(ws->dpy,realdclmp,&Xrep);
	    /* Set foreground GKS -> BlackPixel() */
	    Xrep.red = 0;
	    Xrep.green = 0;
	    Xrep.blue = 0;
	    Xrep.pixel = map->Xpixels[1];
	    XStoreColor(ws->dpy,realdclmp,&Xrep);
	    ws->clmp=realdclmp;
	    ws->vinfo=vinfo;
	    status=1;
	  }
	}
    oops:
	if(AllocColour==0){
	  if(ws->clmp == 0)
	    ws->clmp = DefaultColormap(dpy, DefaultScreen(dpy));
	  assert(ws->hasColor == 0);
	  ws->hasColor = vinfo.class != StaticGray && vinfo.class != GrayScale;
	  
	  if (XcMap_init(&realmap, map->length))
	    {
	      ws->XcMap = realmap;
	      ws->colorCount = XcMap_size(&ws->XcMap);
	      ws->predefinedColorCount = ws->colorCount;
	      ws->vinfo = vinfo;
	      ws->screenNum = screenNum;
	      //XcMap_enter(
	      //	  map, GKS_BACKGROUND_INDEX, BlackPixel(dpy, screenNum));
	      //map=&ws->XcMap;
	      //XcMap_enter(
	      //	  map, GKS_FOREGROUND_INDEX, WhitePixel(dpy, screenNum));
	      Xrep.red = 1;
	      Xrep.green = 1;
	      Xrep.blue = 1;
	      Xrep.flags=DoRed|DoGreen|DoBlue;
	      if (XAllocColor(ws->dpy, ws->clmp, &Xrep)) {
		map->Xpixels[1]=Xrep.pixel;
		map->GKSindexes[Xrep.pixel]=1;
	      }
	      Xrep.red = 0;
	      Xrep.green = 0;
	      Xrep.blue = 0;
	      Xrep.flags=DoRed|DoGreen|DoBlue;
	      if (XAllocColor(ws->dpy, ws->clmp, &Xrep)) {
		map->Xpixels[0]=Xrep.pixel;
		map->GKSindexes[Xrep.pixel]=0;
	      }


	      status = 1;		/* success */
	    }
	}
    }

    return status;
}


/*
 * Copy color information from one structure to another.
 */
    void
XcCopy(toWs, fromWs)
    WS_STATE_PTR	toWs;
    WS_STATE_PTR	fromWs;
{
    toWs->clmp = fromWs->clmp;
    toWs->hasColor = fromWs->hasColor;
    toWs->colorCount = fromWs->colorCount;
    toWs->predefinedColorCount = fromWs->predefinedColorCount;
    XcMap_copy(&toWs->XcMap, &fromWs->XcMap);
}


/*
 * WHAT:   Set the color associated with a GKS color-index.
 *
 * HOW:    Get the X-server color in the workstation X colormap that is
 *         closest to the desired GKS color and store it in the mapping
 *         tables.
 *
 * INPUT:  Pointer to a GKS workstation-structure; a GKS color-index;
 *	   and a GKS representation of the desired color.
 *
 * OUTPUT: Return value:
 *		 0	Error
 *		 1	Success
 */
XcSetColour(ws, GKSindex, GKSrep)
    WS_STATE_PTR    ws;			/* the GKS workstation */
    Gint            GKSindex;		/* GKS color-index */
    Gcobundl       *GKSrep;		/* GKS color-representation */
{
    int             ReturnStatus = 0;	/* failure */
    XColor          Xrep;		/* X color-representation */
    XcMap	   *map;		/* for convenience */

    assert(ws != NULL);
    assert(ws->dpy != NULL);
    assert(GKSindex >= 0);
    assert(GKSrep != NULL);

    map = &ws->XcMap;

    /*
     * Convert a GKS colour representation [0.0-1.0] to an X color
     * representation (unsigned short).
     */
    Xrep.red = 65535 * GKSrep->red;
    Xrep.green = 65535 * GKSrep->green;
    Xrep.blue = 65535 * GKSrep->blue;

    /*
     * Allocate the X-server color closest to the desired GKS one and save the
     * GKS <-> X mapping.
     */
    if(AllocColour){
      Xrep.pixel = map->Xpixels[GKSindex];
      XStoreColor(ws->dpy,realdclmp,&Xrep);
      ReturnStatus = 1;
    }
    else{
      if (XAllocColor(ws->dpy, ws->clmp, &Xrep)) {
	ws->XcMap.Xpixels[GKSindex]=Xrep.pixel;
	//ws->XcMap.GKSindexes[Xrep.pixel] =  GKSindex;
	//XcMap_enter(map, GKSindex, Xrep.pixel);
	ReturnStatus = 1;		/* success */
      }
    }
    return ReturnStatus;
}


/*
 * WHAT:   Map a GKS color-index to an X pixel-value (i.e. color-cell
 *	   index).
 *
 * HOW:	   Use the GKS-to-X mapping to determine the pixel-value.
 *
 * INPUT:  Pointer to a GKS workstation-structure WITH A VALID DISPLAY (i.e.
 *	   with valid "dpy").
 *
 * OUTPUT: Return value:
 *		-1	Color corresponding to GKS colour index is undefined
 *		else	X pixel-value corresponding to GKS colour-index.
 *			Out-of-range GKS color-indices are mapped to the
 *			nearest limit.
 */
    long
XcPixelValue(ws, ColourIndex)
    WS_STATE_PTR    ws;			/* the GKS workstation */
    Gint            ColourIndex;	/* GKS color-index */
{
    return XcMap_pixel(&ws->XcMap, ColourIndex);;
}


/*
 * WHAT:   Map an X pixel-value (i.e. color-cell index) to an GKS colour-index.
 *
 * HOW:	   Use the X-to-GKS mapping to determine the GKS colour-index.
 *
 * INPUT:  Pointer to a GKS workstation-structure WITH A VALID DISPLAY (i.e.
 *	   with valid "dpy").
 *
 * OUTPUT: GKS colour-index corresponding to X pixel-value.  Out-of-range
 *	   X pixel-values are mapped to the nearest GKS colour-index.
 */
    Gint
XcColourIndex(ws, PixelValue)
    WS_STATE_PTR    ws;			/* the GKS workstation */
    unsigned long   PixelValue;		/* X pixel-value */
{
    if(AllocColour){
      XcMap          *map=&realmap;                /* color mapping */
      Gint            ColourIndex;        /* returned value */
      for(ColourIndex = 0; ColourIndex  <  map->length; ColourIndex++)
	if (PixelValue == map->Xpixels[ColourIndex])break;
      return(ColourIndex);
    }
    else
      return XcMap_index(&ws->XcMap, PixelValue);
}


/*
 * WHAT:   Terminate use of the mapping-table in the given workstation
 *	   structure.
 *
 * HOW:	   Deallocate X colorcells and release allocated storage.
 *
 * INPUT:  Pointer to a GKS workstation-structure WITH A NON-GARBAGE "XcMap"
 *	   MEMBER.
 *
 * OUTPUT: Success flag and modified workstation structure with NULL mapping-
 *	   tables.
 */
XcEnd(ws)
    WS_STATE_PTR    ws;			/* the GKS workstation */
{
    assert(ws != NULL);
    {
	XcMap	*map = &ws->XcMap;
	if(AllocColour){
	  XFreeColors(ws->dpy,realdclmp,map->Xpixels,map->length,0);
	}

	assert(map != NULL);
	XcMap_destroy(map);
    }
    XcNew(ws);
    return 1;
}

int
xXgkssetAllocColour(flag,numcolours)
     Bool flag;
     int numcolours;
{
    AllocColour = flag ? 1 : 0;
    if(AllocColour)AllocNumColours = numcolours;
    return((int)AllocColour);
}

int
xXgksinqAllocColour()
{
    return(AllocColour);
}

int
xXgksinqAllocNumColours()
{
    return(AllocNumColours);
}

int
xXgkssetCTB(ctb)
     Colormap ctb;
{
    realdclmp=ctb;
    return(1);
}

Colormap
  xXgksinqCTB()
{
    return(realdclmp);
}

unsigned long IveGetPixel(i)
    int i;
{
  XcMap          *map;
  
  map = &realmap;

  if(i<map->length){
    //printf("got %d\n",map->Xpixels[i]);
    return(map->Xpixels[i]);
  }
  return((unsigned long)0);
}
void IVE_TO_X(i, pixel)
     int i;
     unsigned long pixel;
{
  XcMap          *map;
  
  map = &realmap;
  map->Xpixels[i] = pixel;
  //map->GKSindexes[pixel] =  i;
}
