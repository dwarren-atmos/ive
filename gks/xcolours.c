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
#include "gks_implem.h"

unsigned long *ive_main_color_list;
#ifdef lint
    static void	lint_malloc(n) size_t n; { n++; }
#   define	malloc(n)	(lint_malloc((n)), 0)
#else
    static char afsid[]	= "$__Header$";
    static char rcsid[]	= "$Id: xcolours.c,v 1.7 2002/12/26 22:49:35 warren Exp $";
#endif

#ifdef DEBUG
#   undef NDEBUG
#endif

/*
 * Macros for indexing into the color-conversion tables for visuals with
 * separate RGB palettes:
 */
#define IRED(map,color)    (((color) & (map)->red_mask)  /(map)->red_mult)
#define IGRN(map,color)    (((color) & (map)->green_mask)/(map)->green_mult)
#define IBLU(map,color)    (((color) & (map)->blue_mask) /(map)->blue_mult)

/*
 * Macros for computing the intensity bit-patterns for visuals with separate
 * RGB palettes:
 */
#define RED(map,ndx)	((ndx) * (map)->red_mult)
#define GRN(map,ndx)	((ndx) * (map)->green_mult)
#define BLU(map,ndx)	((ndx) * (map)->blue_mult)

/*
 * Macro for computing the color value (either GKS colour-index or X pixel-
 * value) for visuals with separate RGB palettes:
 */
#define RGB(map,tbl,color)	(\
	RED(map, (tbl)->rgb[IRED(map,color)].red) + \
	GRN(map, (tbl)->rgb[IGRN(map,color)].green) + \
	BLU(map, (tbl)->rgb[IBLU(map,color)].blue))

/*
 * Macro for computing the color value (either GKS colour-index or X pixel-
 * value) regardless of the visual class.
 */
#define COLOR(map,tbl,ndx)	\
	((map)->SeparateRGB ? RGB(map,tbl,ndx) : (tbl)->color[ndx])


extern int		XgksSIGIO_OFF();
extern int		XgksSIGIO_ON();

static unsigned long	MaskToMult();
static int AllocColour=0;
static int AllocNumColours=0;
static XcMap realmap = {0,0,0,0,0,0,0,0,NULL,NULL};
static Colormap realdclmp = 0; /*note this is global so that an application can
			  allocate its own colormap for xgks to use*/

/*
 * AllocColour was added at the University of Washington to provide a way
 * to cause XGKS colours to behave such that gsetcolourrep  will take effect
 * immediately, rather than at the next redraw. AllocNumColors is the size
 * of the colour table to allocate
 */

    int
xXgksSetColourRep(ws, idx, rep)
    WS_STATE_PTR    ws;
    int             idx;
    Gcobundl       *rep;

{
    int             ncolours;

    if (ws->ewstype != X_WIN)
	return OK;

    /* restore the Signal Default Function */

    (void) XgksSIGIO_OFF(ws->dpy);

    /* initial some values and check the index value */

    ncolours = ws->wscolour;

    if (ncolours < 3) {
	(void) XgksSIGIO_ON(ws->dpy);		/* for black&white screen */
	return 0;
    }
    if ((idx < 0) || (idx >= ncolours)) {
	(void) XgksSIGIO_ON(ws->dpy);		/* index value out of the
	 * size of the colour map *//* c1147 d1 */
	return 1;
    }
    (void) XcSetColour(ws, idx, rep);		/* set color --SRE */

    /* Restore the interrupt of I/O signals */

    (void) XgksSIGIO_ON(ws->dpy);

    return 0;
}


    int
xXgksInqColourRep(ws, idx, type, rep)
    WS_STATE_PTR    ws;
    int             idx;

 /* ARGSUSED */
    Gqtype          type;
    Gcobundl       *rep;

{
    Display        *dpy;
    XColor          colour_ret;

    /*****************************************************************/
    /* NOTE: This routine is now only called for the GREALIZED case! */
    /*       When type == GSET, everything is handled in             */
    /*       ginqcolourrep() in colors.c  (DWO)                      */
    /*****************************************************************/

    if (ws->ewstype != X_WIN)
	return OK;

    /* restore the Signal Default Function */

    (void) XgksSIGIO_OFF(ws->dpy);

    /* check the validity of the index value */

    dpy = ws->dpy;

    /*
     * Removed check for valid idx here because that check has already been
     * done before this routine is called
     */

    /* get the RGB values */
    if(!AllocColour) /* if allocating cells don't reallocate */{
      colour_ret.pixel = XcPixelValue(ws, idx);
      XQueryColor(dpy, realdclmp, &colour_ret);
      /*      colour_ret.flags=DoRed|DoGreen|DoBlue;
	      XAllocColor(dpy, realdclmp, &colour_ret);*/
    }
    else{
      colour_ret.pixel = XcPixelValue(ws, idx);
      XQueryColor(dpy, realdclmp, &colour_ret);
    }

    /* set the returned RGB values */

    rep->red = (Gfloat) colour_ret.red / 65535.0;
    rep->green = (Gfloat) colour_ret.green / 65535.0;
    rep->blue = (Gfloat) colour_ret.blue / 65535.0;

    (void) XgksSIGIO_ON(ws->dpy);

    return 0;
}


/*
 * WHAT:   Create a new instance of the color-index mapping abstraction.
 *
 * HOW:    Set the color-mapping tables in the GKS workstation-
 *         structure to NULL.
 *
 * INPUT:  Pointer to a GKS workstation-structure with assumed garbage
 *	   in the mapping-table member.
 *
 * OUTPUT: Success flag.
 */
XcNew(ws)
    WS_STATE_PTR    ws;			/* the GKS workstation */
{
    assert(ws != NULL);
    
    ws->XcMap.NumEntries	= 0;
    ws->XcMap.ToX.rgb		= NULL;
    ws->XcMap.ToGKS.rgb		= NULL;
    ws->XcMap.ToX.color		= NULL;
    ws->XcMap.ToGKS.color	= NULL;
    return 1;
}


/*
 * WHAT:   Initialize the color-mapping for the given display.
 *
 * HOW:    Allocate storage for the forward and inverse color-mapping tables
 *	   and set them to the identity transform (except for the GKS
 *	   foreground and background colors wich will have the following
 *	   mapping:
 *		GKS background <-> X WhitePixel()
 *		GKS foreground <-> X BlackPixel().
 *	   The size of the tables are based on the number of colors for the
 *	   workstation and the visual class.
 *
 * INPUT:  Pointer to a GKS workstation-structure with assumed garbage
 *	   in the color-mapping member and a visual information structure.
 *
 * OUTPUT: Success flag and modified GKS workstation-structure (color-mapping
 *	   member is initialized).
 */
XcInit(ws, vinfo)
    WS_STATE_PTR    ws;			/* the GKS workstation */
    XVisualInfo    *vinfo;		/* visual info for window */
{
    int             ReturnStatus = 0;	/* failure */
    XcMap          *map;
    XcTable        *ToX, *ToGKS;
    unsigned        nbytes;
    XColor Xrep;

    assert(ws != NULL);
    assert(ws->dpy != NULL);
    assert(vinfo != NULL);

    if(!realdclmp)
      realdclmp = ws->dclmp;

/*    map = &ws->XcMap;*/
    map = &realmap;
    ws->XcMap = realmap;
    ToX = &map->ToX;
    ToGKS = &map->ToGKS;
    
    if(AllocColour && vinfo->colormap_size >AllocNumColours){
	map->NumEntries = AllocNumColours;
    }
    else{
	AllocColour = 0;
	map->NumEntries = vinfo->colormap_size;
    }
    ive_main_color_list=
	(unsigned long *)malloc(map->NumEntries * (sizeof(unsigned long)));
    
    if (vinfo->class == TrueColor || vinfo->class == DirectColor) {
      AllocColour = 0;
    }
    map->SeparateRGB = 0;
    
    nbytes = sizeof(unsigned long) * vinfo->colormap_size;
    
    if ((ToX->color = (unsigned long *) malloc((size_t)nbytes)) == NULL) {
      (void) fprintf(stderr,
	        "XcInit: Couldn't allocate %u-bytes for GKS-to-X color-map.\n",
		     nbytes);
    } else {
      if ((ToGKS->color = (unsigned long *) malloc((size_t)nbytes)) 
	  == NULL) {
	(void) fprintf(stderr,
	    "XcInit: Couldn't allocate %u-bytes for X-to-GKS color-map.\n",
		       nbytes);
      } else {
	register int       i,status;
	unsigned long mask[1];
	
	/* Initialize mapping table with trivial mapping */
	if(AllocColour){
	  mask[0]=0;
	  if(!XAllocColorCells(ws->dpy,realdclmp,FALSE,mask,0,
			       ToX->color,map->NumEntries)){
	    perror("error allocating color map\n");
	    AllocColour = 0;
	  }
	}
	if(AllocColour){
	  for (i = 0; i < map->NumEntries; ++i){
	    ToGKS->color[i] = i;
	    ive_main_color_list[i]=ToX->color[i];
	  }
	}
	else{
	  for (i = 0; i < vinfo->colormap_size; ++i){
	    ToX->color[i] = ToGKS->color[i] = i;
	    ive_main_color_list[i]=ToX->color[i];
	  }
	}
	
	/* Set background GKS -> WhitePixel() */
	if(AllocColour){
	  Xrep.red = 1;
	  Xrep.green = 1;
	  Xrep.blue = 1;
	  Xrep.flags=DoRed|DoGreen|DoBlue;
	  Xrep.pixel = ToX->color[0];
	  XStoreColor(ws->dpy,realdclmp,&Xrep);
	}
	else{
	  Xrep.red = 1;
	  Xrep.green = 1;
	  Xrep.blue = 1;
	  Xrep.flags=DoRed|DoGreen|DoBlue;
	  XAllocColor(ws->dpy, realdclmp, &Xrep);
	  ToX->color[0] = Xrep.pixel;
	}

	/* Set foreground GKS -> BlackPixel() */
	if(AllocColour){
	  Xrep.red = 0;
	  Xrep.green = 0;
	  Xrep.blue = 0;
	  Xrep.pixel = ToX->color[1];
	  XStoreColor(ws->dpy,realdclmp,&Xrep);
	}
	else{
	  Xrep.red = 0;
	  Xrep.green = 0;
	  Xrep.blue = 0;
	  Xrep.flags=DoRed|DoGreen|DoBlue;
	  XAllocColor(ws->dpy, realdclmp, &Xrep);
	  ToX->color[1] = Xrep.pixel;
	}

	if(!AllocColour){
	  /* Set WhitePixel() -> background GKS */
	  ToGKS->color[1] = 0;
	  
	  /* Set BlackPixel() -> foreground GKS */
	  ToGKS->color[1] = 1;
	}
	ReturnStatus = 1;
      }
    }
    

    return ReturnStatus;
}


/*
 * Compute the color-index multiplier corresponding to a color-mask.
 * See chapter 7 (Color) in the Xlib Programming Manual for a discussion
 * of these concepts.
 */
    static unsigned long
MaskToMult(mask)
    unsigned long   mask;
{
    unsigned long   mult;

    for (mult = 1; mult != 0; mult <<= 1)
	if (mask & mult)
	    break;

    return mult;
}


/*
 * WHAT:   Set the color associated with a GKS color-index.
 *
 * HOW:	   Get the X-server color in the default X colormap that is closest
 *	   to the desired GKS color and store it in the mapping tables.
 *
 * INPUT:  Pointer to a GKS workstation-structure; a GKS color-index;
 *	   and a GKS representation of the desired color.
 *
 * OUTPUT: Success flag (0 => failure) and modified color-index mapping-table.
 */
XcSetColour(ws, GKSindex, GKSrep)
    WS_STATE_PTR    ws;			/* the GKS workstation */
    Gint            GKSindex;		/* GKS color-index */
    Gcobundl       *GKSrep;		/* GKS color-representation */
{
    int             ReturnStatus = 0;	/* failure */
    XColor          Xrep;		/* X color-representation */

    assert(ws != NULL);
    assert(GKSindex >= 0);
    assert(GKSrep != NULL);

    /* Convert GKS [0.-1.] representation to X (unsigned short) rep. */
    Xrep.red = 65535 * GKSrep->red;
    Xrep.green = 65535 * GKSrep->green;
    Xrep.blue = 65535 * GKSrep->blue;
    Xrep.flags = DoRed|DoGreen|DoBlue;

    /*
     * Get the X-server color closest to the desired GKS one and save its
     * color-cell index (i.e. pixel-value) in the table.  Also, set the
     * inverse (i.e. X-to-GKS) color-transformation
     */
    if(!AllocColour){
      /*      if (XAllocColor(ws->dpy, ws->dclmp, &Xrep)) {*/
      if (XAllocColor(ws->dpy, realdclmp, &Xrep)) {
	/*	  XcMap          *map = &ws->XcMap;	*/
          XcMap          *map = &realmap;
	  XcTable        *ToX = &map->ToX;
	  XcTable        *ToGKS = &map->ToGKS;

	  if (map->SeparateRGB) {
	    ToX->rgb[IRED(map, GKSindex)].red
	      = (unsigned) IRED(map, Xrep.pixel);
	    ToX->rgb[IGRN(map, GKSindex)].green
	      = (unsigned) IGRN(map, Xrep.pixel);
	    ToX->rgb[IBLU(map, GKSindex)].blue
	      = (unsigned) IBLU(map, Xrep.pixel);
	    
	    ToGKS->rgb[IRED(map, Xrep.pixel)].red
	      = (unsigned) IRED(map, GKSindex);
	    ToGKS->rgb[IGRN(map, Xrep.pixel)].green
	      = (unsigned) IGRN(map, GKSindex);
	    ToGKS->rgb[IBLU(map, Xrep.pixel)].blue
	      = (unsigned) IBLU(map, GKSindex);
	    ToX->rgb[IRED(map, GKSindex)].red=Xrep.red;
	    ToX->rgb[IRED(map, GKSindex)].green=Xrep.green;
	    ToX->rgb[IRED(map, GKSindex)].blue=Xrep.blue;

	    ToGKS->rgb[IRED(map, GKSindex)].red=Xrep.red;
	    ToGKS->rgb[IRED(map, GKSindex)].green=Xrep.green;
	    ToGKS->rgb[IRED(map, GKSindex)].blue=Xrep.blue;
	    ive_main_color_list[GKSindex]=Xrep.pixel;
	  } else {
	    ToX->color[GKSindex] = Xrep.pixel;
	    ToGKS->color[GKSindex] = (unsigned long) GKSindex;
	  }

	  ReturnStatus = 1;

    } else {
	  static int	SecondTry	= 0;	/* second attempt? */

	  if (SecondTry) {
	      (void) fprintf(stderr,
		    "XcSetColour: Couldn't allocate X color: RGB = %u %u %u.\n",
			   Xrep.red, Xrep.green, Xrep.blue);
	  } else {
	      unsigned long	pixel	= XcPixelValue(ws, GKSindex);
	      unsigned long	planes	= 0;
#if defined(IRIX)
	      XFreeColors(ws->dpy, ws->dclmp, &pixel, 1, planes);
#else
	      if (XFreeColors(ws->dpy, ws->dclmp, &pixel, 1, planes) == 0) {
		  SecondTry	= 1;
		  ReturnStatus	= XcSetColour(ws, GKSindex, GKSrep);
		  SecondTry	= 0;
	      }
#endif
	  }
      }
    }
    else{
/*	XcMap          *map = &ws->XcMap;*/
	XcMap          *map = &realmap;
	XcTable        *ToX = &map->ToX;
	Xrep.pixel = ToX->color[GKSindex];
	Xrep.flags = DoRed|DoGreen|DoBlue;
	XStoreColor(ws->dpy,realdclmp,&Xrep);
    }

    return ReturnStatus;
}


/*
 * WHAT:   Map a GKS color-index to an X pixel-value (i.e. color-cell
 *	   index).
 *
 * HOW:	   Use the GKS-to-X color-mapping to determine the pixel-value
 *	   -- either by simple lookup (for non true-color visuals) or
 *	   by computation (for true-color visuals).
 *
 * INPUT:  Pointer to a GKS workstation-structure WITH A VALID DISPLAY (i.e.
 *	   with valid "dpy").
 *
 * OUTPUT: X pixel-value corresponding to GKS color-index.  Out-of-range
 *	   GKS color-indices are mapped to the nearest X pixel-value.
 */
    unsigned long
XcPixelValue(ws, ColourIndex)
    WS_STATE_PTR    ws;			/* the GKS workstation */
    Gint            ColourIndex;	/* GKS color-index */
{
    XcMap          *map;		/* color mapping */
    unsigned long   PixelValue;		/* returned value */

    assert(ws != NULL);
    assert(ColourIndex >= 0);

/*    map = &ws->XcMap;*/
    map = &realmap;

    assert(map != NULL);

    if (ColourIndex < 0) {
	ColourIndex = 0;
    } else if (ColourIndex >= map->NumEntries) {
	ColourIndex = map->NumEntries - 1;
    }
    PixelValue = (unsigned long) COLOR(map, &map->ToX, ColourIndex);

    return PixelValue;
}


/*
 * WHAT:   Map an X pixel-value (i.e. color-cell index) to an GKS colour-index.
 *
 * HOW:	   Use the X-to-GKS color-mapping to determine the pixel-value
 *	   -- either by simple lookup (for non true-color visuals) or
 *	   by computation (for true-color visuals).
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
    XcMap          *map;		/* color mapping */
    Gint            ColourIndex;	/* returned value */

    assert(ws != NULL);

/*    map = &ws->XcMap;*/
    map = &realmap;

    assert(map != NULL);

    if(AllocColour){
	for(ColourIndex = 0; ColourIndex  <  map->NumEntries; ColourIndex++)
 	  if (PixelValue == map->ToX.color[ColourIndex])break;

	/* note: still returns bad pix val if no match */
    }
    else{
	if (PixelValue >= map->NumEntries)
	  PixelValue = map->NumEntries - 1;

	ColourIndex = (unsigned long) COLOR(map, &map->ToGKS, PixelValue);
    }
    return ColourIndex;
}


/*
 * WHAT:   Terminate use of the mapping-table in the given workstation
 *	   structure.
 *
 * HOW:	   Free-up allocated storage, if necessary, and set the mapping-tables
 *	   to NULL pointers.
 *
 * INPUT:  Pointer to a GKS workstation-structure WITH A NON-GARBAGE "XcTable"
 *	   MEMBER.
 *
 * OUTPUT: Success flag and modified workstation structure with NULL mapping-
 *	   tables.
 */
XcEnd(ws)
    WS_STATE_PTR    ws;			/* the GKS workstation */
{
    XcMap          *map;
    XcTable        *ToX, *ToGKS;

    assert(ws != NULL);

/*    map = &ws->XcMap;*/
    map = &realmap;
    ToX = &map->ToX;
    ToGKS = &map->ToGKS;

    if (map->SeparateRGB) {
	if (ToX->rgb != NULL) {
	    ufree((voidp)ToX->rgb);
	    ToX->rgb = NULL;
	}
	if (ToGKS->rgb != NULL) {
	    ufree((voidp) ToGKS->rgb);
	    ToGKS->rgb = NULL;
	}
    } else {
	if (ToX->color != NULL) {
	    if(AllocColour)
	      XFreeColors(ws->dpy,realdclmp,ToX->color,map->NumEntries,0);
	    ufree((voidp) ToX->color);
	    ToX->color = NULL;
	}
	if (ToGKS->color != NULL) {
	    ufree((voidp)ToGKS->color);
	    ToGKS->color = NULL;
	}
    }

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
  if(i<realmap.NumEntries){
    if(AllocColour){
      return(ive_main_color_list[i]);
    }
    else{
      XcMap          *map;
      XcTable        *ToX;
  
      map = &realmap;
      ToX = &map->ToX;
      return(ToX->colour[i]);
    }
  }
  return((unsigned long)0);
}

void IVE_TO_X(i, pixel)
     int i;
     unsigned long pixel;
{
  XcMap          *map;
  XcTable        *ToX;
  
  map = &realmap;
  ToX = &map->ToX;
  ToX->color[i] = pixel;
}

