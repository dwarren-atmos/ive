/*=================================================================
===								===
=== FILE	: XgksP.h					===
===								===
=== CONTENTS	: Private include file for Xgks widget v2.4	===
===								===
=== AUTHOR	: Neil Bowers, University of Leeds.		===
===								===
=================================================================*/


#ifndef _XgksP_h
#define _XgksP_h

#include "XgksWidget.h"
#include <X11/CoreP.h>
#ifdef linux
#include <X11/Xos.h>
#endif


typedef struct
{
	int		dummy;
} XgksClassPart;


typedef struct _XgksClassRec
{
	CoreClassPart	core_class;
	XgksClassPart	xgks_class;
} XgksClassRec;


extern XgksClassRec xgksClassRec;


typedef struct
{
	int		ws_id;			/* workstation identifier */
	Pixel		foreground;
	Boolean		invert_mono;
	Boolean		backing_store;
	Boolean		soft_clipping;
} XgksPart;


typedef struct _XgksRec
{
	CorePart	core;
	XgksPart	xgks;
} XgksRec;


#endif  _XgksP_h
