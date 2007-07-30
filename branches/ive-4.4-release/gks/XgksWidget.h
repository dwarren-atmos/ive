/*=================================================================
===								===
=== FILE	: XgksWidget.h					===
===								===
=== CONTENTS	: Public include file for Xgks widget v2.4	===
===								===
=== AUTHOR	: Neil Bowers, University of Leeds.		===
===								===
=================================================================*/

#ifndef _XgksWidget_h
#define _XgksWidget_h


/*-------------------------------------------------------------------------
				Resources 

 Name			Class		RepType		Default Value
 ----			-----		-------		-------------
 wsId			Count		int		1
 foreground		Foreground	Pixel		XtDefaultForeground
 invertMono		Boolean		Boolean		False
 backingStore		Boolean		Boolean		False
 softClipping		Boolean		Boolean		False

-------------------------------------------------------------------------*/


/* Xgks specific resources */

#define XtNwsId		"wsId"
#define XtNinvertMono	"invertMono"
#define XtNbackingStore	"backingStore"
#define XtNsoftClipping	"softClipping"

#define XtCCount	"Count"


/* declare specific XgksWidget class and instance datatypes */

typedef struct _XgksClassRec*	XgksWidgetClass;
typedef struct _XgksRec*	XgksWidget;


/* declare the class constant */

extern WidgetClass		xgksWidgetClass;


#endif  _XgksWidget_h
