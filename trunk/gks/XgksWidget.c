static char rcsid[] = "$Id: XgksWidget.c,v 1.5 2002/08/09 19:57:38 warren Exp $";
/* $Id: XgksWidget.c,v 1.5 2002/08/09 19:57:38 warren Exp $
 *
 * $Log: XgksWidget.c,v $
 * Revision 1.5  2002/08/09 19:57:38  warren
 * Fixed for Linux and tested with DEC sun etc.
 * most items inside #ifdef's, or were general cleanup/fixes
 *
 * Revision 1.4  1996/10/18 14:53:57  harry
 * Fix declarations.
 *
 */

/*=================================================================
===								===
=== FILE	: XgksWidget.c					===
===								===
=== CONTENTS	: Implementation file for Xgks widget v2.4	===
===								===
=== AUTHOR	: Neil Bowers, University of Leeds.		===
===								===
=================================================================*/

#include <stdio.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <xgks.h>

#include "XgksWidgetP.h"


#define offset(field) XtOffset(XgksWidget, xgks.field)
static XtResource resources[] =
{
        {XtNwsId, XtCCount, XtRInt, sizeof(int),
		offset(ws_id), XtRString, "1"},

	{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
		offset(foreground), XtRString, "XtDefaultForeground"},

	{XtNinvertMono, XtCBoolean, XtRBoolean, sizeof(Boolean),
		offset(invert_mono), XtRImmediate, (caddr_t)False},

	{XtNbackingStore, XtCBoolean, XtRBoolean, sizeof(Boolean),
		offset(backing_store), XtRImmediate, (caddr_t)False},

	{XtNsoftClipping, XtCBoolean, XtRBoolean, sizeof(Boolean),
		offset(soft_clipping), XtRImmediate, (caddr_t)False}

};
#undef offset

static void	XgksRealize(),
		XgksDestroy();
static Boolean	XgksSetValues();


#define SuperClass	((WidgetClass) &widgetClassRec)

XgksClassRec xgksClassRec =
{
	/* CoreClassPart */
	{
		/* core fields */
		/* superclass		 */ SuperClass,
		/* class_name		 */ "Xgks",
		/* widget_size		 */ sizeof(XgksRec),
		/* class_initialize	 */ NULL,
		/* class_part_initialize */ NULL,
		/* class_inited	 	 */ FALSE,
		/* initialize		 */ NULL,
		/* initialize_hook	 */ NULL,
		/* realize		 */ XgksRealize,
		/* actions		 */ NULL,
		/* num_actions		 */ 0,
		/* resources		 */ resources,
		/* num_resources	 */ XtNumber(resources),
		/* xrm_class		 */ NULLQUARK,
		/* compress_motion	 */ TRUE,
		/* compress_exposure	 */ TRUE,
		/* compress_enterleave	 */ TRUE,
		/* visible_interest	 */ FALSE,
		/* destroy		 */ XgksDestroy,
		/* resize		 */ NULL,
		/* expose		 */ NULL,
		/* set_values		 */ XgksSetValues,
		/* set_values_hook	 */ NULL,
		/* set_values_almost	 */ XtInheritSetValuesAlmost,
		/* get_values_hook	 */ NULL,
		/* accept_focus	 	 */ NULL,
		/* version		 */ XtVersion,
		/* callback_private	 */ NULL,
		/* tm_table		 */ NULL,
		/* query_geometry	 */ XtInheritQueryGeometry,
		/* display_accelerator	 */ XtInheritDisplayAccelerator,
		/* extension		 */ NULL
	},
	/* XgksClassPart */
	{
		/* dummy		 */ 0
	}
};

WidgetClass xgksWidgetClass = (WidgetClass) &xgksClassRec;


/*--------------------------------------------------------
-- XgksRealize()					--
--	Create an XGKS workstation and stuff		--
--	its window in the widget.			--
--------------------------------------------------------*/
static void
XgksRealize(widget, value_mask, attributes)
	Widget			widget;
	Mask			*value_mask;
	XSetWindowAttributes	*attributes;
{
  int XgksWidgetOpenWs();
	XgksWidget	xw = (XgksWidget) widget;


	if (XgksWidgetOpenWs(xw, xw->xgks.ws_id,
			     DisplayString(XtDisplay(xw)),NULL) != 0)
		XtError("XgksWidgetError: xw_gopenws failed");
}



/*--------------------------------------------------------
-- XgksDestroy()					--
--	Destroy Xgks widget and workstation.		--
--------------------------------------------------------*/
static void
XgksDestroy(widget)
	Widget	widget;
{
  int XgksWidgetCloseWs();
	XgksWidget	gw = (XgksWidget) widget;


	if (XgksWidgetCloseWs(gw,gw->xgks.ws_id) != 0)
		XtError("XgksWidgetError: xw_closews failed");
}


/*--------------------------------------------------------
-- XgksSetValues()					--
--	Xgks Widget's set-values proc.			--
--------------------------------------------------------*/
static Boolean
XgksSetValues(current,request,new,args,num_args)
	Widget		current;
	Widget		request;
	Widget		new;
	ArgList		args;
	Cardinal	*num_args;
{
	XgksWidget	new_gw = (XgksWidget) new,
			req_gw = (XgksWidget) request,
			cur_gw = (XgksWidget) current;


	if (!XtIsRealized(current))
		return False;

	if (req_gw->xgks.ws_id  != cur_gw->xgks.ws_id)
	{
		XtWarningMsg(
			"IllegalResourceSetting",
			"ModifiedWsId",
			"XgksWidgetError",
			"XgksWidget - wsId resource cannot be modified after widget is realized",
			NULL,0);
		new_gw->xgks.ws_id = cur_gw->xgks.ws_id;
	}

	return False;
}
