#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/XmAll.h>
#include <stdio.h>
#include <string.h>
#include <locale.h>
#include <sys/param.h>
#include <math.h>
#include <volume.h>
#include <ive.h>
#include <ive_text_enum.h>
#include<properties.h>
extern Widget Box;
extern float StretchPercent;
extern void toggleRef();
extern void reset_button_3d();
extern void movU();
extern void movD();
extern void movL();
extern void movR();
extern void rotaU();
extern void rotaUD();
extern void rotaD();
extern void rotaL();
extern void rotaLR();
extern void rotaR();
extern void set3dLRMult();
extern void set3dUDMult();
extern void StretchFun();
extern void print3d();
extern void pdf3d();
XmString NewString();

ToggleButton t[10]={{NULL,0},{NULL,1},{NULL,2},{NULL,3},{NULL,4},
		    {NULL,5},{NULL,6},{NULL,7},{NULL,8},{NULL,9},};
struct
{
  
  Widget ThreeD;
  Widget TransLab;
  Widget TransLRt, TransUDt;
  Widget toggle[10];
  Widget moveU, moveD, moveL, moveR;
  Widget RotLab;
  Widget RotUD, RotLR;
  Widget ZoomLab, StrSlide;
  Widget reset,print;
  Widget scroll, Objs, UDCol, RLCol, RUDCol, RRLCol;
}controls_3D = {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		NULL,NULL,NULL};

void controlPad3D(Objects *OBJ)
{

  
  XmString str2;
  char* WidgNames[] = {"ObjectOn1", "ObjectOn2","ObjectOn3","ObjectOn4","ObjectOn5"};
  char* WidgLabels[] = {"1","2","3","4","5"};
  char str[80];
  int i;
  
  if(controls_3D.ThreeD == NULL){

    controls_3D.ThreeD = XtVaCreateManagedWidget("3D Controls",
						 xmFormWidgetClass,
						 Properties.control_form_3d,
						 //ThreeD,
						 XmNtopAttachment,
						 XmATTACH_FORM,
						 XmNleftAttachment,
						 XmATTACH_FORM,
						 XmNrightAttachment,
						 XmATTACH_FORM,
						 XmNresizable,TRUE,
						 XmNresizePolicy,
						 XmRESIZE_ANY,
						 XmNmarginWidth,0,
						 NULL);
    XtAddCallback(Properties.ind_form_3d,XmNdestroyCallback,im_gone_callback,
		  &controls_3D.ThreeD);
    //************************************
    str2 = NewString("Translate");
    controls_3D.TransLab = XtVaCreateManagedWidget("TranslateLab",
						   xmLabelWidgetClass,
						   controls_3D.ThreeD,
						   XmNlabelString,str2,
						   XmNleftAttachment,XmATTACH_FORM,
						   XmNtopAttachment,XmATTACH_FORM,
						   XmNleftOffset, 60,
						   NULL);
    
    
    
    controls_3D.moveU = XtVaCreateManagedWidget("MoveUp",
						xmArrowButtonWidgetClass,
						controls_3D.ThreeD,
						XmNarrowDirection,XmARROW_UP,
						XmNleftAttachment,XmATTACH_FORM,
						XmNtopAttachment, 
						XmATTACH_WIDGET,
						XmNtopWidget,
						controls_3D.TransLab,
						//XmNtopOffset, 10,
						XmNleftOffset, 30,
						NULL);
    XtAddCallback(controls_3D.moveU ,XmNactivateCallback,movU,OBJ);
    
    sprintf(str, "%d",UDMult);
    controls_3D.TransUDt =     XtVaCreateManagedWidget("TransUD",
						       xmTextFieldWidgetClass,
						       controls_3D.ThreeD,
						       XmNcolumns, 3,
						       XmNtopAttachment, 
						       XmATTACH_WIDGET,
						       XmNtopWidget,
						       controls_3D.moveU,
						       XmNleftAttachment,XmATTACH_FORM,
						       XmNtopOffset, 5,
						       XmNleftOffset, 25,
						       XmNvalue,str,
						       NULL);
    XtAddCallback(controls_3D.TransUDt ,XmNactivateCallback,set3dUDMult,NULL);
    XtAddCallback(controls_3D.TransUDt,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)ThreeD_CONTROL_TEXT_BOXES);
    XtAddCallback(controls_3D.TransUDt,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)ThreeD_CONTROL_TEXT_BOXES);
    XtAddCallback(controls_3D.TransUDt,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)ThreeD_CONTROL_TEXT_BOXES);
    
    
    
    controls_3D.moveD = XtVaCreateManagedWidget("MoveDown",
						xmArrowButtonWidgetClass,
						controls_3D.ThreeD,
						XmNarrowDirection,XmARROW_DOWN,
						XmNleftAttachment,XmATTACH_FORM,
						XmNtopAttachment, 
						XmATTACH_WIDGET,
						XmNtopWidget,
						controls_3D.TransUDt,
						XmNtopOffset, 5,
						XmNleftOffset,30,
						NULL);
    XtAddCallback(controls_3D.moveD ,XmNactivateCallback,movD,OBJ);
    
    
    //*******************************************************
    
    controls_3D.moveL = XtVaCreateManagedWidget("MoveLeft",
						xmArrowButtonWidgetClass,
						controls_3D.ThreeD,
						XmNarrowDirection,XmARROW_LEFT,
						XmNtopAttachment, 
						XmATTACH_WIDGET,
						XmNtopWidget,
						controls_3D.moveU,
						XmNtopOffset, 5,
						XmNleftAttachment, 
						XmATTACH_WIDGET,
						XmNleftWidget,
						controls_3D.TransUDt,
						XmNleftOffset, 20,
						NULL);
    XtAddCallback(controls_3D.moveL ,XmNactivateCallback,movL,OBJ);
    
    
    sprintf(str, "%d",LRMult);
    controls_3D.TransLRt =     XtVaCreateManagedWidget("TransLR",
						       xmTextFieldWidgetClass,
						       controls_3D.ThreeD,
						       XmNcolumns, 3,
						       XmNtopAttachment, 
						       XmATTACH_WIDGET,
						       XmNtopWidget,
						       controls_3D.moveU,
						       XmNtopOffset, 2,
						       XmNleftAttachment, 
						       XmATTACH_WIDGET,
						       XmNleftWidget,
						       controls_3D.moveL,
						       XmNleftOffset, 5,
						       XmNvalue,str,
						       NULL);
    XtAddCallback(controls_3D.TransLRt ,XmNactivateCallback,set3dLRMult,NULL);
    XtAddCallback(controls_3D.TransLRt,XmNactivateCallback,
		  cleanup_box_call,(XtPointer)ThreeD_CONTROL_TEXT_BOXES);
    XtAddCallback(controls_3D.TransLRt,XmNmodifyVerifyCallback,
		  check_num,(XtPointer)ThreeD_CONTROL_TEXT_BOXES);
    XtAddCallback(controls_3D.TransLRt,XmNmotionVerifyCallback,
		  text_box_motion,(XtPointer)ThreeD_CONTROL_TEXT_BOXES);
    
    controls_3D.moveR = XtVaCreateManagedWidget("MoveRight",
						xmArrowButtonWidgetClass,
						controls_3D.ThreeD,
						XmNarrowDirection,XmARROW_RIGHT,
						XmNtopAttachment, 
						XmATTACH_WIDGET,
						XmNtopWidget,
						controls_3D.moveU,
						XmNtopOffset, 5,
						XmNleftAttachment, 
						XmATTACH_WIDGET,
						XmNleftWidget,
						controls_3D.TransLRt,
						XmNleftOffset, 5,
						NULL);
    XtAddCallback(controls_3D.moveR ,XmNactivateCallback,movR,OBJ);
    /*
    controls_3D.rotform = XtVaCreateManagedWidget("_rf",
						  xmFormWidgetClass,
						  controls_3D.ThreeD,
						  //XmNleftAttachment,XmATTACH_FORM,
						  //XmNrightAttachment,XmATTACH_FORM,
						  XmNtopAttachment,XmATTACH_WIDGET,
						  XmNtopWidget, controls_3D.moveD,
						  //XmNorientation,XmHORIZONTAL,
						  //XmNseparatorType,XmSHADOW_ETCHED_OUT,
						  NULL);
    //*******************************************************
    */
    str2 = NewString("Rotate");
    controls_3D.RotLab = XtVaCreateManagedWidget("RotateLab",
						 xmLabelWidgetClass,
						 controls_3D.ThreeD,
						 XmNlabelString,str2,
						 XmNleftAttachment,XmATTACH_FORM,
						 XmNtopAttachment,XmATTACH_WIDGET,
						 XmNtopWidget, controls_3D.moveD,
						 XmNleftOffset, 60,
						 NULL);
    
    XmStringFree(str2);
    
    
    controls_3D.RotUD = XtVaCreateManagedWidget("RotUD",
						xmScaleWidgetClass,
						controls_3D.ThreeD,
						XmNorientation,XmVERTICAL,
						XmNshowValue,TRUE,
						XmNheight, 100,
						XmNwidth, 40,
						XmNminimum,-180,
						XmNmaximum,180,
						XmNvalue,((int)yRotation-360)%180,
						XmNleftAttachment,XmATTACH_FORM,
						XmNleftOffset, 10,
						XmNtopAttachment, 
						XmATTACH_WIDGET,
						XmNtopWidget,
						controls_3D.RotLab,
						XmNshowArrows,
						XmEACH_SIDE,
						NULL);
    
    controls_3D.RotLR =     XtVaCreateManagedWidget("RotLR",
						    xmScaleWidgetClass,
						    controls_3D.ThreeD,
						    XmNorientation,XmHORIZONTAL,
						    XmNheight, 40,
						    XmNwidth, 100,
						    XmNshowValue,TRUE,
						    XmNminimum,-180,
						    XmNmaximum,180,
						    XmNvalue,((int)xRotation-360)%180,
						    XmNcolumns, 3,
						    XmNtopAttachment, 
						    XmATTACH_WIDGET,
						    XmNtopWidget,
						    controls_3D.RotLab,
						    XmNleftAttachment,XmATTACH_WIDGET,
						    XmNleftWidget,
						    controls_3D.RotUD,
						    //XmNtopOffset, 10,
						    XmNleftOffset, 20,
						    XmNshowArrows,
						    XmEACH_SIDE,
						    NULL);
    XtAddCallback(controls_3D.RotUD ,XmNvalueChangedCallback,rotaUD,OBJ);
    XtAddCallback(controls_3D.RotLR ,XmNvalueChangedCallback,rotaLR,OBJ);
    //*************************************
    /*    controls_3D.zoomform = XtVaCreateManagedWidget("_zs",
						   xmFormWidgetClass,
						   controls_3D.ThreeD,
						   XmNleftAttachment,XmATTACH_FORM,
						   XmNrightAttachment,XmATTACH_FORM,
						   XmNtopAttachment,XmATTACH_WIDGET,
						   XmNtopWidget, controls_3D.RotUD,
						   //XmNorientation,XmHORIZONTAL,
						   //XmNseparatorType,XmSHADOW_ETCHED_OUT,
						   NULL);
    
    //***************************************
    */
    str2 = NewString("Z\nO\nO\nM");
    controls_3D.StrSlide = XtVaCreateManagedWidget("StretchSlider",
						   xmScaleWidgetClass,
						   controls_3D.ThreeD,
						   XmNorientation,XmVERTICAL,
						   XmNshowValue,TRUE,
						   XmNminimum,1,
						   XmNmaximum,300,
						   XmNheight, 180,
						   XmNwidth, 50,
						   XmNvalue,(int)(StretchPercent*100),
						   XmNleftAttachment,XmATTACH_WIDGET,
						   XmNleftWidget,
						   controls_3D.RotLR,
						   XmNtopAttachment, 
						   XmATTACH_WIDGET,
						   XmNtopWidget,
						   controls_3D.moveU,
						   XmNtopOffset, 2,
						   XmNshowArrows,
						   XmEACH_SIDE,
						   NULL);

    controls_3D.ZoomLab = XtVaCreateManagedWidget("ZoomLab",
						  xmLabelWidgetClass,
						  controls_3D.ThreeD,
						  XmNlabelString,str2,
						  XmNleftAttachment,XmATTACH_WIDGET,
						  XmNleftWidget,controls_3D.StrSlide,
						  XmNtopAttachment,XmATTACH_WIDGET,
						  XmNtopWidget, controls_3D.moveU,
						  XmNtopOffset, 60,
						  XmNlayoutDirection,XmTOP_TO_BOTTOM_MASK,
						  NULL);
    
    XmStringFree(str2);
    XtAddCallback(controls_3D.StrSlide ,XmNvalueChangedCallback,StretchFun,OBJ);
    
    
    
    controls_3D.scroll = 	  XtVaCreateManagedWidget("_Scroll",xmScrolledWindowWidgetClass,
							  controls_3D.ThreeD,
							  XmNtopAttachment,XmATTACH_WIDGET,
							  XmNtopWidget, controls_3D.RotUD,
							  XmNscrollBarDisplayPolicy, XmAS_NEEDED,
							  XmNscrollingPolicy,XmAUTOMATIC,
							  XmNtopOffset,10,
							  XmNleftAttachment,XmATTACH_FORM,
							  XmNrightAttachment,XmATTACH_FORM,
							  XmNheight,50,
							  //XmNresizeWidth,False,
							  //XmNresizeHeight,False,
							  NULL);
    
    
    controls_3D.Objs = XtVaCreateManagedWidget("_Objs",xmRowColumnWidgetClass,
					       controls_3D.scroll,
					       XmNorientation,XmHORIZONTAL,
					       XmNpacking,XmPACK_TIGHT,
					       XmNresizeWidth,False,
					       XmNresizeHeight,True,
					       XmNwidth,220,
					       XmNleftAttachment,XmATTACH_FORM,
					       XmNrightAttachment,XmATTACH_FORM,
					       NULL);
    
    str2 = NewString("Reset");
    controls_3D.reset = XtVaCreateManagedWidget("Reset",
						xmPushButtonWidgetClass,
						controls_3D.ThreeD,
						XmNlabelString,str2,
						XmNtopAttachment,XmATTACH_WIDGET,
						XmNtopWidget, controls_3D.RotLR,
						XmNleftAttachment,XmATTACH_WIDGET,
						XmNleftWidget, controls_3D.RotUD,
						XmNleftOffset,20,
						XmNtopOffset,20,
						//XmNrightAttachment,XmATTACH_FORM,
						//XmNrightOffset,10,
						NULL);
    XtAddCallback(controls_3D.reset ,XmNactivateCallback,reset_button_3d,OBJ);
    XmStringFree(str2);
    
    str2 = NewString("PDF");
    controls_3D.print = XtVaCreateManagedWidget("Print3d",
						xmPushButtonWidgetClass,
						controls_3D.ThreeD,
						XmNlabelString,str2,
						XmNtopAttachment,XmATTACH_WIDGET,
						XmNtopWidget, controls_3D.RotLR,
						XmNleftAttachment,XmATTACH_WIDGET,
						XmNleftWidget, controls_3D.reset,
						XmNleftOffset,20,
						XmNtopOffset,20,
						NULL);
    XtAddCallback(controls_3D.print ,XmNactivateCallback,pdf3d,OBJ);
    XmStringFree(str2);
    
    
    for(i=0; i < 10 && i< OBJ->objects; i++)
      {
	t[i].O=OBJ;
	str2 = NewString(OBJ->Field[i]);
	controls_3D.toggle[i] = XtVaCreateManagedWidget(WidgNames[i],
							xmToggleButtonWidgetClass,
							controls_3D.Objs,
							XmNlabelString,str2,
							XmNmarginHeight,5,
							XmNmarginWidth,5,
							XmNset, TRUE,
							NULL);
	XtAddCallback(controls_3D.toggle[i] ,XmNarmCallback,toggleRef,&t[i]);
	XtAddCallback(controls_3D.toggle[i] ,XmNdisarmCallback,toggleRef,&t[i]);
	XmStringFree(str2);
      }
    //XtManageChild(controls_3D.ThreeD);
    //XtManageChild(ThreeD);
  }
  else{
    for(i=0; i < 10; i++)
      {
	t[i].O=NULL;
	if(controls_3D.toggle[i] != NULL){
	  XtDestroyWidget(controls_3D.toggle[i]);
	  controls_3D.toggle[i]=NULL;
	}
      }
    for(i=0; i < 10 && i< OBJ->objects; i++)
      {
	t[i].O=OBJ;
	str2 = NewString(OBJ->Field[i]);
	controls_3D.toggle[i] = XtVaCreateManagedWidget(WidgNames[i],
							xmToggleButtonWidgetClass,
							controls_3D.Objs,
							XmNlabelString,str2,
							XmNmarginHeight,5,
							XmNmarginWidth,5,
							XmNset, TRUE,
							NULL);
	XtAddCallback(controls_3D.toggle[i] ,XmNarmCallback,toggleRef,&t[i]);
	XtAddCallback(controls_3D.toggle[i] ,XmNdisarmCallback,toggleRef,&t[i]);
	XmStringFree(str2);
      }
  }	  
}
