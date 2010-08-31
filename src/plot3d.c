#include <ive_gl.h>
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#define DEF3DSTUFF
#include <Xm/XmAll.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <sys/file.h>
#include <udunits.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <malloc.h>
#include <stdlib.h>
#include <GL/glx.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <unistd.h>
#include <signal.h>
#include <volume.h>
#include <ive_widgets.h>
//#include <ive_gks.h>
extern Widget xgks_widget;

float mids[3];

#include <math.h>

static Display *dpy;

DisplayField setField()
{
  DisplayField dF;
  dF.xPos=xPosition;
  dF.xRot=xRotation;
  dF.yPos=yPosition;
  dF.yRot=yRotation;
  dF.xStr=xStretch;
  dF.yStr=yStretch;
  dF.zRot=zRotation;
  return dF;
}

int toggle(int bool){
  if (bool)
    bool=0;
  else
    bool=1;
  return bool;
} //just a toggle switch

float clip(float clipD, float changeClip)
{
  clipD=clipD+changeClip;
  return clipD;
}// changes clip distance on key press (change call to change amount of clipping)


void reDraw(Objects *obj){
  int blargh;
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glLoadIdentity();
  glRotatef(xRotation,1,0,0);
  glRotatef(yRotation,0,1,0);
  glRotatef(zRotation,0,0,1);
  glOrtho(xPosition-xStretch/StretchPercent,xPosition+xStretch/StretchPercent,yPosition-yStretch/StretchPercent,yPosition+yStretch/StretchPercent,-clipDistanceIVE2,clipDistanceIVE); 
  if(glIsList(terrlist))
    glCallList(terrlist);
  for (blargh=0;blargh<obj->objects;blargh++)
    {
      if (obj->objectOn[blargh])
	{
	  glCallList(obj->listName[blargh]);
	}
    }
  glFlush();
  if ( IveDblBufferFlag )
    glXSwapBuffers(dpy, XtWindow(xgks_widget) );
}//clears and redraws object in new location after rotate move or clip or anything

void toggleRef(Widget w,  ToggleButton *t, XmAnyCallbackStruct *call)
{
  Bool set;
  set = XmToggleButtonGetState(w);
  if(set == TRUE)
    t->O->objectOn[t->N]=1;
  else
    t->O->objectOn[t->N]=0;
  reDraw(t->O);
}

 
int move(int in, int x){
  x= in+x;
  return x;
}//move in specified direction by specified amount

int rotate(int in, int rotatey){
  rotatey= in+rotatey;
  if(rotatey<0)
      {
	rotatey=rotatey+360;
      }
  if(rotatey>360)
      {
	rotatey=rotatey-360;
      }
  return rotatey;
}//rotate about either

static int pressed=0;
static int oldx,oldy;
static int ignorewheel = 0;

void reset_wheel(){
  ignorewheel=0;
}

void reset_all_3d()
{
  int i,error;
  float mins[4],maxs[4];
  i = 4;
  (void)getrarr_("plmin_scaled",mins,&i,&error,12);
  (void)getrarr_("plmax_scaled",maxs,&i,&error,12);
  LRMult=abs((int)(maxs[0]-mins[0])/20.);
  UDMult=abs((int)(maxs[1]-mins[1])/20.);
  xRotation=0;
  yRotation=0;
  zRotation=0;
  xPosition=0;
  yPosition=0;
  xStretch=mids[0];
  yStretch=mids[1];
  StretchPercent=1;
  clipDistanceIVE2=clipDistanceIVE=0.;
  i = THREED_CONTROL_FORM;
  (void)ui_update_(&i);
}

void reset_button_3d(Widget w, Objects *obj)
{
  reset_all_3d();
  reDraw(obj);
}

void StretchFun(Widget w, Objects *obj, XmAnyCallbackStruct *call)
{
  int i;
  XmScaleGetValue(w,&i);
  StretchPercent=(float)i/100.;
  reDraw(obj);  
}

void rotaR(Widget w, Objects *obj)
{
  yRotation=rotate(1, yRotation);
  reDraw(obj);
}

void rotaL(Widget w, Objects *obj)
{
  yRotation=rotate(-1, yRotation);
	reDraw(obj);
}
void rotaLR(Widget w, Objects *obj, XmAnyCallbackStruct *call)
{
  int rot;
  XmScaleGetValue(w,&rot);
  yRotation=360-((rot+360)%360);
  reDraw(obj);
}

void rotaU(Widget w, Objects *obj)
{
  xRotation=rotate(-1, xRotation);
  reDraw(obj);
}

void rotaUD(Widget w, Objects *obj, XmAnyCallbackStruct *call)
{
  int rot;
  XmScaleGetValue(w,&rot);
  xRotation=(rot+360)%360;
  reDraw(obj);
}

void rotaD(Widget w, Objects *obj)
{
	xRotation=rotate(1, xRotation);
	reDraw(obj);
}

void movU(Widget w, Objects *obj)
{
	yPosition=move(-scalarY*UDMult, yPosition);
	reDraw(obj);
}

void movD(Widget w, Objects *obj)
{
	yPosition=move(scalarY*UDMult, yPosition);
	reDraw(obj);
}

void movR(Widget w, Objects *obj)
{
	xPosition=move(-scalarX*LRMult, xPosition);
	reDraw(obj);
}

void movL(Widget w, Objects *obj)
{
	xPosition=move(scalarX*LRMult, xPosition);
	reDraw(obj);
}
void set3dLRMult(Widget w, long data,XmAnyCallbackStruct *call )
{
  char *str;
  str = XmTextFieldGetString(w);
  LRMult= atoi(str);
  free(str);
}
void set3dUDMult(Widget w, long data,XmAnyCallbackStruct *call )
{
  char *str;
  str = XmTextFieldGetString(w);
  UDMult= atoi(str);
  free(str);
}
void ive_3dinput(w, data, event, dispatch)
     Widget w;
     void * *data;
     XEvent *event;
     Boolean *dispatch;

     //void ive_3dinput(w, data, call)
     //Widget w;
     //Objects *data;
     //XmDrawingAreaCallbackStruct *call;
{
  extern Objects IVE_Object;
  //  XEvent *event = call->event;
  XButtonEvent *xbutton = (XButtonEvent *)event;
  XKeyEvent *xkey = (XKeyEvent *)event;
  XMotionEvent *xmove = (XMotionEvent *)event;
  if (event->type == ButtonPress) {
    if(xbutton->button == Button1){
      pressed=1;
      oldx = xbutton->x;
	oldy = xbutton->y;
	return;
    }
  }
  if (event->type == ButtonRelease) { //must be release
    //    if(xbutton->button == Button1){


    //int diffx, diffy;
    // diffx = xbutton->x - oldx;
    // diffy = xbutton->y - oldy;
    // pressed = 0;
    // if(abs(diffx)>abs(diffy)){
    //	yRotation=rotate(abs(diffx)/diffx,yRotation);
    // } 
    //else{ 
    //  	if (diffy) 
    //{
    //	 xRotation=  rotate(abs(diffy)/diffy, xRotation);
    // }
    //}
    //reDraw(&IVE_Objects);
    // return;
    //}

    if(xbutton->button == Button4){
      xStretch=xStretch-1;
      yStretch=yStretch-1;
    }
    if(xbutton->button == Button5){
      xStretch=xStretch+1;
      yStretch=yStretch+1;       
    }
    reDraw(&IVE_Object);
    return;

  }
  if (event->type == KeyPress) {
    switch(XKeycodeToKeysym(XtDisplay(w), xkey->keycode, 0)){
    case XK_Up:
      yPosition = move(-scalarY, yPosition);
       break;
    case XK_Down:
      yPosition = move(scalarY, yPosition);
      break;
    case XK_Right:
      xPosition = move(-scalarX, xPosition);
      break;
    case XK_Left:
      xPosition = move(scalarX, xPosition);
      break;
    case XK_a:
      yRotation = rotate(-1, yRotation);
       break;
    case XK_d:
      yRotation = rotate(1, yRotation);
       break;
    case XK_w:
      xRotation = rotate(-1, xRotation);
       break;
    case XK_s:
      xRotation = rotate(1, xRotation);
       break;
    case XK_comma:
      clipDistanceIVE2 = clip(clipDistanceIVE2,-1);
       break;
    case XK_period:
      clipDistanceIVE2 = clip(clipDistanceIVE2,1);
       break;
    case XK_1:
      IVE_Object.objectOn[0]=toggle(IVE_Object.objectOn[0]);
      break;
    case XK_2:
      IVE_Object.objectOn[1]=toggle(IVE_Object.objectOn[1]);
      break;
    case XK_3:
      IVE_Object.objectOn[2]=toggle(IVE_Object.objectOn[2]);
      break;
    case XK_4:
      IVE_Object.objectOn[3]=toggle(IVE_Object.objectOn[3]);
      break;
    case XK_5:
      IVE_Object.objectOn[4]=toggle(IVE_Object.objectOn[4]);
      break;
    case XK_BackSpace:
      reset_all_3d();
      break;
    }
    reDraw(&IVE_Object);
    return;//above check for key press and take appropriate action before going to redraw
  }
}
static int font_inited=0;
static void
init_font(char* f)
{
   XFontStruct* font_info;
   int first;
   int last;
   
   if(font_inited)return;
   ive_font_base = glGenLists(256);
   if (!glIsList(ive_font_base)) {
      fprintf(stderr, "my_init(): Out of display lists. - Exiting.\n");
      exit (-1);
   }
   /* Load the font. */
   font_info = XLoadQueryFont(dpy, f);
   if (!font_info) {
     fprintf(stderr, "XLoadQueryFont() failed - Exiting.\n");
     exit(-1);
   }
   else {
     /* Tell GLX which font & glyphs to use. */
     first = font_info->min_char_or_byte2;
     last  = font_info->max_char_or_byte2;
     glXUseXFont(font_info->fid, first, last-first+1, ive_font_base+first);
   }
   font_inited=1;
}

void plot3d(mins, maxs, IVE_Objects)
     float *mins, *maxs;
     Objects *IVE_Objects;
{
  //int colorindex;
  //Gcobundl gkscolor;
  
  float crossProductArray[3][2];//used for non-smoothing normals

  int k,i,j;
  int saveflag,error;
  int place2BeA=0;
  int place2BeB=0;
  int place2BeC=0;
  int pt1Ref;
  int pt2Ref;
  int pt3Ref;
  float fixNegY=1, fixNegX=1, fixNegZ=1;
  struct plainpoint *plpt;

  GLfloat lightpos0[] = {-15,0,-5, 0};
  GLfloat lightpos1[] = {15,0,-15, 0};
  GLfloat lightpos2[] = {0,15,0, 0};
  GLfloat lightpos3[] = {0,0,-15, 0};
  GLfloat lightpos4[4];
  GLfloat lightpos5[4];
  GLfloat light0col[]={1.f,.8f,.8f,1.f};
  GLfloat light1col[]={.8f,1.f,.8f,1.f};
  GLfloat light2col[]={.8f,.8f,1.f,1.f};
  GLfloat light3col[]= {.8f,.8f,.8f,1.f};
  GLfloat white[] = {1.f,1.f, 1.f, 1.f};
  GLfloat black[] = {0.f,0.f,0.f,1};
  GLfloat blue[] = {.0f,.0f,.5f,1};
  GLfloat green[] ={.0f,.5f,.0f,1};
  GLfloat red[] ={.5f,.0f,.0f,1};
  GLfloat grey[] ={.2f,.2f,.2f,1};
  GLfloat yellow[] ={.5f,.5f,.0f,1};
  GLfloat purple[] ={.5f,.0f,.5f,1};
  GLfloat *IVEcolors[]={red,green,blue,grey,yellow,purple,white,black};
  GLfloat lightgrey[] ={.1f,.1f,.1f,.5};
  GLfloat ground[] = {.3,.06,.06,.1};
  FILE *file;

  dpy = XtDisplay(xgks_widget);
  init_font("fixed");
  (void)getlvar_("savflg", &saveflag, &error, 6);
  mids[0]=(mins[0]+maxs[0])/2;
  mids[2]=(mins[1]+maxs[1])/2;
  mids[1]=(mins[2]+maxs[2])/2;


  //  file = fopen("pointsafter","w+");
  //fprintf(file,"triangles: %d\n", IVE_Objects->Surface[0].size);
  //fprintf(file,"normals: %d\n",IVE_Objects->NormalList[0].size);
  //fprintf(file,"min:{%f, %f, %f}\n",
  //mins[0],mins[2],mins[1]);
  //	  mins[slab_3.xaxis-1],mins[slab_3.yaxis-1],mins[slab_3.zaxis-1]);
  //fprintf(file,"max:{%f, %f, %f}\n",
  //maxs[0],maxs[2],maxs[1]);
  //maxs[slab_3.xaxis-1],maxs[slab_3.yaxis-1],maxs[slab_3.zaxis-1]);
  //for (i=0;i<IVE_Objects->Surface[0].size;i++)
  //{   
      //fprintf(file,"{%f, %f, %f}[%d] {%f, %f, %f}[%d] {%f, %f, %f}[%d]\n",
      //      IVE_Objects->Surface[0].items[i].pt[0].xCoord,IVE_Objects->Surface[0].items[i].pt[0].yCoord,IVE_Objects->Surface[0].items[i].pt[0].zCoord,IVE_Objects->Surface[0].items[i].pt[0].normalRef,
      //      IVE_Objects->Surface[0].items[i].pt[1].xCoord,IVE_Objects->Surface[0].items[i].pt[1].yCoord,IVE_Objects->Surface[0].items[i].pt[1].zCoord,IVE_Objects->Surface[0].items[i].pt[1].normalRef,
      //      IVE_Objects->Surface[0].items[i].pt[2].xCoord,IVE_Objects->Surface[0].items[i].pt[2].yCoord,IVE_Objects->Surface[0].items[i].pt[2].zCoord,IVE_Objects->Surface[0].items[i].pt[2].normalRef);
  //}
  //for(i=0; i<IVE_Objects->NormalList[0].size; i++){
  //fprintf(file,"{%f, %f, %f}\n",IVE_Objects->NormalList[0].normal[i].xCoord,IVE_Objects->NormalList[0].normal[i].yCoord,IVE_Objects->NormalList[0].normal[i].zCoord);
  //}
  //fclose(file);

  // make sure we are not an overlay//
  if(!IVE_Objects->objectDone[0]){
    int dx,dy,dz;
    if(glIsList(terrlist))
      glDeleteLists(terrlist,1);
    glNewList(terrlist, GL_COMPILE);//sets a List for each file passed      
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, lightgrey);
    dx=(maxs[0]-mins[0])/20;
    dy=(maxs[2]-mins[2])/20; //here z means font back not up down
    dz=(maxs[1]-mins[1])/20;
    glBegin(GL_LINES);
    for (i=mins[0]; i<maxs[0]; i+=dx){
      glVertex3f(i-mids[0],mins[2]-mids[1],mins[1]-mids[2]);
      glVertex3f(i-mids[0],mins[2]-mids[1],maxs[1]-mids[2]);
    }
    for (i=mins[1]; i<maxs[1]; i+=dz){
      glVertex3f(mins[0]-mids[0],mins[2]-mids[1],i-mids[2]);
      glVertex3f(maxs[0]-mids[0],mins[2]-mids[1],i-mids[2]);
    }
    glEnd();
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, purple);
    for (i=mins[0];i<maxs[0]; i += (maxs[0]-mins[0])/3){
      char s[10];
      sprintf(s,"%d",i);
      glRasterPos3f(i-mids[0],mins[2]-mids[1],mins[1]-mids[2]);
      glPushAttrib(GL_LIST_BIT);
      glListBase(ive_font_base);
      glCallLists(strlen(s), GL_UNSIGNED_BYTE, (GLubyte *)s);
      glPopAttrib();
    }
    for (i=mins[1]; i<maxs[1]; i+=(dz*5)){
      char s[10];
      sprintf(s,"%d",i);
      glRasterPos3f(mins[0]-mids[0],mins[2]-mids[1],i-mids[2]);
      glPushAttrib(GL_LIST_BIT);
      glListBase(ive_font_base);
      glCallLists(strlen(s), GL_UNSIGNED_BYTE, (GLubyte *)s);
      glPopAttrib();
    }
    glEndList();
  }
  //  
  for(i=0;i<IVE_Objects->objects;i++)
    {
      if(!IVE_Objects->objectDone[i])
	for(j=0;j<IVE_Objects->Surface[i].size;j++)
	  {
	    for(k=0;k<3;k++)
	      {
		IVE_Objects->Surface[i].items[j].pt[k].xCoord= IVE_Objects->Surface[i].items[j].pt[k].xCoord-mids[0];
		IVE_Objects->Surface[i].items[j].pt[k].yCoord= IVE_Objects->Surface[i].items[j].pt[k].yCoord-mids[1];
		IVE_Objects->Surface[i].items[j].pt[k].zCoord= IVE_Objects->Surface[i].items[j].pt[k].zCoord-mids[2];
	      }
	    IVE_Objects->objectDone[i]=1;
	  }
      if(!strncmp(IVE_Objects->Field[i],"Terrain",7))
	IVE_Objects->Surface[i].color=-1    ;
      else
	IVE_Objects->Surface[i].color=i;//This line determines the color, add a line here to check to see if a color is already specified if you don't want it to default colors.
    }

  xStretch=mids[0];
  yStretch=mids[1];
  clipDistanceIVE=mids[2];

  //ginqcolorrep(WS_X,colorindex,GREALIZED,&gkscolor);
  clipDistanceIVE=clipDistanceIVE*2.5*fixNegZ;//little buffer just to make sure rounding errors don't accidentally clip something
  clipDistanceIVE2=1.1*clipDistanceIVE;//used for to avoid background clipping when intentionally clipping foreground

  //xStretch= 5;
  //yStretch= 5;

  /* for (k =0; k< nObjects; k++)
    {
  for(place2BeC=0; place2BeC<Objects.Surface[k].size; place2BeC++)
    {
      crossProductArray[0][0]=Objects.Surface[k].items[place2BeC].pt[1].xCoord-Objects.Surface[k].items[place2BeC].pt[0].xCoord;
      crossProductArray[1][0]=Objects.Surface[k].items[place2BeC].pt[1].yCoord-Objects.Surface[k].items[place2BeC].pt[0].yCoord;
      crossProductArray[2][0]=Objects.Surface[k].items[place2BeC].pt[1].zCoord-Objects.Surface[k].items[place2BeC].pt[0].zCoord;
      crossProductArray[0][1]=Objects.Surface[k].items[place2BeC].pt[2].xCoord-Objects.Surface[k].items[place2BeC].pt[0].xCoord;
      crossProductArray[1][1]=Objects.Surface[k].items[place2BeC].pt[2].yCoord-Objects.Surface[k].items[place2BeC].pt[0].yCoord;
      crossProductArray[2][1]=Objects.Surface[k].items[place2BeC].pt[2].zCoord-Objects.Surface[k].items[place2BeC].pt[0].zCoord;
      Objects.NormalList[k].normal[place2BeC].xCoord=((crossProductArray[1][0]*crossProductArray[2][1])-(crossProductArray[2][0]*crossProductArray[1][1]));
      Objects.NormalList[k].normal[place2BeC].yCoord=-((crossProductArray[0][0]*crossProductArray[2][1])-(crossProductArray[2][0]*crossProductArray[0][1]));
      Objects.NormalList[k].normal[place2BeC].zCoord=((crossProductArray[0][0]*crossProductArray[1][1])-(crossProductArray[1][0]*crossProductArray[0][1]));      }
    }*/
  //I saved this incase the new way of doing normals doesnt' work out... this calculates the cross product for the normal for each triangle

  
  glXMakeCurrent(dpy,IveGlxWindow,IveGlxContext);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);
  glClearColor( 1.0, 1.0, 1.0, 1.0 );//set background to white
  glEnable(GL_DEPTH_TEST);//prevents background from displaying infront of foreground
  glDepthMask(GL_TRUE);//necessary for depth testing... forgot why...
  glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glLoadIdentity();
  if(!saveflag){
    glShadeModel(GL_SMOOTH);//allows for a triangle to have different colors at each vertex without this you just get one color per object
    glEnable(GL_LIGHTING);//enables lights
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHT1);
    glEnable(GL_LIGHT2);
    glEnable(GL_LIGHT3);//4 lights in total
    glEnable(GL_LIGHT4);//5 lights in total
    glEnable(GL_LIGHT5);//6 lights in total
    glLightfv(GL_LIGHT0, GL_POSITION, lightpos0);
    glLightfv(GL_LIGHT1, GL_POSITION, lightpos1);
    glLightfv(GL_LIGHT2, GL_POSITION, lightpos2);
    glLightfv(GL_LIGHT3, GL_POSITION, lightpos3);//set position of lights (using GL floats from in main function)
    lightpos4[0]=mids[0];
    lightpos4[1]=maxs[2]+.1*(maxs[2]-mins[2]);
    lightpos4[2]=mins[1]-.1*(maxs[1]-mins[1]);
    lightpos4[3]=0;
    lightpos5[0]=mids[0];
    lightpos5[1]=mins[2]-.1*(maxs[2]-mins[2]);
    lightpos5[2]=mins[1]-.1*(maxs[1]-mins[1]);
    lightpos5[3]=0;
    glLightfv(GL_LIGHT4, GL_POSITION, lightpos4);//set position of lights (using GL floats from in main function)
    glLightfv(GL_LIGHT5, GL_POSITION, lightpos5);//set position of lights (using GL floats from in main function)
    glLightfv(GL_LIGHT0,GL_DIFFUSE,light0col);
    //glLightfv(GL_LIGHT1,GL_DIFFUSE,light1col);
    glLightfv(GL_LIGHT2,GL_DIFFUSE,light2col);
    glLightfv(GL_LIGHT3,GL_DIFFUSE,light3col);//set colors of lights (using floats above)
    glLightfv(GL_LIGHT1,GL_AMBIENT,light1col);
    glLightfv(GL_LIGHT4,GL_SPECULAR,light1col); 
    glLightfv(GL_LIGHT5,GL_SPECULAR,light1col);
  }

  glOrtho(xPosition-xStretch/StretchPercent,xPosition+xStretch/StretchPercent,yPosition-yStretch/StretchPercent,yPosition+yStretch/StretchPercent,-clipDistanceIVE2,clipDistanceIVE);// This line (repeated in redraw) sets window size and location
  glPushMatrix();

  //      (void) glPointSize(5.0);

  //printf("%f,%f,%f\n",xStretch,yStretch,clipDistanceIVE);

  for(k=0; k<nObjects;k++)
    {
      if(glIsList(IVE_Objects->listName[k]))continue;
      IVE_Objects->listName[k]=k+1;
      glNewList(IVE_Objects->listName[k], GL_COMPILE);//sets a List for each file passed  object    
      glEnable(GL_NORMALIZE);
      glBegin(GL_TRIANGLES);
      for (place2BeC=0;place2BeC<IVE_Objects->Surface[k].size;place2BeC++)
	    {


	      // glNormal3f(Objects.NormalList[k].normal[place2BeC].xCoord, Objects.NormalList[k].normal[place2BeC].yCoord,Objects.NormalList[k].normal[place2BeC].zCoord);//old normal method
	      if(IVE_Objects->Surface[k].color<0)
		glMaterialfv(GL_FRONT, GL_DIFFUSE, ground);//set color of object
	      else
		glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, IVEcolors[IVE_Objects->Surface[k].color]);//set color of object
	      
	      glNormal3f(IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[0].normalRef].xCoord/((maxs[0]-mins[0])/10),
			 IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[0].normalRef].yCoord/((maxs[1]-mins[1])/10),
			 IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[0].normalRef].zCoord/((maxs[2]-mins[2])/10));//normal for point 1
	      
	      glVertex3f(IVE_Objects->Surface[k].items[place2BeC].pt[0].xCoord,IVE_Objects->Surface[k].items[place2BeC].pt[0].yCoord,IVE_Objects->Surface[k].items[place2BeC].pt[0].zCoord);

	      glNormal3f(IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[1].normalRef].xCoord/((maxs[0]-mins[0])/10),
			 IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[1].normalRef].yCoord/((maxs[1]-mins[1])/10),
			 IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[1].normalRef].zCoord/((maxs[2]-mins[2])/10));//normal for point 2
	      
	      glVertex3f(IVE_Objects->Surface[k].items[place2BeC].pt[1].xCoord,IVE_Objects->Surface[k].items[place2BeC].pt[1].yCoord,IVE_Objects->Surface[k].items[place2BeC].pt[1].zCoord);

	      glNormal3f(IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[2].normalRef].xCoord/((maxs[0]-mins[0])/10),
			 IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[2].normalRef].yCoord/((maxs[1]-mins[1])/10),
			 IVE_Objects->NormalList[k].normal[IVE_Objects->Surface[k].items[place2BeC].pt[2].normalRef].zCoord/((maxs[2]-mins[2])/10));//normal for point 3
	      
	      glVertex3f(IVE_Objects->Surface[k].items[place2BeC].pt[2].xCoord,IVE_Objects->Surface[k].items[place2BeC].pt[2].yCoord,IVE_Objects->Surface[k].items[place2BeC].pt[2].zCoord);

	      /*  glBegin(GL_LINES);
	      glVertex3f(Objects.Surface[k].items[place2BeC].pt[0].xCoord,Objects.Surface[k].items[place2BeC].pt[0].yCoord,Objects.Surface[k].items[place2BeC].pt[0].zCoord);
	      glVertex3f(Objects.Surface[k].items[place2BeC].pt[0].xCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[0].normalRef].xCoord/((maxs[0]-mins[0])/10)),
			 Objects.Surface[k].items[place2BeC].pt[0].yCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[0].normalRef].yCoord/((maxs[1]-mins[1])/10)),
			 Objects.Surface[k].items[place2BeC].pt[0].zCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[0].normalRef].zCoord/((maxs[2]-mins[2])/10)));

	      glVertex3f(Objects.Surface[k].items[place2BeC].pt[1].xCoord,Objects.Surface[k].items[place2BeC].pt[1].yCoord,Objects.Surface[k].items[place2BeC].pt[1].zCoord);
	      glVertex3f(Objects.Surface[k].items[place2BeC].pt[1].xCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[1].normalRef].xCoord/((maxs[0]-mins[0])/10)),
			 Objects.Surface[k].items[place2BeC].pt[1].yCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[1].normalRef].yCoord/((maxs[1]-mins[1])/10)),
			 Objects.Surface[k].items[place2BeC].pt[1].zCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[1].normalRef].zCoord/((maxs[2]-mins[2])/10)));

	      glVertex3f(Objects.Surface[k].items[place2BeC].pt[2].xCoord,Objects.Surface[k].items[place2BeC].pt[2].yCoord,Objects.Surface[k].items[place2BeC].pt[2].zCoord);
	      glVertex3f(Objects.Surface[k].items[place2BeC].pt[2].xCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[2].normalRef].xCoord/((maxs[0]-mins[0])/10)),
			 Objects.Surface[k].items[place2BeC].pt[2].yCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[2].normalRef].yCoord/((maxs[1]-mins[1])/10)),
			 Objects.Surface[k].items[place2BeC].pt[2].zCoord+(Objects.NormalList[k].normal[Objects.Surface[k].items[place2BeC].pt[2].normalRef].zCoord/((maxs[2]-mins[2])/10)));
			 glEnd();*/

	    }
	      glEnd();
	  glEndList();
    }



  glPopMatrix();
  if(glIsList(terrlist)){
    printf("trun on terrain\n");
    glCallList(terrlist);
  }
 for(k=0;IVE_Objects->listName[k];k++)
    {
      if (IVE_Objects->objectOn[k]) 
	printf("turn on object %d\n",k);
	glCallList(IVE_Objects->listName[k]);
    }
   if ( IveDblBufferFlag )
     glXSwapBuffers( dpy, IveGlxWindow );
  glFlush();
  controlPad3D(IVE_Objects);
}
