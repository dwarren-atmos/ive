#include <ive_gl.h>
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#define DEF3DSTUFF
//myuw netid is
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
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

void reDraw(Objects *obj){//wires can probably be eliminated from the program entirely... artifact from previous version
  int blargh;
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glLoadIdentity();
  glOrtho(xPosition-xStretch,xPosition+xStretch,yPosition-yStretch,yPosition+yStretch,-clipDistanceIVE2,clipDistanceIVE); 
  glRotatef(xRotation,1,0,0);
  glRotatef(yRotation,0,1,0);
  glRotatef(zRotation,0,0,1);
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

int move_x(int in, int x){
  x= in+x;
  return x;
}//move along x direction

int move_y(int in, int y){
  y= in+y;
  return y;
}// move along y-direction

int rotate_y(int in, int rotatey){
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
}//rotate about y-axis

int rotate_x(int in, int rotatex){
  rotatex= in+rotatex;
  if(rotatex<0)
      {
	rotatex=rotatex+360;
      }
  if(rotatex>360)
      {
	rotatex=rotatex-360;
      }
  return rotatex;
}//rotate about x-axis

static int pressed=0;
static int oldx,oldy;
static int ignorewheel = 0;

void reset_wheel(){
  ignorewheel=0;
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
  static int wires;
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
    //	yRotation=rotate_y(abs(diffx)/diffx,yRotation);
    // } 
    //else{ 
    //  	if (diffy) 
    //{
    //	 xRotation=  rotate_x(abs(diffy)/diffy, xRotation);
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
      yPosition = move_y(-scalarY, yPosition);
       break;
    case XK_Down:
      yPosition = move_y(scalarY, yPosition);
      break;
    case XK_Right:
      xPosition = move_x(-scalarX, xPosition);
      break;
    case XK_Left:
      xPosition = move_x(scalarX, xPosition);
      break;
    case XK_a:
      yRotation = rotate_y(-1, yRotation);
       break;
    case XK_d:
      yRotation = rotate_y(1, yRotation);
       break;
    case XK_w:
      xRotation = rotate_x(-1, xRotation);
       break;
    case XK_s:
      xRotation = rotate_x(1, xRotation);
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
    case XK_q:
      wires=(toggle(wires));
      break;
    case XK_BackSpace:
      xRotation=0;
      yRotation=0;
      zRotation=0;
      xPosition=0;
      yPosition=0;
      xStretch=mids[0];
      yStretch=mids[1];
      clipDistanceIVE2=clipDistanceIVE;
      break;
    }
    reDraw(&IVE_Object);
    return;//above check for key press and take appropriate action before going to redraw
  }
}
 
void plot3d(mins, maxs, IVE_Objects)
     float *mins, *maxs;
     Objects *IVE_Objects;
{
  //int colorindex;
  //Gcobundl gkscolor;
  
  float crossProductArray[3][2];//used for non-smoothing normals

  int k,i,j;
  int place2BeA=0;
  int place2BeB=0;
  int place2BeC=0;
  int pt1Ref;
  int pt2Ref;
  int pt3Ref;
  float fixNegY=1, fixNegX=1, fixNegZ=1;
  
  GLfloat lightpos0[] = {-15,0,-5, 0};
  GLfloat lightpos1[] = {15,0,-15, 0};
  GLfloat lightpos2[] = {0,15,0, 0};
  GLfloat lightpos3[] = {0,0,-15, 0};
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

  FILE *file;

  mids[0]=(mins[0]+maxs[0])/2;
  mids[2]=(mins[1]+maxs[1])/2;
  mids[1]=(mins[2]+maxs[2])/2;


  //  file = fopen("pointsafter","w+");
  //fprintf(file,"triangles: %d\n", IVE_Objects->Surface[0].size);
  //fprintf(file,"normals: %d\n",IVE_Objects->NormalList[0].size);
  //fprintf(file,"min:{%f, %f, %f}\n",
  // mins[0],mins[2],mins[1]);
  //	  mins[slab_3.xaxis-1],mins[slab_3.yaxis-1],mins[slab_3.zaxis-1]);
  //fprintf(file,"max:{%f, %f, %f}\n",
  // 	  maxs[0],maxs[2],maxs[1]);
  //maxs[slab_3.xaxis-1],maxs[slab_3.yaxis-1],maxs[slab_3.zaxis-1]);
  //for (i=0;i<IVE_Objects->Surface[0].size;i++)
  //{   
  //  fprintf(file,"{%f, %f, %f}[%d] {%f, %f, %f}[%d] {%f, %f, %f}[%d]\n",
  //	      IVE_Objects->Surface[0].items[i].pt[0].xCoord,IVE_Objects->Surface[0].items[i].pt[0].yCoord,IVE_Objects->Surface[0].items[i].pt[0].zCoord,IVE_Objects->Surface[0].items[i].pt[0].normalRef,
  //	      IVE_Objects->Surface[0].items[i].pt[1].xCoord,IVE_Objects->Surface[0].items[i].pt[1].yCoord,IVE_Objects->Surface[0].items[i].pt[1].zCoord,IVE_Objects->Surface[0].items[i].pt[1].normalRef,
  //	      IVE_Objects->Surface[0].items[i].pt[2].xCoord,IVE_Objects->Surface[0].items[i].pt[2].yCoord,IVE_Objects->Surface[0].items[i].pt[2].zCoord,IVE_Objects->Surface[0].items[i].pt[2].normalRef);
  //   }
  //for(i=0; i<IVE_Objects->NormalList[0].size; i++){
  //  fprintf(file,"{%f, %f, %f}\n",IVE_Objects->NormalList[0].normal[i].xCoord,IVE_Objects->NormalList[0].normal[i].yCoord,IVE_Objects->NormalList[0].normal[i].zCoord);
  //}
  //fclose(file);

   for(i=0;i<IVE_Objects->objects;i++)
    {
      for(j=0;j<IVE_Objects->Surface[i].size;j++)
	{
	  for(k=0;k<3;k++)
	    {
	      IVE_Objects->Surface[i].items[j].pt[k].xCoord= IVE_Objects->Surface[i].items[j].pt[k].xCoord-mids[0];
	      IVE_Objects->Surface[i].items[j].pt[k].yCoord= IVE_Objects->Surface[i].items[j].pt[k].yCoord-mids[1];
	      IVE_Objects->Surface[i].items[j].pt[k].zCoord= IVE_Objects->Surface[i].items[j].pt[k].zCoord-mids[2];
	    }
	}
      IVE_Objects->Surface[i].color=i;//This line determines the color, add a line here to check to see if a color is already specified if you don't want it to default colors.
      }

  xStretch=mids[0];
  yStretch=mids[1];
  clipDistanceIVE=mids[2];

  //ginqcolorrep(WS_X,colorindex,GREALIZED,&gkscolor);
  dpy = XtDisplay(xgks_widget);
  clipDistanceIVE=clipDistanceIVE*1.1*fixNegZ;//little buffer just to make sure rounding errors don't accidentally clip something
  clipDistanceIVE2=clipDistanceIVE;//used for to avoid background clipping when intentionally clipping foreground

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


  glClearColor( 1.0, 1.0, 1.0, 1.0 );//set background to white
  glEnable(GL_DEPTH_TEST);//prevents background from displaying infront of foreground
  glDepthMask(GL_TRUE);//necessary for depth testing... forgot why...
  glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glLoadIdentity();
  glShadeModel(GL_SMOOTH);//allows for a triangle to have different colors at each vertex without this you just get one color per object
  glEnable(GL_LIGHTING);//enables lights
   glEnable(GL_LIGHT0);
   glEnable(GL_LIGHT1);
   glEnable(GL_LIGHT2);
   glEnable(GL_LIGHT3);//4 lights in total
   glLightfv(GL_LIGHT0, GL_POSITION, lightpos0);
  glLightfv(GL_LIGHT1, GL_POSITION, lightpos1);
  glLightfv(GL_LIGHT2, GL_POSITION, lightpos2);
  glLightfv(GL_LIGHT3, GL_POSITION, lightpos3);//set position of lights (using GL floats from in main function)
  glLightfv(GL_LIGHT0,GL_DIFFUSE,light0col);
  //glLightfv(GL_LIGHT1,GL_DIFFUSE,light1col);
  glLightfv(GL_LIGHT2,GL_DIFFUSE,light2col);
  glLightfv(GL_LIGHT3,GL_DIFFUSE,light3col);//set colors of lights (using floats above)
  //glLightfv(GL_LIGHT1,GL_AMBIENT,white);


  glOrtho(xPosition-xStretch,xPosition+xStretch,yPosition-yStretch,yPosition+yStretch,-clipDistanceIVE2,clipDistanceIVE);// This line (repeated in redraw) sets window size and location
  glPushMatrix();

  //      (void) glPointSize(5.0);

  printf("%f,%f,%f\n",xStretch,yStretch,clipDistanceIVE);

  for(k=0; k<nObjects;k++)
    { 
      IVE_Objects->listName[k]=glGenLists(1);
      glNewList(IVE_Objects->listName[k], GL_COMPILE);//sets a List for each file passed      
      glEnable(GL_NORMALIZE);

	  for (place2BeC=0;place2BeC<IVE_Objects->Surface[k].size;place2BeC++)
	    {
	      glBegin(GL_TRIANGLES);


	      // glNormal3f(Objects.NormalList[k].normal[place2BeC].xCoord, Objects.NormalList[k].normal[place2BeC].yCoord,Objects.NormalList[k].normal[place2BeC].zCoord);//old normal method
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
	      glEnd();

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
	  glEndList();
    }



  glPopMatrix();
 for(k=0;IVE_Objects->listName[k];k++)
    {
      if (IVE_Objects->objectOn[k]) 
	glCallList(IVE_Objects->listName[k]);
    }
   if ( IveDblBufferFlag )
     glXSwapBuffers( dpy, IveGlxWindow );
  glFlush();
}

