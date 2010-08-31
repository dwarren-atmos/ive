/*Most of this comes via: http://www.opengl.org/resources/code/samples/mjktips/Feedback.html*/
#include <volume.h>
#include <ive_gl.h>
#include <math.h>
#include <malloc.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <Xm/XmAll.h>
#include <stdlib.h>
#include <stdio.h>

extern Widget xgks_widget;
static int minx=1200,miny=1200,maxx=0,maxy=0;

#define EPS_GOURAUD_THRESHOLD 0.1  /* Lower for better (slower) */
#define EPS_SMOOTH_LINE_FACTOR 0.06 
#define Max(a,b) (((a)>(b))?(a):(b))
#define Min(a,b) (((a)<(b))?(a):(b))

typedef struct _Feedback3Dcolor {
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  GLfloat alpha;
} Feedback3Dcolor;

GLfloat pointSize;

static char *gouraudtriangleEPS[] =
{
  "/bd{bind def}bind def /triangle { aload pop   setrgbcolor  aload pop 5 3",
  "roll 4 2 roll 3 2 roll exch moveto lineto lineto closepath fill } bd",
  "/computediff1 { 2 copy sub abs threshold ge {pop pop pop true} { exch 2",
  "index sub abs threshold ge { pop pop true} { sub abs threshold ge } ifelse",
  "} ifelse } bd /computediff3 { 3 copy 0 get 3 1 roll 0 get 3 1 roll 0 get",
  "computediff1 {true} { 3 copy 1 get 3 1 roll 1 get 3 1 roll 1 get",
  "computediff1 {true} { 3 copy 2 get 3 1 roll  2 get 3 1 roll 2 get",
  "computediff1 } ifelse } ifelse } bd /middlecolor { aload pop 4 -1 roll",
  "aload pop 4 -1 roll add 2 div 5 1 roll 3 -1 roll add 2 div 3 1 roll add 2",
  "div 3 1 roll exch 3 array astore } bd /gouraudtriangle { computediff3 { 4",
  "-1 roll aload 7 1 roll 6 -1 roll pop 3 -1 roll pop add 2 div 3 1 roll add",
  "2 div exch 3 -1 roll aload 7 1 roll exch pop 4 -1 roll pop add 2 div 3 1",
  "roll add 2 div exch 3 -1 roll aload 7 1 roll pop 3 -1 roll pop add 2 div 3",
  "1 roll add 2 div exch 7 3 roll 10 -3 roll dup 3 index middlecolor 4 1 roll",
  "2 copy middlecolor 4 1 roll 3 copy pop middlecolor 4 1 roll 13 -1 roll",
  "aload pop 17 index 6 index 15 index 19 index 6 index 17 index 6 array",
  "astore 10 index 10 index 14 index gouraudtriangle 17 index 5 index 17",
  "index 19 index 5 index 19 index 6 array astore 10 index 9 index 13 index",
  "gouraudtriangle 13 index 16 index 5 index 15 index 18 index 5 index 6",
  "array astore 12 index 12 index 9 index gouraudtriangle 17 index 16 index",
  "15 index 19 index 18 index 17 index 6 array astore 10 index 12 index 14",
  "index gouraudtriangle 18 {pop} repeat } { aload pop 5 3 roll aload pop 7 3",
  "roll aload pop 9 3 roll 4 index 6 index 4 index add add 3 div 10 1 roll 7",
  "index 5 index 3 index add add 3 div 10 1 roll 6 index 4 index 2 index add",
  "add 3 div 10 1 roll 9 {pop} repeat 3 array astore triangle } ifelse } bd",
  NULL
};
static float WIDTHMULT;
static float HEIGHTMULT;

void threedheader(FILE *file){
  GLfloat clearColor[4], viewport[4];
  GLfloat lineWidth;
  int i;
  int error;
  float aspect;

  (void)getrvar_("aratio", &aspect, &error, 6);
  /* Read back a bunch of OpenGL state to help make the EPS
     consistent with the OpenGL clear color, line width, point
     size, and viewport. */
  glGetFloatv(GL_VIEWPORT, viewport);
  glGetFloatv(GL_COLOR_CLEAR_VALUE, clearColor);
  glGetFloatv(GL_LINE_WIDTH, &lineWidth);
  glGetFloatv(GL_POINT_SIZE, &pointSize);
  
  /* Emit EPS header. */
  fputs("%!PS-Adobe-2.0 EPSF-2.0\n", file);
  /* Notice %% for a single % in the fprintf calls. */
  fprintf(file, "%%%%Creator: %s (using OpenGL feedback)\n", "IVE");
  WIDTHMULT=(8*72)/(viewport[3]-viewport[1]);
  HEIGHTMULT=(11*72)/(viewport[2]-viewport[0]);
  if(aspect>0.){
    if(aspect<1.)
      WIDTHMULT *= aspect;
    else if(aspect>1)
      HEIGHTMULT /= aspect;
  }
  fprintf(file, "%%%%BoundingBox:  (atend)\n");
  
  fputs("%%EndComments\n", file);
  fputs("\n", file);
  fputs("gsave\n", file);
  fputs("\n", file);
  
  /* Output Frederic Delhoume's "gouraudtriangle" PostScript
     fragment. */
  fputs("% the gouraudtriangle PostScript fragement below is free\n", file);
  fputs("% written by Frederic Delhoume (delhoume@ilog.fr)\n", file);
  fprintf(file, "/threshold %g def\n", EPS_GOURAUD_THRESHOLD);
  for (i = 0; gouraudtriangleEPS[i]; i++) {
    fprintf(file, "%s\n", gouraudtriangleEPS[i]);
  }
  
  fprintf(file, "\n%g setlinewidth\n", lineWidth);
  
  /* Clear the background like OpenGL had it. */
  fprintf(file, "%g %g %g setrgbcolor\n",
	  clearColor[0], clearColor[1], clearColor[2]);
  fprintf(file, "18.75 112.5 translate %f %f scale\n",WIDTHMULT, HEIGHTMULT);
}

void print3DcolorVertex (GLint size, GLint *count, 
                         GLfloat *buffer)
{
  int i;
  
  printf ("  ");
  printf("(%4.2f,%4.2f,%4.2f) color (%4.2f,%4.2f,%4.2f,%4.2f)",buffer[size-(*count)],buffer[size-(*count)-1],
	 buffer[size-(*count)-2],buffer[size-(*count)-3],buffer[size-(*count)-4],buffer[size-(*count)-5],buffer[size-(*count)-6]);
  *count = *count - 7;
  
  printf ("\n");
}

void check_error(int s){
  switch(s){
  case GL_INVALID_ENUM:
    printf("GL_INVALID_ENUM\n");
    break;
  case GL_INVALID_VALUE:
    printf("GL_INVALID_VALUE\n");
    break;
  case GL_INVALID_OPERATION:
    printf("GL_INVALID_OPERATION\n");
    break;
  case GL_STACK_OVERFLOW:
    printf("GL_OVERFLOW\n");
    break;
  case GL_STACK_UNDERFLOW:
    printf("GL_UNDERFLOW\n");
    break;
  case GL_OUT_OF_MEMORY:
    printf("GL_OUT_OF_MEMORY\n");
    break;
  }
}

print3d(Widget w, Objects *obj, XmAnyCallbackStruct *call )
{
  GLfloat feedBuffer[3000000];
  GLfloat *token,*end;
  GLint nvertices,smooth;
  GLuint size;
  GLfloat dx, dy, dr, dg, db, absR, absG, absB, colormax;
  int steps,s;
  Feedback3Dcolor *vertex;
  GLfloat xstep, ystep, rstep, gstep, bstep;
  GLfloat xnext, ynext, rnext, gnext, bnext, distance;  
  GLfloat red, green, blue;
  
  int i;
  FILE *file;
  
  bzero(feedBuffer,3000000);
  file = fopen("ive3d.ps", "w");
  threedheader(file);
  //glMatrixMode (GL_PROJECTION);

  glLoadIdentity ();
  glRotatef(xRotation,1,0,0);
  glRotatef(yRotation,0,1,0);
  glRotatef(zRotation,0,0,1);
  glOrtho(xPosition-xStretch/StretchPercent,xPosition+xStretch/StretchPercent,yPosition-yStretch/StretchPercent,
	  yPosition+yStretch/StretchPercent,-clipDistanceIVE2,clipDistanceIVE);
  s=glGetError();
  check_error(s);
  glFeedbackBuffer (3000000, GL_3D_COLOR, feedBuffer);
  s=glGetError();
  check_error(s);
  (void) glRenderMode (GL_FEEDBACK);
  s=glGetError();
  check_error(s);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

  if(glIsList(terrlist)){
    glCallList(terrlist);
    s=glGetError();
    check_error(s);
  }
  for (i=0;i<obj->objects;i++)
   {
     if(glIsList(obj->listName[i]))
       {
	 glCallList(obj->listName[i]);
	 s=glGetError();
	 check_error(s);
       }
     glFlush();
     if ( IveDblBufferFlag )
       glXSwapBuffers(XtDisplay(xgks_widget), XtWindow(xgks_widget) );
   }
  size = glRenderMode (GL_RENDER);
  s=glGetError();
  check_error(s);
  
  
  end=feedBuffer+3000000;
  token=feedBuffer;
  while (token<end && *token>0) {
    switch((int)(*token)){
    case GL_BITMAP_TOKEN:
    case GL_DRAW_PIXEL_TOKEN:
    case GL_COPY_PIXEL_TOKEN:
    case GL_POINT_TOKEN:
      token++;
       vertex = (Feedback3Dcolor *) token;
       fprintf(file, "%g %g %g setrgbcolor\n", vertex[0].red, vertex[0].green, vertex[0].blue);
       fprintf(file, "%g %g %g 0 360 arc fill\n\n", vertex[0].x, vertex[0].y, pointSize / 2.0);
       token += 7;           /* Each vertex element in the feedback
				buffer is 7 GLfloats. */
       minx = Min(vertex[0].x, minx);
       maxx = Max(vertex[0].x, maxx);
       miny = Min(vertex[0].y, miny);
       maxy = Max(vertex[0].y, maxy);
       break;
    case GL_LINE_RESET_TOKEN:
    case GL_LINE_TOKEN:
      token++;
      vertex = (Feedback3Dcolor *) token;
       
      dr = vertex[1].red - vertex[0].red;
      dg = vertex[1].green - vertex[0].green;
      db = vertex[1].blue - vertex[0].blue;
      
      if (dr != 0 || dg != 0 || db != 0) {
	/* Smooth shaded line. */
	dx = vertex[1].x - vertex[0].x;
	dy = vertex[1].y - vertex[0].y;
	 distance = sqrt(dx * dx + dy * dy);
	 
	 absR = fabs(dr);
	 absG = fabs(dg);
	 absB = fabs(db);
	 colormax = Max(absR, Max(absG, absB));
	 steps = Max(1.0, colormax * distance * EPS_SMOOTH_LINE_FACTOR);
	 
	 xstep = dx / steps;
	 ystep = dy / steps;
	 
	 rstep = dr / steps;
	 gstep = dg / steps;
	 bstep = db / steps;
	 
	 xnext = vertex[0].x;
	 ynext = vertex[0].y;
	 rnext = vertex[0].red;
	 gnext = vertex[0].green;
	 bnext = vertex[0].blue;
	 
	 /* Back up half a step; we want the end points to be
	    exactly the their endpoint colors. */
	 xnext -= xstep / 2.0;
	 ynext -= ystep / 2.0;
	 rnext -= rstep / 2.0;
	 gnext -= gstep / 2.0;
	 bnext -= bstep / 2.0;
       } else {
	 /* Single color line. */
	 steps = 0;
       }
       
       fprintf(file, "%g %g %g setrgbcolor\n",
	       vertex[0].red, vertex[0].green, vertex[0].blue);
       fprintf(file, "%g %g moveto\n", vertex[0].x, vertex[0].y);
       minx = Min(vertex[0].x, minx);
       maxx = Max(vertex[0].x, maxx);
       miny = Min(vertex[0].y, miny);
       maxy = Max(vertex[0].y, maxy);
       
       for (i = 0; i < steps; i++) {
	 xnext += xstep;
	 ynext += ystep;
	 rnext += rstep;
	 gnext += gstep;
	 bnext += bstep;
	 fprintf(file, "%g %g lineto stroke\n", xnext, ynext);
	 fprintf(file, "%g %g %g setrgbcolor\n", rnext, gnext, bnext);
	 fprintf(file, "%g %g moveto\n", xnext, ynext);
       }
       fprintf(file, "%g %g lineto stroke\n", vertex[1].x, vertex[1].y);
       minx = Min(vertex[1].x, minx);
       maxx = Max(vertex[1].x, maxx);
       miny = Min(vertex[1].y, miny);
       maxy = Max(vertex[1].y, maxy);

       token += 14;          /* Each vertex element in the feedback
			      buffer is 7 GLfloats. */
       
       break;
     case GL_POLYGON_TOKEN:
       token++;
       nvertices = *token;
       token++;
       vertex = (Feedback3Dcolor *) token;

       if (nvertices > 0) {
	 red = vertex[0].red;
	 green = vertex[0].green;
	 blue = vertex[0].blue;
	 smooth = 0;
	 for (i = 1; i < nvertices; i++) {
	   if (red != vertex[i].red || green != vertex[i].green || blue != vertex[i].blue) {
	     smooth = 1;
	     break;
	   }
	 }
        
      if (smooth) {
        /* Smooth shaded polygon; varying colors at vetices. */
        int triOffset;
	
        /* Break polygon into "nvertices-2" triangle fans. */
        for (i = 0; i < nvertices - 2; i++) {
          triOffset = i * 7;
          fprintf(file, "[%g %g %g %g %g %g]",
		  vertex[0].x, vertex[i + 1].x, vertex[i + 2].x,
		  vertex[0].y, vertex[i + 1].y, vertex[i + 2].y);
          fprintf(file, " [%g %g %g] [%g %g %g] [%g %g %g] gouraudtriangle\n",
		  vertex[0].red, vertex[0].green, vertex[0].blue,
		  vertex[i + 1].red, vertex[i + 1].green, vertex[i + 1].blue,
		  vertex[i + 2].red, vertex[i + 2].green, vertex[i + 2].blue);
        }
      } else {
        /* Flat shaded polygon; all vertex colors the same. */
        fprintf(file, "newpath\n");
        fprintf(file, "%g %g %g setrgbcolor\n", red, green, blue);
	
        /* Draw a filled triangle. */
        fprintf(file, "%g %g moveto\n", vertex[0].x, vertex[0].y);
        for (i = 1; i < nvertices; i++) {
          fprintf(file, "%g %g lineto\n", vertex[i].x, vertex[i].y);
        }
        fprintf(file, "closepath fill\n\n");
      }
       }
       for (i = 0; i < nvertices; i++) {
	 minx = Min(vertex[i].x, minx);
	 maxx = Max(vertex[i].x, maxx);
	 miny = Min(vertex[i].y, miny);
	 maxy = Max(vertex[i].y, maxy);
       }
       token += nvertices * 7;  /* Each vertex element in the
				   feedback buffer is 7 GLfloats. */
       break;
       

     case GL_PASS_THROUGH_TOKEN:
       token++;
       printf ("GL_PASS_THROUGH_TOKEN\n");
       printf ("  %4.2f\n",*token);
       token++;
       break;
     default:
       token += 8;
       //printf ("ARRGH!!! Other_TOKEN\n");
       break;
     }
   }
    fputs("%Add `showpage' to the end of this file to be able to print to a printer.\n",
	  file);
    fputs("grestore\n\n", file);
    fprintf(file,"%%%%Trailer\n");
    fprintf(file, "%%%%BoundingBox: %g %g %g %g\n",
	    (minx)*WIDTHMULT,(miny)*HEIGHTMULT,(maxx+19)*WIDTHMULT,(maxy+113)*HEIGHTMULT);
    fprintf(file,"%%%%EOF\n");
    

  fclose(file);
}

