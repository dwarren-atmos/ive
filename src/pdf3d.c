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
#include<hpdf.h>
#include <setjmp.h>

extern Widget xgks_widget;
static int minx=1200,miny=1200,maxx=0,maxy=0;

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


static float WIDTHMULT;
static float HEIGHTMULT;
static HPDF_Doc pdf;
static HPDF_Page pdfpage;
static jmp_buf myenv;

void pdferror_handler  (HPDF_STATUS   error_no,
                                       HPDF_STATUS   detail_no,
                                       void         *user_data)
{
  char *msg;
  printf ("ERROR: error_no=%04X, detail_no=%u\n", (HPDF_UINT)error_no,
          (HPDF_UINT)detail_no);
  ive_haru_status_to_errmsg(error_no, &msg);
  printf("%s\n",msg);
  longjmp(myenv, 1);
}


void pdfthreedheader(){
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
  
  /* Start PDF */
  pdf = HPDF_New (pdferror_handler, NULL);
  if (!pdf) {
    printf ("error: cannot create PdfDoc object\n");
    return;
  }
  if (setjmp(myenv)) {
    HPDF_Free (pdf);
    printf ("returned: cannot create PdfDoc object\n");
    return;
  }
  
  HPDF_SetCompressionMode (pdf, HPDF_COMP_ALL);
  pdfpage = HPDF_AddPage(pdf);
  WIDTHMULT=(8*72)/(viewport[3]-viewport[1]);
  HEIGHTMULT=(8*72)/(viewport[2]-viewport[0]);
  if(aspect!=0.){
    if(aspect<1.)
      WIDTHMULT *= aspect;
    else if(aspect>1)
      HEIGHTMULT /= aspect;
  }
  HPDF_Page_SetHeight(pdfpage,72*8 +2);
  HPDF_Page_SetWidth(pdfpage,72*8  +2);
}


static void check_error(int s){
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

void pdf3d(Widget w, Objects *obj, XmAnyCallbackStruct *call )
{
  GLfloat *feedBuffer;
  GLfloat *token,*end;
  GLint nvertices,smooth, stat;
  GLuint size;
  GLfloat dx, dy, dr, dg, db, absR, absG, absB, colormax;
  int steps,s;
  Feedback3Dcolor *vertex;
  GLfloat xstep, ystep, rstep, gstep, bstep;
  GLfloat xnext, ynext, rnext, gnext, bnext, distance;  
  GLfloat red, green, blue, alpha=0.;
  HPDF_Font pdffont;
  HPDF_UINT16 mode;
  HPDF_ExtGState gstate;
  
  int i,numtri;
  //glMatrixMode (GL_PROJECTION);

  glLoadIdentity ();
  glRotatef(xRotation,1,0,0);
  glRotatef(yRotation,0,1,0);
  glRotatef(zRotation,0,0,1);
  glOrtho(xPosition-xStretch/StretchPercent,xPosition+xStretch/StretchPercent,yPosition-yStretch/StretchPercent,
	  yPosition+yStretch/StretchPercent,-clipDistanceIVE2,clipDistanceIVE);
  s=glGetError();
  check_error(s);
  numtri=0;
  for (i=0;i<obj->objects;i++)
    if(numtri < obj->Surface[i].size)numtri=obj->Surface[i].size;
  feedBuffer=(GLfloat *)malloc((30*numtri + 10*80)*sizeof(GLfloat)); //10 vals for each vertex xyztttrgba plus 10 vals for each grid line (20/direction????)
  bzero(feedBuffer,30*numtri*sizeof(GLfloat));
  glFeedbackBuffer (30*numtri, GL_3D_COLOR, feedBuffer);
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
      // if ( IveDblBufferFlag )
     // glXSwapBuffers(XtDisplay(xgks_widget), XtWindow(xgks_widget) );
    }
  size = glRenderMode (GL_RENDER);
  s=glGetError();
  check_error(s);
  
  
  pdf=NULL;
  pdfthreedheader();
  if(!pdf){
    free(feedBuffer);
    return;
  }
  end=feedBuffer+(30*numtri);
  token=feedBuffer;
  HPDF_Page_GSave(pdfpage);
  while (token<end && *token>0) {
    switch((int)(*token)){
    case GL_BITMAP_TOKEN:
    case GL_DRAW_PIXEL_TOKEN:
    case GL_COPY_PIXEL_TOKEN:
    case GL_POINT_TOKEN:
      token++;
      vertex = (Feedback3Dcolor *) token;
      if(alpha != vertex[0].alpha){
	alpha = vertex[0].alpha;
	HPDF_Page_GRestore (pdfpage);
	HPDF_Page_GSave(pdfpage);
	gstate = HPDF_CreateExtGState(pdf);
	HPDF_ExtGState_SetAlphaFill (gstate,alpha-.1);
	HPDF_ExtGState_SetAlphaStroke (gstate,alpha-.1);
	HPDF_Page_SetExtGState(pdfpage,gstate);
      }
      HPDF_Page_SetRGBStroke(pdfpage,vertex[0].red, vertex[0].green, vertex[0].blue);
      HPDF_Page_SetRGBFill(pdfpage,vertex[0].red, vertex[0].green, vertex[0].blue);
      HPDF_Page_MoveTo(pdfpage,vertex[0].x*WIDTHMULT, vertex[0].y*HEIGHTMULT);
      HPDF_Page_Circle(pdfpage, vertex[0].x*WIDTHMULT, vertex[0].y*HEIGHTMULT, pointSize / 2.0);
      HPDF_Page_Stroke(pdfpage);
      token += 7;           /* Each vertex element in the feedback
			       buffer is 7 GLfloats. */
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
	steps = Max(1.0, colormax * distance * 0.06);
	
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
      
      if(alpha != vertex[0].alpha){
	alpha = vertex[0].alpha;
	HPDF_Page_GRestore (pdfpage);
	HPDF_Page_GSave(pdfpage);
	gstate = HPDF_CreateExtGState(pdf);
	HPDF_ExtGState_SetAlphaFill (gstate,alpha-.1);
	HPDF_ExtGState_SetAlphaStroke (gstate,alpha-.1);
	HPDF_Page_SetExtGState(pdfpage,gstate);
      }
      HPDF_Page_SetExtGState(pdfpage,gstate);
      HPDF_Page_SetLineWidth(pdfpage, 1.);
      HPDF_Page_SetLineCap(pdfpage, HPDF_BUTT_END);
      HPDF_Page_SetLineJoin(pdfpage,HPDF_MITER_JOIN);
      HPDF_Page_SetDash(pdfpage, NULL,0,0);
      HPDF_Page_SetRGBStroke(pdfpage,vertex[0].red, vertex[0].green, vertex[0].blue);
      HPDF_Page_MoveTo(pdfpage,vertex[0].x*WIDTHMULT, vertex[0].y*HEIGHTMULT);       
      
      for (i = 0; i < steps; i++) {
	xnext += xstep;
	ynext += ystep;
	rnext += rstep;
	gnext += gstep;
	bnext += bstep;
	HPDF_Page_LineTo(pdfpage, xnext*WIDTHMULT, ynext*HEIGHTMULT);
	HPDF_Page_SetRGBStroke(pdfpage,vertex[0].red, vertex[0].green, vertex[0].blue);
	HPDF_Page_MoveTo(pdfpage,xnext*WIDTHMULT, ynext*HEIGHTMULT);       
      }
      HPDF_Page_LineTo(pdfpage, vertex[1].x*WIDTHMULT, vertex[1].y*HEIGHTMULT);
      HPDF_Page_Stroke(pdfpage);
      
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
        
	if(alpha != vertex[0].alpha){
	  alpha = vertex[0].alpha;
	  HPDF_Page_GRestore (pdfpage);
	  HPDF_Page_GSave(pdfpage);
	  gstate = HPDF_CreateExtGState(pdf);
	  HPDF_ExtGState_SetAlphaFill (gstate,alpha-.1);
	  HPDF_ExtGState_SetAlphaStroke (gstate,alpha-.1);
	  HPDF_Page_SetExtGState(pdfpage,gstate);
	}
	HPDF_Page_SetExtGState(pdfpage,gstate);
	HPDF_Page_SetRGBStroke(pdfpage,vertex[0].red, vertex[0].green, vertex[0].blue);
	HPDF_Page_SetRGBFill(pdfpage,vertex[0].red, vertex[0].green, vertex[0].blue);
	HPDF_Page_SetLineWidth(pdfpage, 0.1);
	/* Draw a filled triangle. */
	HPDF_Page_MoveTo(pdfpage,vertex[0].x*WIDTHMULT, vertex[0].y*HEIGHTMULT);       
        for (i = 1; i < nvertices; i++) {
	  HPDF_Page_LineTo(pdfpage,vertex[i].x*WIDTHMULT, vertex[i].y*HEIGHTMULT);
	}
	HPDF_Page_LineTo(pdfpage,vertex[0].x*WIDTHMULT, vertex[0].y*HEIGHTMULT);
	HPDF_Page_Fill(pdfpage);
	//}
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
  HPDF_Page_GRestore (pdfpage);
  stat = HPDF_SaveToFile (pdf, "ive3D.pdf");
  free(feedBuffer);
  HPDF_Free(pdf);
}

