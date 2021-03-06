#include<GL/gl.h>
#include<GL/glx.h>
#ifndef MEMDBG
#include <stdlib.h>
#endif
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/XmAll.h>
#include <X11/keysym.h>
#include <xgks.h>
#include <ive_gks.h>

#define labellist 200

#ifndef IVE_CUBE_PLOTS
extern int ive_checkcube();
extern void ive_scattercube();
extern void ive_trianglecube();
extern void ive_draw_box();
extern int curdisplist;
extern struct{
  float x,y,z;
} center;

#endif

#define volume_dep(a,b,c) *(volume + (a) + (((b) + ((c) * ni)) * nj))
#define min_color user_colors_.min_user_color
#define max_color user_colors_.max_user_color

GLuint terrlist;

//extern GLXContext IveGlxContext;
//extern Widget gl_widget;

extern struct{
    int locked, dimavg, phys;
    float avgmin, avgmax, *slab;
    float slope, intercept, vlocked, *data;
    int xaxis, yaxis, zaxis;
    char datfil[256], field[256];
} slab_3;

struct plainpoint {
    float x,y,z;
};

struct voxind{float x,y,z;
  int c;} *rendervol, *tervol;

GLuint ive_font_base;

typedef struct {float xCoord, yCoord, zCoord; unsigned int normalRef;} Point;

typedef struct {Point pt[3];} Triangle;

typedef struct {int size; Point *normal;}NormalList; 
               //one of these per object

typedef struct {
  int size;
  Triangle *items;
  int color;
} Surface; //one of these per file passed

typedef struct {
  int objects;
  Surface *Surface;
  NormalList *NormalList;
  int objectOn[10];
  int objectDone[10];
  int objectPType[10]; /*0=isosurface,1=scatter,2=wireframe*/
  GLuint listName[10];
  char *Field[10];
}Objects;

typedef struct {
  int x,y;
  GLfloat *points;
}terrmesh;

typedef struct
{
	Objects *O;
	int N;
}ToggleButton;

float StretchPercent;
float xPosition;
float yPosition;
float xStretch;
float yStretch;
float xRotation;
float yRotation;
float zRotation;
float clipDistanceIVE;
float clipDistanceIVE2;
static int scalarX=1, scalarY=1, scalarZ=1;
int LRMult,UDMult;
static float mins3[3], maxs3[3], mids3[3];


struct TRIANGLE {
  struct plainpoint p1;
  struct plainpoint p2;
  struct plainpoint p3;
  int      normal1,normal2,normal3; //array indexes
};

struct TRIANGLES{
  int num_triangles,num_normals,*norm_counts;
  struct TRIANGLE *tri;
  struct plainpoint *normals;
  struct plainpoint *norm_points;
};

#define phpts3_dep(x,y,z) (phpts3.pt + (x) + (((y) + ((z) * phpts3.numx)) * phpts3.numy))

extern struct{
  struct plainpoint *pt;
  int numx;
  int numy;
  int numz;
}phpts3;

typedef struct {
  float xPos;
  float yPos;
  float yStr;
  float xStr;
  float xRot;
  float yRot;
  float zRot;}DisplayField; //used to set size of window

extern int *doubleBufferAttributes;
extern int *singleBufferAttributes;

int nObjects;
