#include<GL/gl.h>
#include<GL/glx.h>
#include <stdlib.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/XmAll.h>
#include <X11/keysym.h>
#include <xgks.h>
#include <ive_gks.h>



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

typedef struct {float xCoord, yCoord, zCoord; int normalRef;} Point;

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
  GLuint listName[10];
  char *Field[10];
}Objects;

typedef struct
{
	Objects *O;
	int N;
}ToggleButton;


static float xPosition=0.f;
static float yPosition=0.f;
static float xStretch;
static float yStretch;
float xRotation;
float yRotation;
static float zRotation=0.f;
static float clipDistanceIVE=0;
static float clipDistanceIVE2=0;
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
