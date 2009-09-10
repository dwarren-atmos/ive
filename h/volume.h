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

struct point {
    float x,y,z;
};

struct TRIANGLE {
  struct point p1;
  struct point p2;
  struct point p3;
};

struct TRIANGLES{
    int num_triangles;
    struct TRIANGLE *tri;
};

#define phpts3_dep(x,y,z) (phpts3.pt + (x) + (((y) + ((z) * phpts3.numx)) * phpts3.numy))

extern struct{
  struct point *pt;
  int numx;
  int numy;
  int numz;
}phpts3;


