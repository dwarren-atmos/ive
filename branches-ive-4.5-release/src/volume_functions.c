#define IVE_CUBE_PLOTS
#define FLT_MAX 3.40282347e+38f
#include <volume.h>
#include <window.h>
#include <ive_3d_macros.h>
extern int ginqcolorrep();
extern char *  gluErrorString();


struct{
  float x,y,z;
} center;

static struct{
  float x,y,z;
} transp = {0.,0.,0.};


extern struct {
  int num_of_colors,total_colors, highs[5], lows[5],
    total_line_colors, line_colors[5];}user_lines_colors_;



int curdisplist=0;

static float get_x0(A,B,C,D)
     float A,B,C,D;
{
      return((A-B) / ((A+C) - (B+D)));
}
static float get_y0(A,B,C,D)
     float A,B,C,D;
{
      return((A-D) / ((A+C) - (B+D)));
}

int ive_checkline(a,b,val)
    float a,b,val;
{
  float min,max;
  max = a < b ? b:a;
  min = a < b ? a:b;
  if(val >= min  && val <= max)return(1);
  return(0);
}

void get_errors(char *message){
  GLenum error = glGetError();
  if (error) {
    char *err = (char *) gluErrorString( error );
    fprintf( stderr, "GL Error: %s at %s\n", err, message );
  }
}

int ive_checkcube(a,b,c,d,e,f,g,h,val)
     float a,b,c,d,e,f,g,h,val;
{
  float max, min;
  max = min = a;
  if(max < b) max = b;
  else if(min > b) min = b;
  if(max < c) max = c;
  else if(min > c) min = c;
  if(max < d) max = d;
  else if(min > d) min = d;
  if(max < e) max = e;
  else if(min > e) min = e;
  if(max < f) max = f;
  else if(min > f) min = f;
  if(max < g) max = g;
  else if(min > g) min = g;
  if(max < h) max = h;
  else if(min > h) min = h;

  if(val >= min  && val <= max)return(1);
  return(0);
}
  
static float get_dist(val, v1, v2)
     float val, v1, v2;
{
  if(v1 == v2) return (0.0);
  return((val - v1)/(v2 - v1));
}

static float check_line(val,x,y)
     float val,x,y;
{
  float min,max;
  if(x == y){
    if(val == x)
      return(2.0);
    return(-1.0);
  }
  min = x > y ? y : x;
  max = x > y ? x : y;
  /*  printf("Check: %f <= %f>= %f\n", min,val,max);*/
  if(min <= val && max >= val){
    return((val - x)/(y - x));
  }
  return (FLT_MAX);
}

void ive_draw_points(x, y, z, vert, val)
     int x,y,z;
     float vert[8], val;
{
  int hicolor, error, i,j;
  float dist;
  struct{float x,y,z;}pt[24];
  Gcobundl co;
  ginqcolorrep(WS_X,user_lines_colors_.highs[0],GREALIZED,&co);
  i=0;/****/
  /* / D  / \
     ----/   \C  
    |    |   /
   E|  A |B /
    |    | /
     ---- 
       F
     *vert[0]=volume_dep(i,j,k);
     *vert[1]=volume_dep(i+1,j,k);
     *vert[2]=volume_dep(i+1,j,k+1);
     *vert[3]=volume_dep(i,j,k+1);
     *vert[4]=volume_dep(i,j+1,k);
     *vert[5]=volume_dep(i+1,j+1,k);
     *vert[6]=volume_dep(i+1,j+1,k+1);
     *vert[7]=volume_dep(i,j+1,k+1);
     */

  /*********A**********/
  if((dist = check_line(val,vert[0],vert[1])) != FLT_MAX){
    pt[i].x = phpts3_dep(x,y,z)->x + 
      dist*(phpts3_dep(x+1,y,z)->x - phpts3_dep(x,y,z)->x);
    pt[i].y = phpts3_dep(x,y,z)->y + 
      dist*(phpts3_dep(x+1,y,z)->y - phpts3_dep(x,y,z)->y);
    pt[i].z = phpts3_dep(x,y,z)->z + 
	dist*(phpts3_dep(x+1,y,z)->z - phpts3_dep(x,y,z)->z);
    i++;
  }
  if((dist = check_line(val,vert[1],vert[2])) != FLT_MAX){
    pt[i].x = phpts3_dep(x+1,y,z)->x + 
      dist*(phpts3_dep(x+1,y,z+1)->x - phpts3_dep(x+1,y,z)->x);
    pt[i].y = phpts3_dep(x+1,y,z)->y + 
      dist*(phpts3_dep(x+1,y,z+1)->y - phpts3_dep(x+1,y,z)->y);
    pt[i].z = phpts3_dep(x+1,y,z)->z + 
	dist*(phpts3_dep(x+1,y,z+1)->z - phpts3_dep(x,y+1,z)->z);
    i++;
  }
  if((dist = check_line(val,vert[2],vert[3])) != FLT_MAX){
    pt[i].x = phpts3_dep(x+1,y,z+1)->x + 
      dist*(phpts3_dep(x,y,z+1)->x - phpts3_dep(x+1,y,z+1)->x);
    pt[i].y = phpts3_dep(x+1,y,z+1)->y + 
      dist*(phpts3_dep(x,y,z+1)->y - phpts3_dep(x+1,y,z+1)->y);
    pt[i].z = phpts3_dep(x+1,y,z+1)->z + 
	dist*(phpts3_dep(x,y,z+1)->z - phpts3_dep(x+1,y,z+1)->z);
    i++;
  }
  if((dist = check_line(val,vert[3],vert[0])) != FLT_MAX){
    pt[i].x = phpts3_dep(x,y,z+1)->x + 
      dist*(phpts3_dep(x,y,z)->x - phpts3_dep(x,y,z+1)->x);
    pt[i].y = phpts3_dep(1,y,z+1)->y + 
      dist*(phpts3_dep(x,y,z)->y - phpts3_dep(x,y,z+1)->y);
    pt[i].z = phpts3_dep(1,y,z+1)->z + 
	dist*(phpts3_dep(x,y,z)->z - phpts3_dep(x,y,z+1)->z);
    i++;
  }

  /*****B***********/
  if((dist = check_line(val,vert[2],vert[6])) != FLT_MAX){
    pt[i].x = phpts3_dep(x+1,y,z+1)->x + 
      dist*(phpts3_dep(x+1,y+1,z+1)->x - phpts3_dep(x+1,y,z+1)->x);
    pt[i].y = phpts3_dep(x+1,y,z+1)->y + 
      dist*(phpts3_dep(x+1,y+1,z+1)->y - phpts3_dep(x+1,y,z+1)->y);
    pt[i].z = phpts3_dep(x+1,y,z+1)->z + 
	dist*(phpts3_dep(x+1,y+1,z+1)->z - phpts3_dep(x+1,y,z+1)->z);
    i++;
  }
  if((dist = check_line(val,vert[6],vert[7])) != FLT_MAX){
    pt[i].x = phpts3_dep(x+1,y+1,z+1)->x + 
      dist*(phpts3_dep(x,y+1,z+1)->x - phpts3_dep(x+1,y+1,z+1)->x);
    pt[i].y = phpts3_dep(x+1,y+1,z+1)->y + 
      dist*(phpts3_dep(x,y+1,z+1)->y - phpts3_dep(x+1,y+1,z+1)->y);
    pt[i].z = phpts3_dep(x+1,y+1,z+1)->z + 
	dist*(phpts3_dep(x,y+1,z+1)->z - phpts3_dep(x+1,y+1,z+1)->z);
    i++;
  }
  if((dist = check_line(val,vert[7],vert[3])) != FLT_MAX){
    pt[i].x = phpts3_dep(x,y+1,z+1)->x + 
      dist*(phpts3_dep(x,y,z+1)->x - phpts3_dep(x,y+1,z+1)->x);
    pt[i].y = phpts3_dep(x,y+1,z+1)->y + 
      dist*(phpts3_dep(x,y,z+1)->y - phpts3_dep(x,y+1,z+1)->y);
    pt[i].z = phpts3_dep(x,y+1,z+1)->z + 
	dist*(phpts3_dep(x,y,z+1)->z - phpts3_dep(x,y+1,z+1)->z);
    i++;
  }
  /*********C**********/
  if((dist = check_line(val,vert[4],vert[5])) != FLT_MAX){
    pt[i].x = phpts3_dep(x,y+1,z)->x + 
      dist*(phpts3_dep(x+1,y+1,z)->x - phpts3_dep(x,y+1,z)->x);
    pt[i].y = phpts3_dep(x,y+1,z)->y + 
      dist*(phpts3_dep(x+1,y+1,z)->y - phpts3_dep(x,y+1,z)->y);
    pt[i].z = phpts3_dep(x,y+1,z)->z + 
	dist*(phpts3_dep(x+1,y+1,z)->z - phpts3_dep(x,y+1,z)->z);
    i++;
  }
  if((dist = check_line(val,vert[5],vert[6])) != FLT_MAX){
    pt[i].x = phpts3_dep(x+1,y+1,z)->x + 
      dist*(phpts3_dep(x+1,y+1,z+1)->x - phpts3_dep(x+1,y+1,z)->x);
    pt[i].y = phpts3_dep(x+1,y+1,z)->y + 
      dist*(phpts3_dep(x+1,y+1,z+1)->y - phpts3_dep(x+1,y+1,z)->y);
    pt[i].z = phpts3_dep(x+1,y+1,z)->z + 
	dist*(phpts3_dep(x+1,y+1,z+1)->z - phpts3_dep(x+1,y+1,z)->z);
    i++;
  }
  if((dist = check_line(val,vert[7],vert[4])) != FLT_MAX){
    pt[i].x = phpts3_dep(x,y+1,z+1)->x + 
      dist*(phpts3_dep(x,y+1,z)->x - phpts3_dep(x,y+1,z+1)->x);
    pt[i].y = phpts3_dep(x,y+1,z+1)->y + 
      dist*(phpts3_dep(x,y+1,z)->y - phpts3_dep(x,y+1,z+1)->y);
    pt[i].z = phpts3_dep(x,y+1,z+1)->z + 
      dist*(phpts3_dep(x,y+1,z)->z - phpts3_dep(x,y+1,z+1)->z);
    i++;
  }

  /*********D**********/
  if((dist = check_line(val,vert[1],vert[5])) != FLT_MAX){
    pt[i].x = phpts3_dep(x+1,y,z)->x + 
      dist*(phpts3_dep(x+1,y+1,z)->x - phpts3_dep(x+1,y,z)->x);
    pt[i].y = phpts3_dep(x+1,y,z)->y + 
      dist*(phpts3_dep(x+1,y+1,z)->y - phpts3_dep(x+1,y,z)->y);
    pt[i].z = phpts3_dep(x+1,y,z)->z + 
	dist*(phpts3_dep(x+1,y+1,z)->z - phpts3_dep(x+1,y,z)->z);
    i++;
  }

  /*****E***********/
  if((dist = check_line(val,vert[4],vert[0])) != FLT_MAX){
    pt[i].x = phpts3_dep(x,y+1,z)->x + 
      dist*(phpts3_dep(x,y,z)->x - phpts3_dep(x,y+1,z)->x);
    pt[i].y = phpts3_dep(x,y+1,z)->y + 
      dist*(phpts3_dep(x,y,z)->y - phpts3_dep(x,y+1,z)->y);
    pt[i].z = phpts3_dep(x,y+1,z)->z + 
	dist*(phpts3_dep(x,y,z)->z - phpts3_dep(x,y+1,z)->z);
    i++;
  }
  /*********F**********/
  /*************/     
  glBegin(GL_POINTS);
  glColor3f(co.red,co.green,co.blue);
  /*  printf("Num points/box = %d\n",i);*/
  for(j=0; j<i; j++){
    glVertex3f(pt[j].x  - transp.x, pt[j].y  - transp.y, pt[j].z  - transp.z);
    /*    printf("%f, %f, %f\n",
	  pt[j].x  - transp.x, pt[j].y  - transp.y, pt[j].z  - transp.z);*/
  }
  glEnd();
}
void ive_draw_box(x, y, z, vert, val)
     int x,y,z;
     float vert[8], val;
{
  int error;
  Gcobundl co;
  struct{int x,y,z;}pt[8];
  
  ginqcolorrep(WS_X,user_lines_colors_.highs[0],GREALIZED,&co);

  pt[0].x = phpts3_dep(x,y,z)->x;
  pt[0].y = phpts3_dep(x,y,z)->y;
  pt[0].z = phpts3_dep(x,y,z)->z;
  pt[1].x = phpts3_dep(x+1,y,z)->x;
  pt[1].y = phpts3_dep(x+1,y,z)->y;
  pt[1].z = phpts3_dep(x+1,y,z)->z;
  pt[2].x=phpts3_dep(x+1,y,z+1)->x;
  pt[2].x=phpts3_dep(x+1,y,z+1)->y;
  pt[2].x=phpts3_dep(x+1,y,z+1)->z;
  pt[3].x=phpts3_dep(x,y,z+1)->x;
  pt[3].x=phpts3_dep(x,y,z+1)->y;
  pt[3].x=phpts3_dep(x,y,z+1)->z;
  pt[4].x=phpts3_dep(x,y+1,z)->x;
  pt[4].x=phpts3_dep(x,y+1,z)->y;
  pt[4].x=phpts3_dep(x,y+1,z)->z;
  pt[5].x=phpts3_dep(x+1,y+1,z)->x;
  pt[5].x=phpts3_dep(x+1,y+1,z)->y;
  pt[5].x=phpts3_dep(x+1,y+1,z)->z;
  pt[6].x=phpts3_dep(x+1,y+1,z+1)->x;
  pt[6].x=phpts3_dep(x+1,y+1,z+1)->y;
  pt[6].x=phpts3_dep(x+1,y+1,z+1)->z;
  pt[7].x=phpts3_dep(x,y+1,z+1)->x;
  pt[7].x=phpts3_dep(x,y+1,z+1)->y;
  pt[7].x=phpts3_dep(x,y+1,z+1)->z;

  glBegin(GL_LINE_STRIP);

  glColor3f(co.red,co.green,co.blue);
  get_errors("index setting");
  
  glVertex3f(pt[0].x - transp.x, pt[0].y - transp.y, pt[0].z - transp.z);
  glVertex3f(pt[1].x - transp.x, pt[1].y - transp.y, pt[1].z - transp.z);
  glVertex3f(pt[2].x - transp.x, pt[2].y - transp.y, pt[2].z - transp.z);
  glVertex3f(pt[3].x - transp.x, pt[3].y - transp.y, pt[3].z - transp.z);
  glVertex3f(pt[0].x - transp.x, pt[0].y - transp.y, pt[0].z - transp.z);
  glVertex3f(pt[4].x - transp.x, pt[4].y - transp.y, pt[4].z - transp.z);
  glVertex3f(pt[5].x - transp.x, pt[5].y - transp.y, pt[5].z - transp.z);
  glVertex3f(pt[6].x - transp.x, pt[6].y - transp.y, pt[6].z - transp.z);
  glVertex3f(pt[7].x - transp.x, pt[7].y - transp.y, pt[7].z - transp.z);
  glVertex3f(pt[3].x - transp.x, pt[3].y - transp.y, pt[3].z - transp.z);
  glEnd();
  glBegin(GL_LINES);

  glColor3f(co.red,co.green,co.blue);
  get_errors("index setting");
  glVertex3f(pt[1].x - transp.x, pt[1].y - transp.y, pt[1].z - transp.z);
  glVertex3f(pt[5].x - transp.x, pt[5].y - transp.y, pt[5].z - transp.z);
  glVertex3f(pt[2].x - transp.x, pt[2].y - transp.y, pt[2].z - transp.z);
  glVertex3f(pt[6].x - transp.x, pt[6].y - transp.y, pt[6].z - transp.z); 
  glEnd();
}

static void ive_draw_wire_surf(filled, x, y, z, vert, val)
     int filled, x,y,z;
     float vert[8], val;
{
  int corner[8] = {0,0,0,0,0,0,0,0};
  int hicolor, error, i,j,k,l, count=0, numtri = 0, fudge = 0, fudge2 = 0;
  int a,b,c,d,e,f,g,h;
  int c1, c2, c3, c4;
  int updown;
  float dist=0.0;
  struct{int x,y,z;}pt[8];
  struct{float x,y,z;}point[12];
  Gcobundl co;
  
  ginqcolorrep(WS_X,user_lines_colors_.highs[0],GREALIZED,&co);
  i=0;/****/
  /* / D  / \
     ----/   \C  
    |    |   /
   E|  A |B /
    |    | /
     ---- 
       F */
  /*****      7-----6
   *****     /|    /|
   *****    4-----5 |
   *****    | 3---|-2
   *****    |/    |/ 
   *****    0-----1  
   *****
   *vert[0]=volume_dep(x,y,z);
   *vert[1]=volume_dep(x+1,y,z);
   *vert[2]=volume_dep(x+1,y,z+1);
   *vert[3]=volume_dep(x,y,z+1);
   *vert[4]=volume_dep(x,y+1,z);
   *vert[5]=volume_dep(x+1,y+1,z);
   *vert[6]=volume_dep(x+1,y+1,z+1);
   *vert[7]=volume_dep(x,y+1,z+1);
   */
   pt[0].x = phpts3_dep(x,y,z)->x;
   pt[0].y = phpts3_dep(x,y,z)->y;
   pt[0].z = phpts3_dep(x,y,z)->z;
   pt[1].x = phpts3_dep(x+1,y,z)->x;
   pt[1].y = phpts3_dep(x+1,y,z)->y;
   pt[1].z = phpts3_dep(x+1,y,z)->z;
   pt[2].x=phpts3_dep(x+1,y,z+1)->x;
   pt[2].y=phpts3_dep(x+1,y,z+1)->y;
   pt[2].z=phpts3_dep(x+1,y,z+1)->z;
   pt[3].x=phpts3_dep(x,y,z+1)->x;
   pt[3].y=phpts3_dep(x,y,z+1)->y;
   pt[3].z=phpts3_dep(x,y,z+1)->z;
   pt[4].x=phpts3_dep(x,y+1,z)->x;
   pt[4].y=phpts3_dep(x,y+1,z)->y;
   pt[4].z=phpts3_dep(x,y+1,z)->z;
   pt[5].x=phpts3_dep(x+1,y+1,z)->x;
   pt[5].y=phpts3_dep(x+1,y+1,z)->y;
   pt[5].z=phpts3_dep(x+1,y+1,z)->z;
   pt[6].x=phpts3_dep(x+1,y+1,z+1)->x;
   pt[6].y=phpts3_dep(x+1,y+1,z+1)->y;
   pt[6].z=phpts3_dep(x+1,y+1,z+1)->z;
   pt[7].x=phpts3_dep(x,y+1,z+1)->x;
   pt[7].y=phpts3_dep(x,y+1,z+1)->y;
   pt[7].z=phpts3_dep(x,y+1,z+1)->z;
  for(i=0; i<8; i++){
    if(vert[i] <= val){
      corner[i] = 1;
      count++;
    }
  }
  
  switch (count){
  case 0:
  case 8:
    break;
  case 1:
  case 7:
    updown = (count==1) ? 1:0;
    /*      10000000 1  */
    numtri = 1;
    for (i=0; i<8; i++){
      if(corner[i] == updown){
	switch (i){
	case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
	  break;
	case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
	  break;
	case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
	  break;
	case 3:a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
	  break;
	case 4:e=0;f=1;g=2;g=3;a=4;b=5;c=6;d=7;
	  break;
	case 5:e=1;f=2;g=3;h=0;a=5;b=6;c=7;d=4;
	  break;
	case 6:e=2;f=3;g=0;h=1;a=6;b=7;c=4;d=5;
	  break;
	case 7:e=3;f=0;g=1;h=2;a=7;b=4;c=5;d=6;
	  break;
	}
	if((dist = get_dist(val,vert[a],vert[b])) != FLT_MAX){
	point[0].x = pt[a].x + dist*(pt[b].x - pt[a].x);
	point[0].y = pt[a].y + dist*(pt[b].y - pt[a].y);
	point[0].z = pt[a].z + dist*(pt[b].z - pt[a].z);
	}
	if((dist = get_dist(val,vert[a],vert[e])) != FLT_MAX){
	point[1].x = pt[a].x + dist*(pt[e].x - pt[a].x);
	point[1].y = pt[a].y + dist*(pt[e].y - pt[a].y);
	point[1].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	}
	if((dist = get_dist(val,vert[a],vert[d])) != FLT_MAX){
	point[2].x = pt[a].x + dist*(pt[d].x - pt[a].x);
	point[2].y = pt[a].y + dist*(pt[d].y - pt[a].y);
	point[2].z = pt[a].z + dist*(pt[d].z - pt[a].z);
	}
      }
    }
    break;
  case 2:
  case 6:
    updown = (count==2) ? 1:0;
    for (i=0; i<8; i++){
      if(corner[i] == updown){
	for(j=1; i+j<8; j++){
	  if(corner[i+j] == updown){
	    if((i!=3 && j==1) || j==4 || (i==1 && j==3) ||(i==4 &&j==3)){ 
	      /*we have 11000000 or 10001000 #2*/
	      if(j != 4){
		if(i==0 && j!= 3){
		  a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
		}
		else if(i==1){
		  a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
		}
		else if(i==2){	
		  a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
		}
		else if(i==0){
		  a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		}
		else if(i==4 && j!=3){
		  a=5;b=4;c=7;d=6;e=1;f=0;g=3;h=2;
		}
		else if(i==5){
		  a=6;b=5;c=4;d=7;e=2;f=1;g=0;h=3;
		}
		else if(i==6){
		  a=7;b=6;c=5;d=4;e=3;f=2;g=1;h=0;
		}
		else if(i==4){/*j==3*/
		  a=4;b=7;c=6;d=5;e=0;f=3;g=2;h=1;
		}
	      }
	      else{/*j==4*/
		if(i==0){
		  a=0;b=4;c=5;d=1;e=3;f=7;g=6;h=2;
		}
		else if(i==1){
		  a=1;b=5;c=6;d=2;e=0;f=4;g=7;h=3;
		}
		else if(i==2){
		  a=2;b=6;c=7;d=3;e=1;f=5;g=4;h=0;
		}
		else if(i==3){
		  a=3;b=7;c=4;d=0;e=2;f=6;g=5;h=1;
		}
		else{return;}
	      }
	      numtri = 2;
	      dist = get_dist(val,vert[a],vert[d]);
	      point[0].x = pt[a].x + dist*(pt[d].x - pt[a].x);
	      point[0].y = pt[a].y + dist*(pt[d].y - pt[a].y);
	      point[0].z = pt[a].z + dist*(pt[d].z - pt[a].z);
	      
	      dist = get_dist(val,vert[a],vert[e]);
	      point[1].x = pt[a].x + dist*(pt[e].x - pt[a].x);
	      point[1].y = pt[a].y + dist*(pt[e].y - pt[a].y);
	      point[1].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	      
	      dist = get_dist(val,vert[b],vert[c]);
	      point[2].x = pt[b].x + dist*(pt[c].x - pt[b].x);
	      point[2].y = pt[b].y + dist*(pt[c].y - pt[b].y);
	      point[2].z = pt[b].z + dist*(pt[c].z - pt[b].z);
	      
	      point[3].x = point[1].x;
	      point[3].y = point[1].y;
	      point[3].z = point[1].z;

	      point[4].x = point[2].x;
	      point[4].y = point[2].y;
	      point[4].z = point[2].z;
		
	      dist = get_dist(val,vert[b],vert[f]);
	      point[5].x = pt[b].x + dist*(pt[f].x - pt[b].x);
	      point[5].y = pt[b].y + dist*(pt[f].y - pt[b].y);
	      point[5].z = pt[b].z + dist*(pt[f].z - pt[b].z);
	      
	    }
	    else if((j==1 && i==3) || j==5 || j==3 || j==7 || j==6 || j==2){ 
	      /*we have 10000100 10000001 #3 and 10000010 #4*
	       *2 independant corners                        */
	      numtri = 2;
	      k = i + j;
	      switch (i){
	      case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
		break;
	      case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
		break;
	      case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
		break;
	      case 3:a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		break;
	      case 4:e=0;f=1;g=2;g=3;a=4;b=5;c=6;d=7;
		break;
	      case 5:e=1;f=2;g=3;h=0;a=5;b=6;c=7;d=4;
		break;
	      case 6:e=2;f=3;g=0;h=1;a=6;b=7;c=4;d=5;
		break;
	      case 7:e=3;f=0;g=1;h=2;a=7;b=4;c=5;d=6;
		break;
	      }
	      dist = get_dist(val,vert[a],vert[b]);
	      point[0].x = pt[a].x + dist*(pt[b].x - pt[a].x);
	      point[0].y = pt[a].y + dist*(pt[b].y - pt[a].y);
	      point[0].z = pt[a].z + dist*(pt[b].z - pt[a].z);
	      
	      dist = get_dist(val,vert[a],vert[e]);
	      point[1].x = pt[a].x + dist*(pt[e].x - pt[a].x);
	      point[1].y = pt[a].y + dist*(pt[e].y - pt[a].y);
	      point[1].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	      
	      dist = get_dist(val,vert[a],vert[d]);
	      point[2].x = pt[a].x + dist*(pt[d].x - pt[a].x);
	      point[2].y = pt[a].y + dist*(pt[d].y - pt[a].y);
	      point[2].z = pt[a].z + dist*(pt[d].z - pt[a].z);
	      
	      switch (k){
	      case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
		break;
	      case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
		break;
	      case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
		break;
	      case 3:a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		break;
	      case 4:e=0;f=1;g=2;g=3;a=4;b=5;c=6;d=7;
		break;
	      case 5:e=1;f=2;g=3;h=0;a=5;b=6;c=7;d=4;
		break;
	      case 6:e=2;f=3;g=0;h=1;a=6;b=7;c=4;d=5;
		break;
	      case 7:e=3;f=0;g=1;h=2;a=7;b=4;c=5;d=6;
		break;
	      }
	      dist = get_dist(val,vert[a],vert[b]);
	      point[3].x = pt[a].x + dist*(pt[b].x - pt[a].x);
	      point[3].y = pt[a].y + dist*(pt[b].y - pt[a].y);
	      point[3].z = pt[a].z + dist*(pt[b].z - pt[a].z);
	      
	      dist = get_dist(val,vert[a],vert[e]);
	      point[4].x = pt[a].x + dist*(pt[e].x - pt[a].x);
	      point[4].y = pt[a].y + dist*(pt[e].y - pt[a].y);
	      point[4].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	      
	      dist = get_dist(val,vert[a],vert[d]);
	      point[5].x = pt[a].x + dist*(pt[d].x - pt[a].x);
	      point[5].y = pt[a].y + dist*(pt[d].y - pt[a].y);
	      point[5].z = pt[a].z + dist*(pt[d].z - pt[a].z);
	    }
	  }
	}
      }
    }
    break;
  case 3:
  case 5:
    updown = (count==3) ? 1:0;
    /*11100000 #5*
     *11000010 #6*
     *10001010 #7*/
    for (i=0; i<8; i++)
      if(corner[i] == updown){
	for(j=1; i+j<8; j++)
	  if(corner[i+j] == updown){
	    for(k=1; i+j+k<8; k++)
	      if(corner[i+j+k] == updown){
		c1=i; c2=i+j; c3=i+j+k;
		if((c3 < 4) || (c1 >3) || 
		   (c2 <4 && c2==(c1+1) && (c3==c1+4 || c3== c2+4)) ||
		   (c2 >3 && c3==(c2+1) && (c1==c2-4 || c1== c2-4)))
		  { 
		    numtri=3;
		    /*bottom*/
		    if(c1==1 && c2==2 && c3==3){
		      a=0; b=1; c=2; d=3; e=4; f=5; g=6; h=7;}
		    else if(c1==0 && c2==1 && c3==2){
		      a=3; b=0; c=1; d=2; e=7; f=4; g=5; h=6;}
		    else if(c1==0 && c2==1 && c3==3){
		      a=2; b=3; c=0; d=1; e=6; f=7; g=4; h=5;}
		    else if(c1==0 && c2==2 && c3==3){
		      a=2; b=3; c=0; d=1; e=6; f=7; g=4; h=5;}
		    
		    /*top*/
		    else if(c1==4 && c2==5 && c3==6){
		      a=7; b=6; c=5; d=4; e=3; f=2; g=1; h=0;}
		    else if(c1==5 && c2==6 && c3==7){
		      a=4; b=7; c=6; d=5; e=0; f=3; g=2; h=1;}
		    else if(c1==4 && c2==6 && c3==7){
		      a=5; b=4; c=7; d=6; e=1; f=0; g=3; h=2;}
		    else if(c1==4 && c2==5 && c3==7){
		      a=6; b=5; c=4; d=7; e=2; f=1; g=0; h=3;}
		    
		    /*front*/
		    else if(c1==0 && c2==1 && c3==5){
		      a=4; b=5; c=1; d=0; e=7; f=6; g=2; h=3;}
		    else if(c1==1 && c2==4 && c3==5){
		      a=0; b=4; c=5; d=1; e=3; f=7; g=6; h=2;}
		    else if(c1==0 && c2==4 && c3==5){
		      a=1; b=0; c=4; d=5; e=2; f=3; g=7; h=6;}
		    else if(c1==0 && c2==1 && c3==4){
		      a=5; b=1; c=0; d=4; e=6; f=2; g=3; h=7;}
		    
		    /*back*/
		    else if(c1==2 && c2==6 && c3==7){
		      a=3; b=2; c=6; d=7; e=0; f=1; g=5; h=4;}
		    else if(c1==2 && c2==3 && c3==6){
		      a=7; b=3; c=2; d=6; e=4; f=0; g=5; h=5;}
		    else if(c1==2 && c2==3 && c3==7){
		      a=6; b=7; c=3; d=2; e=5; f=4; g=0; h=1;}
		    else if(c1==3 && c2==6 && c3==7){
		      a=2; b=6; c=7; d=3; e=1; f=5; g=4; h=0;}
		    
		    /*right*/
		    else if(c1==2 && c2==5 && c3==6){
		      a=1; b=5; c=6; d=2; e=0; f=4; g=7; h=3;}
		    else if(c1==1 && c2==5 && c3==6){
		      a=2; b=1; c=5; d=6; e=3; f=0; g=4; h=7;}
		    else if(c1=1 && c2==2 && c3==5){
		      a=6; b=2; c=1; d=5; e=7; f=3; g=0; h=4;}
		    else if(c1==1 && c2==2 && c3==6){
		      a=5; b=6; c=2; d=7; e=4; f=7; g=3; h=0;}

		    /*back*/
		    if(c1==0 && c2==3 && c3==7){
		      a=4; b=0; c=3; d=7; e=5; f=1; g=2; h=6;}
		    else if(c1==0 && c2==3 && c3==4){
		      a=7; b=4; c=0; d=3; e=6; f=5; g=1; h=2;}
		    else if(c1==0 && c2==4 && c3==7){
		      a=3; b=7; c=4; d=0; e=2; f=6; g=5; h=1;}
		    else if(c1==3 && c2==4 && c3==7){
		      a=0; b=3; c=7; d=4; e=1; f=2; g=6; h=5;}

		    
		    
		    dist = get_dist(val,vert[d],vert[h]);
		    point[0].x = pt[d].x + dist*(pt[h].x - pt[d].x);
		    point[0].y = pt[d].y + dist*(pt[h].y - pt[d].y);
		    point[0].z = pt[d].z + dist*(pt[h].z - pt[d].z);

		    dist = get_dist(val,vert[d],vert[a]);
		    point[1].x = pt[d].x + dist*(pt[a].x - pt[d].x);
		    point[1].y = pt[d].y + dist*(pt[a].y - pt[d].y);
		    point[1].z = pt[d].z + dist*(pt[a].z - pt[d].z);

		    dist = get_dist(val,vert[b],vert[f]);
		    point[2].x = pt[b].x + dist*(pt[f].x - pt[b].x);
		    point[2].y = pt[b].y + dist*(pt[f].y - pt[b].y);
		    point[2].z = pt[b].z + dist*(pt[f].z - pt[b].z);

		    point[3].x = point[0].x;
		    point[3].y = point[0].y;
		    point[3].z = point[0].z;
		    
		    point[4].x = point[2].x;
		    point[4].y = point[2].y;
		    point[4].z = point[2].z;
		    
		    dist = get_dist(val,vert[c],vert[g]);
		    point[5].x = pt[c].x + dist*(pt[g].x - pt[c].x);
		    point[5].y = pt[c].y + dist*(pt[g].y - pt[c].y);
		    point[5].z = pt[c].z + dist*(pt[g].z - pt[c].z);

		    point[6].x = point[1].x;
		    point[6].y = point[1].y;
		    point[6].z = point[1].z;
		    
		    point[7].x = point[2].x;
		    point[7].y = point[2].y;
		    point[7].z = point[2].z;
		    
		    dist = get_dist(val,vert[a],vert[b]);
		    point[8].x = pt[c].x + dist*(pt[b].x - pt[a].x);
		    point[8].y = pt[c].y + dist*(pt[b].y - pt[a].y);
		    point[8].z = pt[c].z + dist*(pt[b].z - pt[a].z);
		    
		  }
		
		/*independant corners on diagnols*/
		else if((c1==0 && c2==2 &&(c3==5 || c3==7))||
			(c1==0 && c2==5 && c3 == 7) ||
			(c1==1 && c2==3 &&(c3==4 || c3==6))||
			(c1==1 && c2==4 && c3==6)||
			(c1==2 && c2==4 && c3==5)||
			(c1==2 && c2==5 && c3==7)||
			(c1==3 && c2==4 && c3==5)){
		  numtri=3;
		  switch (c1){
		  case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
		    break;
		  case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
		    break;
		  case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
		    break;
		  case 3:a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		    break;
		  }
		  dist = get_dist(val,vert[a],vert[b]);
		  point[0].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		  point[0].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		  point[0].z = pt[a].z + dist*(pt[b].z - pt[a].z);
		  
		  dist = get_dist(val,vert[a],vert[e]);
		  point[1].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		  point[1].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		  point[1].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	
		  dist = get_dist(val,vert[a],vert[d]);
		  point[2].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		  point[2].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		  point[2].z = pt[a].z + dist*(pt[d].z - pt[a].z);

		  switch (c2){
		  case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
		    break;
		  case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
		    break;
		  case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
		    break;
		  case 3:a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		    break;
		  case 4:e=0;f=1;g=2;g=3;a=4;b=5;c=6;d=7;
		    break;
		  case 5:e=1;f=2;g=3;h=0;a=5;b=6;c=7;d=4;
		    break;
		  case 6:e=2;f=3;g=0;h=1;a=6;b=7;c=4;d=5;
		    break;
		  case 7:e=3;f=0;g=1;h=2;a=7;b=4;c=5;d=6;
		    break;
		  }
		  dist = get_dist(val,vert[a],vert[b]);
		  point[3].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		  point[3].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		  point[3].z = pt[a].z + dist*(pt[b].z - pt[a].z);
		  
		  dist = get_dist(val,vert[a],vert[e]);
		  point[4].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		  point[4].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		  point[4].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	
		  dist = get_dist(val,vert[a],vert[d]);
		  point[5].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		  point[5].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		  point[5].z = pt[a].z + dist*(pt[d].z - pt[a].z);

		  switch (c3){
		  case 4:e=0;f=1;g=2;g=3;a=4;b=5;c=6;d=7;
		    break;
		  case 5:e=1;f=2;g=3;h=0;a=5;b=6;c=7;d=4;
		    break;
		  case 6:e=2;f=3;g=0;h=1;a=6;b=7;c=4;d=5;
		    break;
		  case 7:e=3;f=0;g=1;h=2;a=7;b=4;c=5;d=6;
		    break;
		  }
		  dist = get_dist(val,vert[a],vert[b]);
		  point[6].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		  point[6].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		  point[6].z = pt[a].z + dist*(pt[b].z - pt[a].z);
		  
		  dist = get_dist(val,vert[a],vert[e]);
		  point[7].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		  point[7].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		  point[7].z = pt[a].z + dist*(pt[e].z - pt[a].z);
	
		  dist = get_dist(val,vert[a],vert[d]);
		  point[8].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		  point[8].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		  point[8].z = pt[a].z + dist*(pt[d].z - pt[a].z);
		}
		else{
		  /*two adjacent and 1 apart*/
		  if(c2<4){
		    if(j==1){
		      c4 = c3;
		      switch (c1){
		      case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
			break;
		      case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
			break;
		      case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
			break;
		      }
		    }
		    if(j==2){
		      if(k==4){/*c2 under c4*/
			c4 = c1;
			switch(c2){
			case 2:a=2;b=6;c=7;d=3;e=1;f=5;g=4;h=0;
			  break;
			case 3:a=3;b=7;c=4;d=0;e=2;f=6;g=5;h=1;
			  break;
			}
		      }
		      else{/*c1 under c3*/
			c4 = c2;
			switch(c1){
			case 0:a=0;b=4;c=5;d=1;e=3;f=7;g=6;h=2;
			  break;
			case 1:a=1;b=5;c=6;d=2;e=0;f=4;g=7;h=3;
			  break;
			}
		      }
		    }
		    if(j==3){
		      c4 = c3;
		      a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		    }
		  }
		  else if(j==4){
		    c4 = c3;
		    switch (c1){
		    case 0:a=0;b=4;c=5;d=1;e=3;f=7;g=6;h=2;
		      break;
		    case 1:a=1;b=5;c=6;d=0;e=0;f=4;g=7;h=3;
		      break;
		    case 2:a=2;b=6;c=7;d=3;e=1;f=5;g=4;h=0;
		      break;
		    case 3:a=3;b=7;c=4;d=0;e=2;f=6;g=5;h=1;
		      break;
		    }
		  }
		  else if(k==1){/*c2 c3 adjacent*/
		    c4 = c1;
		    switch (c2){
		    case 4:a=0;b=4;c=5;d=1;e=3;f=7;g=6;h=2;
		      break;
		    case 5:a=1;b=5;c=6;d=0;e=0;f=4;g=7;h=3;
		      break;
		    case 6:a=2;b=6;c=7;d=3;e=1;f=5;g=4;h=0;
		      break;
		    }
		  }    
		  else{/*c1 under c3 - c2 on top*/
		    c4 = c2;
		    switch (c1){
		    case 2:a=2;b=6;c=7;d=3;e=1;f=5;g=4;h=0;
		      break;
		    case 3:a=3;b=7;c=4;d=0;e=2;f=6;g=5;h=1;
		      break;
		    }
		  }
		  numtri = 3;
		    dist = get_dist(val,vert[a],vert[d]);
		  point[0].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		  point[0].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		  point[0].z = pt[a].z + dist*(pt[d].z - pt[a].z);
		  
		  dist = get_dist(val,vert[a],vert[e]);
		  point[1].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		  point[1].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		  point[1].z = pt[a].z + dist*(pt[e].z - pt[a].z);
		  
		  dist = get_dist(val,vert[b],vert[c]);
		  point[2].x = pt[b].x + dist*(pt[c].x - pt[b].x);
		  point[2].y = pt[b].y + dist*(pt[c].y - pt[b].y);
		  point[2].z = pt[b].z + dist*(pt[c].z - pt[b].z);
		  
		  point[3].x = point[1].x;
		  point[3].y = point[1].y;
		  point[3].z = point[1].z;
		  
		  point[4].x = point[2].x;
		  point[4].y = point[2].y;
		  point[4].z = point[2].z;
		  
		  dist = get_dist(val,vert[b],vert[f]);
		  point[5].x = pt[b].x + dist*(pt[f].x - pt[b].x);
		  point[5].y = pt[b].y + dist*(pt[f].y - pt[b].y);
		  point[5].z = pt[b].z + dist*(pt[f].z - pt[b].z);
		  switch (c4){
		  case 0:a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;
		    break;
		  case 1:a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;
		    break;
		  case 2:a=2;b=3;c=0;d=1;e=6;f=7;g=4;h=5;
		    break;
		  case 3:a=3;b=0;c=1;d=2;e=7;f=4;g=5;h=6;
		    break;
		  case 4:e=0;f=1;g=2;g=3;a=4;b=5;c=6;d=7;
		    break;
		  case 5:e=1;f=2;g=3;h=0;a=5;b=6;c=7;d=4;
		    break;
		  case 6:e=2;f=3;g=0;h=1;a=6;b=7;c=4;d=5;
		    break;
		  case 7:e=3;f=0;g=1;h=2;a=7;b=4;c=5;d=6;
		    break;
		  }
		  dist = get_dist(val,vert[a],vert[b]);
		  point[6].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		  point[6].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		  point[6].z = pt[a].z + dist*(pt[b].z - pt[a].z);
		  
		  dist = get_dist(val,vert[a],vert[e]);
		  point[7].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		  point[7].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		  point[7].z = pt[a].z + dist*(pt[e].z - pt[a].z);
		  
		  dist = get_dist(val,vert[a],vert[d]);
		  point[8].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		  point[8].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		  point[8].z = pt[a].z + dist*(pt[d].z - pt[a].z);
		  
		}
	      }
	  }
      }
    break;
  case 4:
    for (i=0; i<8; i++)
      if(corner[i] == 1){
	for(j=1; i+j<8; j++)
	  if(corner[i+j] == 1){
	    for(k=1; i+j+k<8; k++)
	      if(corner[i+j+k] == 1){
		for(l=1; i+j+k+l<8; l++)
		  if(corner[i+j+k+l] == 1){
		    c1=i; c2=i+j; c3=i+j+k, c4=i+j+k+l;
		    /* faces first */
		    if(c4==3 || c1==4 ||(c3%4==c1 && c4%4==c2 &&(j==1||j==3))){
		      /* 11110000 #8*/
		      numtri=2;
		      if(c1==4 || c4==3){
			a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;}
		      else if((c1==0 && c4==7) || (c1==1 && c4==6)){
			a=4;b=0;c=3;d=7;e=5;f=1;g=2;h=6;}
		      else if((c1==0 && c4==5) || (c1==2 && c4==7)){
			a=4;b=5;c=1;d=0;e=7;f=6;g=2;h=3;}

		      dist = get_dist(val,vert[a],vert[e]);
		      point[0].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		      point[0].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		      point[0].z = pt[a].z + dist*(pt[e].z - pt[a].z);

		      dist = get_dist(val,vert[b],vert[f]);
		      point[1].x = pt[b].x + dist*(pt[f].x - pt[b].x);
		      point[1].y = pt[b].y + dist*(pt[f].y - pt[b].y);
		      point[1].z = pt[b].z + dist*(pt[f].z - pt[b].z);

		      dist = get_dist(val,vert[d],vert[h]);
		      point[2].x = pt[d].x + dist*(pt[h].x - pt[d].x);
		      point[2].y = pt[d].y + dist*(pt[h].y - pt[d].y);
		      point[2].z = pt[d].z + dist*(pt[h].z - pt[d].z);
		      
		      dist = get_dist(val,vert[c],vert[g]);
		      point[3].x = pt[c].x + dist*(pt[g].x - pt[c].x);
		      point[3].y = pt[c].y + dist*(pt[g].y - pt[c].y);
		      point[3].z = pt[c].z + dist*(pt[g].z - pt[c].z);
		      
		      point[4].x = point[1].x;
		      point[4].y = point[1].y;
		      point[4].z = point[1].z;
		      
		      point[5].x = point[2].x;
		      point[5].y = point[2].y;
		      point[5].z = point[2].z;
		      
		    }
		    /*bisect on diaganol 10101010 #10*/
		    else if(c3%4==c1 && c4%4==c2){
		      /*other instances already caught*/
		      numtri=4;
		      if(c1==0){
			a=0;b=1;c=2;d=3;e=4;f=5;g=6;h=7;}
		      else if(c1==1){
			a=1;b=2;c=3;d=0;e=5;f=6;g=7;h=4;}

		      dist = get_dist(val,vert[a],vert[b]);
		      point[0].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		      point[0].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		      point[0].z = pt[a].z + dist*(pt[b].z - pt[a].z);

		      dist = get_dist(val,vert[e],vert[f]);
		      point[1].x = pt[e].x + dist*(pt[f].x - pt[e].x);
		      point[1].y = pt[e].y + dist*(pt[f].y - pt[e].y);
		      point[1].z = pt[e].z + dist*(pt[f].z - pt[e].z);
		      
		      dist = get_dist(val,vert[a],vert[d]);
		      point[2].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		      point[2].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		      point[2].z = pt[a].z + dist*(pt[d].z - pt[a].z);

		      dist = get_dist(val,vert[e],vert[h]);
		      point[3].x = pt[e].x + dist*(pt[h].x - pt[e].x);
		      point[3].y = pt[e].y + dist*(pt[h].y - pt[e].y);
		      point[3].z = pt[e].z + dist*(pt[h].z - pt[e].z);
		      
		      point[4].x = point[1].x;
		      point[4].y = point[1].y;
		      point[4].z = point[1].z;
		      
		      point[5].x = point[2].x;
		      point[5].y = point[2].y;
		      point[5].z = point[2].z;

		      dist = get_dist(val,vert[b],vert[c]);
		      point[6].x = pt[b].x + dist*(pt[c].x - pt[b].x);
		      point[6].y = pt[b].y + dist*(pt[c].y - pt[b].y);
		      point[6].z = pt[b].z + dist*(pt[c].z - pt[b].z);

		      dist = get_dist(val,vert[c],vert[d]);
		      point[7].x = pt[c].x + dist*(pt[d].x - pt[c].x);
		      point[7].y = pt[c].y + dist*(pt[d].y - pt[c].y);
		      point[7].z = pt[c].z + dist*(pt[d].z - pt[c].z);

		      dist = get_dist(val,vert[f],vert[g]);
		      point[8].x = pt[f].x + dist*(pt[g].x - pt[f].x);
		      point[8].y = pt[f].y + dist*(pt[g].y - pt[f].y);
		      point[8].z = pt[f].z + dist*(pt[g].z - pt[f].z);
		      
		      dist = get_dist(val,vert[g],vert[h]);
		      point[9].x = pt[g].x + dist*(pt[h].x - pt[g].x);
		      point[9].y = pt[g].y + dist*(pt[h].y - pt[g].y);
		      point[9].z = pt[g].z + dist*(pt[h].z - pt[g].z);

		      point[10].x = point[7].x;
		      point[10].y = point[7].y;
		      point[10].z = point[7].z;
		      
		      point[11].x = point[8].x;
		      point[11].y = point[8].y;
		      point[11].z = point[8].z;
		    }
		    else if(j==2 && (c2==2 && k==3 || c2==3 && k==1) && l==2){
		      /*4 independant corners 	  10100101 #13*/
		      numtri=4;
		      for(i=0; i<4; i++){
			if(i==0)a=c1;
			if(i==1)a=c2;
			if(i==2)a=c3;
			if(i==3)a=c4;
			
			if(a>= 4)fudge=4;
			dist = get_dist(val,vert[a],vert[(a+1)%4 + fudge]);
			point[3*i].x = pt[a].x + 
			  dist*(pt[(a+1)%4 + fudge].x -	pt[a].x);
			point[3*i].y = pt[a].y + 
			  dist*(pt[(a+1)%4 + fudge].y - pt[a].y);
			point[3*i].z = pt[a].z + 
			  dist*(pt[(a+1)%4 + fudge].z - pt[a].z);
			
			dist = get_dist(val,vert[a],vert[(a+3)%4 + fudge]);
			point[3*i+1].x = pt[a].x + 
			  dist*(pt[(a+3)%4 + fudge].x - pt[a].x);
			point[3*i+1].y = pt[a].y + 
			  dist*(pt[(a+3)%4 + fudge].y - pt[a].y);
			point[3*i+1].z = pt[a].z + 
			  dist*(pt[(a+3)%4 + fudge].z - pt[a].z);
			
			dist = get_dist(val,vert[a],vert[(a+4)%8 + fudge]);
			point[3*i+2].x = pt[a].x + 
			  dist*(pt[(a+4)%8 + fudge].x - pt[a].x);
			point[3*i+2].y = pt[a].y + 
			  dist*(pt[(a+4)%8 + fudge].y - pt[a].y);
			point[3*i+2].z = pt[a].z + 
			  dist*(pt[(a+4)%8 + fudge].z - pt[a].z);
			
		      }

		    }
		    /*11100001 *12*/
		    else if((c3<4 && c4-4 != c1 && c4-4 != c2 && 
			     c4-4 != c3)||
			    (c2>4 && c1+4 != c2 && c1+4 != c3 && 
			     c1+4 != c4)){
		      numtri=4;
		      /*bottom*/
		      if(c3<4){
			if(c1==1 && c2==2 && c3==3){
			  a=0; b=1; c=2; d=3; e=4; f=5; g=6; h=7;}
			else if(c1==0 && c2==1 && c3==2){
			  a=3; b=0; c=1; d=2; e=7; f=4; g=5; h=6;}
			else if(c1==0 && c2==1 && c3==3){
			  a=2; b=3; c=0; d=1; e=6; f=7; g=4; h=5;}
			else if(c1==0 && c2==2 && c3==3){
			  a=1; b=2; c=3; d=0; e=5; f=6; g=7; h=4;}
		      }
		      else{
			/*top*/
			if(c2==4 && c3==5 && c4==6){
			  a=7; b=6; c=5; d=4; e=3; f=2; g=1; h=0;}
			else if(c2==5 && c3==6 && c4==7){
			  a=4; b=7; c=6; d=5; e=0; f=3; g=2; h=1;}
			else if(c2==4 && c3==6 && c4==7){
			  a=5; b=4; c=7; d=6; e=1; f=0; g=3; h=2;}
			else if(c2==4 && c3==5 && c4==7){
			  a=6; b=5; c=4; d=7; e=2; f=1; g=0; h=3;}
		      }
		    
		    dist = get_dist(val,vert[d],vert[h]);
		    point[0].x = pt[d].x + dist*(pt[h].x - pt[d].x);
		    point[0].y = pt[d].y + dist*(pt[h].y - pt[d].y);
		    point[0].z = pt[d].z + dist*(pt[h].z - pt[d].z);

		    dist = get_dist(val,vert[d],vert[a]);
		    point[1].x = pt[d].x + dist*(pt[a].x - pt[d].x);
		    point[1].y = pt[d].y + dist*(pt[a].y - pt[d].y);
		    point[1].z = pt[d].z + dist*(pt[a].z - pt[d].z);
		    
		    dist = get_dist(val,vert[b],vert[f]);
		    point[2].x = pt[b].x + dist*(pt[f].x - pt[b].x);
		    point[2].y = pt[b].y + dist*(pt[f].y - pt[b].y);
		    point[2].z = pt[b].z + dist*(pt[f].z - pt[b].z);
		    
		    point[3].x = point[0].x;
		    point[3].y = point[0].y;
		    point[3].z = point[0].z;
		    
		    point[4].x = point[2].x;
		    point[4].y = point[2].y;
		    point[4].z = point[2].z;
		    
		    dist = get_dist(val,vert[c],vert[g]);
		    point[5].x = pt[c].x + dist*(pt[g].x - pt[c].x);
		    point[5].y = pt[c].y + dist*(pt[g].y - pt[c].y);
		    point[5].z = pt[c].z + dist*(pt[g].z - pt[c].z);
		    
		    point[6].x = point[1].x;
		    point[6].y = point[1].y;
		    point[6].z = point[1].z;
		    
		    point[7].x = point[2].x;
		    point[7].y = point[2].y;
		    point[7].z = point[2].z;
		    
		    dist = get_dist(val,vert[a],vert[b]);
		    point[8].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		    point[8].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		    point[8].z = pt[a].z + dist*(pt[b].z - pt[a].z);
		    
		    dist = get_dist(val,vert[a],vert[e]);
		    point[9].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		    point[9].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		    point[9].z = pt[a].z + dist*(pt[e].z - pt[a].z);
		    
		    dist = get_dist(val,vert[f],vert[e]);
		    point[10].x = pt[f].x + dist*(pt[e].x - pt[f].x);
		    point[10].y = pt[f].y + dist*(pt[e].y - pt[f].y);
		    point[10].z = pt[f].z + dist*(pt[e].z - pt[f].z);
		    
		    dist = get_dist(val,vert[h],vert[e]);
		    point[11].x = pt[h].x + dist*(pt[e].x - pt[h].x);
		    point[11].y = pt[h].y + dist*(pt[e].y - pt[h].y);
		    point[11].z = pt[h].z + dist*(pt[e].z - pt[h].z);
		    }
		    else if((c3 <4 &&(c4%4 + 1)%4 != c1 && (c4%4 + 1)%4 != c2
			     && (c4%4 + 1)%4 != c3)||
			    (c2 >4 &&(c1+1)%4 != c4%4 && (c1+1)%4 != c2%4 &&
			     (c1+1)%4 != c3%4)){
		      /*11100010 #14*/
		      numtri=3;
		      /*bottom*/
		      if(c3<4){
			if(c1==1 && c2==2 && c3==3){
			  a=0; b=1; c=2; d=3; e=4; f=5; g=6; h=7;}
			else if(c1==0 && c2==1 && c3==2){
			  a=3; b=0; c=1; d=2; e=7; f=4; g=5; h=6;}
			else if(c1==0 && c2==1 && c3==3){
			  a=2; b=3; c=0; d=1; e=6; f=7; g=4; h=5;}
			else if(c1==0 && c2==2 && c3==3){
			  a=1; b=2; c=3; d=0; e=5; f=6; g=7; h=4;}
		      }
		      else{
			/*top*/
			if(c2==4 && c3==5 && c4==6){
			  a=7; b=6; c=5; d=4; e=3; f=2; g=1; h=0;}
			else if(c2==5 && c3==6 && c4==7){
			  a=4; b=7; c=6; d=5; e=0; f=3; g=2; h=1;}
			else if(c2==4 && c3==6 && c4==7){
			  a=5; b=4; c=7; d=6; e=1; f=0; g=3; h=2;}
			else if(c2==4 && c3==5 && c4==7){
			  a=6; b=5; c=4; d=7; e=2; f=1; g=0; h=3;}
		      }
		      dist = get_dist(val,vert[c],vert[g]);
		      point[0].x = pt[c].x + dist*(pt[g].x - pt[c].x);
		      point[0].y = pt[c].y + dist*(pt[g].y - pt[c].y);
		      point[0].z = pt[c].z + dist*(pt[g].z - pt[c].z);

		      dist = get_dist(val,vert[e],vert[h]);
		      point[1].x = pt[e].x + dist*(pt[h].x - pt[e].x);
		      point[1].y = pt[e].y + dist*(pt[h].y - pt[e].y);
		      point[1].z = pt[e].z + dist*(pt[h].z - pt[e].z);
		      
		      dist = get_dist(val,vert[h],vert[g]);
		      point[2].x = pt[h].x + dist*(pt[g].x - pt[h].x);
		      point[2].y = pt[h].y + dist*(pt[g].y - pt[h].y);
		      point[2].z = pt[h].z + dist*(pt[g].z - pt[h].z);
		      
		      point[3].x = point[0].x;
		      point[3].y = point[0].y;
		      point[3].z = point[0].z;

		      point[4].x = point[1].x;
		      point[4].y = point[1].y;
		      point[4].z = point[1].z;

		      dist = get_dist(val,vert[a],vert[d]);
		      point[5].x = pt[a].x + dist*(pt[d].x - pt[a].x);
		      point[5].y = pt[a].y + dist*(pt[d].y - pt[a].y);
		      point[5].z = pt[a].z + dist*(pt[d].z - pt[a].z);
		      
		      point[6].x = point[0].x;
		      point[6].y = point[0].y;
		      point[6].z = point[0].z;

		      point[7].x = point[5].x;
		      point[7].y = point[5].y;
		      point[7].z = point[5].z;

		      dist = get_dist(val,vert[a],vert[b]);
		      point[8].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		      point[8].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		      point[8].z = pt[a].z + dist*(pt[b].z - pt[a].z);
		      
		    }	
		    else if((c3 <4 &&(c4%4 + 1)%4 != c1 && (c4%4 + 1)%4 != c2
			     && (c4%4 + 1)%4 != c3)||
			    (c2 >4 &&(c1+1)%4 != c4%4 && (c1+1)%4 != c2%4 &&
			     (c1+1)%4 != c3%4)){
		      /*10110010 11*/
		      numtri=3;
		      /*bottom*/
		      if(c3<4){
			if(c1==1 && c2==2 && c3==3){
			  a=0; b=1; c=2; d=3; e=4; f=5; g=6; h=7;}
			else if(c1==0 && c2==1 && c3==2){
			  a=3; b=0; c=1; d=2; e=7; f=4; g=5; h=6;}
			else if(c1==0 && c2==1 && c3==3){
			  a=2; b=3; c=0; d=1; e=6; f=7; g=4; h=5;}
			else if(c1==0 && c2==2 && c3==3){
			  a=1; b=2; c=3; d=0; e=5; f=6; g=7; h=4;}
		      }
		      else{
			/*top*/
			if(c2==4 && c3==5 && c4==6){
			  a=7; b=6; c=5; d=4; e=3; f=2; g=1; h=0;}
			else if(c2==5 && c3==6 && c4==7){
			  a=4; b=7; c=6; d=5; e=0; f=3; g=2; h=1;}
			else if(c2==4 && c3==6 && c4==7){
			  a=5; b=4; c=7; d=6; e=1; f=0; g=3; h=2;}
			else if(c2==4 && c3==5 && c4==7){
			  a=6; b=5; c=4; d=7; e=2; f=1; g=0; h=3;}
		      }
		      dist = get_dist(val,vert[d],vert[h]);
		      point[0].x = pt[d].x + dist*(pt[h].x - pt[d].x);
		      point[0].y = pt[d].y + dist*(pt[h].y - pt[d].y);
		      point[0].z = pt[d].z + dist*(pt[h].z - pt[d].z);

		      dist = get_dist(val,vert[c],vert[b]);
		      point[1].x = pt[c].x + dist*(pt[b].x - pt[c].x);
		      point[1].y = pt[c].y + dist*(pt[b].y - pt[c].y);
		      point[1].z = pt[c].z + dist*(pt[b].z - pt[c].z);

		      dist = get_dist(val,vert[a],vert[b]);
		      point[2].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		      point[2].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		      point[2].z = pt[a].z + dist*(pt[b].z - pt[a].z);

		      point[3].x = point[0].x;
		      point[3].y = point[0].y;
		      point[3].z = point[0].z;

		      point[4].x = point[1].x;
		      point[4].y = point[1].y;
		      point[4].z = point[1].z;

		      dist = get_dist(val,vert[f],vert[g]);
		      point[5].x = pt[f].x + dist*(pt[g].x - pt[f].x);
		      point[5].y = pt[f].y + dist*(pt[g].y - pt[f].y);
		      point[5].z = pt[f].z + dist*(pt[g].z - pt[f].z);
	
		      point[6].x = point[0].x;
		      point[6].y = point[0].y;
		      point[6].z = point[0].z;

		      point[7].x = point[5].x;
		      point[7].y = point[5].y;
		      point[7].z = point[5].z;

		      dist = get_dist(val,vert[h],vert[g]);
		      point[8].x = pt[h].x + dist*(pt[g].x - pt[h].x);
		      point[8].y = pt[h].y + dist*(pt[g].y - pt[h].y);
		      point[8].z = pt[h].z + dist*(pt[g].z - pt[h].z);
		    }	
		    else{		    /*10110001 #9*/
		      if(c3<4){
			if(c1==0 && c2==2 && c3==3){
			  a=0; b=1; c=2; d=3; e=4; f=5; g=6; h=7;}
			else if(c1==1 && c2==2 && c3==3){
			  a=3; b=0; c=1; d=2; e=7; f=4; g=5; h=6;}
			else if(c1==0 && c2==1 && c3==2){
			  a=2; b=3; c=0; d=1; e=6; f=7; g=4; h=5;}
			else if(c1==0 && c2==1 && c3==3){
			  a=1; b=2; c=3; d=0; e=5; f=6; g=7; h=4;}
		      }
		      else{
			/*top*/
			if(c2==4 && c3==5 && c4==7){
			  a=7; b=6; c=5; d=4; e=3; f=2; g=1; h=0;}
			else if(c2==4 && c3==5 && c4==6){
			  a=4; b=7; c=6; d=5; e=0; f=3; g=2; h=1;}
			else if(c2==5 && c3==6 && c4==7){
			  a=5; b=4; c=7; d=6; e=1; f=0; g=3; h=2;}
			else if(c2==4 && c3==6 && c4==7){
			  a=6; b=5; c=4; d=7; e=2; f=1; g=0; h=3;}
		      }
		      numtri=4;
		      dist = get_dist(val,vert[g],vert[h]);
		      point[0].x = pt[g].x + dist*(pt[h].x - pt[g].x);
		      point[0].y = pt[g].y + dist*(pt[h].y - pt[g].y);
		      point[0].z = pt[g].z + dist*(pt[h].z - pt[g].z);

		      dist = get_dist(val,vert[a],vert[e]);
		      point[1].x = pt[a].x + dist*(pt[e].x - pt[a].x);
		      point[1].y = pt[a].y + dist*(pt[e].y - pt[a].y);
		      point[1].z = pt[a].z + dist*(pt[e].z - pt[a].z);

		      dist = get_dist(val,vert[e],vert[h]);
		      point[2].x = pt[e].x + dist*(pt[h].x - pt[e].x);
		      point[2].y = pt[e].y + dist*(pt[h].y - pt[e].y);
		      point[2].z = pt[e].z + dist*(pt[h].z - pt[e].z);
		      
		      point[3].x = point[0].x;
		      point[3].y = point[0].y;
		      point[3].z = point[0].z;

		      point[4].x = point[1].x;
		      point[4].y = point[1].y;
		      point[4].z = point[1].z;

		      dist = get_dist(val,vert[a],vert[b]);
		      point[5].x = pt[a].x + dist*(pt[b].x - pt[a].x);
		      point[5].y = pt[a].y + dist*(pt[b].y - pt[a].y);
		      point[5].z = pt[a].z + dist*(pt[b].z - pt[a].z);

		      point[6].x = point[0].x;
		      point[6].y = point[0].y;
		      point[6].z = point[0].z;

		      point[7].x = point[5].x;
		      point[7].y = point[5].y;
		      point[7].z = point[5].z;

		      dist = get_dist(val,vert[c],vert[g]);
		      point[8].x = pt[c].x + dist*(pt[g].x - pt[c].x);
		      point[8].y = pt[c].y + dist*(pt[g].y - pt[c].y);
		      point[8].z = pt[c].z + dist*(pt[g].z - pt[c].z);

		      point[9].x = point[5].x;
		      point[9].y = point[5].y;
		      point[9].z = point[5].z;

		      point[10].x = point[8].x;
		      point[10].y = point[8].y;
		      point[10].z = point[8].z;

		      dist = get_dist(val,vert[c],vert[b]);
		      point[11].x = pt[c].x + dist*(pt[b].x - pt[c].x);
		      point[11].y = pt[c].y + dist*(pt[b].y - pt[c].y);
		      point[11].z = pt[c].z + dist*(pt[b].z - pt[c].z);
		    }
		  }
	      }
	  }
      }
    break;
  }
  if(filled){
    glColor3f(co.red,co.green,co.blue);
    glBegin(GL_TRIANGLES);
    for(i=0; i< 3*numtri; i++){
      glVertex3f(point[i].x-transp.x,point[i].y-transp.y,
		 point[i].z-transp.z);
    }
    glEnd();
  }
  else{
        glColor3f(co.red,co.green,co.blue);
    glIndexi(4);
    get_errors("index setting triangles");
    for(i=0; i< 3*numtri; i+=3){
      glBegin(GL_LINE_STRIP);
      glVertex3f(point[i].x-transp.x,point[i].y-transp.y,
		 point[i].z-transp.z);
      glVertex3f(point[i+1].x-transp.x,point[i+1].y-transp.y,
		 point[i+1].z-transp.z);
      glVertex3f(point[i+2].x-transp.x,point[i+2].y-transp.y,
		 point[i+2].z-transp.z);
      glVertex3f(point[i].x-transp.x,point[i].y-transp.y,
		 point[i].z-transp.z);
      glEnd();
    }
  }
}

void ive_draw_spider(filled, x, y, z, vert, val)
     int filled,x,y,z;
     float vert[8], val;
{
  int hicolor, error, i=0,j=0, k=0, l=0, A=0,B=0,C=0,D=0,E=0,F=0, numtri=0;
  int sideA[4] = {0,1,2,3};
  int sideB[4] = {4,5,6,7};
  int sideC[4] = {1,8,5,9};
  int sideD[4] = {3,10,7,11};
  int sideE[4] = {0,9,4,11};
  int sideF[4] = {2,10,6,8};
  float dist, cval,tval1,tval2;
  struct{float x,y,z; int inuse;}pt[12];
  struct{float x,y,z;}tripts[36],cpt,mid={0.,0.,0};
  Gcobundl co;
  ginqcolorrep(WS_X,user_lines_colors_.highs[0],GREALIZED,&co);
  
  /*  /----/\  <--B (Top)
     /    /  \
     ----/    \ <- F (Back)
D-> |    | C /
    | E  |  / <--A (Bottom)
    |    | /
     ---- 
       

     *vert[0]=volume_dep(i,j,k);
     *vert[1]=volume_dep(i+1,j,k);
     *vert[2]=volume_dep(i+1,j,k+1);
     *vert[3]=volume_dep(i,j,k+1);
     *vert[4]=volume_dep(i,j+1,k);
     *vert[5]=volume_dep(i+1,j+1,k);
     *vert[6]=volume_dep(i+1,j+1,k+1);
     *vert[7]=volume_dep(i,j+1,k+1);
       
     Face A
     pt[0]  is along vert[0] to vert[1]
     pt[1]  is along vert[1] to vert[2]
     pt[2]  is along vert[2] to vert[3]
     pt[3]  is along vert[3] to vert[0]
     Face B
     pt[4]  is along vert[4] to vert[5]
     pt[5]  is along vert[5] to vert[6]
     pt[6]  is along vert[6] to vert[7]
     pt[7]  is along vert[7] to vert[4]
     Face C 
     pt[1]
     pt[8]  is along vert[2] to vert[6]
     pt[5]
     p1[9]  is along vert[1] to vert[5]
     Face D
     pt[3]
     pt[10] is along vert[3] to vert[7]
     pt[7]
     pt[11] is along vert[0] to vert[4]
     FACE E
     pt[0]
     pt[9]
     pt[4]
     pt[11]
     Face F
     pt[2]
     pt[10]
     pt[6]
     pt[8]
  */

  /* Get Points set up */
  if((dist = check_line(val,vert[0],vert[1])) != FLT_MAX){
    pt[0].x = phpts3_dep(x,y,z)->x + 
      dist*(phpts3_dep(x+1,y,z)->x - phpts3_dep(x,y,z)->x);
    pt[0].y = phpts3_dep(x,y,z)->y + 
      dist*(phpts3_dep(x+1,y,z)->y - phpts3_dep(x,y,z)->y);
    pt[0].z = phpts3_dep(x,y,z)->z + 
	dist*(phpts3_dep(x+1,y,z)->z - phpts3_dep(x,y,z)->z);
    pt[0].inuse=1;
    A++; E++;
    mid.x += pt[0].x;
    mid.y += pt[0].y;
    mid.z += pt[0].z;
    i++;
  }
  else{pt[0].inuse=0;}

  if((dist = check_line(val,vert[1],vert[2])) != FLT_MAX){
    pt[1].x = phpts3_dep(x+1,y,z)->x + 
      dist*(phpts3_dep(x+1,y,z+1)->x - phpts3_dep(x+1,y,z)->x);
    pt[1].y = phpts3_dep(x+1,y,z)->y + 
      dist*(phpts3_dep(x+1,y,z+1)->y - phpts3_dep(x+1,y,z)->y);
    pt[1].z = phpts3_dep(x+1,y,z)->z + 
	dist*(phpts3_dep(x+1,y,z+1)->z - phpts3_dep(x,y+1,z)->z);
    pt[1].inuse=1;   
    A++; C++;
    mid.x += pt[1].x;
    mid.y += pt[1].y;
    mid.z += pt[1].z;
    i++;
  }
  else{pt[1].inuse=0;}

  if((dist = check_line(val,vert[2],vert[3])) != FLT_MAX){
    pt[2].x = phpts3_dep(x+1,y,z+1)->x + 
      dist*(phpts3_dep(x,y,z+1)->x - phpts3_dep(x+1,y,z+1)->x);
    pt[2].y = phpts3_dep(x+1,y,z+1)->y + 
      dist*(phpts3_dep(x,y,z+1)->y - phpts3_dep(x+1,y,z+1)->y);
    pt[2].z = phpts3_dep(x+1,y,z+1)->z + 
	dist*(phpts3_dep(x,y,z+1)->z - phpts3_dep(x+1,y,z+1)->z);
    pt[2].inuse=1;
    A++; F++;
    mid.x += pt[2].x;
    mid.y += pt[2].y;
    mid.z += pt[2].z;
    i++;
  }
  else{pt[2].inuse=0;}

  if((dist = check_line(val,vert[3],vert[0])) != FLT_MAX){
    pt[3].x = phpts3_dep(x,y,z+1)->x + 
      dist*(phpts3_dep(x,y,z)->x - phpts3_dep(x,y,z+1)->x);
    pt[3].y = phpts3_dep(1,y,z+1)->y + 
      dist*(phpts3_dep(x,y,z)->y - phpts3_dep(x,y,z+1)->y);
    pt[3].z = phpts3_dep(1,y,z+1)->z + 
	dist*(phpts3_dep(x,y,z)->z - phpts3_dep(x,y,z+1)->z);
    pt[3].inuse=1;
    A++; D++;
    mid.x += pt[3].x;
    mid.y += pt[3].y;
    mid.z += pt[3].z;
    i++;
  }
  else{pt[3].inuse=0;}

  if((dist = check_line(val,vert[4],vert[5])) != FLT_MAX){
    pt[4].x = phpts3_dep(x,y+1,z)->x + 
      dist*(phpts3_dep(x+1,y+1,z)->x - phpts3_dep(x,y+1,z)->x);
    pt[4].y = phpts3_dep(x,y+1,z)->y + 
      dist*(phpts3_dep(x+1,y+1,z)->y - phpts3_dep(x,y+1,z)->y);
    pt[4].z = phpts3_dep(x,y+1,z)->z + 
	dist*(phpts3_dep(x+1,y+1,z)->z - phpts3_dep(x,y+1,z)->z);
    pt[4].inuse=1;
    B++; E++;
    mid.x += pt[4].x;
    mid.y += pt[4].y;
    mid.z += pt[4].z;
    i++;
  }
  else{pt[4].inuse=0;}

  if((dist = check_line(val,vert[5],vert[6])) != FLT_MAX){
    pt[5].x = phpts3_dep(x+1,y+1,z)->x + 
      dist*(phpts3_dep(x+1,y+1,z+1)->x - phpts3_dep(x+1,y+1,z)->x);
    pt[5].y = phpts3_dep(x+1,y+1,z)->y + 
      dist*(phpts3_dep(x+1,y+1,z+1)->y - phpts3_dep(x+1,y+1,z)->y);
    pt[5].z = phpts3_dep(x+1,y+1,z)->z + 
	dist*(phpts3_dep(x+1,y+1,z+1)->z - phpts3_dep(x+1,y+1,z)->z);
    pt[5].inuse=1;
    B++; C++;
    mid.x += pt[5].x;
    mid.y += pt[5].y;
    mid.z += pt[5].z;
    i++;
  }
  else{pt[5].inuse=0;}

  if((dist = check_line(val,vert[6],vert[7])) != FLT_MAX){
    pt[6].x = phpts3_dep(x+1,y+1,z+1)->x + 
      dist*(phpts3_dep(x,y+1,z+1)->x - phpts3_dep(x+1,y+1,z+1)->x);
    pt[6].y = phpts3_dep(x+1,y+1,z+1)->y + 
      dist*(phpts3_dep(x,y+1,z+1)->y - phpts3_dep(x+1,y+1,z+1)->y);
    pt[6].z = phpts3_dep(x+1,y+1,z+1)->z + 
	dist*(phpts3_dep(x,y+1,z+1)->z - phpts3_dep(x+1,y+1,z+1)->z);
    pt[6].inuse=1;
    B++;F++;
    mid.x += pt[6].x;
    mid.y += pt[6].y;
    mid.z += pt[6].z;
    i++;
  }
  else{pt[6].inuse=0;}

  if((dist = check_line(val,vert[7],vert[4])) != FLT_MAX){
    pt[7].x = phpts3_dep(x,y+1,z+1)->x + 
      dist*(phpts3_dep(x,y+1,z)->x - phpts3_dep(x,y+1,z+1)->x);
    pt[7].y = phpts3_dep(x,y+1,z+1)->y + 
      dist*(phpts3_dep(x,y+1,z)->y - phpts3_dep(x,y+1,z+1)->y);
    pt[7].z = phpts3_dep(x,y+1,z+1)->z + 
      dist*(phpts3_dep(x,y+1,z)->z - phpts3_dep(x,y+1,z+1)->z);
    pt[7].inuse=1;
    B++; D++;
    mid.x += pt[7].x;
    mid.y += pt[7].y;
    mid.z += pt[7].z;
    i++;
  }
  else{pt[7].inuse=0;}

  if((dist = check_line(val,vert[2],vert[6])) != FLT_MAX){
    pt[8].x = phpts3_dep(x+1,y,z+1)->x + 
      dist*(phpts3_dep(x+1,y+1,z+1)->x - phpts3_dep(x+1,y,z+1)->x);
    pt[8].y = phpts3_dep(x+1,y,z+1)->y + 
      dist*(phpts3_dep(x+1,y+1,z+1)->y - phpts3_dep(x+1,y,z+1)->y);
    pt[8].z = phpts3_dep(x+1,y,z+1)->z + 
	dist*(phpts3_dep(x+1,y+1,z+1)->z - phpts3_dep(x+1,y,z+1)->z);
    pt[8].inuse=1;
    C++; F++; 
    mid.x += pt[8].x;
    mid.y += pt[8].y;
    mid.z += pt[8].z;
    i++;
  }
  else{pt[8].inuse=0;}

  if((dist = check_line(val,vert[1],vert[5])) != FLT_MAX){
    pt[9].x = phpts3_dep(x+1,y,z)->x + 
      dist*(phpts3_dep(x+1,y+1,z)->x - phpts3_dep(x+1,y,z)->x);
    pt[9].y = phpts3_dep(x+1,y,z)->y + 
      dist*(phpts3_dep(x+1,y+1,z)->y - phpts3_dep(x+1,y,z)->y);
    pt[9].z = phpts3_dep(x+1,y,z)->z + 
	dist*(phpts3_dep(x+1,y+1,z)->z - phpts3_dep(x+1,y,z)->z);
    pt[9].inuse=1;
    C++; E++;
    mid.x += pt[9].x;
    mid.y += pt[9].y;
    mid.z += pt[9].z;
    i++;
  }
  else{pt[9].inuse=0;}

  if((dist = check_line(val,vert[7],vert[3])) != FLT_MAX){
    pt[10].x = phpts3_dep(x,y+1,z+1)->x + 
      dist*(phpts3_dep(x,y,z+1)->x - phpts3_dep(x,y+1,z+1)->x);
    pt[10].y = phpts3_dep(x,y+1,z+1)->y + 
      dist*(phpts3_dep(x,y,z+1)->y - phpts3_dep(x,y+1,z+1)->y);
    pt[10].z = phpts3_dep(x,y+1,z+1)->z + 
	dist*(phpts3_dep(x,y,z+1)->z - phpts3_dep(x,y+1,z+1)->z);
    pt[10].inuse=1;
    D++; F++;
    mid.x += pt[10].x;
    mid.y += pt[10].y;
    mid.z += pt[10].z;
    i++;
  }
  else{pt[10].inuse=0;}

  if((dist = check_line(val,vert[4],vert[0])) != FLT_MAX){
    pt[11].x = phpts3_dep(x,y+1,z)->x + 
      dist*(phpts3_dep(x,y,z)->x - phpts3_dep(x,y+1,z)->x);
    pt[11].y = phpts3_dep(x,y+1,z)->y + 
      dist*(phpts3_dep(x,y,z)->y - phpts3_dep(x,y+1,z)->y);
    pt[11].z = phpts3_dep(x,y+1,z)->z + 
	dist*(phpts3_dep(x,y,z)->z - phpts3_dep(x,y+1,z)->z);
    pt[11].inuse=1 ;
    D++; E++;
    mid.x += pt[11].x;
    mid.y += pt[11].y;
    mid.z += pt[11].z;
    i++;
  }
  else{pt[11].inuse=0;}

  mid.x /= (float)i;
  mid.y /= (float)i;
  mid.z /= (float)i;

  /* Find triangles on each face - we know that no face can have an 
     odd number of hits from the proof found at:
     http://www.casi.net/D.DMT/D.Overview/AcademicPressPaper14-03/
     So we either have 0 1 or 2 triangles associated with each face
     This algorithm was developed by D. B. Karron, Ph.D. 
  */
  /* We use the Knit High Above, Knit Low Below rules as specified by 
     D. B. Karron, Ph.D. To do this we need the disambiguation value
     which is given by the point i + x0, j + y0, where 
     x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D))

     B(i,j+1)     C(i+1,j+1)
     --------------
     |   |        |
     |   |        |
     |   |        |
     |---|--------|
  / \|   |        |
   | |   |        |
   y0|   |        |
     --------------
     A(i,j)        D(i+1,j)
     x0 ->
  */
  numtri=0;
  printf("A=%d B=%d C=%d D=%d E=%d F=%d i=%d\n",A,B,C,D,E,F,i);
  if(A==2){
    for(i=0;i<4;i++)
      if(pt[sideA[i]].inuse) break;
    for(j=3;j>i;j--)
      if(pt[sideA[j]].inuse) break;
    if(i==j){printf("OOPS - only found 1point on face A\n");}
    else{
      tripts[numtri*3].x = pt[sideA[i]].x;
      tripts[numtri*3].y = pt[sideA[i]].y;
      tripts[numtri*3].x = pt[sideA[i]].z;

      tripts[numtri*3 + 1].x = pt[sideA[j]].x;
      tripts[numtri*3 + 1].y = pt[sideA[j]].y;
      tripts[numtri*3 + 1].x = pt[sideA[j]].z;

      tripts[numtri*3 + 2].x = mid.x;
      tripts[numtri*3 + 2].y = mid.y;
      tripts[numtri*3 + 2].x = mid.z;
      numtri++;
    }
  }
  if(A==4){
    /*vertexes 0,1,2,3  
      x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D)) 
    */
      cpt.x = i + get_x0(vert[0],vert[3],vert[2],vert[1]);
      cpt.y = j;
      cpt.z = k + get_y0(vert[0],vert[3],vert[2],vert[1]);
      tval1 = vert[0] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[3] - vert[0]);
      tval2 = vert[1] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[2] - vert[1]);
      cval = tval1 + get_dist(cpt.x,(float)x,(float)(x+1))*(tval2 - tval1);
      if(val > cval){ /*knit high*/
	KNITHIGH(vert[0],vert[3],pt[0],pt[1],pt[2],pt[3]);}
      else{
	KNITLOW(vert[0],vert[3],pt[0],pt[1],pt[2],pt[3]);}
  }
  if(B==2){
    for(i=0;i<4;i++)
      if(pt[sideB[i]].inuse) break;
    for(j=3;j>i;j--)
      if(pt[sideB[j]].inuse) break;
    if(i==j){printf("OOPS - only found 1point on face B\n");}
    else{
      tripts[numtri*3].x = pt[sideB[i]].x;
      tripts[numtri*3].y = pt[sideB[i]].y;
      tripts[numtri*3].x = pt[sideB[i]].z;

      tripts[numtri*3 + 1].x = pt[sideB[j]].x;
      tripts[numtri*3 + 1].y = pt[sideB[j]].y;
      tripts[numtri*3 + 1].x = pt[sideB[j]].z;

      tripts[numtri*3 + 2].x = mid.x;
      tripts[numtri*3 + 2].y = mid.y;
      tripts[numtri*3 + 2].x = mid.z;
      numtri++;
    }
  }
  if(B==4){
    /*vertexes 0,1,2,3  
      x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D)) 
    */
      cpt.x = i + get_x0(vert[4],vert[7],vert[6],vert[5]);
      cpt.y = j+1;
      cpt.z = k + get_y0(vert[4],vert[7],vert[6],vert[5]);
      tval1 = vert[4] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[7] - vert[4]);
      tval2 = vert[5] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[6] - vert[5]);
      cval = tval1 + get_dist(cpt.x,(float)x,(float)(x+1))*(tval2 - tval1);
      if(val > cval){ /*knit high*/
	KNITHIGH(vert[4],vert[7],pt[4],pt[5],pt[6],pt[7]);}
      else{
	KNITLOW(vert[4],vert[7],pt[4],pt[5],pt[6],pt[7]);}
  }
  if(C==2){
    for(i=0;i<4;i++)
      if(pt[sideC[i]].inuse) break;
    for(j=3;j>i;j--)
      if(pt[sideC[j]].inuse) break;
    if(i==j){printf("OOPS - only found 1point on face C\n");}
    else{
      tripts[numtri*3].x = pt[sideC[i]].x;
      tripts[numtri*3].y = pt[sideC[i]].y;
      tripts[numtri*3].x = pt[sideC[i]].z;

      tripts[numtri*3 + 1].x = pt[sideC[j]].x;
      tripts[numtri*3 + 1].y = pt[sideC[j]].y;
      tripts[numtri*3 + 1].x = pt[sideC[j]].z;

      tripts[numtri*3 + 2].x = mid.x;
      tripts[numtri*3 + 2].y = mid.y;
      tripts[numtri*3 + 2].x = mid.z;
      numtri++;
    }
  }
  if(C==4){
    /*vertexes 0,1,2,3  
      x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D)) 
    */
      cpt.x = i + 1;
      cpt.y = j + get_y0(vert[1],vert[2],vert[6],vert[5]);
      cpt.z = k + get_x0(vert[1],vert[2],vert[6],vert[5]);
      tval1 = vert[1] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[2] - vert[1]);
      tval2 = vert[5] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[6] - vert[5]);
      cval = tval1 + get_dist(cpt.y,(float)y,(float)(y+1))*(tval2 - tval1);
      if(val > cval){ /*knit high*/
	KNITHIGH(vert[1],vert[2],pt[9],pt[5],pt[8],pt[1]);}
      else{
	KNITLOW(vert[1],vert[2],pt[9],pt[5],pt[8],pt[1]);}
  }
  if(D==2){
    for(i=0;i<4;i++)
      if(pt[sideD[i]].inuse) break;
    for(j=3;j>i;j--)
      if(pt[sideD[j]].inuse) break;
    if(i==j){printf("OOPS - only found 1point on face D\n");}
    else{
      tripts[numtri*3].x = pt[sideD[i]].x;
      tripts[numtri*3].y = pt[sideD[i]].y;
      tripts[numtri*3].x = pt[sideD[i]].z;

      tripts[numtri*3 + 1].x = pt[sideD[j]].x;
      tripts[numtri*3 + 1].y = pt[sideD[j]].y;
      tripts[numtri*3 + 1].x = pt[sideD[j]].z;

      tripts[numtri*3 + 2].x = mid.x;
      tripts[numtri*3 + 2].y = mid.y;
      tripts[numtri*3 + 2].x = mid.z;
      numtri++;
    }
  }
  if(D==4){
    /*vertexes 0,1,2,3  
      x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D)) 
    */
      cpt.x = i;
      cpt.y = j + get_y0(vert[4],vert[7],vert[3],vert[0]);
      cpt.z = k + get_x0(vert[4],vert[7],vert[3],vert[0]);
      tval1 = vert[4] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[7] - vert[4]);
      tval2 = vert[0] + get_dist(cpt.z,(float)k,(float)(k+1))*(vert[3] - vert[0]);
      cval = tval1 + get_dist(cpt.y,(float)y,(float)(y+1))*(tval2 - tval1);
      if(val > cval){ /*knit high*/
	KNITHIGH(vert[4],vert[7],pt[11],pt[3],pt[10],pt[7]);}
      else{
	KNITLOW(vert[4],vert[7],pt[11],pt[3],pt[10],pt[7]);}
  }
  if(E==2){
    for(i=0;i<4;i++)
      if(pt[sideE[i]].inuse) break;
    for(j=3;j>i;j--)
      if(pt[sideE[j]].inuse) break;
    if(i==j){printf("OOPS - only found 1point on face E\n");}
    else{
      tripts[numtri*3].x = pt[sideE[i]].x;
      tripts[numtri*3].y = pt[sideE[i]].y;
      tripts[numtri*3].x = pt[sideE[i]].z;

      tripts[numtri*3 + 1].x = pt[sideE[j]].x;
      tripts[numtri*3 + 1].y = pt[sideE[j]].y;
      tripts[numtri*3 + 1].x = pt[sideE[j]].z;

      tripts[numtri*3 + 2].x = mid.x;
      tripts[numtri*3 + 2].y = mid.y;
      tripts[numtri*3 + 2].x = mid.z;
      numtri++;
    }
  }
  if(E==4){
    /*vertexes 0,1,2,3  
      x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D)) 
    */
      cpt.x = i + get_x0(vert[4],vert[0],vert[1],vert[5]);
      cpt.y = j + get_y0(vert[4],vert[0],vert[1],vert[5]);;
      cpt.z = k;
      tval1 = vert[0] + get_dist(cpt.y,(float)j,(float)(j+1))*(vert[4] - vert[0]);
      tval2 = vert[1] + get_dist(cpt.y,(float)j,(float)(j+1))*(vert[5] - vert[1]);
      cval = tval1 + get_dist(cpt.x,(float)x,(float)(x+1))*(tval2 - tval1);
      if(val > cval){ /*knit high*/
	KNITHIGH(vert[0],vert[4],pt[4],pt[9],pt[0],pt[11]);}
      else{
	KNITLOW(vert[0],vert[4],pt[4],pt[9],pt[0],pt[11]);}
  }
  if(F==2){
    for(i=0;i<4;i++)
      if(pt[sideF[i]].inuse) break;
    for(j=3;j>i;j--)
      if(pt[sideF[j]].inuse) break;
    if(i==j){printf("OOPS - only found 1point on face F\n");}
    else{
      tripts[numtri*3].x = pt[sideF[i]].x;
      tripts[numtri*3].y = pt[sideF[i]].y;
      tripts[numtri*3].x = pt[sideF[i]].z;

      tripts[numtri*3 + 1].x = pt[sideF[j]].x;
      tripts[numtri*3 + 1].y = pt[sideF[j]].y;
      tripts[numtri*3 + 1].x = pt[sideF[j]].z;

      tripts[numtri*3 + 2].x = mid.x;
      tripts[numtri*3 + 2].y = mid.y;
      tripts[numtri*3 + 2].x = mid.z;
      numtri++;
    }
  }
  if(F==4){
    /*vertexes 0,1,2,3  
      x0= (A-B) / ((A+C) - (B+D)) and y0= (A-D) / ((A+C) - (B+D)) 
    */
      cpt.x = i + get_x0(vert[3],vert[7],vert[6],vert[2]);
      cpt.y = j + get_y0(vert[3],vert[7],vert[6],vert[2]);
      cpt.z = k;
      tval1 = vert[3] + get_dist(cpt.y,(float)j,(float)(j+1))*(vert[7] - vert[3]);
      tval2 = vert[2] + get_dist(cpt.y,(float)j,(float)(j+1))*(vert[6] - vert[2]);
      cval = tval1 + get_dist(cpt.x,(float)x,(float)(x+1))*(tval2 - tval1);
      if(val > cval){ /*knit high*/
	KNITHIGH(vert[3],vert[7],pt[2],pt[8],pt[6],pt[10]);}
      else{
	KNITLOW(vert[3],vert[7],pt[2],pt[8],pt[6],pt[10]);}
  }
  if(filled){
    /*glColor3f(co.red,co.green,co.blue);*/
    glColor3f(1.,0.,0.);
    glBegin(GL_TRIANGLES);
    for(i=0; i< 3*numtri; i++){
      glVertex3f(tripts[i].x-transp.x,tripts[i].y-transp.y,
		 tripts[i].z-transp.z);
    }
    glEnd();
  }
  else{
    /*        glColor3f(co.red,co.green,co.blue);*/
        glColor3f(1.,0.,0.);
	get_errors("index setting triangles");
	printf("numtri: %d\n",numtri);
    for(i=0; i< 3*numtri; i+=3){
      glBegin(GL_LINE_STRIP);
      glVertex3f(tripts[i].x-transp.x,tripts[i].y-transp.y,
		 tripts[i].z-transp.z);
      glVertex3f(tripts[i+1].x-transp.x,tripts[i+1].y-transp.y,
		 tripts[i+1].z-transp.z);
      glVertex3f(tripts[i+2].x-transp.x,tripts[i+2].y-transp.y,
		 tripts[i+2].z-transp.z);
      glVertex3f(tripts[i].x-transp.x,tripts[i].y-transp.y,
		 tripts[i].z-transp.z);
      glEnd();
    }
  }
}


void ive_draw_wire(x, y, z, vert, val)
     int x,y,z;
     float vert[8], val;
{
  /*  ive_draw_spider(0, x, y, z, vert, val);*/
  ive_draw_wire_surf(0, x, y, z, vert, val);
}

void ive_draw_surf(x, y, z, vert, val)
     int x,y,z;
     float vert[8], val;
{
  ive_draw_wire_surf(1, x, y, z, vert, val);
  /*  ive_draw_spider(1, x, y, z, vert, val);*/
}

