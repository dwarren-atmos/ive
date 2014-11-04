#include <volume.h>
#include <ive_gl.h>
#include <math.h>
#include <malloc.h>
#define MAXDIM 4

extern void getavar_(),getivar_(),make_help_widget_(),getiarr_(),getlvar_(),
  getrarr_(),getrvar_();
extern float * slicer3d_();
extern Widget xgks_widget;
extern double interp_();
extern float deref2d_();

Objects IVE_Object;
#define OBS IVE_Object.Surface[nObjects-1]
#define OBN IVE_Object.NormalList[nObjects-1]

static int first_3d=0;

void reset_nobjects()
{
  int i;
  if(nObjects > 0){
    if(IVE_Object.NormalList){
      if(IVE_Object.NormalList->normal)
	free(IVE_Object.NormalList->normal);
      free(IVE_Object.NormalList);
      IVE_Object.NormalList=NULL;
    }
    if(IVE_Object.Surface){
      if(IVE_Object.Surface->items)
	free(IVE_Object.Surface->items);
      free(IVE_Object.Surface);
      IVE_Object.Surface=NULL;
    }
  }
  for(i = 0; i < nObjects; i++){
    free(IVE_Object.Field[i]);
    IVE_Object.Field[i]=NULL;
    IVE_Object.objectOn[i] = 0;
    IVE_Object.objectDone[i] = 0;
    if(glIsList(IVE_Object.listName[i]))
      glDeleteLists(IVE_Object.listName[i],1);
  }
  if(glIsList(terrlist)){
    glDeleteLists(terrlist,1);
    IVE_Object.listName[i] = -1;
  }
  IVE_Object.objects = 0;
  nObjects=0;

}

static void compute_normal(v, p1, p2, p3)
     struct plainpoint *v,p1,p2,p3;
{
  //(p2-p1)X(p3-p1)
  struct plainpoint v1,v2;
  v1.x=p2.x-p1.x;
  v1.y=p2.y-p1.y;
  v1.z=p2.z-p1.z;

  v2.x=p3.x-p1.x;
  v2.y=p3.y-p1.y;
  v2.z=p3.z-p1.z;

  v->x=(v1.y*v2.z)-(v1.z*v2.y);
  v->y=(v1.z*v2.x)-(v1.x*v2.z);
  v->z=(v1.x*v2.y)-(v1.y*v2.x);

  float vMag;
  vMag=(v->x*v->x)+(v->y*v->y)+(v->z*v->z);
  vMag=sqrt(vMag);
  v->x=(v->x/vMag);
  v->y=(v->y/vMag);
  v->z=(v->z/vMag);

}

static int equal_points(p1, p2, delta)
     struct plainpoint p1, p2;
     float delta;
{
  float x,y,z;
  x=abs(p1.x-p2.x);
  y=abs(p1.y-p2.y);
  z=abs(p1.z-p2.z);
  if(x<delta && y<delta && z<delta)
    //if((p1.x==p2.x)&&(p1.y==p2.y)&&(p1.z==p2.z))
    return(1);
  
  return(0);
}

static int equal_ter(p1, p2, delta)
     struct plainpoint p1, p2;
     float delta;
{
  if((p1.x==p2.x)&&(p1.y==p2.y))
    return(1);

  return(0);
}

static int check_out_range(t, nx, ny, nz)
     struct TRIANGLE t;
     int nx,ny,nz;
{
  if(t.p1.x > nx || t.p1.x < 0)
    return(1);
  if(t.p2.x > nx || t.p2.x < 0)
    return(1);
  if(t.p3.x > nx || t.p3.x < 0)
    return(1);

  if(t.p1.y > ny || t.p1.y < 0)
    return(1);
  if(t.p2.y > ny || t.p2.y < 0)
    return(1);
  if(t.p3.y > ny || t.p3.y < 0)
    return(1);

  if(t.p1.z > nz || t.p1.z < 0)
    return(1);
  if(t.p2.z > nz || t.p2.z < 0)
    return(1);
  if(t.p3.z > nz || t.p3.z < 0)
    return(1);
  return(0);
}

void make3d_(varpt, x, y, z, t)
     float  *varpt;
     int    *x, *y, *z, *t;
{


  const float zero = 0.0;
  void (*ive_3d_pltfunc)();
  char pltyp[16];
  float pt[4], mins[4], maxs[4], stag[4], vert[8], delta[4], dx, 
    *volume, *topohghts, dims, special, sint, mindelta, mymins[3], mymaxs[3],
    *terrain;
  double domain_intercept[4], domain_slope[4];
  unsigned long pixel;  
  extern unsigned long IveGetPixel();
  int fix[4], ni, nj, nk, nteri, nterk, i, j, k, l, m, imap,
    saveflag, buttons, error, phys, fd, lval, width, height, npts, linlog;
  int count = 0, allocated_norms, allocated_triangles;
  struct{float x,y,z;}bpt[8];
  struct TRIANGLES triangles,smalltri,tertris;
  terrmesh terrainmesh;
  FILE *file;
  char field[80];
  //  extern void ive_draw_surf(),  ive_draw_box(), ive_draw_points(),
  //ive_draw_wire(), 
  extern void ive_get_surface();
  extern void ive_get_points();

  /*Start the 3D thing*/
  //  (void)setup_3D();
  if(!first_3d){
    terrlist=glGenLists(1);
    xRotation=0.;
    yRotation=0.;
    LRMult=1.;
    UDMult=1.;
    (void)reset_all_3d();
    first_3d=1;
  }
  /* get  the type of plot */
  (void)getavar_("plotyp3",pltyp,&error,7,16);

  /*get linlog*/
  (void)getivar_("linlog3d", &linlog, &error, 8);
  if(error || !linlog)
    linlog=0;

  /*get the locked dimension*/
  (void)getivar_("use_buttons", &buttons, &error, 11);
  if(error || !buttons){
    (void)make_help_widget_("3d volumes must be orthagonal, and selected with the slicer widget");
    return;
  }
  buttons=4;
  (void)getiarr_("lock", fix, &buttons, &error, 4);
  if(fix[0] + fix[1] + fix[2] + fix[3] != 1){
    (void)make_help_widget_("You must lock exactly one dimension to generate a volume");
    return;
  }

  /*Overlay?*/
  (void)getlvar_("savflg", &saveflag, &error, 6);
  if (!saveflag){
    (void)reset_nobjects();
  }
  i=4;
  (void)getrarr_("plwmin_scaled",mins, &i, &error, 13);
  (void)getrarr_("plwmax_scaled",maxs, &i, &error, 13);
  (void)getdarr_("domain_slope", domain_slope, &i, &error,12);
  (void)getdarr_("domain_intercept", domain_intercept, &i, &error,16);
  (void)getrarr_("grid_delta", delta, &i, &error,10);
  bzero(field,80);
  (void)getavar_("field",field,&error,5,80);
  i=4;
  getrarr_("stag",stag, &i, &error, 4);
  mindelta = delta[0];
  if(delta[1]<mindelta)mindelta=delta[1];
  if(delta[2]<mindelta)mindelta=delta[2];
  if(delta[3]<mindelta)mindelta=delta[3];

  /*Get Volume*/
  (void)getivar_("num_dims", &dims, &error, 8);
  (void)getrvar_("spval", &special, &error, 5);
  (void)getrvar_("sint", &sint, &error, 4);
  phys=0;
  for(lval=0; lval<4; lval++){
    if(fix[lval])break;
  }
  pt[0] = pt[1] = pt[2] = pt[3] = 0.0;
  lval++;
  switch(lval){
  case 1: (void)getrvar_("xloc", &pt, &error, 4);
    break;
  case 2: (void)getrvar_("yloc", &pt[1], &error, 4);
    break;
  case 3: (void)getrvar_("xloc", &pt[2], &error, 4);
    break;
  case 4: (void)getrvar_("tloc", &pt[3], &error, 4);
    break;
  }
  volume = (float *)slicer3d_(varpt, x, y, z, t,
			      &dims, &lval, pt, &nk, &nj, &ni, &special, &phys);
  
  // get approx dx for limiting search later
  dx = abs(maxs[0]-mins[0])/ni;
  if(mindelta==0)mindelta=1.0e-15;
  allocated_triangles=3*ni*nj;
  //allocated_norms=ni;
  allocated_norms=3*ni*nj; //same as triangles now
  if(!saveflag){
    struct plainpoint *plpt;
    terrain=malloc(ni*nj*sizeof(float));
    terrainmesh.x=ni;
    terrainmesh.y=nj;
    terrainmesh.points=(GLfloat *)malloc(ni*nj*3*sizeof(GLfloat));
    (void)horiz_ter_trans_(terrain,&ni,&nj,&stag[0],&stag[1],
			   &mins[2],&error);
    if(!error){
      int one=1;
      float rone=1.;
      k=0;
      imap = 2;
      float xi,yj,xip1,yjp1;
      int ip1,jp1,ip2,jp2;
      npts=1;
      tertris.tri=(struct TRIANGLE *)malloc(2*(ni-1)*(nj-1)*sizeof(struct TRIANGLE));
      tertris.norm_counts=(int *)malloc(2*ni*nj*sizeof(int));
      tertris.normals=(struct plainpoint *)malloc(2*ni*nj*sizeof(struct plainpoint));
      tertris.norm_points=(struct plainpoint *)malloc(2*ni*nj*sizeof(struct plainpoint));
      bzero(tertris.tri,2*(ni-1)*(nj-1)*sizeof(struct TRIANGLE));
      bzero(tertris.norm_counts,2*ni*nj*sizeof(int));
      bzero(tertris.normals,2*ni*nj*sizeof(struct plainpoint));
      bzero(tertris.norm_points,2*ni*nj*sizeof(struct plainpoint));
      tertris.num_normals=0;
      tertris.num_triangles=(ni-1)*(nj-1)*2;
      for(i=0; i<ni-1; i++){
	for(j=0; j<nj-1; j++){
	  xi=i;
	  yj=j;
	  xip1=i+1;
	  yjp1=j+1;
	  ip1=xip1;
	  jp1=yjp1;
	  ip2=i+2;
	  jp2=j+2;
	  (void) cpmpxy_(&imap, &xi,&yj, 
			 &tertris.tri[k].p1.x, &tertris.tri[k].p1.y, 
			 &special);
	  tertris.tri[k].p1.z=interp_(terrain, &ni,&nj,&one,&one,
				      &xi,&yj,&rone,&rone,&special);
	  (void)scale_(&tertris.tri[k].p1.x,&npts,
	  	       domain_slope,domain_intercept,&special);
	  (void)scale_(&tertris.tri[k].p1.y,&npts,
	  	       domain_slope+1,domain_intercept+1,&special);
	  (void)scale_(&tertris.tri[k].p1.z,&npts,
	  	       domain_slope+2,domain_intercept+2,&special);
	  
	  (void) cpmpxy_(&imap, &xip1, &yj,
			 &tertris.tri[k].p2.x, &tertris.tri[k].p2.y, 
			 &special);
	  tertris.tri[k].p2.z=interp_(terrain, &ni,&nj,&one,&one,
				      &xip1,&yj,&rone,&rone);
	  (void)scale_(&tertris.tri[k].p2.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&tertris.tri[k].p2.y,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  (void)scale_(&tertris.tri[k].p2.z,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  
	  (void) cpmpxy_(&imap, &xip1, &yjp1,
			 &tertris.tri[k].p3.x, &tertris.tri[k].p3.y, 
			 &special);
	  tertris.tri[k].p3.z=interp_(terrain, &ni,&nj,&one,&one,
				      &xip1,&yjp1,&rone,&rone);
	  (void)scale_(&tertris.tri[k].p3.x,&npts,
	  	       domain_slope,domain_intercept,&special);
	  (void)scale_(&tertris.tri[k].p3.y,&npts,
	  	       domain_slope+1,domain_intercept+1,&special);
	  (void)scale_(&tertris.tri[k].p3.z,&npts,
	  	       domain_slope+2,domain_intercept+2,&special);
	  
	  if(tertris.tri[k].p1.z>mins[2] && tertris.tri[k].p2.z>mins[2] && tertris.tri[k].p3.z>mins[2])
	    {
	      k++;
	      tertris.tri[k].p1.x=tertris.tri[k-1].p1.x;
	      tertris.tri[k].p1.y=tertris.tri[k-1].p1.y;
	      tertris.tri[k].p1.z=tertris.tri[k-1].p1.z;
	      tertris.tri[k].p2.x=tertris.tri[k-1].p3.x;
	      tertris.tri[k].p2.y=tertris.tri[k-1].p3.y;
	      tertris.tri[k].p2.z=tertris.tri[k-1].p3.z;
	    }
	  else{
	    tertris.tri[k].p2.x=tertris.tri[k].p3.x;
	    tertris.tri[k].p2.y=tertris.tri[k].p3.y;
	    tertris.tri[k].p2.z=tertris.tri[k].p3.z;
	  }
	  (void) cpmpxy_(&imap, &xi, &yjp1,
			 &tertris.tri[k].p3.x, &tertris.tri[k].p3.y, &special);
	  tertris.tri[k].p3.z=interp_(terrain, &ni,&nj,&one,&one,
				      &xi,&yjp1,&rone,&rone);
	  (void)scale_(&tertris.tri[k].p3.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&tertris.tri[k].p3.y,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  (void)scale_(&tertris.tri[k].p3.z,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  
	  if(tertris.tri[k].p1.z>mins[2] && tertris.tri[k].p2.z>mins[2] && tertris.tri[k].p3.z>mins[2])
	    k++;
	}
      }
      for(i=0; i<k; i++)
	{
	  struct plainpoint mynormal;
	  int p1done, p2done, p3done;
	  p1done=0;p2done=0;p3done=0;
	  compute_normal(&mynormal, 
			 tertris.tri[i].p1, 
			 tertris.tri[i].p2,
			 tertris.tri[i].p3);
	  goto skip;
	  for (m=0; m<tertris.num_normals; m++){
	    if(!p1done && equal_ter(tertris.tri[i].p1,tertris.norm_points[m],
				    mindelta))
	      {
		tertris.normals[m].x += mynormal.x;
		tertris.normals[m].y += mynormal.y;
		tertris.normals[m].z += mynormal.z;
		tertris.norm_counts[m]+=1;
		tertris.tri[i].normal1=m;
		p1done=1;
	      }
	    
	    if(!p2done && equal_ter(tertris.tri[i].p2,tertris.norm_points[m],
				    mindelta))
	      {
		tertris.normals[m].x += mynormal.x;
		tertris.normals[m].y += mynormal.y;
		tertris.normals[m].z += mynormal.z;
		tertris.norm_counts[m]+=1;
		tertris.tri[i].normal2=m;
		p2done=1;
	      }
	    
	    if(!p3done && equal_ter(tertris.tri[i].p3,tertris.norm_points[m],mindelta))
	      {
		tertris.normals[m].x += mynormal.x;
		tertris.normals[m].y += mynormal.y;
		tertris.normals[m].z += mynormal.z;
		tertris.norm_counts[m]+=1;
		tertris.tri[i].normal3=m;
		p3done=1;
	      }
	    if(p1done&&p2done&&p3done)break;
	  }
	  if(!p1done){
	    tertris.norm_counts[tertris.num_normals]=1;
	    tertris.normals[tertris.num_normals].x = mynormal.x;
	    tertris.normals[tertris.num_normals].y = mynormal.y;
	    tertris.normals[tertris.num_normals].z = mynormal.z;
	    tertris.norm_points[tertris.num_normals].x=tertris.tri[i].p1.x;
	    tertris.norm_points[tertris.num_normals].y=tertris.tri[i].p1.y;
	    tertris.norm_points[tertris.num_normals].z=tertris.tri[i].p1.z;
	    //tertris.tri[i].normal1=tertris.num_normals;
	    tertris.tri[i].normal1=3*i;
	    tertris.num_normals++;
	  }
	  
	  if(!p2done){
	    tertris.norm_counts[tertris.num_normals]=1;
	    tertris.normals[tertris.num_normals].x = mynormal.x;
	    tertris.normals[tertris.num_normals].y = mynormal.y;
	    tertris.normals[tertris.num_normals].z = mynormal.z;
	    tertris.norm_points[tertris.num_normals].x=tertris.tri[i].p2.x;
	    tertris.norm_points[tertris.num_normals].y=tertris.tri[i].p2.y;
	    tertris.norm_points[tertris.num_normals].z=tertris.tri[i].p2.z;
	    //tertris.tri[i].normal2=tertris.num_normals;
	    tertris.tri[i].normal2=3*i+1;
	    tertris.num_normals++;
	  }
	  
	  if(!p3done){
	    tertris.norm_counts[tertris.num_normals]=1;
	    tertris.normals[tertris.num_normals].x = mynormal.x;
	    tertris.normals[tertris.num_normals].y = mynormal.y;
	    tertris.normals[tertris.num_normals].z = mynormal.z;
	    tertris.norm_points[tertris.num_normals].x=tertris.tri[i].p3.x;
	    tertris.norm_points[tertris.num_normals].y=tertris.tri[i].p3.y;
	    tertris.norm_points[tertris.num_normals].z=tertris.tri[i].p3.z;
	    //tertris.tri[i].normal3=tertris.num_normals;
	    tertris.tri[i].normal3=3*i+2;
	    tertris.num_normals++;
	  }
	skip:
	  //Above is for figuring point normals. This is all we do for face normals
	  tertris.norm_counts[tertris.num_normals]=1;
	  tertris.normals[tertris.num_normals].x = mynormal.x;
	  tertris.normals[tertris.num_normals].y = mynormal.y;
	  tertris.normals[tertris.num_normals].z = mynormal.z;
	  tertris.norm_points[tertris.num_normals].x=tertris.tri[i].p1.x;
	  tertris.norm_points[tertris.num_normals].y=tertris.tri[i].p1.y;
	  tertris.norm_points[tertris.num_normals].z=tertris.tri[i].p1.z;
	  //tertris.tri[i].normal1=tertris.num_normals;
	  tertris.tri[i].normal1=i;
	  tertris.tri[i].normal2=i;
	  tertris.tri[i].normal3=i;
	  tertris.num_normals++;
	}
      //for(i=0; i<k; i++){
      //tertris.normals[i].x= (tertris.normals[i].x/(float)tertris.norm_counts[i]);
      //tertris.normals[i].y= (tertris.normals[i].y/(float)tertris.norm_counts[i]);
      //tertris.normals[i].z= (tertris.normals[i].z/(float)tertris.norm_counts[i]);
      //}
      if(tertris.num_triangles == 0){
	free(tertris.tri);
	free(tertris.norm_counts);
	free(tertris.normals);
	free(tertris.norm_points);
	nObjects=0;
      }
      else{
	nObjects=1;
	IVE_Object.objects = nObjects;
	IVE_Object.Surface = malloc(nObjects*sizeof(Surface));
	IVE_Object.objectDone[nObjects-1]=0;
	OBS.size = tertris.num_triangles;
	OBS.items = malloc(OBS.size*sizeof(Triangle));
	IVE_Object.NormalList = malloc(nObjects*sizeof(NormalList));
	OBN.size = tertris.num_normals;
	OBN.normal = malloc(OBN.size*sizeof(Point));
	IVE_Object.Field[0] = malloc(8*sizeof(char));    
	strcpy(IVE_Object.Field[nObjects-1],"Terrain");
	//printf("terrain: %d triangles, %d normals\n",OBS.size,OBN.size);
#ifdef OMP
#pragma omp parallel for default(shared)
#endif
	for(i=0; i<tertris.num_triangles; i++){
	  OBS.items[i].pt[0].xCoord = tertris.tri[i].p1.x;
	  OBS.items[i].pt[0].yCoord = tertris.tri[i].p1.z;
	  OBS.items[i].pt[0].zCoord = tertris.tri[i].p1.y;
	  OBS.items[i].pt[0].normalRef = tertris.tri[i].normal1;
	  OBS.items[i].pt[1].xCoord = tertris.tri[i].p2.x;
	  OBS.items[i].pt[1].yCoord = tertris.tri[i].p2.z;
	  OBS.items[i].pt[1].zCoord = tertris.tri[i].p2.y;
	  OBS.items[i].pt[1].normalRef = tertris.tri[i].normal2;
	  OBS.items[i].pt[2].xCoord = tertris.tri[i].p3.x;
	  OBS.items[i].pt[2].yCoord = tertris.tri[i].p3.z;
	  OBS.items[i].pt[2].zCoord = tertris.tri[i].p3.y;
	  OBS.items[i].pt[2].normalRef = tertris.tri[i].normal3;
	}
#ifdef OMP
#pragma omp parallel for default(shared)
#endif
	for(i=0; i<tertris.num_normals; i++){
	  OBN.normal[i].xCoord = tertris.normals[i].x;
	  OBN.normal[i].yCoord = tertris.normals[i].z;
	  OBN.normal[i].zCoord = tertris.normals[i].y;
	  OBN.normal[i].normalRef = i;
	}
	free(tertris.tri);
	free(tertris.norm_counts);
	free(tertris.normals);
	free(tertris.norm_points);
      }
    }
    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //Call to fortran -> gives array terrain[nj][ni] (YxX with value=height
    //FORTRAN arrays are filled backwards!!!
    //To get x,y and z use: (for terrain[j][i] - where i is in x and j is in y)
    //imap=2
    //(void)cpmpxyz_(&imap,&i,&j,&,terrain[j][i])
    //cpmpxyz_ does the array space to physical space conversion
    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  }
  if(pltyp[0] != 'S') {

  triangles.tri = 
    (struct TRIANGLE *)malloc(allocated_triangles*sizeof(struct TRIANGLE)); 
  smalltri.tri = (struct TRIANGLE *)malloc(10*sizeof(struct TRIANGLE));
  smalltri.num_triangles=0;
  triangles.num_triangles=0;
  triangles.num_normals=0;
  smalltri.num_normals=0;
  //if(special == 0.)special=1.E-19;
  for(i=0; i<ni-1; i++){
    for(j=0; j<nj-1; j++){
      for(k=0; k<nk-1; k++){
	int one=1;
	float x,y,z,xp1,yp1,zp1,t;
	struct plainpoint points[8];
	struct TRIANGLE *dwtri;
	t=1.;
	x=i+1; y=j+1; z=k+1; //interp is fortran
	xp1=(x+1.); yp1=(y+1.); zp1=(z+1.);
	//if(i==ni)xp1=(i);
	//if(j==nj)yp1=(j);
	//if(k==nk)zp1=(k);
	vert[0]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&yp1,&z,&t,&special);
	vert[1]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&yp1,&z,&t,&special);
	vert[2]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&y,&z,&t,&special);
	vert[3]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&y,&z,&t,&special);
	vert[4]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&yp1,&zp1,&t,&special);
	vert[5]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&yp1,&zp1,&t,&special);
	vert[6]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&y,&zp1,&t,&special);
	vert[7]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&y,&zp1,&t,&special);
	points[0].x=x; points[0].y=yp1; points[0].z=z;
	points[1].x=xp1; points[1].y=yp1;points[1].z=z;
	points[2].x=xp1; points[2].y=y;	points[2].z=z;
	points[3].x=x;	points[3].y=y;	points[3].z=z;
	points[4].x=x;	points[4].y=yp1; points[4].z=zp1;
	points[5].x=xp1; points[5].y=yp1; points[5].z=zp1;
	points[6].x=xp1; points[6].y=y; points[6].z=zp1;
	points[7].x=x;	points[7].y=y; points[7].z=zp1;
	
	smalltri.num_triangles=0;
	  if(vert[0]==special||vert[1]==special||vert[2]==special||
	     vert[3]==special||vert[4]==special||vert[5]==special||
	     vert[7]==special||vert[7]==special)
	    continue;
	ive_get_surface(vert,sint,points,&smalltri);
	if(smalltri.num_triangles==0)continue;
	//ive_get_points(0,0,0,vert,sint,&smalltri);
	//ive_get_surface(i,j,k,vert,sint,&smalltri);
	if(triangles.num_triangles + smalltri.num_triangles > 
	   allocated_triangles-1){
	  struct TRIANGLE *dwtri;
	  void *ptr;
	  allocated_triangles += 2*ni*nj;
	  //printf("realloc triangles %lx %d\n",triangles.tri, allocated_triangles);
	  dwtri = (struct TRIANGLE *) realloc(triangles.tri, 
					      (allocated_triangles*sizeof(struct TRIANGLE)));
	  if(dwtri !=(struct TRIANGLE *)NULL)triangles.tri=dwtri;
	  else {printf("out of memory for triangles\n");break;}

	}
	imap=2;
	for (l=0; l< smalltri.num_triangles; l++){
	  npts=1;
	  if(equal_points(smalltri.tri[l].p1,smalltri.tri[l].p2,mindelta) || 
	     equal_points(smalltri.tri[l].p1,smalltri.tri[l].p3,mindelta) || 
	     equal_points(smalltri.tri[l].p3,smalltri.tri[l].p2,mindelta))
	    {
	      continue;
	    }
	  if(check_out_range(smalltri.tri[l],ni-1,nj-1,nk-1))
	    {
	      continue;
	    }
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p1.x,&smalltri.tri[l].p1.y,&smalltri.tri[l].p1.z,
			 &triangles.tri[triangles.num_triangles].p1.x,
			 &triangles.tri[triangles.num_triangles].p1.z,
			 &triangles.tri[triangles.num_triangles].p1.y,&special);
	  if(triangles.tri[triangles.num_triangles].p1.x==special){
	    continue;
	  }
	  (void)scale_(&triangles.tri[triangles.num_triangles].p1.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p1.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p1.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  //triangles.num_triangles++;  

	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p2.x,&smalltri.tri[l].p2.y,&smalltri.tri[l].p2.z,
			 &triangles.tri[triangles.num_triangles].p2.x,
			 &triangles.tri[triangles.num_triangles].p2.z,
			 &triangles.tri[triangles.num_triangles].p2.y,&special);
	  if(triangles.tri[triangles.num_triangles].p2.x==special){
	    continue;
	  } 
	  (void)scale_(&triangles.tri[triangles.num_triangles].p2.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p2.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p2.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  //triangles.num_triangles++;  

	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p3.x,&smalltri.tri[l].p3.y,&smalltri.tri[l].p3.z,
			 &triangles.tri[triangles.num_triangles].p3.x,
			 &triangles.tri[triangles.num_triangles].p3.z,
			 &triangles.tri[triangles.num_triangles].p3.y,&special);
	  if(triangles.tri[triangles.num_triangles].p3.x==special){
	    continue;
	    }
	  (void)scale_(&triangles.tri[triangles.num_triangles].p3.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p3.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p3.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);

	  triangles.num_triangles++;
	}
      }
    }
  }
  free(smalltri.tri);
  if(triangles.num_triangles == 0){
    printf("No triangles found at %s@%f\n",field,sint);
    /*
    if(triangles.normals)free(triangles.normals);
    if(triangles.norm_counts)free(triangles.norm_counts);
    if(triangles.norm_points)free(triangles.norm_points);
    */
    if(triangles.tri)free(triangles.tri);
    if(nObjects >0){
      (void)plot3d(mins, maxs, &IVE_Object);
    }
    if(volume != NULL)free(volume);
    return;
  }
  allocated_norms=triangles.num_triangles;
  triangles.normals=(struct plainpoint *)malloc(allocated_norms*sizeof(struct plainpoint));
  triangles.norm_points=(struct plainpoint *)malloc(allocated_norms*sizeof(struct plainpoint));
  triangles.norm_counts=(int *)malloc(allocated_norms*sizeof(int));
  for(i=0; i<triangles.num_triangles; i++){
    struct plainpoint mynormal;
    int p1done, p2done, p3done;
    p1done=0;p2done=0;p3done=0;
    compute_normal(&mynormal, 
		   triangles.tri[i].p1, 
		   triangles.tri[i].p2,
		   triangles.tri[i].p3);
    //    if(!(i%10000))printf("check %d\n",i);
    goto skip2;
    for (m=triangles.num_normals-1; m<=0;m--){
      
      if(!p1done && equal_points(triangles.tri[i].p1,triangles.norm_points[m],mindelta))
	{
	  triangles.normals[m].x += mynormal.x;
	  triangles.normals[m].y += mynormal.y;
	  triangles.normals[m].z += mynormal.z;
	  triangles.norm_counts[m]++;
	  triangles.tri[i].normal1=m;
	  p1done=1;
	}
      
      if(!p2done && equal_points(triangles.tri[i].p2,triangles.norm_points[m],mindelta))
	{
	  triangles.normals[m].x += mynormal.x;
	  triangles.normals[m].y += mynormal.y;
	  triangles.normals[m].z += mynormal.z;
	  triangles.norm_counts[m]++;
	  triangles.tri[i].normal2=m;
	  p2done=1;
	}
      
      if(!p3done && equal_points(triangles.tri[i].p3,triangles.norm_points[m],mindelta))
	{
	  triangles.normals[m].x += mynormal.x;
	  triangles.normals[m].y += mynormal.y;
	  triangles.normals[m].z += mynormal.z;
	  triangles.norm_counts[m]++;
	  triangles.tri[i].normal3=m;
	  p3done=1;
	}
      if(p1done && p2done && p3done)break;
      if(triangles.normals[m].x < (triangles.tri[i].p1.x - 10*dx))break;
    }
    if(p1done && p2done && p3done)continue;
  skip2:
    /*
    if(triangles.num_normals+(3-p1done-p2done-p3done)> allocated_norms-1){
      void *ptr;
      allocated_norms += 100;
      ptr=(struct plainpoint *)realloc(triangles.norm_points,
				       allocated_norms*sizeof(struct plainpoint));
      if(ptr!=0)
	triangles.norm_points=ptr;
      else {break;}
      ptr=(struct plainpoint *)realloc(triangles.normals,
			       allocated_norms*sizeof(struct plainpoint));
      if(ptr != 0)
	triangles.normals=ptr;
      else{break;}

      ptr=(int *)realloc(triangles.norm_counts,allocated_norms*sizeof(int));
      if(ptr != 0)
	triangles.norm_counts =ptr;
      else {break;}
    }
    if(!p1done){
      triangles.norm_counts[triangles.num_normals]=1;
      triangles.normals[triangles.num_normals].x = mynormal.x;
      triangles.normals[triangles.num_normals].y = mynormal.y;
      triangles.normals[triangles.num_normals].z = mynormal.z;
      triangles.norm_points[triangles.num_normals].x=triangles.tri[i].p1.x;
      triangles.norm_points[triangles.num_normals].y=triangles.tri[i].p1.y;
      triangles.norm_points[triangles.num_normals].z=triangles.tri[i].p1.z;
      triangles.tri[i].normal1=triangles.num_normals;
      triangles.num_normals++; 
   }
	      
    if(!p2done){
      triangles.norm_counts[triangles.num_normals]=1;
      triangles.normals[triangles.num_normals].x = mynormal.x;
      triangles.normals[triangles.num_normals].y = mynormal.y;
      triangles.normals[triangles.num_normals].z = mynormal.z;
      triangles.norm_points[triangles.num_normals].x=triangles.tri[i].p2.x;
      triangles.norm_points[triangles.num_normals].y=triangles.tri[i].p2.y;
      triangles.norm_points[triangles.num_normals].z=triangles.tri[i].p2.z;
      triangles.tri[i].normal2=triangles.num_normals;
      triangles.num_normals++;
    }
	      
    if(!p3done){
      triangles.norm_counts[triangles.num_normals]=1;
      triangles.normals[triangles.num_normals].x = mynormal.x;
      triangles.normals[triangles.num_normals].y = mynormal.y;
      triangles.normals[triangles.num_normals].z = mynormal.z;
      triangles.norm_points[triangles.num_normals].x=triangles.tri[i].p3.x;
      triangles.norm_points[triangles.num_normals].y=triangles.tri[i].p3.y;
      triangles.norm_points[triangles.num_normals].z=triangles.tri[i].p3.z;
      triangles.tri[i].normal3=triangles.num_normals;
      triangles.num_normals++;
    }
    Above is for dealing with point normals we do side normals now!!!!!
    */ 
      triangles.norm_counts[triangles.num_normals]=1;
      triangles.normals[triangles.num_normals].x = mynormal.x;
      triangles.normals[triangles.num_normals].y = mynormal.y;
      triangles.normals[triangles.num_normals].z = mynormal.z;
      triangles.norm_points[triangles.num_normals].x=triangles.tri[i].p3.x;
      triangles.norm_points[triangles.num_normals].y=triangles.tri[i].p3.y;
      triangles.norm_points[triangles.num_normals].z=triangles.tri[i].p3.z;
      triangles.tri[i].normal1=triangles.num_normals;
      triangles.tri[i].normal2=triangles.num_normals;
      triangles.tri[i].normal3=triangles.num_normals;
      triangles.num_normals++;
  }

  //printf("here\n");

  //for (i=0; i<triangles.num_normals; i++){
  //triangles.normals[i].x= -1.*(triangles.normals[i].x/(float)triangles.norm_counts[i]);
  //triangles.normals[i].y= -1.*(triangles.normals[i].y/(float)triangles.norm_counts[i]);
  //triangles.normals[i].z= -1.*(triangles.normals[i].z/(float)triangles.norm_counts[i]);
  //}
  //  printf("got %d triangles %d normals\n",
  //triangles.num_triangles,triangles.num_normals);

 
  
  /*now make it look right for Brian's code */
  }
  else{
    // we're going to cheat here and use triangle structures for points 
    // to not have to muck with the plotting code for now.
    triangles.tri = 
      (struct TRIANGLE *)malloc(allocated_triangles*sizeof(struct TRIANGLE)); 
    smalltri.tri = (struct TRIANGLE *)malloc(12*sizeof(struct TRIANGLE));
    smalltri.num_triangles=0;
    triangles.num_triangles=0;
    triangles.num_normals=0;
    smalltri.num_normals=0;

    for(i=0; i<ni-1; i++){
      for(j=0; j<nj-1; j++){
	for(k=0; k<nk-1; k++){
	  int one=1;
	  float x,y,z,xp1,yp1,zp1,t;
	  struct plainpoint points[8];
	  struct TRIANGLE *dwtri;
	  t=1.;
	  x=i+1.; y=j+1.; z=k+1.; //interp is fortran
	  xp1=(x+1.); yp1=(y+1.); zp1=(z+1.);
	  vert[0]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&yp1,&z,&t,&special);
	  vert[1]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&yp1,&z,&t,&special);
	  vert[2]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&y,&z,&t,&special);
	  vert[3]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&y,&z,&t,&special);
	  vert[4]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&yp1,&zp1,&t,&special);
	  vert[5]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&yp1,&zp1,&t,&special);
	  vert[6]=(float)interp_(volume,&ni,&nj,&nk,&one,&xp1,&y,&zp1,&t,&special);
	  vert[7]=(float)interp_(volume,&ni,&nj,&nk,&one,&x,&y,&zp1,&t,&special);
	  points[0].x=x; points[0].y=yp1; points[0].z=z;
	  points[1].x=xp1; points[1].y=yp1;points[1].z=z;
	  points[2].x=xp1; points[2].y=y;	points[2].z=z;
	  points[3].x=x;	points[3].y=y;	points[3].z=z;
	  points[4].x=x;	points[4].y=yp1; points[4].z=zp1;
	  points[5].x=xp1; points[5].y=yp1; points[5].z=zp1;
	  points[6].x=xp1; points[6].y=y; points[6].z=zp1;
	  points[7].x=x;	points[7].y=y; points[7].z=zp1;
	
	  smalltri.num_triangles=0;
	  if(vert[0]==special||vert[1]==special||vert[2]==special||
	     vert[3]==special||vert[4]==special||vert[5]==special||
	     vert[7]==special||vert[7]==special)
	    continue;
	  ive_get_points(vert,sint,points,&smalltri);
	  if(smalltri.num_triangles==0)continue;
	  //ive_get_points(0,0,0,vert,sint,&smalltri);
	  //ive_get_surface(i,j,k,vert,sint,&smalltri);
	  if(triangles.num_triangles + smalltri.num_triangles > 
	     allocated_triangles-1){
	    struct TRIANGLE *dwtri;
	    void *ptr;
	    allocated_triangles += 2*ni*nj;
	    //printf("realloc triangles %lx %d\n",triangles.tri, allocated_triangles);
	    dwtri = (struct TRIANGLE *) realloc(triangles.tri, 
						(allocated_triangles*sizeof(struct TRIANGLE)));
	    if(dwtri !=(struct TRIANGLE *)NULL)triangles.tri=dwtri;
	    else {printf("out of memory for triangles\n");break;}
	    
	  }
	  imap=2;
	  for (l=0; l< smalltri.num_triangles; l++){
	    npts=1;
	  if(check_out_range(smalltri.tri[l],ni-1,nj-1,nk-1))
	    {
	      continue;
	    }
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p1.x,&smalltri.tri[l].p1.y,&smalltri.tri[l].p1.z,
			 &triangles.tri[triangles.num_triangles].p1.x,
			 &triangles.tri[triangles.num_triangles].p1.z,
			 &triangles.tri[triangles.num_triangles].p1.y,&special);
	  if(triangles.tri[triangles.num_triangles].p1.x==special){
	    continue;
	  }
	  (void)scale_(&triangles.tri[triangles.num_triangles].p1.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p1.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles].p1.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  //triangles.num_triangles++;  
	  triangles.tri[triangles.num_triangles].p2.x = triangles.tri[triangles.num_triangles].p1.x;
	  triangles.tri[triangles.num_triangles].p2.y = triangles.tri[triangles.num_triangles].p1.y;
	  triangles.tri[triangles.num_triangles].p2.z = triangles.tri[triangles.num_triangles].p1.z;
	  triangles.tri[triangles.num_triangles].p3.x = triangles.tri[triangles.num_triangles].p1.x;
	  triangles.tri[triangles.num_triangles].p3.y = triangles.tri[triangles.num_triangles].p1.y;
	  triangles.tri[triangles.num_triangles].p3.z = triangles.tri[triangles.num_triangles].p1.z;
	  
	  triangles.num_triangles++;
	  }
	}
      }
    }
    free(smalltri.tri);
    if(triangles.num_triangles == 0){
      printf("No points found at %s@%f\n",field,sint);
      if(triangles.tri)free(triangles.tri);
      if(nObjects >0){
	(void)plot3d(mins, maxs, &IVE_Object);
      }
      if(volume != NULL)free(volume);
      return;
    }
    allocated_norms=triangles.num_triangles;
    triangles.normals=(struct plainpoint *)malloc(allocated_norms*sizeof(struct plainpoint));
    triangles.norm_points=(struct plainpoint *)malloc(allocated_norms*sizeof(struct plainpoint));
    triangles.norm_counts=(int *)malloc(allocated_norms*sizeof(int));
    for(i=0; i<triangles.num_triangles; i++){
      struct plainpoint mynormal;
      compute_normal(&mynormal, 
		     triangles.tri[i].p1, 
		     triangles.tri[i].p2,
		     triangles.tri[i].p3);
      //    if(!(i%10000))printf("check %d\n",i);
      triangles.norm_counts[triangles.num_normals]=1;
      triangles.normals[triangles.num_normals].x = mynormal.x;
      triangles.normals[triangles.num_normals].y = mynormal.y;
      triangles.normals[triangles.num_normals].z = mynormal.z;
      triangles.norm_points[triangles.num_normals].x=triangles.tri[i].p3.x;
      triangles.norm_points[triangles.num_normals].y=triangles.tri[i].p3.y;
      triangles.norm_points[triangles.num_normals].z=triangles.tri[i].p3.z;
      triangles.tri[i].normal1=triangles.num_normals;
      triangles.tri[i].normal2=triangles.num_normals;
      triangles.tri[i].normal3=triangles.num_normals;
      triangles.num_normals++;
 
    }
  }

  if(nObjects++){
    void *ptr;
    (void)reset_all_3d();
    IVE_Object.objects = nObjects;
    ptr = realloc( IVE_Object.Surface, nObjects*sizeof(Surface));
    IVE_Object.Surface = ptr;
    ptr = realloc(IVE_Object.NormalList,nObjects*sizeof(NormalList));
    IVE_Object.NormalList = ptr;
    IVE_Object.Field[nObjects-1] = malloc(strlen(field)+100*sizeof(char));
    IVE_Object.objectDone[nObjects-1]=0;
  }
  else{ //first object
    (void)reset_all_3d();
    IVE_Object.objects = nObjects;
    IVE_Object.Surface = malloc(nObjects*sizeof(Surface));
    IVE_Object.NormalList = malloc(nObjects*sizeof(NormalList));
    IVE_Object.Field[0] = malloc(strlen(field)+100*sizeof(char));
    IVE_Object.objectDone[nObjects-1]=0;
  }
  switch(pltyp[0]){
  case 'I':
    //printf("call plot3d\n");
    IVE_Object.objectPType[nObjects-1]=0;
    break;
  case 'S':
    IVE_Object.objectPType[nObjects-1]=1;
    //printf("points\n");
    break;
  case 'W':
    IVE_Object.objectPType[nObjects-1]=2;
    //printf("WireFrame\n");
    break;
  }

  sprintf(IVE_Object.Field[nObjects-1],"%s@%.0f",field,sint);
  //printf("%s@%.0f\n",field,sint);
  //strcpy(IVE_Object.Field[nObjects-1], field);
  OBS.size = triangles.num_triangles;
  OBS.items = malloc(OBS.size*sizeof(Triangle));
  OBN.size = triangles.num_normals;//change this line when we hop over to point normals instead of triangle normals.
  //printf("first total norms %d\n",OBN.size);
  OBN.normal = malloc(OBN.size*sizeof(Point));
#ifdef OMP
#pragma omp parallel for default(shared)
#endif
  //printf("d total norms %d triangles %d\n",triangles.num_normals,triangles.num_triangles);
  for(i=0; i<triangles.num_triangles; i++){
    OBS.items[i].pt[0].xCoord = triangles.tri[i].p1.x;
    OBS.items[i].pt[0].yCoord = triangles.tri[i].p1.y;
    OBS.items[i].pt[0].zCoord = triangles.tri[i].p1.z;
    OBS.items[i].pt[0].normalRef = triangles.tri[i].normal1;
    OBS.items[i].pt[1].xCoord = triangles.tri[i].p2.x;
    OBS.items[i].pt[1].yCoord = triangles.tri[i].p2.y;
    OBS.items[i].pt[1].zCoord = triangles.tri[i].p2.z;
    OBS.items[i].pt[1].normalRef = triangles.tri[i].normal2;
    OBS.items[i].pt[2].xCoord = triangles.tri[i].p3.x;
    OBS.items[i].pt[2].yCoord = triangles.tri[i].p3.y;
    OBS.items[i].pt[2].zCoord = triangles.tri[i].p3.z;
    OBS.items[i].pt[2].normalRef = triangles.tri[i].normal3;
    //    printf("(%f,%f,%f) (%f,%f,%f) (%f,%f,%f)\n",
    //	   OBS.items[i].pt[0].xCoord,OBS.items[i].pt[0].yCoord,OBS.items[i].pt[0].zCoord,
    //	   OBS.items[i].pt[1].xCoord,OBS.items[1].pt[0].yCoord,OBS.items[i].pt[1].zCoord,
    //	   OBS.items[i].pt[2].xCoord,OBS.items[i].pt[2].yCoord,OBS.items[i].pt[2].zCoord);
  }
  //printf("total norms %d\n",OBN.size);
#ifdef OMP
#pragma omp parallel for default(shared)
#endif
  for(i=0; i<triangles.num_normals; i++){
    OBN.normal[i].xCoord = triangles.normals[i].x;
    OBN.normal[i].yCoord = triangles.normals[i].y;
    OBN.normal[i].zCoord = triangles.normals[i].z;
  }
  free(triangles.normals);
  free(triangles.norm_counts);
  free(triangles.tri);
  if(linlog){
    //[x,z,y]
    mins[1]=log10f(mins[1]);
    maxs[1]=log10f(maxs[1]);
  }
  (void)plot3d(mins, maxs, &IVE_Object);
  if(volume != NULL)free(volume);
}

void
updateViewport(Widget w, XtPointer clientData, XtPointer callData)
{
  Dimension width, height;
  
  XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
  glViewport(0, 0, (GLint) width, (GLint) height);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glCallList(curdisplist);
  glXSwapBuffers(XtDisplay(xgks_widget),IveGlxWindow);
  glFlush();    
}
 



