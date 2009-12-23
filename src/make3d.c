#include <volume.h>
#include <ive_gl.h>
#include <math.h>
#define MAXDIM 4


extern void getavar_(),getivar_(),make_help_widget_(),getiarr_(),getlvar_(),
  getrarr_(),getrvar_();
extern float * slicer3d_();
extern Widget xgks_widget;
extern double interp_();
#define SDIV 1
#define SDIVF 1.000

Objects IVE_Object;
#define OBS IVE_Object.Surface[nObjects-1]
#define OBN IVE_Object.NormalList[nObjects-1]

void reset_nobjects()
{
  int i;
  if(nObjects > 0){
    if(IVE_Object.NormalList)
      free(IVE_Object.NormalList);
    if(IVE_Object.Surface)
      free(IVE_Object.Surface);
  }
  IVE_Object.objects = 0 ;
  nObjects=0;
  for(i=0; i<10; i++){
    IVE_Object.objectOn[i] = 1;
    IVE_Object.listName[i] = 0;
}
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
  if((p1.x==p2.x)&&(p1.y==p2.y)&&(p1.z==p2.z))
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
  float pt[4], mins[4], maxs[4], stag[4], vert[8],delta[4],
    *volume, *topohghts, dims, special, sint, mindelta, mymins[3], mymaxs[3];
  double domain_intercept[4], domain_slope[4];
  unsigned long pixel;  
  extern unsigned long IveGetPixel();
  int fix[4], ni, nj, nk, nteri, nterk, i, j, k, l, m, imap,
    saveflag, buttons, error, phys, fd, lval, width, height;
  int count = 0, allocated_norms=100;
  struct{float x,y,z;}bpt[8];
  struct TRIANGLES triangles,smalltri;
  FILE *file;
  //  extern void ive_draw_surf(),  ive_draw_box(), ive_draw_points(),
  //ive_draw_wire(), 
  extern void ive_get_surface();

  /*Start the 3D thing*/
  //  (void)setup_3D();

  /* get  the type of plot */
  (void)getavar_("plotyp3",pltyp,&error,7,16);

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
    i=4;
    (void)getrarr_("plwmin_scaled",mins, &i, &error, 13);
    (void)getrarr_("plwmax_scaled",maxs, &i, &error, 13);
    (void)getdarr_("domain_slope", domain_slope, &i, &error,12);
    (void)getdarr_("domain_intercept", domain_intercept, &i, &error,16);
    (void)getrarr_("grid_delta", delta, &i, &error,10);
    (void)reset_nobjects();
  }
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
  printf("sint: %f\n",sint);
  phys=0;
  for(lval=0; lval<4; lval++){
    if(fix[lval])break;
  }
  pt[0] = pt[1] = pt[2] = pt[3] = 0.0;
  lval++;
  switch(lval){
  case 1: (void)getrvar_("xloc", pt, &error, 4);
    break;
  case 2: (void)getrvar_("yloc", pt+1, &error, 4);
    break;
  case 3: (void)getrvar_("xloc", pt+2, &error, 4);
    break;
  case 4: (void)getrvar_("tloc", pt+3, &error, 4);
    break;
  }
  volume = (float *)slicer3d_(varpt, x, y, z, t,
		    &dims, &lval, pt, &nk, &nj, &ni, &special, &phys);



  smalltri.tri = (struct TRIANGLE *)malloc(10*sizeof(struct TRIANGLE));
  triangles.tri = (struct TRIANGLE *)NULL; //malloc(sizeof(struct TRIANGLE));
  triangles.norm_counts=(int *)malloc(allocated_norms*sizeof(int));
  triangles.normals=(struct plainpoint *)malloc(allocated_norms*sizeof(struct plainpoint));
  triangles.norm_points=(struct plainpoint *)malloc(allocated_norms*sizeof(struct plainpoint));
  smalltri.num_triangles=0;
  triangles.num_triangles=0;
  triangles.num_normals=0;
  smalltri.num_normals=0;
   
  for(i=0; i<SDIV*ni-1; i++){
    for(j=0; j<SDIV*nj-1; j++){
      for(k=0; k<SDIV*nk-1; k++){
	int one=1, subtri, myl;
	float x,y,z,xp1,yp1,zp1,t;
	struct plainpoint points[8],mynormal;
	t=1.;
	x=i/SDIVF; y=j/SDIVF; z=k/SDIVF;
	xp1=(i+1.)/SDIVF; yp1=(j+1.)/SDIVF; zp1=(k+1.)/SDIVF;
	if(i==SDIV*ni)xp1=(i)/SDIVF;
	if(j==SDIV*ni)yp1=(j)/SDIVF;
	if(k==SDIV*ni)zp1=(k)/SDIVF;
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
	//sive_get_surface(i,j,k,vert,sint,&smalltri);
	//	printf("got: %d triangles\n",smalltri.num_triangles);
	triangles.tri = (struct TRIANGLE *)
	  realloc(triangles.tri, 
		  (triangles.num_triangles +smalltri.num_triangles)*sizeof(struct TRIANGLE));
	imap=2;
	subtri=0;
	myl=-1;
	for (l=0; l< smalltri.num_triangles; l++){
	  int npts=1,p1done,p2done,p3done;
	  p1done=0;
	  p2done=0;
	  p3done=0;
	  if(equal_points(smalltri.tri[l].p1,smalltri.tri[l].p2,mindelta) || 
	     equal_points(smalltri.tri[l].p1,smalltri.tri[l].p3,mindelta) || 
	     equal_points(smalltri.tri[l].p3,smalltri.tri[l].p2,mindelta))
	    {
	      subtri++;
	      continue;
	    }
	  if(check_out_range(smalltri.tri[l],ni-1,nj-1,nk-1))
	    {
	      subtri++;
	      continue;
	    }
	  myl++;
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p1.x,&smalltri.tri[l].p1.y,&smalltri.tri[l].p1.z,
			 &triangles.tri[triangles.num_triangles+myl].p1.x,
			 &triangles.tri[triangles.num_triangles+myl].p1.z,
			 &triangles.tri[triangles.num_triangles+myl].p1.y,&special);
	  if(triangles.tri[triangles.num_triangles+myl].p1.x==special){
	    subtri++;
	    myl--;
	    continue;
	    }
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p1.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p1.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p1.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	 
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p2.x,&smalltri.tri[l].p2.y,&smalltri.tri[l].p2.z,
			 &triangles.tri[triangles.num_triangles+myl].p2.x,
			 &triangles.tri[triangles.num_triangles+myl].p2.z,
			 &triangles.tri[triangles.num_triangles+myl].p2.y,&special);
	  if(triangles.tri[triangles.num_triangles+myl].p1.x==special){
	    subtri++;
	    myl--;
	    continue;
	    }
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p2.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p2.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p2.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p3.x,&smalltri.tri[l].p3.y,&smalltri.tri[l].p3.z,
			 &triangles.tri[triangles.num_triangles+myl].p3.x,
			 &triangles.tri[triangles.num_triangles+myl].p3.z,
			 &triangles.tri[triangles.num_triangles+myl].p3.y,&special);
	  if(triangles.tri[triangles.num_triangles+myl].p1.x==special){
	    subtri++;
	    myl--;
	    continue;
	    }
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p3.x,&npts,
		       domain_slope,domain_intercept,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p3.y,&npts,
		       domain_slope+2,domain_intercept+2,&special);
	  (void)scale_(&triangles.tri[triangles.num_triangles+myl].p3.z,&npts,
		       domain_slope+1,domain_intercept+1,&special);
	  
	  compute_normal(&mynormal, 
			 triangles.tri[triangles.num_triangles+myl].p1, 
			 triangles.tri[triangles.num_triangles+myl].p2,
			 triangles.tri[triangles.num_triangles+myl].p3);
	  
	  for (m=0; m<triangles.num_normals; m++){
	    if(equal_points(triangles.tri[triangles.num_triangles+myl].p1,triangles.norm_points[m],mindelta))
	      {
		triangles.normals[m].x += mynormal.x;
		triangles.normals[m].y += mynormal.y;
		triangles.normals[m].z += mynormal.z;
		triangles.norm_counts[m]+=2;
		triangles.tri[triangles.num_triangles+myl].normal1=m;
		p1done=1;
	      }
	    if(equal_points(triangles.tri[triangles.num_triangles+myl].p2,triangles.norm_points[m],mindelta))
	      {
		triangles.normals[m].x += mynormal.x;
		triangles.normals[m].y += mynormal.y;
		triangles.normals[m].z += mynormal.z;
		triangles.norm_counts[m]+=2;
		triangles.tri[triangles.num_triangles+myl].normal2=m;
		p2done=1;
	      }
	    if(equal_points(triangles.tri[triangles.num_triangles+myl].p3,triangles.norm_points[m],mindelta))
	      {
		triangles.normals[m].x += mynormal.x;
		triangles.normals[m].y += mynormal.y;
		triangles.normals[m].z += mynormal.z;
		triangles.norm_counts[m]+=2;
		triangles.tri[triangles.num_triangles+myl].normal3=m;
		p3done=1;
	      }
	  } 
	    
	  if(p1done && p2done && p3done)continue;
	  if(triangles.num_normals+(3-p1done-p2done-p3done)> allocated_norms-1){
	    allocated_norms += 100;
	    triangles.norm_counts = (int *)realloc(triangles.norm_counts,allocated_norms*sizeof(int));
	    triangles.norm_points =(struct plainpoint *)realloc(triangles.norm_points,
							   allocated_norms*sizeof(struct plainpoint));
	    triangles.normals = (struct plainpoint *)realloc(triangles.normals,
							allocated_norms*sizeof(struct plainpoint));
	  }
	  if(!p1done){
	    triangles.norm_counts[triangles.num_normals]=2;
	    triangles.normals[triangles.num_normals].x = mynormal.x;
	    triangles.normals[triangles.num_normals].y = mynormal.y;
	    triangles.normals[triangles.num_normals].z = mynormal.z;
	    triangles.norm_points[triangles.num_normals].x=triangles.tri[triangles.num_triangles+l].p1.x;
	    triangles.norm_points[triangles.num_normals].y=triangles.tri[triangles.num_triangles+l].p1.y;
	    triangles.norm_points[triangles.num_normals].z=triangles.tri[triangles.num_triangles+l].p1.z;
	    triangles.tri[triangles.num_triangles+myl].normal1=triangles.num_normals;
	    triangles.num_normals++;
	  }
	  if(!p2done){
	    triangles.norm_counts[triangles.num_normals]=2;
	    triangles.normals[triangles.num_normals].x = mynormal.x;
	    triangles.normals[triangles.num_normals].y = mynormal.y;
	    triangles.normals[triangles.num_normals].z = mynormal.z;
	    triangles.norm_points[triangles.num_normals].x=triangles.tri[triangles.num_triangles+l].p2.x;
	    triangles.norm_points[triangles.num_normals].y=triangles.tri[triangles.num_triangles+l].p2.y;
	    triangles.norm_points[triangles.num_normals].z=triangles.tri[triangles.num_triangles+l].p2.z;
	    triangles.tri[triangles.num_triangles+myl].normal2=triangles.num_normals;
	    triangles.num_normals++;
	  }
	  if(!p3done){
	    triangles.norm_counts[triangles.num_normals]=2;
	    triangles.normals[triangles.num_normals].x = mynormal.x;
	    triangles.normals[triangles.num_normals].y = mynormal.y;
	    triangles.normals[triangles.num_normals].z = mynormal.z;
	    triangles.norm_points[triangles.num_normals].x=triangles.tri[triangles.num_triangles+l].p3.x;
	    triangles.norm_points[triangles.num_normals].y=triangles.tri[triangles.num_triangles+l].p3.y;
	    triangles.norm_points[triangles.num_normals].z=triangles.tri[triangles.num_triangles+l].p3.z;
	    triangles.tri[triangles.num_triangles+myl].normal3=triangles.num_normals;
	    triangles.num_normals++;
	  }
	}
	triangles.num_triangles += smalltri.num_triangles - subtri;
      }
    }
  }
  for (i=0; i<triangles.num_normals; i++){
    triangles.normals[i].x= -1.*(triangles.normals[i].x/(float)triangles.norm_counts[i]);
    triangles.normals[i].y= -1.*(triangles.normals[i].y/(float)triangles.norm_counts[i]);
    triangles.normals[i].z= -1.*(triangles.normals[i].z/(float)triangles.norm_counts[i]);
  }

 
  
  /*now make it look right for Brian's code */
  

  if(nObjects++){
    void *ptr;
    IVE_Object.objects = nObjects;
    ptr = realloc(nObjects, sizeof(Surface));
    if(ptr)
      IVE_Object.Surface = ptr;
    ptr = realloc(nObjects, sizeof(Surface));
    if(ptr)
      IVE_Object.NormalList =  ptr;
  }
  else{ //first object
    IVE_Object.objects = nObjects;
    IVE_Object.Surface = calloc(nObjects, sizeof(Surface));
    IVE_Object.NormalList = calloc(nObjects, sizeof(Surface));
  }

  OBS.size = triangles.num_triangles;
  OBS.items = calloc(OBS.size, sizeof(Triangle));
  OBN.size = triangles.num_triangles;//change this line when we hop over to point normals instead of triangle normals.
  OBN.normal = calloc(OBN.size, sizeof(Point));
  
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
  }
  for(i=0; i<triangles.num_normals; i++){
    OBN.normal[i].xCoord = triangles.normals[i].x;
    OBN.normal[i].yCoord = triangles.normals[i].y;
    OBN.normal[i].zCoord = triangles.normals[i].z;
  }

  file = fopen("points","w+");
  fprintf(file,"triangles: %d\n", triangles.num_triangles);
  fprintf(file,"normals: %d\n",triangles.num_normals);
  fprintf(file,"min:{%f, %f, %f}\n",
    mins[0],mins[2],mins[1]);
  //	  mins[slab_3.xaxis-1],mins[slab_3.yaxis-1],mins[slab_3.zaxis-1]);
  fprintf(file,"max:{%f, %f, %f}\n",
  	  maxs[0],maxs[2],maxs[1]);
  //maxs[slab_3.xaxis-1],maxs[slab_3.yaxis-1],maxs[slab_3.zaxis-1]);
  for(i=0; i<triangles.num_triangles; i++){
    fprintf(file,"{%f, %f, %f}[%d] {%f, %f, %f}[%d] {%f, %f, %f}[%d]\n",
	    triangles.tri[i].p1.x,triangles.tri[i].p1.y,triangles.tri[i].p1.z,triangles.tri[i].normal1,
	    triangles.tri[i].p2.x,triangles.tri[i].p2.y,triangles.tri[i].p2.z,triangles.tri[i].normal2,
	    triangles.tri[i].p3.x,triangles.tri[i].p3.y,triangles.tri[i].p3.z,triangles.tri[i].normal3);
  }
  for(i=0; i<triangles.num_normals; i++){
    fprintf(file,"{%f, %f, %f}\n",triangles.normals[i].x,triangles.normals[i].y,triangles.normals[i].z);
  }
  fclose(file);
  
  switch(pltyp[0]){
  case 'I':
    printf("hey\n");
    (void)plot3d(mins, maxs, &IVE_Object);
    break;
  case 'S':
    printf("points\n");
    break;
  case 'B':
    printf("box\n");
    break;
  case 'W':
    printf("WireFrame\n");
    break;
  }
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
 



