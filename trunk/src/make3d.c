#include <volume.h>
#include <ive_gl.h>
#define MAXDIM 4

extern void getavar_(),getivar_(),make_help_widget_(),getiarr_(),getlvar_(),
  getrarr_(),getrvar_();
extern float * slicer3d_();
extern Widget xgks_widget;


void make3d_(varpt, x, y, z, t)
     float  *varpt;
     int    *x, *y, *z, *t;
{
  const float zero = 0.0;
  void (*ive_3d_pltfunc)();
  char pltyp[16];
  float pt[4], mins[4], maxs[4], stag[4], vert[8],
    *volume, *topohghts, dims, special, sint;
  unsigned long pixel;
  extern unsigned long IveGetPixel();
  int fix[4], ni, nj, nk, nteri, nterk, i, j, k, l, m, imap,
    saveflag, buttons, error, phys, fd, lval, width, height;
  int count = 0;
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
    getrarr_("plwmin",mins, &i, &error, 6);
    getrarr_("plwmax",maxs, &i, &error, 6);
  }
  i=4;
  getrarr_("stag",stag, &i, &error, 4);

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
  triangles.tri = (struct TRIANGLE *)malloc(sizeof(struct TRIANGLE));
  smalltri.num_triangles=0;
  triangles.num_triangles=0;
  if(! triangles.tri){
    perror("can't malloc memory\n");
    return;
  }
  for(i=0; i<ni-1; i++){
    for(j=0; j<nj-1; j++){
      for(k=0; k<nk-1; k++){
	/*
	vert[0]=volume_dep(i,j,k);
	vert[1]=volume_dep(i+1,j,k);
	vert[2]=volume_dep(i+1,j+1,k);
	vert[3]=volume_dep(i,j+1,k);
	vert[4]=volume_dep(i,j,k+1);
	vert[5]=volume_dep(i+1,j,k+1);
	vert[6]=volume_dep(i+1,j+1,k+1);
	vert[7]=volume_dep(i,j+1,k+1);
	*/
	vert[0]=volume_dep(i,j,k+1);
	vert[1]=volume_dep(i+1,j,k+1);
	vert[2]=volume_dep(i+1,j,k);
	vert[3]=volume_dep(i,j,k);
	vert[4]=volume_dep(i,j+1,k+1);
	vert[5]=volume_dep(i+1,j+1,k+1);
	vert[6]=volume_dep(i+1,j+1,k);
	vert[7]=volume_dep(i,j+1,k);
	smalltri.num_triangles=0;
	ive_get_surface(i,j,k,vert,sint,&smalltri);
	triangles.tri = (struct TRIANGLE *)
	  realloc(triangles.tri, 
		  (triangles.num_triangles +smalltri.num_triangles)*sizeof(struct TRIANGLE));
	imap=2;
	for (l=0; l< smalltri.num_triangles; l++){
	  /*
	  triangles.tri[triangles.num_triangles+l].p1.x = smalltri.tri[l].p1.x;
	  triangles.tri[triangles.num_triangles+l].p1.z = smalltri.tri[l].p1.y;
	  triangles.tri[triangles.num_triangles+l].p1.y = smalltri.tri[l].p1.z;
	  triangles.tri[triangles.num_triangles+l].p2.x = smalltri.tri[l].p2.x;
	  triangles.tri[triangles.num_triangles+l].p2.z = smalltri.tri[l].p2.y;
	  triangles.tri[triangles.num_triangles+l].p2.y = smalltri.tri[l].p2.z;
	  triangles.tri[triangles.num_triangles+l].p3.x = smalltri.tri[l].p3.x;
	  triangles.tri[triangles.num_triangles+l].p3.z = smalltri.tri[l].p3.y;
	  triangles.tri[triangles.num_triangles+l].p3.y = smalltri.tri[l].p3.z;
	  */
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p1.x,&smalltri.tri[l].p1.y,&smalltri.tri[l].p1.z,
			 &triangles.tri[triangles.num_triangles+l].p1.x,
			 &triangles.tri[triangles.num_triangles+l].p1.z,
			 &triangles.tri[triangles.num_triangles+l].p1.y);
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p2.x,&smalltri.tri[l].p2.y,&smalltri.tri[l].p2.z,
			 &triangles.tri[triangles.num_triangles+l].p2.x,
			 &triangles.tri[triangles.num_triangles+l].p2.z,
			 &triangles.tri[triangles.num_triangles+l].p2.y);
	  (void)cpmpxyz_(&imap,&smalltri.tri[l].p3.x,&smalltri.tri[l].p3.y,&smalltri.tri[l].p3.z,
			 &triangles.tri[triangles.num_triangles+l].p3.x,
			 &triangles.tri[triangles.num_triangles+l].p3.z,
			 &triangles.tri[triangles.num_triangles+l].p3.y);
	}
	triangles.num_triangles += smalltri.num_triangles;
      }
    }
  }
  /*
  if(triangles.num_triangles>0){
    mins[0]=200000;maxs[0]= 0;
    mins[1]=200000;maxs[1]= 0;
    mins[2]=200000;maxs[2]= 0;
  }
  for(i=0; i<triangles.num_triangles; i++){
    if(triangles.tri[i].p1.x <mins[0])mins[0]= triangles.tri[i].p1.x;
    if(triangles.tri[i].p2.x <mins[0])mins[0]= triangles.tri[i].p2.x;
    if(triangles.tri[i].p3.x <mins[0])mins[0]= triangles.tri[i].p3.x;

    if(triangles.tri[i].p1.y <mins[1])mins[1]= triangles.tri[i].p1.y;
    if(triangles.tri[i].p2.y <mins[1])mins[1]= triangles.tri[i].p2.y;
    if(triangles.tri[i].p3.y <mins[1])mins[1]= triangles.tri[i].p3.y;

    if(triangles.tri[i].p1.z <mins[2])mins[2]= triangles.tri[i].p1.z;
    if(triangles.tri[i].p2.z <mins[2])mins[2]= triangles.tri[i].p2.z;
    if(triangles.tri[i].p3.z <mins[2])mins[2]= triangles.tri[i].p3.z;

    if(triangles.tri[i].p1.x >maxs[0])maxs[0]= triangles.tri[i].p1.x;
    if(triangles.tri[i].p2.x >maxs[0])maxs[0]= triangles.tri[i].p2.x;
    if(triangles.tri[i].p3.x >maxs[0])maxs[0]= triangles.tri[i].p3.x;

    if(triangles.tri[i].p1.y >maxs[1])maxs[1]= triangles.tri[i].p1.y;
    if(triangles.tri[i].p2.y >maxs[1])maxs[1]= triangles.tri[i].p2.y;
    if(triangles.tri[i].p3.y >maxs[1])maxs[1]= triangles.tri[i].p3.y;

    if(triangles.tri[i].p1.z >maxs[2])maxs[2]= triangles.tri[i].p1.z;
    if(triangles.tri[i].p2.z >maxs[2])maxs[2]= triangles.tri[i].p2.z;
    if(triangles.tri[i].p3.z >maxs[2])maxs[2]= triangles.tri[i].p3.z;
  }
  */

  file = fopen("points","w+");
  fprintf(file,"triangles: %d\n", triangles.num_triangles);
  fprintf(file,"min:{%f, %f, %f}\n",
    mins[0],mins[2],mins[1]);
  //	  mins[slab_3.xaxis-1],mins[slab_3.yaxis-1],mins[slab_3.zaxis-1]);
  fprintf(file,"max:{%f, %f, %f}\n",
  	  maxs[0],maxs[2],maxs[1]);
  //maxs[slab_3.xaxis-1],maxs[slab_3.yaxis-1],maxs[slab_3.zaxis-1]);
  for(i=0; i<triangles.num_triangles; i++){
    fprintf(file,"{%f, %f, %f} {%f, %f, %f} {%f, %f, %f}\n",
	    triangles.tri[i].p1.x,triangles.tri[i].p1.y,triangles.tri[i].p1.z,
	    triangles.tri[i].p2.x,triangles.tri[i].p2.y,triangles.tri[i].p2.z,
	    triangles.tri[i].p3.x,triangles.tri[i].p3.y,triangles.tri[i].p3.z);
  }
  fclose(file);
  switch(pltyp[0]){
  case 'I':
    //ive_3d_pltfunc=ive_draw_surf;
    printf("surf\n");
    break;
  case 'S':
    //ive_3d_pltfunc=ive_draw_points;
    printf("points\n");
    break;
  case 'B':
    //ive_3d_pltfunc=ive_draw_box;
    printf("box\n");
    break;
  case 'W':
    //ive_3d_pltfunc=ive_draw_wire;
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
 



