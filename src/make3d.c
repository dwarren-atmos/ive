#include <volume.h>
#define MAXDIM 4

static float  boxx[2], boxy[2], boxz[2], lenx, leny, lenz;

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
  int fix[4], ni, nj, nk, nteri, nterk, i, j, k,
    saveflag, buttons, error, phys, fd, lval, width, height;
  struct{float x,y,z;}bpt[8];
  extern void ive_draw_surf(),  ive_draw_box(), ive_draw_points(),
    ive_draw_wire();

  /*Start the 3D thing*/
  (void)setup_3D();

  /* get  the type of plot */
  (void)getavar_("plotyp3",pltyp,&error,7,16);
  switch(pltyp[0]){
  case 'I':
    ive_3d_pltfunc=ive_draw_surf;
    printf("surf\n");
    break;
  case 'S':
    ive_3d_pltfunc=ive_draw_points;
    printf("points\n");
    break;
  case 'B':
    ive_3d_pltfunc=ive_draw_box;
    printf("box\n");
    break;
  case 'W':
    ive_3d_pltfunc=ive_draw_wire;
    printf("WireFrame\n");
    break;
  }

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
  /*  topohghts=(float *)malloc(ni*nj*sizeof(float));
  (void)horiz_ter_trans_(topohghts, &ni,&nj,stag, stag+1,&zero,&error);
  for(j=0;j<nj;j++){
    for(i=0;i<ni;i++)
      printf("%.2f ",*(topohghts+i+(j*i)));
    printf("\n");
    }*/
  XtVaGetValues(gl_widget,
		XmNwidth, &width,
		XmNheight, &height,
		NULL);
  glViewport(0,width,0,height);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL); 
  glClearDepth(1.0);
  pixel=IveGetPixel(0);
  /*  glClearIndex((float)pixel);*/  
  glClearColor(1.0,1.0,1.0,1.0);
  if(!saveflag){
    /*    gclearws(WS_X, 1);*/
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
    glFlush();
  }

  boxx[1] = maxs[slab_3.xaxis-1];
  boxx[0] = mins[slab_3.xaxis-1];
  boxy[1] = maxs[slab_3.yaxis-1];
  boxy[0] = mins[slab_3.xaxis-1];
  boxz[1] = maxs[slab_3.zaxis-1];
  boxz[0] = mins[slab_3.zaxis-1];
  lenx = .2*(boxx[1] - boxx[0]);
  leny = .2*(boxy[1] - boxy[0]);
  lenz = .2*(boxz[1] - boxz[0]);
  bpt[0].x = boxx[0];
  bpt[0].y = boxy[0];
  bpt[0].z = boxz[0];
  bpt[1].x = boxx[1];
  bpt[1].y = boxy[0];
  bpt[1].z = boxz[0];
  bpt[2].x = boxx[1];
  bpt[2].y = boxy[0];
  bpt[2].z = boxz[1];
  bpt[3].x = boxx[0];
  bpt[3].y = boxy[0];
  bpt[3].z = boxz[1];
  bpt[4].x = boxx[0];
  bpt[4].y = boxy[1];
  bpt[4].z = boxz[0];
  bpt[5].x = boxx[1];
  bpt[5].y = boxy[1];
  bpt[5].z = boxz[0];
  bpt[6].x = boxx[1];
  bpt[6].y = boxy[1];
  bpt[6].z = boxz[1];
  bpt[7].x = boxx[0];
  bpt[7].y = boxy[1];
  bpt[7].z = boxz[1];

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-.8,.8,-.8,.8,-.8,.8);
  glTranslatef(-.5,-.5,-.5);
  curdisplist++;
  curdisplist %= 10;
  glNewList(curdisplist, GL_COMPILE);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glOrtho(-1.,1.,-1.,1.,-1.,1.);
  /*  glOrtho(boxx[0]/boxx[1],1.,boxy[0]/boxy[1],1.,boxz[0]/boxz[1],1.);*/
  /*  glOrtho(boxx[0]-lenx,boxx[1]+lenx,boxy[0]-leny,boxy[1]+leny,
      boxz[0]-lenz,boxz[1]+lenz);*/
  glScalef(1./boxx[1],1./boxy[1],1./boxz[1]);
  glTranslatef(-(boxx[0]/boxx[1]),-(boxy[0]/boxy[1]),
	       -(boxz[0]/boxz[1]));
  glEnable(GL_BLEND);
  {
  /*  gluPerspective(120.0, 2., 5*lenz, 20*lenz);*/
    static GLfloat light0_pos[] = { 0.0, 0.0, 10.0, 0.0 };
    static GLfloat light1_pos[] = { 0.0, 0.0, -10.0, 0.0 };
    static GLfloat light_ambient[] = { 0.15, 0.15, 0.15, 1.0 };
    static GLfloat light_diffuse[] = { 0.6, 0.6, 0.6, 1.0 };
    static GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };
    static GLfloat model_ambient[] = { 0.0, 0.0, 0.0, 1.0 };
    static GLfloat mat_specular[] = { 0.5, 0.5, 0.5, 1.0 };
    light0_pos[2] *= (float)nk;
    light1_pos[2] *= (float)nk;
    glLightfv( GL_LIGHT0, GL_AMBIENT, light_ambient );
    glLightfv( GL_LIGHT0, GL_DIFFUSE, light_diffuse );
    glLightfv( GL_LIGHT0, GL_SPECULAR, light_specular );
    glLightfv( GL_LIGHT0, GL_POSITION, light0_pos );
    glLightfv( GL_LIGHT1, GL_AMBIENT, light_ambient );
    glLightfv( GL_LIGHT1, GL_DIFFUSE, light_diffuse );
    glLightfv( GL_LIGHT1, GL_SPECULAR, light_specular );
    glLightfv( GL_LIGHT1, GL_POSITION, light1_pos );
    glLightModelfv( GL_LIGHT_MODEL_AMBIENT, model_ambient );
    glLightModeli( GL_LIGHT_MODEL_TWO_SIDE, 0 );
    glLightModeli( GL_LIGHT_MODEL_LOCAL_VIEWER, 0 );
    glMaterialfv( GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular );
    glMaterialf( GL_FRONT_AND_BACK, GL_SHININESS, 100.0 );
    glEnable( GL_LIGHT0 );
    glEnable( GL_LIGHT1 );
    glEnable( GL_NORMALIZE );
   }

  glColor3f(0.,0.,0.);
  glBegin(GL_LINE_LOOP);
  glVertex3f(bpt[0].x,bpt[0].y,bpt[0].z);
  glVertex3f(bpt[1].x,bpt[1].y,bpt[1].z);
  glVertex3f(bpt[2].x,bpt[2].y,bpt[2].z);
  glVertex3f(bpt[3].x,bpt[3].y,bpt[3].z);
  glEnd();
  glBegin(GL_LINE_LOOP);
  glVertex3f(bpt[4].x,bpt[4].y,bpt[4].z);
  glVertex3f(bpt[5].x,bpt[5].y,bpt[5].z);
  glVertex3f(bpt[6].x,bpt[6].y,bpt[6].z);
  glVertex3f(bpt[7].x,bpt[7].y,bpt[7].z);
  glEnd();
  glBegin(GL_LINE_LOOP);
  glVertex3f(bpt[0].x,bpt[0].y,bpt[0].z);
  glVertex3f(bpt[4].x,bpt[4].y,bpt[4].z);
  glVertex3f(bpt[5].x,bpt[5].y,bpt[5].z);
  glVertex3f(bpt[1].x,bpt[1].y,bpt[1].z);
  glEnd();
  glBegin(GL_LINE_LOOP);
  glVertex3f(bpt[3].x,bpt[3].y,bpt[3].z);
  glVertex3f(bpt[7].x,bpt[7].y,bpt[7].z);
  glVertex3f(bpt[6].x,bpt[6].y,bpt[6].z);
  glVertex3f(bpt[2].x,bpt[2].y,bpt[2].z);
  glEnd();
  for(k=0; k<nk-1; k++)
    for(j=0; j<nj-1; j++)
      for(i=0; i<ni-1; i++){
      if(ive_checkcube(volume_dep(i,j,k), volume_dep(i+1,j,k), 
      volume_dep(i,j+1,k), volume_dep(i+1,j+1,k),
      volume_dep(i,j,k+1), volume_dep(i,j+1,k+1), 
      volume_dep(i+1,j,k+1), volume_dep(i+1,j+1,k+1),sint)){
      vert[0]=volume_dep(i,j,k);
      vert[1]=volume_dep(i+1,j,k);
      vert[2]=volume_dep(i+1,j,k+1);
      vert[3]=volume_dep(i,j,k+1);
      vert[4]=volume_dep(i,j+1,k);
      vert[5]=volume_dep(i+1,j+1,k);
      vert[6]=volume_dep(i+1,j+1,k+1);
      vert[7]=volume_dep(i,j+1,k+1);
      (*ive_3d_pltfunc)(i,j,k,vert,sint);
      }
      }
  
  
  glEndList();
  
  glCallList(curdisplist);
  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
  glFlush();    
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
  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
  glFlush();    
}
 



void
glinput(Widget w, XtPointer clientData, XtPointer callData)
{
    XmDrawingAreaCallbackStruct *cd = (XmDrawingAreaCallbackStruct *) callData;
    char            buffer[1];
    KeySym          keysym;
    int             rc;

    switch (cd->event->type) {
    case KeyRelease:
        /*
         * It is necessary to convert the keycode to a keysym before it is
         * possible to check if it is an escape
         */
        rc = XLookupString((XKeyEvent *) cd->event, buffer, 1, &keysym, NULL);
        switch (keysym) {
        case XK_Up:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(36.0, 1., 0., 0.);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_Down:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(-36.0, 1., 0., 0.);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_Left:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(-36.0, 0., 1., 0.);
	  glCallList(curdisplist);
	  glLoadIdentity();
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_Right:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(36.0, 0., 1., 0.);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_i:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(36.0, 0., 0., 1.);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_o:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(-36.0, 0., 0., 1.);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_j:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(36.0, 1., 1., 1.);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_k:
	  glMatrixMode(GL_PROJECTION);
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glRotatef(-36.0, 1., 1., 1.);
	  glCallList(curdisplist);
	  glLoadIdentity();
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_r:
	  glMatrixMode(GL_PROJECTION);
	  glLoadIdentity();
	  glOrtho(-1.,1.,-1.,1.,-1.,1.);
	  glTranslatef(-(.5 + boxx[0]/boxx[1]),-(.5+ boxy[0]/boxy[1]),
		       -(.5 + boxz[0]/boxz[1]));
	  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  glCallList(curdisplist);
	  glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
	  glFlush();    
	  break;
        case XK_Escape:
	  exit(0);
        }
        break;
    default:
      glCallList(curdisplist);
      glXSwapBuffers(XtDisplay(gl_widget),XtWindow(gl_widget));
      glFlush();    
      break;
    }
}
