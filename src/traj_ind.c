#include <stdlib.h>
#include <string.h>
#include <strings.h>
extern int chk_point();
extern int checkoutbounds();
extern void getrarr_(),make_help_widget_(),huen_(),getivar_();
extern void getavar_(), get_pointer_(), phys_2_index_trans_(), getrvar_();
extern double interp_();
static float wmin[4], wmax[4], dmin[4], dmax[4];
static int xyaxis[2];
#define MAXPTS 500
int traj_ind_(u,v,w, x, y, z, t, bfield, unx,uny,unz,unt,
	      vnx,vny,vnz,vnt,wnx,wny,wnz,wnt)
     int  *unx,*uny,*unz,*unt, *vnx,*vny,*vnz,*vnt,*wnx,*wny,*wnz,*wnt;
     float *u, *v, *w, **x, **y, **z, **t, **bfield;
{
  extern struct {float start[4];}traj_start_;
  extern struct {float ustag[4],  vstag[4], wstag[4];}traj_stag_;
  extern struct {char att_var_traj[80], traj_component[3][80], 
		   traj_units[3][80];}traj_pointers_;
  int i, length, num_pts, dims,grounded, bx, by, bz, bt, bg, error;
  float phys[4], *tmp_cord, *flip, delt, *x_cord, *y_cord, *z_cord, *t_cord;
  float *bgptr;
  char traj_background[80];
  

  dims=4;
  (void)getrarr_("plwmin",wmin,&dims,&i,6,4);
  (void)getrarr_("plwmax",wmax,&dims,&i,6,4);
  (void)getrarr_("phmin",dmin,&dims,&i,6,4);
  (void)getrarr_("phmax",dmax,&dims,&i,6,4);

  (void)getivar_("xaxis",&xyaxis[0],&i,5);
  (void)getivar_("yaxis",&xyaxis[1],&i,5);
  xyaxis[0]=xyaxis[0]-1; xyaxis[1]=xyaxis[1]-1;

  bg=0;
  (void)getavar_("traj_background_field",traj_background,&i,21,80);
  if(traj_background[0] != '\0' && traj_background[0] != ' '&&
     strncasecmp(traj_background,"depth",5)){
    (void)get_pointer_(traj_background, &bgptr, &bx,&by,&bz,&bt,
                       (int)strlen(traj_background));
    bg=1;
  }
  /* start with 2*time steps points*/
  
  length = 2*sizeof(float)*(*unt);
  if((x_cord=(float *)malloc(length)) == NULL){
    make_help_widget_("can't allocate memory in traj for x dimention");
    return(0);
  }
  if((y_cord=(float *)malloc(length)) == NULL){
    make_help_widget_("can't allocate memory in traj for y dimention");
    if(x_cord != NULL)free(x_cord);
    return(0);
  }
  if((z_cord=(float *)malloc(length)) == NULL){
    make_help_widget_("can't allocate memory in traj for z dimention");
    if(x_cord != NULL)free(x_cord);
    if(y_cord != NULL)free(y_cord);
    return(0);
  }
  if((t_cord=(float *)malloc(length)) == NULL){
    make_help_widget_("can't allocate memory in traj for t dimention");
    if(x_cord != NULL)free(x_cord);
    if(y_cord != NULL)free(y_cord);
    if(z_cord != NULL)free(z_cord);
    return(0);
  }
  
    
  
  /* Don't need to check if start is in range - was done before we got here */
  
    /*     initialise the backward integration*/
  phys[0]=x_cord[0]=traj_start_.start[0];
  phys[1]=y_cord[0]=traj_start_.start[1];
  phys[2]=z_cord[0]=traj_start_.start[2];
  phys[3]=t_cord[0]=traj_start_.start[3];
  
  num_pts=0;
  
  /*Backwards integration*/
  delt = -100;
  grounded = 0;
  /* arbitrary choice of 500 max points to avoid infinite loop*/
  while(!checkoutbounds(phys, dmin, dmax) && !grounded && num_pts < MAXPTS){
    (void)huen_(phys, u, v, w, &dims, unx, uny, unz, unt,
		vnx, wny, vnz, vnt, wnx, wny, wnz, wnt, &grounded,
		&delt, &error);
    if(num_pts == length/sizeof(float)){
      length = length +  2*sizeof(float)*(*unt);
      if((tmp_cord=(float *)realloc(x_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for x dimention");
	if(x_cord != NULL)free(x_cord);
	return(0);
      }
      else{x_cord=tmp_cord;}
      
      if((tmp_cord=(float *)realloc(y_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for y dimention");
	if(x_cord != NULL)free(x_cord);
	if(y_cord != NULL)free(y_cord);
	return(0);
      }
      else{y_cord=tmp_cord;}
      
      if((tmp_cord=(float *)realloc(z_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for z dimention");
	if(x_cord != NULL)free(x_cord);
	if(y_cord != NULL)free(y_cord);
	if(z_cord != NULL)free(z_cord);
	return(0);
      }
      else{z_cord=tmp_cord;}
      
      if((tmp_cord=(float *)realloc(t_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for z dimention");
	if(x_cord != NULL)free(x_cord);
	if(y_cord != NULL)free(y_cord);
	if(z_cord != NULL)free(z_cord);
	if(t_cord != NULL)free(t_cord);
	return(0);
      }
      else{t_cord=tmp_cord;}
    }
    if(chk_point(phys)){
      x_cord[num_pts]=phys[0];
      y_cord[num_pts]=phys[1];
      z_cord[num_pts]=phys[2];
      phys[3]=t_cord[num_pts]=traj_start_.start[3];
      num_pts++;
    }
  }
    
    /*arrays are backwards*/
  if((flip=(float *)malloc(sizeof(float)*num_pts)) == NULL){
    make_help_widget_("can't allocate memory in traj for flip");
    if(x_cord != NULL)free(x_cord);
    if(y_cord != NULL)free(y_cord);
    if(z_cord != NULL)free(z_cord);
    if(t_cord != NULL)free(t_cord);
    return(0);
  }
  for(i=0; i<num_pts; i++){
    flip[i] = x_cord[num_pts-(i+1)];
  }
  for(i=0; i<num_pts; i++){
    x_cord[i] =  flip[i];
  }
  for(i=0; i<num_pts; i++){
    flip[i] = y_cord[num_pts-(i+1)];
  }
  for(i=0; i<num_pts; i++){
    y_cord[i] =  flip[i];
  }
  for(i=0; i<num_pts; i++){
    flip[i] = z_cord[num_pts-(i+1)];
  }
  for(i=0; i<num_pts; i++){
    z_cord[i] =  flip[i];
  }
  for(i=0; i<num_pts; i++){
    flip[i] = t_cord[num_pts-(i+1)];
  }
  for(i=0; i<num_pts; i++){
    t_cord[i] =  flip[i];
  }
  free(flip);



  
  /*Forwards integration*/
  delt = 100;
  grounded = 0;
  phys[0]=traj_start_.start[0];
  phys[1]=traj_start_.start[1];
  phys[2]=traj_start_.start[2];
  phys[3]=traj_start_.start[3];
  while(!checkoutbounds(phys, dmin, dmax) && !grounded && num_pts < MAXPTS){
    (void)huen_(phys, u, v, w, &dims, unx, uny, unz, unt,
		vnx, wny, vnz, vnt, wnx, wny, wnz, wnt, &grounded,
		&delt, &error);
    if(num_pts == length/sizeof(float)){
      length = length +  2*sizeof(float)*(*unt);
      if((tmp_cord=(float *)realloc(x_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for x dimention");
	if(x_cord != NULL)free(x_cord);
	return(0);
      }
      else{x_cord=tmp_cord;}
      
      if((tmp_cord=(float *)realloc(y_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for y dimention");
	if(x_cord != NULL)free(x_cord);
	if(y_cord != NULL)free(y_cord);
	return(0);
      }
      else{y_cord=tmp_cord;}
      
      if((tmp_cord=(float *)realloc(z_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for z dimention");
	if(x_cord != NULL)free(x_cord);
	if(y_cord != NULL)free(y_cord);
	if(z_cord != NULL)free(z_cord);
	return(0);
      }
      else{z_cord=tmp_cord;}
      
      if((tmp_cord=(float *)realloc(t_cord,length)) == NULL){
	make_help_widget_("can't allocate memory in traj for z dimention");
	if(x_cord != NULL)free(x_cord);
	if(y_cord != NULL)free(y_cord);
	if(z_cord != NULL)free(z_cord);
	if(t_cord != NULL)free(t_cord);
	return(0);
      }
      else{t_cord=tmp_cord;}
    }
    if(chk_point(phys)){
      x_cord[num_pts]=phys[0];
      y_cord[num_pts]=phys[1];
      z_cord[num_pts]=phys[2];
      phys[3]=t_cord[num_pts]=traj_start_.start[3];
      num_pts++;
    }
  }
  *x = x_cord;
  *y = y_cord;
  *z = z_cord;
  *t = t_cord;
  if(bg){
    float special, *bf;
    bf = (float *)malloc(num_pts * sizeof(float));
    if(bf==(float *)NULL)
      make_help_widget_("couldn't get background color points");
    else{
      float pval[4],cval[4];
      int need[4],one=1, four=4;
      i=16;
      (void)getrvar_("spval",&special,&error,5);
      need[0]=need[1]=need[2]=need[3]=1;
      for (i=0; i<num_pts; i++){
        pval[0]=x_cord[i];
        pval[1]=y_cord[i];
        pval[2]=z_cord[i];
        pval[3]=t_cord[i];
        cval[0]=cval[1]=cval[2]=cval[3]=special;
        phys_2_index_trans_(pval, cval, need, &four,  &one);
        bf[i] = (float)interp_(bgptr,&bx,&by,&bz,&bt,
			       &cval[0],&cval[1],
			       &cval[2],&cval[3],
			       &special);
      }
    }
    *bfield = bf;
  }
  else
    *bfield = (float *)NULL;
  if(num_pts == MAXPTS-1)
    (void)make_help_widget_
      ("Exceded MAXPTS in tradjectory - probably in loop");
  return(num_pts);
}

