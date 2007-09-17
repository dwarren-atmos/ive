#include <stdlib.h>
extern int chk_point();
extern int checkoutbounds();
extern void getrarr_(),make_help_widget_(),huen_();
static float wmin[4], wmax[4], dmin[4], dmax[4];

int traj_ind_(u,v,w, x, y, z, t, unx,uny,unz,unt,
	      vnx,vny,vnz,vnt,wnx,wny,wnz,wnt)
     int  *unx,*uny,*unz,*unt, *vnx,*vny,*vnz,*vnt,*wnx,*wny,*wnz,*wnt;
     float *u, *v, *w, **x, **y, **z, **t;
{
  extern struct {float start[4];}traj_start_;
  extern struct {float ustag[4],  vstag[4], wstag[4];}traj_stag_;
  extern struct {char att_var_traj[80], traj_component[3][80], 
		   traj_units[3][80];}traj_pointers_;
  int i, length, num_pts, dims;
  float phys[4], *tmp_cord, *flip, delt, *x_cord, *y_cord, *z_cord, *t_cord;
  
  dims=4;
  (void)getrarr_("plwmin",wmin,&dims,&i,6,4);
  (void)getrarr_("plwmax",wmax,&dims,&i,6,4);
  (void)getrarr_("phmin",dmin,&dims,&i,6,4);
  (void)getrarr_("phmax",dmax,&dims,&i,6,4);

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
  while(!checkoutbounds(phys, dmin, dmax)){
    (void)huen_(phys, u, v, w, &dims, unx, uny, unz, unt,
		vnx, wny, vnz, vnt, wnx, wny, wnz, wnt, &delt);
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
  phys[0]=traj_start_.start[0];
  phys[1]=traj_start_.start[1];
  phys[2]=traj_start_.start[2];
  phys[3]=traj_start_.start[3];
  while(!checkoutbounds(phys, dmin, dmax)){
    (void)huen_(phys, u, v, w, &dims, unx, uny, unz, unt,
		vnx, wny, vnz, vnt, wnx, wny, wnz, wnt, &delt);
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
  return(num_pts);
}

