static char ident[] = "$Id: property_sheets.c,v 1.24 2002/05/23 19:41:12 davidive Exp $";
/*
 *
 *  $Log: property_sheets.c,v $
 *  Revision 1.24  2002/05/23 19:41:12  davidive
 *  Fixed last crash the correct way. Don't write to dim array with index = -1
 *
 *  Revision 1.23  2002/05/22 17:56:37  davidive
 *  First working version of trajectories colored by alternative fields
 *  There is not a complete interface yet!!!
 *
 *  Revision 1.22  1998/12/17 23:17:14  warren
 *  Cleanup of pointer casts to long instead of int, freeing XmStrings when no
 *  longer in use, passing all args to sprinf's and making correctly casting NULLs
 *
 *  Revision 1.21  1997/06/03 19:14:10  davidive
 *  The big mouse slicing changeover. We have the new form, only requires you
 *  set 2 points (eg x1,y1) (x2,y2) and window or zloc and tloc, and now
 *  always runs through init_points_ to catch all these things.
 *  note: only actually works when two dimentions have points set. Needs to
 *  be expanded into 1, 3 or 4.
 *
 *  Revision 1.20  1997/04/15 17:23:42  harry
 *  Make get_var_atts_ symmetric with set_var_atts_ by adding disp_units.
 *  Change NOZERO command to (OFF, ON) from (0, 1).
 *
 *  Revision 1.19  1997/02/27 18:58:47  warren
 *  removed printf statement
 *
 *  Revision 1.18  1997/02/21 21:17:02  warren
 *  Final fixes for point/mouse slicing and expanding into 2d. It is tested
 *  with the z dimention free and seems to work with others, but still should
 *  be looked at with known data for this.
 *
 *  Revision 1.17  1997/02/21 20:01:48  harry
 *  Fix vector interval to be floating point and have values less than one.
 *
 * Revision 1.16  1996/10/11  01:12:35  warren
 * To fill or not to fill is no longer the only question. Now there is a seperate
 * question of solid or pattern. Also first part of mouse slicing. 2d -> 1d
 * Labels are not fixed yet.
 *
 *  Revision 1.15  1996/08/07 18:14:01  harry
 *  Fix return for get_dim for "vslice" plots.  This fix only works with
 *  current restricted arbitrary slices (i.e. XY vs Z slices).
 *
 * Revision 1.14  1996/07/24  17:08:00  harry
 * Fix get_dim to mirror axis positions that are plotted.
 *
 * Revision 1.13  1995/01/30  15:26:05  warren
 * Fixed crash problem where get_dim_atts was being called with no file.
 *
 * Revision 1.12  1994/02/09  01:24:23  warren
 * make sure data_display_units is saved and increase MKS in areas calls.
 *
 * Revision 1.11  1993/10/26  19:21:25  warren
 * init freecnt to 0 in get_dim()
 *
 * Revision 1.10  1993/10/18  23:07:08  warren
 * updates from 3.0 version.
 *
 * Revision 1.9  1993/10/13  00:03:29  warren
 * Update current version for fixes to 3.0 version related to the meaning of
 * default in the property sheet boxes.
 *
 * Revision 1.8  1993/10/04  22:20:00  warren
 * added number of unlabeled lines to attributes.
 *
 * Revision 1.7  1993/08/12  18:54:03  harry
 * Change name of command stored from CONOTUR_LEVELS to CONTOUR_VALUES.
 *
 * Revision 1.6  1993/07/09  16:35:51  harry
 * Add number of contour values to attributes and change its meaning so
 * that a zero or negative number means use contour interval instead.
 *
 * Revision 1.5  1993/07/02  23:04:26  harry
 * Forgot to add 1 to P_NOZERO for call to save_command_.
 *
 * Revision 1.4  1993/06/29  16:31:21  harry
 * Remove obsolete routine read_d_var, correct add_field to set
 * data_display_orig, change free_var to copy attributes, change
 * get/set_var_atts to not overflow character variables.
 *
 * Revision 1.3  1993/05/25  00:09:44  warren
 * Fixed dales bug
 *
 * Revision 1.2  1993/05/20  17:32:10  warren
 * Additions and changes to make the new forms active.
 *
 * Revision 1.1.1.1  1993/04/07  19:41:07  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.7  1993/03/18  19:20:06  warren
 * set default vector int to 3,3
 *
 * Revision 1.6  1993/02/02  19:32:49  warren
 * fortran wrapper for get_dim
 *
 * Revision 1.5  1993/02/02  19:19:39  warren
 * removed print statements
 *
 * Revision 1.4  1993/02/01  23:40:15  warren
 * implemented direction dependant properties
 *
 */
#include "cdfinfo.h"
#include "diminfo.h"
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <cmndlst.h>

extern void save_command_(),getivar_(),getiarr_(),getrarr_(),getavar_(),setrvar_(),setivar_(),getrvar_();
extern int check_points_line_(),getvid_();
extern cdf_info var_file;

#define VAR var_file.vars[*var_id].vatts
#define DIM dim_atts[dim]
#define NUMDIMS 10
#define bigone 1.0E38  
  
static dim_attributes dim_atts[NUMDIMS];

int get_var_atts_(var_id, lin, cont_int, cont_vals, nozero, dp_units,
		  matic, mitic, min, max, num_vals, num_labs, do_labs, len1)
     int *lin, *var_id, *nozero, *num_vals, *num_labs, *do_labs;
     float *cont_int, *cont_vals;
     char *dp_units;
     float *matic, *mitic, *min, *max;
     int len1;
{
    int i;
    char buff[256];
    if(*var_id < var_file.nvars && *var_id >=0){
	*cont_int = VAR.cont_int;
	*lin = VAR.lin;
	*nozero =  VAR.no_zero;
	for(i = 0; i<40; i++)
	  cont_vals[i] = VAR.cont_values[i];
	strncpy(dp_units, VAR.disp_units, len1);
	*matic = VAR.major_tic;
	*mitic = VAR.minor_tic; 
	*min = VAR.lmin;
	*max = VAR.lmax;
	*num_vals = VAR.num_vals;
	*num_labs = VAR.num_labs;
	*do_labs = VAR.do_labs;
	if (VAR.no_zero) strcpy(buff, "NOZERO=OFF");
	else strcpy(buff, "NOZERO=ON");
	i=P_NOZERO+1;
	save_command_(&i, buff, strlen(buff));
	if(*num_vals > 0){
	    sprintf(buff, "CONTOUR_VALUES=%f", VAR.cont_values[0]);
	    for (i=1; i<*num_vals; i++){
		sprintf(buff,"%s,%f",buff,VAR.cont_values[i]);
	    }
	    i = P_CONTOUR_VALUES + 1;
	    save_command_(&i, buff, strlen(buff));
	}
	else{
	    sprintf(buff, "CONTOUR_INTERVAL=%f", VAR.cont_int);
	    i=P_CONTOUR_INTERVAL + 1;
	    save_command_(&i, buff, strlen(buff));
	    
	}
	return(0);
    }
    return(1);
}

int set_var_atts_(var_id,  lin, cont_int, cont_vals, nozero, dp_units, 
		  matic, mitic, min, max, num_vals, num_labs, do_labs, len1)
     int *lin, *var_id, *nozero, *num_vals, *num_labs, *do_labs;
     float *cont_int, *cont_vals;
     char *dp_units;
     float *matic, *mitic, *min, *max;
     int len1;
{
    int i;
    if(*var_id < var_file.nvars && *var_id >=0){
	VAR.cont_int = *cont_int;
	VAR.lin = *lin;
	VAR.no_zero = *nozero;
	for(i = 0; i<40; i++)
	  VAR.cont_values[i] = cont_vals[i];
	strncpy(VAR.disp_units, dp_units, len1);
	VAR.major_tic = *matic;
	VAR.minor_tic = *mitic; 
	VAR.lmin = *min;
	VAR.lmax = *max;
	VAR.num_vals = *num_vals;
	VAR.num_labs = *num_labs;
	VAR.do_labs = *do_labs;
	return(0);
    }
    return(1);
}

void init_dim_atts_()
{ 
    sprintf(dim_atts[0].name,"%s",  "X");
    dim_atts[0].lin = 1;
    dim_atts[0].min_tic = 0.0;
    dim_atts[0].maj_tic = 0.0;
    dim_atts[0].lmin = bigone;
    dim_atts[0].lmax = bigone;
    dim_atts[0].v_interval = 3;
    dim_atts[0].disp_units[0] = '\0';
    
    sprintf(dim_atts[1].name,"%s",  "Y");
    dim_atts[1].lin = 1;
    dim_atts[1].min_tic = 0.0;
    dim_atts[1].maj_tic = 0.0;
    dim_atts[1].lmin = bigone;
    dim_atts[1].lmax = bigone;
    dim_atts[1].v_interval = 3;
    dim_atts[1].disp_units[0] = '\0';
    
    sprintf(dim_atts[2].name,"%s",  "Z");
    dim_atts[2].lin = 1;
    dim_atts[2].min_tic = 0.0;
    dim_atts[2].maj_tic = 0.0;
    dim_atts[2].lmin = bigone;
    dim_atts[2].lmax = bigone;
    dim_atts[2].v_interval = 3;
    dim_atts[2].disp_units[0] = '\0';
    
    sprintf(dim_atts[3].name,"%s",  "T");
    dim_atts[3].lin = 1;
    dim_atts[3].min_tic = 0.0;
    dim_atts[3].maj_tic = 0.0;
    dim_atts[3].lmin = bigone;
    dim_atts[3].lmax = bigone;
    dim_atts[3].v_interval = 3;
    dim_atts[3].disp_units[0] = '\0';
    
    sprintf(dim_atts[4].name,"%s",  "XY");
    dim_atts[4].lin = 1;
    dim_atts[4].min_tic = 0.0;
    dim_atts[4].maj_tic = 0.0;
    dim_atts[4].lmin = bigone;
    dim_atts[4].lmax = bigone;
    dim_atts[4].v_interval = 3;
    dim_atts[4].disp_units[0] = '\0';
    
    sprintf(dim_atts[5].name,"%s",  "XZ");
    dim_atts[5].lin = 1;
    dim_atts[5].min_tic = 0.0;
    dim_atts[5].maj_tic = 0.0;
    dim_atts[5].lmin = bigone;
    dim_atts[5].lmax = bigone;
    dim_atts[5].v_interval = 3;
    dim_atts[5].disp_units[0] = '\0';
    
    sprintf(dim_atts[6].name,"%s",  "YZ");
    dim_atts[6].lin = 1;
    dim_atts[6].min_tic = 0.0;
    dim_atts[6].maj_tic = 0.0;
    dim_atts[6].lmin = bigone;
    dim_atts[6].lmax = bigone;
    dim_atts[6].v_interval = 3;
    dim_atts[6].disp_units[0] = '\0';

    sprintf(dim_atts[7].name,"%s",  "XT");
    dim_atts[7].lin = 1;
    dim_atts[7].min_tic = 0.0;
    dim_atts[7].maj_tic = 0.0;
    dim_atts[7].lmin = bigone;
    dim_atts[7].lmax = bigone;
    dim_atts[7].v_interval = 3;
    dim_atts[7].disp_units[0] = '\0';
    
    sprintf(dim_atts[8].name,"%s",  "YT");
    dim_atts[8].lin = 1;
    dim_atts[8].min_tic = 0.0;
    dim_atts[8].maj_tic = 0.0;
    dim_atts[8].lmin = bigone;
    dim_atts[8].lmax = bigone;
    dim_atts[8].v_interval = 3;
    dim_atts[8].disp_units[0] = '\0';

    sprintf(dim_atts[9].name,"%s",  "ZT");
    dim_atts[9].lin = 1;
    dim_atts[9].min_tic = 0.0;
    dim_atts[9].maj_tic = 0.0;
    dim_atts[9].lmin = bigone;
    dim_atts[9].lmax = bigone;
    dim_atts[9].v_interval = 3;
    dim_atts[9].disp_units[0] = '\0';
}

int get_dim(ax)
     int ax; /*0 = hor; 1 = vert*/
{
    float point_1[4], point_2[4], point_3[4];
    int use_buttons, err, dim=4, zeroes, i, freecnt=0;
    int fix[4], v[3];
    float A,B,C,D;
    char tmp[4];
    
    (void)getivar_("use_buttons",&use_buttons,&err,11);
    (void)getiarr_("lock",fix,&dim,&err,4,4);

    (void)getrarr_("point_1",point_1,&dim,&err,7,3);
    (void)getrarr_("point_2",point_2,&dim,&err,7,3);
    (void)getrarr_("point_3",point_3,&dim,&err,7,3);

    for (i=0; i<4; i++)
      if(fix[i] == 0)
	freecnt++;

    if(use_buttons){
	switch (freecnt){
	  case 0:
	  case 3:
	  case 4:
	    return(-99);
	    break;
	  case 2: /*2d contour or vector*/
	    switch(ax){
	      case 0:
		if (!fix[0]) strcpy(tmp, "X");
		else if (!fix[1]) strcpy(tmp, "Y");
		else strcpy(tmp, "T");
		break;
	      case 1:
		if (!fix[2]) strcpy(tmp, "Z");
		else if (!fix[3]) strcpy(tmp, "T");
		else strcpy(tmp, "Y");
		break;
	    }
	    break;
	  case 1:
	    switch(ax){
	      case 0:
		if(!fix[0] /*X*/) strcpy(tmp, "X");
		else if(!fix[1] /*Y*/) strcpy(tmp, "Y");
		else if(!fix[2] /*Z*/) strcpy(tmp, "VAR");
		else if(!fix[3] /*T*/) strcpy(tmp, "T");
		break;
	      case 1:
		if(!fix[2] /*Z*/) strcpy(tmp, "Z");
		else strcpy(tmp, "VAR");
		break;
	    }
	    break;
	}
	
	for (i = 0; i < NUMDIMS; i++){
	    if(!strcasecmp(dim_atts[i].name,tmp)){
		return(i);
	    }
	}
	return(-1);
    }
    else {

/*
angle through time/space
  */
	if((i=(int)check_points_line_()) == 2){
	    if (point_1[3] == point_2[3] && 
		point_2[3] == point_3[3]) {	/* Time locked */
		fix[3] = 1;
		v[0] = 0;
		v[1] = 1;
		v[2] = 2;
	    }
	    else if (point_1[2] == point_2[2] && 
		     point_2[2] == point_3[2]) {	/* Z locked */
		v[0] = 0;
		v[1] = 1;
		fix[2] = 1;
		v[2] = 3;
	    }	
	    else if (point_1[1] == point_2[1] && 
		     point_2[1] == point_3[1]) {	/* Y locked */
		v[0] = 0;
		v[1] = 2;
		v[2] = 3;
		fix[1] = 1;
	    }
	    else {			/* X locked */
		v[0] = 1;
		v[1] = 2;
		v[2] = 3;
		fix[0] = 1;
	    }
	    
	    A = point_1[v[1]]*(point_2[v[2]]-point_3[v[2]]) + 
		point_2[v[1]]*(point_3[v[2]]-point_1[v[2]]) + 
		point_3[v[1]]*(point_1[v[2]]-point_2[v[2]]);
	    B = point_1[v[0]]*(point_2[v[2]]-point_3[v[2]]) + 
		point_2[v[0]]*(point_3[v[2]]-point_1[v[2]]) + 
		point_3[v[0]]*(point_1[v[2]]-point_2[v[2]]);
	    C = point_1[v[0]]*(point_2[v[1]]-point_3[v[1]]) + 
		point_2[v[0]]*(point_3[v[1]]-point_1[v[1]]) + 
		point_3[v[0]]*(point_1[v[1]]-point_2[v[1]]);
	    D = point_1[v[0]]*((point_2[v[1]]*point_3[v[2]]) - 
			       (point_3[v[1]]*point_2[v[2]])) - 
		point_2[v[0]]*((point_1[v[1]]*point_3[v[2]]) - 
			       (point_3[v[1]]*point_1[v[2]])) +
		point_3[v[0]]*((point_1[v[1]]*point_2[v[2]]) - 
			       (point_2[v[1]]*point_1[v[2]]));
	    zeroes = !A + !B + !C;
	    
	    if (zeroes == 2) {
		if (A) {	/* v[0] = D/A slice constant v[0] */
		    fix[v[0]] = 1;
		}
		else if (B) {   /* v[1] = -D/B slice constant v[1] */
		    fix[v[1]] = 1;
		}
		else {		/* v[2] = D/C slice constant v[2] */
		    fix[v[2]] = 1;
		}
		switch(ax){
		case 0:
		    if (!fix[0]) strcpy(tmp, "X");
		    else if (!fix[1]) strcpy(tmp, "Y");
		    else strcpy(tmp, "T");
		    break;
		case 1:
		    if (!fix[2]) strcpy(tmp, "Z");
		    else if (!fix[3]) strcpy(tmp, "T");
		    else strcpy(tmp, "Y");
		    break;
		}
	    }
	    else if (zeroes == 1) { /*diagonal with one free */
		if(!A){ /* v[1] = (C/B)v[2] -D/B or
			   v[2] = (B/C)v[1] + D/C and v[0] is free*/
		    switch(ax){
		    case 0:
			if(v[1] == 1){
			    if(v[2] == 2)
				strcpy(tmp,"YZ");
			    else
				strcpy(tmp,"YT");
			}
			else
			    strcpy(tmp,"ZT");
			break;
		    case 1:
			if (v[0] == 0) strcpy(tmp, "X");
			else if (v[0] == 1) strcpy(tmp, "Y");
			break;
		    }
		
		}
		else if(!B){ /* v[0] = -(C/A)v[2] + D/A or
				v[2] = -(A/C)v[0] + D/C and v[1] is free*/
		    switch(ax){
		    case 0:
			if(v[0] == 0){
			    if(v[2] == 2)
				strcpy(tmp,"XZ");
			    else
				strcpy(tmp,"TT");
			}
			else
			    if(v[2] == 2)
				strcpy(tmp,"YZ");
			    else
				strcpy(tmp,"YT");
			break;
		    case 1:
			if (v[1] == 1) strcpy(tmp, "Y");
			else if (v[1] == 2) strcpy(tmp, "Z");
			break;
		    }
		}
		else if(!C){ /* v[0] = (B/A)v[1] + (D/A) or
				v[1] = -(D/B) + (A/B)v[0]  and v[2] is free*/
		    switch(ax){
		    case 0:
			if(v[0] == 0){
			    if(v[1] == 1)
				strcpy(tmp,"XY");
			    else
				strcpy(tmp,"XZ");
			}
			else
				strcpy(tmp,"YZ");
			break;
		    case 1:
			if (v[2] == 2) strcpy(tmp, "Z");
			else strcpy(tmp, "T");
			break;
		    }		
		}
	    }
	    for (i = 0; i < NUMDIMS; i++){
		if(!strcasecmp(dim_atts[i].name,tmp)){
		    return(i);
		}
	    }

	}
	if(i == 1){
	    switch (ax){
	    case 0:
		if((point_1[0] != point_2[1]) || (point_2[0] != point_3[0]))
		    strcpy(tmp, "X");
		else if((point_1[1] != point_2[1]) || 
			(point_2[1] != point_3[1]))
		    strcpy(tmp, "Y");
		else if((point_1[2] != point_2[2]) || 
			(point_2[2] != point_3[2]))
		    strcpy(tmp, "VAR");
		else if((point_1[3] != point_2[3]) || 
			(point_2[3] != point_3[3]))
		    strcpy(tmp, "T");
		break;
	    case 1:
		if((point_1[0] != point_2[1]) || (point_2[0] != point_3[0]))
		    strcpy(tmp, "VAR");
		else if((point_1[1] != point_2[1]) || 
			(point_2[1] != point_3[1]))
		    strcpy(tmp, "VAR");
		else if((point_1[2] != point_2[2]) || 
			(point_2[2] != point_3[2]))
		    strcpy(tmp, "Z");
		else if((point_1[3] != point_2[3]) || 
			(point_2[3] != point_3[3]))
		    strcpy(tmp, "T");
		break;
		
	    }
	    for (i = 0; i < NUMDIMS; i++){
		if(!strcasecmp(dim_atts[i].name,tmp)){
		    return(i);
		}
	    }
	    return(-1);
	}
    }
    return (-99);
}

void get_dim_atts_()
{
    int i,dim, linlog[2], vid, *var_id, err;
    char field[256], buff[256];
    linlog[0]=linlog[1]=0;
    
    for (i=0;i<256;i++)field[i] = '\0';
    (void)getavar_("field",field,&i,5,256);
    if(field[0] == ' ' || field[0]=='\0')return;
    vid=getvid_(field,strlen(field));
    var_id=&vid;

    dim = get_dim(0); /*horizontal*/ 
    if(dim < NUMDIMS && dim>=0){
	setivar_("incx",&DIM.v_interval,&i,4);
	linlog[0] = DIM.lin;
	setrvar_("xmajor",&DIM.maj_tic,&i,6);
	setrvar_("xminor",&DIM.min_tic,&i,6);
	sprintf(buff,"XTIC=%f,%f",DIM.maj_tic,DIM.min_tic);
	i=P_XTICS+1;
	save_command_(&i, buff, strlen(buff));
	setrvar_("abscissa_max",&DIM.lmax,&i,12);
	setrvar_("abscissa_min",&DIM.lmin,&i,12);
	sprintf(buff,"ABSCISSA=%f,%f",DIM.lmax,DIM.lmin);
	i=P_ABSCISSA+1;
	save_command_(&i, buff, strlen(buff));
	/*	sprintf(dp_units,"%s",DIM.disp_units); */
	dim = get_dim(1); /*vertical*/
	if(dim < NUMDIMS && dim>=0){
	    setivar_("incy",&DIM.v_interval,&i,4);
	    linlog[1] = DIM.lin;
	    setrvar_("ymajor",&DIM.maj_tic,&i,6);
	    setrvar_("yminor",&DIM.min_tic,&i,6);
	    sprintf(buff,"YTICS=%f,%f",DIM.maj_tic,DIM.min_tic);
	    i=P_YTICS+1;
	    save_command_(&i, buff, strlen(buff));
	    setrvar_("ordinate_max",&DIM.lmax,&i,12);
	    setrvar_("ordinate_min",&DIM.lmin,&i,12);
	    sprintf(buff,"ORDINATE=%f,%f",DIM.lmax,DIM.lmin);
	    i=P_ORDINATE+1;
	    save_command_(&i, buff, strlen(buff));
	    /*	sprintf(dp_units,"%s",DIM.disp_units); */
	}
	else if(dim == -1){
	    linlog[1] = VAR.lin;
	    setrvar_("ymajor",&VAR.major_tic,&i,6);
	    setrvar_("yminor",&VAR.minor_tic,&i,6);
	    sprintf(buff,"YTICS=%f,%f",VAR.major_tic,VAR.minor_tic);
	    i=P_YTICS+1;
	    save_command_(&i, buff, strlen(buff));
	    setrvar_("ordinate_max",&VAR.lmax,&i,12);
	    setrvar_("ordinate_min",&VAR.lmin,&i,12);
	    sprintf(buff,"ORDINATE=%f,%f",VAR.lmax,VAR.lmin);
	    i=P_ORDINATE+1;
	    save_command_(&i, buff, strlen(buff));
	    /*	sprintf(dp_units,"%s",DIM.disp_units); */
	}
    }
    else if(dim == -1){
	linlog[0] = VAR.lin;
	setrvar_("xmajor",&VAR.major_tic,&i,6);
	setrvar_("xminor",&VAR.minor_tic,&i,6);
	sprintf(buff,"XTIC=%f,%f",VAR.major_tic,VAR.minor_tic);
	i=P_XTICS+1;
	save_command_(&i, buff, strlen(buff));
	setrvar_("abscissa_max",&VAR.lmax,&i,12);
	setrvar_("abscissa_min",&VAR.lmin,&i,12);
	sprintf(buff,"ABSCISSA=%f,%f",VAR.lmax,VAR.lmin);
	i=P_ABSCISSA+1;
	save_command_(&i, buff, strlen(buff));
	/*	sprintf(dp_units,"%s",DIM.disp_units); */
	dim = get_dim(1); /*vertical*/
	if(dim < NUMDIMS && dim>=0){
	    setivar_("incy",&DIM.v_interval,&i,4);
	    linlog[1] = DIM.lin;
	    setrvar_("ymajor",&DIM.maj_tic,&i,6);
	    setrvar_("yminor",&DIM.min_tic,&i,6);
	    sprintf(buff,"YTICS=%f,%f",DIM.maj_tic,DIM.min_tic);
	    i=P_YTICS+1;
	    save_command_(&i, buff, strlen(buff));
	    setrvar_("ordinate_max",&DIM.lmax,&i,12);
	    setrvar_("ordinate_min",&DIM.lmin,&i,12);
	    sprintf(buff,"ORDINATE=%f,%f",DIM.lmax,DIM.lmin);
	    i=P_ORDINATE+1;
	    save_command_(&i, buff, strlen(buff));
	    /*	sprintf(dp_units,"%s",DIM.disp_units); */
	}
    }
    else{
	return;
    }
    if(linlog[0]){
	if(linlog[1])
	  i = 1;
	else
	  i = 2;
    }
    else{
	if(linlog[1])
	  i = 3;
	else
	  i = 4;
    }
    setivar_("linlog",&i,&err,6);
    switch(i){
      case 1:
	sprintf(buff,"AXES=LINLIN");
	break;
      case 2:
	sprintf(buff,"AXES=LINLOG");
	break;
      case 3:
	sprintf(buff,"AXES=LOGLIN");
	break;
      case 4:
	sprintf(buff,"AXES=LOGLOG");
	break;
    }
    i=P_AXES+1;
    save_command_(&i, buff, strlen(buff));
}

void set_dim_atts_()
{
    int i,dim, err, linlog[2], vid, *var_id;
    char field[256];
    
    for (i=0;i<256;i++)field[i] = '\0';
    (void)getavar_("field",field,&i,5,256);
    vid=getvid_(field,strlen(field));
    if(vid == -99)return;
    var_id=&vid;

   getivar_("linlog",&i,&err,6);
   switch(i){
     case 1:
       linlog[0] = 1;
       linlog[1] = 1;
       break;
     case 2:
       linlog[0] = 1;
       linlog[1] = 0;
       break;
     case 3:
       linlog[0] = 0;
       linlog[1] = 1;
       break;
     case 4:
       linlog[0] = 0;
       linlog[1] = 0;
       break;
   }
    dim = get_dim(0); /*horizontal*/
    if(dim < NUMDIMS && dim>=0){
	getivar_("incx",&DIM.v_interval,&i,4);
	DIM.lin = linlog[0];
	getrvar_("xmajor",&DIM.maj_tic,&i,6);
	getrvar_("xminor",&DIM.min_tic,&i,6);
	getrvar_("abscissa_max",&DIM.lmax,&i,12);
	getrvar_("abscissa_min",&DIM.lmin,&i,12);
/*	sprintf(dp_units,"%s",DIM.disp_units); */
	dim = get_dim(1); /*vertical*/
	if(dim < NUMDIMS && dim>=0){
	    getivar_("incy",&DIM.v_interval,&i,4);
	    DIM.lin = linlog[1];
	    getrvar_("ymajor",&DIM.maj_tic,&i,6);
	    getrvar_("yminor",&DIM.min_tic,&i,6);
	    getrvar_("ordinate_max",&DIM.lmax,&i,12);
	    getrvar_("ordinate_min",&DIM.lmin,&i,12);
	    /*	sprintf(dp_units,"%s",DIM.disp_units); */
	}
	else if(dim == -1){
	    getrvar_("ymajor",&VAR.major_tic,&i,6);
	    getrvar_("yminor",&VAR.minor_tic,&i,6);
	    getrvar_("ordinate_max",&VAR.lmax,&i,12);
	    getrvar_("ordinate_min",&VAR.lmin,&i,12);
	    /*	sprintf(dp_units,"%s",DIM.disp_units); */
	}
    }
    else if(dim == -1){
	getrvar_("xmajor",&VAR.major_tic,&i,6);
	getrvar_("xminor",&VAR.minor_tic,&i,6);
	getrvar_("abscissa_max",&VAR.lmax,&i,12);
	getrvar_("abscissa_min",&VAR.lmin,&i,12);
	/*	sprintf(dp_units,"%s",DIM.disp_units); */
	dim = get_dim(1); /*vertical*/
	if(dim < NUMDIMS && dim>=0){
	    getivar_("incy",&DIM.v_interval,&i,4);
	    DIM.lin = linlog[1];
	    getrvar_("ymajor",&DIM.maj_tic,&i,6);
	    getrvar_("yminor",&DIM.min_tic,&i,6);
	    getrvar_("ordinate_max",&DIM.lmax,&i,12);
	    getrvar_("ordinate_min",&DIM.lmin,&i,12);
	    /*	sprintf(dp_units,"%s",DIM.disp_units); */
	}
    }
}

int get_dim_(ax)
     int *ax; /*0 = hor; 1 = vert*/
{
    return(get_dim(*ax));
}


void get_traj_atts_(var_id, traj_val, traj_step)
     int *var_id;
     float *traj_val, *traj_step;
{
  int dim;
  if(*var_id < var_file.nvars && *var_id >=0){
    *traj_val = VAR.traj_val;
    *traj_step = VAR.traj_step;
  }
  else if(*var_id < 0 && *var_id >-5){
    dim = -(*var_id) - 1;
    *traj_val = DIM.traj_val;
    *traj_step = DIM.traj_step;    
  }
  else{
    *traj_val = -1e19;
    *traj_step = 0;
  }
}

void set_traj_atts_(var_id, traj_val, traj_step)
     int *var_id;
     float *traj_val, *traj_step;
{
  int dim;
  if(*var_id < var_file.nvars && *var_id >=0){
    VAR.traj_val = *traj_val;
    VAR.traj_step = *traj_step;
  }
  else if(*var_id < 0 && *var_id >-5){
    dim = -(*var_id) - 1;
    DIM.traj_val = *traj_val;
    DIM.traj_step = *traj_step;    
  }
}
