#ifdef ultrix
#define memalign(size1,size2) valloc(size2)
#endif
#ifdef __osf__
#define memalign(size1,size2) malloc(size2)
#endif
/*
   This routine calculates a derivative for the default transforms.
   The form of the field is:

   D[field:dir:type]

   where "field" is the field to calculate the derivative on and "dir"
   is the direction of the derivative (x, y, z, t).  "type" is an optional
   parameter - the default value of "3" means to do a 3 point centered
   difference for the derivative with a 2 point derivative for the edge
   points, resulting in a field on the same grid as the original.  A
   "type" value of "2" means do a 2 point derivative and assign it to 
   the mid-point of the grid, thus resulting in a grid with one
   less point in the direction of the derivative.

   For example, the derivative of u in the x direction is:

   D[u:x:]

   $Log: derivative.c,v $
   Revision 1.7  2001/04/11 19:11:07  harryive
   Add new_field_trans and load_atts calls to the end of getvar.  Remove them
   from other places where they are now no longer needed.

   Revision 1.6  1997/12/18 22:32:46  harry
   Add Id and Log.


   */

static char ident[] = "$Id: derivative.c,v 1.7 2001/04/11 19:11:07 harryive Exp $";

#include <string.h>
#include <strings.h>
#include <malloc.h>
#include <stdio.h>

#define FIELD(i, j, k, t) *(field+i+(dims[0]*((j)+dims[1]*((k)+dims[2]*(t)))))

extern void getaarr_(),make_help_widget_(),index_2_phys_trans_(),getrarr_();

float *getvar_();
int isfunc_(char *, char*, char*, char*, char*, int, int, int, int, int);

float *
derivative_(char *name, int *ndims, int *dims, float *stag, float *min,
	   float *max, float*missing, char *data_units,
	   char *data_display_units, char *dim_names,
	   int len1, int len2, int len3, int len4)
{
    static int four=4;
    char dim_display_names[4][80], arg1[80], arg2[80], arg3[80], buf[80], *a;
    static int flag[4] = {1, 1, 1, 1};
    struct point4 {
	float v[4];
    } *cmpt, *c, *phpt, *phpt1, *phpt2, *p0, *p1, *p2;
    float *f, *field, *p, *result=NULL, f0, f1, f2, delta[4], d, d2;
    int i, j, k, t, num, error, dir, iflag[4];

    *arg1 = *arg2 = *arg3 = '\0';
    if (!isfunc_(name, "D", arg1, arg2, arg3, 80, 1, 80, 80, 80)) return(NULL);
    if ((a=strchr(arg1, ' ')) && a-arg1 < 80) *a = '\0';
    else arg1[79] = '\0';
    if ((a=strchr(arg2, ' ')) && a-arg2 < 80) *a = '\0';
    else arg2[79] = '\0';
    if ((a=strchr(arg3, ' ')) && a-arg3 < 80) *a = '\0';
    else arg3[79] = '\0';
    /*
       Get dimension names.
       */
    getaarr_("dim_display_names", dim_display_names, &four, &error, 17, 80);
    /*
       Get the first (and maybe only) field.
       */
    if ((field = getvar_(arg1, ndims, dims, stag, min, max, missing,
			  data_units, data_display_units, dim_names, &error,
			  strlen(arg1), len2, len3, len4)) == NULL)
	return(NULL);
    for (dir = 0; dir < 4; dir++)
	if (strcasecmp(dim_display_names[dir], arg2) == 0) break;
    if (dir == 4 || dims[dir] < 2) {
	sprintf(buf, "Invalid derivative direction - %s", arg2);
	make_help_widget_(buf);
	return NULL;
    }
    getrarr_("field_delta", delta, &four, &error, 11);
    if (strcmp(arg3, "#") == 0 || strcmp(arg3, "3") == 0) {
	/*
	   Time to allocate space for this puppy.
	   */
	if ((result = (float *) memalign(sizeof(float),
		     sizeof(float)*dims[0]*dims[1]*dims[2]*dims[3])) == NULL) {
	    make_help_widget_("derivatite - can't allocate memory - field");
	    return NULL;
	}
	/*
	   x-direction
	   */
	if (dir == 0) {
	    p = result;
	    /*
	       Unevenly spaced data.
	       */
	    if (delta[dir] == 0.) {
		for (i=0; i < 4; ++i) iflag[i] = (i == 0);
		if ((cmpt = (struct point4 *) memalign(sizeof(struct point4),
				 2*sizeof(struct point4)*dims[0])) 
		    == (struct point4 *) NULL) {
		    make_help_widget_
			("derivative - can't allocate memory - cmpt");
		    free(result);
		    return NULL;
		}
		phpt = cmpt + dims[0];
		for (i=0; i < dims[0]; i++) cmpt[i].v[0] = i+1;
		for (t=0; t < dims[3]; t++) {
		    for (i=0; i < dims[0]; i++) cmpt[i].v[3] = t+1;
		    for (k=0; k < dims[2]; k++) {
			for (i=0; i < dims[0]; i++)
			    cmpt[i].v[2] = k+1;
			for(j=0; j < dims[1]; j++) {
			    for (i=0; i < dims[0]; i++) cmpt[i].v[1] = j+1;
			    index_2_phys_trans_(phpt, cmpt, iflag, &four,
						dims);
			    for (i=dims[0]-1; i >0;  i--)
				phpt[i].v[0] = phpt[i].v[0]-phpt[i-1].v[0];
			    f = field + dims[1]*(j+dims[2]*(k+dims[3]*t));
			    if (*missing) {
				if(f[1] == *missing || f[0] == *missing)
				    *(p++) = *missing;
				else *(p++) = (f[1]-f[0])/phpt[1].v[0];
			    
				for (i=1; i < dims[0]-1; i++, p++) {
				    if (f[i] == *missing ||
					(f[i+1] == *missing && 
					 f[i-1] == *missing))
					*p = *missing;
				    else if (f[i-1] == *missing)
					*p = (f[i+1]-f[i])/phpt[i+1].v[0];
				    else if (f[i+1] == *missing)
					*p = (f[i]-f[i-1])/phpt[i].v[0];
				    else
		 *p = (((f[i+1]-f[i])*phpt[i].v[0]/phpt[i+1].v[0]) +
		       ((f[i]-f[i-1]*phpt[i+1].v[0]/phpt[i].v[0]))) /
			   (phpt[i+1].v[0]+phpt[i].v[0]);
				}
				if (f[i] == *missing || f[i-1] == *missing)
				    *(p++) = *missing;
				else
				    *(p++) = (f[i]-f[i-1])/phpt[i].v[0];
			    }
			    else {
				*(p++) = (f[1]-f[0])/phpt[1].v[0];
				for (i=1; i < dims[0]-1; i++, p++)
		 *p = (((f[i+1]-f[i])*phpt[i].v[0]/phpt[i+1].v[0]) +
		       ((f[i]-f[i-1]*phpt[i+1].v[0]/phpt[i].v[0]))) /
			   (phpt[i+1].v[0]+phpt[i].v[0]);
				*(p++) = (f[i]-f[i-1])/phpt[i].v[0];
			    }
			}
		    }
		}
		free(cmpt);
	    }
	    /*
	       Evenly spaced data.
	       */
	    else {
		d = 1/delta[0];
		d2 = 0.5*d;
		for (t=0; t < dims[3]; t++)
		    for (k=0; k < dims[2]; k++)
			for(j=0; j < dims[1]; j++) {
			    f = field + dims[1]*(j+dims[2]*(k+dims[3]*t));
			    if (*missing) {
				if (f[0] == *missing || f[1] == *missing)
				    *(p++) = *missing;
				else *(p++) = (f[1]-f[0])*d;
				for (i=1; i < dims[0]-1; i++, p++) {
				    if (f[i] == *missing ||
					(f[i+1] == *missing &&
					 f[i-1] == *missing))
					*p = *missing;
				    else if (f[i-1] == *missing)
					*p = (f[i+1]-f[i])*d;
				    else if (f[i+1] == *missing)
					*p = (f[i]-f[i-1])*d;
				    else *p = (f[i+1]-f[i-1])*d2;
				}
			    }
			    else {
				*(p++) = (f[1]-f[0])*d;
				for (i=1; i < dims[0]-1; i++, p++)
				    *(p++) = (f[i]-f[i-1])*d;
				    *p = (f[i+1]-f[i-1])*d2;
				*(p++) = (f[i]-f[i-1])*d;
			    }
			}
	    }
	}
	/*
	   y-direction
	   */
	else if (dir == 1) {
	    p = result;
	    /*
	       Unevenly spaced data.
	       */
	    if (delta[dir] == 0.) {
		for (i=0; i < 4; ++i) iflag[i] = (i == 1);
		if ((cmpt = (struct point4 *) memalign(sizeof(struct point4),
				 4*sizeof(struct point4)*dims[0])) 
		    == (struct point4 *)NULL) {
		    make_help_widget_
			("derivative - can't allocate memory - cmpt");
		    free(result);
		    return NULL;
		}
		phpt = cmpt + dims[0];
		phpt1 = phpt + dims[0];
		phpt2 = phpt1 + dims[0];
		for (i=0; i < dims[0]; i++) 
		    cmpt[i].v[0] = i+1;
		for (t=0; t < dims[3]; t++) {
		    for (i=0; i < dims[0]; i++) cmpt[i].v[3] = t+1;
		    for (k=0; k < dims[2]; k++) {
			for (i=0; i < dims[0]; i++)
			    cmpt[i].v[2] = k+1;
			/*
			   j = 0 case
			   */
			for (i=0; i < dims[0]; i++) cmpt[i].v[1] = 1;
			index_2_phys_trans_(phpt1, cmpt, iflag, &four, dims);
			for (i=0; i < dims[0]; i++) cmpt[i].v[1] = 2;
			index_2_phys_trans_(phpt2, cmpt, iflag, &four, dims);
			if (*missing) 
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i, 0, k, t);
				f2 = FIELD(i, 1, k, t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)/(phpt2[i].v[1]-phpt1[i].v[1]);
			    }
			else
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, 1, k, t) - FIELD(i, 0, k, t))/
				    (phpt2[i].v[1]-phpt1[i].v[1]);
			/*
			   j = 1, dims[1]-2 case
			   */
			for (j=1; j < dims[1]-1; j++) {
			    memcpy(phpt,phpt1,dims[0]*sizeof(struct point4));
			    memcpy(phpt1,phpt2,dims[0]*sizeof(struct point4));
			    for (i=0; i < dims[0]; i++) cmpt[i].v[1] = j+2;
			    index_2_phys_trans_(phpt2,cmpt,iflag,&four,dims);
			    if (*missing)
				for (i=0; i < dims[0]; i++, p++) {
				    f0 = FIELD(i,j-1,k,t);
				    f1 = FIELD(i,j,k,t);
				    f2 = FIELD(i,j+1,k,t);
				    if (f1 == *missing ||
					 (f2 == *missing && f0 == *missing))
					*p = *missing;
				    else if (f0 == *missing)
					*p = (f2-f1)/
					    (phpt2[i].v[1]-phpt1[i].v[1]);
				    else if (f2 == *missing)
					*p = (f1-f0)/
					    (phpt1[i].v[1]-phpt[i].v[1]);
				    else {
					d = (phpt1[i].v[1]-phpt[i].v[1])/
					    (phpt2[i].v[1]-phpt1[i].v[1]);
					*p = ((f2-f1)*d + (f1-f0)/d)/
					    (phpt2[i].v[1]-phpt[i].v[1]);
				    }
				}
			    else
				for (i=0; i < dims[0]; i++, p++) {
				    d = (phpt1[i].v[1]-phpt[i].v[1])/
					(phpt2[i].v[1]-phpt1[i].v[1]);
				    *p = ((FIELD(i,j+1,k,t)-FIELD(i,j,k,t))*d +
					  (FIELD(i,j,k,t)-FIELD(i,j-1,k,t))/d)/
					      (phpt2[i].v[1]-phpt[i].v[1]);
			    }
			}
			/*
			   j = dims[1]-1 case
			   */
			if (*missing)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i,dims[1]-2,k,t);
				f2 = FIELD(i,dims[1]-1,k,t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)/(phpt2[i].v[1]-phpt1[i].v[1]);
			    }
			else
			    for (i=0; i < dims[0]; i++, p++) {
				*p = (FIELD(i, dims[1]-1, k, t) - 
				      FIELD(i, dims[1]-2, k, t))/
					  (phpt2[i].v[1]-phpt1[i].v[1]);
			    }
		    }
		}
		free(cmpt);
	    }
	    /*
	       Evenly spaced data.
	       */
	    else {
		d = 1/delta[1];
		d2 = 0.5*d;
		for (t=0; t < dims[3]; t++)
		    for (k=0; k < dims[2]; k++) {
			/*
			   j = 0 case
			   */
			if (*missing)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i, 0, k, t);
				f2 = FIELD(i, 1, k, t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)*d;
			    }
			else
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, 1, k, t)-FIELD(i, 0, k, t))*d;
			/*
			   j = 1, dims[1]-2 case
			   */
			if (*missing)
			    for (j=1; j < dims[1]-1; j++)
				for (i=0; i < dims[0]; i++, p++) {
				    f0 = FIELD(i,j-1,k,t);
				    f1 = FIELD(i,j,k,t);
				    f2 = FIELD(i,j+1,k,t);
				    if (f1 == *missing ||
					 (f2 == *missing && f0 == *missing))
					*p = *missing;
				    else if (f0 == *missing)
					*p = (f2-f1)*d;
				    else if (f2 == *missing)
					*p = (f1-f0)*d;
				    else
					*p = (f2-f0)*d2;
				}
			else
			    for (j=1; j < dims[1]-1; j++)
				for (i=0; i < dims[0]; i++, p++)
				    *p = (FIELD(i,j+1,k,t)-FIELD(i,j-1,k,t))
					*d2;
			/*
			   j = dims[1]-1 case
			   */
			if (*missing)
			    for (i=0; i < dims[0]; i++,p++) {
				f1 = FIELD(i,dims[1]-2,k,t);
				f2 = FIELD(i,dims[1]-1,k,t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else *p = (f2-f1)*d;
			    }
			else
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, dims[1]-1, k, t)
				      -FIELD(i, dims[1]-2, k, t))*d;
		    }
	    }
	}
	/*
	   z-direction
	   */
	else if (dir == 2) {
	    p = result;
	    /*
	       Unevenly spaced data.
	       */
	    if (delta[dir] == 0.) {
		for (i=0; i < 4; ++i) iflag[i] = (i == 2);
		num = dims[0]*dims[1];
		if ((cmpt = (struct point4 *) memalign(sizeof(struct point4),
			 4*sizeof(struct point4)*num))
		    == (struct point4 *)NULL) {
		    make_help_widget_
			("derivative - can't allocate memory - cmpt");
		    free(result);
		    return NULL;
		}
		phpt = cmpt + num;
		phpt1 = phpt + num;
		phpt2 = phpt1 + num;
		c = cmpt;
		for (j=0; j < dims[1]; j++)
		    for (i=0; i < dims[0]; i++, c++) {
			c->v[0] = i;
			c->v[1] = j;
		    }
		for (t=0; t < dims[3]; t++) {
		    for (i=0; i < num; i++) cmpt[i].v[3] = t+1;
		    /*
		       k = 0 case
		       */
		    for (i=0; i < num; i++) cmpt[i].v[2] = 1;
		    index_2_phys_trans_(phpt1, cmpt, iflag, &four, &num);
		    for (i=0; i < num; i++) cmpt[i].v[2] = 2;
		    index_2_phys_trans_(phpt2, cmpt, iflag, &four, &num);
		    p1 = phpt1;
		    p2 = phpt2;
		    if (*missing) 
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++, p1++, p2++) {
				f1 = FIELD(i, j, 0, t);
				f2 = FIELD(i, j, 1, t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)/(p2->v[2]-p1->v[2]);
			    }
		    else
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++, p1++, p2++)
				*p = (FIELD(i,j,1,t) - FIELD(i,j,0,t))/
				    (p2->v[2]-p1->v[2]);
		    /*
		       k = 1, dims[2]-2 case
		       */
		    for (k=1; k < dims[2]-1; k++) {
			memcpy(phpt, phpt1, num*sizeof(struct point4));
			memcpy(phpt1, phpt2, num*sizeof(struct point4));
			for (i=0; i < num; i++) cmpt[i].v[2] = k+2;
			index_2_phys_trans_(phpt2, cmpt, iflag, &four, &num);
			p0 = phpt;
			p1 = phpt1;
			p2 = phpt2;
			if (*missing)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; 
				     i++,p++,p0++,p1++,p2++) {
				    f0 = FIELD(i,j,k-1,t);
				    f1 = FIELD(i,j,k,t);
				    f2 = FIELD(i,j,k+1,t);
				    if (f1 == *missing ||
					 (f2 == *missing && f0 == *missing))
					*p = *missing;
				    else if (f0 == *missing)
					*p = (f2-f1)/(p2->v[2]-p1->v[2]);
				    else if (f2 == *missing)
					*p = (f1-f0)/(p1->v[2]-p0->v[2]);
				    else {
					d = (p1->v[2]-p0[i].v[2])/
					    (p2->v[2]-p1->v[2]);
					*p = ((f2-f1)*d + (f1-f0)/d)/
					    (p2->v[2]-p0[i].v[2]);
				    }
				}
			else
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; 
				     i++,p++,p0++,p1++,p2++) {
				    d = (p1->v[2]-p0[i].v[2])/
					(p2->v[2]-p1->v[2]);
				    *p = ((f2-f1)*d + (f1-f0)/d)/
					(p2->v[2]-p0[i].v[2]);
			    }
		    }
		    /*
		       k = dims[2]-1 case
		       */
		    p1 = phpt1;
		    p2 = phpt2;
		    if (*missing)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++,p++,p1++,p2++) {
				f1 = FIELD(i,j,dims[2]-2,t);
				f2 = FIELD(i,j,dims[2]-1,t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)/(p2->v[2]-p1->v[2]);
			    }
		    else
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++,p++,p1++,p2++) {
				*p = (FIELD(i, j, dims[2]-1, t) - 
				      FIELD(i, j, dims[2]-2, t))/
					  (p2->v[2]-p1->v[2]);
			    }
		}
		free(cmpt);
	    }
	    /*
	       Evenly spaced data.
	       */
	    else {
		d = 1/delta[2];
		d2 = 0.5*d;
		for (t=0; t < dims[3]; t++) {
		    /*
		       k = 0 case
		       */
		    if (*missing)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1=FIELD(i,j,0,t);
				f2=FIELD(i,j,1,t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)*d;
			    }
		    else
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, j, 1, t)-FIELD(i, j, 0, t))*d;
		    /*
		       k = 1, dims[2]-2 case
		       */
		    if (*missing)
			for (k=1; k < dims[2]-1; k++)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; i++, p++) {
				    f0 = FIELD(i,j,k-1,t);
				    f1 = FIELD(i,j,k,t);
				    f2 = FIELD(i,j,k+1,t);
				    if (f1 == *missing ||
					 (f2 == *missing && f0 == *missing))
					*p = *missing;
				    else if (f0 == *missing)
					*p = (f2-f1)*d;
				    else if (f2 == *missing)
					*p = (f1-f0)*d;
				    else
					*p = (f2-f0)*d2;
				}
		    else
			for (k=1; k < dims[2]-1; k++)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; i++, p++)
				    *p = (FIELD(i,j,k+1,t)-FIELD(i,j,k-1,t))
					*d2;
		    /*
		       k = dims[2] case
		       */
		    if (*missing)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i,j,dims[2]-2,t);
				f2 = FIELD(i,j,dims[2]-1,t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else *p = (f2-f1)*d;
			    }
		    else
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, j, dims[2]-1, t)
				      -FIELD(i, j, dims[2]-2, t))*d;
		}
	    }
	}
	/*
	   t-direction
	   */
	else {
	    p = result;
	    /*
	       Unevenly spaced data.
	       */
	    if (delta[dir] == 0.) {
		for (i=0; i < 4; ++i) iflag[i] = (i == 3);
		num = dims[0]*dims[1]*dims[2];
		if ((cmpt = (struct point4 *) memalign(sizeof(struct point4),
			 4*sizeof(struct point4)*num))
		    == (struct point4 *)NULL) {
		    make_help_widget_
			("derivative - can't allocate memory - cmpt");
		    free(result);
		    return NULL;
		}
		phpt = cmpt + num;
		phpt1 = phpt + num;
		phpt2 = phpt1 + num;
		c = cmpt;
		for (k=0; k < dims[2]; k++)
		    for (j=0; j < dims[1]; j++)
			for (i=0; i < dims[0]; i++, c++) {
			    c->v[0] = i;
			    c->v[1] = j;
			    c->v[2] = k;
			}
		/*
		   t = 0 case
		   */
		for (i=0; i < num; i++) cmpt[i].v[3] = 1;
		index_2_phys_trans_(phpt1, cmpt, iflag, &four, &num);
		for (i=0; i < num; i++) cmpt[i].v[3] = 2;
		index_2_phys_trans_(phpt2, cmpt, iflag, &four, &num);
		p1 = phpt1;
		p2 = phpt2;
		if (*missing) 
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++, p1++, p2++) {
				f1 = FIELD(i, j, k, 0);
				f2 = FIELD(i, j, k, 1);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)/(p2->v[3]-p1->v[3]);
			    }
		else
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++, p1++, p2++)
				*p = (FIELD(i,j,k,1) - FIELD(i,j,k,0))/
				    (p2->v[3]-p1->v[3]);
		/*
		   t = 1, dims[3]-2 case
		   */
		for (t=1; t < dims[3]-1; t++) {
		    memcpy(phpt, phpt1, num*sizeof(struct point4));
		    memcpy(phpt1, phpt2, num*sizeof(struct point4));
		    for (i=0; i < num; i++) cmpt[i].v[3] = t+2;
		    index_2_phys_trans_(phpt2, cmpt, iflag, &four, &num);
		    p0 = phpt;
		    p1 = phpt1;
		    p2 = phpt2;
		    if (*missing)
			for (k=0; k < dims[2]; k++)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; 
				     i++,p++,p0++,p1++,p2++) {
				    f0 = FIELD(i,j,k,t-1);
				    f1 = FIELD(i,j,k,t);
				    f2 = FIELD(i,j,k,t+1);
				    if (f1 == *missing ||
					 (f2 == *missing && f0 == *missing))
					*p = *missing;
				    else if (f0 == *missing)
					*p = (f2-f1)/(p2->v[3]-p1->v[3]);
				    else if (f2 == *missing)
					*p = (f1-f0)/(p1->v[3]-p0->v[3]);
				    else {
					d = (p1->v[3]-p0[i].v[3])/
					    (p2->v[3]-p1->v[3]);
					*p = ((f2-f1)*d + (f1-f0)/d)/
					    (p2->v[3]-p0[i].v[3]);
				    }
				}
		    else
			for (k=0; k < dims[2]; k++)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; 
				     i++,p++,p0++,p1++,p2++) {
				    d = (p1->v[3]-p0[i].v[3])/
					(p2->v[3]-p1->v[3]);
				    *p = ((f2-f1)*d + (f1-f0)/d)/
					(p2->v[3]-p0[i].v[3]);
				}
		}
		/*
		   t = dims[3]-1 case
		   */
		p1 = phpt1;
		p2 = phpt2;
		if (*missing)
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++,p++,p1++,p2++) {
				f1 = FIELD(i,j,k,dims[3]-2);
				f2 = FIELD(i,j,k,dims[3]-1);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)/(p2->v[3]-p1->v[3]);
			    }
		else
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++,p++,p1++,p2++) {
				*p = (FIELD(i, j, k, dims[3]-1) - 
				      FIELD(i, j, k, dims[3]-2))/
					  (p2->v[3]-p1->v[3]);
			    }
	        free(cmpt);
	    }
	    /*
	       Evenly spaced data.
	       */
	    else {
		d = 1/delta[3];
		d2 = 0.5*d;
		/*
		   t = 0 case
		   */
		if (*missing)
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i, j, k, 0);
				f2 = FIELD(i, j, k, 1);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else
				    *p = (f2-f1)*d;
			    }
		else
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, j, k, 1)-FIELD(i, j, k, 0))*d;
		/*
		   t = 1, dims[3]-2 case
		   */
		if (*missing)
		    for (t=1; t < dims[3]-1; t++)
			for (k=0; k < dims[2]; k++)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; i++, p++) {
				    f0 = FIELD(i,j,k,t-1);
				    f1 = FIELD(i,j,k,t);
				    f2 = FIELD(i,j,k,t+1);
				    if (f1 == *missing ||
					 (f2 == *missing && f0 == *missing))
					*p = *missing;
				    else if (f0 == *missing)
					*p = (f2-f1)*d;
				    else if (f2 == *missing)
					*p = (f1-f0)*d;
				    else
					*p = (f2-f0)*d2;
				}
		else
		    for (t=1; t < dims[3]-1; t++)
			for (k=0; k < dims[2]; k++)
			    for (j=0; j < dims[1]; j++)
				for (i=0; i < dims[0]; i++, p++)
				    *p = (FIELD(i,j,k,t+1)-FIELD(i,j,k,t-1))
					*d2;
		/*
		   t = dims[3] case
		   */
		if (*missing)
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i,j,k,dims[3]-2);
				f2 = FIELD(i,j,k,dims[3]-1);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else *p = (f2-f1)*d;
			    }
		else
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i, j, k, dims[3]-1)
				      -FIELD(i, j, k, dims[3]-2))*d;
	    }
	}
    }
    else if (strcmp(arg3, "2") == 0) {
	/*
	   Only works for even grid.
	   */
	if (delta[dir] == 0.) return (NULL);
	/*
	   Time to allocate space for this puppy.
	   */
	dims[dir]--;
	if ((result = (float *) memalign(sizeof(float),
		     sizeof(float)*dims[0]*dims[1]*dims[2]*dims[3])) == NULL) {
	    make_help_widget_("derivatite - can't allocate memory - field");
	    return NULL;
	}
	dims[dir]++;
	/*
	   x-direction
	   */
	if (dir == 0) {
	    p = result;
	    d = 1/delta[0];
	    for (t=0; t < dims[3]; t++)
		for (k=0; k < dims[2]; k++)
		    for(j=0; j < dims[1]; j++) {
			f = field + dims[1]*(j+dims[2]*(k+dims[3]*t));
			if (*missing) {
			    for (i=0; i < dims[0]-1; i++, p++) {
				if (f[i] == *missing ||
				    f[i+1] == *missing)
				    *p = *missing;
				else *p = (f[i+1]-f[i])*d;
			    }
			}
			else {
			    for (i=0; i < dims[0]-1; i++, p++)
				*p = (f[i+1]-f[i])*d;
			}
		    }
	}
	/*
	   y-direction
	   */
	else if (dir == 1) {
	    p = result;
	    d = 1/delta[1];
	    for (t=0; t < dims[3]; t++)
		for (k=0; k < dims[2]; k++) {
		    if (*missing)
			for (j=0; j < dims[1]-1; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i, j, k, t);
				f2 = FIELD(i, j+1, k, t);
				if (f1 == *missing || f2 == *missing)
				    *p = *missing;
				else *p = (f2-f1)*d;
			    }
		    else
			for (j=0; j < dims[1]-1; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i,j+1,k,t)-FIELD(i,j,k,t))*d;
		}
	}
	/*
	   z-direction
	   */
	else if (dir == 2) {
	    p = result;
	    d = 1/delta[2];
	    for (t=0; t < dims[3]; t++) {
		if (*missing)
		    for (k=0; k < dims[2]-1; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i, j, k, t);
				f2 = FIELD(i, j, k+1, t);
				if (f1 == *missing || f2 == *missing)
					*p = *missing;
				    else *p = (f2-f1)*d;
				}
		else
		    for (k=0; k < dims[2]-1; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i,j,k+1,t)-FIELD(i,j,k,t))*d;
	    }
	}
	/*
	   t-direction
	   */
	else {
	    p = result;
	    d = 1/delta[3];
	    if (*missing)
		for (t=0; t < dims[3]-1; t++)
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++) {
				f1 = FIELD(i, j, k, t);
				f2 = FIELD(i, j, k, t+1);
				if (f1 == *missing || f2 == *missing)
					*p = *missing;
				else *p = (f2-f1)*d;
			    }
	    else
		for (t=0; t < dims[3]-1; t++)
		    for (k=0; k < dims[2]; k++)
			for (j=0; j < dims[1]; j++)
			    for (i=0; i < dims[0]; i++, p++)
				*p = (FIELD(i,j,k,t+1)-FIELD(i,j,k,t))*d;
	}
	dims[dir]--;
	min[dir] += 0.5*delta[dir];
	max[dir] -= 0.5*delta[dir];
    }
    return(result);
}
