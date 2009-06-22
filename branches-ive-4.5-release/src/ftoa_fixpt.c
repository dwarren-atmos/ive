#include <stdio.h>
#define ABS(a) ((a) < 0 ? -(a) : (a))
void ftoa_fixpt_(in_num, num_dec, out_buf, len)
     float *in_num;
     int   *num_dec;
     char  *out_buf;
     int   len;      /* FORTRAN dummy var for length of out_buf */
/* $Id: ftoa_fixpt.c,v 1.1 1994/06/28 17:43:31 warren Exp $
 * Purpose:
 * This routine is passed a real - in_num,
 * an int num_dec and 
 * a return buffer out_buf. 
 * It writes the real in the return buffer using the number of decimal places 
 * specified by the int.
 *
 * History:
 * $Log: ftoa_fixpt.c,v $
 * Revision 1.1  1994/06/28 17:43:31  warren
 * First cut at fancy tic marks.
 *
 */
{
    char buff[80]; /* buffer for holding format string */
    int i;
    double tester=1.0;
    
    for(i=0; i<len; i++)
      out_buf[i] = ' ';
    if(*num_dec==0){
	sprintf(out_buf,"%.0f\n",*in_num);
    }
    else{
	for(i=0; i<*num_dec; i++){
	    tester /= 10.0;
	}
	if(*in_num>10000.0 ||  (ABS(*in_num) < tester && *in_num != 0.0)){
	    sprintf(buff, "%%.%dE",*num_dec);
	    sprintf(out_buf,buff,*in_num);
	}
	else{
	    sprintf(buff, "%%.%df",*num_dec);
	    sprintf(out_buf,buff,*in_num);
	}	    
    }

    return;
}
