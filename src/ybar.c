/* $Id: ybar.c,v 1.12 2000/02/14 21:59:37 harry Exp $
 * $Log: ybar.c,v $
 * Revision 1.12  2000/02/14 21:59:37  harry
 * Allow point surface plots.
 *
 * Revision 1.11  1998/12/17 23:40:13  warren
 * Removed unused Yuri code
 *
 * Revision 1.10  1995/05/02 21:03:20  harry
 * Correct point printout.
 *
 * Revision 1.9  1994/03/30  22:47:37  yuri
 * Added comments and more RIGHT comments.
 * More stuff will follow.
 *
 * Revision 1.8  1994/01/20  23:17:17  yuri
 * New format of numbers in averaging messages. (After long long testing).
 *
 * Revision 1.7  1993/11/03  23:47:53  warren
 * general cleanup for port to alpha including getting rid of uchar and ushort.
 *
 */

/*
#define Debug
*/
#include    <stdio.h>
#include    <math.h>
#include    <malloc.h>


/***********************************************************************/
void write_value_ch_(pt1,result, surface)
     float *pt1;
     char result[10];
     int surface;
{
    char message [1000];
    
    if (surface) 
	sprintf(message," Value at ( %.3f, %.3f, sfc, %.3f )\n          is  %s",
		pt1[0],pt1[1],pt1[3] , result);
    else
	sprintf(message," Value at ( %.3f, %.3f, %.3f, %.3f )\n          is  %s",
		pt1[0],pt1[1],pt1[2],pt1[3] , result);
      (void) make_value_widget_ (message);
} 

/***********************************************************************/
void write_result_ch_(pt1,pt2,result, surface)
     float *pt1,*pt2;
     char result[10];
     int surface;
{
    char message [1000];

    if (surface)
	sprintf(message," Average from ( %.3f, %.3f, sfc, %.3f )\n           to ( %.3f, %.3f, sfc, %.3f ) \n                     is  %s",
		pt1[0],pt1[1],pt1[3],pt2[0],pt2[1],pt2[3], result);
    else
	sprintf(message," Average from ( %.3f, %.3f, %.3f, %.3f )\n           to ( %.3f, %.3f, %.3f, %.3f ) \n                     is  %s",
		pt1[0],pt1[1],pt1[2],pt1[3],pt2[0],pt2[1],pt2[2],pt2[3],
		result);
      (void) make_result_widget_ (message);
} 
