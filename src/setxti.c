/*-----------------------------------------------------------------------
 *     $Id: setxti.c,v 1.1 2001/10/25 17:49:44 davidive Exp $
 *     Purpose:
 *        This routine sets the intervals for major and minor tic marks
 *        along the plot abscissa.
 *     Arguments:
 *        error  logi  output  indicates error encountered in this
 *                             routine.
 *     History:
 *        $Log: setxti.c,v $
 *        Revision 1.1  2001/10/25 17:49:44  davidive
 *        moved xtics and ytics to C
 *
 *-----------------------------------------------------------------------*/
#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>

void setxti_(error)
     int *error;
{
    double value, strtod();
    float rvar,val1, val2;
    char *ptr, **argv, **get_args();
    int lock[4], argc, i, freecount;
    *error = 0;
    (void)getrvar_("xmajor",&val1,error,6);
    if(*error)
      return;
    (void)getrvar_("xminor",&val2,error,6);
    if (*error)
      return;
    argv = get_args(&argc);
    if(argv[1] != NULL){ /*argv[0] is function name*/
      if(*argv[1] == 'd' || *argv[1] == 'D')
	val1=0.0;
      else{
	if ((value = strtod(argv[1], &ptr)) == 0.0 &&
	    ptr == argv[1]){
	  (void)make_help_widget_("Setxtic requires \"default\" or a real number");
	  *error=1;
	  return;
	}
	val1=(float)value;
      }
    }
    if(argv[2] != NULL){
      if(*argv[2] == 'd' || *argv[2] == 'D')
	val1=0.0;
      else{
	if ((value = strtod(argv[2], &ptr)) == 0.0 &&
	    ptr == argv[2]){
	  (void)make_help_widget_("Setxtic requires \"default\" or a real number");
	  *error=1;
	  return;
	}
	val2=(float)value;
      }
    }
    
    /*New values will be set only if no error was found in the tests
     *above. If no values were given, then the corresponding variable
     *is reset to its previous value.*/

    if(!*error){
      (void)setrvar_("xmajor",&val1,error,6);
      (void)setrvar_("xminor",&val2,error,6);
    }
    i=4;
    (void)getiarr_("lock",lock,&i, error,4,4);
    freecount=0;
    for(i=0; i<4; i++){
      if(lock[i])freecount++;
    }
    if(freecount == 1 && lock[2] == 0){
      (void)setrvar_("major_tic",&val1,error,9);
      (void)setrvar_("minor_tic",&val2,error,9);
    }      
    
}
    
void setyti_(error)
     int *error;
{
    double value, strtod();
    float rvar,val1, val2;
    char *ptr, **argv, **get_args();
    int lock[4], argc, i, freecount;
    *error = 0;
    (void)getrvar_("ymajor",&val1,error,6);
    if(*error)
      return;
    (void)getrvar_("yminor",&val2,error,6);
    if (*error)
      return;
    argv = get_args(&argc);
    if(argv[1] != NULL){ /*argv[0] is function name*/
      if(*argv[1] == 'd' || *argv[1] == 'D')
	val1=0.0;
      else{
	if ((value = strtod(argv[1], &ptr)) == 0.0 &&
	    ptr == argv[1]){
	  (void)make_help_widget_("Setytic requires \"default\" or a real number");
	  *error=1;
	  return;
	}
	val1=(float)value;
      }
    }
    if(argv[2] != NULL){
      if(*argv[2] == 'd' || *argv[2] == 'D')
	val2=0.0;
      else{
	if ((value = strtod(argv[2], &ptr)) == 0.0 &&
	    ptr == argv[2]){
	  (void)make_help_widget_("Setytic requires \"default\" or a real number");
	  *error=1;
	  return;
	}
	val2=(float)value;
      }
    }
    
    /*New values will be set only if no error was found in the tests
     *above. If no values were given, then the corresponding variable
     *is reset to its previous value.*/

    if(!*error){
      (void)setrvar_("ymajor",&val1,error,6);
      (void)setrvar_("yminor",&val2,error,6);
    }
    i=4;
    (void)getiarr_("lock",lock,&i, error,4,4);
    freecount=0;
    for(i=0; i<4; i++){
      if(lock[i])freecount++;
    }
    if(freecount == 1 && lock[2] != 0){
      (void)setrvar_("major_tic",&val1,error,9);
      (void)setrvar_("minor_tic",&val2,error,9);
    }      
    
}
    
