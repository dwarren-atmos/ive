#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>
#include <volume.h>
extern Objects IVE_Object;
extern float move();
extern float rotate();

extern void getrvar_(),make_help_widget_(),setrvar_(),getiarr_();
void rot3d_(error)
     int *error;
{
    double value, strtod();
    float rvar,val1, val2;
    char *ptr, **argv, **get_args();
    int lock[4], argc, i, freecount;
    *error = 0;
    argv = get_args(&argc);
    if(argv[1] != NULL){ /*argv[0] is function name*/
      if(*argv[1] == 'd' || *argv[1] == 'D')
	val1=0.0;
      else{
	if ((value = strtod(argv[1], &ptr)) == 0.0 &&
	    ptr == argv[1]){
	  (void)make_help_widget_("Rotation requires \"default\" or a real number");
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
	  (void)make_help_widget_("Rotation requires \"default\" or a real number");
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
      xRotation+=val1;
      yRotation+=val2;
      xRotation=(int)(xRotation+360)%360;
      yRotation=(int)(yRotation+360)%360;
      reDraw(&IVE_Object);
    }
}
    
void trans3d_(error)
     int *error;
{
    double value, strtod();
    float rvar,val1, val2;
    char *ptr, **argv, **get_args();
    int lock[4], argc, i, freecount;
    *error = 0;
    val1=0.0;
    val2=0.0;
    argv = get_args(&argc);
    if(argv[1] != NULL){ /*argv[0] is function name*/
      if(*argv[1] == 'd' || *argv[1] == 'D')
	val1=0.0;
      else{
	if ((value = strtod(argv[1], &ptr)) == 0.0 &&
	    ptr == argv[1]){
	  (void)make_help_widget_("Translation requires \"default\" or a real number");
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
	  (void)make_help_widget_("Translation requires \"default\" or a real number");
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
      xPosition+=val1;
      yPosition+=val2;
      reDraw(&IVE_Object);
    }  
}
    
void zoom3d_(error)
     int *error;
{
    double value, strtod();
    float rvar,val1;
    char *ptr, **argv, **get_args();
    int lock[4], argc, i, freecount;
    *error = 0;
    argv = get_args(&argc);
    if(argv[1] != NULL){ /*argv[0] is function name*/
      if(*argv[1] == 'd' || *argv[1] == 'D')
	val1=0.0;
      else{
	if ((value = strtod(argv[1], &ptr)) == 0.0 &&
	    ptr == argv[1]){
	  (void)make_help_widget_("Translation requires \"default\" or a real number");
	  *error=1;
	  return;
	}
	val1=(float)value;
	  }
	}

    
    /*New values will be set only if no error was found in the tests
     *above. If no values were given, then the corresponding variable
     *is reset to its previous value.*/

    if(!*error){
      StretchPercent=val1;
      reDraw(&IVE_Object);
    }    
}
