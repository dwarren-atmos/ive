#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>

struct {
  int plot_axis;
  int rec_axis;
  float slope;
  float intercept;
} angle_plot={0,0,0.,0.};

static int chk_axis(char *ax){
  switch(*ax){
  case 'D':
    return(-1);
    break;
  case 'X':
    return(0);
    break;
  case 'Y':
    return(1);
    break;
  case 'Z':
    return(2);
    break;
  case 'T':
    return(3);
    break;
  case 'd':
    return(-1);
    break;
  case 'x':
    return(0);
    break;
  case 'y':
    return(1);
    break;
  case 'z':
    return(2);
    break;
  case 't':
    return(3);
    break;
  }
  /* can't get here, but compiler complains*/
  return(5)
}

void point_fix_label_value_(int *axes, float *lab){
  if(angle_plot.plot_axis >-1 && angle_plot.plot_axis != angle_plot.rec_axis){
    *lab = *lab * angle_plot.slope + angle_plot.intercept;
    *axes = angle_plot.plot_axis;
  }
}

void point_axis_(int *error){

  char *ptr, **argv, **get_args();
  int argc, axis;

  *error=0;
   argv = get_args(&argc);
   if (argc != 2 ||((axis = chk_axis(argv[1]))<-1)){
     (void)make_help_widget_
       ("point_axis requires one of X, Y, Z, T or Default");
     *error=1;
     return;
   }
   angle_plot.plot_axis=axis;
}
