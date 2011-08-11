#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <in_commands.h>

extern void make_help_widget_();

extern struct {
    float tic_width_maj, tic_width_min;
    int tic_labels, tic_decimals;
    float tic_scale;
}tic_stuff_; 
/*declared in pltwin.icl - Solaris 2 seems to give FORTRAN preference*/

void tic_scale_(error)
     int *error;
{
    double num, strtod();
    char *ptr, **argv, **get_args();
    int argc;
    *error = 0;
    
    argv = get_args(&argc);
    if (argc != 2 ||(((num = strtod(*(argv+1), &ptr)) == 0.0 &&
		      ptr == *(argv+1)))){
	(void)make_help_widget_("Tic_scale requires a real number");
        *error=1;
        return;
    }
    tic_stuff_.tic_scale = (float)num;
}
    
void tic_width_(error)
     int *error;
{
    char *ptr, **argv, **get_args();
    int argc;

    *error = 0;

    argv = get_args(&argc);
    if (argc <  2 ){
	(void)make_help_widget_
	  ("Tic_width requires one or two arguments consisting of \
thin, thick or thickest");
	*error = 1;
	return;
    }
    if(argv[1] != NULL){
	if (!strncasecmp(argv[1],"thin",4)){
	    tic_stuff_.tic_width_maj = 1.0;
	}
	else
	  if (!strncasecmp(argv[1],"thickest",8)){
	      tic_stuff_.tic_width_maj = 3.0;
	  }
	  else
	    if (!strncasecmp(argv[1],"thick",5)){
		tic_stuff_.tic_width_maj = 2.0;
	    }
	    else{
		char buff[256];
		sprintf(buff, "%s is not a valid argument for Tic_width",
			argv[1]);
		(void)make_help_widget_(buff);
		*error = 1;
		return;
	    }
    }
    
    if(argv[2] != NULL){
	if(argc == 3){
	    if (!strncasecmp(argv[2],"thin",4)){
		tic_stuff_.tic_width_min = 1.0;
	    }
	    else
	      if (!strncasecmp(argv[2],"thickest",8)){
		  tic_stuff_.tic_width_min = 3.0;
	      }
	      else
		if (!strncasecmp(argv[2],"thick",5)){
		    tic_stuff_.tic_width_min = 2.0;
		}
		else{
		    char buff[256];
		    sprintf(buff, "%s is not a valid argument for Tic_width",
			    argv[1]);
		    (void)make_help_widget_(buff);
		    *error = 1;
		    return;
		}
	}
    }
}

void tic_labels_(error)
     int *error;
{
    char  **argv, **get_args();
    int argc;

    *error = 0;
    argv = get_args(&argc);
    if (argc != 2) {
	*error = 1;
	(void)make_help_widget_
	  ("Tic_labels requires normal, bold or off");
	return;
    }

    if (*argv[1]=='n' || *argv[1]=='N') /*norm*/
      tic_stuff_.tic_labels = 0;
    else
      if (*argv[1]=='b' || *argv[1]=='B') /*bold*/
	tic_stuff_.tic_labels = 1;
      else
	if (*argv[1]=='o' || *argv[1]=='O') /*off*/
	  tic_stuff_.tic_labels = 2;
	else{
	    *error = 1;
	    (void)make_help_widget_
	      ("Tic_labels requires on or off");
	}
}

void tic_decimals_(error)
     int *error;
{
    char  **argv, **get_args();
    int argc;
    long num, strtol();
    char *ptr;

    *error = 0;
    argv = get_args(&argc);
    if (argc != 2) {
        (void)make_help_widget_("Tic_decimals requires a number");
        *error=1;
        return;
    }
    
    if(*argv[1] == 'd' || *argv[1] == 'D'){
	tic_stuff_.tic_decimals = -1;
	return;
    }
    if(((num = strtol(*(argv+1), &ptr, 10)) == 0)&& 
	ptr == argv[1]){
        (void)make_help_widget_("Max_plot requires a real number");
        *error=1;
        return;
    }
    tic_stuff_.tic_decimals = (int)num;
}
    
