#ifdef MEMDBG
#include <mnemosyne.h>
#endif
#include <string.h>
#include <stdio.h>
#include <in_commands.h>
extern struct {
  int curseg, curoverlay[15], curlist, curplot, segs[8*15], maxplot;
  }segments_;

void set_max_plot_(){
    int i,j;
    
    if(*prscmd_.comand == ' '){
	(void)make_help_widget_("Max_plot requires a value from 1 to 15");
	return;
    }
    j = 80;
    (void)catnull_(prscmd_.comand, &j, &i,80);
    i = atoi (prscmd_.comand);
    if(i<0 || i >15){
	(void)make_help_widget_("Max_plot requires a value from 1 to 15");
 	return;
    }
    
    
    segments_.maxplot=i;
}