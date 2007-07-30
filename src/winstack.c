/*
 * $Id: winstack.c,v 1.3 1997/04/30 18:41:57 harry Exp $
 *
 * $Log: winstack.c,v $
 * Revision 1.3  1997/04/30 18:41:57  harry
 * Change save_window to save_plot_window to avoid conflict with existing
 * routine.
 *
 *
 */
static char ident[] = "Id$";

static struct wind {
    float mins[4], maxs[4];
};

static struct wind wind_list[10];

void save_plot_window_(float mins[4], float maxs[4]){
   int i,j;
   int diff=0;

   for(i=0; i<4; i++){
       if(wind_list[0].mins[i] != mins[i])diff++;
       if(wind_list[0].maxs[i] != maxs[i])diff++;
   }
   if(!diff)return;
   
   for(j=9; j>0; j--){
       for(i=0; i<4; i++){
	   wind_list[j].mins[i]=wind_list[j-1].mins[i];
	   wind_list[j].maxs[i]=wind_list[j-1].maxs[i];
       }
   }
   for(i=0; i<4; i++){
       wind_list[0].mins[i]=mins[i];
       wind_list[0].maxs[i]=maxs[i];
   }
}

void retreive_window_(float mins[4], float maxs[4]){
    int i,j;
    
    for(i=0; i<4; i++){
	if(wind_list[1].mins[i] != wind_list[1].maxs[i]){
	    mins[i]=wind_list[1].mins[i];
	    maxs[i]=wind_list[1].maxs[i];
	}
    }

    for(j=0; j<9; j++){
	for(i=0; i<4; i++){
	    wind_list[j].mins[i]=wind_list[j+1].mins[i];
	    wind_list[j].maxs[i]=wind_list[j+1].maxs[i];
	}
    }
    for(i=0; i<4; i++){
	wind_list[9].mins[i]=0;
	wind_list[9].maxs[i]=0;
    }
}

void init_window_list_(){
    int i,j;
    
    for(j=0; j<9; j++){
	for(i=0; i<4; i++){
	    wind_list[j].mins[i]=0;
	    wind_list[j].maxs[i]=0;
	}
    }
}
