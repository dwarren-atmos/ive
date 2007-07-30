#include <X11/X.h>
#include <X11/Intrinsic.h>

extern Widget Box;

int chkcolor_(){
    if (XtScreen(Box)->root_depth > 2)
      return (1);

    return(0);
}
