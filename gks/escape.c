#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include "xgks.h"
#include "fortxgks.h"

/*
 * WHAT:   Set the AllocColour variable to true or false. If true gks will
 *       allocate private colors (currently fixed at 64) if available.
 *
 * HOW:    Calls private routine in x/xcolors.
 *
 * INPUT:  Boolean
 *
 * OUTPUT: Integer, copy of AllocColour 0 or 1.
 */
int
gescsetAllocColour(flag,numcolours)
     Bool flag;
     int numcolours;
{
      return(xXgkssetAllocColour(flag,numcolours));
  }
int
gescsetAllocColour_(flag,numcolours)
     Bool *flag;
     int *numcolours;
{
      return(xXgkssetAllocColour(*flag,*numcolours));
  }

/*
 * WHAT:   Read the AllocColour variable 
 *
 * HOW:    Calls private routine in x/xcolors.
 *
 * INPUT:  Boolean
 *
 * OUTPUT: Integer, copy of AllocColour 0 or 1.
 */
int
gescinqAllocColour()
{
      return(xXgksinqAllocColour());
  }

/*
 * WHAT:   Read the AllocNumColours variable 
 *
 * HOW:    Calls private routine in x/xcolors.
 *
 * INPUT:  Boolean
 *
 * OUTPUT: Integer, copy of AllocNumColours.
 */
int
gescinqAllocNumColours()
{
      return(xXgksinqAllocNumColours());
  }

/*
 * WHAT:   set the real color table
 *
 * HOW:    Calls private routine in x/xcolors.
 *
 * INPUT:  X Colormap
 *
 * OUTPUT: Integer, 1
 */
int
gescsetCTB(ctb)
Colormap ctb;
{
      return(xXgkssetCTB(ctb));
  }
int
gescsetCTB_(ctb)
Colormap *ctb;
{
      return(gescsetCTB(*ctb));
  }
/*
 * WHAT:   get the real colormap
 *
 * HOW:    Calls private routine in x/xcolors.
 *
 * INPUT:  
 *
 * OUTPUT: X Colormap
 */

Colormap
  ginqCTB()
{
  Colormap xXgksinqCTB();
      return(xXgksinqCTB());
  }



/*
 * WHAT:   set the Postscriot page width
 *
 * HOW:    sets global var from segments.c
 *
 * INPUT:  width in inches.
 *
 * OUTPUT: 
 */

gescset_page_width(i)
     int i;
{
      extern int STANDARD_PAGE_W;
      STANDARD_PAGE_W = i;
  }

/*
 * WHAT:   get the Postscriot page width
 *
 * HOW:    gets global var from segments.c
 *
 * INPUT:  
 *
 * OUTPUT: width in inches
 */

float
gescinq_page_width(i)
     int i;
{
      extern int STANDARD_PAGE_W;
      return(STANDARD_PAGE_W);
}


