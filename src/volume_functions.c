#define IVE_CUBE_PLOTS
#define FLT_MAX 3.40282347e+38f
#include <volume.h>
#include <window.h>
#include <ive_3d_macros.h>
#define EPSLON 1.0e-15
extern int ginqcolorrep();
extern char *  gluErrorString();
static int edgeTable[256]={
  0x0  , 0x109, 0x203, 0x30a, 0x406, 0x50f, 0x605, 0x70c,
  0x80c, 0x905, 0xa0f, 0xb06, 0xc0a, 0xd03, 0xe09, 0xf00,
  0x190, 0x99 , 0x393, 0x29a, 0x596, 0x49f, 0x795, 0x69c,
  0x99c, 0x895, 0xb9f, 0xa96, 0xd9a, 0xc93, 0xf99, 0xe90,
  0x230, 0x339, 0x33 , 0x13a, 0x636, 0x73f, 0x435, 0x53c,
  0xa3c, 0xb35, 0x83f, 0x936, 0xe3a, 0xf33, 0xc39, 0xd30,
  0x3a0, 0x2a9, 0x1a3, 0xaa , 0x7a6, 0x6af, 0x5a5, 0x4ac,
  0xbac, 0xaa5, 0x9af, 0x8a6, 0xfaa, 0xea3, 0xda9, 0xca0,
  0x460, 0x569, 0x663, 0x76a, 0x66 , 0x16f, 0x265, 0x36c,
  0xc6c, 0xd65, 0xe6f, 0xf66, 0x86a, 0x963, 0xa69, 0xb60,
  0x5f0, 0x4f9, 0x7f3, 0x6fa, 0x1f6, 0xff , 0x3f5, 0x2fc,
  0xdfc, 0xcf5, 0xfff, 0xef6, 0x9fa, 0x8f3, 0xbf9, 0xaf0,
  0x650, 0x759, 0x453, 0x55a, 0x256, 0x35f, 0x55 , 0x15c,
  0xe5c, 0xf55, 0xc5f, 0xd56, 0xa5a, 0xb53, 0x859, 0x950,
  0x7c0, 0x6c9, 0x5c3, 0x4ca, 0x3c6, 0x2cf, 0x1c5, 0xcc ,
  0xfcc, 0xec5, 0xdcf, 0xcc6, 0xbca, 0xac3, 0x9c9, 0x8c0,
  0x8c0, 0x9c9, 0xac3, 0xbca, 0xcc6, 0xdcf, 0xec5, 0xfcc,
  0xcc , 0x1c5, 0x2cf, 0x3c6, 0x4ca, 0x5c3, 0x6c9, 0x7c0,
  0x950, 0x859, 0xb53, 0xa5a, 0xd56, 0xc5f, 0xf55, 0xe5c,
  0x15c, 0x55 , 0x35f, 0x256, 0x55a, 0x453, 0x759, 0x650,
  0xaf0, 0xbf9, 0x8f3, 0x9fa, 0xef6, 0xfff, 0xcf5, 0xdfc,
  0x2fc, 0x3f5, 0xff , 0x1f6, 0x6fa, 0x7f3, 0x4f9, 0x5f0,
  0xb60, 0xa69, 0x963, 0x86a, 0xf66, 0xe6f, 0xd65, 0xc6c,
  0x36c, 0x265, 0x16f, 0x66 , 0x76a, 0x663, 0x569, 0x460,
  0xca0, 0xda9, 0xea3, 0xfaa, 0x8a6, 0x9af, 0xaa5, 0xbac,
  0x4ac, 0x5a5, 0x6af, 0x7a6, 0xaa , 0x1a3, 0x2a9, 0x3a0,
  0xd30, 0xc39, 0xf33, 0xe3a, 0x936, 0x83f, 0xb35, 0xa3c,
  0x53c, 0x435, 0x73f, 0x636, 0x13a, 0x33 , 0x339, 0x230,
  0xe90, 0xf99, 0xc93, 0xd9a, 0xa96, 0xb9f, 0x895, 0x99c,
  0x69c, 0x795, 0x49f, 0x596, 0x29a, 0x393, 0x99 , 0x190,
  0xf00, 0xe09, 0xd03, 0xc0a, 0xb06, 0xa0f, 0x905, 0x80c,
  0x70c, 0x605, 0x50f, 0x406, 0x30a, 0x203, 0x109, 0x0   };

const int triTable[256][16] = 
  {{-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 3, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {9, 2, 10, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {2, 8, 3, 2, 10, 8, 10, 9, 8, -1, -1, -1, -1, -1, -1, -1},
   {3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 11, 2, 8, 11, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 9, 0, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 11, 2, 1, 9, 11, 9, 8, 11, -1, -1, -1, -1, -1, -1, -1},
   {3, 10, 1, 11, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 10, 1, 0, 8, 10, 8, 11, 10, -1, -1, -1, -1, -1, -1, -1},
   {3, 9, 0, 3, 11, 9, 11, 10, 9, -1, -1, -1, -1, -1, -1, -1},
   {9, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 10, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {3, 4, 7, 3, 0, 4, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1},
   {9, 2, 10, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1},
   {2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1, -1},
   {8, 4, 7, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {11, 4, 7, 11, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1, -1},
   {9, 0, 1, 8, 4, 7, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1},
   {4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, -1, -1, -1, -1},
   {3, 10, 1, 3, 11, 10, 7, 8, 4, -1, -1, -1, -1, -1, -1, -1},
   {1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, -1, -1, -1, -1},
   {4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, -1, -1, -1, -1},
   {4, 7, 11, 4, 11, 9, 9, 11, 10, -1, -1, -1, -1, -1, -1, -1},
   {9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 10, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {3, 0, 8, 1, 2, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1},
   {5, 2, 10, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1, -1},
   {2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1, -1},
   {9, 5, 4, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 11, 2, 0, 8, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1},
   {0, 5, 4, 0, 1, 5, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1},
   {2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, -1, -1, -1, -1},
   {10, 3, 11, 10, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1},
   {4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, -1, -1, -1, -1},
   {5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, -1, -1, -1, -1},
   {5, 4, 8, 5, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1},
   {9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1, -1},
   {0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1, -1},
   {1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {9, 7, 8, 9, 5, 7, 10, 1, 2, -1, -1, -1, -1, -1, -1, -1},
   {10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1, -1},
   {8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, -1, -1, -1, -1},
   {2, 10, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1},
   {7, 9, 5, 7, 8, 9, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1},
   {9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, -1, -1, -1, -1},
   {2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1, -1},
   {11, 2, 1, 11, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1, -1},
   {9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, -1, -1, -1, -1},
   {5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, -1},
   {11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, -1},
   {11, 10, 5, 7, 11, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 3, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {9, 0, 1, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 8, 3, 1, 9, 8, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1},
   {1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1, -1},
   {9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1, -1},
   {5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1, -1},
   {2, 3, 11, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {11, 0, 8, 11, 2, 0, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1},
   {0, 1, 9, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1},
   {5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, -1, -1, -1, -1},
   {6, 3, 11, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, -1, -1, -1, -1},
   {3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1, -1},
   {6, 5, 9, 6, 9, 11, 11, 9, 8, -1, -1, -1, -1, -1, -1, -1},
   {5, 10, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 3, 0, 4, 7, 3, 6, 5, 10, -1, -1, -1, -1, -1, -1, -1},
   {1, 9, 0, 5, 10, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1},
   {10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1, -1},
   {6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1, -1},
   {8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1, -1},
   {7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, -1},
   {3, 11, 2, 7, 8, 4, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1},
   {5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, -1, -1, -1, -1},
   {0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1},
   {9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, -1},
   {8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, -1, -1, -1, -1},
   {5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, -1},
   {0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, -1},
   {6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, -1, -1, -1, -1},
   {10, 4, 9, 6, 4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 10, 6, 4, 9, 10, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1},
   {10, 0, 1, 10, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1, -1},
   {8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, -1, -1, -1, -1},
   {1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1, -1},
   {3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1, -1},
   {0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1},
   {10, 4, 9, 10, 6, 4, 11, 2, 3, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, -1, -1, -1, -1},
   {3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, -1, -1, -1, -1},
   {6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, -1},
   {9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, -1, -1, -1, -1},
   {8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, -1},
   {3, 11, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1, -1},
   {6, 4, 8, 11, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {7, 10, 6, 7, 8, 10, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1},
   {0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, -1, -1, -1, -1},
   {10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1, -1},
   {10, 6, 7, 10, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1, -1},
   {2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, -1},
   {7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1, -1},
   {7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, -1, -1, -1, -1},
   {2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, -1},
   {1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, -1},
   {11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, -1, -1, -1, -1},
   {8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, -1},
   {0, 9, 1, 11, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, -1, -1, -1, -1},
   {7, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {3, 0, 8, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 1, 9, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {8, 1, 9, 8, 3, 1, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1},
   {10, 1, 2, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 10, 3, 0, 8, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1},
   {2, 9, 0, 2, 10, 9, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1},
   {6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, -1, -1, -1, -1},
   {7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1, -1},
   {2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1, -1},
   {1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1, -1},
   {10, 7, 6, 10, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1, -1},
   {10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, -1, -1, -1, -1},
   {0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, -1, -1, -1, -1},
   {7, 6, 10, 7, 10, 8, 8, 10, 9, -1, -1, -1, -1, -1, -1, -1},
   {6, 8, 4, 11, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {3, 6, 11, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1, -1},
   {8, 6, 11, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1, -1},
   {9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, -1, -1, -1, -1},
   {6, 8, 4, 6, 11, 8, 2, 10, 1, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, -1, -1, -1, -1},
   {4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, -1, -1, -1, -1},
   {10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, -1},
   {8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1},
   {0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, -1, -1, -1, -1},
   {1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1, -1},
   {8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, -1, -1, -1, -1},
   {10, 1, 0, 10, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1, -1},
   {4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, -1},
   {10, 9, 4, 6, 10, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 9, 5, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 3, 4, 9, 5, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1},
   {5, 0, 1, 5, 4, 0, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1},
   {11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, -1, -1, -1, -1},
   {9, 5, 4, 10, 1, 2, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1},
   {6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, -1, -1, -1, -1},
   {7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, -1, -1, -1, -1},
   {3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, -1},
   {7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1, -1},
   {9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, -1, -1, -1, -1},
   {3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, -1, -1, -1, -1},
   {6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, -1},
   {9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, -1, -1, -1, -1},
   {1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, -1},
   {4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, -1},
   {7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, -1, -1, -1, -1},
   {6, 9, 5, 6, 11, 9, 11, 8, 9, -1, -1, -1, -1, -1, -1, -1},
   {3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1, -1},
   {0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, -1, -1, -1, -1},
   {6, 11, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, -1, -1, -1, -1},
   {0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, -1},
   {11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, -1},
   {6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, -1, -1, -1, -1},
   {5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1, -1},
   {9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1, -1},
   {1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, -1},
   {1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, -1},
   {10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, -1, -1, -1, -1},
   {0, 3, 8, 5, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {10, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {11, 5, 10, 7, 5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {11, 5, 10, 11, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1, -1},
   {5, 11, 7, 5, 10, 11, 1, 9, 0, -1, -1, -1, -1, -1, -1, -1},
   {10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, -1, -1, -1, -1},
   {11, 1, 2, 11, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, -1, -1, -1, -1},
   {9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, -1, -1, -1, -1},
   {7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, -1},
   {2, 5, 10, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1},
   {8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, -1, -1, -1, -1},
   {9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, -1, -1, -1, -1},
   {9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, -1},
   {1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1, -1},
   {9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1, -1},
   {9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {5, 8, 4, 5, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1},
   {5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, -1, -1, -1, -1},
   {0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, -1, -1, -1, -1},
   {10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, -1},
   {2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, -1, -1, -1, -1},
   {0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, -1},
   {0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, -1},
   {9, 4, 5, 2, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1, -1},
   {5, 10, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1, -1},
   {3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, -1},
   {5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, -1, -1, -1, -1},
   {8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1, -1},
   {0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, -1, -1, -1, -1},
   {9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 11, 7, 4, 9, 11, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1},
   {0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, -1, -1, -1, -1},
   {1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, -1, -1, -1, -1},
   {3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, -1},
   {4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, -1, -1, -1, -1},
   {9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, -1},
   {11, 7, 4, 11, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1, -1},
   {11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, -1, -1, -1, -1},
   {2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1, -1},
   {9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, -1},
   {3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, -1},
   {1, 10, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1, -1},
   {4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, -1, -1, -1, -1},
   {4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {9, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {3, 0, 9, 3, 9, 11, 11, 9, 10, -1, -1, -1, -1, -1, -1, -1},
   {0, 1, 10, 0, 10, 8, 8, 10, 11, -1, -1, -1, -1, -1, -1, -1},
   {3, 1, 10, 11, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 2, 11, 1, 11, 9, 9, 11, 8, -1, -1, -1, -1, -1, -1, -1},
   {3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, -1, -1, -1, -1},
   {0, 2, 11, 8, 0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {3, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {2, 3, 8, 2, 8, 10, 10, 8, 9, -1, -1, -1, -1, -1, -1, -1},
   {9, 10, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, -1, -1, -1, -1},
   {1, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
   {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
  };



struct{
  float x,y,z;
} center;

static struct{
  float x,y,z;
} transp = {0.,0.,0.};


extern struct {
  int num_of_colors,total_colors, highs[5], lows[5],
    total_line_colors, line_colors[5];}user_lines_colors_;



int curdisplist=0;

static float get_x0(A,B,C,D)
     float A,B,C,D;
{
      return((A-B) / ((A+C) - (B+D)));
}
static float get_y0(A,B,C,D)
     float A,B,C,D;
{
      return((A-D) / ((A+C) - (B+D)));
}

int ive_checkline(a,b,val)
    float a,b,val;
{
  float min,max;
  max = a < b ? b:a;
  min = a < b ? a:b;
  if(val >= min  && val <= max)return(1);
  return(0);
}

void get_errors(char *message){
  GLenum error = glGetError();
  if (error) {
    char *err = (char *) gluErrorString( error );
    fprintf( stderr, "GL Error: %s at %s\n", err, message );
  }
}

int ive_checkcube(a,b,c,d,e,f,g,h,val)
     float a,b,c,d,e,f,g,h,val;
{
  float max, min;
  max = min = a;
  if(max < b) max = b;
  else if(min > b) min = b;
  if(max < c) max = c;
  else if(min > c) min = c;
  if(max < d) max = d;
  else if(min > d) min = d;
  if(max < e) max = e;
  else if(min > e) min = e;
  if(max < f) max = f;
  else if(min > f) min = f;
  if(max < g) max = g;
  else if(min > g) min = g;
  if(max < h) max = h;
  else if(min > h) min = h;
  
  if(val > min  && val <= max)printf("%f %f %f\n",min,max,val);
  if(min==max)return(0);
  if(val > min  && val <= max)return(1);
  return(0);
}
  
static float get_dist(val, v1, v2)
     float val, v1, v2;
{
  if(v1 == v2) return (0.0);
  return((val - v1)/(v2 - v1));
}

static float check_line(val,x,y)
     float val,x,y;
{
  float min,max;
  if(x == y){
    if(val == x)
      return(2.0);
    return(-1.0);
  }
  min = x > y ? y : x;
  max = x > y ? x : y;
  /*  printf("Check: %f <= %f>= %f\n", min,val,max);*/
  if(min <= val && max >= val){
    return((val - x)/(y - x));
  }
  return (FLT_MAX);
}

static void vertinterp(val,p1, p2, val1, val2, pout )
     float val, val1, val2;
     struct plainpoint p1, p2, *pout;
{
  double dist;
  /*
  if(abs(val1-val)<EPSLON){
    pout->x=p1.x;
    pout->y=p1.y;
    pout->z=p1.z;
    return;
  }
  if(abs(val2-val)<EPSLON){
    pout->x=p2.x;
    pout->y=p2.y;
    pout->z=p2.z;
    return;
  }
  if(abs(val1-val2)<EPSLON){
    pout->x=p1.x;
    pout->y=p1.y;
    pout->z=p1.z;
    return;
  }
  */
  dist = (val - val1)/(val2 - val1);
  pout->x = p1.x + dist * (p2.x - p1.x);
  pout->y = p1.y + dist * (p2.y - p1.y);
  pout->z = p1.z + dist * (p2.z - p1.z);
}


void ive_get_surface(vert, val, points, triangles)
     float vert[8], val;
     struct plainpoint points[8];
     struct TRIANGLES *triangles;
{
  /* Original from http://local.wasp.uwa.edu.au/~pbourke/geometry/polygonise/  */

  /*****      4-----5
   *****     /|    /|
   *****    7-----6 |
   *****    | 0---|-1
   *****    |/    |/ 
   *****    3-----2  
   *****
   *****/
  /*****      ---4---
   *****     7|    5|
   *****    ---6--- |
   *****    | ---0|--
   *****    |3    |1 
   *****    ---2---  
   *****
   *****/
  int index=0; /*index of cube type*/
  int i,l;
  struct plainpoint verts[12]={{0,0,0},{0,0,0},{0,0,0},{0,0,0},{0,0,0},
			  {0,0,0},{0,0,0},{0,0,0},{0,0,0},{0,0,0},
			  {0,0,0},{0,0,0}};

  float dist;

  if(vert[0] < val) index |= 1;
  if(vert[1] < val) index |= 2;
  if(vert[2] < val) index |= 4;
  if(vert[3] < val) index |= 8;
  if(vert[4] < val) index |= 16;
  if(vert[5] < val) index |= 32;
  if(vert[6] < val) index |= 64;
  if(vert[7] < val) index |= 128;
  // Find the vertices where the surface intersects the cube 
  if (index == 0 || index == 255){
    triangles->num_triangles=0;
    return;
  }
  //  printf("index: %d\n",index);
  if (edgeTable[index] & 1)
    {
      vertinterp(val,points[0],points[1],vert[0],vert[1],&verts[0]);
    }
  if (edgeTable[index] & 2)
    {
      vertinterp(val,points[1],points[2],vert[1],vert[2],&verts[1]);
      //      printf("&2 dist=%f\n",dist);
    }
  if (edgeTable[index] & 4)
    {
      vertinterp(val,points[2],points[3],vert[2],vert[3],&verts[2]);
    }
  if (edgeTable[index] & 8)
    {
      vertinterp(val,points[3],points[0],vert[3],vert[0],&verts[3]);
      //printf("&8 dist=%f\n",dist);
    }
  if (edgeTable[index] & 16)
    {
      vertinterp(val,points[4],points[5],vert[4],vert[5],&verts[4]);
    }
  if (edgeTable[index] & 32)
    {
      vertinterp(val,points[5],points[6],vert[5],vert[6],&verts[5]);
      //printf("&32 dist=%f\n",dist);
    }
  if (edgeTable[index] & 64)
    {
      vertinterp(val,points[6],points[7],vert[6],vert[7],&verts[6]);
      //printf("got here\n");
    }
  if (edgeTable[index] & 128)
    {
      vertinterp(val,points[7],points[4],vert[7],vert[4],&verts[7]);
      //printf("&128 dist=%f\n",dist);
    }
  if (edgeTable[index] & 256)
    {
      vertinterp(val,points[0],points[4],vert[0],vert[4],&verts[8]);
      //printf("&256 dist=%f\n",dist);
    }
  if (edgeTable[index] & 512)
    {
      vertinterp(val,points[1],points[5],vert[1],vert[5],&verts[9]);
      //printf("&512 dist=%f\n",dist);
    }
  if (edgeTable[index] & 1024)
    {
      vertinterp(val,points[2],points[6],vert[2],vert[6],&verts[10]);
      //printf("&1024 dist=%f\n",dist);
    }	 
  if (edgeTable[index] & 2048)
    {
      vertinterp(val,points[3],points[7],vert[3],vert[7],&verts[11]);
      //printf("&2048 dist=%f\n",dist);
    }
  //Don't need to see if we have one as that was done before we got here.
   /* Create the triangle */
   for (i=0;triTable[index][i]!=-1;i+=3) {
      triangles->tri[triangles->num_triangles].p1.x = verts[triTable[index][i  ]].x;
      triangles->tri[triangles->num_triangles].p1.y = verts[triTable[index][i  ]].y;
      triangles->tri[triangles->num_triangles].p1.z = verts[triTable[index][i  ]].z;
      triangles->tri[triangles->num_triangles].p2.x = verts[triTable[index][i+1]].x;
      triangles->tri[triangles->num_triangles].p2.y = verts[triTable[index][i+1]].y;
      triangles->tri[triangles->num_triangles].p2.z = verts[triTable[index][i+1]].z;
      triangles->tri[triangles->num_triangles].p3.x = verts[triTable[index][i+2]].x;
      triangles->tri[triangles->num_triangles].p3.y = verts[triTable[index][i+2]].y;
      triangles->tri[triangles->num_triangles].p3.z = verts[triTable[index][i+2]].z;
      triangles->num_triangles++;
   }

   //   for(l=0; l<12; l++){
   //if(verts[l].y != 0 && ( verts[l].y<j || verts[l].y > j+1))
   //  printf("i,j,k %d,%d,%d verts[%d].y=%f\n",i,j,k,l,verts[l].y);
   //}
} 


/*
void ive_get_points(i, j, k, vert, val, triangles)
     int i,j,k;
     float vert[8], val;
     struct TRIANGLES *triangles;
     {
*/
  /* Original from http://local.wasp.uwa.edu.au/~pbourke/geometry/polygonise/  */

  /*****     points passed same as for triangles
   *****     
   *****    7
   *****    | 0
   *****    |/ 
   *****    3-----2  
   *****
   *****/
/*
  int l;
  struct plainpoint verts[12]={{-1,-1,-1},{-1,-1,-1},{-1,-1,-1},{-1,-1,-1},{-1,-1,-1},
			  {-1,-1,-1},{-1,-1,-1},{-1,-1,-1},{-1,-1,-1},{-1,-1,-1},
			  {-1,-1,-1},{-1,-1,-1}};
  float dist;
  int dd=-1;

  if(vert[3] == val){
    verts[0].x = 0.;
    verts[0].y = 0.;
    verts[0].z = 0.;
  }
  
  if(vert[3] < val){
    if(vert[7]>val){
      dd++;
      dist=vertinterp(vert[3],vert[7],val);
      verts[dd].x = 0.;
      verts[dd].y = dist;
      verts[dd].z = 0.;
    }
    if(vert[2]>val){
      dd++;
      dist=vertinterp(vert[3],vert[2],val);
      verts[dd].x = dist;
      verts[dd].y = 0.;
      verts[dd].z = 0.;
    }
    if(vert[0]>val){
      dd++;
      dist=vertinterp(vert[3],vert[0],val);
      verts[dd].x = 0.;
      verts[dd].y = 0.;
      verts[dd].z = dist;
    }
  }
  if(vert[3] > val){
    if(vert[7]<val){
      dd++;
      dist=vertinterp(vert[3],vert[7],val);
      verts[dd].x = 0.;
      verts[dd].y = dist;
      verts[dd].z = 0.;
    }
    if(vert[2]<val){
      dd++;
      dist=vertinterp(vert[3],vert[2],val);
      verts[dd].x = dist;
      verts[dd].y = 0.;
      verts[dd].z = 0.;
    }
    if(vert[0]<val){
      dd++;
      dist=vertinterp(vert[3],vert[0],val);
      verts[dd].x = 0.;
      verts[dd].y = 0.;
      verts[dd].z = dist;
    }
  }
  if(dd > -1){
    for(i=dd+1; i<3; i++){
      verts[i].x = verts[dd].x;
      verts[i].y = verts[dd].y;
      verts[i].z = verts[dd].z;
    }
    triangles->tri[0].p1.x = verts[0].x;
    triangles->tri[0].p1.y = verts[0].y;
    triangles->tri[0].p1.z = verts[0].z;
    triangles->tri[0].p2.x = verts[1].x;
    triangles->tri[0].p2.y = verts[1].y;
    triangles->tri[0].p2.z = verts[1].z;
    triangles->tri[0].p3.x = verts[2].x;
    triangles->tri[0].p3.y = verts[2].y;
    triangles->tri[0].p3.z = verts[2].z;
    triangles->num_triangles=1;
  }
  else
    triangles->num_triangles=0;
}
*/


void ive_get_points(vert, val, points, triangles)
     float vert[8], val;
     struct plainpoint points[8];
     struct TRIANGLES *triangles;
{
  /* Original from http://local.wasp.uwa.edu.au/~pbourke/geometry/polygonise/  */
  
  /*****      4-----5
   *****     /|    /|
   *****    7-----6 |
   *****    | 0---|-1
   *****    |/    |/ 
   *****    3-----2  
   *****
   *****/
  /*****      ---4---
   *****     7|    5|
   *****    ---6--- |
   *****    | ---0|--
   *****    |3    |1 
   *****    ---2---  
   *****
   *****/
  int i,l;
  
  float dist;
  struct plainpoint verts;
  triangles->num_triangles = 0;
  for(i=0;i<8;i++){
    if(vert[i] > 0.0){
      triangles->tri[triangles->num_triangles].p1.x=points[i].x;
      triangles->tri[triangles->num_triangles].p1.y=points[i].y;
      triangles->tri[triangles->num_triangles].p1.z=points[i].z;
      triangles->num_triangles++;
    }
  }
  return;
  printf("%f: %f %f %f %f %f %f %f %f\n",val,vert[0],vert[1],vert[2],vert[3],vert[4],vert[5],vert[6],vert[7]);
  if(vert[0] <= val && vert[1] >= val) {
    vertinterp(val,points[0], points[1], vert[0], vert[1], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[3] <= val && vert[2] >= val) {
    vertinterp(val,points[3], points[2], vert[3], vert[2], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[0] <= val && vert[3] >= val) {
    vertinterp(val,points[0], points[3], vert[0], vert[3], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[1] <= val && vert[2] >= val) {
    vertinterp(val,points[1], points[2], vert[1], vert[2], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[4] <= val && vert[5] >= val) {
    vertinterp(val,points[4], points[5], vert[4], vert[5], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[7] <= val && vert[6] >= val) {
    vertinterp(val,points[7], points[6], vert[7], vert[6], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[7] <= val && vert[4] >= val) {
    vertinterp(val,points[4], points[7], vert[4], vert[7], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[5] <= val && vert[6] >= val) {
    vertinterp(val,points[5], points[6], vert[5], vert[6], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[0] <= val && vert[4] >= val) {
    vertinterp(val,points[0], points[4], vert[0], vert[4], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[1] <= val && vert[5] >= val) {
    vertinterp(val,points[1], points[5], vert[1], vert[5], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[2] <= val && vert[6] >= val) {
    vertinterp(val,points[2], points[6], vert[2], vert[6], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  if(vert[3] <= val && vert[7] >= val) {
    vertinterp(val,points[3], points[7], vert[3], vert[7], &verts);
    triangles->tri[triangles->num_triangles].p1.x=verts.x;
    triangles->tri[triangles->num_triangles].p1.y=verts.y;
    triangles->tri[triangles->num_triangles].p1.z=verts.z;
    triangles->num_triangles++;
  }
  
}
