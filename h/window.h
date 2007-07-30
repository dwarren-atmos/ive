/* This file contains the window_points struct which is used in all slicers,
 * mouse ops and cpmpxy
 */

extern struct wpt{
    float x1; 
    float x2;
    float y1;
    float y2;
    int   imin, imax, jmin, jmax;
    int   xaxis;
    int   yaxis;
    int   window_axes;
    int   reverse;
} window_points_;

extern struct wpt3{
    float x1; 
    float x2;
    float y1;
    float y2;
    float z1;
    float z2;
    int   imin, imax, jmin, jmax, kmin, kmax;
    int   xaxis;
    int   yaxis;
    int   zaxis;
    int   window_axes;
    int   reverse;
} window_points3_;

