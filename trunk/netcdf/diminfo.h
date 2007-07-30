typedef struct {
    char name[8];
    int lin;     /* 1 = lin 0 = log */
    float min_tic;                /*minor tick mark interval */
    float maj_tic;                /*major tick mark interval */
    float lmin;                   /* min value for line plots */
    float lmax;                   /* max value for line plots */ 
    float v_interval;             /* number of points to skip in vectors */
    char disp_units[ UT_NAMELEN ];/* display unit name */
             /* added to track trajectories*/
    float traj_val;               /* midpoint value for colors */
  float traj_step;              /* step size between color changes */
  } dim_attributes;

