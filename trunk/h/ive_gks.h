#define WS_X 1
#define WS_WISS 2
#define WS_MO 3
#define NUM_COLORS 140
#define MAXLIST 15

struct ive_color {
    float r;
    float g;
    float b;
};

struct ive_color *color_table_list[MAXLIST], *traj_table;
extern struct{
    int min_user_color, max_user_color, min_traj_color, max_traj_color;
}user_colors_;
