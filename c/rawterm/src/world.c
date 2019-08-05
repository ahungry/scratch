#include "term.h"
#include "util.h"
#include "world.h"

void
init_world ()
{
  world.cx = 10;
  world.cy = 10;
  world.rowoff = 0;
  world.numrows = 0;
  world.row = NULL;

  if (get_window_size (&world.rows, &world.cols) == -1) die("get_window_size");
}
