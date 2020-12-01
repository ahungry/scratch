#include <stdio.h>

int
main (int argc, char *argv[])
{
  FILE *fh = fopen ("/sys/class/backlight/intel_backlight/brightness", "w+");

  // Either will trigger the free tcache complaint
  // fwrite ("1.5", 3, 1, fh);
  fwrite ("105", 3, 1, fh);
  fclose (fh);
  fclose (fh);

  return 0;
}
