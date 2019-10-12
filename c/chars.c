#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

int
main ()
{
  char *s = "ARROW_UP";
  int len = strlen (s);
  printf ("%s %d\n", s, len);

  return 0;
}
