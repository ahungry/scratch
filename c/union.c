#include <stdio.h>
#include <stdlib.h>

typedef struct one {
  int x;
} one_t;

typedef struct two {
  int x;
} two_t;

typedef union one_two {
  one_t ot;
  two_t tt;
} one_two_t;

one_t
get_ot (one_two_t *x)
{
  return x->ot;
}

one_t
get_out (union one_two *x)
{
  return x->ot;
}

typedef union _point_pointed_t point_pointed_t;
union _point_pointed_t {
  struct {
    int x;
    int y;
  } upoint;
  struct {
    int a, b;
  } upointed;
};

typedef struct {
  int x;
  int y;
} _point_pointed_t_upoint;

typedef struct {
  int a, b;
} _point_pointed_t_upointed;


_point_pointed_t_upoint _point_pointed_t_upoint_get (union _point_pointed_t *x) {
  return x->upoint;
}

int
main (int argc, char *argv[])
{
  one_two_t *x = malloc (sizeof (one_two_t));
  one_t *ot = malloc (sizeof (one_t));

  ot->x = 20;
  x->ot = *ot;

  printf ("The result is: %d\n", (get_ot (x)).x);

  return 0;
}
