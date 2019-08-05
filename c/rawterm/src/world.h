#ifndef RT_WORLD_H
#define RT_WORLD_H

#include <termios.h>

// Keep all the state of app in here.

struct erow
{
  int size;
  char *chars;
};
typedef struct erow erow;

struct world_atom
{
  char *udp_out_host;
  char *udp_out_port;
  char *udp_listen_port;
  int cx, cy;
  int rowoff;
  int rows;
  int cols;
  int numrows;
  erow *row;
  struct termios orig_termios;
};

typedef struct world_atom world_atom;

void
init_world ();

world_atom world;

#endif
