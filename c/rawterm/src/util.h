#ifndef RT_UTIL_H
#define RT_UTIL_H

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>     /* defines STDIN_FILENO, system calls,etc */

struct abuf
{
  char *b;
  int len;
};
typedef struct abuf abuf;

// Acts like a constructor
#define ABUF_INIT {NULL, 0}

void
die (const char *s);

void
ab_append (struct abuf *ab, const char *s, int len);

void
ab_write (struct abuf *ab);

void
ab_free (struct abuf *ab);

int
get_byte_size_of_int_as_char (int n);

#endif
