#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>     /* defines STDIN_FILENO, system calls,etc */

#include "util.h"

void
die (const char *s)
{
  perror (s);
  exit (1);
}

void
ab_append (struct abuf *ab, const char *s, int len)
{
  char *new = realloc (ab->b, ab->len + len);

  if (new == NULL) return;

  memcpy (&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}

void
ab_write (struct abuf *ab)
{
  write (STDOUT_FILENO, ab->b, ab->len);
}

void
ab_free (struct abuf *ab)
{
  free (ab->b);
  ab->len = 0;
}

int
get_byte_size_of_int_as_char (int n)
{
  return (int) ((ceil (log10 (n)) + 1) * sizeof (char));
}
