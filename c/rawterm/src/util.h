#ifndef RT_UTIL_H
#define RT_UTIL_H

struct abuf
{
  char *b;
  int len;
};
typedef struct abuf abuf;

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
